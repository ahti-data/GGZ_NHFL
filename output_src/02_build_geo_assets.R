# 02_build_geo_assets.R
#
# Bouwt de kaartlagen voor het kickoff-dashboard uit de gemeentegeografie van
# maptool_v4. Zie PLAN.md par. 4.
#
# Uit maptool_v4/data/ komt alleen geo_municipalities.rds (3,7 MB) -- de wijk-
# en buurtlagen en de drie geojson-bronbestanden (samen 284 MB) blijven waar ze
# zijn; het kickoff-dashboard heeft ze niet nodig.
#
# Uitvoer in dashboard/data/:
#   geo_gemeente.rds   -- vereenvoudigde gemeentegrenzen per geo-jaar, met provincie
#   geo_provincie.rds  -- 12 provincies per geo-jaar (dissolve van bovenstaande)
#   geo_regio.rds      -- NH+FL / Overig Nederland / NH+FL-als-geheel per geo-jaar
#
# De provincie- en regiolagen worden gedissolved uit exact dezelfde gemeentelaag
# waarop ook de cijfers geaggregeerd worden. Een kant-en-klare provincielaag
# (PDOK, of de provinces.json in shiny_dashboard_template) zou net andere grenzen
# geven en dus slivers of net-buiten-vallende gemeenten opleveren.
#
# Eenmalig te draaien vanuit de repo-root:
#   Rscript output_src/02_build_geo_assets.R

suppressMessages({
  library(sf)
  library(dplyr)
})

sf::sf_use_s2(FALSE)  # planaire operaties; snel genoeg en robuuster bij dissolve

GEO_YEARS   <- c(2021L, 2022L, 2023L)
NHFL        <- c("Noord-Holland", "Flevoland")
SIMPLIFY_KEEP <- 0.05   # aandeel punten dat ms_simplify() behoudt

SRC_GEO    <- file.path("..", "maptool_v4", "data", "geo_municipalities.rds")
LOOKUP     <- file.path("dashboard", "data", "gemeente_provincie.csv")
OUT_DIR    <- file.path("dashboard", "data")

stopifnot(file.exists(SRC_GEO), file.exists(LOOKUP))

# --- 1. Gemeentelaag ----------------------------------------------------------
message("Gemeentelaag inlezen...")
gm <- readRDS(SRC_GEO) %>% filter(jaar %in% GEO_YEARS)

lookup <- read.csv(LOOKUP, colClasses = "character", encoding = "UTF-8")
lookup$jaar <- as.integer(lookup$jaar)

gm <- gm %>% left_join(lookup, by = c("code", "jaar"))
if (any(is.na(gm$provincie))) {
  stop(sum(is.na(gm$provincie)), " gemeente(n) zonder provincie na de join -- ",
       "draai eerst output_src/03_build_gemeente_provincie.R")
}

# Per jaar verwerken, niet in een keer. mapshaper bouwt een topologie over de
# hele laag; gemeenten uit verschillende jaren liggen over elkaar heen en zouden
# die topologie vertroebelen.

#' Verwijder kleine gaten uit een gedissolvede laag.
#'
#' De gemeentegrenzen in maptool_v4 sluiten niet overal exact op elkaar aan: na
#' het dissolven van Gelderland (2023) blijven 271 gaatjes over van samen 4,5
#' km2 op ruim 5.000 km2, het grootste 0,29 km2. Dat zijn geen meren of
#' enclaves maar afrondingsartefacten in de brondata, en op de kaart zien ze
#' eruit als spookgrenzen dwars door een provincie.
#'
#' Gaten boven de drempel blijven staan -- mocht er ooit een echt ingesloten
#' gebied zijn, dan verdwijnt dat niet stilzwijgend.
#'
#' @param sf_obj sf-object met (multi)polygonen.
#' @param max_opp_m2 Gaten kleiner dan dit oppervlak worden dichtgemaakt.
drop_kleine_gaten <- function(sf_obj, max_opp_m2 = 1e6) {
  # Oppervlakte in vierkante meters vraagt om een geprojecteerd stelsel;
  # EPSG:28992 (RD New) is het Nederlandse.
  geom_rd <- sf::st_geometry(sf::st_transform(sf_obj, 28992))

  # Alle binnenringen eerst verzamelen en in een keer opmeten. st_area() per
  # ring aanroepen is hier onwerkbaar traag: op de onvereenvoudigde landsdekking
  # gaat het om duizenden ringen, en elke losse aanroep bouwt zijn eigen sfc op.
  ringen <- list()
  adres  <- list()
  for (i in seq_along(geom_rd)) {
    g <- geom_rd[[i]]
    delen <- if (inherits(g, "MULTIPOLYGON")) g else list(g)
    for (j in seq_along(delen)) {
      p <- delen[[j]]
      if (length(p) > 1) {
        for (k in 2:length(p)) {
          ringen[[length(ringen) + 1L]] <- sf::st_polygon(list(p[[k]]))
          adres[[length(adres) + 1L]]   <- c(i, j, k)
        }
      }
    }
  }
  if (!length(ringen)) return(sf_obj)

  opp <- as.numeric(sf::st_area(sf::st_sfc(ringen, crs = 28992)))
  adres <- do.call(rbind, adres)
  weg <- adres[opp < max_opp_m2, , drop = FALSE]
  if (!nrow(weg)) return(sf_obj)

  schoon <- lapply(seq_along(geom_rd), function(i) {
    g <- geom_rd[[i]]
    delen <- if (inherits(g, "MULTIPOLYGON")) g else list(g)
    nieuw <- lapply(seq_along(delen), function(j) {
      p <- delen[[j]]
      if (length(p) <= 1) return(p)
      te_weg <- weg[weg[, 1] == i & weg[, 2] == j, 3]
      if (!length(te_weg)) return(p)
      p[-te_weg]
    })
    if (inherits(g, "MULTIPOLYGON")) sf::st_multipolygon(nieuw) else sf::st_polygon(nieuw[[1]])
  })

  sf::st_geometry(sf_obj) <- sf::st_transform(
    sf::st_sfc(schoon, crs = 28992), sf::st_crs(sf_obj)
  )
  sf::st_make_valid(sf_obj)
}

#' Dissolve op een veld: eerst samenvoegen, dan pas vereenvoudigen.
#'
#' De volgorde is wezenlijk. Vereenvoudigen voordat er gedissolved wordt trekt
#' aangrenzende gemeentegrenzen uit elkaar: bij keep = 0.05 groeien de gaatjes
#' in de brondata van hooguit 0,3 km2 naar 1 tot 25 km2, en die blijven daarna
#' als spookgrenzen dwars door de provincie zichtbaar. Dissolven op de
#' onvereenvoudigde geometrie en het resultaat daarna vereenvoudigen levert een
#' provincie op met alleen zijn eigen buitengrens.
#'
#' ms_dissolve() werkt op gedeelde bogen en is robuuster dan sf::st_union() op
#' lengte-/breedtegraden; [drop_kleine_gaten()] ruimt op wat de brondata zelf
#' aan gaatjes bevat.
dissolve_op <- function(sf_obj, veld) {
  samengevoegd <- sf::st_make_valid(rmapshaper::ms_dissolve(sf_obj, field = veld))
  zonder_gaten <- drop_kleine_gaten(samengevoegd)
  sf::st_make_valid(
    rmapshaper::ms_simplify(zonder_gaten, keep = SIMPLIFY_KEEP, keep_shapes = TRUE)
  )
}

gemeente_lijst <- list()
provincie_lijst <- list()
regio_lijst <- list()

for (y in GEO_YEARS) {
  message("Jaar ", y, "...")
  jaar_gm <- gm %>% filter(jaar == y)

  # De gemeentelaag zelf wordt wel gewoon vereenvoudigd: daar is elke gemeente
  # een eigen vlak, dus gaten tussen buren zijn er niet aan de orde.
  gemeente_lijst[[as.character(y)]] <- sf::st_make_valid(
    rmapshaper::ms_simplify(jaar_gm, keep = SIMPLIFY_KEEP, keep_shapes = TRUE)
  )

  # --- Provincies -------------------------------------------------------------
  provincie_lijst[[as.character(y)]] <- dissolve_op(jaar_gm, "provincie") %>%
    rename(gebied = provincie) %>%
    mutate(jaar = y)

  # --- Regio's ----------------------------------------------------------------
  # Twee gebiedsindelingen in een laag, onderscheiden door `indeling`:
  #   nh_fl_vs_rest -- "Noord-Holland & Flevoland" naast "Overig Nederland"
  #   nh_fl_totaal  -- alleen "Noord-Holland & Flevoland", als een vlak
  # De losse provincies NH en FL (kaartweergave 4) komen uit geo_provincie.rds.
  vs_rest <- jaar_gm %>%
    mutate(regio = ifelse(provincie %in% NHFL,
                          "Noord-Holland & Flevoland", "Overig Nederland")) %>%
    dissolve_op("regio") %>%
    rename(gebied = regio) %>%
    mutate(jaar = y, indeling = "nh_fl_vs_rest")

  totaal <- vs_rest %>%
    filter(gebied == "Noord-Holland & Flevoland") %>%
    mutate(indeling = "nh_fl_totaal")

  regio_lijst[[as.character(y)]] <- dplyr::bind_rows(vs_rest, totaal)
}

gm_simple <- dplyr::bind_rows(gemeente_lijst)
pv        <- dplyr::bind_rows(provincie_lijst) %>% select("jaar", "gebied", "geometry")
regio     <- dplyr::bind_rows(regio_lijst) %>% select("jaar", "indeling", "gebied", "geometry")

saveRDS(gm_simple, file.path(OUT_DIR, "geo_gemeente.rds"), compress = "xz")
saveRDS(pv, file.path(OUT_DIR, "geo_provincie.rds"), compress = "xz")
saveRDS(regio, file.path(OUT_DIR, "geo_regio.rds"), compress = "xz")

# --- Controle: is er echt gedissolved? ----------------------------------------
# Blijven er binnengrenzen staan, dan uit zich dat in een groot aantal ringen
# per polygoon (haarscheurtjes worden gaten). Een provincie hoort maar een
# handvol ringen te hebben: de buitengrens plus eventuele eilanden.
ringen <- function(g) {
  parts <- if (inherits(g, "MULTIPOLYGON")) g else list(g)
  sum(vapply(parts, length, integer(1)))
}
max_ringen <- max(vapply(sf::st_geometry(pv), ringen, integer(1)))
if (max_ringen > 25) {
  warning("Tot ", max_ringen, " ringen in een provincie -- mogelijk zijn er ",
          "binnengrenzen blijven staan. Controleer de dissolve.")
} else {
  message("Dissolve-controle: hoogstens ", max_ringen, " ringen per provincie.")
}

# --- Samenvatting -------------------------------------------------------------
sz <- function(f) sprintf("%.2f MB", file.size(file.path(OUT_DIR, f)) / 1e6)
message("\nGeschreven:")
message("  geo_gemeente.rds  ", nrow(gm_simple), " rijen  ", sz("geo_gemeente.rds"))
message("  geo_provincie.rds ", nrow(pv), " rijen  ", sz("geo_provincie.rds"))
message("  geo_regio.rds     ", nrow(regio), " rijen  ", sz("geo_regio.rds"))

for (y in GEO_YEARS) {
  message("  ", y, ": ", sum(gm_simple$jaar == y), " gemeenten, ",
          sum(pv$jaar == y), " provincies, waarvan NH/FL ",
          sum(gm_simple$jaar == y & gm_simple$provincie %in% NHFL), " gemeenten")
}
