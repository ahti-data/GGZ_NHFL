#' GGZ ZPM -- kaartlaag: bins, legenda, leaflet en PNG-export
#'
#' De bin- en legendalogica en de PNG-export zijn overgenomen uit
#' maptool_v4/app.R (`palette_generator()` regel 1366 en `output$downloadMap`
#' regel 1936) en teruggebracht tot wat het kickoff-dashboard nodig heeft: geen
#' drill-down naar wijk/buurt, geen negen zorgdomeinen, geen profieltabellen.

suppressMessages({
  library(leaflet)
  library(ggplot2)
  library(sf)
})

#' Standaardkleuren, uit de ahti-huisstijl (`data/metadata/brand_colors.R`).
GGZ_KLEUR_LAAG   <- "#F4F4F4"
GGZ_KLEUR_HOOG   <- "#009DDC"
GGZ_KLEUR_MIDDEN <- "#F4F4F4"
GGZ_KLEUR_INDEX_LAAG <- "#EE3124"
GGZ_KLEUR_GEEN_DATA  <- "#D9D9D9"

#' Bepaal de klassegrenzen en het kleurenpalet voor een kaart.
#'
#' Gewone uitkomsten krijgen acht gelijke klassen over het waardebereik. Indices
#' zijn rond 1 gecentreerd: die krijgen twee reeksen van vier, met 1 als
#' scharnierpunt, zodat "meer dan verwacht" en "minder dan verwacht" ook aan de
#' kleur af te lezen zijn.
#'
#' @param values Numerieke vector met de te tekenen waarden.
#' @param is_index Is dit een indexuitkomst?
#' @param kleur_laag,kleur_hoog,kleur_midden Kleuren voor het palet. Bij een
#'   indexschaal is `kleur_laag` de kleur voor "minder dan verwacht" en
#'   `kleur_midden` die op het scharnierpunt 1,00x. Alle drie zijn in de UI
#'   instelbaar; [GGZ_KLEUR_INDEX_LAAG] is enkel de standaardwaarde die de UI
#'   invult zodra er een indexuitkomst gekozen wordt.
#' @return Lijst met `bins`, `palette` (een leaflet-kleurfunctie), `error` en
#'   `message`. Bij te weinig variatie is `error` TRUE en `palette` NULL.
ggz_palette <- function(values, is_index = FALSE,
                        kleur_laag = NULL,
                        kleur_hoog = GGZ_KLEUR_HOOG,
                        kleur_midden = GGZ_KLEUR_MIDDEN) {
  # De lage kleur betekent iets anders per soort schaal en heeft dus een andere
  # standaard: bij een gewone schaal het lichtste eind van een oplopende reeks,
  # bij een indexschaal "minder dan verwacht". Zou hier altijd het lichtgrijs
  # gelden, dan vielen bij een index de lage en de middenkleur samen en werd de
  # hele onderkant van de schaal een egaal vlak.
  if (is.null(kleur_laag)) {
    kleur_laag <- if (is_index) GGZ_KLEUR_INDEX_LAAG else GGZ_KLEUR_LAAG
  }
  vals <- values[!is.na(values)]
  if (length(vals) == 0) {
    return(list(bins = NULL, palette = NULL, error = TRUE,
                message = "Geen data beschikbaar voor deze selectie."))
  }

  rng <- range(vals)

  # Een enkel gebied (kaartweergave 'NH/FL als geheel') heeft geen bereik om
  # klassen over te verdelen. Dan een symbolisch bandje rond de waarde, zodat
  # het vlak nog steeds een kleur krijgt.
  #
  # `enkel` markeert dat geval, want de grenzen van dat bandje zijn verzonnen:
  # ze als klassegrenzen in de legenda zetten zou een gebied met waarde 1,01x
  # laten zien onder het kopje "0,91x". Wie hier tekent, toont de waarde zelf.
  if (diff(rng) == 0) {
    breedte <- max(abs(rng[1]) * 0.1, 1e-9)
    bins <- c(rng[1] - breedte, rng[1] + breedte)
    pal <- ggz_bin_palette(bins, kleur_hoog)
    return(list(bins = bins, palette = pal, error = FALSE, message = NULL,
                enkel = TRUE, waarde = rng[1]))
  }

  # Nooit meer klassen dan er gebieden zijn. Acht klassen over twaalf provincies
  # leest prettig, maar acht klassen over de twee vlakken van "NH/FL vs. rest"
  # levert een legenda op waarvan driekwart leeg is.
  n_klassen <- min(8L, length(unique(vals)))

  if (is_index) {
    midden <- 1
    lo <- min(rng[1], midden)
    hi <- max(rng[2], midden)
    span_onder <- midden - lo
    span_boven <- hi - midden

    # Klassen verdelen naar rato van de werkelijke spreiding aan weerszijden van
    # 1. Liggen alle gebieden boven de verwachting, dan is er onder de 1 niets te
    # verdelen en gaan alle klassen naar de bovenkant -- anders zou de helft van
    # de klassen samenvallen op precies 1 en zouden alle gebieden in een enkele
    # klasse belanden.
    if (span_onder <= 0) {
      onder <- 0L; boven <- n_klassen
    } else if (span_boven <= 0) {
      onder <- n_klassen; boven <- 0L
    } else {
      onder <- max(1L, min(n_klassen - 1L,
                           as.integer(round(n_klassen * span_onder / (span_onder + span_boven)))))
      boven <- n_klassen - onder
    }

    bins <- unique(sort(c(
      if (onder > 0) seq(lo, midden, length.out = onder + 1) else midden,
      if (boven > 0) seq(midden, hi, length.out = boven + 1) else midden
    )))

    # De kleuren worden per klasse vastgelegd ten opzichte van het scharnierpunt,
    # niet gelijkmatig over alle klassen uitgesmeerd. Een kleurenschaal die van
    # rood via grijs naar blauw loopt hoort grijs te zijn bij 1,00x -- ook als de
    # klassen ongelijk over beide zijden verdeeld zijn.
    kleuren_onder <- if (onder > 0) {
      grDevices::colorRampPalette(c(kleur_laag, kleur_midden))(onder + 1)[seq_len(onder)]
    } else character(0)
    kleuren_boven <- if (boven > 0) {
      grDevices::colorRampPalette(c(kleur_midden, kleur_hoog))(boven + 1)[-1]
    } else character(0)
    bin_kleuren <- c(kleuren_onder, kleuren_boven)
  } else {
    bins <- unique(seq(rng[1], rng[2], length.out = n_klassen + 1))
    bin_kleuren <- grDevices::colorRampPalette(c(kleur_laag, kleur_hoog))(max(1L, length(bins) - 1L))
  }

  if (length(bins) < 2) {
    return(list(bins = NULL, palette = NULL, error = TRUE,
                message = "Onvoldoende variatie in de data om een legenda te tonen."))
  }
  bin_kleuren <- rep_len(bin_kleuren, length(bins) - 1L)

  list(
    bins    = bins,
    palette = ggz_bin_palette(bins, bin_kleuren),
    error   = FALSE,
    message = NULL,
    enkel   = FALSE,
    waarde  = NA_real_
  )
}

#' Maak een kleurfunctie die een waarde op zijn klassekleur afbeeldt.
#'
#' Doet wat `leaflet::colorBin()` doet, maar met kleuren die per klasse zijn
#' vastgelegd in plaats van gelijkmatig over het bereik geïnterpoleerd. Dat is
#' nodig voor de indexschaal, waar de middenkleur op 1,00x moet vallen en niet
#' in het midden van de klassenreeks.
#'
#' @param bins Klassegrenzen (lengte n + 1).
#' @param bin_kleuren Kleuren per klasse (lengte n).
#' @return Een functie die een numerieke vector op kleuren afbeeldt; `NA` in,
#'   `NA` uit.
ggz_bin_palette <- function(bins, bin_kleuren) {
  force(bins); force(bin_kleuren)
  function(x) {
    idx <- cut(x, breaks = bins, include.lowest = TRUE, labels = FALSE)
    out <- rep(NA_character_, length(x))
    ok <- !is.na(idx)
    out[ok] <- bin_kleuren[idx[ok]]
    out
  }
}

#' Bouw de verticale legenda naast de kaart.
#'
#' Overgenomen uit `output$legendUI` in maptool_v4: gekleurde blokjes van hoog
#' naar laag, met pijlen en "Meer"/"Minder" verticaal ernaast.
#' @param palette_data Uitvoer van [ggz_palette()].
#' @param uitkomst Sleutel uit [GGZ_UITKOMSTEN], voor de opmaak van de labels.
#' @return Een `shiny::HTML()`-blok.
ggz_legend_html <- function(palette_data, uitkomst) {
  if (isTRUE(palette_data$error) || is.null(palette_data$palette)) {
    msg <- palette_data$message %||% "Geen data beschikbaar"
    return(shiny::HTML(sprintf(
      "<div style='text-align:center; padding:20px; color:#666;'>&#9888;<br>%s</div>", msg)))
  }

  bins <- palette_data$bins
  pal  <- palette_data$palette
  is_index <- ggz_is_index(uitkomst)

  # Een kaart van een enkel vlak heeft geen kleurenschaal om af te lezen; daar
  # is de waarde zelf de hele legenda.
  if (isTRUE(palette_data$enkel)) {
    return(shiny::HTML(sprintf(
      paste0("<div style='display:flex; flex-direction:column; align-items:center;",
             " justify-content:center; height:100%%; text-align:center;'>",
             "<div style='background-color:%s; width:26px; height:26px;",
             " border:1px solid rgba(0,0,0,0.2); margin-bottom:8px;'></div>",
             "<div style='font-size:13px; font-weight:bold;'>%s</div>",
             "<div style='font-size:11px; color:#524F50; margin-top:6px;'>",
             "Een gebied, dus geen schaal</div></div>"),
      pal(palette_data$waarde),
      ggz_format_value(palette_data$waarde, uitkomst))))
  }

  meer   <- if (is_index) "Meer dan verwacht" else "Meer"
  minder <- if (is_index) "Minder dan verwacht" else "Minder"

  digits <- ggz_bin_digits(bins, uitkomst)
  labels <- ggz_format_value(head(bins, -1), uitkomst, digits = digits)
  kleuren <- vapply(seq_len(length(bins) - 1),
                    function(i) pal((bins[i] + bins[i + 1]) / 2), character(1))

  # De kleurvlakken rekken mee via `align-self: stretch`, niet via `height: 100%`.
  # Een percentagehoogte moet zich verhouden tot een ouder met een *definitieve*
  # hoogte, en die keten loopt hier door een div die Shiny zelf om de uitvoer
  # heen zet. Lost die keten niet op, dan vallen alle vlakken terug op nul
  # hoogte en houd je acht randen van 1px over: samen een zwarte streep in
  # plaats van een legenda. De min-height is de laatste vangnetregel.
  blokjes <- paste(vapply(rev(seq_along(kleuren)), function(i) {
    sprintf(paste0("<div style='flex:1 1 0; display:flex; align-items:stretch;",
                   " min-height:16px;'>",
                   "<div style='background-color:%s; width:20px; flex:0 0 20px;",
                   " align-self:stretch; border:1px solid rgba(0,0,0,0.15);",
                   " box-sizing:border-box;'></div>",
                   "<span style='margin-left:8px; font-size:12px;",
                   " align-self:center;'>%s</span></div>"),
            kleuren[i], labels[i])
  }, character(1)), collapse = "")

  shiny::HTML(paste0(
    "<div style='height:100%; min-height:240px; display:flex;",
    " flex-direction:row; align-items:stretch;'>",
    "<div style='display:flex; flex-direction:column; align-items:center;",
    " justify-content:flex-start; margin-right:8px;'>",
    "<div style='width:0; height:0; border-left:5px solid transparent;",
    " border-right:5px solid transparent; border-bottom:10px solid black;",
    " margin-bottom:5px;'></div>",
    "<div style='writing-mode:vertical-rl; transform:rotate(180deg);",
    " text-align:center; font-size:13px; font-weight:bold;'>", meer, "</div></div>",
    "<div style='display:flex; flex-direction:column; flex:1 1 auto;",
    " align-self:stretch;'>", blokjes, "</div>",
    "<div style='display:flex; flex-direction:column; align-items:center;",
    " justify-content:flex-end; margin-left:8px;'>",
    "<div style='writing-mode:vertical-rl; text-align:center; font-size:13px;",
    " font-weight:bold;'>", minder, "</div>",
    "<div style='width:0; height:0; border-left:5px solid transparent;",
    " border-right:5px solid transparent; border-top:10px solid black;",
    " margin-top:5px;'></div></div></div>"
  ))
}

#' Kies de kaartlaag die bij een weergave hoort.
#'
#' @param geo Lijst uit [ggz_load_geo()].
#' @param weergave Sleutel uit [GGZ_WEERGAVEN].
#' @param geo_jaar Geo-jaar.
#' @return Een sf-object met een kolom `gebied`.
ggz_geo_layer <- function(geo, weergave, geo_jaar) {
  switch(weergave,
    "nl_provincie" = geo$provincie[geo$provincie$jaar == geo_jaar, ],
    "nhfl_provincies" = {
      pv <- geo$provincie[geo$provincie$jaar == geo_jaar, ]
      pv[pv$gebied %in% NHFL_PROVINCIES, ]
    },
    "nl_nhfl_rest" = geo$regio[geo$regio$jaar == geo_jaar &
                                 geo$regio$indeling == "nh_fl_vs_rest", ],
    "nhfl_totaal"  = geo$regio[geo$regio$jaar == geo_jaar &
                                 geo$regio$indeling == "nh_fl_totaal", ],
    "nhfl_gemeente" = {
      gm <- geo$gemeente[geo$gemeente$jaar == geo_jaar &
                           geo$gemeente$provincie %in% NHFL_PROVINCIES, ]
      gm$gebied <- gm$code
      gm
    },
    "gemeente" = {
      gm <- geo$gemeente[geo$gemeente$jaar == geo_jaar, ]
      gm$gebied <- gm$code
      gm
    },
    stop("Onbekende weergave: ", weergave)
  )
}

#' Het kaartbeeld: het gebied dat getekend wordt, precies passend.
#'
#' Afgeleid uit de bounding box van de laag zelf, niet uit een vast zoomniveau.
#' Zo vult zowel heel Nederland als alleen Noord-Holland/Flevoland het beschikbare
#' vlak, ongeacht de schermbreedte.
#' @param geo_layer sf-laag uit [ggz_geo_layer()].
#' @return Lijst met `lng1`, `lat1`, `lng2`, `lat2`.
ggz_map_bounds <- function(geo_layer) {
  bb <- suppressWarnings(sf::st_bbox(geo_layer))
  list(lng1 = unname(bb["xmin"]), lat1 = unname(bb["ymin"]),
       lng2 = unname(bb["xmax"]), lat2 = unname(bb["ymax"]))
}

#' Voeg de leesbare gebiedsnaam toe aan een geaggregeerd frame.
#'
#' Bij de gemeenteweergave is `gebied` een gemeentecode; die wordt hier vervangen
#' door de gemeentenaam uit de kaartlaag. Bij de overige weergaven is `gebied`
#' al een naam.
ggz_add_names <- function(agg, geo_layer) {
  if (!"gemeentenaam" %in% names(geo_layer)) {
    agg$gebiedsnaam <- agg$gebied
    return(agg)
  }
  namen <- sf::st_drop_geometry(geo_layer)[, c("gebied", "gemeentenaam")]
  agg <- merge(agg, namen, by = "gebied", all.x = TRUE)
  agg$gebiedsnaam <- ifelse(is.na(agg$gemeentenaam), agg$gebied, agg$gemeentenaam)
  agg$gemeentenaam <- NULL
  agg
}

#' Bouw de leaflet-kaart.
#'
#' @param geo_layer sf-laag uit [ggz_geo_layer()].
#' @param agg Geaggregeerde cijfers met kolommen `gebied` en de uitkomsten.
#' @param uitkomst Sleutel uit [GGZ_UITKOMSTEN].
#' @param palette_data Uitvoer van [ggz_palette()].
ggz_leaflet <- function(geo_layer, agg, uitkomst, palette_data) {
  dat <- merge(geo_layer, agg[, c("gebied", "gebiedsnaam", uitkomst)],
               by = "gebied", all.x = TRUE)
  dat <- sf::st_as_sf(dat)
  waarden <- dat[[uitkomst]]

  bounds <- ggz_map_bounds(dat)
  pal <- palette_data$palette

  vulkleur <- if (is.null(pal)) {
    rep(GGZ_KLEUR_GEEN_DATA, nrow(dat))
  } else {
    ifelse(is.na(waarden), GGZ_KLEUR_GEEN_DATA, pal(waarden))
  }

  labels <- sprintf("<b>%s</b><br>%s: %s",
                    dat$gebiedsnaam,
                    ggz_label(uitkomst, GGZ_UITKOMSTEN),
                    ggz_format_value(waarden, uitkomst))

  leaflet::leaflet(dat, options = leaflet::leafletOptions(zoomControl = TRUE)) %>%
    leaflet::fitBounds(bounds$lng1, bounds$lat1, bounds$lng2, bounds$lat2) %>%
    leaflet::addPolygons(
      color = "#444444",
      weight = 0.8,
      fillOpacity = 1,
      fillColor = vulkleur,
      label = lapply(labels, shiny::HTML),
      labelOptions = leaflet::labelOptions(direction = "auto"),
      layerId = ~gebied,
      highlightOptions = leaflet::highlightOptions(weight = 2.5, color = "#000000",
                                                   bringToFront = TRUE)
    ) %>%
    htmlwidgets::onRender(
      "function(el, x) { this.getContainer().style.backgroundColor = 'white'; }")
}

#' Schrijf een ggplot naar een PNG met een echt transparante achtergrond.
#'
#' `ggsave(..., bg = "transparent")` is niet overal genoeg: "transparent" is
#' de kleur RGB (0,0,0) met alpha 0, en op een grafisch apparaat dat geen
#' semi-transparantie ondersteunt laat R de alpha stilzwijgend vallen -- dan
#' wordt "transparant" gewoon opaak ZWART. Dat gebeurt niet bij elke
#' R-installatie: op een werkplek met een volledige Cairo-build (zoals hier)
#' werkt `bg = "transparent"` gewoon, maar op een minimale Docker-server zonder
#' `libcairo2` kan diezelfde aanroep alles -- achtergrond, legendavlak, en de
#' vlakken rond elk legendablokje -- als zwart wegzetten, waardoor de hele
#' kaart en legenda te donker ogen. Zie PLAN.md voor waar dit gemeld is.
#'
#' `ragg::agg_png` is de enige hier vertrouwde weg: het is een op zichzelf
#' staande renderer (geen systeembibliotheek nodig, dus onafhankelijk van wat
#' er op een specifieke server toevallig geinstalleerd is) en getest op
#' precies dit punt. Een voor de hand liggend alternatief,
#' `grDevices::png(type = "cairo")`, is bewust NIET als tussenstap opgenomen:
#' op de ontwikkelmachine waar dit geschreven is -- met `capabilities("cairo")
#' == TRUE` -- leverde die aanroep via `ggsave()` een RGB-PNG zonder
#' alphakanaal op, dus zonder enige transparantie. Zo'n stap zou de illusie
#' van een vangnet geven zonder er een te zijn.
#'
#' Is `ragg` niet geinstalleerd, dan valt deze functie terug op gewone
#' `ggsave()` en waarschuwt ze expliciet: op een systeem zonder goede
#' alpha-ondersteuning kan "transparant" dan als opaak zwart wegschrijven --
#' precies het gemelde probleem, waarbij niet alleen de achtergrond maar ook
#' de legenda en de kaartkleuren te donker oogden.
#'
#' @param file Doelbestand.
#' @param plot ggplot-object.
#' @param width,height Afmetingen in inches.
#' @param dpi Resolutie.
ggz_ggsave_transparent <- function(file, plot, width, height, dpi) {
  if (requireNamespace("ragg", quietly = TRUE)) {
    ggplot2::ggsave(file, plot, device = ragg::agg_png, width = width,
                    height = height, dpi = dpi, units = "in", bg = "transparent")
  } else {
    warning("Package 'ragg' niet beschikbaar -- de transparante achtergrond ",
            "kan op dit systeem als opaak zwart wegschrijven, met te donker ",
            "ogende kleuren tot gevolg. Installeer 'ragg' om dit te garanderen: ",
            "install.packages('ragg').")
    ggplot2::ggsave(file, plot, device = "png", width = width, height = height,
                    dpi = dpi, units = "in", bg = "transparent")
  }
}

#' Statische kaart voor de PNG-download.
#'
#' Losse ggplot-versie van dezelfde kaart, met titel, ondertitel en bronvermelding
#' erin gebrand -- overgenomen uit `output$downloadMap` in maptool_v4. Handig om
#' zo in een deck of memo te plakken.
#'
#' @param titel,ondertitel Teksten boven de kaart.
#' @param bron Bronregel rechtsonder.
#' @return Een ggplot-object.
ggz_static_map <- function(geo_layer, agg, uitkomst, palette_data, titel,
                           ondertitel, bron = "Bron: ahti — CBS-microdata, NL-output") {
  dat <- merge(geo_layer, agg[, c("gebied", "gebiedsnaam", uitkomst)],
               by = "gebied", all.x = TRUE)
  dat <- sf::st_as_sf(dat)

  bins <- palette_data$bins
  pal  <- palette_data$palette
  if (is.null(bins) || length(bins) < 2 || is.null(pal)) {
    stop("Onvoldoende data om de kaart te exporteren.")
  }

  dat$klasse <- cut(dat[[uitkomst]], breaks = bins, include.lowest = TRUE, dig.lab = 6)

  midden <- vapply(seq_len(length(bins) - 1),
                   function(i) mean(c(bins[i], bins[i + 1])), numeric(1))
  kleuren <- pal(midden)
  niveaus <- levels(dat$klasse)
  if (length(kleuren) != length(niveaus)) kleuren <- rep_len(kleuren, length(niveaus))
  names(kleuren) <- niveaus

  # Legendalabels in dezelfde notatie als de kaart zelf, niet de ruwe
  # intervalnotatie die cut() oplevert. Het aantal decimalen komt uit
  # ggz_bin_digits(), zodat de klassen van een kleine uitkomst niet allemaal
  # dezelfde tekst krijgen.
  digits <- ggz_bin_digits(bins, uitkomst)
  legenda_labels <- if (isTRUE(palette_data$enkel)) {
    # Bij een enkel gebied zijn de klassegrenzen een verzonnen bandje rond de
    # waarde (zie ggz_palette()); toon dan de waarde zelf.
    setNames(ggz_format_value(palette_data$waarde, uitkomst), niveaus)
  } else {
    setNames(
      paste0(ggz_format_value(head(bins, -1), uitkomst, digits = digits), " – ",
             ggz_format_value(bins[-1], uitkomst, digits = digits)),
      niveaus
    )
  }

  ggplot2::ggplot(dat) +
    # key_glyph = "rect" tekent rechthoekige legendablokjes. Zonder dat leidt
    # geom_sf() elk blokje af uit de getekende gebieden, en blijft een klasse
    # waar toevallig geen enkel gebied in valt zonder blokje achter -- een gat in
    # een verder doorlopende kleurenschaal. (show.legend = "polygon" doet
    # hetzelfde, maar loopt in ggplot2 4.0.1 stuk op precies die lege klasse.)
    ggplot2::geom_sf(ggplot2::aes(fill = .data$klasse), color = "#444444",
                     linewidth = 0.15, key_glyph = "rect") +
    ggplot2::scale_fill_manual(
      values = kleuren,
      # Gebieden zonder waarde zijn hier onderdrukte gebieden, niet ontbrekende
      # geografie; een kale "NA" in de legenda zegt de lezer dat niet.
      labels = function(x) ifelse(is.na(x), "Te weinig waarnemingen",
                                  legenda_labels[as.character(x)]),
      na.value = GGZ_KLEUR_GEEN_DATA,
      drop = FALSE,
      name = ggz_label(uitkomst, GGZ_UITKOMSTEN),
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::labs(title = titel, subtitle = ondertitel, caption = bron) +
    ggplot2::theme_void() +
    ggplot2::theme(
      # Titel en ondertitel uitlijnen op de hele figuur, niet op het kaartvlak:
      # gecentreerd op het paneel loopt een lange titel (domein plus uitkomst)
      # links buiten beeld, omdat het paneel smaller is dan de figuur.
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title    = ggplot2::element_text(size = 15, face = "bold", hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 11, hjust = 0, color = "#524F50"),
      plot.caption  = ggplot2::element_text(size = 9, hjust = 1, color = "#524F50"),
      legend.position = "right",
      legend.title  = ggplot2::element_text(size = 11, face = "bold"),
      legend.text   = ggplot2::element_text(size = 9),
      plot.margin   = ggplot2::margin(16, 16, 16, 16),
      # Transparante achtergrond, zodat de kaart op een gekleurde dia geplakt
      # kan worden zonder wit blok eromheen. ggsave() krijgt bg = "transparent"
      # mee; zonder deze theme-instellingen tekent ggplot alsnog witte vlakken.
      plot.background   = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.key        = ggplot2::element_rect(fill = "transparent", colour = NA)
    )
}
