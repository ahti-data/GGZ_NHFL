# 03_build_gemeente_provincie.R
#
# Bouwt de gemeente -> provincie lookup voor het kickoff-dashboard.
#
# Bron: de officiele CBS-tabel "Gebieden in Nederland", per jaar. Die tabellen
# staan al in de organisatie (infectieziekten_monitor_prod), dus ze worden hier
# hergebruikt in plaats van opnieuw gedownload. Zie PLAN.md par. 4.2.
#
# Formaatdetails van de bron:
#   - CSV met ';' als scheidingsteken, UTF-8 met BOM;
#   - gemeentecodes staan als "GM0358    " -- prefix "GM" eraf en trimmen, dan
#     sluiten ze aan op de 4-cijferige `code` in geo_municipalities.rds;
#   - provincienamen zijn met spaties opgevuld tot vaste breedte.
#
# Uitvoer: dashboard/data/gemeente_provincie.csv met kolommen jaar, code, provincie.
#
# Eenmalig te draaien vanuit de repo-root:
#   Rscript output_src/03_build_gemeente_provincie.R

suppressMessages({
  library(dplyr)
})

# Geo-jaren die het dashboard nodig heeft. Bij dataversie v2 hoort geo-jaar =
# datajaar - 1 (v2 gebruikt T-1 demografie), dus datajaren 2022-2024 vragen om
# geografie 2021-2023.
GEO_YEARS <- c(2021L, 2022L, 2023L)

GEBIEDEN_DIR <- normalizePath(
  file.path("..", "infectieziekten_monitor_prod", "infectieziekten_monitor", "data", "input"),
  mustWork = FALSE
)

OUT_FILE <- file.path("dashboard", "data", "gemeente_provincie.csv")

#' Zoek het bronbestand voor een geo-jaar.
#'
#' De bestandsnamen dragen een exportdatum ("Gebieden_in_Nederland_2023_10032026_102956.csv"),
#' dus er wordt op jaar gematcht, niet op een volledige naam.
find_gebieden_file <- function(year, dir = GEBIEDEN_DIR) {
  hits <- list.files(dir, pattern = sprintf("^Gebieden_in_Nederland_%d_.*\\.csv$", year),
                     full.names = TRUE)
  if (length(hits) == 0) {
    stop("Geen 'Gebieden in Nederland'-bestand gevonden voor ", year, " in ", dir)
  }
  # Bij meerdere exports: de nieuwste (namen sorteren chronologisch binnen een jaar).
  sort(hits, decreasing = TRUE)[[1]]
}

#' Lees een 'Gebieden in Nederland'-CSV en geef jaar/code/provincie terug.
read_gebieden <- function(path, year) {
  raw <- read.csv2(path, fileEncoding = "UTF-8-BOM", check.names = FALSE,
                   colClasses = "character")

  gm_col <- grep("gemeenten/Code", names(raw), value = TRUE)[1]
  pv_col <- grep("Provincies/Naam", names(raw), value = TRUE)[1]
  if (is.na(gm_col) || is.na(pv_col)) {
    stop("Verwachte kolommen niet gevonden in ", basename(path),
         " -- gevonden kolommen: ", paste(names(raw), collapse = " | "))
  }

  out <- tibble::tibble(
    jaar      = as.integer(year),
    code      = sub("^GM", "", trimws(raw[[gm_col]])),
    provincie = trimws(raw[[pv_col]])
  )

  # Lege regels en gebieden zonder provincie (bv. het landtotaal) vallen af.
  out <- out[nzchar(out$code) & nzchar(out$provincie), , drop = FALSE]

  dupes <- out$code[duplicated(out$code)]
  if (length(dupes)) {
    stop("Gemeentecode(s) meer dan een keer in ", basename(path), ": ",
         paste(unique(dupes), collapse = ", "))
  }
  out
}

lookup <- dplyr::bind_rows(lapply(GEO_YEARS, function(y) {
  f <- find_gebieden_file(y)
  message("  ", y, ": ", basename(f))
  read_gebieden(f, y)
}))

# --- Controle tegen de geografie die we werkelijk gaan tekenen -----------------
# Elke gemeente in de kaartlaag moet precies een provincie krijgen; anders valt
# er stilzwijgend een gebied uit een provincietotaal.
geo_path <- file.path("..", "maptool_v4", "data", "geo_municipalities.rds")
if (file.exists(geo_path)) {
  gm <- readRDS(geo_path)
  for (y in GEO_YEARS) {
    geo_codes <- unique(gm$code[gm$jaar == y])
    lk_codes  <- lookup$code[lookup$jaar == y]
    missing   <- setdiff(geo_codes, lk_codes)
    if (length(missing)) {
      stop("Geo-jaar ", y, ": ", length(missing),
           " gemeente(n) zonder provincie in de lookup: ",
           paste(head(missing, 10), collapse = ", "))
    }
    message("  ", y, ": ", length(geo_codes), " gemeenten, ",
            dplyr::n_distinct(lookup$provincie[lookup$jaar == y]), " provincies, 0 zonder match")
  }
} else {
  warning("geo_municipalities.rds niet gevonden op ", geo_path,
          " -- controle tegen de kaartlaag overgeslagen.")
}

dir.create(dirname(OUT_FILE), recursive = TRUE, showWarnings = FALSE)
write.csv(lookup, OUT_FILE, row.names = FALSE, fileEncoding = "UTF-8")
message("Geschreven: ", OUT_FILE, " (", nrow(lookup), " rijen)")
