#' GGZ ZPM -- data laden, onderdrukken en aggregeren
#'
#' De NL-output database bewaart cijfers uitsluitend op gemeente-, wijk- en
#' buurtniveau. Alles wat het kickoff-dashboard boven gemeenteniveau toont
#' (provincies, NH/FL versus de rest, NH/FL als geheel) wordt hier berekend.
#'
#' Zie PLAN.md par. 5 voor de onderbouwing van de formules.

suppressMessages({
  library(dplyr)
})

# ---- Vaste keuzelijsten ------------------------------------------------------

#' De vier GGZ ZPM-domeinen, als UI-label -> outcome_type in de database.
GGZ_DOMEINEN <- c(
  "GGZ ZPM (totaal)"          = "zvwggzzpmtotaal",
  "GGZ ZPM - Consult"         = "sub_zvwggzzpmconsult",
  "GGZ ZPM - Overig prestaties" = "sub_zvwggzzpmoverigprest",
  "GGZ ZPM - Verblijf"        = "sub_zvwggzzpmverblijf"
)

GGZ_JAREN <- c(2022L, 2023L, 2024L)

#' De zeven gevraagde uitkomstmaten, als UI-label -> interne sleutel.
#'
#' `index_kosten_pg` en `index_kosten_pc` staan er allebei in omdat nog niet
#' vaststaat welke van de twee de collega met "index kosten" bedoelt (PLAN.md
#' par. 7, vraag 3). Zodra dat duidelijk is kan er een weg.
GGZ_UITKOMSTEN <- c(
  "Relatief aantal (aandeel gebruikers)" = "relatief_aantal",
  "Totaal aantal gebruikers"             = "totaal_aantal",
  "Kosten totaal"                        = "kosten_totaal",
  "Kosten per gebruiker"                 = "kosten_per_gebruiker",
  "Kosten per capita"                    = "kosten_per_capita",
  "Index gebruik"                        = "index_gebruik",
  "Index kosten (p.g.)"                  = "index_kosten_pg",
  "Index kosten (p.c.)"                  = "index_kosten_pc"
)

#' De vijf kaartweergaven die de collega vroeg.
GGZ_WEERGAVEN <- c(
  "Nederland - provincies"              = "nl_provincie",
  "Nederland - NH/FL vs. rest"          = "nl_nhfl_rest",
  "Noord-Holland & Flevoland - geheel"  = "nhfl_totaal",
  "Noord-Holland & Flevoland - provincies" = "nhfl_provincies",
  "Noord-Holland & Flevoland - gemeenten"  = "nhfl_gemeente"
)

GGZ_POPULATIES <- c(
  "Totale populatie" = "pop",
  "0-17 jaar"        = "sub18",
  "18-65 jaar"       = "18-65",
  "65+ jaar"         = "65+"
)

NHFL_PROVINCIES <- c("Noord-Holland", "Flevoland")

#' Uitkomsten die een verhouding zijn en dus nooit met n vermenigvuldigd worden.
GGZ_INDEX_UITKOMSTEN <- c("index_gebruik", "index_kosten_pg", "index_kosten_pc")

# ---- Geo-jaar ----------------------------------------------------------------

#' Geo-jaar bij een datajaar.
#'
#' Dataversie v2 rekent met T-1 demografie, dus daar hoort de CBS-geografie van
#' het jaar ervoor bij. v1 gebruikt dezelfde-jaar geografie. Overgenomen uit
#' `geo_year()` in maptool_v4/app.R.
#' @param data_year Datajaar.
#' @param version `"v1"` of `"v2"`.
ggz_geo_year <- function(data_year, version = "v2") {
  data_year <- as.integer(data_year)
  if (identical(as.character(version), "v2")) data_year - 1L else data_year
}

# ---- Laden -------------------------------------------------------------------

#' Waar de dashboarddata vandaan komt.
#'
#' De echte extract heet `ggz_zpm_gemeente.rds` (zie
#' `output_src/01_extract_ggz_zpm.R`). Zolang die er niet is -- de database is
#' niet vanaf elke werkplek bereikbaar -- valt het dashboard terug op het
#' synthetische bestand, en zet [ggz_is_synthetic()] een waarschuwing in de UI.
GGZ_DATA_FILE           <- file.path("data", "ggz_zpm_gemeente.rds")
GGZ_DATA_FILE_SYNTHETIC <- file.path("data", "ggz_zpm_gemeente_synthetic.rds")

#' Draait het dashboard op verzonnen cijfers?
#' @return `TRUE` als het echte extract ontbreekt en de synthetische stand-in
#'   gebruikt wordt.
ggz_is_synthetic <- function() {
  !file.exists(GGZ_DATA_FILE) && file.exists(GGZ_DATA_FILE_SYNTHETIC)
}

#' Lees de GGZ ZPM-cijfers op gemeenteniveau.
#' @return Data frame met een rij per gemeente x jaar x versie x populatie x domein.
ggz_load_data <- function() {
  path <- if (file.exists(GGZ_DATA_FILE)) GGZ_DATA_FILE else GGZ_DATA_FILE_SYNTHETIC
  if (!file.exists(path)) {
    stop("Geen dataset gevonden. Draai output_src/01_extract_ggz_zpm.R ",
         "(of, zonder databasetoegang, output_src/00_build_synthetic_data.R).")
  }
  readRDS(path)
}

#' Lees de kaartlagen (zie output_src/02_build_geo_assets.R).
#' @return Lijst met `gemeente`, `provincie` en `regio` als sf-objecten.
ggz_load_geo <- function() {
  list(
    gemeente  = readRDS(file.path("data", "geo_gemeente.rds")),
    provincie = readRDS(file.path("data", "geo_provincie.rds")),
    regio     = readRDS(file.path("data", "geo_regio.rds"))
  )
}

# ---- Voorbereiden ------------------------------------------------------------

#' Reken de opgeslagen measures om naar optelbare grootheden.
#'
#' De database bewaart aandelen, gemiddelden en indexcijfers. Die zijn geen van
#' alle optelbaar over gemeenten heen. Deze functie zet ze om naar de grootheden
#' die dat wel zijn -- aantallen gebruikers en totale kosten, geobserveerd en
#' verwacht -- zodat [ggz_aggregate()] kan sommeren.
#'
#' De verwachte waarden komen rechtstreeks uit de `comp_*`-kolommen. De NL-output
#' bewaart die naast de geobserveerde `target_*`-waarden; de maptool zelf tekent
#' ze als "Verwacht" tegenover "Geobserveerd" en berekent de index als
#' `target / comp`.
#'
#' Ontbreken de `comp_*`-kolommen (een ouder extract), dan valt de functie terug
#' op `verwacht = geobserveerd / index`. Dat is wiskundig hetzelfde maar
#' onnauwkeuriger en onvollediger: `index_*` is voor aanzienlijk minder rijen
#' gevuld dan `comp_*`, dus die route verliest gebieden.
#'
#' @param data Data frame uit [ggz_load_data()].
#' @return Hetzelfde frame met de kolommen `gebruikers`, `kosten_totaal`,
#'   `verwacht_gebruikers`, `verwacht_kosten_pc` en `verwacht_kosten_pg_totaal`.
ggz_prepare <- function(data) {
  safe_div <- function(x, idx) ifelse(is.na(idx) | idx == 0, NA_real_, x / idx)

  # Ontbreken de comp-kolommen, dan worden ze eerst gereconstrueerd uit
  # target / index. Daarna loopt alles langs dezelfde formule. Dat is bewust:
  # een aparte rekenweg voor de terugvaloptie ging eerder mis doordat de teller
  # dan met geobserveerde en de noemer met verwachte gebruikers gewogen werd --
  # twee verschillende noemers in een verhouding die er een hoort te hebben.
  if (!all(c("comp_USE", "comp_COSTS", "comp_PCCOSTS") %in% names(data))) {
    data <- data %>%
      mutate(
        comp_USE     = safe_div(.data$target_USE, .data$index_USE),
        comp_COSTS   = safe_div(.data$target_COSTS, .data$index_COSTS),
        comp_PCCOSTS = safe_div(.data$target_PCCOSTS, .data$index_PCCOSTS)
      )
  }

  data %>%
    mutate(
      gebruikers          = .data$target_USE * .data$n,
      kosten_totaal       = .data$target_PCCOSTS * .data$n,
      verwacht_gebruikers = .data$comp_USE * .data$n,
      verwacht_kosten_pc  = .data$comp_PCCOSTS * .data$n,
      # De verwachting voor kosten *per gebruiker* heeft een andere noemer dan
      # die voor kosten per inwoner: het verwachte kostentotaal dat hoort bij de
      # verwachte kosten per gebruiker maal het verwachte aantal gebruikers.
      # Alleen zo deelt [ggz_aggregate()] straks teller en noemer die bij elkaar
      # horen.
      verwacht_kosten_pg_totaal = .data$comp_COSTS * .data$comp_USE * .data$n
    )
}

#' Onderdruk gebieden met te weinig waarnemingen.
#'
#' Dezelfde regel als de knop "verwijder gemeenten met observaties kleiner dan"
#' in maptool_v4, maar hier standaard aan. Op gemeenteniveau kan een
#' deelprestatie als GGZ ZPM - Verblijf in kleine gemeenten onder de
#' CBS-drempel voor herleidbaarheid uitkomen; die gebieden mogen niet
#' individueel zichtbaar zijn.
#'
#' Onderdrukte gemeenten worden op NA gezet en tellen niet mee in een
#' aggregatie -- zie de kanttekening in [ggz_aggregate()].
#'
#' @param data Data frame uit [ggz_prepare()].
#' @param drempel `"Nee"`, `"<30"` of `"<50"`.
#' @return Hetzelfde frame met een extra kolom `onderdrukt` (logical); bij
#'   onderdrukte rijen zijn de waardekolommen NA.
ggz_suppress <- function(data, drempel = "<30") {
  cutoff <- switch(as.character(drempel), "<30" = 30, "<50" = 50, 0)

  out <- data %>% mutate(onderdrukt = !is.na(.data$gebruikers) & .data$gebruikers < cutoff)
  if (cutoff == 0) return(out %>% mutate(onderdrukt = FALSE))

  waarde_cols <- c("gebruikers", "kosten_totaal", "verwacht_gebruikers",
                   "verwacht_kosten_pg_totaal", "verwacht_kosten_pc",
                   "comp_USE", "comp_COSTS", "comp_PCCOSTS",
                   "target_USE", "target_COSTS", "target_PCCOSTS",
                   "index_USE", "index_COSTS", "index_PCCOSTS")
  waarde_cols <- intersect(waarde_cols, names(out))

  out[out$onderdrukt, waarde_cols] <- NA_real_
  out
}

# ---- Aggregeren --------------------------------------------------------------

#' Aggregeer gemeentecijfers naar het gevraagde gebiedsniveau.
#'
#' Let op de indices: een bevolkingsgewogen gemiddelde van indexcijfers is
#' *fout*. Een index is een verhouding geobserveerd/verwacht en moet als
#' verhouding van sommen herberekend worden -- vandaar dat [ggz_prepare()]
#' eerst terugrekent naar geobserveerde en verwachte aantallen, hier gesommeerd
#' wordt, en de index pas daarna opnieuw gedeeld wordt.
#'
#' Dit steunt op de aanname dat de NL-output-pipeline de index multiplicatief
#' definieert als geobserveerd/verwacht (PLAN.md par. 7, vraag 4). Klopt die
#' aanname niet, dan zijn de indexcijfers boven gemeenteniveau niet bruikbaar;
#' de overige vijf uitkomsten staan er los van.
#'
#' Onderdrukte gemeenten (zie [ggz_suppress()]) vallen uit de som. Voor een
#' provincietotaal is dat een lichte onderschatting; `n_onderdrukt` in de
#' uitvoer laat zien om hoeveel gebieden het gaat.
#'
#' @param data Data frame uit [ggz_prepare()], meestal via [ggz_suppress()].
#' @param by Gebiedsindeling: een van de waarden van [GGZ_WEERGAVEN], of
#'   `"gemeente"` voor geen aggregatie.
#' @param provincie_lookup Data frame met `code`, `jaar` en `provincie`.
#' @param geo_jaar Het geo-jaar waarvan de gemeente-indeling gebruikt wordt.
#' @return Data frame met een rij per gebied en de zeven uitkomsten als kolommen.
ggz_aggregate <- function(data, by, provincie_lookup, geo_jaar) {
  lk <- provincie_lookup %>%
    filter(.data$jaar == geo_jaar) %>%
    select("code", "provincie")

  dat <- data %>% inner_join(lk, by = "code")

  # Welk gebiedslabel hoort bij deze weergave, en welke gemeenten doen mee?
  dat <- switch(by,
    "gemeente" = ,
    "nhfl_gemeente" = dat %>%
      filter(if (by == "nhfl_gemeente") .data$provincie %in% NHFL_PROVINCIES else TRUE) %>%
      mutate(gebied = .data$code),
    "nl_provincie" = dat %>% mutate(gebied = .data$provincie),
    "nhfl_provincies" = dat %>%
      filter(.data$provincie %in% NHFL_PROVINCIES) %>%
      mutate(gebied = .data$provincie),
    "nl_nhfl_rest" = dat %>%
      mutate(gebied = ifelse(.data$provincie %in% NHFL_PROVINCIES,
                             "Noord-Holland & Flevoland", "Overig Nederland")),
    "nhfl_totaal" = dat %>%
      filter(.data$provincie %in% NHFL_PROVINCIES) %>%
      mutate(gebied = "Noord-Holland & Flevoland"),
    stop("Onbekende gebiedsindeling: ", by)
  )

  dat %>%
    group_by(.data$gebied) %>%
    summarise(
      n                   = sum(.data$n, na.rm = TRUE),
      n_gebieden          = dplyr::n(),
      n_onderdrukt        = sum(.data$onderdrukt %||% FALSE, na.rm = TRUE),
      gebruikers          = sum(.data$gebruikers, na.rm = TRUE),
      kosten_totaal_sum   = sum(.data$kosten_totaal, na.rm = TRUE),
      verwacht_gebruikers = sum(.data$verwacht_gebruikers, na.rm = TRUE),
      verwacht_kosten_pg_totaal = sum(.data$verwacht_kosten_pg_totaal, na.rm = TRUE),
      verwacht_kosten_pc  = sum(.data$verwacht_kosten_pc, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Gebieden waar alles onderdrukt is leveren nullen op; die horen NA te zijn.
    mutate(across(c("gebruikers", "kosten_totaal_sum", "verwacht_gebruikers",
                    "verwacht_kosten_pg_totaal", "verwacht_kosten_pc"),
                  ~ ifelse(.x == 0, NA_real_, .x))) %>%
    mutate(
      relatief_aantal      = .data$gebruikers / .data$n,
      totaal_aantal        = .data$gebruikers,
      kosten_totaal        = .data$kosten_totaal_sum,
      kosten_per_capita    = .data$kosten_totaal_sum / .data$n,
      kosten_per_gebruiker = .data$kosten_totaal_sum / .data$gebruikers,
      index_gebruik        = .data$gebruikers        / .data$verwacht_gebruikers,
      index_kosten_pc      = .data$kosten_totaal_sum / .data$verwacht_kosten_pc,
      # Kosten per gebruiker: geobserveerde kosten per gebruiker gedeeld door de
      # verwachte kosten per gebruiker -- elk met zijn eigen noemer, dus niet
      # simpelweg de twee kostentotalen tegen elkaar.
      index_kosten_pg      = (.data$kosten_totaal_sum / .data$gebruikers) /
                             (.data$verwacht_kosten_pg_totaal / .data$verwacht_gebruikers)
    ) %>%
    select(-"kosten_totaal_sum")
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- Presentatie -------------------------------------------------------------

#' Label van een uitkomst, domein of weergave, voor titels en assen.
ggz_label <- function(waarde, keuzes) {
  hit <- names(keuzes)[keuzes == waarde]
  if (length(hit)) hit[[1]] else as.character(waarde)
}

#' Standaard aantal decimalen per soort uitkomst.
GGZ_DECIMALEN <- c(
  relatief_aantal      = 1,
  totaal_aantal        = 0,
  kosten_totaal        = 0,
  kosten_per_gebruiker = 0,
  kosten_per_capita    = 2,
  index_gebruik        = 2,
  index_kosten_pg      = 2,
  index_kosten_pc      = 2
)

#' Formatteer een uitkomstwaarde voor tooltip, legenda en tabel.
#'
#' Aandelen als percentage, kosten in euro's, indices als "1,08x", aantallen als
#' hele getallen met duizendtalscheiding. Nederlandse notatie: komma als
#' decimaalteken, punt als duizendtalscheiding.
#' @param val Numerieke vector.
#' @param uitkomst Sleutel uit [GGZ_UITKOMSTEN].
#' @param digits Aantal decimalen; `NULL` neemt de standaard voor deze uitkomst.
#'   Zie [ggz_bin_digits()] voor waarom de legenda daarvan afwijkt.
ggz_format_value <- function(val, uitkomst, digits = NULL) {
  d <- if (!is.null(digits)) digits else {
    if (uitkomst %in% names(GGZ_DECIMALEN)) GGZ_DECIMALEN[[uitkomst]] else 2
  }
  fmt <- function(x) formatC(x, format = "f", digits = d,
                             big.mark = ".", decimal.mark = ",")
  ifelse(
    is.na(val), "N.v.t.",
    switch(uitkomst,
      "relatief_aantal"      = paste0(fmt(val * 100), "%"),
      "kosten_totaal"        = ,
      "kosten_per_gebruiker" = ,
      "kosten_per_capita"    = paste0("€ ", fmt(val)),
      "index_gebruik"        = ,
      "index_kosten_pg"      = ,
      "index_kosten_pc"      = paste0(fmt(val), "x"),
      fmt(val)
    )
  )
}

#' Hoeveel decimalen heeft de legenda nodig om klassegrenzen te onderscheiden?
#'
#' Met een vast aantal decimalen lopen de labels van een kleine uitkomst in
#' elkaar over: het aandeel gebruikers van GGZ ZPM - Verblijf ligt rond de 0,3%,
#' en op een decimaal nauwkeurig heet elke klasse dan "0,3% - 0,3%". Deze functie
#' zoekt het kleinste aantal decimalen waarbij alle klassegrenzen nog van elkaar
#' verschillen.
#'
#' @param bins Numerieke vector met klassegrenzen.
#' @param uitkomst Sleutel uit [GGZ_UITKOMSTEN].
#' @return Aantal decimalen (hooguit 5).
ggz_bin_digits <- function(bins, uitkomst) {
  bins <- bins[!is.na(bins)]
  standaard <- if (uitkomst %in% names(GGZ_DECIMALEN)) GGZ_DECIMALEN[[uitkomst]] else 2
  if (length(bins) < 2) return(standaard)

  for (d in standaard:5) {
    if (!anyDuplicated(ggz_format_value(bins, uitkomst, digits = d))) return(d)
  }
  5
}

#' Is deze uitkomst een index (en dus rond 1 gecentreerd)?
ggz_is_index <- function(uitkomst) uitkomst %in% GGZ_INDEX_UITKOMSTEN
