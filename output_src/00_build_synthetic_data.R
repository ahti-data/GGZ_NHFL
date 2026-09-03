# 00_build_synthetic_data.R
#
# Genereert VOLLEDIG VERZONNEN cijfers in exact het formaat dat
# 01_extract_ggz_zpm.R uit de database haalt, zodat het dashboard gebouwd en
# getest kan worden zolang de database niet bereikbaar is (zie PLAN.md par. 7,
# vraag 1).
#
# Deze cijfers zijn GEEN CBS-data. Ze zijn getrokken uit een verdeling die qua
# ordegrootte plausibel is, maar ze zeggen niets over de werkelijkheid en mogen
# nooit in een deliverable, presentatie of gesprek met een opdrachtgever
# terechtkomen. Het dashboard toont daarom een waarschuwingsbalk zolang het op
# dit bestand draait -- zie GGZ_IS_SYNTHETIC in dashboard/utils/ggz_data.R.
#
#   Rscript output_src/00_build_synthetic_data.R
#
# Uitvoer: dashboard/data/ggz_zpm_gemeente_synthetic.rds

suppressMessages({
  library(dplyr)
})

set.seed(20260903)

YEARS      <- c(2022L, 2023L, 2024L)
VERSIONS   <- c("v2", "v1")
POPULATIONS <- c("pop", "sub18", "18-65", "65+")

DOMEINEN <- c(
  "zvwggzzpmtotaal",
  "sub_zvwggzzpmconsult",
  "sub_zvwggzzpmoverigprest",
  "sub_zvwggzzpmverblijf"
)

# Ruwe ordegroottes per domein: aandeel gebruikers en kosten per gebruiker.
PROFIEL <- list(
  "zvwggzzpmtotaal"          = list(use = 0.072, cost = 2600),
  "sub_zvwggzzpmconsult"     = list(use = 0.065, cost = 1450),
  "sub_zvwggzzpmoverigprest" = list(use = 0.021, cost = 690),
  "sub_zvwggzzpmverblijf"    = list(use = 0.0035, cost = 21000)
)

OUT_FILE <- file.path("dashboard", "data", "ggz_zpm_gemeente_synthetic.rds")

geo <- readRDS(file.path("dashboard", "data", "geo_gemeente.rds"))
gem <- sf::st_drop_geometry(geo) %>% distinct(code, jaar, provincie)

# Datajaar -> geo-jaar is bij v2 een verschuiving van 1; voor het bouwen van een
# testbestand houden we het simpel en gebruiken we per datajaar de gemeenten van
# het bijbehorende v2-geo-jaar.
geo_for_year <- function(y) gem %>% filter(jaar == y - 1L) %>% select(code, provincie)

# Een vaste, per gemeente iets afwijkende "aard" van het gebied, zodat de kaart
# ruimtelijke samenhang vertoont in plaats van pure ruis, en een vaste
# populatieomvang per gemeente x jaar x populatie (die hoort niet per
# zorgdomein te verschillen).
alle_codes <- unique(gem$code)
aard <- setNames(exp(rnorm(length(alle_codes), 0, 0.18)), alle_codes)
n_basis <- setNames(round(exp(rnorm(length(alle_codes), log(25000), 1.05))), alle_codes)

grid <- expand.grid(
  jaar         = YEARS,
  version      = VERSIONS,
  population   = POPULATIONS,
  outcome_type = DOMEINEN,
  stringsAsFactors = FALSE
)

rows <- lapply(seq_len(nrow(grid)), function(i) {
  y <- grid$jaar[i]; v <- grid$version[i]
  p <- grid$population[i]; d <- grid$outcome_type[i]

  g <- geo_for_year(y)
  if (nrow(g) == 0) return(NULL)

  n_tot <- n_basis[g$code]
  n <- switch(p,
    "pop"   = n_tot,
    "sub18" = round(n_tot * 0.20),
    "18-65" = round(n_tot * 0.61),
    "65+"   = round(n_tot * 0.19)
  )

  prof <- PROFIEL[[d]]
  # Deelpopulaties gebruiken de GGZ anders dan de totale bevolking.
  pop_factor <- switch(p, "pop" = 1, "sub18" = 0.75, "18-65" = 1.25, "65+" = 0.45)

  use  <- prof$use * pop_factor * aard[g$code] * exp(rnorm(nrow(g), 0, 0.12))
  cost <- prof$cost * exp(rnorm(nrow(g), 0, 0.14))

  # De index is per constructie geobserveerd/verwacht: dicht bij 1, met
  # spreiding. Zo is de decompositie in ggz_aggregate() ook echt toetsbaar.
  idx_use  <- exp(rnorm(nrow(g), 0, 0.13))
  idx_cost <- exp(rnorm(nrow(g), 0, 0.11))

  tibble::tibble(
    code            = g$code,
    jaar            = as.integer(y),
    version         = v,
    population      = p,
    outcome_type    = d,
    target_USE      = as.numeric(use),
    target_COSTS    = as.numeric(cost),
    target_PCCOSTS  = as.numeric(use * cost),  # per capita = aandeel x kosten p.g.
    index_USE       = as.numeric(idx_use),
    index_COSTS     = as.numeric(idx_cost),
    index_PCCOSTS   = as.numeric(idx_use * idx_cost),
    n               = as.numeric(n)
  )
})

ggz <- bind_rows(rows) %>%
  arrange(jaar, version, population, outcome_type, code)

dir.create(dirname(OUT_FILE), recursive = TRUE, showWarnings = FALSE)
saveRDS(ggz, OUT_FILE, compress = "xz")

message("Geschreven: ", OUT_FILE)
message("  ", nrow(ggz), " rijen, ", sprintf("%.2f MB", file.size(OUT_FILE) / 1e6))
message("  LET OP: verzonnen cijfers, uitsluitend om het dashboard te kunnen draaien.")
