# 01_extract_ggz_zpm.R
#
# Haalt de GGZ ZPM-slice uit de NL-output database (PostgreSQL) en schrijft er
# een klein bestand van voor het dashboard. Zie PLAN.md par. 3.
#
# Dit is bewust een smalle uitsnede: alleen level_type 'Gemeente', alleen de
# jaren 2022-2024 en alleen de vier GGZ ZPM-uitkomsten. De maptool-database
# bevat negen zorgdomeinen op drie geografische niveaus; daarvan komt hier
# niets anders mee.
#
# LET OP -- eenmalig te draaien vanaf een machine MET databasetoegang. Vanaf een
# willekeurige werkplek loopt de verbinding af in een timeout: de RDS security
# group laat niet elk IP toe. Credentials komen uit .env in de repo-root
# (zelfde formaat als maptool_v4/.env; .env staat in .gitignore).
#
#   Rscript output_src/01_extract_ggz_zpm.R
#
# Uitvoer: dashboard/data/ggz_zpm_gemeente.rds

suppressMessages({
  library(DBI)
  library(dplyr)
  library(tidyr)
})

YEARS <- c(2022L, 2023L, 2024L)

OUTCOME_TYPES <- c(
  "zvwggzzpmtotaal",           # GGZ ZPM (totaal)
  "sub_zvwggzzpmconsult",      # GGZ ZPM - Consult
  "sub_zvwggzzpmoverigprest",  # GGZ ZPM - Overig prestaties
  "sub_zvwggzzpmverblijf"      # GGZ ZPM - Verblijf
)

# target_* = geobserveerde waarde, comp_* = verwachte waarde, index_* = de
# verhouding daartussen. USE = aandeel gebruikers, COSTS = kosten per gebruiker,
# PCCOSTS = kosten per inwoner.
#
# comp_* wordt meegenomen omdat de verwachte waarden rechtstreeks in de database
# staan; ze hoeven dus niet uit target/index teruggerekend te worden. Dat is
# nauwkeuriger (geen deling, geen NA's als een index ontbreekt) en vollediger:
# comp_* telt 17,1 mln rijen tegen index_* 9,7 mln, dus terugrekenen uit de index
# zou stilzwijgend rijen verliezen.
#
# De maptool zelf bevestigt de betekenis: generate_region_plot() haalt
# 'target_*' en 'comp_*' op, labelt die als "Geobserveerd" en "Verwacht", en
# berekent de index als target_val / comp_val.
MEASURE_TYPES <- c(
  "target_USE", "target_COSTS", "target_PCCOSTS",
  "comp_USE", "comp_COSTS", "comp_PCCOSTS",
  "index_USE", "index_COSTS", "index_PCCOSTS"
)

OUT_FILE <- file.path("dashboard", "data", "ggz_zpm_gemeente.rds")

# --- Verbinding ---------------------------------------------------------------
env_file <- file.path(getwd(), ".env")
if (file.exists(env_file)) readRenviron(env_file)

pg_required <- c("PGHOST", "PGDATABASE", "PGUSER", "PGPASSWORD")
missing_pg <- pg_required[!vapply(pg_required, function(k) nzchar(Sys.getenv(k)), logical(1))]
if (length(missing_pg)) {
  stop("Ontbrekende omgevingsvariabelen: ", paste(missing_pg, collapse = ", "),
       ". Zet ze in .env (zie maptool_v4/.env voor het formaat).")
}

con <- dbConnect(
  RPostgres::Postgres(),
  host     = Sys.getenv("PGHOST"),
  port     = as.integer(Sys.getenv("PGPORT", "5432")),
  dbname   = Sys.getenv("PGDATABASE"),
  user     = Sys.getenv("PGUSER"),
  password = Sys.getenv("PGPASSWORD"),
  sslmode  = Sys.getenv("PGSSLMODE", "require")
)
on.exit(if (dbIsValid(con)) dbDisconnect(con), add = TRUE)

quote_in <- function(x) paste(sprintf("'%s'", x), collapse = ", ")

# --- 1. De uitkomstwaarden ----------------------------------------------------
message("Uitkomsten ophalen...")
values <- dbGetQuery(con, sprintf(
  "SELECT code, year, version, population, measure_type, outcome_type, value
     FROM zorgkosten
    WHERE level_type = 'Gemeente'
      AND year IN (%s)
      AND outcome_type IN (%s)
      AND measure_type IN (%s)",
  paste(YEARS, collapse = ", "), quote_in(OUTCOME_TYPES), quote_in(MEASURE_TYPES)
))
message("  ", nrow(values), " rijen")

# --- 2. De noemer n -----------------------------------------------------------
# In maptool_v4 (app.R regel ~828) wordt `n` altijd zonder outcome_type-filter
# opgehaald: n is de populatieomvang van een gebied, niet van een uitkomst.
# Dat gedrag wordt hier exact gespiegeld.
message("Populatieomvang (n) ophalen...")
n_data <- dbGetQuery(con, sprintf(
  "SELECT DISTINCT code, year, version, population, value AS n
     FROM zorgkosten
    WHERE level_type = 'Gemeente'
      AND year IN (%s)
      AND measure_type = 'n'",
  paste(YEARS, collapse = ", ")
))
message("  ", nrow(n_data), " rijen")

dup <- n_data %>% count(code, year, version, population) %>% filter(n > 1)
if (nrow(dup)) {
  stop("n is niet uniek per code/jaar/versie/populatie -- ", nrow(dup),
       " combinatie(s) met meerdere waarden. Controleer de aanname in PLAN.md par. 3.")
}

# --- 3. Naar breed formaat ----------------------------------------------------
# Een rij per gebied x jaar x versie x populatie x zorgdomein, met de measures als
# kolommen. Dat is de vorm waarin utils/ggz_data.R de zeven uitkomsten berekent.
ggz <- values %>%
  mutate(value = as.numeric(value)) %>%
  pivot_wider(names_from = measure_type, values_from = value) %>%
  left_join(mutate(n_data, n = as.numeric(n)),
            by = c("code", "year", "version", "population")) %>%
  rename(jaar = year) %>%
  arrange(jaar, version, population, outcome_type, code)

for (m in MEASURE_TYPES) {
  if (!m %in% names(ggz)) {
    warning("measure_type '", m, "' kwam niet voor in de resultaten.")
    ggz[[m]] <- NA_real_
  }
}

if (any(is.na(ggz$n))) {
  warning(sum(is.na(ggz$n)), " rij(en) zonder n -- die gebieden kunnen niet ",
          "geaggregeerd worden en vallen in het dashboard weg.")
}

dir.create(dirname(OUT_FILE), recursive = TRUE, showWarnings = FALSE)
saveRDS(ggz, OUT_FILE, compress = "xz")

message("\nGeschreven: ", OUT_FILE)
message("  ", nrow(ggz), " rijen, ", sprintf("%.2f MB", file.size(OUT_FILE) / 1e6))
message("  jaren:      ", paste(sort(unique(ggz$jaar)), collapse = ", "))
message("  versies:    ", paste(sort(unique(ggz$version)), collapse = ", "))
message("  populaties: ", paste(sort(unique(ggz$population)), collapse = ", "))
message("  domeinen:   ", paste(sort(unique(ggz$outcome_type)), collapse = ", "))
message("  gemeenten:  ", dplyr::n_distinct(ggz$code))

# --- 4. Controle: kosten totaal langs twee wegen -------------------------------
# PCCOSTS x n moet gelijk zijn aan COSTS x (USE x n). Wijkt dat af, dan klopt de
# interpretatie van de measures niet (zie PLAN.md par. 5.1).
chk <- ggz %>%
  filter(!is.na(target_PCCOSTS), !is.na(target_COSTS), !is.na(target_USE), !is.na(n)) %>%
  mutate(
    via_pc   = target_PCCOSTS * n,
    via_user = target_COSTS * target_USE * n,
    afwijking = abs(via_pc - via_user) / pmax(abs(via_pc), 1)
  )
if (nrow(chk)) {
  message("
Controle kosten totaal (PCCOSTS*n vs COSTS*USE*n):")
  message("  alle rijen       -- mediaan: ", sprintf("%.4f%%", 100 * median(chk$afwijking)),
          " | p95: ", sprintf("%.4f%%", 100 * quantile(chk$afwijking, 0.95)))

  # Naar alle rijen kijken is misleidend: de afwijking is een afrondingseffect en
  # schaalt omgekeerd met de omvang van het gebied (gemeten op de echte data:
  # 6,2% mediaan onder de 30 gebruikers, 0,05% boven de 1.000). Juist die kleine
  # gebieden worden door de CBS-drempel toch al onderdrukt en komen nooit in
  # beeld. De betekenisvolle toets is die op de gebieden die het dashboard
  # werkelijk toont.
  zichtbaar <- chk[!is.na(chk$target_USE * chk$n) & chk$target_USE * chk$n >= 30, ]
  if (nrow(zichtbaar)) {
    med <- median(zichtbaar$afwijking)
    message("  >= 30 gebruikers -- mediaan: ", sprintf("%.4f%%", 100 * med),
            " | p95: ", sprintf("%.4f%%", 100 * quantile(zichtbaar$afwijking, 0.95)),
            " | p99: ", sprintf("%.4f%%", 100 * quantile(zichtbaar$afwijking, 0.99)))
    if (med > 0.005) {
      warning("Op de zichtbare gebieden lopen de twee routes naar 'kosten totaal' ",
              "meer dan 0,5% uiteen. Dat is meer dan afronding verklaart; ",
              "controleer of COSTS werkelijk kosten per gebruiker is.")
    }
  }
}

# --- 5. Controle: is index werkelijk target / comp? ---------------------------
# Zo ja, dan mag de aggregatie de verwachte waarden optellen en de index pas
# daarna opnieuw delen (zie PLAN.md par. 5.2).
chk2 <- ggz %>%
  filter(!is.na(index_USE), !is.na(target_USE), !is.na(comp_USE), comp_USE > 0) %>%
  mutate(afwijking = abs(index_USE - target_USE / comp_USE) / pmax(abs(index_USE), 1e-9))
if (nrow(chk2)) {
  message("
Controle index_USE vs target_USE/comp_USE op ", nrow(chk2), " rijen:")
  message("  mediane relatieve afwijking: ", sprintf("%.6f%%", 100 * median(chk2$afwijking)))
  if (median(chk2$afwijking) > 0.001) {
    warning("index_USE komt niet overeen met target_USE/comp_USE. De aannames ",
            "achter de aggregatie van indexcijfers kloppen dan niet.")
  }
}
