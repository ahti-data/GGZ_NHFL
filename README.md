# GGZ_NHFL

Onderzoek van het Amsterdam health & technology institute (ahti) naar het gebruik en de kosten
van de GGZ onder het zorgprestatiemodel (ZPM) in Noord-Holland en Flevoland, afgezet tegen de
rest van Nederland.

Dit repo bevat op dit moment de **kickoff-versie van het dashboard**: één tabblad met de
kaartweergaven die voor de projectstart gevraagd zijn. Er komen later meer tabbladen bij.

## Wat er in het dashboard zit

Vier GGZ ZPM-domeinen (totaal, consult, overige prestaties, verblijf) over de jaren
2022–2024, met zeven uitkomstmaten, op vijf gebiedsindelingen:

| Kaartweergave | Gebieden |
|---|---|
| Nederland — provincies | 12 |
| Nederland — NH/FL vs. rest | 2 |
| Noord-Holland & Flevoland — geheel | 1 |
| Noord-Holland & Flevoland — provincies | 2 |
| Noord-Holland & Flevoland — gemeenten | 50 (2023) |

Uitkomstmaten: relatief aantal gebruikers, totaal aantal gebruikers, kosten totaal, kosten per
gebruiker, kosten per capita, index gebruik en index kosten.

Bij de kaart horen een PNG-download en een Excel-download van de onderliggende tabel. De
think-cell-export uit het dashboardtemplate blijft beschikbaar in `dashboard/utils/` voor de
grafiektabbladen die hierna komen, maar wordt bij een kaart niet aangeboden.

## Databron

De cijfers komen uit de NL-output database (PostgreSQL), dezelfde bron als de maptool. Daaruit
wordt een smalle uitsnede gehaald: alleen gemeenteniveau, alleen 2022–2024, alleen de vier GGZ
ZPM-uitkomsten. Zie [PLAN.md](PLAN.md) voor de volledige onderbouwing.

Alle cijfers zijn geaggregeerd en niet tot personen herleidbaar. Gebieden met te weinig
gebruikers worden onderdrukt (standaard: minder dan 30), conform de CBS-uitvoerregels.

## Repostructuur

```
output_src/
  00_build_synthetic_data.R      verzonnen cijfers, om zonder DB te kunnen bouwen
  01_extract_ggz_zpm.R           de echte extractie uit PostgreSQL
  02_build_geo_assets.R          kaartlagen uit maptool_v4
  03_build_gemeente_provincie.R  gemeente -> provincie lookup (CBS)
dashboard/
  app.R                          UI en serverlogica
  data/                          extract, kaartlagen, lookup, huisstijl
  utils/ggz_data.R               laden, onderdrukken, aggregeren
  utils/ggz_map.R                bins, legenda, leaflet, PNG-export
  utils/*.R                      think-cell/slide/favorites uit het template
  templates/                     think-cell pptx-templates
  tests/testthat/                testthat-tests
```

## Aan de slag

De drie voorbereidende scripts draaien vanuit de repo-root en hoeven maar één keer:

```bash
Rscript output_src/03_build_gemeente_provincie.R
```

```bash
Rscript output_src/02_build_geo_assets.R
```

```bash
Rscript output_src/01_extract_ggz_zpm.R
```

Het laatste script vraagt om databasetoegang en credentials in `.env` (zelfde formaat als
`maptool_v4/.env`; `.env` staat in `.gitignore`). De NL-output database is niet vanaf elke
werkplek bereikbaar — zonder toegang loopt de verbinding af in een timeout.

Zolang het echte extract ontbreekt, valt het dashboard terug op een synthetisch bestand en
toont het daarover een waarschuwing. Dat bestand maak je zo:

```bash
Rscript output_src/00_build_synthetic_data.R
```

> **Let op:** die cijfers zijn verzonnen. Ze dienen alleen om het dashboard te kunnen bouwen en
> testen, en horen nooit in een deliverable of in een gesprek met een opdrachtgever.

Het dashboard starten:

```bash
Rscript -e "shiny::runApp('dashboard/app.R')"
```

De tests draaien:

```bash
Rscript -e "setwd('dashboard/tests'); source('testthat.R')"
```

## Afhankelijkheden

```r
install.packages(c("shiny", "dplyr", "ggplot2", "tidyr", "tibble", "sf", "leaflet",
                   "rmapshaper", "colourpicker", "DT", "writexl", "jsonlite",
                   "filelock", "testthat", "DBI", "RPostgres"))
```

## Deployment

De workflow in `.github/workflows/` synchroniseert `dashboard/{app.R,data,utils,templates}`
naar `/apps/GGZ_NHFL/` op de Shiny-server. Het doelpad staat rechtstreeks in de workflow,
zoals in `RVS_laatste_1000_dagen`.
Toegang wordt server-side geregeld via Authelia, niet via shinymanager in de app zelf.

Deployen kan pas nadat het echte extract in `dashboard/data/` staat — anders wordt een
dashboard met verzonnen cijfers gepubliceerd.
