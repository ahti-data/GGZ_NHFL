# Dashboard: GGZ ZPM in Noord-Holland en Flevoland
#
# Kickoff-versie. Een tabblad, "Kickoff", met de kaartweergaven die voor de
# projectstart gevraagd zijn: de vier GGZ ZPM-domeinen over 2022-2024, met
# zeven uitkomstmaten, op vijf gebiedsindelingen (heel Nederland op
# provincieniveau, heel Nederland gesplitst in NH/FL versus de rest, en
# Noord-Holland/Flevoland als geheel, als twee provincies of op gemeenteniveau).
#
# De cijfers komen uit de NL-output database (maptool), teruggebracht tot een
# smalle uitsnede -- zie output_src/01_extract_ggz_zpm.R en PLAN.md.
#
# De think-cell-export uit het dashboardtemplate blijft in utils/ staan voor de
# tabbladen die hierna komen, maar wordt op de kaart zelf niet aangeboden: een
# kaart gaat niet naar think-cell. De kaart heeft in plaats daarvan een eigen
# PNG-download.

source("data/metadata/brand_colors.R")
source("utils/format_thinkcell_download.R")
source("utils/slide_download.R")
source("utils/template_admin.R")
source("utils/favorites.R")
source("utils/export_history.R")
source("utils/chart_downloads.R")
source("utils/tab_theme.R")
source("utils/ggz_data.R")
source("utils/ggz_map.R")

library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)

DASHBOARD_TITLE <- "ahti — GGZ ZPM in Noord-Holland en Flevoland"

# Alles wordt eenmalig bij het opstarten ingelezen: het gaat om kleine, statische
# bestanden (samen circa 2 MB), niet om sessie-afhankelijke data.
GGZ_DATA      <- ggz_load_data()
GGZ_GEO       <- ggz_load_geo()
GGZ_LOOKUP    <- read.csv(file.path("data", "gemeente_provincie.csv"),
                          colClasses = c("integer", "character", "character"),
                          encoding = "UTF-8")
GGZ_SYNTHETIC <- ggz_is_synthetic()

# Dataversie en doelpopulatie liggen voor de kickoff vast op de standaard van de
# maptool. Beide zitten wel in het extract, dus een later tabblad kan ze
# openstellen zonder nieuwe databaseronde (PLAN.md par. 7, vragen 5 en 6).
GGZ_VERSION    <- "v2"
GGZ_POPULATION <- "pop"

ui <- fluidPage(
  tc_tab_color_theme(ahti_branding),
  tags$head(tags$style(HTML("
    .ggz-waarschuwing {
      background: #FDECEA; border: 1px solid #EE3124; border-radius: 6px;
      padding: 10px 14px; margin-bottom: 14px; color: #82241A; font-size: 13px;
    }
    .ggz-kerncijfer {
      font-size: 40px; font-weight: 700; color: #336A88; line-height: 1.1;
    }
    .ggz-kerncijfer-label { font-size: 13px; color: #524F50; margin-bottom: 14px; }
    .ggz-context {
      background: #F4F4F4; border-left: 4px solid #009DDC; border-radius: 4px;
      padding: 10px 14px; margin-bottom: 12px; font-size: 14px;
    }
    /* De legenda schaalt mee met de kaarthoogte. Shiny schuift een eigen div
       tussen .ggz-legenda en de gerenderde HTML, dus die tussenlaag moet de
       hoogte doorgeven -- anders klapt de height:100% van de kleurblokjes
       dicht tot een streepje. */
    .ggz-legenda { height: 560px; padding: 8px 0; display: flex; flex-direction: column; }
    .ggz-legenda > .shiny-html-output { flex: 1; min-height: 0; }
    .ggz-legenda-titel {
      font-size: 15px; font-weight: bold; text-align: center; margin-bottom: 10px;
    }
    .ggz-voetnoot { font-size: 12px; color: #524F50; margin-top: 10px; }
  "))),

  titlePanel(DASHBOARD_TITLE),
  p(
    "Dit dashboard toont het gebruik en de kosten van de GGZ onder het ",
    "zorgprestatiemodel (ZPM), op basis van CBS-microdata via de NL-output van ",
    "het Amsterdam health & technology institute (ahti). Alle cijfers zijn ",
    "geaggregeerd en niet tot personen herleidbaar."
  ),

  tabsetPanel(
    id = "main_tabs",

    tabPanel(
      "Kickoff",
      br(),
      if (GGZ_SYNTHETIC) {
        div(class = "ggz-waarschuwing",
            strong("Let op: dit dashboard draait op verzonnen cijfers. "),
            "Het echte extract (", code("dashboard/data/ggz_zpm_gemeente.rds"),
            ") ontbreekt, omdat de NL-output database niet vanaf elke werkplek ",
            "bereikbaar is. De getoonde waarden zijn willekeurig gegenereerd om ",
            "de werking van het dashboard te kunnen tonen en zeggen niets over ",
            "de werkelijkheid. Draai ", code("output_src/01_extract_ggz_zpm.R"),
            " vanaf een machine met databasetoegang om ze te vervangen.")
      },
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput("domein", "Zorgdomein",
                      choices = GGZ_DOMEINEN, selected = GGZ_DOMEINEN[[1]]),
          radioButtons("jaar", "Jaar", choices = GGZ_JAREN,
                       selected = max(GGZ_JAREN), inline = TRUE),
          selectInput("uitkomst", "Uitkomst",
                      choices = GGZ_UITKOMSTEN, selected = "relatief_aantal"),
          selectInput("weergave", "Kaartweergave",
                      choices = GGZ_WEERGAVEN, selected = "nl_provincie"),
          hr(),
          radioButtons(
            "drempel",
            "Onderdruk gebieden met minder gebruikers dan",
            choices = c("Nee", "<30", "<50"), selected = "<30", inline = TRUE
          ),
          helpText(
            "Gebieden met te weinig waarnemingen worden niet apart getoond. ",
            "Dat volgt de CBS-uitvoerregels en dezelfde drempel als de maptool."
          ),
          hr(),
          colourpicker::colourInput("kleur_hoog", "Kleur (hoogste waarde)",
                                    value = GGZ_KLEUR_HOOG),
          colourpicker::colourInput("kleur_laag", "Kleur (laagste waarde)",
                                    value = GGZ_KLEUR_LAAG),
          helpText(
            "Bij een indexuitkomst is 1,00 het scharnierpunt: rood is minder dan ",
            "verwacht, de gekozen kleur is meer dan verwacht."
          )
        ),

        mainPanel(
          width = 9,
          uiOutput("context"),
          uiOutput("kerncijfer"),
          fluidRow(
            column(2, div(class = "ggz-legenda",
                          div(class = "ggz-legenda-titel", "Legenda"),
                          uiOutput("legenda"))),
            column(10, leafletOutput("kaart", height = "560px"))
          ),
          br(),
          div(
            style = paste("border:1px solid #E4E7EE; border-radius:8px;",
                          "padding:12px 14px; background:#FAFAFA; margin-bottom:10px;"),
            downloadButton("download_kaart", "Download kaart (PNG)",
                           class = "btn-primary")
          ),
          chart_data_downloads_ui("kickoff_dl", chart_type = "map",
                                  raw_label = "Download Excel data (tabel bij de kaart)"),
          h4("Cijfers bij de kaart"),
          DT::dataTableOutput("tabel"),
          uiOutput("voetnoot")
        )
      )
    ),

    tabPanel("Favorites", br(), favorites_panel_ui("favorites")),
    tabPanel("Export history", br(), export_history_panel_ui("export_history")),
    tabPanel("Manage templates", br(), template_admin_ui("template_admin"))
  )
)

server <- function(input, output, session) {

  tc_register_app_context(
    input,
    dashboard_title = DASHBOARD_TITLE,
    nav_id = "main_tabs",
    dl_option_prefixes = c(kickoff_dl = "^(domein|jaar|uitkomst|weergave|drempel)$")
  )

  geo_jaar <- reactive(ggz_geo_year(as.integer(input$jaar), GGZ_VERSION))

  # --- Cijfers ---------------------------------------------------------------

  gemeente_data <- reactive({
    GGZ_DATA %>%
      filter(
        .data$jaar         == as.integer(input$jaar),
        .data$version      == GGZ_VERSION,
        .data$population   == GGZ_POPULATION,
        .data$outcome_type == input$domein
      ) %>%
      ggz_prepare() %>%
      ggz_suppress(input$drempel)
  })

  geo_layer <- reactive(ggz_geo_layer(GGZ_GEO, input$weergave, geo_jaar()))

  agg_data <- reactive({
    ggz_aggregate(gemeente_data(), by = input$weergave,
                  provincie_lookup = GGZ_LOOKUP, geo_jaar = geo_jaar()) %>%
      ggz_add_names(geo_layer())
  })

  palette_data <- reactive({
    ggz_palette(
      agg_data()[[input$uitkomst]],
      is_index     = ggz_is_index(input$uitkomst),
      kleur_laag   = input$kleur_laag,
      kleur_hoog   = input$kleur_hoog
    )
  })

  # --- Teksten ---------------------------------------------------------------

  kaart_titel <- reactive({
    sprintf("%s — %s",
            ggz_label(input$domein, GGZ_DOMEINEN),
            ggz_label(input$uitkomst, GGZ_UITKOMSTEN))
  })

  kaart_ondertitel <- reactive({
    sprintf("%s · %s", ggz_label(input$weergave, GGZ_WEERGAVEN), input$jaar)
  })

  output$context <- renderUI({
    dat <- agg_data()
    n_ond <- sum(dat$n_onderdrukt, na.rm = TRUE)
    tekst <- sprintf(
      "De kaart toont %s voor %s in %s, weergegeven op de indeling %s.",
      tolower(ggz_label(input$uitkomst, GGZ_UITKOMSTEN)),
      ggz_label(input$domein, GGZ_DOMEINEN),
      input$jaar,
      tolower(ggz_label(input$weergave, GGZ_WEERGAVEN))
    )
    div(class = "ggz-context",
        tekst,
        if (n_ond > 0) {
          span(sprintf(" %d gemeente(n) zijn onderdrukt wegens te weinig gebruikers en tellen niet mee.", n_ond))
        })
  })

  # Bij een kaart van een enkel vlak (NH/FL als geheel) zegt de kleur niets;
  # daar is het getal zelf de boodschap.
  output$kerncijfer <- renderUI({
    dat <- agg_data()
    if (nrow(dat) != 1) return(NULL)
    div(
      div(class = "ggz-kerncijfer",
          ggz_format_value(dat[[input$uitkomst]], input$uitkomst)),
      div(class = "ggz-kerncijfer-label",
          sprintf("%s — %s, %s", dat$gebiedsnaam[1],
                  ggz_label(input$uitkomst, GGZ_UITKOMSTEN), input$jaar))
    )
  })

  output$voetnoot <- renderUI({
    if (!ggz_is_index(input$uitkomst)) return(NULL)
    div(class = "ggz-voetnoot",
        "Indexcijfers boven gemeenteniveau worden herberekend als de som van de ",
        "geobserveerde waarden gedeeld door de som van de verwachte waarden. Dat ",
        "steunt op de aanname dat de index in de NL-output multiplicatief ",
        "gedefinieerd is als geobserveerd/verwacht; die aanname staat nog open ",
        "(zie PLAN.md, vraag 4).")
  })

  # --- Kaart -----------------------------------------------------------------

  output$kaart <- renderLeaflet({
    pd <- palette_data()
    if (isTRUE(pd$error)) {
      showNotification(pd$message %||% "Geen data beschikbaar voor deze selectie.",
                       type = "warning", duration = 6)
    }
    ggz_leaflet(geo_layer(), agg_data(), input$uitkomst, pd)
  })

  output$legenda <- renderUI(ggz_legend_html(palette_data(), input$uitkomst))

  # --- Tabel -----------------------------------------------------------------

  tabel_data <- reactive({
    dat <- agg_data()
    out <- data.frame(
      Gebied              = dat$gebiedsnaam,
      Waarde              = dat[[input$uitkomst]],
      `Populatie`         = dat$n,
      `Aantal gebruikers` = dat$totaal_aantal,
      `Gebieden`          = dat$n_gebieden,
      `Waarvan onderdrukt`= dat$n_onderdrukt,
      check.names = FALSE
    )
    out[order(-out$Waarde, na.last = TRUE), , drop = FALSE]
  })

  output$tabel <- DT::renderDataTable({
    dat <- tabel_data()
    dat$Waarde <- ggz_format_value(dat$Waarde, input$uitkomst)
    DT::datatable(dat, rownames = FALSE, filter = "top",
                  options = list(pageLength = 12, dom = "ftip")) %>%
      DT::formatCurrency(c("Populatie", "Aantal gebruikers"), currency = "",
                         digits = 0, interval = 3, mark = ".")
  })

  # --- Downloads -------------------------------------------------------------

  output$download_kaart <- downloadHandler(
    filename = function() {
      # De domeinlabels beginnen zelf al met "GGZ ZPM", dus geen extra prefix.
      schoon <- function(x) gsub("^_|_$", "", gsub("[^A-Za-z0-9]+", "_", x))
      sprintf("%s_%s_%s_%s.png",
              schoon(ggz_label(input$domein, GGZ_DOMEINEN)),
              schoon(ggz_label(input$uitkomst, GGZ_UITKOMSTEN)),
              schoon(input$weergave), input$jaar)
    },
    content = function(file) {
      pd <- palette_data()
      if (isTRUE(pd$error)) {
        showNotification(pd$message %||% "Geen kaartdata om te exporteren.",
                         type = "error", duration = 7)
        stop("Geen kaartdata om te exporteren.")
      }
      plot <- ggz_static_map(geo_layer(), agg_data(), input$uitkomst, pd,
                             titel = kaart_titel(), ondertitel = kaart_ondertitel())
      ggplot2::ggsave(file, plot, device = "png", width = 10, height = 9,
                      dpi = 200, bg = "white")
    }
  )

  # chart_type "map" staat noch in TC_SUPPORTED_CHART_TYPES noch in
  # TC_TEMPLATE_BY_CHART_TYPE, dus dit levert alleen de knop "Download Excel
  # data" op -- geen think-cell-export, geen slide, geen favorietenster. Precies
  # wat voor een kaart gewenst is, zonder de think-cell-code aan te tasten.
  chart_data_downloads_server(
    id              = "kickoff_dl",
    data            = tabel_data,
    chart_type      = "map",
    category_col    = "Gebied",
    series_col      = NULL,
    value_col       = "Waarde",
    filename_prefix = "ggz_zpm_kaart",
    source_output   = "NL-output / zorgkosten",
    source_sheet    = reactive(sprintf("%s | %s | %s", input$domein, input$jaar,
                                       input$weergave))
  )

  favorites_panel_server("favorites")
  export_history_panel_server("export_history")
  template_admin_server("template_admin")
}

shinyApp(ui = ui, server = server)
