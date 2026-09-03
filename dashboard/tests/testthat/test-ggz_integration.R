# Integratietest: draait de volledige keten (laden -> voorbereiden -> onderdrukken
# -> aggregeren -> palet -> kaart) over elke combinatie die in de UI te kiezen is.
#
# Draait tegen het databestand dat er is: het echte extract als dat aanwezig is,
# anders de synthetische stand-in. De test controleert de *mechaniek*, niet de
# cijfers, en is dus in beide gevallen zinvol.

app_dir <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = FALSE)

skip_if_no_data <- function() {
  skip_if_not(
    file.exists(file.path(app_dir, "data", "ggz_zpm_gemeente.rds")) ||
      file.exists(file.path(app_dir, "data", "ggz_zpm_gemeente_synthetic.rds")),
    "Geen dataset aanwezig -- draai output_src/01 of output_src/00."
  )
}

with_app_dir <- function(code) {
  oud <- setwd(app_dir)
  on.exit(setwd(oud), add = TRUE)
  force(code)
}

test_that("elke UI-combinatie levert een bruikbare kaart op", {
  skip_if_no_data()
  skip_if_not_installed("sf")
  skip_if_not_installed("leaflet")

  with_app_dir({
    data   <- ggz_load_data()
    geo    <- ggz_load_geo()
    lookup <- read.csv(file.path("data", "gemeente_provincie.csv"),
                       colClasses = c("integer", "character", "character"),
                       encoding = "UTF-8")

    # Het aantal gebieden ligt per weergave vast; alleen het aantal gemeenten
    # verschilt per jaar (herindelingen).
    verwacht <- c(nl_provincie = 12, nl_nhfl_rest = 2,
                  nhfl_totaal = 1, nhfl_provincies = 2)

    mislukt <- character(0)

    for (d in GGZ_DOMEINEN) for (y in GGZ_JAREN) {
      geo_jaar <- ggz_geo_year(y, "v2")
      basis <- data %>%
        dplyr::filter(.data$jaar == y, .data$version == "v2",
                      .data$population == "pop", .data$outcome_type == d) %>%
        ggz_prepare() %>%
        ggz_suppress("<30")

      for (w in GGZ_WEERGAVEN) {
        laag <- ggz_geo_layer(geo, w, geo_jaar)
        agg  <- ggz_aggregate(basis, w, lookup, geo_jaar) %>% ggz_add_names(laag)

        label <- paste(d, y, w)
        if (nrow(agg) == 0) { mislukt <- c(mislukt, paste(label, "leeg")); next }
        if (w %in% names(verwacht) && nrow(agg) != verwacht[[w]]) {
          mislukt <- c(mislukt, sprintf("%s: %d gebieden, verwacht %d",
                                        label, nrow(agg), verwacht[[w]]))
        }
        # Elk gebied met cijfers moet ook een polygoon hebben, anders valt het
        # stilzwijgend van de kaart.
        if (!all(agg$gebied %in% laag$gebied)) {
          mislukt <- c(mislukt, paste(label, "gebied zonder polygoon"))
        }

        for (u in GGZ_UITKOMSTEN) {
          pd <- ggz_palette(agg[[u]], is_index = ggz_is_index(u))
          if (isTRUE(pd$error)) next  # legitiem: alles onderdrukt
          ok <- tryCatch({
            ggz_leaflet(laag, agg, u, pd)
            ggz_legend_html(pd, u)
            TRUE
          }, error = function(e) conditionMessage(e))
          if (!isTRUE(ok)) mislukt <- c(mislukt, paste(label, u, "->", ok))
        }
      }
    }

    expect_equal(mislukt, character(0))
  })
})

test_that("gebiedsindelingen tellen op tot dezelfde totalen", {
  skip_if_no_data()
  skip_if_not_installed("sf")

  with_app_dir({
    data   <- ggz_load_data()
    lookup <- read.csv(file.path("data", "gemeente_provincie.csv"),
                       colClasses = c("integer", "character", "character"),
                       encoding = "UTF-8")

    basis <- data %>%
      dplyr::filter(.data$jaar == 2024L, .data$version == "v2",
                    .data$population == "pop",
                    .data$outcome_type == "zvwggzzpmtotaal") %>%
      ggz_prepare() %>%
      ggz_suppress("Nee")

    agg <- function(w) ggz_aggregate(basis, w, lookup, geo_jaar = 2023L)

    # Twaalf provincies moeten hetzelfde landstotaal geven als NH/FL + de rest.
    expect_equal(sum(agg("nl_provincie")$totaal_aantal),
                 sum(agg("nl_nhfl_rest")$totaal_aantal))

    # NH en FL apart moeten optellen tot NH/FL als geheel.
    expect_equal(sum(agg("nhfl_provincies")$totaal_aantal),
                 agg("nhfl_totaal")$totaal_aantal)

    # En de gemeenten van NH/FL tellen op tot datzelfde geheel.
    expect_equal(sum(agg("nhfl_gemeente")$totaal_aantal),
                 agg("nhfl_totaal")$totaal_aantal)
  })
})

test_that("onderdrukking raakt vooral de kleine deelprestaties", {
  skip_if_no_data()
  skip_if_not_installed("sf")

  with_app_dir({
    data   <- ggz_load_data()
    lookup <- read.csv(file.path("data", "gemeente_provincie.csv"),
                       colClasses = c("integer", "character", "character"),
                       encoding = "UTF-8")

    onderdrukt <- function(domein, drempel) {
      basis <- data %>%
        dplyr::filter(.data$jaar == 2024L, .data$version == "v2",
                      .data$population == "pop", .data$outcome_type == domein) %>%
        ggz_prepare() %>%
        ggz_suppress(drempel)
      sum(ggz_aggregate(basis, "nhfl_gemeente", lookup, 2023L)$n_onderdrukt)
    }

    # Zonder drempel wordt er nooit iets onderdrukt.
    expect_equal(onderdrukt("sub_zvwggzzpmverblijf", "Nee"), 0)

    # Een hogere drempel kan nooit minder gebieden raken dan een lagere.
    expect_gte(onderdrukt("sub_zvwggzzpmverblijf", "<50"),
               onderdrukt("sub_zvwggzzpmverblijf", "<30"))

    # Het totaal raakt niet eerder onderdrukt dan de kleinste deelprestatie.
    expect_lte(onderdrukt("zvwggzzpmtotaal", "<30"),
               onderdrukt("sub_zvwggzzpmverblijf", "<30"))
  })
})
