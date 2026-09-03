test_that("ggz_geo_year schuift alleen bij v2 een jaar terug", {
  expect_equal(ggz_geo_year(2024, "v2"), 2023L)
  expect_equal(ggz_geo_year(2024, "v1"), 2024L)
})

# Een klein, met de hand doorgerekend voorbeeld: twee gemeenten in Noord-Holland
# en een in Flevoland, zodat elk aggregatieniveau iets te sommeren heeft.
mk_data <- function() {
  tibble::tibble(
    code           = c("0363", "0392", "0034"),
    jaar           = 2024L,
    version        = "v2",
    population     = "pop",
    outcome_type   = "zvwggzzpmtotaal",
    n              = c(1000, 500, 250),
    target_USE     = c(0.10, 0.20, 0.40),   # 100, 100, 100 gebruikers
    target_COSTS   = c(2000, 1000, 500),    # kosten p.g.
    target_PCCOSTS = c(200, 200, 200),      # = USE * COSTS
    index_USE      = c(2.0, 1.0, 0.5),
    index_COSTS    = c(1.0, 2.0, 4.0),
    index_PCCOSTS  = c(2.0, 2.0, 2.0),
    # comp_* = verwachte waarden, zo gekozen dat index = target / comp klopt.
    comp_USE       = c(0.10, 0.20, 0.40) / c(2.0, 1.0, 0.5),
    comp_COSTS     = c(2000, 1000, 500)  / c(1.0, 2.0, 4.0),
    comp_PCCOSTS   = c(200, 200, 200)    / c(2.0, 2.0, 2.0)
  )
}

mk_lookup <- function() {
  tibble::tibble(
    code      = c("0363", "0392", "0034"),
    jaar      = 2023L,
    provincie = c("Noord-Holland", "Noord-Holland", "Flevoland")
  )
}

agg <- function(by, drempel = "Nee") {
  ggz_aggregate(
    ggz_suppress(ggz_prepare(mk_data()), drempel),
    by = by, provincie_lookup = mk_lookup(), geo_jaar = 2023L
  )
}

test_that("ggz_prepare rekent aandelen en gemiddelden terug naar optelbare aantallen", {
  p <- ggz_prepare(mk_data())
  expect_equal(p$gebruikers, c(100, 100, 100))
  expect_equal(p$kosten_totaal, c(200000, 100000, 50000))
  # verwacht aantal gebruikers = comp_USE * n
  expect_equal(p$verwacht_gebruikers, c(50, 100, 200))
})

test_that("ggz_prepare gebruikt comp_* als die er zijn, en anders de index", {
  met_comp <- ggz_prepare(mk_data())

  # Zelfde invoer, maar zonder de comp-kolommen: dan valt hij terug op
  # verwacht = geobserveerd / index. Dat hoort hetzelfde antwoord te geven.
  zonder <- mk_data()
  zonder <- zonder[, setdiff(names(zonder), c("comp_USE", "comp_COSTS", "comp_PCCOSTS"))]
  via_index <- ggz_prepare(zonder)

  expect_equal(met_comp$verwacht_gebruikers, via_index$verwacht_gebruikers)
  expect_equal(met_comp$verwacht_kosten_pc, via_index$verwacht_kosten_pc)
  expect_equal(met_comp$verwacht_kosten_pg_totaal, via_index$verwacht_kosten_pg_totaal)
})

test_that("de terugvaloptie overleeft een index van nul of NA", {
  zonder <- mk_data()
  zonder <- zonder[, setdiff(names(zonder), c("comp_USE", "comp_COSTS", "comp_PCCOSTS"))]
  zonder$index_USE <- c(0, NA_real_, 0.5)

  p <- ggz_prepare(zonder)
  expect_true(is.na(p$verwacht_gebruikers[1]))   # deling door nul
  expect_true(is.na(p$verwacht_gebruikers[2]))   # ontbrekende index
  expect_equal(p$verwacht_gebruikers[3], 200)
})

test_that("aggregeren op gemeenteniveau reproduceert de invoer per gemeente", {
  out <- agg("gemeente")
  expect_setequal(out$gebied, c("0363", "0392", "0034"))
  expect_equal(out$n_gebieden, c(1, 1, 1))

  a <- out[out$gebied == "0363", ]
  expect_equal(a$relatief_aantal, 0.10)
  expect_equal(a$kosten_per_gebruiker, 2000)
  expect_equal(a$kosten_per_capita, 200)
  expect_equal(a$index_gebruik, 2.0)
  expect_equal(a$index_kosten_pg, 1.0)
})

test_that("provincietotalen zijn bevolkingsgewogen, niet ongewogen", {
  out <- agg("nl_provincie")
  nh <- out[out$gebied == "Noord-Holland", ]

  expect_equal(nh$n, 1500)
  expect_equal(nh$totaal_aantal, 200)
  expect_equal(nh$relatief_aantal, 200 / 1500)          # niet (0.10 + 0.20)/2
  expect_equal(nh$kosten_totaal, 300000)
  expect_equal(nh$kosten_per_capita, 300000 / 1500)
  expect_equal(nh$kosten_per_gebruiker, 300000 / 200)   # niet (2000 + 1000)/2
})

test_that("de index wordt herberekend als som geobserveerd / som verwacht", {
  out <- agg("nl_provincie")
  nh <- out[out$gebied == "Noord-Holland", ]

  # geobserveerd 100 + 100 = 200; verwacht 100/2 + 100/1 = 150
  expect_equal(nh$index_gebruik, 200 / 150)

  # En dat is aantoonbaar iets anders dan de twee gemakkelijke fouten:
  ongewogen <- mean(c(2.0, 1.0))                        # 1.50
  gewogen   <- weighted.mean(c(2.0, 1.0), c(1000, 500)) # 1.67
  expect_false(isTRUE(all.equal(nh$index_gebruik, ongewogen)))
  expect_false(isTRUE(all.equal(nh$index_gebruik, gewogen)))
  expect_equal(round(nh$index_gebruik, 4), 1.3333)
})

test_that("een gebied met een enkele gemeente reproduceert die gemeente", {
  out <- agg("nl_provincie")
  fl <- out[out$gebied == "Flevoland", ]
  bron <- mk_data()[3, ]

  expect_equal(fl$relatief_aantal, bron$target_USE)
  expect_equal(fl$kosten_per_gebruiker, bron$target_COSTS)
  expect_equal(fl$index_gebruik, bron$index_USE)
  expect_equal(fl$index_kosten_pg, bron$index_COSTS)
})

test_that("kosten totaal komt langs beide routes op hetzelfde uit", {
  # PCCOSTS * n moet gelijk zijn aan COSTS * (USE * n). Dit is de controle die
  # 01_extract_ggz_zpm.R ook op de echte data draait (PLAN.md par. 5.1).
  d <- mk_data()
  expect_equal(d$target_PCCOSTS * d$n,
               d$target_COSTS * d$target_USE * d$n)
})

test_that("de vijf weergaven leveren het juiste aantal gebieden op", {
  expect_equal(nrow(agg("nl_provincie")), 2)      # NH en FL in dit testfixture
  expect_equal(nrow(agg("nl_nhfl_rest")), 1)      # geen gemeente buiten NH/FL
  expect_equal(nrow(agg("nhfl_totaal")), 1)
  expect_equal(nrow(agg("nhfl_provincies")), 2)
  expect_equal(nrow(agg("nhfl_gemeente")), 3)
})

test_that("nl_nhfl_rest scheidt NH/FL van de rest van het land", {
  data <- rbind(mk_data(), transform(mk_data()[1, ], code = "0518"))
  lookup <- rbind(mk_lookup(),
                  tibble::tibble(code = "0518", jaar = 2023L, provincie = "Zuid-Holland"))

  out <- ggz_aggregate(ggz_suppress(ggz_prepare(data), "Nee"),
                       by = "nl_nhfl_rest", provincie_lookup = lookup, geo_jaar = 2023L)

  expect_setequal(out$gebied, c("Noord-Holland & Flevoland", "Overig Nederland"))
  expect_equal(out$n_gebieden[out$gebied == "Overig Nederland"], 1)
  expect_equal(out$n_gebieden[out$gebied == "Noord-Holland & Flevoland"], 3)
})

test_that("onderdrukking maskeert kleine gebieden en telt ze mee als onderdrukt", {
  # Een gemeente met 20 gebruikers valt onder de drempel van 30.
  d <- mk_data()
  d$target_USE[3] <- 20 / d$n[3]
  d$target_PCCOSTS[3] <- d$target_USE[3] * d$target_COSTS[3]

  s <- ggz_suppress(ggz_prepare(d), "<30")
  expect_equal(s$onderdrukt, c(FALSE, FALSE, TRUE))
  expect_true(is.na(s$gebruikers[3]))

  out <- ggz_aggregate(s, by = "nl_nhfl_rest",
                       provincie_lookup = mk_lookup(), geo_jaar = 2023L)
  expect_equal(out$n_onderdrukt, 1)
  expect_equal(out$totaal_aantal, 200)  # de onderdrukte gemeente telt niet mee
})

test_that("drempel 'Nee' onderdrukt niets", {
  s <- ggz_suppress(ggz_prepare(mk_data()), "Nee")
  expect_false(any(s$onderdrukt))
  expect_false(any(is.na(s$gebruikers)))
})

test_that("een gebied waarvan alles onderdrukt is levert NA op, geen nul", {
  d <- mk_data()
  d$target_USE <- 5 / d$n  # overal 5 gebruikers, dus alles onder de drempel
  s <- ggz_suppress(ggz_prepare(d), "<30")

  out <- ggz_aggregate(s, by = "nhfl_totaal",
                       provincie_lookup = mk_lookup(), geo_jaar = 2023L)
  expect_true(is.na(out$totaal_aantal))
  expect_true(is.na(out$relatief_aantal))
  expect_equal(out$n_onderdrukt, 3)
})

test_that("ggz_format_value gebruikt Nederlandse notatie per soort uitkomst", {
  expect_equal(ggz_format_value(0.0723, "relatief_aantal"), "7,2%")
  expect_equal(ggz_format_value(1.0847, "index_gebruik"), "1,08x")
  expect_equal(ggz_format_value(NA_real_, "kosten_totaal"), "N.v.t.")
  expect_match(ggz_format_value(1234567, "kosten_totaal"), "^€ 1\\.234\\.567$")
})

test_that("ggz_format_value laat het aantal decimalen overschrijven", {
  expect_equal(ggz_format_value(0.00347, "relatief_aantal"), "0,3%")
  expect_equal(ggz_format_value(0.00347, "relatief_aantal", digits = 2), "0,35%")
})

test_that("ggz_bin_digits kiest genoeg decimalen om klassen te onderscheiden", {
  # Een uitkomst rond de 7% heeft aan een decimaal genoeg.
  grof <- seq(0.062, 0.087, length.out = 9)
  expect_equal(ggz_bin_digits(grof, "relatief_aantal"), 1)

  # Rond de 0,3% -- zoals GGZ ZPM - Verblijf -- lopen de labels op een decimaal
  # in elkaar over; dan moet er een decimaal bij.
  fijn <- seq(0.0022, 0.0053, length.out = 9)
  d <- ggz_bin_digits(fijn, "relatief_aantal")
  expect_gt(d, 1)
  expect_false(anyDuplicated(ggz_format_value(fijn, "relatief_aantal", digits = d)) > 0)

  # En met de standaardprecisie zouden ze wel botsen -- anders test dit niets.
  expect_true(anyDuplicated(ggz_format_value(fijn, "relatief_aantal", digits = 1)) > 0)
})

test_that("ggz_bin_digits gaat niet onder de standaardprecisie zitten", {
  expect_gte(ggz_bin_digits(seq(1000, 9000, length.out = 9), "kosten_per_capita"),
             GGZ_DECIMALEN[["kosten_per_capita"]])
  expect_equal(ggz_bin_digits(numeric(0), "relatief_aantal"),
               GGZ_DECIMALEN[["relatief_aantal"]])
})

test_that("ggz_label vertaalt sleutels terug naar UI-labels", {
  expect_equal(ggz_label("zvwggzzpmtotaal", GGZ_DOMEINEN), "GGZ ZPM (totaal)")
  expect_equal(ggz_label("nhfl_gemeente", GGZ_WEERGAVEN),
               "Noord-Holland & Flevoland - gemeenten")
  expect_equal(ggz_label("onbekend", GGZ_DOMEINEN), "onbekend")
})

test_that("alle vier de gevraagde domeinen en zeven uitkomsten zitten in de keuzelijsten", {
  expect_setequal(unname(GGZ_DOMEINEN),
                  c("zvwggzzpmtotaal", "sub_zvwggzzpmconsult",
                    "sub_zvwggzzpmoverigprest", "sub_zvwggzzpmverblijf"))
  expect_true(all(c("relatief_aantal", "totaal_aantal", "kosten_totaal",
                    "kosten_per_gebruiker", "kosten_per_capita",
                    "index_gebruik") %in% GGZ_UITKOMSTEN))
  expect_equal(unname(GGZ_JAREN), c(2022L, 2023L, 2024L))
})
