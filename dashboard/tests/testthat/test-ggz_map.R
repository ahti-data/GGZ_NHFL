test_that("ggz_palette maakt niet meer klassen dan er gebieden zijn", {
  # Twaalf provincies: acht klassen, de bovengrens.
  expect_equal(length(ggz_palette(runif(12))$bins) - 1L, 8L)

  # Twee vlakken ("NH/FL vs. rest"): twee klassen, geen legenda vol lege vakjes.
  expect_equal(length(ggz_palette(c(0.070, 0.075))$bins) - 1L, 2L)

  expect_equal(length(ggz_palette(c(1, 2, 3))$bins) - 1L, 3L)
})

test_that("ggz_palette markeert het enkel-gebied-geval en onthoudt de waarde", {
  p <- ggz_palette(1.0123)
  expect_true(p$enkel)
  expect_equal(p$waarde, 1.0123)
  expect_false(p$error)
  # Het vlak moet nog wel een kleur krijgen.
  expect_false(is.na(p$palette(1.0123)))
})

test_that("ggz_palette meldt netjes dat er niets te tonen is", {
  p <- ggz_palette(c(NA_real_, NA_real_))
  expect_true(p$error)
  expect_null(p$palette)
})

test_that("de indexschaal scharniert op 1,00x", {
  # Gebieden aan weerszijden van de verwachting.
  p <- ggz_palette(c(0.8, 0.9, 1.1, 1.2), is_index = TRUE)

  # 1 is een klassegrens, niet een waarde binnen een klasse: geen enkel gebied
  # valt in een klasse die de verwachting overspant.
  expect_true(1 %in% p$bins)

  # De uiteinden dragen de uiterste kleuren; vlak bij 1 zijn de klassen juist
  # het bleekst, zodat de kleur de afstand tot de verwachting weergeeft.
  expect_equal(p$palette(0.8), GGZ_KLEUR_INDEX_LAAG)
  expect_equal(p$palette(1.2), GGZ_KLEUR_HOOG)
  expect_false(identical(p$palette(0.999), p$palette(1.001)))
})

test_that("een index waarvan alles boven de verwachting ligt gebruikt alle klassen", {
  # Dit ging eerder mis: de helft van de klassen viel samen op precies 1,
  # waardoor alle gebieden dezelfde kleur en hetzelfde legendalabel kregen.
  vals <- c(1.004, 1.011, 1.018, 1.025)
  p <- ggz_palette(vals, is_index = TRUE)

  expect_equal(length(p$bins) - 1L, length(vals))
  expect_equal(length(unique(p$palette(vals))), length(vals))

  # En alles blijft aan de "meer dan verwacht"-kant van de schaal.
  expect_equal(min(p$bins), 1)
})

test_that("een index waarvan alles onder de verwachting ligt idem", {
  vals <- c(0.90, 0.93, 0.96)
  p <- ggz_palette(vals, is_index = TRUE)

  expect_equal(length(p$bins) - 1L, length(vals))
  expect_equal(max(p$bins), 1)

  # De klassen lopen tot 1 door, dus de bovenste kan leeg blijven -- de gebieden
  # moeten wel onderscheiden worden en allemaal aan de rode kant vallen.
  expect_gt(length(unique(p$palette(vals))), 1)
  expect_false(anyNA(p$palette(vals)))
})

test_that("ggz_bin_palette geeft NA terug voor NA en voor waarden buiten bereik", {
  pal <- ggz_bin_palette(c(0, 1, 2), c("#000000", "#FFFFFF"))
  expect_equal(pal(0.5), "#000000")
  expect_equal(pal(1.5), "#FFFFFF")
  expect_true(is.na(pal(NA_real_)))
  expect_true(is.na(pal(99)))
})

test_that("de legenda toont de waarde zelf bij een enkel gebied", {
  html <- as.character(ggz_legend_html(ggz_palette(1.0123), "index_gebruik"))
  expect_match(html, "1,01x")
  expect_match(html, "geen schaal")
  # Geen verzonnen klassegrenzen zoals 0,91x.
  expect_false(grepl("0,91x", html, fixed = TRUE))
})

test_that("de legenda labelt de index-uiteinden als meer/minder dan verwacht", {
  html <- as.character(ggz_legend_html(ggz_palette(c(0.8, 1.2), is_index = TRUE),
                                       "index_gebruik"))
  expect_match(html, "Meer dan verwacht")
  expect_match(html, "Minder dan verwacht")

  gewoon <- as.character(ggz_legend_html(ggz_palette(c(0.05, 0.09)), "relatief_aantal"))
  expect_match(gewoon, ">Meer<")
  expect_false(grepl("dan verwacht", gewoon, fixed = TRUE))
})

test_that("ggz_map_bounds omsluit de laag", {
  skip_if_not_installed("sf")
  vierkant <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(4, 52), c(5, 52), c(5, 53), c(4, 53), c(4, 52)))), crs = 4326)
  laag <- sf::st_sf(gebied = "test", geometry = vierkant)

  b <- ggz_map_bounds(laag)
  expect_equal(b$lng1, 4); expect_equal(b$lat1, 52)
  expect_equal(b$lng2, 5); expect_equal(b$lat2, 53)
})

test_that("de lage kleur van de indexschaal is instelbaar", {
  # Standaard rood, maar overschrijfbaar -- dat was de wens van de collega.
  standaard <- ggz_palette(c(0.8, 1.2), is_index = TRUE)
  expect_equal(standaard$palette(0.8), GGZ_KLEUR_INDEX_LAAG)

  eigen <- ggz_palette(c(0.8, 1.2), is_index = TRUE, kleur_laag = "#20153E")
  expect_equal(eigen$palette(0.8), "#20153E")
  # De hoge kant blijft ongemoeid.
  expect_equal(eigen$palette(1.2), GGZ_KLEUR_HOOG)
})

test_that("de middenkleur van de indexschaal is instelbaar", {
  p <- ggz_palette(c(0.8, 0.9, 1.1, 1.2), is_index = TRUE,
                   kleur_laag = "#EE3124", kleur_midden = "#00A55D",
                   kleur_hoog = "#009DDC")
  # De klasse die tegen 1,00x aan ligt, loopt naar de middenkleur toe.
  kleuren <- p$palette(c(0.8, 0.99, 1.01, 1.2))
  expect_equal(kleuren[1], "#EE3124")
  expect_equal(kleuren[4], "#009DDC")
  expect_false(anyNA(kleuren))
})

test_that("een gewone schaal blijft van licht naar de gekozen kleur lopen", {
  p <- ggz_palette(c(0.05, 0.09))
  expect_equal(p$palette(0.05), GGZ_KLEUR_LAAG)
  expect_equal(p$palette(0.09), GGZ_KLEUR_HOOG)
})

test_that("de legendablokjes rekken via flex, niet via een percentagehoogte", {
  # Een percentagehoogte klapt dicht als de ouderketen geen definitieve hoogte
  # oplevert; dan blijven er acht randen van 1px over die als een zwarte streep
  # ogen. Dit is precies wat de collega zag.
  html <- as.character(ggz_legend_html(ggz_palette(c(0.05, 0.07, 0.09)),
                                       "relatief_aantal"))
  expect_match(html, "align-self:stretch")
  expect_match(html, "min-height")
  expect_false(grepl("width:20px; height:100%", html, fixed = TRUE))
})

test_that("ggz_index_js_conditie dekt precies de indexuitkomsten", {
  ja <- ggz_index_js_conditie()
  nee <- ggz_index_js_conditie(negatie = TRUE)

  for (u in GGZ_INDEX_UITKOMSTEN) expect_match(ja, u, fixed = TRUE)
  expect_match(ja, ">= 0", fixed = TRUE)
  expect_match(nee, "< 0", fixed = TRUE)

  # De niet-indexuitkomsten mogen er niet in voorkomen.
  for (u in setdiff(GGZ_UITKOMSTEN, GGZ_INDEX_UITKOMSTEN)) {
    expect_false(grepl(paste0("'", u, "'"), ja, fixed = TRUE))
  }
})

test_that("ggz_ggsave_transparent schrijft een PNG met een echt alfakanaal", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  skip_if_not_installed("sf")

  vierkant <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(4, 52), c(5, 52), c(5, 53), c(4, 53), c(4, 52)))), crs = 4326)
  laag <- sf::st_sf(gebied = c("a", "b"), waarde = c(1, 2), geometry = c(vierkant, vierkant))

  p <- ggplot2::ggplot(laag) +
    ggplot2::geom_sf(ggplot2::aes(fill = waarde)) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA)
    )

  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  ggz_ggsave_transparent(f, p, width = 4, height = 3, dpi = 72)

  expect_true(file.exists(f))
  arr <- png::readPNG(f)
  expect_equal(dim(arr)[3], 4)  # RGBA, geen RGB

  # De hoeken liggen buiten het getekende vlak en horen dus echt transparant te
  # zijn (alfa 0) -- niet stiekem opaak zwart, wat precies het gerapporteerde
  # probleem was op een apparaat zonder goede alfa-ondersteuning.
  hoogte <- dim(arr)[1]; breedte <- dim(arr)[2]
  hoeken <- list(c(1, 1), c(1, breedte), c(hoogte, 1), c(hoogte, breedte))
  for (h in hoeken) expect_equal(arr[h[1], h[2], 4], 0)
})

test_that("ggz_ggsave_transparent gebruikt ragg als dat beschikbaar is", {
  skip_if_not_installed("ragg")
  # Geen device-argument nodig van de aanroeper -- de functie kiest zelf.
  expect_true(is.function(ragg::agg_png))
})

test_that("de terugvalweg zonder ragg waarschuwt in plaats van stil te falen", {
  skip_if_not_installed("sf")

  vierkant <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(4, 52), c(5, 52), c(5, 53), c(4, 53), c(4, 52)))), crs = 4326)
  laag <- sf::st_sf(gebied = "a", geometry = vierkant)
  p <- ggplot2::ggplot(laag) + ggplot2::geom_sf()

  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)

  # requireNamespace() wordt binnen ggz_ggsave_transparent gemockt zodat 'ragg'
  # als niet-beschikbaar telt, ongeacht of dit systeem het pakket wel heeft --
  # zo test dit de terugvalweg in plaats van de systeemstatus van dit systeem.
  # ggz_ggsave_transparent roept requireNamespace() maar op een manier aan
  # (met package = "ragg"), dus de mock hoeft niets anders te onderscheiden;
  # een aanroep naar de oorspronkelijke functie binnen de mock zou, doordat
  # local_mocked_bindings de binding in het hele base-pakket vervangt, zichzelf
  # blijven aanroepen.
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) FALSE,
    .package = "base"
  )

  expect_warning(
    ggz_ggsave_transparent(f, p, width = 2, height = 2, dpi = 72),
    "ragg"
  )
  expect_true(file.exists(f))
})
