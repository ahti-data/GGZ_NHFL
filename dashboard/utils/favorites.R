#' Shared favorites: star a chart, revisit it later, and download every
#' starred chart as one combined deck, rebuilt fresh against today's data.
#'
#' Deliberately per-dashboard, not per-user (kept simple for now — see
#' CLAUDE.md). Persisted as a flat JSON file at a path outside every folder
#' the deploy workflow syncs, so favorites survive a redeploy the same way
#' `state/template_uploads/` does (see `utils/template_admin.R`).
#'
#' A favorite is a *bookmark* -- which chart (`module_id`), plus display
#' metadata for the list -- not a snapshot of an export taken at star-time.
#' Every bulk download rebuilds live from that chart's current reactive data
#' via the session's chart registry (`tc_chart_registry_get()` in
#' `utils/slide_download.R`), the same mechanism Export History's
#' "Regenerate" uses (see `export_history_prepare_regenerate_spec()` in
#' `utils/export_history.R`). A favorite whose chart isn't live in the
#' downloading session (e.g. that chart no longer exists in the dashboard) is
#' simply skipped, with a notification -- there is no frozen-snapshot
#' fallback. Note this shares Export History's own known limitation: the
#' rebuild reads whatever the chart's inputs currently show in that session,
#' not the filter selections active when the favorite was starred (those are
#' kept in `selections` for display only).

FAVORITES_RELATIVE_PATH <- file.path("state", "favorites.json")

#' Where the shared favorites list is stored. Override with
#' `SHINY_FAVORITES_PATH` if a dashboard needs a different location.
favorites_path <- function() {
  Sys.getenv("SHINY_FAVORITES_PATH", FAVORITES_RELATIVE_PATH)
}

#' Read the shared favorites list.
#' @return List of favorite entries (possibly empty).
favorites_list <- function() {
  path <- favorites_path()
  if (!file.exists(path)) return(list())
  entries <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(entries)) return(list())
  entries
}

favorites_write <- function(entries) {
  path <- favorites_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(entries, path, auto_unbox = TRUE, null = "null", na = "null")
  invisible(path)
}

favorites_new_id <- function() {
  paste0(
    "fav_", tc_now("%Y%m%d%H%M%S"), "_",
    paste(sample(c(letters, LETTERS, 0:9), 6, replace = TRUE), collapse = "")
  )
}

#' A fresh id for one "Download all favorites" click -- shared by every
#' history entry logged from that click (see [favorites_build_deck_zip()]),
#' so the whole batch can be found and regenerated together later from
#' Export History. Distinct from [favorites_new_id()] (a *favorite*'s own,
#' permanent id, assigned once at star time) -- this one identifies a
#' *download event*, minted fresh every click, same as a solo download's id.
favorites_download_new_id <- function() {
  paste0(
    "favdl_", tc_now("%Y%m%d%H%M%S"), "_",
    paste(sample(c(letters, LETTERS, 0:9), 6, replace = TRUE), collapse = "")
  )
}

#' Save a new favorite.
#'
#' The full read-modify-write cycle runs under [tc_with_file_lock()] (see
#' `utils/slide_download.R`) so two sessions starring/removing favorites at
#' nearly the same time can't lose one write to the other -- without the
#' lock, both would read the same stale list and each write back only their
#' own change, dropping whichever one wrote second.
#' @param entry List, typically the output of [favorites_capture()].
#' @return The new favorite's id (invisibly).
favorites_add <- function(entry) {
  entry$id         <- favorites_new_id()
  entry$created_at <- tc_now()
  tc_with_file_lock(favorites_path(), function() {
    entries <- favorites_list()
    entries[[length(entries) + 1]] <- entry
    favorites_write(entries)
  })
  invisible(entry$id)
}

#' Remove a favorite by id. See [favorites_add()] for why this locks.
favorites_remove <- function(id) {
  tc_with_file_lock(favorites_path(), function() {
    entries <- favorites_list()
    kept <- Filter(function(e) !identical(e$id, id), entries)
    favorites_write(kept)
  })
  invisible(TRUE)
}

#' Remove several favorites at once. Irreversible -- the UI
#' (`favorites_panel_server()`) gates this behind a confirmation modal
#' before calling it. See [favorites_add()] for why this locks.
#' @param ids Character vector of favorite ids to remove; `NULL` removes
#'   every saved favorite (used by [favorites_remove_all()]).
favorites_remove_ids <- function(ids = NULL) {
  tc_with_file_lock(favorites_path(), function() {
    entries <- favorites_list()
    target_ids <- if (is.null(ids)) vapply(entries, function(e) tc_or(e$id, ""), character(1)) else ids
    kept <- Filter(function(e) !(tc_or(e$id, "") %in% target_ids), entries)
    favorites_write(kept)
  })
  invisible(TRUE)
}

#' Remove every saved favorite, emptying the shared list. Thin wrapper
#' around [favorites_remove_ids()] with `ids = NULL`.
favorites_remove_all <- function() {
  favorites_remove_ids(NULL)
}

#' Coerce a favorite's stored table (a data.frame when freshly captured, or a
#' `list(columns, rows)` shape after a JSON round-trip) back into a plain
#' data.frame.
#'
#' think-cell matrices always have an empty first column header (`""`) by
#' convention (see `format_tc_data()`), and `jsonlite` silently renames an
#' empty data.frame column name to its positional index when serializing a
#' data.frame directly (verified: `toJSON(data.frame(\`\` = 1))` comes back
#' keyed `"1"`, not `""`). Favorites therefore store the column names and row
#' values as two plain arrays instead of relying on JSON object keys to carry
#' column identity, so the header round-trips exactly.
favorites_table_as_df <- function(x) {
  if (is.data.frame(x)) return(x)
  cols <- x$columns
  rows <- x$rows
  if (length(rows) == 0) {
    df <- as.data.frame(matrix(nrow = 0, ncol = length(cols)))
  } else {
    col_values <- lapply(seq_along(cols), function(j) {
      vals <- lapply(rows, function(r) r[[j]])
      vals[vapply(vals, is.null, logical(1))] <- NA
      unlist(vals)
    })
    df <- as.data.frame(col_values, stringsAsFactors = FALSE, check.names = FALSE)
  }
  names(df) <- cols
  df
}

#' Inverse of [favorites_table_as_df()]: shape a data.frame into the
#' `list(columns, rows)` form that survives a JSON round-trip untouched.
favorites_table_to_storage <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  list(
    columns = names(df),
    rows = unname(lapply(seq_len(nrow(df)), function(i) unname(as.list(df[i, , drop = FALSE]))))
  )
}

#' Capture one favorite entry -- a bookmark to a chart plus display metadata,
#' not a data snapshot (see the file header). Every actual export table/slide
#' is rebuilt later, live, from `module_id` via the session's chart registry
#' (see [favorites_live_spec_or_null()]).
#'
#' @param chart_type Resolved (non-reactive) think-cell chart type, for
#'   display in the favorites list only -- a live rebuild re-resolves this
#'   fresh from current data, the same way [chart_data_downloads_server()]'s
#'   `build_export_spec()` does.
#' @param slide_title,figure_title Optional slide text, used only to resolve
#'   `label` below -- not persisted on the entry, since a live rebuild pulls
#'   fresh ones from the chart's current spec.
#' @param dashboard_title,tab_label,subtab_label Breadcrumb metadata for the
#'   favorites list.
#' @param selections Snapshot of the option selections active when starred,
#'   for display only (see the file header note on selection fidelity) --
#'   never re-applied to a live rebuild.
#' @param module_id The chart's `chart_data_downloads_server(id = ...)`,
#'   used to look this chart back up in the session's live chart registry at
#'   download time (see [favorites_live_spec_or_null()]).
#' @param filename_prefix Used only as a last-resort fallback when resolving
#'   `label` below.
#' @param label Optional short display label; defaults to the chart's own
#'   title (`figure_title`/`slide_title`), then the sub-tab, then the prefix.
#' @return A list ready for [favorites_add()].
favorites_capture <- function(
    chart_type,
    slide_title = "", figure_title = "",
    dashboard_title = "", tab_label = "", subtab_label = "",
    selections = NULL,
    module_id = NULL,
    filename_prefix = "chart", label = NULL
) {
  # tc_or() only falls back on NULL, not on "" — and tc_ctx_active_subtab()
  # legitimately returns "" whenever the app hasn't registered a nav/subtab
  # context (see tc_register_app_context()), so pick the first non-empty
  # candidate explicitly rather than chaining tc_or(). Prefers the chart's
  # own title (figure_title, then slide_title) over the sub-tab name, so a
  # favorite reads the same way the chart itself does; sub-tab/prefix are
  # only a fallback for charts that don't have a title wired yet.
  resolved_label <- Find(
    function(x) !is.null(x) && nzchar(x),
    list(label, figure_title, slide_title, subtab_label, filename_prefix)
  )

  list(
    label           = tc_or(resolved_label, "favorite"),
    dashboard_title = dashboard_title,
    tab_label       = tab_label,
    subtab_label    = subtab_label,
    chart_type      = chart_type,
    selections      = selections,
    module_id       = module_id
  )
}

#' Build the HTML page bundling every spec's captured chart image, in the
#' same order as `specs`, so a bulk download's PNGs read as one document
#' instead of N loose files scattered in the ZIP. Self-contained (images
#' embedded as base64 data URIs) -- opens directly in any browser, no
#' server or extra files needed.
#' @param specs Same shape as [tc_build_deck_from_specs()]; only `label` and
#'   `asset_path` are used here.
#' @return The HTML as a single string, or `NA_character_` if no spec has a
#'   usable `asset_path`.
tc_build_charts_overview_html <- function(specs) {
  sections <- vapply(specs, function(s) {
    asset <- s$asset_path
    if (is.null(asset) || !nzchar(asset) || !file.exists(asset)) return("")
    bytes <- tryCatch(readBin(asset, "raw", n = file.info(asset)$size), error = function(e) NULL)
    if (is.null(bytes) || length(bytes) == 0) return("")
    uri <- paste0("data:image/png;base64,", jsonlite::base64_enc(bytes))
    sprintf(
      '<section style="margin-bottom:32px;"><h2 style="font:600 16px sans-serif; color:#111;">%s</h2><img src="%s" style="max-width:100%%; border:1px solid #ddd; border-radius:4px;"></section>',
      htmltools::htmlEscape(tc_or(s$label, "chart")), uri
    )
  }, character(1))
  sections <- sections[nzchar(sections)]
  if (length(sections) == 0) return(NA_character_)
  paste0(
    "<!doctype html><html><head><meta charset=\"utf-8\">",
    "<title>Charts</title></head><body style=\"font-family:sans-serif; max-width:900px; margin:24px auto; padding:0 16px;\">",
    paste(sections, collapse = "\n"),
    "</body></html>"
  )
}

#' Build one combined ZIP from a list of chart "specs" -- two workbooks (one
#' sheet per spec each, same sheet names) -- `favorites_thinkcell_tables.xlsx`
#' (the think-cell-shaped matrix) and `favorites_raw_tables.xlsx` (only for
#' specs that have one) -- a single `charts_overview.html` bundling every
#' spec's captured chart image in order (see [tc_build_charts_overview_html()];
#' skipped when no spec has one), and either a single rendered multi-slide
#' deck (one `ppttc.exe` call over every spec's slide block concatenated into
#' one `.ppttc` array) or the same graceful template+`.ppttc`+README fallback
#' [tc_build_slide_zip()] uses when no renderer is available.
#'
#' Generic over *where* the specs came from -- [favorites_build_deck_zip()]
#' builds them from saved favorites; a "regenerate this bulk download" flow
#' (`utils/export_history.R`) builds them from freshly re-derived live data
#' instead -- so both stay byte-for-byte consistent by construction.
#'
#' @param specs List of `list(label, tc_table, raw_table = NULL, chart_type,
#'   template_path = NA, slide_title = "", figure_title = "", download_id =
#'   NULL, favorite_download_id = NULL, datasheet_log = NULL, asset_path =
#'   NULL)`. `template_path` (already resolved, or `NA`) decides whether a
#'   spec is renderable; `datasheet_log` is the fully-built corner-cell log
#'   string (see [tc_build_datasheet_log()]) -- the caller builds it, since
#'   it already has every field that goes into it.
#' @param zip_path Output `.zip` path (the `file` handed in by downloadHandler).
#' @param ppttc_exe Optional override for the think-cell executable.
#' @return `zip_path`, invisibly.
tc_build_deck_from_specs <- function(specs, zip_path, ppttc_exe = NULL) {
  work <- tempfile("tc_deck_")
  dir.create(work)
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(work, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  if (length(specs) > 0) {
    as_df <- function(x) as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
    labels <- sanitize_excel_sheet_names(
      vapply(specs, function(s) tc_or(s$label, "chart"), character(1))
    )

    # Stamp each sheet with its own chart's provenance log (see
    # tc_stamp_tc_matrix_corner()) -- same corner-cell record the single-chart
    # "Download Excel data (think-cell)" button and the slide's own datasheet
    # already carry, so a PM who only has this combined workbook can still
    # trace any sheet back to its source.
    sheets <- stats::setNames(
      lapply(specs, function(s) tc_stamp_tc_matrix_corner(as_df(s$tc_table), s$datasheet_log)),
      labels
    )
    write_tc_xlsx(sheets, file.path(work, "favorites_thinkcell_tables.xlsx"))

    # raw_table is optional per spec; specs without one simply don't
    # contribute a sheet here rather than failing the whole export.
    has_raw <- vapply(specs, function(s) !is.null(s$raw_table), logical(1))
    if (any(has_raw)) {
      raw_sheets <- stats::setNames(lapply(specs[has_raw], function(s) as_df(s$raw_table)), labels[has_raw])
      write_tc_xlsx(raw_sheets, file.path(work, "favorites_raw_tables.xlsx"))
    }
  }

  tc_write_deck_files(specs, work, ppttc_exe)

  files <- basename(list.files(work, full.names = TRUE))
  zip_path_abs <- normalizePath(zip_path, winslash = "/", mustWork = FALSE)
  setwd(work)
  utils::zip(zipfile = zip_path_abs, files = files, flags = "-q -X")

  invisible(zip_path_abs)
}

#' Write the "slide" half of a combined favorites/export-history export --
#' the captured-image overview page and either a rendered multi-slide
#' `.pptx` or the graceful template+`.ppttc`+README fallback -- into an
#' already-created `work` directory, with no zip. Extracted out of
#' [tc_build_deck_from_specs()] so [tc_build_slide_deck_zip()] (Favorites'
#' "Download slides" button -- just this half, zipped on its own) can share
#' it without duplicating the deck-building logic.
#' @param specs Same shape as [tc_build_deck_from_specs()].
#' @param work An already-created, writable directory.
#' @param ppttc_exe Optional override for the think-cell executable.
#' @param include_tables When `TRUE`, also write each spec's own
#'   `<label>_table.xlsx` (and `<label>_raw.xlsx` when `raw_table` is
#'   present) -- the same per-chart shape [tc_build_slide_zip()] ships for a
#'   single chart. [tc_build_slide_deck_zip()] (Favorites' "Download
#'   slides") wants this so that one zip has everything; the default `FALSE`
#'   keeps [tc_build_deck_from_specs()] as-is, since it already writes its
#'   own *combined*, cross-chart `favorites_thinkcell_tables.xlsx`/
#'   `favorites_raw_tables.xlsx` before calling this.
#' @return Invisible `NULL`.
tc_write_deck_files <- function(specs, work, ppttc_exe = NULL, include_tables = FALSE) {
  if (length(specs) == 0) {
    writeLines("No charts to include.", file.path(work, "README.txt"))
    return(invisible(NULL))
  }

  as_df <- function(x) as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)

  if (include_tables) {
    # A real file name, not an Excel sheet name -- sanitize_excel_sheet_names()
    # doesn't strip '|', which several chart titles use as a separator (e.g.
    # agg_plot_title's "sheet | outcome | maat") and which Windows forbids in
    # file names, crashing the whole ZIP when it reached here unsanitized.
    file_labels <- sanitize_filename_components(
      vapply(specs, function(s) tc_or(s$label, "chart"), character(1))
    )
    for (i in seq_along(specs)) {
      s <- specs[[i]]
      # Same corner-cell provenance stamp the single-chart "Download Excel
      # data (think-cell)" button carries (see tc_stamp_tc_matrix_corner()) --
      # only the think-cell-shaped table gets it; raw_table's column 1 is a
      # real data column, not the blank placeholder think-cell leaves.
      stamped <- tc_stamp_tc_matrix_corner(as_df(s$tc_table), s$datasheet_log)
      write_tc_xlsx(stamped, file.path(work, paste0(file_labels[[i]], "_table.xlsx")))
      if (!is.null(s$raw_table)) {
        write_tc_xlsx(as_df(s$raw_table), file.path(work, paste0(file_labels[[i]], "_raw.xlsx")))
      }
    }
  }

  # One page with every captured chart image in spec order, instead of N
  # loose chart_<label>.png files -- see tc_build_charts_overview_html().
  overview_html <- tc_build_charts_overview_html(specs)
  if (!is.na(overview_html)) {
    writeLines(overview_html, file.path(work, "charts_overview.html"), useBytes = TRUE)
  }

  renderable_idx <- which(vapply(specs, function(s) !is.na(tc_or(s$template_path, NA_character_)), logical(1)))
  rendered <- FALSE

  if (length(renderable_idx) > 0) {
    render_blocks <- vapply(renderable_idx, function(i) {
      s <- specs[[i]]
      tc_build_ppttc_slide_block(
        as_df(s$tc_table), tc_short_path(s$template_path),
        tc_or(s$slide_title, ""), tc_or(s$figure_title, ""),
        chart_id = s$download_id, datasheet_log = s$datasheet_log,
        favorite_download_id = s$favorite_download_id
      )
    }, character(1))
    ppttc_json <- sprintf("[%s]", paste(render_blocks, collapse = ","))
    exe <- tc_or(ppttc_exe, tc_find_ppttc_exe())

    if (!is.null(exe) && !is.na(exe) && nzchar(exe)) {
      out_pptx <- file.path(work, "favorites_deck.pptx")
      res <- tc_render_pptx_ppttc(ppttc_json, out_pptx, exe)
      rendered <- isTRUE(res$ok)
    }

    if (!rendered) {
      # Templates must be referenced by the bare file name copied alongside
      # them (see the matching note in tc_build_slide_zip()) -- a server
      # path resolved here is meaningless on whatever PC opens the bundle.
      portable_blocks <- vapply(renderable_idx, function(i) {
        s <- specs[[i]]
        tc_build_ppttc_slide_block(
          as_df(s$tc_table), basename(s$template_path),
          tc_or(s$slide_title, ""), tc_or(s$figure_title, ""),
          chart_id = s$download_id, datasheet_log = s$datasheet_log,
          favorite_download_id = s$favorite_download_id
        )
      }, character(1))
      portable_json <- sprintf("[%s]", paste(portable_blocks, collapse = ","))
      writeLines(portable_json, file.path(work, "favorites_deck.ppttc"), useBytes = TRUE)
      templates_used <- unique(vapply(specs[renderable_idx], function(s) s$template_path, character(1)))
      for (tpl_path in templates_used) {
        file.copy(tpl_path, file.path(work, basename(tpl_path)), overwrite = TRUE)
      }
      writeLines(paste0(
        "think-cell was not available to render the combined deck automatically.\n",
        "To finish it on a PC with PowerPoint + think-cell:\n\n",
        "  ppttc favorites_deck.ppttc -o favorites_deck.pptx\n\n",
        "(the template files referenced inside favorites_deck.ppttc are included alongside it)\n"
      ), file.path(work, "README_render_deck.txt"), useBytes = TRUE)
    }
  } else {
    writeLines(paste(
      "None of these charts currently have a matching think-cell",
      "template, so no deck could be built. The tables are still included."
    ), file.path(work, "NO_TEMPLATE.txt"))
  }
  invisible(NULL)
}

#' Zip up the "slide" half of a combined favorites/export-history export
#' (see [tc_write_deck_files()]), plus each chart's own `_table.xlsx`/
#' `_raw.xlsx` -- so this single zip alone has everything a chart's own
#' single "Download slide" button would give you, just for every favorite
#' at once. Used by Favorites' "Download slides" bulk button, one of three
#' separate, consistently-named bulk downloads (mirroring a single chart's
#' own raw/think-cell/slide split) that replaced one single combined
#' "Download all favorites" click -- the other two buttons additionally
#' give a *combined*, cross-chart workbook (one sheet per favorite) for
#' whichever format you only need in bulk.
#' @param specs Same shape as [tc_build_deck_from_specs()].
#' @param zip_path Output `.zip` path.
#' @param ppttc_exe Optional override for the think-cell executable.
#' @return `zip_path`, invisibly.
tc_build_slide_deck_zip <- function(specs, zip_path, ppttc_exe = NULL) {
  work <- tempfile("tc_deck_slides_")
  dir.create(work)
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(work, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  tc_write_deck_files(specs, work, ppttc_exe, include_tables = TRUE)

  files <- basename(list.files(work, full.names = TRUE))
  zip_path_abs <- normalizePath(zip_path, winslash = "/", mustWork = FALSE)
  setwd(work)
  utils::zip(zipfile = zip_path_abs, files = files, flags = "-q -X")

  invisible(zip_path_abs)
}

#' Write just the think-cell-shaped combined workbook for a list of specs --
#' one sheet per spec -- with no deck, no overview, no zip wrapper (a bare
#' `.xlsx`). Used by Favorites' "Download Excel data (think-cell formatted)"
#' bulk button -- unlike "Download slides", this one isn't logged to Export
#' History, matching the single-chart "Download data (think-cell)" button's
#' own convention (only a *slide* download is audited).
#' @param specs List of `list(label, tc_table)`.
#' @param path Output `.xlsx` path.
#' @return `path`, invisibly.
tc_build_thinkcell_xlsx_from_specs <- function(specs, path) {
  as_df <- function(x) as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  if (length(specs) == 0) {
    write_tc_xlsx(data.frame(note = "No charts to include."), path)
    return(invisible(path))
  }
  labels <- sanitize_excel_sheet_names(vapply(specs, function(s) tc_or(s$label, "chart"), character(1)))
  sheets <- stats::setNames(lapply(specs, function(s) as_df(s$tc_table)), labels)
  write_tc_xlsx(sheets, path)
  invisible(path)
}

#' Write just the raw-data combined workbook for a list of specs -- one
#' sheet per spec that has one, silently skipping any that don't -- with no
#' deck, no overview, no zip wrapper (a bare `.xlsx`). Used by Favorites'
#' "Download Excel data (raw)" bulk button; not logged to Export History,
#' same reasoning as [tc_build_thinkcell_xlsx_from_specs()].
#' @param specs List of `list(label, raw_table = NULL)`.
#' @param path Output `.xlsx` path.
#' @return `path`, invisibly.
tc_build_raw_xlsx_from_specs <- function(specs, path) {
  as_df <- function(x) as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  has_raw <- vapply(specs, function(s) !is.null(s$raw_table), logical(1))
  if (length(specs) == 0 || !any(has_raw)) {
    write_tc_xlsx(data.frame(note = "No raw data to include."), path)
    return(invisible(path))
  }
  labels <- sanitize_excel_sheet_names(vapply(specs, function(s) tc_or(s$label, "chart"), character(1)))
  raw_sheets <- stats::setNames(lapply(specs[has_raw], function(s) as_df(s$raw_table)), labels[has_raw])
  write_tc_xlsx(raw_sheets, path)
  invisible(path)
}

#' Look up a favorite's chart in the current session's live registry (see
#' `tc_chart_registry_get()` in `utils/slide_download.R`) and pull its
#' current exportable state. `NULL` when the chart isn't live this session,
#' or is faceted (the same scope limitation snapshotting always had --
#' faceted charts were never capturable either) -- there is no snapshot
#' fallback (see the file header). Shared by every live-rebuild path below.
#' @param entry A favorite entry (as returned by [favorites_list()]).
#' @param session The Shiny session driving this download.
favorites_live_spec_or_null <- function(entry, session) {
  reg <- tc_chart_registry_get(session, tc_or(entry$module_id, ""))
  if (is.null(reg)) return(NULL)
  spec <- tryCatch(reg$get_spec(), error = function(e) NULL)
  if (is.null(spec) || isTRUE(spec$is_faceted)) return(NULL)
  spec
}

#' Live `list(label, tc_table, raw_table)` for one favorite -- for the plain
#' xlsx bulk buttons ([favorites_build_thinkcell_xlsx()]/
#' [favorites_build_raw_xlsx()]), which need today's tables but no template
#' resolution or history logging. `NULL` when [favorites_live_spec_or_null()]
#' returns `NULL` (the caller skips it).
#' @param entry A favorite entry.
#' @param session The Shiny session driving this download.
favorites_prepare_live_table <- function(entry, session) {
  spec <- favorites_live_spec_or_null(entry, session)
  if (is.null(spec)) return(NULL)
  list(
    label = tc_or(entry$label, "favorite"),
    tc_table = as.data.frame(tc_or(spec$slide_matrix, spec$tc_data), stringsAsFactors = FALSE, check.names = FALSE),
    raw_table = if (!is.null(spec$raw_data)) as.data.frame(spec$raw_data, stringsAsFactors = FALSE, check.names = FALSE) else NULL
  )
}

#' Live [tc_build_deck_from_specs()]-shaped spec for one favorite -- for the
#' slide-producing bulk buttons ([favorites_build_deck_zip()]/
#' [favorites_build_slides_zip()]). Mirrors
#' `export_history_prepare_regenerate_spec()`'s live branch
#' (`utils/export_history.R`) almost exactly: logs a fresh Export History
#' entry from today's data (only when a matching template exists, same
#' condition this function always used), resolves the template, and builds
#' the datasheet corner-cell log. `NULL` when
#' [favorites_live_spec_or_null()] returns `NULL` (the caller skips it -- no
#' snapshot fallback).
#' @param entry A favorite entry.
#' @param session The Shiny session driving this download.
#' @param favorite_download_id Shared id for this whole bulk click (see
#'   [favorites_download_new_id()]).
#' @param batch_created_at Shared timestamp for this whole bulk click.
#' @param templates_dir Optional templates directory override (mainly for tests).
#' @param captured_image Optional data-URI from this session's bulk-capture
#'   round (see `TC_CHART_CAPTURE_JS`'s `.tc-regenerate-go-btn` handler), for
#'   this entry's own module; `NULL` if none was captured.
favorites_prepare_live_spec <- function(entry, session, favorite_download_id = NULL,
                                         batch_created_at = NULL, templates_dir = NULL,
                                         captured_image = NULL) {
  live_spec <- favorites_live_spec_or_null(entry, session)
  if (is.null(live_spec)) return(NULL)

  tpl_path <- tc_template_for_chart_type(
    live_spec$chart_type, templates_dir = templates_dir,
    override = tc_or(live_spec$template_override, "")
  )

  download_id <- NA_character_
  if (!is.na(tpl_path)) {
    history_entry <- tc_history_capture(
      tc_data           = live_spec$tc_data,
      chart_type        = live_spec$chart_type,
      slide_matrix      = live_spec$slide_matrix,
      raw_data          = live_spec$raw_data,
      slide_title       = live_spec$slide_title,
      figure_title      = live_spec$figure_title,
      template_override = live_spec$template_override,
      slide_order       = live_spec$slide_order,
      dashboard_title   = live_spec$dashboard_title,
      tab_label         = live_spec$tab_label,
      subtab_label      = live_spec$subtab_label,
      selections        = live_spec$selections,
      source_output     = live_spec$source_output,
      source_sheet      = live_spec$source_sheet,
      source_mtime      = live_spec$source_mtime,
      favorite_download_id = favorite_download_id,
      module_id         = tc_or(entry$module_id, ""),
      filename_prefix   = live_spec$filename_prefix,
      templates_dir     = templates_dir
    )
    history_entry$id <- export_history_new_id()
    if (!is.null(batch_created_at)) history_entry$created_at <- batch_created_at
    download_id <- export_history_add(history_entry)
  }

  datasheet_log <- tc_build_datasheet_log(
    dashboard_title = tc_or(live_spec$dashboard_title, ""),
    tab_label       = tc_or(live_spec$tab_label, ""),
    subtab_label    = tc_or(live_spec$subtab_label, ""),
    chart_type      = live_spec$chart_type,
    selections      = live_spec$selections,
    chart_id        = if (is.na(download_id)) NULL else download_id,
    favorite_download_id = favorite_download_id,
    source_output   = tc_or(live_spec$source_output, ""),
    source_sheet    = tc_or(live_spec$source_sheet, ""),
    source_mtime    = tc_or(live_spec$source_mtime, "")
  )

  # entry$label (the favorite's own display name, resolved once at star
  # time) wins over the chart's *current* title, so the name in the
  # favorites list and the name on the download always match -- falling
  # back to the live title chain only if a favorite somehow has no label.
  label <- tc_or(
    Find(function(x) !is.null(x) && nzchar(x),
         list(entry$label, live_spec$figure_title, live_spec$slide_title,
              entry$subtab_label, live_spec$filename_prefix)),
    "favorite"
  )
  slide_matrix <- tc_or(live_spec$slide_matrix, live_spec$tc_data)
  asset_path <- if (is.na(download_id)) NULL else export_history_asset_path(download_id)
  if (!is.null(asset_path)) tc_write_captured_asset(captured_image, asset_path)

  list(
    label = label,
    tc_table = as.data.frame(slide_matrix, stringsAsFactors = FALSE, check.names = FALSE),
    raw_table = if (!is.null(live_spec$raw_data)) as.data.frame(live_spec$raw_data, stringsAsFactors = FALSE, check.names = FALSE) else NULL,
    chart_type = live_spec$chart_type,
    template_path = tpl_path,
    slide_title = live_spec$slide_title,
    figure_title = live_spec$figure_title,
    download_id = if (is.na(download_id)) NULL else download_id,
    favorite_download_id = favorite_download_id,
    datasheet_log = datasheet_log,
    asset_path = asset_path
  )
}

#' Build the rich, [tc_build_deck_from_specs()]-shaped spec list for every
#' *live* favorite (see the file header), auto-logging each renderable one
#' to Export History (`utils/export_history.R`) along the way -- all sharing
#' one fresh `favorite_download_id` (see [favorites_download_new_id()]) so a
#' whole click can be found and regenerated together later, and one fresh
#' `download_id` each. A favorite whose chart isn't live this session is
#' skipped rather than replayed from a snapshot -- its label is returned in
#' `skipped` so callers can notify the user.
#' Shared by [favorites_build_deck_zip()] and [favorites_build_slides_zip()].
#' @param entries Favorites to include; defaults to every saved favorite.
#' @param session The Shiny session driving this download.
#' @param templates_dir Optional templates directory override (mainly for tests).
#' @param captures Named list of data-URIs from this session's bulk-capture
#'   round (see `TC_CHART_CAPTURE_JS`'s `.tc-regenerate-go-btn` handler),
#'   keyed by `module_id`.
#' @return `list(specs, skipped)` -- `skipped` is a character vector of
#'   labels for favorites whose chart wasn't live this session.
favorites_build_specs_with_history <- function(entries = NULL, session, templates_dir = NULL, captures = list()) {
  entries <- tc_or(entries, favorites_list())
  if (length(entries) == 0) return(list(specs = list(), skipped = character(0)))

  favorite_download_id <- favorites_download_new_id()
  # One shared timestamp for every entry logged from this click, rather than
  # each one's independently-generated (near-identical but not exact) time --
  # see export_history_add()'s created_at handling.
  batch_created_at <- tc_now()

  results <- lapply(entries, function(e) {
    favorites_prepare_live_spec(
      e, session, favorite_download_id = favorite_download_id,
      batch_created_at = batch_created_at, templates_dir = templates_dir,
      captured_image = captures[[tc_or(e$module_id, "")]]
    )
  })

  is_skipped <- vapply(results, is.null, logical(1))
  list(
    specs = Filter(Negate(is.null), results),
    skipped = vapply(entries[is_skipped], function(e) tc_or(e$label, "favorite"), character(1))
  )
}

#' Build one combined ZIP (data + slide deck) from every live favorite --
#' see [favorites_build_specs_with_history()] for the live-spec-building
#' this feeds [tc_build_deck_from_specs()].
#' @param zip_path Output `.zip` path (the `file` handed in by downloadHandler).
#' @param entries Favorites to include; defaults to every saved favorite.
#' @param session The Shiny session driving this download.
#' @param ppttc_exe Optional override for the think-cell executable.
#' @param templates_dir Optional templates directory override (mainly for tests).
#' @param captures Named list of data-URIs, keyed by `module_id` (see
#'   [favorites_build_specs_with_history()]).
#' @return Character vector of skipped favorites' labels (invisibly).
favorites_build_deck_zip <- function(zip_path, entries = NULL, session, ppttc_exe = NULL,
                                      templates_dir = NULL, captures = list()) {
  result <- favorites_build_specs_with_history(entries, session, templates_dir, captures)
  tc_build_deck_from_specs(result$specs, zip_path, ppttc_exe)
  invisible(result$skipped)
}

#' Build the slide-deck ZIP -- deck + each chart's own `_table.xlsx`/
#' `_raw.xlsx` (see [tc_write_deck_files()]'s `include_tables`), but no
#' *combined* cross-chart workbook -- from every live favorite. Favorites'
#' "Download slides" bulk button, one of three separate, consistently-named
#' bulk downloads (mirroring a single chart's own raw/think-cell/slide
#' split). Still logged to Export History, same live-spec-building as
#' [favorites_build_deck_zip()].
#' @inheritParams favorites_build_deck_zip
#' @return Character vector of skipped favorites' labels (invisibly).
favorites_build_slides_zip <- function(zip_path, entries = NULL, session, ppttc_exe = NULL,
                                        templates_dir = NULL, captures = list()) {
  result <- favorites_build_specs_with_history(entries, session, templates_dir, captures)
  tc_build_slide_deck_zip(result$specs, zip_path, ppttc_exe)
  invisible(result$skipped)
}

#' Build the combined think-cell-shaped workbook (bare `.xlsx`, no zip) for
#' every live favorite -- Favorites' "Download Excel data (think-cell
#' formatted)" bulk button. Not logged to Export History (matches the
#' single-chart "Download data (think-cell)" button's own convention -- only
#' a *slide* download is audited) -- deliberately builds a much simpler spec
#' than [favorites_build_specs_with_history()] since none of that function's
#' template-resolution/history-logging work is needed just to write a table.
#' @param path Output `.xlsx` path (the `file` handed in by downloadHandler).
#' @param entries Favorites to include; defaults to every saved favorite.
#' @param session The Shiny session driving this download.
#' @return Character vector of skipped favorites' labels (invisibly).
favorites_build_thinkcell_xlsx <- function(path, entries = NULL, session) {
  entries <- tc_or(entries, favorites_list())
  results <- lapply(entries, favorites_prepare_live_table, session = session)
  is_skipped <- vapply(results, is.null, logical(1))
  tc_build_thinkcell_xlsx_from_specs(Filter(Negate(is.null), results), path)
  invisible(vapply(entries[is_skipped], function(e) tc_or(e$label, "favorite"), character(1)))
}

#' Build the combined raw-data workbook (bare `.xlsx`, no zip) for every live
#' favorite -- Favorites' "Download Excel data (raw)" bulk button. Not
#' logged to Export History, same reasoning as [favorites_build_thinkcell_xlsx()].
#' @param path Output `.xlsx` path (the `file` handed in by downloadHandler).
#' @param entries Favorites to include; defaults to every saved favorite.
#' @param session The Shiny session driving this download.
#' @return Character vector of skipped favorites' labels (invisibly).
favorites_build_raw_xlsx <- function(path, entries = NULL, session) {
  entries <- tc_or(entries, favorites_list())
  results <- lapply(entries, favorites_prepare_live_table, session = session)
  is_skipped <- vapply(results, is.null, logical(1))
  tc_build_raw_xlsx_from_specs(Filter(Negate(is.null), results), path)
  invisible(vapply(entries[is_skipped], function(e) tc_or(e$label, "favorite"), character(1)))
}

#' Compact, single-line rendering of a favorite's option selections for the
#' Favorites list.
#'
#' Drops empty values, joins each option as `name: value`, and truncates the
#' whole string so a chart with many options doesn't blow up the row.
#' @param selections Named list of option selections (as stored on a favorite).
#' @param max_chars Soft cap on the returned string length.
#' @return A single string, or "" when there is nothing to show.
favorites_selections_inline <- function(selections, max_chars = 160) {
  if (is.null(selections) || length(selections) == 0) return("")
  nm <- names(selections)
  if (is.null(nm)) nm <- paste0("option_", seq_along(selections))
  parts <- vapply(seq_along(selections), function(i) {
    v <- selections[[i]]
    if (is.null(v) || length(v) == 0) return("")
    v <- paste(as.character(v), collapse = ", ")
    if (!nzchar(trimws(v))) return("")
    sprintf("%s: %s", nm[[i]], v)
  }, character(1))
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) return("")
  out <- paste(parts, collapse = " · ")
  if (nchar(out) > max_chars) out <- paste0(substr(out, 1, max_chars - 1), "…")
  out
}

#' UI for a "Favorites" tab: the saved list plus a combined download.
#' @param id Module id.
#' @param intro Optional override for the intro paragraph (a single string).
#'   Defaults to a description of the *shared, all-dashboard* list; pass a
#'   different one when mounting a per-tab filtered instance (see
#'   [favorites_panel_server()]'s `tab_label_filter`).
favorites_panel_ui <- function(id, intro = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Favorites"),
    shiny::p(class = "text-muted", tc_or(
      intro,
      paste0(
        "Shared across everyone using this dashboard — starring a chart bookmarks ",
        "it here. Every download below rebuilds live from today's data; a favorite ",
        "whose chart isn't currently open in your session is skipped."
      )
    )),
    shiny::downloadButton(ns("download_all_raw"), "Download Excel data (raw)", class = "btn-default"),
    shiny::downloadButton(ns("download_all_thinkcell"), "Download Excel data (think-cell formatted)", class = "btn-primary"),
    shiny::uiOutput(ns("slides_download_control"), inline = TRUE),
    shiny::actionButton(ns("remove_all"), "Remove all", class = "btn-default"),
    shiny::tags$hr(),
    shiny::uiOutput(ns("list")),
    shiny::tags$script(shiny::HTML(TC_CHART_CAPTURE_JS))
  )
}

#' Server logic for a "Favorites" tab.
#'
#' Uses `reactivePoll()` on the favorites file's modification time so the list
#' picks up stars added from any chart's module server without any direct
#' wiring between modules.
#' @param id Module id.
#' @param poll_interval_ms How often to check the favorites file for changes.
#' @param tab_label_filter Optional tab label (matching a favorite's own
#'   `tab_label`, e.g. `"Iteratie 1"`) -- when supplied, this instance shows,
#'   downloads, and removes only favorites starred from that tab, instead of
#'   the whole shared list. Mount one filtered instance per top-level tab
#'   (each with its own module `id`) alongside one unfiltered instance (the
#'   main "Favorites" tab) that always shows the cumulative, all-tabs list --
#'   they all read/write the same underlying `favorites.json`, so nothing
#'   needs to be kept in sync manually.
favorites_panel_server <- function(id, poll_interval_ms = 2000, tab_label_filter = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    entries_reactive <- shiny::reactivePoll(
      poll_interval_ms, session,
      checkFunc = function() {
        path <- favorites_path()
        if (file.exists(path)) as.character(file.info(path)$mtime) else ""
      },
      valueFunc = function() {
        entries <- favorites_list()
        if (is.null(tab_label_filter)) return(entries)
        Filter(function(e) identical(tc_or(e$tab_label, ""), tab_label_filter), entries)
      }
    )

    output$list <- shiny::renderUI({
      entries <- entries_reactive()
      if (length(entries) == 0) {
        return(shiny::tags$p(class = "text-muted",
                             "No favorites saved yet. Star a chart to add one."))
      }
      rows <- lapply(entries, function(e) {
        breadcrumb <- paste(
          Filter(nzchar, c(e$dashboard_title, e$tab_label, e$subtab_label)),
          collapse = " / "
        )
        options_line <- favorites_selections_inline(e$selections)
        details <- shiny::tagList(
          if (nzchar(breadcrumb)) shiny::tags$div(
            style = "font-size:12px; color:#6B7280;", breadcrumb
          ),
          if (nzchar(tc_or(e$chart_type, "")) || nzchar(tc_or(e$created_at, ""))) shiny::tags$div(
            style = "font-size:11px; color:#9CA3AF;",
            paste(Filter(nzchar, c(
              if (nzchar(tc_or(e$chart_type, ""))) paste0("Chart: ", e$chart_type),
              if (nzchar(tc_or(e$created_at, ""))) paste0("Saved: ", e$created_at)
            )), collapse = " · ")
          ),
          if (nzchar(options_line)) shiny::tags$div(
            style = "font-size:11px; color:#6B7280; margin-top:2px;",
            shiny::tags$span(style = "color:#9CA3AF;", "Options: "),
            options_line
          )
        )
        shiny::tags$div(
          style = paste(
            "display:flex; justify-content:space-between; align-items:flex-start;",
            "gap:12px; padding:8px 0; border-bottom:1px solid #eee;"
          ),
          shiny::tags$div(
            shiny::tags$strong(tc_or(e$label, "(untitled)")),
            details
          ),
          shiny::actionButton(session$ns(paste0("remove_", e$id)), "Remove",
                              class = "btn-default btn-sm")
        )
      })
      do.call(shiny::tagList, rows)
    })

    # One-shot removal observers, (re)created whenever the list changes.
    shiny::observe({
      entries <- entries_reactive()
      lapply(entries, function(e) {
        btn_id <- paste0("remove_", e$id)
        shiny::observeEvent(input[[btn_id]], {
          favorites_remove(e$id)
        }, ignoreInit = TRUE, once = TRUE)
      })
    })

    # Surfaced after any bulk download that skipped favorites whose chart
    # isn't live in this session (see favorites_live_spec_or_null() in
    # utils/favorites.R) -- there is no snapshot fallback, so this is the
    # only feedback the user gets when a favorite comes up empty.
    notify_skipped <- function(skipped, total) {
      if (length(skipped) == 0) return(invisible(NULL))
      shiny::showNotification(
        sprintf(
          "Skipped %d of %d favorite(s) — their chart isn't currently open in this session: %s",
          length(skipped), total, paste(skipped, collapse = ", ")
        ),
        type = "warning", duration = 8
      )
    }

    output$download_all_raw <- shiny::downloadHandler(
      filename = function() paste0("favorites_raw_", Sys.Date(), ".xlsx"),
      content = function(file) {
        entries <- entries_reactive()
        skipped <- favorites_build_raw_xlsx(file, entries = entries, session = session)
        notify_skipped(skipped, length(entries))
      }
    )

    output$download_all_thinkcell <- shiny::downloadHandler(
      filename = function() paste0("favorites_thinkcell_", Sys.Date(), ".xlsx"),
      content = function(file) {
        entries <- entries_reactive()
        skipped <- favorites_build_thinkcell_xlsx(file, entries = entries, session = session)
        notify_skipped(skipped, length(entries))
      }
    )

    # "Download slides" needs a fresh screenshot of every live chart before
    # the ZIP is built (so charts_overview.html isn't stale) -- same
    # actionButton + hidden-downloadButton + TC_CHART_CAPTURE_JS pattern
    # Export History's "Regenerate selected" already uses (see
    # utils/export_history.R's selection_banner). Rendered via renderUI
    # (rather than a static button in favorites_panel_ui()) so
    # data-module-ids always reflects the current favorites list.
    output$slides_download_control <- shiny::renderUI({
      module_ids <- unique(Filter(nzchar, vapply(
        entries_reactive(), function(e) tc_or(e$module_id, ""), character(1)
      )))
      shiny::tagList(
        shiny::actionButton(
          session$ns("download_all_slides_go"), "Download slides",
          class = "btn-primary tc-regenerate-go-btn",
          `data-module-ids` = jsonlite::toJSON(module_ids),
          `data-capture-input-id` = session$ns("slides_capture")
        ),
        shiny::tags$span(
          style = "display:none;",
          shiny::downloadButton(session$ns("download_all_slides"), "")
        )
      )
    })

    pending_slides_capture <- shiny::reactiveVal(list())
    shiny::observeEvent(input$slides_capture, {
      pending_slides_capture(tc_or(input$slides_capture$captures, list()))
      session$sendCustomMessage("tc_trigger_download", list(download_id = session$ns("download_all_slides")))
    }, ignoreInit = TRUE)

    output$download_all_slides <- shiny::downloadHandler(
      filename = function() paste0("favorites_slides_", Sys.Date(), ".zip"),
      content = function(file) {
        entries <- entries_reactive()
        skipped <- favorites_build_slides_zip(
          file, entries = entries, session = session, captures = pending_slides_capture()
        )
        notify_skipped(skipped, length(entries))
      }
    )
    # This download link lives inside a `display:none` wrapper (see
    # output$slides_download_control above) -- see the matching note in
    # utils/chart_downloads.R's own output$slide for why this is required.
    shiny::outputOptions(output, "download_all_slides", suspendWhenHidden = FALSE)

    shiny::observeEvent(input$remove_all, {
      n <- length(entries_reactive())
      scope <- if (is.null(tab_label_filter)) {
        "every saved favorite for everyone using this dashboard"
      } else {
        sprintf("all %d favorite(s) starred from this tab", n)
      }
      shiny::showModal(shiny::modalDialog(
        title = "Remove all favorites?",
        sprintf("This deletes %s. This can't be undone.", scope),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("remove_all_confirm"), "Remove all", class = "btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$remove_all_confirm, {
      favorites_remove_ids(vapply(entries_reactive(), function(e) e$id, character(1)))
      shiny::removeModal()
    })
  })
}
