# mod_values.R — calculate EQ-5D values and append them as a column
#
# Every way the package has of turning EQ-5D responses into values is a choice
# in one Method selector: a published value set directly, a crosswalk, or the
# NICE Decision Support Unit's UK mapping. The DSU mapping used to be a page
# of its own, which put the same decision in two places.
#
# The direction of a mapping follows the instrument version set on the Data
# page, and is stated in the method's label and again on the page:
#   3L data -> eqxwr_UK(), giving UK EQ-5D-5L values
#   5L data -> eqxw_UK(),  giving UK EQ-5D-3L values

# A coverage table: how many rows were valued, and why the rest were not. The
# reasons are assigned in priority order among the unvalued rows only, so they
# are disjoint and add up to the unvalued total.
uk_coverage <- function(df, vals, ages, male, mapping) {
  n_rows <- nrow(df)
  has_id <- !is.null(mapping$name_id) && "id" %in% names(df)

  mapped_rows <- !is.na(vals)
  n_mapped    <- sum(mapped_rows)

  states  <- df[, DIMS_STD, drop = FALSE]
  max_lev <- if (identical(mapping$eq5d_version, "3L")) 3L else 5L
  bad_state <- !stats::complete.cases(states) |
    apply(states, 1L, function(r) any(r < 1L | r > max_lev, na.rm = TRUE))

  why <- rep(NA_character_, n_rows)
  unmapped <- !mapped_rows
  why[unmapped & bad_state]                      <- "state"
  why[unmapped & is.na(why) & is.na(ages$value)] <- "age"
  why[unmapped & is.na(why) & is.na(male)]       <- "sex"
  why[unmapped & is.na(why)]                     <- "other"

  pct <- function(n, d) if (d > 0L) sprintf("%.1f%% of rows", 100 * n / d) else ""

  rows <- list(
    c("Rows",   n_rows, ""),
    c("Mapped", n_mapped, pct(n_mapped, n_rows)),
    c("Not mapped — no usable EQ-5D health state", sum(why == "state", na.rm = TRUE), ""),
    c("Not mapped — age missing or unreadable",    sum(why == "age",   na.rm = TRUE), ""),
    c("Not mapped — sex missing",                  sum(why == "sex",   na.rm = TRUE), ""),
    c("Not mapped — other",                        sum(why == "other", na.rm = TRUE), "")
  )
  if (has_id) {
    rows <- c(rows, list(
      c("Respondents with at least one mapped row",
        length(unique(df$id[mapped_rows])),
        sprintf("of %s", format(length(unique(df$id)), big.mark = ",")))))
  }
  if (isTRUE(ages$is_banded)) {
    rows <- c(rows, list(
      c("Age band straddling a DSU band boundary",
        sum(ages$straddles, na.rm = TRUE), "midpoint assumed")))
  }

  # Drop the reasons that account for nothing.
  rows <- Filter(function(r) !grepl("^Not mapped", r[1L]) || as.numeric(r[2L]) > 0L,
                 rows)

  data.frame(
    what = vapply(rows, `[[`, character(1L), 1L),
    n    = as.numeric(vapply(rows, `[[`, character(1L), 2L)),
    note = vapply(rows, `[[`, character(1L), 3L),
    stringsAsFactors = FALSE
  )
}

# The column name offered for a method and value set, so the name says what
# the column holds: utility_<METHOD>_<CODE>, where METHOD is left out for a
# direct value set and is XW (crosswalk), XWR (reverse crosswalk) or DSU (the
# NICE DSU UK mapping), and CODE is the code of the value set the values are
# on -- the actual code, not its label. Before a value set is chosen the code
# is left out.
#
#   direct, CA  -> utility_CA        xwr, CA -> utility_XWR_CA
#   xw, DK      -> utility_XW_DK     uk      -> utility_DSU_GB
suggest_value_col <- function(method, code = NULL) {
  method <- method %||% "direct"
  if (identical(method, "uk")) code <- "GB"   # the UK value set it maps onto
  tag <- switch(method, xw = "XW", xwr = "XWR", uk = "DSU", NULL)
  code <- if (length(code) == 1L && !is.na(code) && nzchar(code))
    gsub("[^A-Za-z0-9_]", "_", code)
  paste(c("utility", tag, code), collapse = "_")
}

# Whether a name the user is about to use is free, an existing value column
# (which may be replaced) or another column of the data (which may not).
value_col_clash <- function(name, rv) {
  df <- rv$processed_data %||% rv$raw_data
  if (is.null(df) || !nzchar(name) || !name %in% names(df)) return("free")
  if (name %in% value_columns(rv)) "value column" else "data column"
}

# ── UI ────────────────────────────────────────────────────────────────────────

mod_values_ui <- function(id) {
  ns <- shiny::NS(id)
  page_shell(
    sidebar_title = "Calculate EQ-5D values",
    sidebar = shiny::uiOutput(ns("controls")),
    shiny::uiOutput(ns("main"))
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_values_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(input$goto_data, goto_page(session, "data"))
    shiny::observeEvent(input$goto_analysis, goto_page(session, "analysis"))

    version <- shiny::reactive({
      shiny::req(rv$mapping)
      rv$mapping$eq5d_version
    })

    is_uk <- shiny::reactive(identical(input$method, "uk"))

    # ── Sidebar ──────────────────────────────────────────────────────────────
    # The method-dependent controls are a separate output, so choosing a
    # method does not re-render the Method selector itself.
    output$controls <- shiny::renderUI({
      if (is.null(rv$mapping)) {
        return(shiny::tagList(
          hint("Select your EQ-5D variables first."),
          shiny::actionButton(ns("goto_data"), "Go to Data",
                              class = "btn-primary w-100",
                              icon = shiny::icon("arrow-right"))
        ))
      }
      # Adding a column changes rv$mapping, which re-renders these controls.
      # Keep what the user chose rather than resetting to the defaults.
      keep <- shiny::isolate(list(method = input$method,
                                  col_name = input$col_name))
      method <- keep$method %||% "direct"
      shiny::tagList(
        shiny::selectInput(ns("method"), "Method",
                           choices = get_utility_method_choices(version()),
                           selected = method),
        shiny::uiOutput(ns("method_opts")),
        shiny::textInput(ns("col_name"), "New column name",
                         value = keep$col_name %||% suggested()),
        shiny::uiOutput(ns("name_note")),
        shiny::actionButton(ns("add"), "Add value column",
                            class = "btn-primary w-100",
                            icon = shiny::icon("plus")),
        # The EQ-5D-Y-3L is offered a direct value set only, so its note
        # describes only that.
        if (identical(version(), "Y3L")) disclosure(
          "About the methods",
          shiny::p("A ", shiny::strong("direct"), " value set values the ",
                   "responses with a value set published for that instrument."),
          shiny::p("There is no crosswalk or mapping between the EQ-5D-Y-3L ",
                   "and another instrument, so EQ-5D-Y-3L responses are ",
                   "valued only with an EQ-5D-Y-3L value set."),
          shiny::p(shiny::strong("Column names."), " The name offered is ",
                   shiny::code("utility_"), ", then the code of the value ",
                   "set: ", shiny::code("utility_DE"), ". Type your own and ",
                   "it is kept.")
        ) else disclosure(
          "About the methods",
          shiny::p("A ", shiny::strong("direct"), " value set values the ",
                   "responses with a value set published for that instrument."),
          shiny::p(shiny::strong("Crosswalk"), " (5L responses, 3L values) ",
                   "uses van Hout et al. (2012); ",
                   shiny::strong("reverse crosswalk"), " (3L responses, 5L ",
                   "values) uses van Hout et al. (2021). Neither depends on ",
                   "anything but the health state."),
          shiny::p("The ", shiny::strong("NICE DSU UK mapping"), " is for the ",
                   "UK only and does depend on the respondent's age and sex, ",
                   "so it asks for those columns."),
          shiny::p(shiny::strong("Column names."), " The name offered is ",
                   shiny::code("utility_"), ", then the method -- ",
                   shiny::code("XW"), ", ", shiny::code("XWR"), " or ",
                   shiny::code("DSU"), ", nothing for a direct value set -- ",
                   "then the code of the value set the values are on: ",
                   shiny::code("utility_CA"), ", ", shiny::code("utility_XWR_CA"),
                   ", ", shiny::code("utility_DSU_GB"), ". Type your own and ",
                   "it is kept.")
        )
      )
    })

    # The value sets on offer are the target instrument's, not the data's: a
    # crosswalk values one instrument's responses with the other's value set.
    # See value_set_version() in global.R.
    vs_version <- shiny::reactive(value_set_version(input$method, version()))

    output$method_opts <- shiny::renderUI({
      shiny::req(rv$mapping)
      if (identical(input$method, "uk")) return(uk_inputs(ns, rv, input))

      ver <- vs_version()
      ch  <- get_country_choices(ver)
      # A set chosen under the previous method may not exist for this one, so
      # it is dropped rather than carried over into a call that would fail.
      prev <- shiny::isolate(input$country) %||% rv$mapping$country %||% ""
      keep <- nzchar(prev) && prev %in% ch

      # The label names the instrument, so the list the user is looking at is
      # not left to inference. output$direction says it in a sentence.
      shiny::selectizeInput(
        ns("country"), paste0(instrument_label(ver), " value set"),
        choices = c("(select)" = "", ch),
        selected = if (keep) prev else "")
    })

    # Offer a column name that says what the column will hold, and keep it in
    # step with the method and value set -- unless the user has typed a name
    # of their own, which is kept. A name counts as theirs when it is not the
    # last one offered; clearing the field hands it back.
    suggested <- shiny::reactive(
      suggest_value_col(input$method,
                        if (!identical(input$method, "uk")) input$country))
    last_offered <- shiny::reactiveVal(NULL)
    shiny::observe({
      new <- suggested()
      cur <- shiny::isolate(input$col_name) %||% ""
      prev <- shiny::isolate(last_offered())
      if (!nzchar(trimws(cur)) || identical(cur, prev) ||
          (is.null(prev) && grepl("^utility", cur)))
        shiny::updateTextInput(session, "col_name", value = new)
      last_offered(new)
    })

    # Say what adding under this name will do to an existing column.
    output$name_note <- shiny::renderUI({
      name <- trimws(input$col_name %||% "")
      switch(value_col_clash(name, rv),
        "value column" = {
          n <- sum(vapply(rv$results %||% list(), uses_value_column,
                          logical(1L), column = name))
          note("warning", paste0(
            "A value column named \"", name, "\" already exists. Adding ",
            "replaces it",
            if (n > 0L) sprintf(" and removes the %d saved result%s that used it",
                                n, if (n == 1L) "" else "s"),
            "."))
        },
        "data column" = note("error", paste0(
          "\"", name, "\" is a column of your data. Choose another name.")),
        NULL)
    })

    # ── Age handling for the UK mapping ──────────────────────────────────────
    ages <- shiny::reactive({
      df <- rv$processed_data %||% rv$raw_data
      shiny::req(df, input$age_col)
      if (!nzchar(input$age_col)) return(NULL)
      x <- df[[input$age_col]]
      if (identical(input$age_kind, "exact")) {
        list(value = eq5dsuite:::.parse_number(x),
             is_banded = FALSE, straddles = rep(FALSE, length(x)))
      } else {
        mid <- eq5dsuite:::eq5d_age_band_midpoint(x)
        list(value = as.numeric(mid),
             is_banded = isTRUE(attr(mid, "banded")),
             straddles = attr(mid, "straddles"))
      }
    })

    output$male_value_ui <- shiny::renderUI({
      df <- rv$processed_data %||% rv$raw_data
      shiny::req(df)
      col <- input$sex_col
      if (is.null(col) || !nzchar(col) || !col %in% names(df)) return(NULL)
      vals <- sort(unique(as.character(df[[col]])))
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0L) return(NULL)
      guess <- vals[tolower(vals) %in% c("male", "m", "1", "men")][1L]
      chosen <- shiny::isolate(input$male_value)
      sel <- if (!is.null(chosen) && chosen %in% vals) chosen
             else if (is.na(guess)) vals[1L] else guess
      shiny::selectInput(ns("male_value"), "Value meaning male",
                         choices = vals, selected = sel)
    })

    # ── Main area ────────────────────────────────────────────────────────────
    output$main <- shiny::renderUI({
      if (is.null(rv$mapping)) {
        return(bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, hint(
          "No variables selected yet. Once the EQ-5D dimensions are selected on the ",
          "Data page, you can value them here."))))
      }
      shiny::tagList(
        shiny::uiOutput(ns("direction")),
        shiny::uiOutput(ns("band_warning")),
        shiny::uiOutput(ns("status")),
        shiny::uiOutput(ns("coverage_ui")),
        bslib::card(
          full_screen = TRUE, fill = FALSE,
          bslib::card_header("Value columns in the dataset"),
          bslib::card_body(fillable = FALSE, DT::DTOutput(ns("preview")))
        ),
        shiny::uiOutput(ns("download_ui")),
        shiny::uiOutput(ns("next_step"))
      )
    })

    # What the chosen method does, in one line, before anything is run.
    output$direction <- shiny::renderUI({
      shiny::req(rv$mapping)
      v <- version()
      if (isTRUE(is_uk())) {
        d <- uk_direction(v)
        return(shiny::tagList(
          note("info", paste0(
            "NICE DSU UK mapping: ", d$from, " → ", d$to,
            ". Your data are ", d$from, " responses, so this produces UK ",
            d$to, " values, using eq5dsuite's ", d$fn, "(). The mapping ",
            "depends on the respondent's age and sex as well as the health ",
            "state.")),
          if (identical(v, "5L"))
            note("warning",
                 "Since NICE's interim methods statement of 27 August 2026
                  (PMG51), EQ-5D-5L data are valued directly with the UK
                  EQ-5D-5L value set — choose Direct and value set GB. This
                  mapping is for topics started before that date, for which
                  NICE says 5L data should still be mapped to 3L utility
                  values, and for reproducing earlier analyses.")
        ))
      }
      lbl <- switch(input$method %||% "direct",
        xw  = paste0("Crosswalk: EQ-5D-5L → EQ-5D-3L. Your 5L responses ",
                     "are valued on a 3L value set, after van Hout et al. ",
                     "(2012)."),
        xwr = paste0("Reverse crosswalk: EQ-5D-3L → EQ-5D-5L. Your 3L ",
                     "responses are valued on a 5L value set, after van Hout ",
                     "et al. (2021)."),
        paste0("Direct: your ", instrument_label(v), " responses are ",
               "valued on an ", instrument_label(v), " value set."))
      note("info", lbl)
    })

    output$band_warning <- shiny::renderUI({
      if (!isTRUE(is_uk())) return(NULL)
      a <- tryCatch(ages(), error = function(e) NULL)
      if (is.null(a) || !isTRUE(a$is_banded)) return(NULL)
      n_str <- sum(a$straddles, na.rm = TRUE)
      note("warning",
        shiny::strong("Ages are banded, so this is an approximation."), " ",
        paste0(
          "The mapping needs an age, and your age column holds bands, so the ",
          "midpoint of each band is assumed. The DSU's bands begin at 35, 45, ",
          "55 and 65, and those are exactly the midpoints of ten-year bands, ",
          "so every band that straddles a DSU boundary is placed in the "),
        shiny::strong("upper"),
        paste0(" band — ", format(n_str, big.mark = ","), " of ",
               format(length(a$straddles), big.mark = ","), " rows here. ",
               "Results are therefore an approximation; use exact ages where ",
               "they are available."))
    })

    # The value columns live on rv, not here: the Analysis page offers the
    # same list in its Utility column selector.
    value_cols <- shiny::reactive(value_columns(rv))
    coverage   <- shiny::reactiveVal(NULL)
    last_added <- shiny::reactiveVal(NULL)

    output$status <- shiny::renderUI({
      cols <- value_cols()
      if (length(cols) == 0L) {
        return(note("info",
          "No EQ-5D values yet. Choose a method on the left, then add a ",
          "column. You can add several, by different methods or value sets, ",
          "and compare them. Only the EQ-5D value analyses need them \u2014 ",
          "the EQ-5D profile and EQ VAS analyses can be run without."))
      }
      df <- rv$processed_data
      shiny::req(df)
      rows <- lapply(cols, function(cn) {
        if (!cn %in% names(df)) return(NULL)
        v <- df[[cn]]
        shiny::tags$tr(
          shiny::tags$td(shiny::tags$strong(cn)),
          shiny::tags$td(sprintf("%d of %d valued", sum(!is.na(v)), length(v))),
          shiny::tags$td(sprintf("mean %.3f", mean(v, na.rm = TRUE))),
          shiny::tags$td(sprintf("range %.3f to %.3f",
                                 min(v, na.rm = TRUE), max(v, na.rm = TRUE)))
        )
      })
      bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE,
        shiny::tags$table(class = "home-needs", shiny::tags$tbody(rows))
      ))
    })

    output$coverage_ui <- shiny::renderUI({
      cov <- coverage()
      if (is.null(cov)) return(NULL)
      bslib::card(
        fill = FALSE,
        bslib::card_header("Coverage"),
        bslib::card_body(fillable = FALSE,
          shiny::tags$table(class = "home-needs", shiny::tags$tbody(
            lapply(seq_len(nrow(cov)), function(i) shiny::tags$tr(
              shiny::tags$td(cov$what[i]),
              shiny::tags$td(format(cov$n[i], big.mark = ","), class = "num"),
              shiny::tags$td(cov$note[i], class = "hint")))
          ))
        ))
    })

    # Shown whether or not values were added: the profile and EQ VAS
    # analyses do not need any.
    output$next_step <- shiny::renderUI({
      shiny::req(rv$mapping)
      has_values <- length(value_cols()) > 0L
      shiny::actionButton(
        ns("goto_analysis"),
        if (has_values) "Go to Analysis" else "Skip to Analysis",
        class = if (has_values) "btn-primary" else "btn-outline-secondary",
        icon = shiny::icon("arrow-right"))
    })

    output$preview <- DT::renderDT({
      df <- rv$processed_data
      shiny::req(df)
      vcols <- value_cols()
      keep <- intersect(c("id", "fu", DIMS_STD,
                          if (isTRUE(is_uk())) c(input$age_col, input$sex_col),
                          vcols),
                        names(df))
      show <- df[, keep, drop = FALSE]
      # Lead with the rows the column just added could value: example_data's
      # first rows have no age or sex, so an unordered preview of a UK mapping
      # shows a column of blanks and looks as though nothing worked.
      newest <- last_added()
      if (!is.null(newest) && newest %in% keep)
        show <- show[order(is.na(show[[newest]])), , drop = FALSE]
      preview_table(head(show, 200L), vcols)
    })

    # ── Download the working dataset ─────────────────────────────────────────
    output$download_ui <- shiny::renderUI({
      if (is.null(rv$processed_data)) return(NULL)
      bslib::card(
        fill = FALSE,
        bslib::card_header("Download the working dataset"),
        bslib::card_body(fillable = FALSE,
          hint("Every column, including the EQ-5D values calculated here. ",
               "Values are written at full precision, not the three decimal ",
               "places the preview shows."),
          shiny::div(
            class = "btn-row",
            shiny::downloadButton(ns("download_csv"),  "CSV",
                                  class = "btn-outline-secondary"),
            # Offered only when writexl is installed. The button used to be
            # there regardless and fail when clicked (review AI05).
            if (has_package("writexl"))
              shiny::downloadButton(ns("download_xlsx"), "XLSX",
                                    class = "btn-outline-secondary"),
            shiny::downloadButton(ns("download_rds"),  "RDS",
                                  class = "btn-outline-secondary")
          ),
          if (!has_package("writexl"))
            hint("An XLSX download needs the writexl package: ",
                 shiny::code("install.packages(\"writexl\")", .noWS = "outside"),
                 ".")
        )
      )
    })

    dl_name <- function(ext) {
      paste0("eq5dsuite_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
    }

    output$download_csv <- shiny::downloadHandler(
      filename = function() dl_name("csv"),
      content = function(file) {
        shiny::req(rv$processed_data)
        utils::write.csv(rv$processed_data, file, row.names = FALSE)
      }
    )

    output$download_xlsx <- shiny::downloadHandler(
      filename = function() dl_name("xlsx"),
      content = function(file) {
        shiny::req(rv$processed_data)
        if (!requireNamespace("writexl", quietly = TRUE)) {
          stop("Package 'writexl' is required for XLSX export. ",
               "Install it with: install.packages('writexl')", call. = FALSE)
        }
        writexl::write_xlsx(rv$processed_data, file)
      }
    )

    output$download_rds <- shiny::downloadHandler(
      filename = function() dl_name("rds"),
      content = function(file) {
        shiny::req(rv$processed_data)
        saveRDS(rv$processed_data, file)
      }
    )

    # ── Add the column ───────────────────────────────────────────────────────
    shiny::observeEvent(input$add, {
      shiny::req(rv$mapping, rv$raw_data)
      uk <- isTRUE(is_uk())

      if (!uk && !nzchar(input$country %||% "")) {
        shiny::showNotification("Choose a value set first.",
                                type = "warning", duration = 4)
        return()
      }
      # The selector offers only value sets this method can use, so this
      # catches a stale selection rather than a bad choice -- but eq5d()'s
      # "No valid countries listed" would not say which of the two was wrong.
      if (!uk && !input$country %in% get_country_choices(vs_version())) {
        shiny::showNotification(
          paste0("That value set is not published for ", instrument_label(vs_version()),
                 ", which is what this method values responses with. ",
                 "Choose another."),
          type = "warning", duration = 6)
        return()
      }
      if (uk && (!nzchar(input$age_col %||% "") || !nzchar(input$sex_col %||% ""))) {
        shiny::showNotification(
          "The NICE DSU UK mapping needs an age column and a sex column.",
          type = "warning", duration = 5)
        return()
      }
      col_name <- trimws(input$col_name %||% "")
      if (!nzchar(col_name)) {
        shiny::showNotification("Give the new column a name.",
                                type = "warning", duration = 4)
        return()
      }

      df <- rv$processed_data %||%
    eq5dsuite:::eq5d_apply_mapping(rv$raw_data, rv$mapping)
      # Only a value column may be replaced. Any other column is data -- a
      # dimension, the EQ VAS, an ID -- and used to be overwritten silently.
      if (identical(value_col_clash(col_name, rv), "data column")) {
        shiny::showNotification(
          paste0('"', col_name, '" is a column of your data and cannot be ',
                 "replaced. Choose another name."),
          type = "error", duration = 8)
        return()
      }
      replacing <- col_name %in% names(df)

      tryCatch({
        if (uk) {
          a <- ages()
          shiny::req(a)
          male <- as.integer(as.character(df[[input$sex_col]]) == input$male_value)
          male[is.na(df[[input$sex_col]])] <- NA_integer_
          vals <- suppressWarnings(do.call(
            pkg_fn(uk_direction(rv$mapping$eq5d_version)$fn),
            list(x = df[, DIMS_STD, drop = FALSE], age = a$value, male = male)))
          coverage(uk_coverage(df, vals, a, male, rv$mapping))
        } else {
          vals <- compute_utility_col(df, input$method, input$country,
                                      rv$mapping$eq5d_version)
          coverage(NULL)
        }
        df[[col_name]] <- vals

        # Results saved from the column's previous values cannot be
        # reproduced once it is overwritten; see replace_value_column().
        dropped <- if (replacing) replace_value_column(rv, col_name) else 0L
        if (replacing)
          shiny::showNotification(
            paste0('Column "', col_name, '" already existed and was replaced',
                   if (dropped > 0L)
                     sprintf("; %d saved result%s that used it %s removed",
                             dropped, if (dropped == 1L) "" else "s",
                             if (dropped == 1L) "was" else "were"),
                   "."),
            type = "warning", duration = 8)

        new_mapping <- rv$mapping
        new_mapping$name_utility <- col_name
        if (!uk) new_mapping$country <- input$country
        rv$processed_data <- df
        rv$mapping        <- new_mapping
        rv$value_cols <- unique(c(rv$value_cols %||% character(0L), col_name))
        last_added(col_name)

        # What the generated script needs to reproduce this column.
        if (uk) {
          d <- uk_direction(rv$mapping$eq5d_version)
          record_step(rv, "value", replace = FALSE, method = "uk",
                      fn = d$fn, from = d$from, to = d$to, column = col_name,
                      age_col = input$age_col, sex_col = input$sex_col,
                      male_value = input$male_value,
                      banded = isTRUE(a$is_banded),
                      # The label -> age table the app actually used, so the
                      # generated script reproduces it exactly instead of
                      # parsing the labels a second time.
                      age_lookup = if (isTRUE(a$is_banded)) {
                        lab <- as.character(rv$processed_data[[input$age_col]])
                        keep <- !is.na(lab) & !is.na(a$value)
                        stats::setNames(a$value[keep], lab[keep])[
                          !duplicated(lab[keep])]
                      })
        } else {
          record_step(rv, "value", replace = FALSE, method = input$method,
                      method_label = names(get_utility_method_choices(
                        rv$mapping$eq5d_version))[
                          match(input$method,
                                get_utility_method_choices(rv$mapping$eq5d_version))],
                      fn = "eq5d", country = input$country,
                      version = switch(input$method,
                                       direct = rv$mapping$eq5d_version,
                                       xw = "XW", xwr = "XWR"),
                      column = col_name)
        }

        shiny::showNotification(
          sprintf('Column "%s" added (%d of %d valued).',
                  col_name, sum(!is.na(vals)), length(vals)),
          type = "message", duration = 4)
      }, error = function(e) {
        shiny::showNotification(paste("Error computing values:",
                                      conditionMessage(e)),
                                type = "error", duration = 8)
      })
    })
  })
}

# The age and sex pickers the NICE DSU UK mapping needs.
uk_inputs <- function(ns, rv, input) {
  df <- rv$processed_data %||% rv$raw_data
  if (is.null(df)) return(hint("Validate your data first."))
  cols <- names(df)
  m <- rv$mapping

  keep <- shiny::isolate(list(age = input$age_col, kind = input$age_kind,
                              sex = input$sex_col))
  pick <- function(chosen, suggested) {
    if (!is.null(chosen) && nzchar(chosen) && chosen %in% cols) return(chosen)
    if (!is.null(suggested) && suggested %in% cols) suggested else ""
  }
  age_sel <- pick(keep$age, m$name_age %||% "ageband")
  sex_sel <- pick(keep$sex, m$name_sex %||% "gender")

  # Offer "Bands" by default only when the column actually looks banded.
  looks_banded <- nzchar(age_sel) &&
    isTRUE(attr(eq5dsuite:::eq5d_age_band_midpoint(df[[age_sel]]), "banded"))

  shiny::tagList(
    shiny::selectInput(ns("age_col"), "Age column",
                       choices = c("(none)" = "", cols), selected = age_sel),
    shiny::radioButtons(ns("age_kind"), "Age recorded as",
                        choices = c("Bands" = "bands", "Exact age" = "exact"),
                        selected = keep$kind %||%
                          (if (looks_banded) "bands" else "exact"),
                        inline = TRUE),
    shiny::selectInput(ns("sex_col"), "Sex column",
                       choices = c("(none)" = "", cols), selected = sex_sel),
    shiny::uiOutput(ns("male_value_ui"))
  )
}
