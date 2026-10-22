# ui.R — main UI definition

navbar_brand <- shiny::tags$span(
  class = "brand",
  shiny::tags$img(src = "logo.png", height = "24px"),
  shiny::tags$strong("eq5dsuite")
)

nav_item <- function(icon, label, value, content) {
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon(icon), " ", label),
    value = value,
    content
  )
}

ui <- bslib::page_navbar(
  id    = "main_nav",
  title = navbar_brand,
  # The house style, after the EQ-5D Aggregate Utility Transporter: a warm
  # off-white ground, terracotta for the things you act on, DM Sans for text
  # and Fraunces for headings. The typefaces are bundled in WWW/fonts and
  # declared in styles.css, so nothing is fetched from a font service; these
  # are plain family names, not bslib::font_google(), for that reason.
  theme = bslib::bs_theme(
    version      = 5,
    bg           = "#FAF8F5",
    fg           = "#3A3330",
    primary      = "#A87968",
    secondary    = "#7A9BB5",
    base_font    = c("DM Sans", "system-ui", "-apple-system", "Segoe UI",
                     "Helvetica Neue", "Arial", "sans-serif"),
    heading_font = c("Fraunces", "Georgia", "Iowan Old Style",
                     "Palatino Linotype", "Times New Roman", "serif"),
    "font-size-base"     = "0.95rem",
    "border-radius"      = "10px",
    "border-radius-sm"   = "8px",
    "border-radius-lg"   = "16px",
    "border-color"       = "#ECE5DB",
    "headings-font-weight" = 600
  ),
  # A light bar, not the dark one bslib gives a dark primary, and the page you
  # are on marked by a filled pill rather than an underline. bg/inverse are
  # deprecated in bslib 0.9; navbar_options() is the way now.
  navbar_options = bslib::navbar_options(
    bg = "#F5F0EA", theme = "light", underline = FALSE
  ),
  window_title = "eq5dsuite",
  # Pages scroll normally; nothing scrolls inside a box.
  fillable = FALSE,
  header = shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    # The idle watch is loaded only when it is going to be used.
    if (ONLINE$enabled)
      shiny::tags$script(src = "idle.js")
  ),

  # In the order the work is done.
  nav_item("house",      "Home",       "home",       mod_home_ui("home")),
  nav_item("upload",     "Data",       "data",       mod_data_ui("data")),
  nav_item("list-check", "Validation", "validation", mod_validation_ui("validation")),
  nav_item("calculator", "Calculate EQ-5D values", "values", mod_values_ui("values")),
  nav_item("chart-bar",  "Analysis",   "analysis",   mod_analysis_ui("analysis")),
  nav_item("table-list", "Results and export", "results", mod_export_ui("export")),

  bslib::nav_spacer(),
  # "eq5dsuite vX", and an icon when CRAN has a newer version.
  bslib::nav_item(mod_version_ui("version"))
)
