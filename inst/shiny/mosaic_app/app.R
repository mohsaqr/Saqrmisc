library(shiny)
library(Saqrmisc)
library(DT)

# Allow uploads up to 100 MB (default is 5 MB).
# Note: if served behind nginx, also set `client_max_body_size 100m;`
# in the server block — otherwise nginx returns 413 before Shiny sees the request.
options(shiny.maxRequestSize = 100 * 1024^2)

# `%||%` is base R only from 4.4.0 and shiny does not export it. The deploy
# server's R version is not guaranteed, so define it when absent.
if (!exists("%||%", envir = baseenv())) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

# ---- built-in demo data ----
# A deterministic education example (Specialization x Final evaluation) so the
# app is usable with one click and mirrors the package documentation example.
.make_demo <- function() {
  spec_levels <- c("Engineering", "Medicine", "Nursing",
                   "Social Sciences", "Kindergarten")
  # Per-specialization pass probabilities drive a visible association.
  pass_prob <- c(Engineering = 0.91, Medicine = 0.94, Nursing = 0.87,
                 `Social Sciences` = 0.85, Kindergarten = 0.50)
  n_per <- c(Engineering = 4709, Medicine = 2925, Nursing = 6606,
             `Social Sciences` = 2190, Kindergarten = 15)
  # Online study is unevenly distributed across specializations AND carries its
  # own pass penalty, so the pooled Specialization effect is partly confounded
  # by study mode. That makes the Stratify control show something real.
  online_prob <- c(Engineering = 0.20, Medicine = 0.10, Nursing = 0.35,
                   `Social Sciences` = 0.65, Kindergarten = 0.70)
  online_penalty <- 0.13
  set.seed(42)
  spec <- rep(spec_levels, times = n_per[spec_levels])
  mode <- ifelse(stats::runif(length(spec)) < online_prob[spec], "Online", "Campus")
  p <- pass_prob[spec] - online_penalty * (mode == "Online")
  evals <- ifelse(stats::runif(length(spec)) < p, "Pass", "Fail")
  # A cohort year with no association, so the app also demonstrates the
  # "stratifier that changes nothing" case.
  year <- sample(c("2022", "2023", "2024"), length(spec), replace = TRUE)
  data.frame(
    Specialization_EN   = factor(spec, levels = spec_levels),
    Final_Evaluation_EN = factor(evals, levels = c("Fail", "Pass")),
    Study_Mode          = factor(mode, levels = c("Campus", "Online")),
    Cohort_Year         = factor(year),
    stringsAsFactors = FALSE
  )
}
demo <- .make_demo()

# ---- helper: compute the analysis (statistics only) ----
# Only the test-related inputs (min_count, test, percentages, variable labels)
# go here — plot styling is applied later and reactively via plot(). The
# mosaic_analysis() call still draws once as a side effect, so we absorb that
# into a null device; the app re-renders the plot itself from the returned
# object, so changing a style control never re-runs the test.
.compute_mosaic <- function(data, var1, var2, opts) {
  grDevices::pdf(NULL)
  on.exit(if (grDevices::dev.cur() > 1L) grDevices::dev.off(), add = TRUE)
  # Warnings are collected rather than suppressed: "expected counts < 5" and
  # "strata dropped" are exactly what a user needs to see before reading the
  # panels, so they are surfaced in the UI instead of being swallowed.
  warns <- character(0)
  res <- tryCatch(
    withCallingHandlers(
      mosaic_analysis(
        data, var1, var2,
        by               = opts$by,
        min_count        = opts$min_count,
        var1_label       = opts$var1_label,
        var2_label       = opts$var2_label,
        show_percentages = opts$show_percentages,
        percentage_base  = opts$percentage_base,
        use_fisher       = opts$use_fisher,
        min_stratum_n    = opts$min_stratum_n,
        p_adjust         = opts$p_adjust,
        seed             = opts$seed,
        verbose          = FALSE
      ),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }),
    error = function(e) structure(conditionMessage(e), class = "mosaic_error")
  )
  if (inherits(res, "mosaic_error"))
    return(list(status = "error", message = as.character(res), value = NULL,
                warnings = warns))
  list(status = "ok", value = res, message = NULL, warnings = warns)
}


# ---- UI ----

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-size: 14px; }

    /* ── Title bar ── */
    .app-title {
      padding: 16px 24px 12px;
      background: linear-gradient(135deg, #1a56db 0%, #2c7be5 60%, #4facfe 100%);
      margin-bottom: 0;
    }
    .app-title-main {
      display: block;
      font-size: 28px; font-weight: 700; letter-spacing: -0.5px;
      color: #fff; font-family: 'Segoe UI', Helvetica, Arial, sans-serif;
    }
    .app-title-sub {
      display: block;
      font-size: 13px; font-weight: 400; letter-spacing: 0.5px;
      color: rgba(255,255,255,0.80); margin-top: 2px;
      font-family: 'Segoe UI', Helvetica, Arial, sans-serif;
    }

    /* ── Sidebar ── */
    .sidebar-panel { background: #f8f9fa; padding: 15px; border-radius: 6px; }
    .section-header { font-weight: 600; margin-top: 14px; margin-bottom: 4px;
                      color: #333; border-bottom: 1px solid #dee2e6; padding-bottom: 3px; }
    .btn-primary  { background-color: #2c7be5; border-color: #2c7be5; }
    .btn-run      { margin-top: 6px; margin-bottom: 10px; }
    .export-strip { margin-top: 18px; padding: 12px; background: #f0f4fb;
                    border-radius: 6px; border: 1px solid #d0dff5; }
    .export-strip h5 { margin-top: 0; margin-bottom: 10px; color: #2c7be5; }

    /* ── Help page ── */
    .help-section {
      background: #fff; border: 1px solid #e4e8ee; border-radius: 8px;
      padding: 20px 24px; margin-bottom: 18px;
    }
    .help-section h4 {
      color: #1a56db; font-weight: 700; margin-top: 0; margin-bottom: 10px;
      border-bottom: 2px solid #e8eff9; padding-bottom: 6px;
    }
    .help-section p, .help-section li { color: #444; line-height: 1.65; }
    .help-section code {
      background: #f0f4fb; color: #c7254e;
      padding: 1px 5px; border-radius: 3px; font-size: 92%;
    }
    .measure-table { width: 100%; border-collapse: collapse; font-size: 13px; }
    .measure-table th {
      background: #1a56db; color: #fff;
      padding: 7px 12px; text-align: left; font-weight: 600;
    }
    .measure-table td { padding: 6px 12px; border-bottom: 1px solid #eee; }
    .measure-table tr:last-child td { border-bottom: none; }
    .measure-table tr:nth-child(even) td { background: #f8f9ff; }
    .badge-fmt {
      display: inline-block; padding: 2px 8px; border-radius: 12px;
      font-size: 11px; font-weight: 600; margin-right: 4px;
      background: #e8eff9; color: #1a56db;
    }
    .stat-callout {
      background: #f0f4fb; border-left: 3px solid #2c7be5;
      padding: 10px 14px; border-radius: 4px; margin-bottom: 14px;
      font-size: 13px; color: #2c3e50;
    }
    .warn-callout {
      background: #fff8e6; border-left: 3px solid #e0a800;
      padding: 10px 14px; border-radius: 4px; margin-bottom: 14px;
      font-size: 13px; color: #6b5200;
    }
    .warn-callout ul { margin: 6px 0 0; padding-left: 18px; }
    .strat-box {
      background: #f4f8ff; border: 1px solid #d0dff5;
      border-radius: 6px; padding: 10px 12px; margin-top: 8px;
    }

    /* ── Footer ── */
    html, body { height: 100%; }
    body { display: flex; flex-direction: column; min-height: 100vh; }
    .container-fluid { flex: 1; }
    .app-footer {
      padding: 18px 24px 14px;
      margin-top: 30px;
      border-top: 2px solid #e4e8ee;
      text-align: center;
      background: #fafbfd;
    }
    .app-footer .footer-authors {
      font-size: 15px; font-weight: 600; color: #2c3e50;
      margin-bottom: 6px;
    }
    .app-footer .footer-authors a { color: #1a56db; text-decoration: none; }
    .app-footer .footer-authors a:hover { text-decoration: underline; }
    .app-footer .footer-authors .sep { margin: 0 10px; color: #adb5bd; font-weight: 400; }
    .app-footer .footer-refs { font-size: 11px; color: #adb5bd; line-height: 2; }
    .app-footer .footer-refs a {
      color: #adb5bd; text-decoration: none; border-bottom: 1px dotted #ced4da;
    }
    .app-footer .footer-refs a:hover { color: #495057; border-bottom-color: #495057; }
    .app-footer .footer-refs .sep { margin: 0 8px; color: #dee2e6; }
  "))),

  tags$div(class = "app-title",
    tags$span(class = "app-title-main", "mosaic"),
    tags$span(class = "app-title-sub", "Categorical Association Explorer")
  ),

  tabsetPanel(
    id = "tabs",

    # ---- Start: load data + set up the analysis ----
    tabPanel("Start", value = "start",
      br(),
      fluidRow(
        column(5,
          div(class = "help-section",
            tags$h4("1 · Data"),
            radioButtons("data_source", "Data source",
                         choices = c("Built-in: demo" = "demo",
                                     "Upload CSV"      = "upload"),
                         selected = "demo"),
            conditionalPanel(
              condition = "input.data_source == 'upload'",
              fileInput("file", "CSV file", accept = c(".csv", "text/csv"),
                        placeholder = "Choose CSV file")
            ),
            div(class = "section-header", "Variables"),
            uiOutput("ui_var1"),
            uiOutput("ui_var2"),

            div(class = "section-header", "Stratify / facet"),
            uiOutput("ui_by"),
            helpText(paste(
              "Optional. Splits the table by a third variable and fits each",
              "group separately \u2014 one mosaic panel and one test per group.",
              "Use it to check whether the association holds in every subgroup.")),
            conditionalPanel(
              condition = "input.by_sel != '__none__'",
              div(class = "strat-box",
                fluidRow(
                  column(6, numericInput("min_stratum_n", "Min group size",
                                         value = 30, min = 0, step = 5)),
                  column(6, selectInput("p_adjust", "p correction",
                                        choices = c("BH" = "BH",
                                                    "Bonferroni" = "bonferroni",
                                                    "Holm" = "holm",
                                                    "None" = "none"),
                                        selected = "BH"))
                ),
                helpText(paste(
                  "One test per group is a multiple-testing problem, so a",
                  "corrected p-value is reported next to the raw one."))
              )
            )
          )
        ),
        column(4,
          div(class = "help-section",
            tags$h4("2 · Analysis options"),
            radioButtons("test", "Statistical test",
                         choices = c("Chi-square"     = "chisq",
                                     "Fisher's exact" = "fisher"),
                         selected = "chisq"),
            numericInput("min_count", "Min category count", value = 10, min = 1, step = 1),
            checkboxInput("show_pct", "Show percentages in table", value = TRUE),
            conditionalPanel(
              condition = "input.show_pct == true",
              selectInput("pct_base", "Percentage base",
                          choices = c("total", "row", "column"), selected = "total")
            ),
            conditionalPanel(
              condition = "input.test == 'fisher'",
              numericInput("seed", "Random seed", value = 42, min = 1, step = 1),
              helpText(paste(
                "Fisher's p-value here is estimated by Monte Carlo, so it is",
                "stochastic. A fixed seed makes a run reproducible."))
            )
          )
        ),
        column(3,
          div(class = "help-section",
            tags$h4("3 · Run"),
            tags$p("Then open the ", tags$strong("Mosaic plot"), " tab — its ",
                   "visualization controls live on that page and update instantly, ",
                   "with no need to re-run."),
            actionButton("run", "Run analysis", class = "btn-primary btn-run",
                         width = "100%", icon = icon("play"))
          )
        )
      )
    ),

    # ---- Summary ----
    tabPanel("Summary",
      br(),
      uiOutput("run_warnings"),
      h5("Statistical summary"),
      DTOutput("stats_table"),
      br(),
      h5("Consolidated frequency table"),
      DTOutput("consolidated_table"),
      uiOutput("export_strip")
    ),

    # ---- Strata (only meaningful for a stratified run) ----
    tabPanel("Strata", value = "strata",
      br(),
      uiOutput("strata_page")
    ),

    # ---- Residuals ----
    tabPanel("Residuals",
      br(),
      div(class = "stat-callout",
          "Standardized Pearson residuals. Values > |2| indicate a cell that deviates significantly from the expected count under independence."),
      uiOutput("residuals_note"),
      DTOutput("residuals_table")
    ),

    # ---- Mosaic plot: visualization controls live on this page ----
    tabPanel("Mosaic plot", value = "plot",
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          class = "sidebar-panel",

          # ---- Plot options (live) ----
          div(class = "section-header", "Plot options"),
          radioButtons("plot_style", "Style",
                       choices = c("Flat (ggplot2)" = "flat",
                                   "Classic (vcd)"  = "classic"),
                       selected = "flat", inline = TRUE),
          textInput("title", "Title", value = ""),

          # Flat-style controls
          conditionalPanel(
            condition = "input.plot_style == 'flat'",
            selectInput("tile_label", "Tile content",
                        choices = c("Count"           = "count",
                                    "Percent"         = "percent",
                                    "Count (percent)" = "count_percent",
                                    "Residual"        = "residual",
                                    "Category"        = "category",
                                    "None"            = "none"),
                        selected = "count"),
            numericInput("label_size", "Tile label size", value = 3.5, min = 1, max = 8, step = 0.5),

            div(class = "section-header", "Category labels"),
            fluidRow(
              column(6, selectInput("col_label_side", "Columns",
                          choices = c("top", "bottom", "both", "none"), selected = "top")),
              column(6, selectInput("col_label_angle", "Columns angle",
                          choices = c("Horizontal" = "0", "Vertical" = "90"), selected = "0"))
            ),
            fluidRow(
              column(6, selectInput("row_label_side", "Rows",
                          choices = c("left", "right", "both", "none"), selected = "left")),
              column(6, selectInput("row_label_angle", "Rows angle",
                          choices = c("Horizontal" = "0", "Vertical" = "90"), selected = "0"))
            )
          ),
          # Classic-style controls
          conditionalPanel(
            condition = "input.plot_style == 'classic'",
            checkboxInput("show_varnames", "Show variable-name titles", value = FALSE),
            numericInput("fontsize", "Font size", value = 8, min = 4, max = 24, step = 1),
            textInput("var1_label", "Row label (optional)",    value = ""),
            textInput("var2_label", "Column label (optional)", value = "")
          ),

          # ---- Legend ----
          div(class = "section-header", "Legend"),
          checkboxInput("show_legend", "Show legend", value = TRUE),
          conditionalPanel(
            condition = "input.show_legend == true && input.plot_style == 'flat'",
            selectInput("legend_position", "Position",
                        choices = c("right", "left", "top", "bottom"), selected = "right"),
            sliderInput("legend_size", "Legend size", min = 0.3, max = 1.5,
                        value = 0.7, step = 0.05),
            textInput("legend_title", "Legend title", value = "Std.\nresidual")
          ),

          # ---- Panels (stratified runs only) ----
          conditionalPanel(
            condition = "output.is_stratified === true",
            div(class = "section-header", "Panels"),
            numericInput("facet_ncol", "Panel columns (0 = auto)",
                         value = 0, min = 0, max = 8, step = 1),
            checkboxInput("facet_show_n", "Show group size in panel title",
                          value = TRUE)
          ),

          # ---- Plot size ----
          # Two ways to size a panel grid: fix the whole canvas and let the
          # panels shrink as more are added, or fix each panel and let the
          # canvas grow. The second keeps every panel equally readable, which
          # is usually what you want once there are more than two or three.
          div(class = "section-header", "Plot size"),
          radioButtons("size_mode", "Size by",
                       choices = c("Whole canvas" = "canvas",
                                   "Each panel"   = "panel"),
                       selected = "canvas", inline = TRUE),

          conditionalPanel(
            condition = "input.size_mode == 'canvas'",
            sliderInput("plot_w", "Canvas width (px)",  min = 400, max = 2400,
                        value = 900, step = 20, ticks = FALSE),
            sliderInput("plot_h", "Canvas height (px)", min = 300, max = 2000,
                        value = 620, step = 20, ticks = FALSE)
          ),
          conditionalPanel(
            condition = "input.size_mode == 'panel'",
            sliderInput("panel_w", "Panel width (px)",  min = 200, max = 1000,
                        value = 380, step = 10, ticks = FALSE),
            sliderInput("panel_h", "Panel height (px)", min = 160, max = 900,
                        value = 340, step = 10, ticks = FALSE),
            uiOutput("canvas_readout")
          )
        ),
        mainPanel(
          width = 9,
          uiOutput("plot_ui")
        )
      )
    ),

    # ---- Export ----
    tabPanel("Export",
      br(),
      h5("Plot"),
      downloadButton("dl_png", "PNG"),
      " ",
      downloadButton("dl_pdf", "PDF"),
      br(), br(),
      h5("Tables"),
      downloadButton("dl_resid", "Residuals CSV"),
      " ",
      downloadButton("dl_summary", "Summary CSV"),
      uiOutput("dl_strata_ui")
    ),

    # ---- Help (reference) ----
    tabPanel("Help", value = "help",
          br(),
          fluidRow(
            column(6,
              div(class = "help-section",
                tags$h4("\U0001f4c2 Input format"),
                tags$p("Upload a CSV with (at least) ", tags$strong("two categorical columns"),
                       " — one becomes the rows of the contingency table, the other the columns. ",
                       "Each row of the CSV is one observation."),
                tags$table(class = "measure-table",
                  tags$thead(tags$tr(tags$th("Specialization_EN"), tags$th("Final_Evaluation_EN"))),
                  tags$tbody(
                    tags$tr(tags$td("Engineering"),     tags$td("Pass")),
                    tags$tr(tags$td("Nursing"),         tags$td("Fail")),
                    tags$tr(tags$td("Medicine"),        tags$td("Pass")),
                    tags$tr(tags$td("Social Sciences"), tags$td("Pass"))
                  )
                )
              ),
              div(class = "help-section",
                tags$h4("\U0001f4ca What you get"),
                tags$ul(
                  tags$li(tags$strong("Summary"), " — chi-square/Fisher result, Cramer's V effect size, and a consolidated observed/expected/percentage table."),
                  tags$li(tags$strong("Residuals"), " — standardized Pearson residuals; values beyond ±2 mark cells that deviate significantly from independence."),
                  tags$li(tags$strong("Mosaic plot"), " — tile areas proportional to cell counts, shaded by residual (blue = more than expected, red = fewer).")
                )
              )
            ),
            column(6,
              div(class = "help-section",
                tags$h4("\U0001f9ea Statistical tests"),
                tags$table(class = "measure-table",
                  tags$thead(tags$tr(tags$th("Test"), tags$th("Best for"))),
                  tags$tbody(
                    tags$tr(tags$td(tags$strong("Chi-square")),     tags$td("General-purpose test of independence with adequate expected counts (≥ 5 per cell).")),
                    tags$tr(tags$td(tags$strong("Fisher's exact")), tags$td("Small samples or sparse tables; computed by Monte Carlo simulation for larger tables."))
                  )
                )
              ),
              div(class = "help-section",
                tags$h4("\U0001f4cf Cramer's V effect size"),
                tags$p("Reported with a df-adjusted interpretation (negligible / small / medium / large), following Cohen's guidelines."),
                tags$div(class = "stat-callout",
                  "Cramer's V ranges 0–1. It measures association strength independent of sample size, so it complements the p-value (which only tells you whether an association exists, not how strong it is)."
                )
              ),
              div(class = "help-section",
                tags$h4("\U0001f9ec Stratify / facet by"),
                tags$p("Pick a third variable on the Start page to split the table by it. ",
                       "Each group is fitted ", tags$strong("separately"),
                       " \u2014 its own contingency table, its own test, its own effect size \u2014 ",
                       "and the plot becomes one mosaic panel per group."),
                tags$p("This is the standard check for ", tags$strong("effect modification"),
                       " and ", tags$strong("Simpson's paradox"), ": an association in the ",
                       "pooled table can weaken, vanish, or reverse inside every subgroup."),
                tags$table(class = "measure-table",
                  tags$thead(tags$tr(tags$th("Row on the Strata tab"), tags$th("Meaning"))),
                  tags$tbody(
                    tags$tr(tags$td(tags$strong("pooled")),
                            tags$td("The test ignoring the stratifier \u2014 what you get without stratifying.")),
                    tags$tr(tags$td(tags$strong("conditional")),
                            tags$td("Cochran\u2013Mantel\u2013Haenszel: the association holding the stratifier fixed."))
                  )
                ),
                tags$div(class = "stat-callout",
                  paste("A pooled result much stronger than every per-group result means",
                        "the stratifier is confounding the association. Per-group results",
                        "that differ in size or direction mean the stratifier modifies it.")),
                tags$ul(
                  tags$li(tags$strong("Same categories everywhere"), " \u2014 the minimum-count filter is applied to the pooled table before splitting, so all panels share rows and columns."),
                  tags$li(tags$strong("One shared colour scale"), " \u2014 a given shade means the same residual in every panel."),
                  tags$li(tags$strong("Corrected p-values"), " \u2014 one test per group is multiple testing; the adjusted column adjusts for it."),
                  tags$li(tags$strong("Panel titles carry n"), " \u2014 panels are drawn equal width, so the group size is printed rather than implied.")
                )
              ),
              div(class = "help-section",
                tags$h4("\U0001f4be Export"),
                tags$ul(
                  tags$li(tags$strong("Plot"), " — PNG (rendered) or PDF (vector)."),
                  tags$li(tags$strong("Residuals CSV"), " and ", tags$strong("Summary CSV"), " for reporting.")
                )
              )
            )
          )
        )
  ),

  tags$footer(class = "app-footer",
    tags$div(class = "footer-authors",
      tags$a(href = "https://saqr.me", target = "_blank", "Mohammed Saqr")
    ),
    tags$div(class = "footer-refs",
      "Built with the ",
      tags$a(href = "https://github.com/mohsaqr/Saqrmisc", target = "_blank", "Saqrmisc"),
      " R package · mosaic_analysis()"
    )
  )
)


# ---- Server ----

server <- function(input, output, session) {

  # ---- reactive: loaded data ----
  data_loaded <- reactive({
    switch(input$data_source,
      demo = demo,
      upload = {
        req(input$file)
        read.csv(input$file$datapath, stringsAsFactors = FALSE)
      }
    )
  })

  # ---- pre-fill variable selectors when source changes ----
  observeEvent(data_loaded(), {
    cols <- colnames(data_loaded())
    v1 <- if (input$data_source == "demo") "Specialization_EN"   else cols[1]
    v2 <- if (input$data_source == "demo") "Final_Evaluation_EN" else cols[min(2, length(cols))]
    updateSelectInput(session, "var1_sel", choices = cols, selected = v1)
    updateSelectInput(session, "var2_sel", choices = cols, selected = v2)
  })

  output$ui_var1 <- renderUI({
    selectInput("var1_sel", "Row variable", choices = colnames(data_loaded()))
  })
  output$ui_var2 <- renderUI({
    cols <- colnames(data_loaded())
    selectInput("var2_sel", "Column variable", choices = cols,
                selected = cols[min(2, length(cols))])
  })

  # The stratifier must differ from both analysis variables, so the two already
  # chosen are removed from the list rather than rejected after the fact.
  output$ui_by <- renderUI({
    cols <- setdiff(colnames(data_loaded()), c(input$var1_sel, input$var2_sel))
    selectInput("by_sel", "Stratify by (optional)",
                choices = c("(none)" = "__none__", stats::setNames(cols, cols)),
                selected = isolate(input$by_sel) %||% "__none__")
  })

  by_var <- reactive({
    b <- input$by_sel
    if (is.null(b) || identical(b, "__none__")) NULL else b
  })

  # ---- reactive: run analysis on demand (statistics only) ----
  analysis <- eventReactive(input$run, {
    d <- data_loaded()
    validate(
      need(input$var1_sel, "Pick a row variable."),
      need(input$var2_sel, "Pick a column variable."),
      need(!identical(input$var1_sel, input$var2_sel),
           "Row and column variables must differ.")
    )
    opts <- list(
      by               = by_var(),
      min_count        = as.integer(input$min_count),
      var1_label       = if (nzchar(input$var1_label)) input$var1_label else NULL,
      var2_label       = if (nzchar(input$var2_label)) input$var2_label else NULL,
      show_percentages = isTRUE(input$show_pct),
      percentage_base  = input$pct_base,
      use_fisher       = identical(input$test, "fisher"),
      min_stratum_n    = as.numeric(input$min_stratum_n %||% 30),
      p_adjust         = input$p_adjust %||% "BH",
      seed             = if (identical(input$test, "fisher")) input$seed else NULL
    )
    out <- withProgress(message = "Running mosaic analysis…", value = 0.5,
                        .compute_mosaic(d, input$var1_sel, input$var2_sel, opts))
    if (out$status == "error")
      showNotification(out$message, type = "error", duration = 8)
    out
  })

  # ---- reactive: live plot styling (applied without re-running the test) ----
  # Reads every styling control; any change re-renders the plot only.
  plot_overrides <- reactive(list(
    plot_style      = input$plot_style,
    title           = input$title,
    tile_label      = input$tile_label,
    percentage_base = input$pct_base,
    col_label_side  = input$col_label_side,
    row_label_side  = input$row_label_side,
    col_label_angle = as.numeric(input$col_label_angle),
    row_label_angle = as.numeric(input$row_label_angle),
    show_varnames   = isTRUE(input$show_varnames),
    fontsize        = input$fontsize,
    show_legend     = isTRUE(input$show_legend),
    legend_position = input$legend_position,
    legend_size     = input$legend_size,
    legend_title    = input$legend_title,
    label_size      = input$label_size,
    # 0 means "let ggplot2 decide", which the package expresses as NULL
    facet_ncol      = if (is.null(input$facet_ncol) || input$facet_ncol < 1) NULL
                      else as.integer(input$facet_ncol),
    facet_show_n    = isTRUE(input$facet_show_n)
  ))

  # Draw the current result with the current styling, into the active device.
  draw_current_plot <- function() {
    do.call(plot, c(list(res_value()), plot_overrides()))
  }

  # On a fresh result, jump straight to the plot. A stratified run needs more
  # canvas than a single mosaic, so the size sliders are nudged to fit the
  # panel grid; the user can still override them afterwards.
  observeEvent(analysis(), {
    a <- analysis()
    if (a$status != "ok") return()
    if (inherits(a$value, "mosaic_stratified")) {
      # Size the canvas sliders to the grid ggplot2 will actually draw, so the
      # two sizing modes start out agreeing with each other.
      d <- ggplot2::wrap_dims(a$value$n_strata)
      updateSliderInput(session, "plot_w",
                        value = max(600, min(2400, 380 * d[2] + 150)))
      updateSliderInput(session, "plot_h",
                        value = max(320, min(2000, 340 * d[1] + 45)))
    }
    updateTabsetPanel(session, "tabs", selected = "plot")
  })

  res_value <- reactive({
    a <- analysis()
    validate(need(a$status == "ok", a$message))
    a$value
  })

  # TRUE when the current result carries per-stratum fits. Exposed to the UI so
  # the panel controls can hide themselves on an un-stratified run.
  is_stratified <- reactive({
    a <- analysis()
    isTRUE(a$status == "ok") && inherits(a$value, "mosaic_stratified")
  })
  output$is_stratified <- reactive(is_stratified())
  outputOptions(output, "is_stratified", suspendWhenHidden = FALSE)

  # Warnings raised during the fit (low expected counts, dropped strata,
  # unobserved categories) are shown rather than silently dropped.
  output$run_warnings <- renderUI({
    a <- analysis()
    req(length(a$warnings) > 0)
    div(class = "warn-callout",
      tags$strong("Notes from this run"),
      tags$ul(lapply(unique(a$warnings), tags$li)))
  })

  # Panel grid of the current result. ggplot2's own layout function is used so
  # the arithmetic here always matches what facet_wrap actually draws.
  grid_dims <- reactive({
    a <- analysis()
    if (!isTRUE(a$status == "ok") || !inherits(a$value, "mosaic_stratified"))
      return(c(nrow = 1L, ncol = 1L))
    nc <- if (is.null(input$facet_ncol) || input$facet_ncol < 1) NULL
          else as.integer(input$facet_ncol)
    d <- ggplot2::wrap_dims(a$value$n_strata, ncol = nc)
    c(nrow = as.integer(d[1]), ncol = as.integer(d[2]))
  })

  # Room the panel grid itself does not occupy: the colour bar, the category
  # labels and an optional title. Without this, per-panel sizing would squeeze
  # the panels to make space for the legend.
  plot_padding <- reactive({
    leg  <- isTRUE(input$show_legend)
    side <- input$legend_position %||% "right"
    c(w = if (leg && side %in% c("left", "right")) 150 else 40,
      h = (if (leg && side %in% c("top", "bottom")) 120 else 45) +
          (if (nzchar(input$title %||% "")) 35 else 0))
  })

  # The single source of truth for plot size: the on-screen render and both
  # download handlers all read this, so an export matches what is displayed.
  plot_dims <- reactive({
    if (identical(input$size_mode, "panel")) {
      g <- grid_dims(); pad <- plot_padding()
      c(w = min(4000, g[["ncol"]] * (input$panel_w %||% 380) + pad[["w"]]),
        h = min(4000, g[["nrow"]] * (input$panel_h %||% 340) + pad[["h"]]))
    } else {
      c(w = input$plot_w %||% 900, h = input$plot_h %||% 620)
    }
  })

  output$canvas_readout <- renderUI({
    g <- grid_dims(); d <- plot_dims()
    n <- g[["nrow"]] * g[["ncol"]]
    div(class = "stat-callout", style = "margin-top:8px; font-size:12px;",
        sprintf("%s in a %d x %d grid \u2192 canvas %d x %d px.",
                if (n == 1) "1 panel" else paste(n, "panel slots"),
                g[["nrow"]], g[["ncol"]], round(d[["w"]]), round(d[["h"]])))
  })

  # ---- Strata tab ----
  output$strata_page <- renderUI({
    a <- analysis()
    if (!isTRUE(a$status == "ok"))
      return(div(class = "stat-callout",
                 "Run an analysis on the Start tab first."))
    if (!is_stratified())
      return(div(class = "stat-callout",
                 HTML(paste("This run was not stratified. Pick a variable under",
                            "<strong>Stratify / facet</strong> on the Start tab",
                            "and run again to fit each group separately."))))
    tagList(
      uiOutput("strata_verdict"),
      h5("Per-group tests"),
      div(class = "stat-callout",
          paste("One row per group, each fitted on its own contingency table.",
                "p_adjusted corrects for testing every group;",
                "compare cramers_v across rows to see whether the association",
                "has the same strength everywhere.")),
      DTOutput("strata_table"),
      br(),
      h5("Pooled vs conditional"),
      div(class = "stat-callout",
          paste("\"pooled\" ignores the stratifier. \"conditional\" is the",
                "Cochran-Mantel-Haenszel test, which holds the stratifier",
                "fixed. A large gap between them means the stratifier is",
                "confounding the association.")),
      DTOutput("overall_table")
    )
  })

  # A plain-language read of pooled vs per-group effect sizes.
  output$strata_verdict <- renderUI({
    req(is_stratified())
    fit <- res_value()
    strata  <- as.data.frame(fit, what = "strata")
    overall <- as.data.frame(fit, what = "overall")
    pooled_v <- overall$cramers_v[overall$scope == "pooled"]
    max_v <- max(strata$cramers_v, na.rm = TRUE)
    min_v <- min(strata$cramers_v, na.rm = TRUE)
    n_sig <- sum(strata$p_adjusted < 0.05, na.rm = TRUE)

    # State the numbers and the ratio rather than collapsing them to a verdict
    # at some cut-off: the reader can weigh a 1.4x gap for themselves, and a
    # hard threshold would flip the message either side of an arbitrary line.
    ratio <- pooled_v / max(max_v, 1e-6)
    spread <- max_v / max(min_v, 1e-6)

    facts <- sprintf(paste("Pooled Cramer's V is %.3f; within groups it ranges",
                           "%.3f to %.3f."),
                     pooled_v, min_v, max_v)

    confound <- if (ratio >= 1.1) {
      sprintf(paste("The pooled association is %.1fx the strongest single group,",
                    "so part of it is carried by %s rather than by the",
                    "association itself."), ratio, fit$by_name)
    } else {
      sprintf(paste("The pooled association is no stronger than within groups,",
                    "so %s is not inflating it."), fit$by_name)
    }

    modify <- if (spread >= 2) {
      sprintf(paste("Strength also differs %.1fx between the weakest and",
                    "strongest group, so %s appears to modify the association."),
              spread, fit$by_name)
    } else {
      sprintf("Strength is comparable across groups (%.1fx spread).", spread)
    }

    div(class = "stat-callout",
        tags$strong("Reading these panels: "),
        paste(facts, confound, modify,
              sprintf("%d of %d groups are significant after correction.",
                      n_sig, nrow(strata))))
  })

  output$strata_table <- renderDT({
    req(is_stratified())
    d <- as.data.frame(res_value(), what = "strata")
    datatable(d, rownames = FALSE,
              options = list(dom = "t", scrollX = TRUE, pageLength = 50,
                             ordering = FALSE)) |>
      formatSignif(c("p_value", "p_adjusted"), 3) |>
      formatStyle("p_adjusted",
                  fontWeight = styleInterval(0.05, c("bold", "normal")),
                  color = styleInterval(0.05, c("#1a56db", "#666")))
  })

  output$overall_table <- renderDT({
    req(is_stratified())
    d <- as.data.frame(res_value(), what = "overall")
    datatable(d, rownames = FALSE,
              options = list(dom = "t", scrollX = TRUE, ordering = FALSE)) |>
      formatSignif("p_value", 3)
  })

  # ---- Summary tab ----
  output$stats_table <- renderDT({
    datatable(as.data.frame(res_value()$stats_summary),
              rownames = FALSE, options = list(dom = "t", ordering = FALSE))
  })

  output$consolidated_table <- renderDT({
    datatable(as.data.frame(res_value()$summary_table),
              rownames = FALSE,
              options = list(pageLength = 25, scrollX = TRUE, ordering = FALSE))
  })

  output$export_strip <- renderUI({
    req(res_value())
    div(class = "export-strip",
      h5("Export"),
      downloadButton("dl_png_s",     "Plot PNG"),
      " ",
      downloadButton("dl_resid_s",   "Residuals CSV"),
      " ",
      downloadButton("dl_summary_s", "Summary CSV")
    )
  })

  # ---- Residuals tab ----
  output$residuals_note <- renderUI({
    req(is_stratified())
    div(class = "stat-callout",
        paste0("Residuals are computed within each level of ",
               res_value()$by_name,
               ". A blank residual marks a category not observed in that group."))
  })

  # A stratified fit has a residual per group x cell, which is a long table; an
  # un-stratified fit keeps the familiar wide row-by-column layout.
  output$residuals_table <- renderDT({
    if (is_stratified()) {
      r <- as.data.frame(res_value(), what = "residuals")
      return(datatable(r, rownames = FALSE,
                       options = list(pageLength = 25, scrollX = TRUE,
                                      order = list(list(0, "asc")))) |>
        formatStyle("std_residual",
                    color = styleInterval(c(-2, 2), c("#c0392b", "#333", "#1a56db")),
                    fontWeight = styleInterval(c(-2, 2), c("bold", "normal", "bold"))))
    }
    r <- res_value()$residuals
    datatable(r, rownames = FALSE,
              options = list(dom = "t", scrollX = TRUE, ordering = FALSE)) |>
      formatStyle(setdiff(colnames(r), "Variable"),
                  color = styleInterval(c(-2, 2), c("#c0392b", "#333", "#1a56db")),
                  fontWeight = styleInterval(c(-2, 2), c("bold", "normal", "bold")))
  })

  # ---- Mosaic plot tab (re-renders live on any styling/size change) ----
  output$plot_ui <- renderUI({
    req(analysis())
    if (analysis()$status != "ok")
      return(div(class = "alert alert-danger", analysis()$message))
    plotOutput("mosaic_plot", width = "auto", height = "auto")
  })

  output$mosaic_plot <- renderPlot(
    { req(res_value()); draw_current_plot() },
    res = 100,
    width  = function() plot_dims()[["w"]],
    height = function() plot_dims()[["h"]]
  )

  # ---- download handlers (use the current result + current styling) ----
  dl_png_fun <- downloadHandler(
    filename = function() paste0("mosaic_", Sys.Date(), ".png"),
    content  = function(f) {
      req(res_value())
      d <- plot_dims()
      grDevices::png(f, width = d[["w"]] * 1.4, height = d[["h"]] * 1.4, res = 130)
      on.exit(grDevices::dev.off(), add = TRUE)
      draw_current_plot()
    }
  )
  output$dl_png   <- dl_png_fun
  output$dl_png_s <- dl_png_fun

  output$dl_pdf <- downloadHandler(
    filename = function() paste0("mosaic_", Sys.Date(), ".pdf"),
    content  = function(f) {
      req(res_value())
      d <- plot_dims()
      grDevices::pdf(f, width = d[["w"]] / 90, height = d[["h"]] / 90)
      on.exit(grDevices::dev.off(), add = TRUE)
      draw_current_plot()
    }
  )

  dl_resid_fun <- downloadHandler(
    filename = function() paste0("mosaic_residuals_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(res_value()$residuals, f, row.names = FALSE)
  )
  output$dl_resid   <- dl_resid_fun
  output$dl_resid_s <- dl_resid_fun

  output$dl_strata_ui <- renderUI({
    req(is_stratified())
    tagList(" ", downloadButton("dl_strata", "Per-group tests CSV"))
  })

  output$dl_strata <- downloadHandler(
    filename = function() paste0("mosaic_strata_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(as.data.frame(res_value(), what = "strata"),
                                     f, row.names = FALSE)
  )

  dl_summary_fun <- downloadHandler(
    filename = function() paste0("mosaic_summary_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(as.data.frame(res_value()$stats_summary), f, row.names = FALSE)
  )
  output$dl_summary   <- dl_summary_fun
  output$dl_summary_s <- dl_summary_fun
}

shinyApp(ui, server)
