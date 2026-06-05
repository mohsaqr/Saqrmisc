library(shiny)
library(Saqrmisc)
library(DT)

# Allow uploads up to 100 MB (default is 5 MB).
# Note: if served behind nginx, also set `client_max_body_size 100m;`
# in the server block — otherwise nginx returns 413 before Shiny sees the request.
options(shiny.maxRequestSize = 100 * 1024^2)

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
  set.seed(42)
  spec <- rep(spec_levels, times = n_per[spec_levels])
  evals <- vapply(spec, function(s) {
    if (stats::runif(1) < pass_prob[[s]]) "Pass" else "Fail"
  }, character(1), USE.NAMES = FALSE)
  data.frame(
    Specialization_EN   = factor(spec, levels = spec_levels),
    Final_Evaluation_EN = factor(evals, levels = c("Fail", "Pass")),
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
  res <- tryCatch(
    suppressWarnings(
      mosaic_analysis(
        data, var1, var2,
        min_count        = opts$min_count,
        var1_label       = opts$var1_label,
        var2_label       = opts$var2_label,
        show_percentages = opts$show_percentages,
        percentage_base  = opts$percentage_base,
        use_fisher       = opts$use_fisher,
        verbose          = FALSE
      )
    ),
    error = function(e) structure(conditionMessage(e), class = "mosaic_error")
  )
  if (inherits(res, "mosaic_error"))
    return(list(status = "error", message = as.character(res), value = NULL))
  list(status = "ok", value = res, message = NULL)
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
            uiOutput("ui_var2")
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
      h5("Statistical summary"),
      DTOutput("stats_table"),
      br(),
      h5("Consolidated frequency table"),
      DTOutput("consolidated_table"),
      uiOutput("export_strip")
    ),

    # ---- Residuals ----
    tabPanel("Residuals",
      br(),
      div(class = "stat-callout",
          "Standardized Pearson residuals. Values > |2| indicate a cell that deviates significantly from the expected count under independence."),
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
                        choices = c("Count"     = "count",
                                    "Percent"   = "percent",
                                    "Residual"  = "residual",
                                    "Category"  = "category",
                                    "None"      = "none"),
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

          # ---- Plot size ----
          div(class = "section-header", "Plot size"),
          sliderInput("plot_w", "Width (px)",  min = 400, max = 1600,
                      value = 900, step = 20, ticks = FALSE),
          sliderInput("plot_h", "Height (px)", min = 300, max = 1200,
                      value = 620, step = 20, ticks = FALSE)
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
      downloadButton("dl_summary", "Summary CSV")
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
      min_count        = as.integer(input$min_count),
      var1_label       = if (nzchar(input$var1_label)) input$var1_label else NULL,
      var2_label       = if (nzchar(input$var2_label)) input$var2_label else NULL,
      show_percentages = isTRUE(input$show_pct),
      percentage_base  = input$pct_base,
      use_fisher       = identical(input$test, "fisher")
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
    label_size      = input$label_size
  ))

  # Draw the current result with the current styling, into the active device.
  draw_current_plot <- function() {
    do.call(plot, c(list(res_value()), plot_overrides()))
  }

  # On a fresh result, jump straight to the plot.
  observeEvent(analysis(), {
    if (analysis()$status == "ok")
      updateTabsetPanel(session, "tabs", selected = "plot")
  })

  res_value <- reactive({
    a <- analysis()
    validate(need(a$status == "ok", a$message))
    a$value
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
  output$residuals_table <- renderDT({
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
    width  = function() input$plot_w,
    height = function() input$plot_h
  )

  # ---- download handlers (use the current result + current styling) ----
  dl_png_fun <- downloadHandler(
    filename = function() paste0("mosaic_", Sys.Date(), ".png"),
    content  = function(f) {
      req(res_value())
      grDevices::png(f, width = input$plot_w * 1.4, height = input$plot_h * 1.4, res = 130)
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
      grDevices::pdf(f, width = input$plot_w / 90, height = input$plot_h / 90)
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

  dl_summary_fun <- downloadHandler(
    filename = function() paste0("mosaic_summary_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(as.data.frame(res_value()$stats_summary), f, row.names = FALSE)
  )
  output$dl_summary   <- dl_summary_fun
  output$dl_summary_s <- dl_summary_fun
}

shinyApp(ui, server)
