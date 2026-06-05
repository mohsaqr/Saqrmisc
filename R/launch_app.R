# Saqrmisc Package: Shiny app launcher
#
# Interactive front-end for mosaic_analysis().

#' Launch the Saqrmisc mosaic analysis Shiny app
#'
#' @description
#' Opens an interactive Shiny application for \code{\link{mosaic_analysis}}.
#' Upload a CSV (or use the built-in demo), pick two categorical variables,
#' and explore the chi-square / Fisher test, Cramer's V effect size,
#' standardized residuals, and the shaded mosaic plot. Every
#' \code{mosaic_analysis()} option is exposed in the sidebar.
#'
#' Requires the \pkg{shiny} and \pkg{DT} packages.
#'
#' @param ... Passed to \code{\link[shiny]{runApp}} (e.g. \code{port},
#'   \code{launch.browser}, \code{host}).
#' @return Called for its side effect (launches the app). No return value.
#' @export
#' @examples
#' if (interactive()) {
#'   launch_app()
#' }
launch_app <- function(...) {
  # nocov start — interactive Shiny entrypoint, not reachable under batch test runs
  for (pkg in c("shiny", "DT")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop(sprintf("Package '%s' is required. Install it with: install.packages('%s')",
                   pkg, pkg), call. = FALSE)
  }
  app_dir <- system.file("shiny", "mosaic_app", package = "Saqrmisc")
  if (!nzchar(app_dir))
    stop("Could not find the Shiny app directory. Re-install the Saqrmisc package.",
         call. = FALSE)
  shiny::runApp(app_dir, ...)
  # nocov end
}
