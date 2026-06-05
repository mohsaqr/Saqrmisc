# Saqrmisc Package: flat ggplot2 mosaic renderer
#
# A modern, flat alternative to the vcd mosaic. Tile AREA encodes counts
# (column width = first-variable marginal, tile height = within-column
# conditional), tile FILL encodes the standardized Pearson residual.

#' @importFrom ggplot2 ggplot aes geom_rect geom_text scale_fill_gradientn
#' @importFrom ggplot2 scale_colour_identity scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 expansion coord_cartesian labs theme_void theme element_rect
#' @importFrom ggplot2 element_text element_blank margin unit guide_colourbar
NULL

# ColorBrewer RdBu (11) — red = negative residual, blue = positive. No new dep.
.mosaic_rdbu <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7",
                  "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac",
                  "#053061")

#' Build a mosaic plot (flat or classic) from precomputed parts (internal)
#'
#' Shared by \code{mosaic_analysis()} and \code{plot.mosaic_analysis()} so the
#' plot can be re-rendered with different styling without recomputing the test.
#'
#' @param parts List with the contingency \code{table}, the standardized
#'   residual matrix, the filtered data, the variable names and labels.
#' @param style List of styling arguments (see \code{mosaic_analysis}).
#' @return For "flat", a ggplot object (not drawn). For "classic", the
#'   \pkg{vcd} structable (drawn to the current device as a side effect).
#' @noRd
build_mosaic_plot <- function(parts, style) {
  if (style$plot_style == "flat") {
    flat_mosaic(
      parts$table, parts$residuals, title = style$title,
      tile_label = style$tile_label, pct_base = style$percentage_base,
      col_label_side = style$col_label_side, row_label_side = style$row_label_side,
      col_label_angle = style$col_label_angle, row_label_angle = style$row_label_angle,
      show_legend = style$show_legend, legend_position = style$legend_position,
      legend_size = style$legend_size, legend_title = style$legend_title,
      label_size = style$label_size)
  } else {
    set_vn <- c(parts$var1_label, parts$var2_label)
    names(set_vn) <- c(parts$var1_name, parts$var2_name)
    vcd::mosaic(
      stats::as.formula(paste("~", parts$var1_name, "+", parts$var2_name)),
      data = parts$data, main = style$title, shade = TRUE,
      legend = style$show_legend, labeling = vcd::labeling_values,
      labeling_args = list(
        varnames = style$show_varnames,
        rot_labels = c(left = 0, top = 0),
        offset_labels = c(left = 2.5, top = 0.5),
        set_varnames = set_vn,
        gp_text = grid::gpar(fontsize = style$fontsize)))
  }
}

# Render a built mosaic to the current device. flat ggplots must be printed;
# classic structables are already drawn by build_mosaic_plot().
draw_mosaic_plot <- function(plot_obj, style) {
  if (style$plot_style == "flat") print(plot_obj)
  invisible(plot_obj)
}

#' Flat ggplot2 mosaic shaded by standardized residuals (internal)
#'
#' @param tab A two-way contingency \code{table} (rows = first variable,
#'   columns = second variable).
#' @param res A matrix of standardized Pearson residuals, same shape as
#'   \code{tab}.
#' @param title Plot title.
#' @param tile_label What to print inside each tile: "count" (default), "percent",
#'   "residual", "category" (the second-variable level), or "none".
#' @param pct_base Base for the "percent" tile label: "total", "row", or "column".
#' @param col_label_side Placement of the first-variable (column) labels: "top"
#'   (default), "bottom", "both", or "none".
#' @param row_label_side Placement of the second-variable (row) labels: "left"
#'   (default), "right", "both", or "none".
#' @param col_label_angle,row_label_angle Text rotation in degrees for the column
#'   and row labels (0 = horizontal, 90 = vertical).
#' @param show_legend Logical; draw the residual colour-bar legend.
#' @param legend_position One of "right", "left", "top", "bottom", "none".
#' @param legend_size Numeric multiplier (>0) scaling the legend key and text.
#' @param legend_title Legend title text.
#' @param label_size Tile-label text size (in ggplot2 mm units).
#' @param palette Character vector of fill colours (low \U2192 high). Defaults
#'   to ColorBrewer RdBu.
#' @param min_label_h,min_label_w Minimum tile height / width (as a proportion
#'   of the plotting area) for a tile to receive a text label.
#' @return A \code{ggplot} object.
#' @noRd
flat_mosaic <- function(tab, res, title = "",
                        tile_label = c("count", "percent", "residual",
                                       "category", "none"),
                        pct_base = "total",
                        col_label_side = c("top", "bottom", "both", "none"),
                        row_label_side = c("left", "right", "both", "none"),
                        col_label_angle = 0, row_label_angle = 0,
                        show_legend = TRUE, legend_position = "right",
                        legend_size = 0.7, legend_title = "Std.\nresidual",
                        label_size = 3.5, palette = NULL,
                        min_label_h = 0.04, min_label_w = 0.03) {
  tile_label     <- match.arg(tile_label)
  col_label_side <- match.arg(col_label_side)
  row_label_side <- match.arg(row_label_side)
  if (is.null(palette)) palette <- .mosaic_rdbu

  rn <- rownames(tab); cn <- colnames(tab)
  R <- nrow(tab); C <- ncol(tab)
  grand <- sum(tab)
  col_tot  <- rowSums(tab)              # first-variable (column) marginal, named
  var2_tot <- colSums(tab)              # second-variable (row) marginal, named

  # column x-extents proportional to first-variable marginal. as.numeric() strips
  # the marginal names so the per-tile data.frame below does not warn about them.
  xw <- as.numeric(col_tot) / grand
  xright <- cumsum(xw); xleft <- xright - xw

  # one stacked column of tiles (top-down) for plot-column i
  build_col <- function(i) {
    h <- as.numeric(tab[i, ]) / col_tot[i]
    data.frame(
      xmin = xleft[i], xmax = xright[i],
      ymin = 1 - cumsum(h), ymax = 1 - c(0, cumsum(h)[-C]),
      count = as.numeric(tab[i, ]), resid = as.numeric(res[i, ]),
      from = rn[i], to = cn, stringsAsFactors = FALSE
    )
  }
  df <- do.call(rbind, lapply(seq_len(R), build_col))
  df <- df[df$count > 0, ]
  df$w <- df$xmax - df$xmin
  df$h <- df$ymax - df$ymin
  df$xmid <- (df$xmin + df$xmax) / 2
  df$ymid <- (df$ymin + df$ymax) / 2

  # percentage per tile, on the requested base
  df$pct <- switch(pct_base,
    total  = df$count / grand * 100,
    row    = df$count / as.numeric(col_tot[df$from]) * 100,
    column = df$count / as.numeric(var2_tot[df$to]) * 100,
    df$count / grand * 100)

  # tile text content
  raw_txt <- switch(tile_label,
    count    = formatC(df$count, format = "d", big.mark = ","),
    percent  = paste0(formatC(df$pct, format = "f", digits = 1), "%"),
    residual = formatC(df$resid, format = "f", digits = 1),
    category = df$to,
    none     = rep("", nrow(df)))

  max_abs <- max(abs(df$resid), 1e-6)
  df$txt_col <- ifelse(abs(df$resid) > 0.55 * max_abs, "white", "grey15")
  df$lab <- ifelse(df$h >= min_label_h & df$w >= min_label_w, raw_txt, "")

  # ---- category-label placement frames ----
  # Drop labels for columns/rows too thin to host text — otherwise a sliver
  # category (e.g. one with very few observations) overprints its neighbour.
  col_centers <- (xleft + xright) / 2
  col_keep <- (xright - xleft) >= min_label_w
  col_sides <- switch(col_label_side, top = "top", bottom = "bottom",
                      both = c("top", "bottom"), none = character(0))
  col_df <- if (length(col_sides) && any(col_keep))
    do.call(rbind, lapply(col_sides, function(s) data.frame(
      x = col_centers[col_keep], y = if (s == "top") 1 else 0, lab = rn[col_keep],
      vj = if (s == "top") -0.4 else 1.4))) else NULL

  pj <- as.numeric(var2_tot) / grand
  row_centers <- 1 - (cumsum(pj) - pj / 2)
  row_keep <- pj >= min_label_h
  row_sides <- switch(row_label_side, left = "left", right = "right",
                      both = c("left", "right"), none = character(0))
  row_df <- if (length(row_sides) && any(row_keep))
    do.call(rbind, lapply(row_sides, function(s) data.frame(
      y = row_centers[row_keep], x = if (s == "left") 0 else 1, lab = cn[row_keep],
      hj = if (s == "left") 1.15 else -0.15))) else NULL

  fill_guide <- if (show_legend && legend_position != "none") {
    ggplot2::guide_colourbar(
      barheight = ggplot2::unit(legend_size * 6, "lines"),
      barwidth  = ggplot2::unit(legend_size * 0.8, "lines"),
      ticks.colour = "grey40", frame.colour = NA)
  } else {
    "none"
  }
  leg_pos <- if (show_legend) legend_position else "none"

  p <- ggplot2::ggplot(df, ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                        ymin = .data$ymin, ymax = .data$ymax)) +
    ggplot2::geom_rect(ggplot2::aes(fill = .data$resid),
                       colour = "white", linewidth = 1.1) +
    ggplot2::geom_text(
      ggplot2::aes(x = .data$xmid, y = .data$ymid, label = .data$lab,
                   colour = .data$txt_col),
      hjust = 0.5, vjust = 0.5, size = label_size)

  if (!is.null(col_df))
    p <- p + ggplot2::geom_text(
      data = col_df, ggplot2::aes(x = .data$x, y = .data$y, label = .data$lab),
      vjust = col_df$vj, hjust = 0.5, angle = col_label_angle,
      size = label_size + 0.1, colour = "grey35", inherit.aes = FALSE)

  if (!is.null(row_df))
    p <- p + ggplot2::geom_text(
      data = row_df, ggplot2::aes(x = .data$x, y = .data$y, label = .data$lab),
      hjust = row_df$hj, vjust = 0.5, angle = row_label_angle,
      size = label_size + 0.1, colour = "grey45", inherit.aes = FALSE)

  # Only reserve margin on a side that actually carries labels, so the tiles
  # use the full width/height when (as by default) labels sit on one side.
  left_e  <- if (row_label_side %in% c("left", "both"))   0.12 else 0.015
  right_e <- if (row_label_side %in% c("right", "both"))  0.12 else 0.015
  top_e   <- if (col_label_side %in% c("top", "both"))    0.06 else 0.015
  bot_e   <- if (col_label_side %in% c("bottom", "both")) 0.06 else 0.015

  p +
    ggplot2::scale_fill_gradientn(colours = palette, limits = c(-max_abs, max_abs),
                                  name = legend_title, guide = fill_guide) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(left_e, right_e))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(bot_e, top_e))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(title = title) +
    ggplot2::theme_void(base_size = 13) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "#fafafa", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#fafafa", colour = NA),
      plot.title = ggplot2::element_text(face = "bold", size = 15, hjust = 0,
                                         margin = ggplot2::margin(b = 12, l = 4)),
      plot.margin = ggplot2::margin(14, 18, 14, 14),
      legend.position = leg_pos,
      legend.title = ggplot2::element_text(size = legend_size * 11),
      legend.text  = ggplot2::element_text(size = legend_size * 10))
}
