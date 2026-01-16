#' Saqrmisc: Comprehensive Data Analysis and Visualization for R
#'
#' @description
#' A comprehensive R package providing functions for statistical analysis,
#' data transformation, and visualization. Designed for exploratory data analysis
#' with robust error handling and publication-ready outputs.
#'
#' @section Main Features:
#' The package is organized into the following function categories:
#'
#' \subsection{Group Comparisons}{
#' \itemize{
#'   \item \code{\link{compare_groups}}: Compare groups with t-tests, ANOVA, post-hoc tests,
#'     Bayesian analysis, and equivalence testing (TOST). Supports stratified analyses.
#' }
#' }
#'
#' \subsection{Correlation Analysis}{
#' \itemize{
#'   \item \code{\link{correlations}}: Full pairwise correlation table with complete statistics
#'     (r, CI, t, df, p, n). Supports bivariate, partial, and semi-partial correlations,
#'     group-wise correlations, and multilevel (within-cluster) correlations.
#'   \item \code{\link{correlation_matrix}}: Publication-ready correlation matrix with
#'     significance stars, heatmap visualization, and optional confidence intervals.
#' }
#' }
#'
#' \subsection{Descriptive Statistics}{
#' \itemize{
#'   \item \code{\link{descriptive_table}}: Publication-ready descriptive statistics with
#'     gt formatting. Supports 20+ statistics and group stratification.
#'   \item \code{\link{categorical_table}}: Frequency tables with cross-tabulation,
#'     chi-square tests, and Cramer's V effect size.
#'   \item \code{\link{auto_describe}}: Automatic detection and description of all variables.
#'   \item \code{\link{data_overview}}: Quick overview of data structure and quality.
#' }
#' }
#'
#' \subsection{Data Transformation}{
#' \itemize{
#'   \item \code{\link{center}} / \code{\link{center_vec}}: Mean-centering with optional
#'     group-wise centering for multilevel data.
#'   \item \code{\link{scale_vars}} / \code{\link{scale_vec}}: Scaling by SD or min-max
#'     normalization with optional group-wise scaling.
#'   \item \code{\link{standardize}} / \code{\link{standardize_vec}}: Z-score standardization
#'     with optional group-wise standardization.
#'   \item \code{\link{reverse_code}} / \code{\link{reverse_code_vec}}: Reverse coding for
#'     Likert scales and similar measures.
#' }
#' }
#'
#' \subsection{Missing Data}{
#' \itemize{
#'   \item \code{\link{missing_analysis}}: Comprehensive missing data analysis with
#'     patterns, Little's MCAR test, and visualizations.
#'   \item \code{\link{replace_missing}}: Imputation using mean, median, mode, or custom
#'     values. Supports group-wise imputation.
#' }
#' }
#'
#' \subsection{Outlier Analysis}{
#' \itemize{
#'   \item \code{\link{outlier_check}}: Detect outliers using IQR, Z-score, or
#'     Mahalanobis distance methods.
#'   \item \code{\link{replace_outliers}}: Replace or winsorize outliers.
#'   \item \code{\link{is_outlier}} / \code{\link{winsorize_vec}}: Vectorized functions
#'     for use with dplyr.
#' }
#' }
#'
#' \subsection{Normality Testing}{
#' \itemize{
#'   \item \code{\link{normality_check}}: Comprehensive normality testing with
#'     Shapiro-Wilk, Kolmogorov-Smirnov, and visual diagnostics.
#' }
#' }
#'
#' \subsection{Clustering}{
#' \itemize{
#'   \item \code{\link{clustering}}: Model-based clustering using Gaussian Mixture Models
#'     via MoEClust. Tests 14 different covariance structures.
#'   \item \code{\link{get_cluster_assignments}}: Extract cluster assignments with
#'     optional membership probabilities.
#'   \item \code{\link{assess_cluster_stability}}: Bootstrap stability assessment.
#'   \item \code{\link{model_comparison_table}}: Compare models by BIC, AIC, or ICL.
#'   \item \code{\link{generate_cluster_report}}: Generate comprehensive cluster reports.
#' }
#' }
#'
#' \subsection{Categorical Analysis}{
#' \itemize{
#'   \item \code{\link{mosaic_analysis}}: Mosaic plot analysis with chi-square tests,
#'     Fisher's exact test, and Cramer's V effect size.
#' }
#' }
#'
#' \subsection{Network Analysis}{
#' \itemize{
#'   \item \code{\link{estimate_single_network}}: Estimate psychological networks using
#'     bootnet, mgm, and qgraph.
#'   \item \code{\link{estimate_grouped_networks}}: Estimate networks by group.
#'   \item \code{\link{compare_networks}}: Compare networks between groups.
#' }
#' }
#'
#' @section API Design:
#' All functions use a consistent API with quoted strings for variable names:
#' \itemize{
#'   \item Variable names are passed as character strings (e.g., \code{group_by = "gender"})
#'   \item Multiple variables use character vectors (e.g., \code{Vars = c("age", "score")})
#'   \item When \code{Vars = NULL}, functions auto-select all numeric variables
#' }
#'
#' @section Output Formats:
#' Most functions support multiple output formats:
#' \itemize{
#'   \item \code{format = "gt"}: Publication-ready gt tables (default)
#'   \item \code{format = "data.frame"}: Raw data frames for further processing
#' }
#'
#' @author Mohammed Saqr \email{saqr@@saqr.me}
#'
#' @seealso
#' Useful links:
#' \itemize{
#'   \item \url{https://github.com/mohsaqr/Saqrmisc}
#'   \item Report bugs at \url{https://github.com/mohsaqr/Saqrmisc/issues}
#' }
#'
#' @importFrom dplyr %>% mutate select filter group_by ungroup summarise arrange
#'   across all_of any_of bind_rows bind_cols n desc everything count relocate first case_when
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col geom_tile geom_boxplot
#'   geom_text geom_hline labs theme theme_minimal element_text element_blank
#'   scale_fill_gradient scale_fill_gradient2 scale_fill_manual scale_fill_viridis_d
#'   scale_color_manual scale_color_viridis_d facet_wrap stat_qq stat_qq_line
#' @importFrom gt gt tab_header tab_style cell_text cell_fill cell_borders cells_body
#'   cells_column_labels cells_title cols_align cols_label fmt_number tab_footnote
#'   tab_source_note tab_options tab_spanner tab_row_group px
#' @importFrom stats cor cor.test t.test aov TukeyHSD kruskal.test wilcox.test
#'   pairwise.t.test pairwise.wilcox.test shapiro.test ks.test chisq.test fisher.test
#'   sd var median quantile IQR complete.cases na.omit p.adjust qnorm qt pchisq qchisq
#'   as.formula lm predict residuals setNames mahalanobis cov ave qqnorm qqline pt
#' @importFrom rlang .data sym
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @importFrom grDevices colorRampPalette pdf png svg dev.off
#' @importFrom grid gpar
#' @importFrom vcd mosaic labeling_values
#' @importFrom utils modifyList packageVersion
#'
#' @keywords internal
"_PACKAGE"
