#' ---
#' title: Data Cleaning Report of bdclean Package
#' output:
#'      highlight: zenburn
#' ---

#' # Data cleaning summary table

#+ echo=F, eval=T
#' `r library('knitr')`
#' `r load("cleaningReport.RData")`
#' `r knitr::kable(recordsTable)`