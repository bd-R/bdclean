#' ---
#' title: Data Cleaning Report of bdclean Package
#' date: 29 September 2017
#' output:
#'      highlight: zenburn
#' ---

#' # Data cleaning summary table

#+ echo=F, eval=T
#' `r library('knitr')`
#' `r load("cleaningReport.RData")`
#' `r knitr::kable(recordsTable)`