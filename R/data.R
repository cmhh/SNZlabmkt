#' Statistics New Zealand Labour Market Time Series Metadata (March 2016)
#'
#' Metadata for time series data for Statistics New Zealand's Household Labour Force Survey (HLFS), Quarterly Employment Survey (QES), and Labour Cost Index (LCI).
#'
#' @format A data table with columns:
#' \describe{
#'   \item{subject}{One of HLF (Household Labour Force Survey), LCI (Labour Cost Index), or QES (Earnings and Employment Survey).}
#'   \item{ref}{Series reference.}
#'   \item{group}{Series grouping.}
#'   \item{title1}{'Title' 1.}
#'   \item{title2}{'Title' 2.}
#'   \item{title3}{'Title' 3.}
#'   \item{title4}{'Title' 4.}
#'   \item{title5}{'Title' 5.}
#'   \item{units}{Unit (NZD, index, number, or percentage).}
#'   \item{magnitude}{Series has been multiplied by 10^(-magnitude) from original.}
#' }
#' @source \url{http://www.stats.govt.nz/~/media/Statistics/Browse%20for%20stats/LabourMarketStatistics/HOTPMar16qtr/Zipped%20csv%20tables.zip}
"metadata"

#' Statistics New Zealand Labour Market Time Series (March 2016)
#'
#' Time series data for Statistics New Zealand's Household Labour Force Survey (HLFS), Quarterly Employment Survey (QES), and Labour Cost Index (LCI).
#'
#' @format A data table with columns:
#' \describe{
#'   \item{ref}{Series reference.}
#'   \item{period}{String describing date.}
#'   \item{value}{Time series value.}
#'   \item{status}{Status--final, provisional, or revised.}
#' }
#' @source \url{http://www.stats.govt.nz/~/media/Statistics/Browse%20for%20stats/LabourMarketStatistics/HOTPMar16qtr/Zipped%20csv%20tables.zip}
"labmkt"
