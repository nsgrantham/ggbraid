#' Average Daily Temperatures
#'
#' A dataset containing daily average temperatures of New York and San Francisco
#' in 2021 as recorded by the US National Weather Service (NWS) at weather.gov.
#'
#' It is difficult to pull data from the NWS. It does not provide the data via
#' an API and the data it returns through its point-and-click interface isn't in
#' plain text format! To make matters worse, you can only retrieve data from a
#' city one month at a time.
#'
#' For San Francisco, visit \url{https://www.weather.gov/wrh/climate?wfo=mtr}
#' and choose "San Francisco City, CA", "Daily data for a month", and a month
#' from 2021; for New York, visit \url{https://www.weather.gov/wrh/climate?wfo=okx}
#' and choose "NY-Central Park Area", "Daily data for a month", and a month from
#' 2021. Copy and paste the data into spreadsheet software for further processing.
#'
#' @format A data frame (specifically a tbl_df) with 730 rows and 3 variables:
#' \describe{
#'   \item{\code{city}}{`New York` or `San Francisco`}
#'   \item{\code{date}}{Date in \code{YYYY-MM-DD} format}
#'   \item{\code{avg}}{Average temperature in degrees Fahrenheit (°F) rounded to
#'   the nearest half degree}
#' }
#'
#' @examples
#' temps
"temps"


#' NBA Finals Game
#'
#' A dataset containing the points scored during Game 1 of the 2018 National
#' Basketball Association (NBA) Finals on May 31, 2018 between the Golden State
#' Warriors and the Cleveland Cavaliers.
#'
#' @format A data frame (specifically a tbl_df) with 129 rows and 3 variables:
#' \describe{
#'   \item{\code{time}}{Game time}
#'   \item{\code{team}}{Golden State Warriors (`GSW`) or Cleveland Cavaliers (`CLE`)}
#'   \item{\code{points}}{Points scored, either 1, 2, or 3 (or 0, only in cases
#'     to mark the start and end of the game)}
#' }
#'
#' @examples
#' hoops
"hoops"
