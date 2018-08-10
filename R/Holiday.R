library(httr)
library(jsonlite)
library(lubridate)

#' Return True when the given date is a holiday in the given state in Germany
#'
#' uses the https://feiertage-api.de/ api. Check the website for an overview of the
#' state keys
#'
#'
#' @param day a date or String in the format yyyy-mm-dd
#' @param state a key for the state e.g. NI for Niedersachsen
#' @export
#' @examples
#' is_holiday("2018-12-25", "NI")
is_holiday <- function(day, state){
  day <- as.character(day)

  year <- lubridate::year(ymd(day))
  url <- paste("https://feiertage-api.de/api/?jahr=",year,"&nur_land=",state, sep = "")
  feiertage <- GET(url)
  feiertage_text <- content(feiertage, "text", encoding = "UTF-8")
  feiertage_json <- unlist(fromJSON(feiertage_text, flatten = TRUE), use.names = FALSE)

  return(day %in% feiertage_json)

}
