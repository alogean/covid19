library(httr)
library(readr)
library(dplyr)

swiss_canton <-
  read_delim(
    "swiss_canton_wikidata_qid_covid_outbreack.csv",
    ";",
    escape_double = FALSE,
    trim_ws = TRUE
  )

get_covid_data_for_ch_canton <- function(canton_abbreviation) {
  response <- httr::GET(
    url = paste0(
      "https://covid19-rest.herokuapp.com/api/openzh/v1/country/CH/area/",
      canton_abbreviation
    ),
    httr::use_proxy("http://gate-zrh.swissre.com", 9443),
    httr::content_type('application/json'),
    encode = 'json'
  )
  parsed_content <- httr::content(response, as = "parsed")
  df <-
    data.frame(matrix(
      unlist(parsed_content$records),
      nrow = length(parsed_content$records),
      byrow = T
    ))
  names(df) <- names(parsed_content$records[[1]])
  return(df)
}

get_qid_from_ch_canton_abbrevation <- function(df, canton_abb) {
  new_df <- df[which(df$canton_abbreviation == canton_abb),]
  return(new_df$q_items)
}

convert_data_time_to_wikidata_format <- function(date, time) {
  if (time == "" | is.na(time)) {
    time <- "00:00"
  }
  return(paste0("+", date, "T", time, "Z/11"))
}

generate_wikidata_triples <- function(canton_abbreviation) {
  qid <-
    get_qid_from_ch_canton_abbrevation(swiss_canton, canton_abbreviation)
  data <- get_covid_data_for_ch_canton(canton_abbreviation)
  data <- subset(data, subset = (ncumul_deceased != ""))
  data <- data[!duplicated(data[c(3,10)]),]
  if (nrow(data) != 0) {
    triples_list <- paste(
      qid,
      "P1120",
      data$ncumul_deceased,
      "P585",
      convert_data_time_to_wikidata_format(data$date, data$time),
      "S854",
      paste0("\"", data$source, "\""),
      sep = "|"
    )
  } else {
    triples_list <- list()
  }
  return(triples_list)
}

triples <-
  lapply(swiss_canton$canton_abbreviation, generate_wikidata_triples)
write.csv(data.frame(unlist(triples)), "list_of_triples.csv")

data <- get_covid_data_for_ch_canton("VS")
data <- subset(data, subset = (ncumul_deceased != ""))

data <- get_covid_data_for_ch_canton("VS")
data_without_duplicate <- subset(data, subset = (ncumul_deceased != ""))
data_final <- data_without_duplicate[duplicated(data_without_duplicate[,"ncumul_deceased"]),]
data_final_2 <- group_by(data_without_duplicate, "ncumul_deceased")
data_final_2 <- data_without_duplicate[!duplicated(data_without_duplicate[c(3,10)]),]

