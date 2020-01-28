## global.R

## Load libraries
library(shiny)          ##
library(shinydashboard) ##
library(shinyWidgets)   ##
library(shinythemes)    ##
library(data.table)     ##
library(DT)             ##
library(shinyjs)        ##
library(shinyalert)     ##
library(readr)          ##

## Check if app is being opening in IE and warn if so
##NOT CURRENTLY WORKING.........
shinyjs::useShinyjs()
tags$meta("http-equiv" = "X-UA-compatible", content = "IE = edge")
includeScript("www/IE.js")

## Load prison dropdown from s3
# dt.prisons <- data.table::as.data.table(s3tools::s3_path_to_full_df(
#   "alpha-app-anvil-access-tool/prisons_and_offices_v2.csv", header = FALSE))
dt.prisons <- data.table::as.data.table(s3tools::s3_path_to_full_df(
  "alpha-app-anvil-access-tool/prison_area_lookup_aat.csv", header = FALSE))

fields <- c("first_name", "surname", "area", "prison", "role", "quantum_id", 
            "bentham", "bentham_reason", "drugs_prison", "drugs_prison_reason",
            "network", "network_reason", "visitors", "visitors_reason",
            "novel_drugs", "novel_drugs_reason",
            "date_requested", "account", "email")

email_choice <- c("@noms.gsi.gov.uk", "@justice.gov.uk", "@hmps.gov.uk", "@hmcts.gov.uk",
                  "@probation.gov.uk", "@justice.gsi.gov.uk", "@digital.justice.gov.uk")
foundErrors <- 0
quantumErr <- 0

tick <- "<i class=\"fa fa-check de-color\" aria-hidden=\"true\"></i>"
cross <- "<i class=\"fa fa-times de-color\" aria-hidden=\"true\"></i>"


saveData <- function (data) {
  ## Reload data from S3
  responses <- as.data.table(s3tools::s3_path_to_full_df(
    "alpha-app-anvil-access-tool/anvil-app-responses_v5.csv", header = TRUE, 
    col_types = list(
      col_character(), 
      col_character(), 
      col_character(), 
      col_character(), 
      col_character(), 
      col_character(), 
      col_integer(), 
      col_character(), 
      col_integer(), 
      col_character(),
      col_integer(), 
      col_character(), 
      col_integer(), 
      col_character(), 
      col_integer(), 
      col_character(), 
      col_date(), 
      col_character(), 
      col_character()
    )))
  names(responses) <- fields
  responses[date_requested == "", date_requested := NA]
  responses[bentham == "", bentham := 0]
  responses[bentham_reason == "" | is.null(bentham_reason) | bentham_reason == "NULL", bentham_reason := NA]
  responses[drugs_prison == "", drugs_prison := 0]
  responses[drugs_prison_reason == "" | is.null(drugs_prison_reason) | drugs_prison_reason == "NULL", drugs_prison_reason := NA]
  responses[network == "", network := 0]
  responses[network_reason == "" | is.null(network_reason) | network_reason == "NULL", network_reason := NA]
  responses[visitors == "", visitors := 0]
  responses[visitors_reason == "" | is.null(visitors_reason) | visitors_reason == "NULL", visitors_reason := NA]
  responses[novel_drugs == "", novel_drugs := 0]
  responses[novel_drugs_reason == "" | is.null(novel_drugs_reason) | novel_drugs_reason == "NULL", novel_drugs_reason := NA]
  responses$date_requested <- as.Date(responses$date_requested, origin = "1970-01-01")

  if (exists ("responses")) {
    data$bentham <- as.integer(data$bentham)
    data$drugs_prison <- as.integer(data$drugs_prison)
    data$network <- as.integer(data$network)
    data$visitors <- as.integer(data$visitors)
    data$novel_drugs <- as.integer(data$novel_drugs)
    responses <- rbind(responses, data, fill = F)
  } else {
    responses <- data
    responses$bentham <- as.integer(responses$bentham)
    responses$drugs_prison <- as.integer(responses$drugs_prison)
    responses$network <- as.integer(responses$network)
    responses$visitors <- as.integer(responses$visitors)
    responses$novel_drugs <- as.integer(responses$novel_drugs)
    responses$account <- "Requested"
  }

  #Format data and save to s3
  responses$area <- as.character (responses$area)
  responses$prison <- as.character (responses$prison)
  responses$role <- as.character (responses$role)
  responses$first_name <- as.character (responses$first_name)
  responses$surname <- as.character (responses$surname)
  responses$quantum_id <- as.character (responses$quantum_id)
  responses$quantum_id <- tolower (responses$quantum_id)
  responses$email <- as.character(responses$email)
  responses$bentham_reason <- as.character(responses$bentham_reason)
  responses$drugs_prison_reason <- as.character(responses$drugs_prison_reason)
  responses$network_reason <- as.character(responses$network_reason)
  responses$visitors_reason <- as.character(responses$visitors_reason)
  responses$novel_drugs_reason <- as.character(responses$novel_drugs_reason)
  responses$date_requested <- as.Date(responses$date_requested, origin = "1970-01-01")
  
  s3tools::write_df_to_csv_in_s3 (responses,
                                  "alpha-app-anvil-access-tool/anvil-app-responses_v5.csv",
                                  overwrite = TRUE,
                                  row.names = FALSE)

}

loadData <- function() {
  responses <- as.data.table(s3tools::s3_path_to_full_df(
    "alpha-app-anvil-access-tool/anvil-app-responses_v5.csv", header = TRUE))
  names(responses) <- fields
  responses
}
