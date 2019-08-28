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

## Check if app is being opening in IE and warn if so
##NOT CURRENTLY WORKING.........
shinyjs::useShinyjs()
tags$meta("http-equiv" = "X-UA-compatible", content = "IE = edge")
includeScript("www/IE.js")

## Load prison dropdown from s3
dt.prisons <- data.table::as.data.table(s3tools::s3_path_to_full_df(
  "alpha-app-anvil-access-tool/prisons_and_offices_v2.csv", header = FALSE))

fields <- c("first_name", "surname", "prison", "role", "quantum_id", 
            "bentham", "bentham_reason", "drugs_prison", "drugs_prison_reason",
            "safety", "categorisation", "date_requested", 
            "account", "email")

email_choice <- c("@noms.gsi.gov.uk", "@justice.gov.uk", "@hmps.gov.uk", "@hmcts.gov.uk",
                  "@probation.gov.uk", "@justice.gsi.gov.uk", "@digital.justice.gov.uk")
foundErrors <- 0
quantumErr <- 0

tick <- "<i class=\"fa fa-check de-color\" aria-hidden=\"true\"></i>"
cross <- "<i class=\"fa fa-times de-color\" aria-hidden=\"true\"></i>"


saveData <- function (data) {
  ## Reload data from S3
  responses <- as.data.table(s3tools::s3_path_to_full_df(
    "alpha-app-anvil-access-tool/anvil-app-responses_v4.csv", header = TRUE))
  names(responses) <- fields
  responses$date_requested <- as.Date(responses$date_requested, origin = "1970-01-01")

  if (exists ("responses")) {
    data$bentham <- as.integer(data$bentham)
    data$drugs_prison <- as.integer(data$drugs_prison)
    responses <- rbind(responses, data, fill = TRUE)
  } else {
    responses <- data
    responses$bentham <- as.integer(data$bentham)
    responses$drugs_prison <- as.integer(data$drugs_prison)
    responses$account <- "Requested"
  }

  #Format data and save to s3
  responses$prison <- as.character (responses$prison)
  responses$role <- as.character (responses$role)
  responses$first_name <- as.character (responses$first_name)
  responses$surname <- as.character (responses$surname)
  responses$quantum_id <- as.character (responses$quantum_id)
  responses$quantum_id <- tolower (responses$quantum_id)
  responses$email <- as.character(responses$email)
  responses$bentham_reason <- as.character(responses$bentham_reason)
  responses$drugs_prison_reason <- as.character(responses$drugs_prison_reason)
  responses$date_requested <- as.Date(responses$date_requested, origin = "1970-01-01")
  
  s3tools::write_df_to_csv_in_s3 (responses,
                                  "alpha-app-anvil-access-tool/anvil-app-responses_v4.csv",
                                  overwrite = TRUE,
                                  row.names = FALSE)

}

loadData <- function() {
  responses <- as.data.table(s3tools::s3_path_to_full_df(
    "alpha-app-anvil-access-tool/anvil-app-responses_v4.csv", header = TRUE))
  names(responses) <- fields
  responses
}

