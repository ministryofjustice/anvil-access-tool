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

## Load prison dropdown from s3
dt.prisons <- data.table::as.data.table(s3tools::s3_path_to_full_df(
  "alpha-app-anvil-access-tool/prisons_and_offices_v2.csv", header = FALSE))

fields <- c("first_name", "surname", "prison", "role", "quantum_id",
            "bentham", "safety", "categorisation", "account", "email")

email_choice <- c("@noms.gsi.gov.uk", "@justice.gov.uk", "@hmps.gov.uk", "@hmcts.gov.uk",
                  "@probation.gov.uk", "@justice.gsi.gov.uk", "@digital.justice.gov.uk")
foundErrors <- 0
quantumErr <- 0

tick <- "<i class=\"fa fa-check de-color\" aria-hidden=\"true\"></i>"
cross <- "<i class=\"fa fa-times de-color\" aria-hidden=\"true\"></i>"

saveData <- function (data) {
  ## Reload data from S3
  responses <- as.data.table(s3tools::s3_path_to_full_df(
    "alpha-app-anvil-access-tool/anvil-app-responses.csv", header = TRUE))
  responses <- responses[,1:10]
  names(responses) <- fields
  data <- as.data.frame (t(data), stringsAsFactors = FALSE)
  if (exists ("responses")) {
    data$bentham <- as.integer(data$bentham)
    data$safety <- as.integer(data$safety)
    data$categorisation <- as.integer(data$categorisation)
    data$account <- "Requested"
    responses <- rbind (responses[,1:10], data)
  }else{
    responses <- data
    responses$bentham <- as.integer(data$bentham)
    responses$safety <- as.integer(data$safety)
    responses$categorisation <- as.integer(data$categorisation)
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

  s3tools::write_df_to_csv_in_s3 (responses,
                                  "alpha-app-anvil-access-tool/anvil-app-responses.csv",
                                  overwrite = TRUE,
                                  row.names = FALSE)
}

loadData <- function() {
  responses <- as.data.table(s3tools::s3_path_to_full_df(
    "alpha-app-anvil-access-tool/anvil-app-responses.csv", header = TRUE))
  responses <- responses[, 1:10]
  names(responses) <- fields
  responses
}
