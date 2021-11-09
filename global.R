## global.R

## Load libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(data.table)
library(DT)
library(shinyjs)
library(shinyalert)
library(readr)
library(s3tools)

## Check if app is being opening in IE and warn if so
##NOT CURRENTLY WORKING.........
shinyjs::useShinyjs()
tags$meta("http-equiv" = "X-UA-compatible", content = "IE = edge")
includeScript("www/IE.js")

## Load prison dropdown from s3
# dt.prisons <- data.table::as.data.table(s3tools::s3_path_to_full_df(
#   "alpha-app-anvil-access-tool/prisons_and_offices_v2.csv", header = FALSE))

##updated prison lookup - updated on feb 2021
dt.prisons <- data.table::as.data.table(s3tools::s3_path_to_full_df(
  "alpha-app-anvil-access-tool/prison_area_lookup_aat.csv", header = FALSE))

dt.users <- data.table::as.data.table(s3tools::s3_path_to_full_df(
  "alpha-app-anvil-access-tool/anvil_app_responses_v6.csv", header = TRUE))


fields <- c("first_name", "surname", "area", "prison", "role",
            "id", "email", "date_requested", 
            "bentham", "bentham_status",
            "drug_convey", "drug_convey_status",
            "network", "network_status",
            "visitors", "visitors_status",
            "novel_drugs", "novel_drugs_status",
            "reason")

email_choice <- c("@noms.gsi.gov.uk", "@justice.gov.uk", "@hmps.gov.uk", "@hmcts.gov.uk",
                  "@probation.gov.uk", "@justice.gsi.gov.uk", "@digital.justice.gov.uk")
foundErrors <- 0
quantumErr <- 0
newuserErr <- 0

# tick <- "<i class=\"fa fa-check\" style=\"color:#34eb4c;\"\" aria-hidden=\"true\"></i>"
tick <- "<i class=\"fa fa-check\" style=\"color:#34eb4c;\"><!-- icon --></i>"
cross <- "<i class=\"fa fa-times\" style=\"color:#ff0303;\"><!-- icon --></i>"
pending <- "<i class=\"far fa-clock\" aria-hidden=\"true\"></i>"
status_lookup <- data.table(status=c("requested","approved","denied","created","deleted",NA),
                            status_disp=c(pending,
                                          "Approved",
                                          cross,
                                          tick,
                                          "Deleted",
                                          "N/A"))
PRISON_SELECT_TEXT <- "--Please Select Prison or Team--"
WHOLE_REGION_TEXT <- "Whole region"

loadData <- function() {
  ## Reload data from S3
  responses <- as.data.table(s3_path_to_full_df(
    "alpha-app-anvil-access-tool/anvil_app_responses_v6.csv",
    header = TRUE))
  
  ##clean up fields and correct formats
  # names(responses) <- fields
  responses[date_requested == "", date_requested := NA]
  responses[bentham == "", bentham := 0]
  responses[drug_convey == "", drug_convey := 0]
  responses[network == "", network := 0]
  responses[visitors == "", visitors := 0]
  responses[novel_drugs == "", novel_drugs := 0]
  responses[reason == "" | is.null(reason) | reason == "NULL", reason := NA]
  responses[, date_requested := format(as.Date(date_requested, format = "%d/%m/%Y"), "%Y-%m-%d")]
  
  return(responses)
}

saveData <- function (data) {
  
  responses <- loadData()
  responses <- rbind(responses, data, fill = F)
  
  ##make sure there are no duplicate rows (for each quantum id).
  ##note order of the table is reversed and then restored...
  ##...to keep the latest entry if date_requested occurs more than once for a given quantum id.
  responses <- responses[seq(nrow(responses),1)]
  responses <- responses[,
                         .SD[which.max(ifelse(is.na(date_requested),
                                              as.Date("0000-01-01"),
                                              as.Date(date_requested)))],
                         by=id]
  responses <- responses[seq(nrow(responses),1)]
  
  ##IF REQUIRED
  ##change date format back to "Excel standard" format i.e. %d/%m/%Y
  responses[, date_requested := format(as.Date(date_requested, format = "%Y-%m-%d"), "%d/%m/%Y")]
  
  s3tools::write_df_to_csv_in_s3 (responses,
                                  "alpha-app-anvil-access-tool/anvil_app_responses_v6.csv",
                                  overwrite = TRUE,
                                  row.names = FALSE)
}
