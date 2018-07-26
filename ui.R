library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(data.table)
library(shinyjs)

# Load prison dropdown from s3
dt.prisons<-as.data.table(s3tools::s3_path_to_full_df("alpha-app-anvil-access-tool/prisons_and_offices_v2.csv",header=FALSE))


# Define UI for application
shinyUI(shiny::fluidPage(

  useShinyjs(),
  shiny::includeCSS("www/custom.css"),


  shiny::div(style="background-color:WhiteSmoke;",
  id="form",

    #Main page
    shiny::fluidRow(

      shiny::column(width=4, offset=1, #class="class_selections",

      shiny::fluidRow(#class="class_top_banner",
          shiny::div(class="class_mainTitle",
              "Anvil access request form")
        ),

        shiny::div(class="class_selections",
          shiny::fluidRow(
            shiny::div(class="class_intro",
              "To gain access to the HMPPS Anvil apps, please enter your details in the fields below and click submit.")
          ),


          #Section First Name
          shiny::fluidRow(
            shiny::column(width=10,
              shiny::div(class="class_first_name",
                shiny::textInput("first_name",
                  label=h3("First Name:"),
                  width="100%"))
            ),

            shiny::column(width=1,
              shiny::div(
                shiny::uiOutput("first_name_icon"))
            )
          ),

          shiny::div(class="class_first_name_err",
            shiny::textOutput("first_name_err")),

          #Section: Surname
          shiny::fluidRow(#style="background-color:blue;",
            shiny::column(width=10,
              shiny::div(class="class_surname",
                shiny::textInput("surname",
                  label=h3("Surname:"),
                  width="100%"))
            ),

            shiny::column(width=1,
              shiny::div(
                shiny::uiOutput("surname_icon"))
            )
          ),

          shiny::div(class="class_surname_err",
            shiny::textOutput("surname_err")),

          #Section: prison
          shiny::fluidRow(
            shiny::column(width=10,
              shiny::div("Prison:", class="class_prison",
                shinyWidgets::pickerInput(
                  inputId = "prison",
                  label = NULL,
                  width="100%",
                  selected="",
                  choices=c("--Please Select Prison--",dt.prisons)))
            ),

            shiny::column(width=1,
              shiny::div(
                shiny::uiOutput("prison_icon"))
            )
          ),

          shiny::div(class="class_prison_err",
            shiny::textOutput("prison_err")),

          #Section: role
          shiny::fluidRow(
            shiny::column(width = 10,
              shiny::div(class="class_role",
                shiny::textInput("role",
                  label=h3("Role:"),
                  width="100%"))
            ),

            shiny::column(width=1,
              shiny::div(
                shiny::uiOutput("role_icon"))
            )
          ),

          shiny::div(class="class_role_err",
            shiny::textOutput("role_err")),


          #Section: quantum id
          shiny::fluidRow(
            shiny::column(width =10,
              shiny::div(class="class_quantum_id",
                shiny::textInput(
                  "quantum_id",
                  label=h3("Quantum ID:"),
                  width="100%"))
            ),
            shiny::column(width=1,
              shiny::uiOutput("quantum_icon"))
          ),

          shiny::div(class="class_quantum_error",
            shiny::textOutput("quantum_error")),

          #Section: checkbox
          fluidRow(
            shiny::column(width=10,
              shiny::div(class="class_app_checkbox",
                shinyWidgets::awesomeCheckboxGroup(
                  inputId = "apps_needed",
                  label = "Please select which apps you require access to",
                  choices = c("Safety Diagnostic Tool", "Bentham", "Prisoner Categorisation"),
                  inline = TRUE,
                  status = "danger"))
            ),

            shiny::column(width=1,
              shiny::uiOutput("apps_icon"))
          ),

          shiny::div(class="class_apps_error",
            shiny::textOutput("apps_err")),

          shiny::div(class="class_submitButton",
            shiny::actionButton ("submitButton", "Submit"))
        )
      ),

      shiny::column(width=7,
        shiny::div(class = "class_access_msg",
          "The following users at your establishment already have access:"),

        shiny::div(class="class_prison_null",
            shiny::textOutput("prison_access_null")),

        shiny::div(class="class_prison_access",
          DT::dataTableOutput("prison_access"))


      )
    )
  )
))



