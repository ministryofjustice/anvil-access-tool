library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(data.table)
library(shinyjs)

# Load prison dropdown from s3
dt.prisons<-as.data.table(s3tools::s3_path_to_full_df("alpha-anvil-access-tool/prisons_and_offices_v2.csv",header=FALSE))


# Define UI for application
shinyUI(fluidPage(

  useShinyjs(),
  includeCSS("www/custom.css"),


  div(style="background-color:WhiteSmoke;",
  id="form",

    #Main page
    fluidRow(

      column(width=4, offset=1, #class="class_selections",

        fluidRow(#class="class_top_banner",
          div(class="class_mainTitle",
              "Anvil access request form")
        ),

        div(class="class_selections",
          fluidRow(
            div(class="class_intro",
              "To gain access to the HMPPS Anvil apps, please enter your details in the fields below and click submit.")
          ),


          #Section First Name
          fluidRow(
            column(width=10,
              div(class="class_first_name",
                textInput("first_name",
                  label=h3("First Name:"),
                  width="100%"))
            ),

            column(width=1,
              div(
                uiOutput("first_name_icon"))
            )
          ),

          div(class="class_first_name_err",
            textOutput("first_name_err")),

          #Section: Surname
          fluidRow(#style="background-color:blue;",
            column(width=10,
              div(class="class_surname",
                textInput("surname",
                  label=h3("Surname:"),
                  width="100%"))
            ),

            column(width=1,
              div(
                uiOutput("surname_icon"))
            )
          ),

          div(class="class_surname_err",
            textOutput("surname_err")),

          #Section: prison
          fluidRow(
            column(width=10,
              div("Select Prison:", class="class_prison",
                pickerInput(
                  inputId = "prison",
                  label = NULL,
                  width="100%",
                  choices<- dt.prisons[,1]))
            ),

            column(width=1,
              div(
                uiOutput("prison_icon"))
            )
          ),

          div(class="class_prison_err",
            textOutput("prison_err")),

          #Section: role
          fluidRow(
            column(width = 10,
              div(class="class_role",
                textInput("role",
                  label=h3("Role:"),
                  width="100%"))
            ),

            column(width=1,
              div(
                uiOutput("role_icon"))
            )
          ),

          div(class="class_role_err",
            textOutput("role_err")),


          #Section: quantum id
          fluidRow(
            column(width =10,
              div(class="class_quantum_id",
                textInput(
                  "quantum_id",
                  label=h3("Quantum ID:"),
                  width="100%"))
            ),
            column(width=1,
              uiOutput("quantum_icon"))
          ),

          div(class="class_quantum_error",
            textOutput("quantum_error")),

          #Section: checkbox
          fluidRow(
            column(width=10,
              div(class="class_app_checkbox",
                awesomeCheckboxGroup(
                  inputId = "apps_needed",
                  label = "Please select which apps you require access to",
                  choices = c("Safety Diagnostic Tool", "Bentham", "Prisoner Categorisation"),
                  inline = TRUE,
                  status = "danger"))
            ),

            column(width=1,
              uiOutput("apps_icon"))
          ),

          div(class="class_apps_error",
            textOutput("apps_err")),

          div(class="class_submitButton",
            actionButton ("submitButton", "Submit"))
        )
      ),

      column(width=7,
        div(class = "class_access_msg",
          "The following users at your establishment already have access:"),

        div(class="class_prison_null",
            textOutput("prison_access_null")),

        div(class="class_prison_access",
          dataTableOutput("prison_access"))


      )
    )
  )
))



