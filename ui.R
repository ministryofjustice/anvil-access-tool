library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(data.table)
library(shinyjs)

dt.prisons<-as.data.table(s3tools::s3_path_to_full_df("alpha-anvil-access-tool/prisons_and_offices.csv",header=FALSE))


# Define UI for application
shinyUI(fillPage(
  
  tags$head(
    tags$style(HTML(".shiny-notification{position:fixed,top:200px;width:15em;}"))
  ),
  
  useShinyjs(),
  includeCSS("www/custom.css"),
  div(style="background-color:WhiteSmoke;",
  id="form",
  
    #Main page
    fluidRow(

        column(width=5, offset=1,#class="class_selections",
               
          fluidRow(#class="class_top_banner",
            div(
              class="class_mainTitle",
                "Anvil access request form")
          ),
               
          div(class="class_selections",  
            fluidRow(
              div(
                class="class_intro",
                  "To gain access to the HMPPS Anvil apps, please enter your details in the fields below and click submit.")
            ),
               

            #Section First Name  
            fluidRow(
              column(width=10,
                div(
                  class="class_first_name",
                  textInput(
                    "first_name",
                    label=h3("First Name:")))
              ),
              
              column(width=1,
                div(
                  #class="class_first_name_icon",
                  uiOutput("first_name_icon"))
              )
            ),
      
            div(
              class="class_first_name_err",
              textOutput("first_name_err")),
       
            #Section: Surname
            fluidRow(#style="background-color:blue;",
              column(width=10,
                div(
                  class="class_surname",
                  textInput(
                    "surname",
                    label=h3("Surname:")))
              ),
            
              column(width=1,
                div(
                 # class="class_surname_icon",
                  uiOutput("surname_icon"))
              )
            ),
            
            div(
              class="class_surname_err",
              textOutput("surname_err")),
            
            #Section: prison
            fluidRow(#style="background-color:green;",
              column(width=10,
                     
                div(
                  class="class_prison",
                  pickerInput(
                    inputId = "prison",
                    label = "Select Prison:", 
                    choices<- dt.prisons[,1]))
              ),
              column(width=1,
                div(
               #   class="class_prison_icon",
                  uiOutput("prison_icon"))
                  
              )
            ),
            
            div(
              class="class_prison_err",
              textOutput("prison_err")),
            
            #Section: role
            fluidRow(#style="background-color:yellow;",
              column(width = 10,
                div(
                  class="class_role",
                  textInput(
                    "role",
                    label=h3("Role:")))
              ),
              
              column(width=1,
                div(
                 # class="class_role_icon",
                  uiOutput("role_icon"))
              )
            ),
      
            div(
              class="class_role_err",
              textOutput("role_err")),
            
            
            #Section: quantum id
            fluidRow(#style="background-color:red;",
              column(width =10,
                div(
                  class="class_quantum_id",
                    textInput(
                      "quantum_id",
                      label=h3("Quantum ID:")))
              ),
              column(width=1,
                div(
              #    class="class_quantum_icon",
                  uiOutput("quantum_icon"))
              )
            ),
            
            div(
              class="class_quantum_error",
              textOutput("quantum_error")),
            
            #Section: checkbox
            div(#style="background-color:blue;",
              class="class_app_checkbox",
              awesomeCheckboxGroup(
                inputId = "apps_needed",
                label = "Please select which apps you require access to", 
                choices = c("Safety Diagnostic Tool", "Bentham", "Prisoner Categorisation"),
                inline = TRUE,
                status = "danger")),
            
            div(
              class="class_submitButton",
              actionButton ("submitButton", "Submit"))
          )),
      
    column(width=6,
      div(
        class = "class_access_msg",
          "The following users at your establishment already have access:"),
             
      div(
        class="class_prison_access",
        dataTableOutput("prison_access"))
    )
  )

    
    
  ) 
))



