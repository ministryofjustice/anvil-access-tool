
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)

dt.prisons<-as.data.table(s3tools::s3_path_to_full_df("alpha-anvil-access-tool/prisons.csv",header=FALSE))


# Define UI for application
shinyUI(fluidPage(
 # theme = shinytheme("cerulean"),
  #dashboardHeader(title="Anvil access request form",titleWidth=450),
  useShinyjs(),
  includeCSS("www/custom.css"),
  div(
  id="form",
  
    #row 1 
    fluidRow(
      column(width=8, style="background-color:yellow;",
      div(
        class="class_mainTitle",
        title="Anvil access request form",solidHeader = TRUE,"To gain access to the HMPPS Anvil apps, please fill in the fields below and submit the form."),
    
      fluidRow(
        column(width=6,
        div(
        class="class_first_name",
        textInput(
          "first_name",
          label=h3("First Name:")))),
        
        column(width=2,
        
        div(
          class="class_first_name_icon",
          uiOutput("first_name_icon")))
        ),

        
      
      div(
        class="class_first_name_err",
        textOutput("first_name_err")),


      div(
        class="class_surname",
        textInput(
          "surname",
          label=h3("Surname:"))),
      
      div(
        class="class_surname_err",
        textOutput("surname_err")),

      div(
        class="class_prison",
        pickerInput(
          inputId = "prison",
          label = "Select Prison:", 
          choices<- dt.prisons[,1]),
        textOutput("prison_err"),
        uiOutput("prison_icon")),

      div(
        class="class_role",
        textInput(
          "role",
          label=h3("Role:"))),

      div(
        class="class_role_err",
        textOutput("role_err")),
      
      div(
        class="class_quantum_id",
        textInput(
          "quantum_id",
          label=h3("Quantum ID:"))),
      
      div(
        class="class_quantum_error",
        textOutput("quantum_error"),
        uiOutput("quantum_icon")),
   
        div(
          class="class_app_checkbox",
          awesomeCheckboxGroup(
            inputId = "apps_needed",
            label = "Please select which apps you require access to", 
            choices = c("Safety Diagnostic Tool", "Bentham", "Prisoner Categorisation"),
            inline = TRUE,
            status = "danger"))
        
      ),
      
      column(width=2, style="background-color:green",
             
             
             div(
               class="class_surname_icon",
               uiOutput("surname_icon")) 
             
             ),
      
      column(width=8, style="background-color:red;",
             div(
               class = "class_access_msg",
               "The following users at your prison already have access"
             ),
             
             div(
               class="class_prison_access",
               uiOutput("prison_access"))
      )    

    ),
    
    

fluidRow(
     div(
       class="class_submitButton",
       actionButton ("submitButton", "Submit"))

      )
    
  
  

  )))



