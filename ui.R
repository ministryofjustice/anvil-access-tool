
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)

#dt.prisons<-as.data.table(s3tools::s3_path_to_full_df("alpha-anvil-access-tool/prisons.txt"))
#dt.prisons<-read.table("/home/hbutchermoj/anvil-access-app/anvil-access-tool/prisons.txt",header=FALSE)
dt.prisons<-read.delim("prisons.txt",header=FALSE)
dt.prisons[,1]<-as.character(dt.prisons[,1])
choice.prisons<-dt.prisons$V1

# Define UI for application
shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  #dashboardHeader(title="Anvil access request form",titleWidth=450),
  useShinyjs(),
  div(
  id="form",
  


  dashboardBody(
    
    fluidRow(
      box(title="Anvil access request form",solidHeader = TRUE,"To gain access to the HMPPS Anvil apps, please fill in the fields below and submit the form.")
    ),
    
    fluidRow(
      box(
        textInput(
          "first_name",
          label=h3("First Name:"))
      )
    ),
      
    fluidRow(
      box(
        textInput(
          "surname",
          label=h3("Surname:"))
      )
    ),
    
    fluidRow(
      box(
        pickerInput(
          inputId = "prison",
          label = "Select Prison:", 
          choices<-unique(dt.prisons$V1))
        )
    ),
      
    fluidRow(
      box(
        textInput(
          "role",
          label=h3("Role:"))
        )
    ),
  
    fluidRow(
      box(
        textInput(
          "quantum_id",
          label=h3("Quantum ID:")),
        textOutput("quantum_error")
        )
    ),
  
    fluidRow(
    
      box(
      awesomeCheckboxGroup(
        inputId = "apps_needed",
        label = "Please select which apps you require access to", 
        choices = c("Safety Diagnostic Tool", "Bentham", "Prisoner Categorisation"),
        inline = TRUE,
        status = "danger")
      )
    ),
    
    fluidRow(
      box(
      actionButton ("submitButton", "Submit"))
    )
  )
)


  

)
)

