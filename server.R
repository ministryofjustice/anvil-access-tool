#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(data.table)

fields<-c("first_name","surname","prison","role","quantum_id","apps_needed")
errorMes<-c(0,0,0,0,0,0,0)
foundErrors<-0

saveData<-function(data){
  #filePath<-"/home/hbutchermoj/anvil-access-app/anvil-access-tool"
  #fileName<-"test.csv"
  
  #write.csv(x=data,file=file.path(filePath,fileName),row.names = FALSE,quote=TRUE)
  
  data<-as.data.frame(t(data))
  if(exists("responses")){
    responses<<-rbind(responses,data)
  }else{
    responses<<-data
  }
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  
  formData<-reactive({
    data<-sapply(fields,function(x) input[[x]])
    data
  })
  
  observeEvent(input$submitButton,{
    
  
    
    
    #Check ID is 6 characters long
    if (nchar(input$quantum_id)!=6){
      foundErrors<-1
      msg<-"Quantum ID must be 6 characters long."
    }
    
    
    #Check second letter of ID is "Q"
   if (substring(input$quantum_id,2,2)!="Q" && substring(input$quantum_id,2,2)!="q"){
      foundErrors<-1
      msg<-paste(msg,"Quantum ID must have 'Q' as it's second character.")
    }
      
    
    #Check 1st character is character
    if (is.na(as.numeric(substring(input$quantum_id,1,1)))){
      foundErrors<-1
      msg<-paste(msg, "first character of Quantum ID must be a letter")
    }
  
    #Check 2nd character is character
    if (is.na(as.numeric(substring(input$quantum_id,2,1)))){
      foundErrors<-1
      msg<-paste(msg, "Second character of Quantum ID must be a letter")
    }

    #Check 3rd character is character
    if (is.na(as.numeric(substring(input$quantum_id,3,1)))){
      foundErrors<-1
      msg<-paste(msg, "Third character of Quantum ID must be a letter")
    }

    #Check 6th character is character
    if (is.na(as.numeric(substring(input$quantum_id,6,1)))){
      foundErrors<-1
      msg<-paste(msg, "Sixth character of Quantum ID must be a letter")
    }

    #Check 4th and 5th characters are numbers
    if (!is.na(as.numeric(substring(input$quantum_id,4,5)))){
      foundErrors<-1
      msg<-paste(msg, "Fourth & Fifth characters of Quantum ID must be numbers")
    }

    
    if (foundErrors==1){
      showNotification(msg,type="error")
      output$quantum_error<-renderText({
        "Quantum ID must be of the format: 3 Letters, 2 Numbers, 3 Letters. The second letter should be 'Q'."
      })
    }else{
    saveData(formData())  
    showNotification("Thank you. Your responses have been submitted successfully.",type="message")
    shinyjs::reset("form")
    
      #  showNotification("Error: Your Quantum ID must be 3 letter, 2 digits, 1 letter.",type="error")
    }
  })

  

})
