
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(data.table)

fields<-c("first_name","surname","prison","role","quantum_id","bentham","safety","categorisation")
foundErrors<-0

saveData<-function(data){
  
  #write.csv(x=data,file=file.path(filePath,fileName),row.names = FALSE,quote=TRUE)
  
  data<-as.data.frame(t(data),stringsAsFactors=FALSE)
  if(exists("responses")){
    bentham_check<-as.integer("Bentham"%in%unlist(responses[nrow(responses),6]))
    safety_check<-as.integer("Safety Diagnostic Tool"%in%unlist(responses[nrow(responses),6]))
    cat_check<-as.integer("Prisoner Categorisation"%in%unlist(responses[nrow(responses),6]))
    data[["bentham"]]<-bentham_check;data
    data[["safety"]]<-safety_check;data
    data[["categorisation"]]<-cat_check;data
    responses<<-rbind(responses,data)
  }else{
    responses<<-data
    responses$bentham<<-as.integer("Bentham"%in%unlist(responses[1,6]))
    responses$safety<<-as.integer("Safety Diagnostic Tool"%in%unlist(responses[1,6]))
    responses$categorisation<<-as.integer("Prisoner Categorisation"%in%unlist(responses[1,6]))
  }
}


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  
  formData<-reactive({
    data<-sapply(fields,function(x) input[[x]])
  })
  
  responses_subset<-reactive({responses[responses$prison==input$prison,c('first_name','surname','role','quantum_id','bentham','safety','categorisation')]})


  output$prison_access<-renderDataTable({
    req(nrow(responses_subset())>0)
    responses_subset()
    #options=list("searching":false)
    })


  #Submit Button action
  observeEvent(input$submitButton,{
    if (nchar(input$first_name)==0){
      output$first_name_err<-renderText({"First Name must not be blank"})
      output$first_name_icon<-renderUI({icon("times")})
    }else{
      output$first_name_icon<-renderUI({icon("check")})
    }
    
    
    if (nchar(input$surname)==0){
      output$surname_err<-renderText({"Surname must not be blank"})
      output$surname_icon<-renderUI({icon("times")})
    }else{
      output$surname_icon<-renderUI({icon("check")})
    }
    
    
    if (nchar(input$role)==0){
      output$role<-renderText({"Role must not be blank"})
      output$role_icon<-renderUI({icon("times")})
    }else{
      output$role_error<-renderUI({icon("check")})
    }
    
    if (nchar(input$prison)==0){
      output$prison<-renderText({"Prison must not be blank"})
      output$prison<-renderUI({icon("times")})
    }else{
      output$prison<-renderUI({icon("check")})
    }
    
    output$quantum_error<-renderText({"Quantum ID must be of the format: AAA99A with a Q as the second character."})
    output$quantum_icon<-renderUI({icon("times")})
    
    #Create Quantum message
    msg<-""
    
    #Check ID is 6 characters long
    if (nchar(input$quantum_id)!=6){
      foundErrors<-1
      msg<-paste(msg,"Quantum ID must be 6 characters long.")
    }
    
    
    #Check second letter of ID is "Q"
   if (substring(input$quantum_id,2,2)!="Q" && substring(input$quantum_id,2,2)!="q"){
      foundErrors<-1
      msg<-paste(msg,"Quantum ID must have 'Q' as it's second character.")
    }
      
    
    #Check 1st character is character
    if (!is.na(as.numeric(substring(input$quantum_id,1,1)))){
      foundErrors<-1
      msg<-paste(msg, "first character of Quantum ID must be a letter")
    }
  
    #Check 2nd character is character
    if (!is.na(as.numeric(substring(input$quantum_id,2,1)))){
      foundErrors<-1
      msg<-paste(msg, "Second character of Quantum ID must be a letter")
    }

    #Check 3rd character is character
    if (!is.na(as.numeric(substring(input$quantum_id,3,1)))){
      foundErrors<-1
      msg<-paste(msg, "Third character of Quantum ID must be a letter")
    }

    #Check 6th character is character
    if (!is.na(as.numeric(substring(input$quantum_id,6,1)))){
      foundErrors<-1
      msg<-paste(msg, "Sixth character of Quantum ID must be a letter")
    }

    #Check 4th and 5th characters are numbers
    if (is.na(as.numeric(substring(input$quantum_id,4,5)))){
      foundErrors<-1
      msg<-paste(msg, "Fourth & Fifth characters of Quantum ID must be numbers")
    }

    
    if (foundErrors==1){
      showNotification(id="error_notif",msg,type="error",duration=NULL)
      output$quantum_error<-renderText({
        "Quantum ID must be of the format: 3 Letters, 2 Numbers, 3 Letters. The second letter should be 'Q'."})
      output$quantum_icon<-renderUI({icon("times")})
      
      
    }else{
    saveData(formData())  
    showNotification(id="success_notif","Thank you. Your responses have been submitted successfully.",type="message",duration=NULL)
    shinyjs::reset("form")
    foundErrors<-0

    }
  })

  

})
