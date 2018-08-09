library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(data.table)
library(DT)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {


  form_data <- shiny::reactive({
    data <- sapply(fields, function(x) input[[x]])
  })


  responses_subset <- shiny::reactive({
    responses[responses$prison == input$prison, c("first_name", "surname", "role", "quantum_id",
                                                 "bentham", "safety", "categorisation")]})

  output$prison_access <- DT::renderDataTable({
    shiny::req(nrow(responses_subset()) > 0)
    access_table <- responses_subset()
    access_table$bentham <- ifelse (access_table$bentham == 1, tick, cross)
    access_table$safety <- ifelse (access_table$safety == 1, tick, cross)
    access_table$categorisation <- ifelse (access_table$categorisation == 1, tick, cross)
    DT::datatable(access_table, escape = FALSE, options = list("searching" = FALSE,
                                                    paging = FALSE,
                                                    scrollCollapse = T,
                                                    dom = "t",
                                                    scrollY = "500px"),
                                                    rownames = FALSE,
                                                    colnames = c("First Name", "Surname", "Role", "Quantum ID",
                                                    "Bentham", "Safety Tool", "Categorisation Tool"))
  })

  output$prison_access_null <- shiny::renderText({
    shiny::req(!nrow(responses_subset()) > 0)
    "No users at your prison have access."
  })


  #Submit Button action
  observeEvent(input$submitButton, {
    if (nchar(input$first_name) == 0){
      output$first_name_err <- shiny::renderText({
        "First Name must not be blank."})
      output$first_name_icon <- shiny::renderUI({icon("times")})
      foundErrors <- 1
    }else{
      output$first_name_err <- shiny::renderText({""})
      output$first_name_icon <- shiny::renderUI({icon("check")})
    }

    if (nchar(input$surname) == 0){
      output$surname_err <- shiny::renderText({"Surname must not be blank."})
      output$surname_icon <- shiny::renderUI({icon("times")})
      foundErrors <- 1
    }else{
      output$surname_err <- shiny::renderText({""})
      output$surname_icon <- shiny::renderUI({icon("check")})
    }


    if (nchar(input$role) == 0){
      output$role_err <- shiny::renderText({"Role must not be blank."})
      output$role_icon <- shiny::renderUI({icon("times")})
      foundErrors <- 1
    }else{
      output$role_err <- shiny::renderText({""})
      output$role_icon <- shiny::renderUI({icon("check")})
    }

    if ((input$prison) == "--Please Select Prison--"){
      output$prison_err <- shiny::renderText({"Please select a prison."})
      output$prison_icon <- shiny::renderUI({icon("times")})
      foundErrors <- 1
    }else{
      output$prison_err <- shiny::renderText({""})
      output$prison_icon <- shiny::renderUI({icon("check")})
    }


    if(is.null(input$apps_needed)){
      output$apps_err <- shiny::renderText({"Please select at least one app."})
      output$apps_icon <- shiny::renderUI({icon("times")})
      foundErrors <- 1
    }else{
      output$apps_err <- shiny::renderText({""})
      output$apps_icon <- shiny::renderUI({icon("check")})
    }

    #Check ID is 6 characters long
    if (nchar(input$quantum_id) != 6){
      foundErrors <- 1
      quantumErr <- 1
    }


    #Check second letter of ID is "Q"
    if (substring(input$quantum_id, 2, 2) != "Q" && substring(input$quantum_id, 2, 2) != "q"){
      foundErrors <- 1
      quantumErr <- 1
    }


    #Check 1st character is character
    if (!is.na(as.numeric(substring(input$quantum_id, 1, 1)))){
      foundErrors <- 1
      quantumErr <- 1
    }

    #Check 2nd character is character
    if (!is.na(as.numeric(substring(input$quantum_id, 2, 1)))){
      foundErrors <- 1
      quantumErr <- 1
    }

    #Check 3rd character is character
    if (!is.na(as.numeric(substring(input$quantum_id, 3, 1)))){
      foundErrors <- 1
      quantumErr <- 1
    }

    #Check 6th character is character
    if (!is.na(as.numeric(substring(input$quantum_id, 6, 1)))){
      foundErrors <- 1
      quantumErr <- 1
    }

    #Check 4th and 5th characters are numbers
    if (is.na(as.numeric(substring(input$quantum_id, 4, 5)))){
      foundErrors <- 1
      quantumErr <- 1
    }


    #Print Quantum ID validation messages
    if (quantumErr == 1){
      output$quantum_error <- shiny::renderText({"Quantum ID must be of the format: AAA99A with a Q as the second character."})
      output$quantum_icon <- shiny::renderUI({icon("times")})
    }else{
      output$quantum_error <- shiny::renderText({""})
      output$quantum_icon <- shiny::renderUI({icon("check")})
    }


    if (foundErrors == 1){
      #Show error message
      shiny::showNotification(id = "error_notif", "Please correct the errors listed above.", type = "error", duration = 5)

    }else{
      #Save data to responses datatable & show success message
      saveData (form_data())
      shiny::showNotification(id = "success_notif", "Thank you. Your responses have been submitted successfully.", type = "message", duration = 5)
      shinyjs::reset("form")
      foundErrors <- 0
      quantumErr <- 0
    }
  })
  
  
})
