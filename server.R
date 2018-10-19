## server.R

## Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  form_data <- reactive({
    data <- sapply(fields, function(x) input[[x]])
  })

  responses_subset <- reactive({
    responses <- loadData()
    responses[responses$prison == input$prison,
              c("first_name", "surname", "role", "quantum_id",
                "bentham", "safety", "categorisation", "account", "email")]
  })
  
  output$prison_access <- renderDataTable({
    req(nrow(responses_subset()) > 0)
    access_table <- responses_subset()
    access_table$bentham <- ifelse(access_table$bentham == 1, tick, cross)
    access_table$safety <- ifelse(access_table$safety == 1, tick, cross)
    access_table$categorisation <- ifelse(access_table$categorisation == 1, tick, cross)
    
    ## Render table but remove final column (9) so don't display email address
    ## as makes table too wide
    datatable(access_table[,-9], escape = FALSE,
              options = list(paging = FALSE,
                             scrollCollapse = T,
                             dom = "ft", 
                             scrollX = FALSE,
                             scrollY = "500px"),
              rownames = FALSE,
              colnames = c("First Name", "Surname", "Role", "Quantum ID",
                           "Bentham", "Safety Tool", "Cat Tool",
                           "Account Status"))
  })

  output$prison_access_null <- renderText({
    req(!nrow(responses_subset()) > 0)
    "No users at your prison have access."
  })

  #Submit Button action
  observeEvent(input$submitButton, {
    if(nchar(input$first_name) == 0) {
      output$first_name_err <- renderText({
        "First Name must not be blank."})
      output$first_name_icon <- renderUI({icon("times")})
      foundErrors <- 1
    } else {
      output$first_name_err <- renderText({""})
      output$first_name_icon <- renderUI({icon("check")})
    }

    if(nchar(input$surname) == 0) {
      output$surname_err <- renderText({"Surname must not be blank."})
      output$surname_icon <- renderUI({icon("times")})
      foundErrors <- 1
    } else {
      output$surname_err <- renderText({""})
      output$surname_icon <- renderUI({icon("check")})
    }

    if(!grepl("@", input$email) || (grepl(",", input$email))) {
      output$email_err <- renderText({"Email address invalid."})
      output$email_icon <- renderUI({icon("times")})
      foundErrors <- 1
    } else {
      output$email_err <- renderText({""})
      output$email_icon <- renderUI({icon("check")})
    }

    if((input$email != input$email_confirmation) || nchar(input$email_confirmation) == 0 ) {
      output$email_confirmation_err <- renderText({"Email does not match above."})
      output$email_confirmation_icon <- renderUI({icon("times")})
      foundErrors <- 1
    } else {
      output$email_confirmation_err <- renderText({""})
      output$email_confirmation_icon <- renderUI({icon("check")})
    }

    if(nchar(input$role) == 0) {
      output$role_err <- renderText({"Role must not be blank."})
      output$role_icon <- renderUI({icon("times")})
      foundErrors <- 1
    } else {
      output$role_err <- renderText({""})
      output$role_icon <- renderUI({icon("check")})
    }

    if((input$prison) == "--Please Select Prison--") {
      output$prison_err <- renderText({"Please select a prison."})
      output$prison_icon <- renderUI({icon("times")})
      foundErrors <- 1
    } else {
      output$prison_err <- renderText({""})
      output$prison_icon <- renderUI({icon("check")})
    }

    if(all(is.null(input$bentham),
           is.null(input$safety),
           is.null(input$categorisation))) {
      output$apps_err <- renderText({"Please select at least one app."})
      output$apps_icon <- renderUI({icon("times")})
      foundErrors <- 1
    } else {
      output$apps_err <- renderText({""})
      output$apps_icon <- renderUI({icon("check")})
    }
    
    ## check if this quantum id is already in the list
    if(input$quantum_id %in% unlist(responses_subset()[, 4])) {
      output$apps_err <- renderText({"This Quantum account already has access
        or has requested access. If you think this is not the case please email
        anvil@noms.gsi.gov.uk and explain your case"})
      output$apps_icon <- renderUI({icon("times")})
      foundErrors <- 1
    } else {
      output$apps_err <- renderText({""})
      output$apps_icon <- renderUI({icon("check")})
    }

    #Check ID is 6 characters long
    if(nchar(input$quantum_id) != 6) {
      foundErrors <- 1
      quantumErr <- 1
    }

    #Check second letter of ID is "Q"
    if(substring(input$quantum_id, 2, 2) != "Q" && substring(input$quantum_id, 2, 2) != "q") {
      foundErrors <- 1
      quantumErr <- 1
    }

    #Check 1st character is character
    if(!is.na(as.numeric(substring(input$quantum_id, 1, 1)))) {
      foundErrors <- 1
      quantumErr <- 1
    }

    #Check 2nd character is character
    if(!is.na(as.numeric(substring(input$quantum_id, 2, 1)))) {
      foundErrors <- 1
      quantumErr <- 1
    }

    #Check 3rd character is character
    if(!is.na(as.numeric(substring(input$quantum_id, 3, 1)))){
      foundErrors <- 1
      quantumErr <- 1
    }

    #Check 6th character is character
    if(!is.na(as.numeric(substring(input$quantum_id, 6, 1)))){
      foundErrors <- 1
      quantumErr <- 1
    }

    #Check 4th and 5th characters are numbers
    if(is.na(as.numeric(substring(input$quantum_id, 4, 5)))){
      foundErrors <- 1
      quantumErr <- 1
    }

    #Print Quantum ID validation messages
    if(quantumErr == 1){
      output$quantum_error <- renderText({"Quantum ID must be of the format:
        AAA99A with a Q as the second character."})
      output$quantum_icon <- renderUI({icon("times")})
    } else {
      output$quantum_error <- renderText({""})
      output$quantum_icon <- renderUI({icon("check")})
    }

    if(foundErrors == 1){
      #Show error message
      shinyalert("There are errors in your submission.
                 Please correct and resubmit.",type = "error")
    } else {
      #Save data to responses datatable & show success message
      saveData(form_data())
      shinyalert("Thank you. Your responses have been submitted successfully.",
                 type = "success")
      reset("form")
      output$first_name_icon<-renderText({""})
      output$surname_icon<-renderText({""})
      output$email_icon<-renderText({""})
      output$email_confirmation_icon<-renderText({""})
      output$role_icon<-renderText({""})
      output$prison_icon<-renderText({""})
      output$apps_icon<-renderText({""})
      output$quantum_icon<-renderText({""})
      foundErrors <- 0
      quantumErr <- 0
    }
  })
})
