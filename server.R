## server.R

## Define server logic
shinyServer(function(input, output, session) {

  form_data <- reactive({
    data <- data.table(
      first_name = input$first_name, 
      surname = input$surname, 
      prison = input$prison, 
      role = input$role, 
      quantum_id = input$quantum_id, 
      bentham = input$bentham, 
      bentham_reason = ifelse(input$bentham == 1, input$bentham_reason, NA), 
      drugs_prison = input$drugs, 
      drugs_prison_reason = ifelse(input$drugs == 1, input$drugs_reason, NA),  
      safety = NA, 
      categorisation = NA, 
      date_requested = as.Date(Sys.Date()), 
      email = input$email, 
      account = "Requested"
    )
  })
  
  responses_subset <- reactive({
    responses <- loadData()
    responses[responses$prison == input$prison & (responses$bentham == 1 | responses$drugs_prison == 1),
              c("first_name", "surname", "role", "quantum_id", 
                "bentham", "bentham_reason", "drugs_prison", "drugs_prison_reason",
                "safety", "categorisation", "date_requested", 
                "account", "email")]
  })
  
  
  output$prison_access <- renderDataTable({
    req(nrow(responses_subset()) > 0)
    access_table <- responses_subset()
    access_table$bentham <- ifelse(access_table$bentham == 1, tick, cross)
    access_table$drugs_prison <- ifelse(access_table$drugs_prison == 1, tick, cross)
    
    ## Render table but remove columns so don't display email address
    ## or reasons as makes table too wide

    datatable(access_table[, c(1:5, 7, 12)], escape = FALSE,
              options = list(paging = FALSE,
                             scrollCollapse = T,
                             dom = "ft", 
                             scrollX = FALSE,
                             scrollY = "500px"),
              rownames = FALSE,
              colnames = c("First Name", "Surname", "Role", "Quantum ID",
                           "Bentham", "Drugs in Prisons",
                           "Account Status")
              )
  })

  output$prison_access_null <- renderText({
    req(!nrow(responses_subset()) > 0)
    "No users at your prison have access."
  })

  #Show Bentham reason box if require access to Bentham
  
  output$bentham_check <- renderUI({
    if (input$bentham == TRUE){
      fluidRow(column(width = 10,
                      div(class = "class_bentham_reason",
                          textAreaInput("bentham_reason",
                                        label = h5("If you are requesting access to the Bentham app,
                                              you must outline why access is required:"),
                                        width = "100%",
                                        rows = 6)
                      )))
    }})
  
  #Show drugs reason box if require access to drugs app
  
  output$drugs_check <- renderUI({
    if (input$drugs == TRUE){
      fluidRow(column(width = 10,
                      div(class = "class_bentham_reason",
                          textAreaInput("drugs_reason",
                                        label = h5("If you are requesting access to the Drugs in Prisons app,
                                              you must outline why access is required:"),
                                        width = "100%",
                                        rows = 6)
                      )))
    }})
  
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
           is.null(input$drugs))) {
      output$apps_err <- renderText({"Please select at least one app."})
      output$apps_icon <- renderUI({icon("times")})
      foundErrors <- 1
    } else {
      output$apps_err <- renderText({""})
      output$apps_icon <- renderUI({icon("check")})
    }
    
    ## check if this quantum id is already in the list
    if(input$quantum_id %in% unlist(responses_subset()[, 4])) {
      
      ## if any requests are made where access doesn't already exist
      if (any((all(unlist(responses_subset()[quantum_id == input$quantum_id, 5]) == 0) &
               input$bentham == T) |
              (all(unlist(responses_subset()[quantum_id == input$quantum_id, 9]) == 0) &
               input$drugs == T))) {
        
        output$apps_err <- renderText({""})
        output$apps_icon <- renderUI({icon("check")})
        
      ##only requests made are for apps where access exists
      } else {
        
        output$apps_err <- renderText({"This Quantum account already has access
       or has requested access. If you think this is not the case please email
       anvil@noms.gsi.gov.uk and explain your case."})
        output$apps_icon <- renderUI({icon("times")})
        foundErrors <- 1
      }
      
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
    
    # Check Bentham reason if Bentham ticked
    if(input$bentham == TRUE &&
       input$bentham_reason == "") {
      output$bentham_reason_err <- renderText({"You must provide a reason why access to the Bentham app is required."})
      foundErrors <- 1
    } else {
      output$bentham_reason_err <- renderText({""})
    }  
    
    
    # Check Drugs reason if Drugs ticked
    if(input$drugs == TRUE &&
       input$drugs_reason == "") {
      output$drugs_reason_err <- renderText({"You must provide a reason why access to the Drugs in Prisons app is required."})
      foundErrors <- 1
    } else {
      output$drug_reason_err <- renderText({""})
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
      output$bentham_reason_icon<-renderText({""})
      foundErrors <- 0
      quantumErr <- 0
    }
  })
})
