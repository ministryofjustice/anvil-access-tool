## server.R

## Define server logic
shinyServer(function(input, output, session) {
  
  output$prison_picker <- renderUI({
    pickerInput(inputId = "prison",
                label = NULL,
                width = "100%",
                selected = "",
                choices = c("--Please Select Prison (if relevant)--", c("Whole region", unique(dt.prisons[active == T & soct_region == input$area, prison]))))
  })
  
  form_data <- reactive({
    data <- data.table(
      first_name = input$first_name,
      surname = input$surname,
      area = input$area,
      prison = ifelse(is.null(input$prison), NA, ifelse(input$prison == "--Please Select Prison (if relevant)--", NA, input$prison)),
      role = input$role,
      quantum_id = input$quantum_id,
      bentham = input$bentham,
      bentham_reason = ifelse(input$bentham == 1, input$bentham_reason, NA),
      drugs_prison = input$drugs,
      drugs_prison_reason = ifelse(input$drugs == 1, input$drugs_reason, NA),
      network = input$network,
      network_reason = ifelse(input$network == 1, input$network_reason, NA),
      visitors = input$visitors,
      visitors_reason = ifelse(input$visitors == 1, input$visitors_reason, NA),
      novel_drugs = input$novel_drugs,
      novel_drugs_reason = ifelse(input$novel_drugs == 1, input$novel_drugs_reason, NA),
      date_requested = as.Date(Sys.Date()),
      email = input$email,
      account = "Requested"
    )
  })

  responses_all <- reactive({
    reactive_trigger <- input$area
    responses <- loadData()
    responses <- responses[(bentham == 1 | drugs_prison == 1 | network == 1 | visitors == 1 | novel_drugs == 1),
                           c("first_name", "surname", "role", "quantum_id", 
                             "bentham", "bentham_reason", "drugs_prison", "drugs_prison_reason",
                             "network", "network_reason", "visitors", "visitors_reason",
                             "novel_drugs", "novel_drugs_reason",
                             "date_requested", "account", "email", "area")]
    return(responses)
  })
    
  responses_subset <- reactive({
    responses <- responses_all()[area == input$area & (bentham == 1 | drugs_prison == 1 | network == 1 | visitors == 1 | novel_drugs == 1)]
    return(responses)
  })
  
  output$prison_access <- renderDataTable({
    req(nrow(responses_subset()) > 0)
    access_table <- responses_subset()
    access_table$bentham <- ifelse(access_table$bentham == 1, tick, cross)
    access_table$drugs_prison <- ifelse(access_table$drugs_prison == 1, tick, cross)
    access_table$network <- ifelse(access_table$network == 1, tick, cross)
    access_table$visitors <- ifelse(access_table$visitors == 1, tick, cross)
    access_table$novel_drugs <- ifelse(access_table$novel_drugs == 1, tick, cross)
    
    ## Render table but remove columns so don't display email address
    ## or reasons as makes table too wide
    datatable(access_table[, c(1:5, 7, 9, 11, 13, 16)], escape = FALSE,
              options = list(paging = FALSE,
                             scrollCollapse = T,
                             dom = "ft", 
                             scrollX = FALSE,
                             scrollY = "500px"),
              rownames = FALSE,
              colnames = c("First Name", "Surname", "Role", "Quantum ID",
                           "Seized Media", "Drug Conveyancing", "Network",
                           "Visitors", "Novel Drugs",
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
                                        label = h5("If you are requesting access to the Seized Media Database,
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
                                        label = h5("If you are requesting access to the Drugs Conveyancing app,
                                              you must outline why access is required:"),
                                        width = "100%",
                                        rows = 6)
                      )))
    }})
  
  #Show Network reason box if require access to app
  output$network_check <- renderUI({
    if (input$network == TRUE){
      fluidRow(column(width = 10,
                      div(class = "class_bentham_reason",
                          textAreaInput("network_reason",
                                        label = h5("If you are requesting access to the Prison Network app,
                                              you must outline why access is required:"),
                                        width = "100%",
                                        rows = 6)
                      )))
    }})
  
  #Show visitors reason box if require access to app
  output$visitors_check <- renderUI({
    if (input$visitors == TRUE){
      fluidRow(column(width = 10,
                      div(class = "class_bentham_reason",
                          textAreaInput("visitors_reason",
                                        label = h5("If you are requesting access to the Prison Visitor app,
                                              you must outline why access is required:"),
                                        width = "100%",
                                        rows = 6)
                      )))
    }})
  
  #Show novel drugs reason box if require access to app
  output$novel_drugs_check <- renderUI({
    if (input$novel_drugs == TRUE){
      fluidRow(column(width = 10,
                      div(class = "class_bentham_reason",
                          textAreaInput("novel_drugs_reason",
                                        label = h5("If you are requesting access to the Novel Drug Words app,
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
    
    if((input$area) == "--Please Select Area--") {
      output$prison_err <- renderText({"Please select an area"})
      output$prison_icon <- renderUI({icon("times")})
      foundErrors <- 1
    } else {
      output$prison_err <- renderText({""})
      output$prison_icon <- renderUI({icon("check")})
    }
    
    if(all(isFALSE(input$bentham),
           isFALSE(input$drugs),
           isFALSE(input$network),
           isFALSE(input$visitors),
           isFALSE(input$novel_drugs))) {
      
      output$apps_err <- renderText({"Please select at least one app."})
      output$apps_icon <- renderUI({icon("times")})
      foundErrors <- 1
      
      ## if apps selected, check if this quantum id is already in the list
    } else if (input$quantum_id %in% unlist(responses_all()[, 4])) {
      
      ##only allow requests made for new apps i.e. where access doesn't already exists
      bentham_already <- any(unlist(responses_all()[quantum_id == input$quantum_id, 5]) == 1) & input$bentham == T
      drugs_already <- any(unlist(responses_all()[quantum_id == input$quantum_id, 7]) == 1) & input$drugs == T
      network_already <- any(unlist(responses_all()[quantum_id == input$quantum_id, 9]) == 1) & input$network == T
      visitors_already <- any(unlist(responses_all()[quantum_id == input$quantum_id, 11]) == 1) & input$visitors == T
      novel_drugs_already <- any(unlist(responses_all()[quantum_id == input$quantum_id, 13]) == 1) & input$novel_drugs == T
      
      # print(any(bentham_already, drugs_already, network_already, visitors_already, novel_drugs_already))
      
      if (any(bentham_already, drugs_already, network_already, visitors_already, novel_drugs_already)) {
        
        alreadys <- c()
        if (bentham_already) { alreadys <- c(alreadys, "<br>Seized Media Database (previously Bentham)") }
        if (drugs_already) { alreadys <- c(alreadys, "<br>Drug Conveyancing App") }
        if (network_already) { alreadys <- c(alreadys, "<br>Prison Network App") }
        if (visitors_already) { alreadys <- c(alreadys, "<br>Prison Visitors App") }
        if (novel_drugs_already) { alreadys <- c(alreadys, "<br>Novel Drug Words App") }
        
        output$apps_err <- renderUI({
          HTML(paste0("This Quantum account already has access, or has requested access to, the following apps:<br>",
                 paste(alreadys, collapse = ", "),
                 "<br><br>Please remove those already requested, or if you think this is not the case please email intel_app_support.digital.justice.gov.uk and explain your case."))
        })
        output$apps_icon <- renderUI({icon("times")})
        foundErrors <- 1
        
        ## apps selected and quantum id has a new set of requests
      } else {
        
        output$apps_err <- renderText({""})
        output$apps_icon <- renderUI({icon("check")})
      }
      
      ## apps selected and quantum id not on list... so requests where access doesn't already exist
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
      output$bentham_reason_err <- renderText({"You must provide a reason why access to the Seized Media Database is required."})
      output$apps_icon <- renderUI({icon("times")})
      foundErrors <- 1
    } else {
      output$bentham_reason_err <- renderText({""})
    }
    
    
    # Check Drugs reason if Drugs ticked
    if(input$drugs == TRUE &&
       input$drugs_reason == "") {
      output$drugs_reason_err <- renderText({"You must provide a reason why access to the Drug Conveyancing App is required."})
      output$apps_icon <- renderUI({icon("times")})
      foundErrors <- 1
    } else {
      output$drugs_reason_err <- renderText({""})
    }
    
    # Check Network reason if network ticked
    if(input$network == TRUE &&
       input$network_reason == "") {
      output$network_reason_err <- renderText({"You must provide a reason why access to the Prison Network App is required."})
      output$apps_icon <- renderUI({icon("times")})
      foundErrors <- 1
    } else {
      output$network_reason_err <- renderText({""})
    } 
    
    # Check Visitors reason if Visitors ticked
    if(input$visitors == TRUE &&
       input$visitors_reason == "") {
      output$visitors_reason_err <- renderText({"You must provide a reason why access to the Prison Visitors App is required."})
      output$apps_icon <- renderUI({icon("times")})
      foundErrors <- 1
    } else {
      output$visitors_reason_err <- renderText({""})
    } 
    
    # Check Novel Drugs reason if Novel Drugs ticked
    if(input$novel_drugs == TRUE &&
       input$novel_drugs_reason == "") {
      output$novel_drugs_reason_err <- renderText({"You must provide a reason why access to the Novel Drug Words App is required."})
      output$apps_icon <- renderUI({icon("times")})
      foundErrors <- 1
    } else {
      output$novel_drugs_reason_err <- renderText({""})
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
