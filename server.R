## server.R

## Define server logic
shinyServer(function(input, output, session) {
  
  output$prison_picker <- renderUI({
    pickerInput(inputId = "prison",
                label = NULL,
                width = "100%",
                selected = "",
                choices = c(PRISON_SELECT_TEXT,
                            c(WHOLE_REGION_TEXT,
                              unique(dt.prisons[active == T & soct_region == input$area,
                                                prison]))))
  })
  
  ##set up status values...
  ##...note that this relies on users not being allowed to submit requests if they already have access (below)
  bentham_status <- reactive({
    bentham_status <- responses_all()[id == tolower(input$quantum_id), bentham_status]
    ifelse(input$bentham, "requested",
           ifelse(length(bentham_status)==0, NA, bentham_status))
  })
  network_status <- reactive({
    network_status <- responses_all()[id == tolower(input$quantum_id), network_status]
    ifelse(input$network, "requested",
           ifelse(length(network_status)==0, NA, network_status))
  })
  novel_drugs_status <- reactive({
    novel_drugs_status <- responses_all()[id == tolower(input$quantum_id), novel_drugs_status]
    ifelse(input$novel_drugs, "requested",
           ifelse(length(novel_drugs_status)==0, NA, novel_drugs_status))
  })
  visitors_status <- reactive({
    visitors_status <- responses_all()[id == tolower(input$quantum_id), visitors_status]
    ifelse(input$visitors, "requested",
           ifelse(length(visitors_status)==0, NA, visitors_status))
  })
  drug_convey_status <- reactive({
    drug_convey_status <- responses_all()[id == tolower(input$quantum_id), drug_convey_status]
    ifelse(input$drugs, "requested",
           ifelse(length(drug_convey_status)==0, NA, drug_convey_status))
  })
  
  ##reactively produce form response data
  form_data <- reactive({
    data <- data.table(
      first_name = input$first_name,
      surname = input$surname,
      area = input$area,
      prison = ifelse(is.null(input$prison), NA,
                      ifelse(input$prison == PRISON_SELECT_TEXT, NA, input$prison)),
      role = input$role,
      id = tolower(input$quantum_id),
      email = tolower(input$email),
      date_requested = format(Sys.time(), tz="Europe/London", "%Y-%m-%d %H:%M:%S"),
      bentham = as.integer(input$bentham),
      bentham_status = bentham_status(),
      drug_convey = as.integer(input$drugs),
      drug_convey_status = drug_convey_status(),
      network = as.integer(input$network),
      network_status = network_status(),
      visitors = as.integer(input$visitors),
      visitors_status = visitors_status(),
      novel_drugs = as.integer(input$novel_drugs),
      novel_drugs_status = novel_drugs_status(),
      reason = ifelse((input$bentham |
                         input$drugs |
                         input$network |
                         input$visitors |
                         input$novel_drugs) &
                        !is.null(input$overall_reason),
                      input$overall_reason, NA)
    )
  })

  responses_all <- reactive({
    reactive_trigger <- input$area
    return(loadData()[(bentham == 1 | drug_convey == 1 | network == 1 | visitors == 1 | novel_drugs == 1)])
  })
    
  responses_subset <- reactive({
    
    if (isTruthy(input$prison)) {
      
      if (input$prison != PRISON_SELECT_TEXT &
          input$prison != WHOLE_REGION_TEXT) {
        
        responses <- responses_all()[area == input$area & prison == input$prison]
      
      } else {
        
        responses <- responses_all()[area == input$area]
      }
    } else {
      
      responses <- responses_all()[area == input$area]
    }
    
    ##implement symbols
    responses <- merge(responses, status_lookup[, .(status, bentham_status_disp=status_disp)],
                       by.x = "bentham_status", by.y = "status", all.x = T)
    responses <- merge(responses, status_lookup[, .(status, network_status_disp=status_disp)],
                       by.x = "network_status", by.y = "status", all.x = T)
    responses <- merge(responses, status_lookup[, .(status, drug_convey_status_disp=status_disp)],
                       by.x = "drug_convey_status", by.y = "status", all.x = T)
    responses <- merge(responses, status_lookup[, .(status, visitors_status_disp=status_disp)],
                       by.x = "visitors_status", by.y = "status", all.x = T)
    responses <- merge(responses, status_lookup[, .(status, novel_drugs_status_disp=status_disp)],
                       by.x = "novel_drugs_status", by.y = "status", all.x = T)
    
    ##only keep those to be displayed
    responses[, name := paste0(first_name, " ", surname)]
    return(responses[, .(name, role, id,
                  bentham_status_disp,
                  drug_convey_status_disp,
                  network_status_disp,
                  visitors_status_disp,
                  novel_drugs_status_disp)])
  })
  
  output$prison_access <- renderDataTable({
    req(nrow(responses_subset()) > 0)
    # access_table <- responses_subset()
    # access_table$bentham <- ifelse(access_table$bentham == 1, tick, cross)
    # access_table$drugs_prison <- ifelse(access_table$drug_convey == 1, tick, cross)
    # access_table$network <- ifelse(access_table$network == 1, tick, cross)
    # access_table$visitors <- ifelse(access_table$visitors == 1, tick, cross)
    # access_table$novel_drugs <- ifelse(access_table$novel_drugs == 1, tick, cross)
    
    ## Render table but remove columns so don't display email address
    ## or reasons as makes table too wide
    datatable(responses_subset(),
              escape = FALSE,
              options = list(paging = FALSE,
                             scrollCollapse = T,
                             dom = "ft", 
                             scrollX = FALSE,
                             scrollY = "500px"),
              rownames = FALSE,
              colnames = c("Name", "Role", "Quantum ID",
                           "Seized Media", "Drug Conveyancing", "Network",
                           "Visitors", "Novel Drugs")
    )
  })
  
  output$prison_access_null <- renderText({
    req(!nrow(responses_subset()) > 0)
    "No users at your prison have access."
  })
  
  #Show reason box if require access to any of the apps
  output$bentham_check <- renderUI({
    if (input$bentham == TRUE |
        input$drugs == TRUE |
        input$network == TRUE |
        input$visitors == TRUE |
        input$novel_drugs == TRUE){
      fluidRow(column(width = 10,
                      div(class = "class_bentham_reason",
                          textAreaInput("overall_reason",
                                        label = h5("If you are requesting access to an app,
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
    
    if((tolower(input$email) != tolower(input$email_confirmation)) || nchar(input$email_confirmation) == 0 ) {
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
    } else if (tolower(input$quantum_id) %in% unlist(responses_all()[, id])) {
      
      ##only allow requests made for new apps i.e. where access doesn't already exists
      bentham_already <- any(unlist(responses_all()[id == tolower(input$quantum_id), bentham]) == 1) & input$bentham == T
      drugs_already <- any(unlist(responses_all()[id == tolower(input$quantum_id), drug_convey]) == 1) & input$drugs == T
      network_already <- any(unlist(responses_all()[id == tolower(input$quantum_id), network]) == 1) & input$network == T
      visitors_already <- any(unlist(responses_all()[id == tolower(input$quantum_id), visitors]) == 1) & input$visitors == T
      novel_drugs_already <- any(unlist(responses_all()[id == tolower(input$quantum_id), novel_drugs]) == 1) & input$novel_drugs == T
      
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
                 "<br><br>Please remove those already requested, or if you think this is not the case please email intel_app_support@digital.justice.gov.uk and explain your case."))
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

    #Check that the format of the quantum id is correct
    if(!grepl("^[a-z]{1}q[a-z]{1}[0-9]{2}[a-z]{1}$",tolower(input$quantum_id))){
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
    if ((input$bentham == TRUE | input$drugs == TRUE | 
       input$network == TRUE | input$visitors == TRUE |
       input$novel_drugs == TRUE) & 
       !is.null(input$overall_reason)) {
      
      if (input$overall_reason == "") {
        output$bentham_reason_err <- renderText({"You must provide a reason why access to the apps is required."})
        output$apps_icon <- renderUI({icon("times")})
        foundErrors <- 1
      } else {
        output$bentham_reason_err <- renderText({""})
      }
    
    } else {
      output$bentham_reason_err <- renderText({""})
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
