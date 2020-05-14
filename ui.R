## ui.R

shinyUI(
  shiny::fluidPage(
    
    useShinyjs(),
    useShinyalert(),
    includeCSS("www/custom.css"),
    div(style = "background-color:WhiteSmoke;",
        
        ## Main page
        fluidRow(
          column(width = 4, #class = "class_selections",
                 id = "form",
                 fluidRow(#class = "class_top_banner",
                   div(class = "class_mainTitle", "MoJ Prison Intelligence tools access request form")
                 ),
                 div(class = "class_selections",
                     fluidRow(
                       div(class = "class_intro",
                           HTML("<p><div style='color:red'>To gain access to the MoJ prison intelligence tools,
                              please enter your details in the fields below and click submit.</div></p>
                                <p>If you have any issues submitting this form,
                              please contact intel_app_support@digital.justice.gov.uk</p>"))
                     ),
                     
                     ## Section First Name
                     fluidRow(
                       column(width = 10,
                              div(class = "class_first_name",
                                  textInput("first_name",
                                            label = h3("First Name:"),
                                            width = "100%")
                              )
                       ),
                       column(width = 1,
                              div(shiny::uiOutput("first_name_icon"))
                       )
                     ),
                     div(class = "class_first_name_err",
                         textOutput("first_name_err")
                     ),
                     
                     ## Section: Surname
                     fluidRow(
                       column(width = 10,
                              div(class = "class_surname",
                                  textInput("surname",
                                            label = h3("Surname:"),
                                            width = "100%")
                              )
                       ),
                       column(width = 1,
                              div(uiOutput("surname_icon"))
                       )
                     ),
                     div(class = "class_surname_err",
                         textOutput ("surname_err")
                     ),
                     
                     ## Section: Email address
                     fluidRow(
                       column(width = 10,
                              div(class = "class_email",
                                  textInput("email",
                                            label = h3("Email:"),
                                            width = "100%")
                              )
                       ),
                       column(width = 1,
                              div(uiOutput("email_icon"))
                       )
                     ),
                     div(class = "class_email_err",
                         textOutput("email_err")
                     ),
                     fluidRow( 
                       column(width = 10,
                              div(class = "class_email_confirmation",
                                  textInput("email_confirmation",
                                            label = h3("Please confirm email:"),
                                            width = "100%")
                              )
                       ),
                       column(width = 1,
                              div(uiOutput("email_confirmation_icon"))
                       )
                     ),
                     div(class = "class_email_confirmation_err",
                         textOutput("email_confirmation_err")
                     ),
                     
                     ## Section: area and prison
                     fluidRow(
                       column(width = 10,
                              div("Area:", class = "class_prison",
                                  pickerInput(inputId = "area",
                                              label = NULL,
                                              width = "100%",
                                              selected = "",
                                              choices = c("--Please Select Area--", unique(dt.prisons[active == T, soct_region])))
                              )
                       ),
                       column(width = 1,
                              div(uiOutput("prison_icon"))
                       )
                     ),
                     
                     fluidRow(
                       column(width = 10,
                              conditionalPanel(
                                condition = "condition=input.area!='--Please Select Area--'&&input.area!='HQ'",
                                
                                div("Prison (if relevant):", class = "class_prison",
                                    uiOutput("prison_picker")
                                )
                              )
                       )
                     ),
                     
                     div(class = "class_prison_err",
                         textOutput("prison_err")
                     ),
                     
                     ## Section: role
                     fluidRow(
                       column(width = 10,
                              div(class = "class_role",
                                  textInput("role",
                                            label = h3("Role:"),
                                            width = "100%")
                              )
                       ),
                       column(width = 1,
                              div(uiOutput("role_icon"))
                       )
                     ),
                     div(class = "class_role_err",
                         textOutput("role_err")
                     ),
                     
                     ## Section: quantum id
                     fluidRow(
                       column(width = 10,
                              div(class = "class_quantum_id",
                                  textInput("quantum_id",
                                            label = h3("Quantum ID:"),
                                            width = "100%")
                              )
                       ),
                       column(width = 1,
                              uiOutput("quantum_icon"))
                     ),
                     div(class = "class_quantum_error",
                         textOutput("quantum_error")
                     ),
                     
                     ## Section: checkbox
                     fluidRow(
                       column(width = 10,
                              div(class = "class_app_checkbox",
                                  "Choose which apps you require access to",
                                  div("These apps require permission from the National Intelligence Unit."),
                                  awesomeCheckbox(inputId = "bentham",
                                                  label = HTML("Seized Media Database<br>(previously Bentham)"),
                                                  value = FALSE,
                                                  status = "danger"),
                                  awesomeCheckbox(inputId = "drugs",
                                                  label = "Drug Conveyancing App",
                                                  value = FALSE,
                                                  status = "danger"),
                                  awesomeCheckbox(inputId = "network",
                                                  label = "Prison Network App",
                                                  value = FALSE,
                                                  status = "danger"),
                                  awesomeCheckbox(inputId = "visitors",
                                                  label = "Prison Visitors App",
                                                  value = FALSE,
                                                  status = "danger"),
                                  awesomeCheckbox(inputId = "novel_drugs",
                                                  label = "Novel Drug Words App",
                                                  value = FALSE,
                                                  status = "danger")
                              )
                       ),
                       
                       column(width = 1,
                              uiOutput("apps_icon")
                       )
                     ),
                     div(class = "class_apps_error",
                         uiOutput("apps_err")
                     ),
                     
                     # Section: Bentham reason
                     
                     uiOutput("bentham_check"),
                     
                     column(width = 1),
                     div(class = "class_bentham_reason_error",
                         textOutput("bentham_reason_err")
                     ),
                     
                     # Section: Drugs reason
                     
                     uiOutput("drugs_check"),
                     
                     column(width = 1),
                     div(class = "class_bentham_reason_error",
                         textOutput("drugs_reason_err")
                     ),
                     
                     # Section: Network reason
                     
                     uiOutput("network_check"),
                     
                     column(width = 1),
                     div(class = "class_bentham_reason_error",
                         textOutput("network_reason_err")
                     ),
                     
                     # Section: Visitors reason
                     
                     uiOutput("visitors_check"),
                     
                     column(width = 1),
                     div(class = "class_bentham_reason_error",
                         textOutput("visitors_reason_err")
                     ),
                     
                     # Section: Drugs reason
                     
                     uiOutput("novel_drugs_check"),
                     
                     column(width = 1),
                     div(class = "class_bentham_reason_error",
                         textOutput("novel_drugs_reason_err")
                     ),
                     
                     div(class = "class_submitButton",
                         actionButton("submitButton", "Submit")
                     )
                 )
          ),
          
          column(width = 8,
                 div(class = "class_access_msg",
                      HTML("The following users at your establishment have already requested 
                      access to apps. Status is indicated by:<br>
                      <br>Tick - account has been created
                      <br>Cross - account has been denied
                      <br>Clock - account requested but pending creation
                      <br>'Approved' - account approved but not created (please contact us)
                      <br>'Deleted' - account deleted (please contact us if not expected)
                      <br>'N/A' - no account application received for")),
                 
                 div(class = "class_prison_null",
                     textOutput("prison_access_null")
                 ),
                 
                 div(class = "class_prison_access",
                     dataTableOutput("prison_access")
                 )
          )
        )
    )
  )
)
