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
            div(class = "class_mainTitle", "Anvil access request form")
          ),
          div(class = "class_selections",
            fluidRow(
              div(class = "class_intro",
                  p("To gain access to the HMPPS Anvil apps,
                  please enter your details in the fields below and click submit."),
                  p("If you have any issues submitting this form,
                    please contact anvil@noms.gsi.gov.uk"))
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
            
            ## Section: prison
            fluidRow(
              column(width = 10,
                div("Prison:", class = "class_prison",
                  pickerInput(inputId = "prison",
                              label = NULL,
                              width = "100%",
                              selected = "",
                              choices = c("--Please Select Prison--", dt.prisons))
                )
              ),
              column(width = 1,
                div(uiOutput("prison_icon"))
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
                  awesomeCheckbox(inputId = "safety",
                                  label = "Safety Diagnostic Tool",
                                  value = FALSE,
                                  status = "danger"),
                  div("Bentham and Drugs in Prisons require permission from
                      the National Intelligence Unit."),
                  # awesomeCheckbox(inputId = "categorisation",
                  #                 label = "Prisoner Categorisation",
                  #                 value = FALSE,
                  #                 status = "danger"),
                  awesomeCheckbox(inputId = "bentham",
                                  label = "Bentham",
                                  value = FALSE,
                                  status = "danger"),
                  awesomeCheckbox(inputId = "drugs",
                                  label = "Drugs in Prisons",
                                  value = FALSE,
                                  status = "danger")
                )
              ),
              
              column(width = 1,
                uiOutput("apps_icon")
              )
            ),
            div(class = "class_apps_error",
              textOutput("apps_err")
            ),
            
            # Section: Bentham reason
            
            uiOutput("bentham_check"),
            
              column(width = 1),
            div(class = "class_bentham_reason_error",
                textOutput("bentham_reason_err"),
                br()
            ),
            
            # Section: Drugs reason
            
            uiOutput("drugs_check"),
            
            column(width = 1),
            div(class = "class_bentham_reason_error",
                textOutput("drugs_reason_err"),
                br()
            ),
            
            div(class = "class_submitButton",
              actionButton("submitButton", "Submit")
            )
          )
        ),
  
        column(width = 8,
          div(class = "class_access_msg",
                      "The following users at your establishment have already requested 
                      access to the ticked apps. The final column states whether their account
                      has been created."),
  
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