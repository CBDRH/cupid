#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny bslib shinyWidgets tippy
#' @noRd
app_ui <- function(request) {
  tagList(

    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic
    page_navbar(
      title = "CommUnity Prevention Initiative Dashboard (CUPID)",
      theme = bslib::bs_theme(
        bootswatch = "cosmo"),

      # First navbar tab
      nav_panel(
        title = "Filters",

        page_sidebar(

          sidebar = sidebar(class = "my-sidebar", width=250,
                            # helpText("Select your search criteria"),

                            # value_box(
                            #   title = "Number of studies based on applied filters",
                            #   value = uiOutput("searchCount"),
                            #   showcase = icon("home"),
                            #   theme = "secondary",
                            #   # Search button
                            # ),


                            # custom value box
                            tags$div(
                              class = "custom-value-box",

                              # top row: icon + value
                              tags$div(
                                class = "box-top-row",
                                tags$div(class = "box-icon", icon("file-pdf")),
                                tags$div(class = "box-value", uiOutput("searchCount"))
                              ),

                              # explanatory text (full width)
                              tags$div(class = "box-text", uiOutput("searchMessage")),

                              # full-width button
                              tags$div(
                                class = "box-button",
                                actionBttn(
                                  inputId = 'bttnFilter',
                                  label = "Summarise Evidence",
                                  icon = icon("scale-balanced"),
                                  size = 'xs',
                                  block = TRUE
                                )
                              )
                            ),

                            accordion(
                              class = "my-accordion",
                              open = NULL, # Opens the first panel
                              multiple = FALSE,

                              accordion_panel(

                                id = 'intervention',

                                "Intervention",

                                checkboxGroupButtons(
                                  inputId = "drug",
                                  label = tags$span("Drug type",
                                                    tags$span(id = "info_drug", icon("info-circle", class = "info-icon"))
                                  ),
                                  choices = c("Alcohol",
                                              "Nicotine",
                                              "Cannabis",
                                              "Other drugs"),
                                  selected = c('Alcohol'),
                                  direction = "vertical",
                                  size = 'sm',
                                  width = '150px',
                                  checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                                ),
                                tippy_this("info_drug", placement = "right", theme = "light",
                                           tooltip = "Select one of more drug types.\nNicotine includes tobacco,
                         smokeless tobacco and vaping. Other drugs includes methamphetamine,
                         ecstacy, cocaine, inhalants, opioids, caffiene, pharmaceuticals,
                         nitrous oxide, and any unspecified substance."
                                )


                              ),

                              accordion_panel(
                                id = 'population',

                                "Participants",

                                # Gender
                                checkboxGroupButtons(
                                  inputId = "gender",
                                  direction = "vertical",
                                  size = 'sm',
                                  width = '150px',
                                  checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                                  label = "Gender",
                                  choices = list("No specific gender" = "No",
                                                 "Male" = "Male",
                                                 "Female" = "Female",
                                                 "Non-binary" = "Non-binary"),
                                  selected = c("No", "Male",  "Female", "Non-binary")
                                ),

                                # Life stage
                                checkboxGroupButtons(
                                  inputId = "life_stage",
                                  direction = "vertical",
                                  size = 'sm',
                                  width = '150px',
                                  checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                                  label = "Life stage",
                                  choiceNames = c("Younger years",  "Adolescents and youth", "Young adults", "Adults", "Older adults"),
                                  choiceValues = c("Younger years",  "Adolescents", "Young adults", "Adults", "Older adults"),
                                  selected = c("Younger years",  "Adolescents", "Young adults", "Adults", "Older adults")
                                ),

                                # Priority populations
                                checkboxGroupButtons(
                                  inputId = "priority_population",
                                  direction = "vertical",
                                  size = 'sm',
                                  width = '150px',
                                  checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                                  label = "Priority populations",
                                  choiceNames = c("No priority", "CALD",  "First Nations", "LGBTQI+", "People in custody", "Families and parents"),
                                  choiceValues = c("No", "CALD",  "First Nations", "LGBTQI+", "People in custody", "Families and parents"),
                                  selected = c("No", "CALD",  "First Nations", "LGBTQI+", "People in custody", "Families and parents")
                                )

                              ), # Close Accordion Panel

                              accordion_panel(
                                id = 'settings',

                                "Community context",

                                # Continent
                                checkboxGroupButtons(
                                  inputId = "continent",
                                  direction = "vertical",
                                  size = 'sm',
                                  width = '150px',
                                  checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                                  label = "Continent",
                                  choices = c("Africa",  "Asia", "Australia/Oceania", "Europe", "North America", "South America"),
                                  selected = c("Africa",  "Asia", "Australia/Oceania", "Europe", "North America", "South America")
                                ),

                                # Urbanicity
                                checkboxGroupButtons(
                                  inputId = "urbanicity",
                                  direction = "vertical",
                                  size = 'sm',
                                  width = '150px',
                                  checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                                  label = "Urbanicity",
                                  choices = c("Metropolitan",  "Regional", "Rural or remote"),
                                  selected = c("Metropolitan",  "Regional", "Rural or remote")
                                ),


                                # Setting
                                checkboxGroupButtons(
                                  inputId = "setting",
                                  direction = "vertical",
                                  size = 'sm',
                                  width = '150px',
                                  checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                                  label = "Setting",
                                  choices = c("Schools",  "Colleges or universities", "Small community", "Medium community", "Large community", "Other place-based", "Identity or heritage"),
                                  selected = c("Schools",  "Colleges or universities", "Small community", "Medium community", "Large community", "Other place-based", "Identity or heritage")
                                ),

                                # Organisations
                                checkboxGroupButtons(
                                  inputId = "organisations",
                                  direction = "vertical",
                                  size = 'sm',
                                  width = '150px',
                                  checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                                  label = "Organisations",
                                  choices = c("Culture or heritage-based organisations or groups", "Health services",  "Local government or higher government", "Other organisations", "Police", "Religious", "Researchers", "Schools or higher education", "Youth services"),
                                  selected = c("Culture or heritage-based organisations or groups", "Health services",  "Local government or higher government", "Other organisations", "Police", "Religious", "Researchers", "Schools or higher education", "Youth services")
                                ),


                              ), # Close Accordion Panel

                              accordion_panel(
                                id = 'activity',

                                "Activity",

                                # resources
                                checkboxGroupButtons(
                                  inputId = "resources",
                                  direction = "vertical",
                                  size = 'sm',
                                  width = '150px',
                                  checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                                  label = "Resources",
                                  choices = c("Low",  "Moderate", "High"),
                                  selected = c("Low",  "Moderate", "High")
                                ),

                                # duration
                                checkboxGroupButtons(
                                  inputId = "duration",
                                  direction = "vertical",
                                  size = 'sm',
                                  width = '150px',
                                  checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                                  label = "Duration",
                                  choices = c("Short term",  "Medium term", "Long term"),
                                  selected = c("Short term",  "Medium term", "Long term")
                                )

                              ), # Close Accordion Panel

                              accordion_panel(
                                id = 'evaluation',

                                "Evaluation",

                                # sample
                                checkboxGroupButtons(
                                  inputId = "sample",
                                  direction = "vertical",
                                  size = 'sm',
                                  width = '150px',
                                  checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                                  label = "Sample",
                                  choices = c("Small (\u2264 500)", "Medium (500-5000)", "Large (\u2265 5000)"),
                                  selected = c("Small (\u2264 500)", "Medium (500-5000)", "Large (\u2265 5000)")
                                ),

                                # quality
                                checkboxGroupButtons(
                                  inputId = "quality",
                                  direction = "vertical",
                                  size = 'sm',
                                  width = '150px',
                                  checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                                  label = "Quality",
                                  choices = c("No qualtity assessment",  "Weak", "Moderate", "Strong"),
                                  selected = c("No qualtity assessment",  "Weak", "Moderate", "Strong")
                                )

                              ) # Close Accordion Panel


                            ) # Close Accordion


          ),

          page_fillable(
            div(id = "banner",


                virtualSelectInput(
                  inputId = "initiative",
                  label = tags$span("Prevention Activity",
                                    tags$span(id = "info_initiative", icon("info-circle", class = "info-icon"))
                  ),

                  selected = NULL,
                  choices = list(
                    "<i class='fa-solid fa-people-group'></i> Mobilisation" = c(
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Alternative activities",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Dry events",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Building community support"
                    ),

                    "<i class='fa-solid fa-handshake'></i> Community coalitions" = c(
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Establishing a coalition and building its capacity",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Community education",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Coordinating coalition activities"
                    ),

                    "<i class='fa-solid fa-legal'></i> Community-level policy" = c(
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Banning production",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Limiting sales",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Licensed premises outlet density",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Licensed premises outlet operating hours",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Legal purchasing age",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Product strength",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Advertising / marketing restrictions",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Price regulation",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Location bans",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Health warning labels"
                    ),

                    "<i class='fa-solid fa-school'></i> School-based" = c(
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> School Curriculum change (community-wide)",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> School Curriculum change (individual school[s])",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> School non-curriculum sessions or activities",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> School environment policy (including signage, newsletter)",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Education and training for teachers"
                    ),

                    "<i class='fa-solid fa-people-roof'></i> Parenting" = c(
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Parent arm to school-based initiative",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Parenting"
                    ),

                    "<i class='fa-solid fa-building-shield'></i> Police / local authorities" = c(
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Punishment",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Presence",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Information dissemination",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Roadside checkpoints",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Compliance checking"
                    ),

                    "<i class='fa-solid fa-cash-register'></i> Licensed premises / vendor / retail outlet" = c(
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Responsible service training",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Policy change (individual vendor, voluntary)",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Environmental safety (individual vendor[s])",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Restrictions for high risk patrons"
                    ),

                    "<i class='fa-solid fa-radio'></i> Media campaign" = c(
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Community-wide (radio, television, etc.)",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Targeted (Social media, event specific, etc.)"
                    ),

                    "<i class='fa-solid fa-person-digging'></i> Workplace" = c(
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Workplace policies",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Workplace support and education"
                    ),

                    "<i class='fa-solid fa-user-doctor'></i> Health service" = c(
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Remotely delivered services (helplines, websites, etc.)",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Staff upskilling (without specific service)",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> New health services",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Offering additional services (brief intervention, etc.)",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Mobile health services"
                    ),

                    "<i class='fa-solid fa-building-columns'></i> Environmental / Infrastructure" = c(
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Risk assessment and environment improvement",
                      "<i class='fa-solid fa-lightbulb myLightbulb'></i> Transport options"
                    )
                  ),
                  showValueAsTags = TRUE,
                  search = TRUE,
                  multiple = FALSE,
                  width = '95%',
                  html = TRUE
                ),
                tippy_this("info_initiative", placement = "right", theme = "light",
                           tooltip = "Select the initiative of interest"
                )
            ),

            card(
              shiny::textOutput("test")
            )

          ) # Closes page_fillable

        ) # Closes page_sidebar
      ) # closes nav panel


    ) # closes page_navbar
  ) # end taglist
} # end function

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "cupid"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
