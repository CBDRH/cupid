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
      title = "CUPID",
      theme = bslib::bs_theme(
        bootswatch = "cosmo"),

      # First navbar tab
      nav_panel(
        title = "Prevention Menu",

        page_sidebar(

          sidebar = sidebar(class = "my-sidebar",
            helpText("Select your search criteria"),

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
                              "Nicotene",
                              "Cannabis",
                              "Other drugs"),
                  selected = c('Alcohol'),
                  direction = "vertical",
                  size = 'sm',
                  width = '150px',
                  checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                ),
                tippy_this("info_drug", placement = "right", theme = "light",
                           tooltip = "Select one of more drug types.\nNicotene includes tobacco,
                         smokeless tobacco and vaping. Other drugs includes methamphetamine,
                         ecstacy, cocaine, inhalants, opioids, caffiene, pharmaceuticals,
                         nitrous oxide, and any unspecified substance."
                ),

                virtualSelectInput(
                  inputId = "initiative",
                  label = tags$span("Initiative type",
                                    tags$span(id = "info_initiative", icon("info-circle", class = "info-icon"))
                  ),

                  selected = c("Alternative activities",
                               "Dry events",
                               "Building community support"),
                  choices = list(
                    "Community mobilisation" = c("Alternative activities",
                                                 "Dry events",
                                                 "Building community support"),
                    "Community coalitions" = c("Establishing a coalition and building its capacity",
                                               "Community education",
                                               "Coordinating coalition activities"),
                    "Community-level policy" = c("Banning production",
                                                 "Limiting sales",
                                                 "Licensed premises / vendor / retail outlet density",
                                                 "Licensed premises / vendor / retail outlet operating hours / days",
                                                 "Legal purchasing age",
                                                 "Product strength",
                                                 "Advertising / marketing restrictions",
                                                 "Price regulation",
                                                 "Location bans",
                                                 "Health warning labels"),
                    "School-based" = c("School Curriculum change (community-wide)",
                                       "School Curriculum change (individual school[s])",
                                       "School non-curriculum sessions or activities",
                                       "School environment policy (including signage, newsletter)",
                                       "Education and training for teachers"),
                    "Parenting" = c("Parent arm to school-based initiative",
                                    "Parenting"),
                    "Police / local authorities" = c("Punishment",
                                                     "Presence",
                                                     "Information dissemination",
                                                     "Roadside checkpoints",
                                                     "Compliance checking"),
                    "Licensed premisis / vendor / retail outlet" = c("Responsible service training",
                                                                     "Policy change (individual vendor, voluntary)",
                                                                     "Environmental safety (individual vendor[s])",
                                                                     "Restrictions for high risk patrons"),
                    "Media campaign" = c("Community-wide (radio, television, etc.)",
                                         "Targeted (Social media, event specific, etc.)"),
                    "Workplace" = c("Workplace policies",
                                    "Workplace support and education"),
                    "Health service" = c("Remotely delivered services (helplines, websites, etc.)",
                                         "Staff upskilling (without specific service)",
                                         "New health services",
                                         "Offering additional services (brief intervention, etc.)",
                                         "Mobile health services"),
                    "Environmental / Infrastructure" = c("Risk assessment and environment improvement",
                                                         "Transport options")
                  ),
                  showValueAsTags = TRUE,
                  search = TRUE,
                  multiple = TRUE,
                  showDropboxAsPopup = TRUE
                ),
                tippy_this("info_initiative", placement = "right", theme = "light",
                            tooltip = "Select one or more initiatives"
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
                  choices = c("Male",  "Female", "Non-binary"),
                  selected = c("Male",  "Female", "Non-binary")
                ),

                # Life stage
                checkboxGroupButtons(
                  inputId = "life_stage",
                  direction = "vertical",
                  size = 'sm',
                  width = '150px',
                  checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                  label = "Life stage",
                  choices = c("Younger years",  "Adolescent and youth", "Young adults", "Adults", "Older adults"),
                  selected = c("Younger years",  "Adolescent and youth", "Young adults", "Adults", "Older adults")
                ),

                # Priority populations
                checkboxGroupButtons(
                  inputId = "priority_population",
                  direction = "vertical",
                  size = 'sm',
                  width = '150px',
                  checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                  label = "Priority populations",
                  choices = c("CALD",  "First Nations", "LGBTQI+", "People in custody", "Families and parents"),
                  selected = c("CALD",  "First Nations", "LGBTQI+", "People in custody", "Families and parents")
                )

              ), # Close Accordion Panel

              accordion_panel(
                id = 'settings',

                "Context",

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
                  choices = c("Small (≤500)", "Medium (500-5000)", "Large (≥5000)"),
                  selected = c("Small (≤500)", "Medium (500-5000)", "Large (≥5000)")
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

          plotOutput("randplot1")

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
