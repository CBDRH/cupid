#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny dplyr
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic


  # test space
  output$test <- renderText({
   filtered_data()$initiative
  })


  # Filtering the data
  filtered_data <- reactive({

    req(study_level_data)
    df <- study_level_data

    df |>
      filter(drug %in% input$drug) |>
      filter(gender %in% input$gender) |>
      filter(lifestages %in% input$life_stage) |>
      filter(priority %in% input$priority_population)


  })


  # Count search results
  # count <- sample(1:50, 1)

  n_studies <- reactiveVal(NULL)
  color <- reactiveVal(NULL)
  observe({
    n_studies(nrow(filtered_data()))

    color(if (n_studies() < 10) "#9EBDFF"
    else "white")
  })


  output$searchCount <- renderUI({

    tags$span(style = paste0("color:", color(), "; font-weight: bold;"), paste0(n_studies()))


  })


  # Placeholder - what happens when the button is clicked
  observeEvent(input$bttnFilter, {

    x <- nrow(filtered_data())

    showModal(
      modalDialog(
        title = "Warning",
        paste(x),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })

}
