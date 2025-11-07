#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic


  # Count search results
  # count <- sample(1:50, 1)
  count <- 50
  output$searchCount <- renderUI({

    color <- if (count < 15) "grey80"
    else "white"

    tags$span(style = paste0("color:", color, "; font-weight: bold;"), paste0(count))

  })


  output$searchMessage<- renderUI({

    if (count < 15) tags$span("Number of studies based on your search criteria", tags$br(),
                              "(Try broadening your search terms)")
    else "Number of studies based on your search criteria"


  })



  # Placeholder - what happens when the button is clicked
  observeEvent(input$bttnFilter, {
    showModal(
      modalDialog(
        title = "Warning",
        "Don't forget to finish developing the app, Mark",
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })

}
