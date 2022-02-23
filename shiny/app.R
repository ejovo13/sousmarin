# app.R
library(shiny)
library(sousmarin)


ui <- fluidPage(
    # bs_theme(
    #     bootswatch = "cosmo"
    # ),
    numericInput(inputId = "n",
        "Sample size", value = 3), # Let's use this n value to generate a Covariance matrix
    # plotOutput(outputId = "hist"),
    textOutput(outputId = "matrix_text"),
    uiOutput(outputId = "matrix")
)
server <- function(input, output, session) {
    output$hist <- renderPlot({
        hist(rnorm(input$n))
    })

    output$matrix_text <- renderText({
        "Covariance matrix: "
    })

    output$matrix <- renderTable({
        mat <- rposdef(input$n)
        mat = mat / mat[1,1]
    })


}
shinyApp(ui = ui, server = server)