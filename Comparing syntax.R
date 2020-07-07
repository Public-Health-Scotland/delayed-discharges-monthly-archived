library(diffr)
library(shiny)

file1 = "01_Validation.R"
file2 = "01a_Validation_Highland.R"

ui <- fluidPage(
  h1("Comparing Highland Validation script"),
  checkboxInput("wordWrap", "Word Wrap",
                value = TRUE),
  diffrOutput("exdiff")
)

server <- function(input, output, session) {
  output$exdiff <- renderDiffr({
    diffr(file1, file2, wordWrap = input$wordWrap,
          before = "Validation", after = "Highland")
  })
}

shinyApp(ui, server)
