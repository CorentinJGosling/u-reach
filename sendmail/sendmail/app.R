library(shiny)
library(shinyforms)

# Define the first form: basic information
basicInfoForm <- list(
  id = "basicinfo",
  questions = list(
    list(id = "title", type = "text", title = "U-REACH title", mandatory = TRUE,
         hint = "Indicate the name of your U-REACH project. The word 'U-REACH' should be included."),
    list(id = "date", type = "text", title = "Anticipated completion date", mandatory = TRUE),
    list(id = "name", type = "text", title = "Named contact", mandatory = TRUE)
  ),
  storage = list(
    type = STORAGE_TYPES$FLATFILE,
    path = "responses"
  ),
  name = "",
  password = "shinyforms",
  reset = TRUE,
  validations = list(
    list(condition = "nchar(input$name) >= 3",
         message = "Name must be at least 3 characters")
  )
)

ui <- fluidPage(
  h1("U-REACH checklist"),
  formUI(basicInfoForm)
)

server <- function(input, output, session) {
  formServer(basicInfoForm)
}

shinyApp(ui = ui, server = server)
