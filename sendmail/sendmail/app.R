library(shiny)
library(shinyforms)
library(DT)
library(shinyWidgets)
library(shinyjs)

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
  tags$head(tags$style(HTML('
  .show{display:flex;}
  .hide{display: none;}
  '))),
  shinyjs::useShinyjs(),
  div(id="top",
      h1("U-REACH checklist"),
      materialSwitch(
        inputId = "btnLogin",
        label = "Admin login",
        status = "primary",
        right = TRUE),
      ),
  div(id="admin",
      textInput("user", "Username"),
      textInput("pwd", "Password"),
      actionButton("log", "Login")
      ),
  formUI(basicInfoForm)
)

server <- function(input, output, session) {
  observe({
    enable_login <- isTRUE(input$btnLogin)
    toggleClass(selector = "#admin", class="hide",
                condition = !enable_login)

  })

  formServer(basicInfoForm)
}

shinyApp(ui = ui, server = server)
