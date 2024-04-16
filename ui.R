library(purrr)
library(here)
library(shiny)
library(shinyGovstyle)

fluidPage(
  shinyGovstyle::header(
    'EH',
    'Poll Position', 
    logo = 'crown'
  ),
  shinyGovstyle::banner(
    'banner',
    'alpha',
    'This is a new service – your <a class="govuk-link" href="https://homeoffice.com">feedback</a> will help us to improve it.'
  ),
  shinyGovstyle::gov_layout(
    size = 'full',
    sidebarLayout(
      sidebarPanel(
        textInput("topic", "Enter a topic:"),
        shinyGovstyle::button_Input(
          inputId = "search_and_summarise", 
          label = "Search and summarise", 
          type = "start"
        )
      ),
      mainPanel(
        tableOutput('top_questions')
      )
    )
  ),
  shinyGovstyle::footer(TRUE)
)