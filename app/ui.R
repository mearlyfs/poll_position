library(purrr)
library(here)
library(shiny)
library(shinyGovstyle)

fluidPage(
  shinyGovstyle::header(
    'News+Views',
    '',
    logo = 'crown'
  ),
  shinyGovstyle::banner(
    'banner',
    'alpha',
    'This is a new service – your <a class="govuk-link" href="https://homeoffice.com">feedback</a> will help us to improve it.'
  ),
  shinyGovstyle::gov_layout(
    size = 'full',
    tabsetPanel(
      tabPanel("Views",
               sidebarLayout(
                 sidebarPanel(
                   textInput("topic", "Enter a topic:"),
                   shinyGovstyle::button_Input(
                     inputId = "search_and_summarise",
                     label = "Search and summarise",
                     type = "start"
                   ),
                   uiOutput("top_questions_radio"),
                   width = 4
                 ),
                 mainPanel(
                   uiOutput("choose_breakdown_post_radio_selection"),
                   uiOutput("choose_response_post_radio_selection"),
                   uiOutput("graphs_post_radio_selection"),
                   uiOutput("hyperlink_post_selection"),
                   conditionalPanel(
                     condition = "output.show_export_button",
                     downloadButton("export_graph", "Export Graph")
                   )
                 )
               )
      ),
      tabPanel("News",
               fluidRow(
                 column(6,
                        h3("National News"),
                        tableOutput("national_news_table")
                 ),
               )
      )
    )
  ),
  shinyGovstyle::footer(TRUE)
)