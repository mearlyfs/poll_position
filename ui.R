library(shiny)

fluidPage(
  titlePanel(".gov Polling Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      textInput("topic", "Enter Topic:", value = ""),
      actionButton("submit", "Search")
    ),
    mainPanel(
      h3("Top 5 Relevant Questions"),
      tableOutput("top_questions")
    )
  )
)