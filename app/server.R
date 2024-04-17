library(shiny)
library(openai) # Assuming you have installed and set up the openai API library
library(stringdist)
library(proxyC)
library(ggplot2)
library(stringr)
library(directlabels)
library(RColorBrewer)
library(lubridate)
library(tidyverse)
library(remotes)
library(httr)
library(jsonlite)
library(here)

# Load your data
data <- bind_rows(read.csv(here::here("data/Savanta questions and data 1.csv")),
                  read.csv(here::here("data/Savanta questions and data 2.csv")),
                  read.csv(here::here("data/Savanta questions and data 3.csv")))

questions <- read_csv(here::here("data/Savanta Questions.csv"))
polling_data <- full_join(data, questions)

# there is an error in this version of the input data and an NA row gets through, as well as some other rows that weren't read in properly
# fix these if these are not fixed in a future dataset

polling_data <- polling_data %>% filter(!is.na(question))
questions <- questions %>% filter(!is.na(question))

polling_data <- polling_data %>% mutate(date=ymd(date))
polling_data <- polling_data %>% mutate(breakdown=case_when(breakdown=="Social grade"~"Socioeconomic",
                                                            breakdown=="Regions"~"Region",
                                                            T~breakdown))

polling_data <- polling_data %>% filter(!response%in%c("Error variance","Standard Deviation","Base for Stats"))



# Function to retrieve text embeddings using OpenAI API
get_embeddings <- function(text_vec) {
  # Replace 'openai_api_key' with your actual OpenAI API key
  embedding <- openai::create_embedding(model='text-embedding-3-small', input = text_vec, openai_api_key = openai_key)
  res <- do.call(rbind, embedding$data$embedding) %>%
    as.matrix()
  rownames(res) <- text_vec
  return(res)
}

# Function to generate summary using OpenAI LLM
generate_summary <- function(text) {
  # Replace 'openai_api_key' with your actual OpenAI API key
  response <- openai::create_chat_completion(
    messages = text,
    model = "gpt-4",
    max_tokens = 100,
    n = 1,
    stop = NULL,
    temperature = 0.7,
    openai_api_key = openai_key
  )
}

function(input, output, session) {
  # Reactive function to filter and find top questions
  top_questions <- eventReactive(input$search_and_summarise, {
    # Get user-provided topic and its embedding
    user_topic <- input$topic
    topic_embedding <- get_embeddings(user_topic)
    rownames(topic_embedding) <- 'sim'
    text_embedding <- get_embeddings(questions$question)
    
    # Calculate similarity between topic embedding and each question
    sim_raw <- proxyC::simil(
      text_embedding,
      topic_embedding,
      method = 'cosine'
    )
    res <- as.matrix(sim_raw) %>%
      as_tibble() %>%
      mutate(questions = rownames(text_embedding))
    res <- res %>%
      arrange(desc(sim))
    
    top_questions_data <- res$questions[1:5]
    
    return(top_questions_data) # Get top 5
  })
  
  output$top_questions_radio <- renderUI({
    shinyGovstyle::radio_button_Input("radio_choice","Most relevant polling questions:",choices=top_questions(),small = T)
    #top_questions()
  })
  
  
  question_radio_choice <- eventReactive(input$radio_choice, {
    return(input$radio_choice) 
  })
  
  output$choose_breakdown_post_radio_selection <- renderUI({
    
    ### This section makes a box to choose which breakdown to plot
    
    if(length(question_radio_choice())>0){
      shinyGovstyle::select_Input("breakdown_selection","Breakdown",select_text = c("Total","Age","Gender","Region","Socioeconomic"),select_value=c("Total","Age","Gender","Region","Socioeconomic"))
    }
    
  })
  
  breakdown_choice <- eventReactive(input$breakdown_selection, {
    return(input$breakdown_selection) 
  })
  
  
  output$choose_response_post_radio_selection <- renderUI({
    
    ### This section makes a box to choose which response to plot graphs for 
    question_selection <- question_radio_choice()
    question_data <- polling_data %>% filter(question==question_selection,
                                             !response%in%c("Total","Unweighted Total","Unweighted row"),
                                             !substr(category,1,3)%in%c("Net","NET"))
    
    question_responses <- question_data$response %>% unique %>% sort
    
    if("NET: Rank 1-3"%in%question_responses){
      default_response <- "NET: Rank 1-3"
    }else{
      default_response <- question_responses[1]
    }
    
    shinyGovstyle::select_Input("response_selection","Poll response:",select_text = question_responses,select_value = question_responses)
    
    
  })
  
  response_choice <- eventReactive(input$response_selection, {
    return(input$response_selection) 
  })
  
  output$graphs_post_radio_selection <- renderUI({
    
    ### This section draws the graphs 
    
    graph_response <- response_choice()
    graph_breakdowns <- breakdown_choice()
    
    question_selection <- question_radio_choice()
    question_data <- polling_data %>% filter(question==question_selection)
    
    # Clean up the question data slightly
    question_data <- question_data %>% filter(!response%in%c("Total","Unweighted Total","Unweighted row"),
                                              !substr(category,1,3)%in%c("Net","NET","Sum","SUM"))
    
    # Select the only the data necessary to make the graph
    graph_data <- question_data %>% filter(breakdown%in%c(graph_breakdowns,"Total"),response==graph_response)
    
    # In the first case, the graph is not broken down by any characteristic:
    if(graph_breakdowns=="Total"){
      plot <- graph_data %>% 
        ggplot(aes(x=date,y=percent)) + geom_line() + geom_point() + scale_y_continuous(labels = scales::percent) + 
        theme_minimal() + labs(title=paste0("Savanta","\n",str_wrap(question_selection,width=70)),y="",x="") + scale_x_date(date_labels="%b %Y")
    }
    
    # If asked to break down by age, gender or socioeconomic
    if(graph_breakdowns%in%c("Age","Gender","Socioeconomic")){
      
      # Pre-load colours. Black for Total first. I don't like the yellow that is sixth place in "Set1", but otherwise it's an easy palette to pull from quickly, unless anyone has the GSS colour scheme to hand.
      graph_data_cols <- c("#000000",brewer.pal(9,"Set1")[c(1:5,7:9)],brewer.pal(8,"Set2"))
      names(graph_data_cols) <- unique(c("Total",unique(graph_data$category)))  # manipulate so that "Total" appears first in the vector
      x_axis_padding <- if(min(graph_data$date)==max(graph_data$date)){0}else(5)
      
      plot <- graph_data %>% 
        ggplot(aes(x=date,y=percent,col=category)) + geom_line() + geom_point() + scale_y_continuous(labels = scales::percent) + geom_dl(aes(label=category),method="last.qp") +
        theme_minimal() + labs(title=paste0("Savanta","\n",str_wrap(question_selection,width=70)),y="",x="") + scale_x_date(date_labels="%b %Y",limits=c(min(graph_data$date),max(graph_data$date)+x_axis_padding)) + 
        scale_colour_manual(values = graph_data_cols,guide="none") + facet_grid(category~.)
    }
    
    # If not by the other categories if by region
    if(graph_breakdowns=="Region"){
      
      graph_data <- graph_data %>% mutate(category=factor(category,levels=c("North-East","North-West","Yorkshire & Humberside","East Midlands","West Midlands","Eastern","London","South-East","South-West","Wales","Scotland","Northern Ireland","Total")))
      
      graph_data_cols <- c("#000000",brewer.pal(9,"Set1")[c(1:5,7:9)],brewer.pal(8,"Set2"))
      names(graph_data_cols) <- unique(c("Total",levels(graph_data$category)))  # manipulate to that "Total" appears first in the vector
      
      x_axis_padding <- if(min(graph_data$date)==max(graph_data$date)){0}else(15)
      
      plot <- graph_data %>% 
        ggplot(aes(x=date,y=percent,col=category)) + geom_line() + geom_point() + scale_y_continuous(labels = scales::percent) + geom_dl(aes(label=category),method="last.qp") +
        theme_minimal() + labs(title=paste0("Savanta","\n",str_wrap(question_selection,width=70)),y="",x="") + scale_x_date(date_labels="%b %Y",limits=c(min(graph_data$date),max(graph_data$date)+x_axis_padding)) + 
        scale_colour_manual(values = graph_data_cols,guide="none") + facet_wrap(~category,ncol=2)
    }
    
    # Finally, plot the graph
    renderPlot({  
      plot
    })
    
  })
  
  output$hyperlink_post_selection <- renderUI({
    if (length(question_radio_choice()) > 0) {
      url <- "https://example.com"  # Replace with the actual URL of the source website
      link_text <- "Source: Polling Company"  # Replace with the desired link text
      a(href = url, target = "_blank", link_text)
    }
  })
  
  # Reactive function to fetch and display news based on user input
  observeEvent(input$search_and_summarise, {
    # Get user-provided topic and language
    user_topic <- input$topic
    language <- "en"  # Assuming default language is English
    
    # Construct the API request URL with dynamic topic and language
    url <- 'https://v3-api.newscatcherapi.com/api/search?'
    params <- list(q = user_topic, lang = language)
    
    # Send GET request to News Catcher API
    response <- GET(url, query = params, add_headers(`x-api-token` = newscatcher_key))
    
    # Parse JSON response
    data <- content(response, "text") %>% fromJSON(flatten = TRUE)
    
    # Filter and process news data
    nationalnews <- c("BBC", "The Telegraph", "Times", "Daily Mirror", "Sky", "ITV")
    data <- data$articles
    if (!is.null(data$articles) && nrow(data$articles) > 0) {
      # Process and filter news data
      National_news <- data$articles %>%
        filter(country == "GB") %>%
        arrange(desc(score)) %>%
        filter(name_source %in% nationalnews) %>%
        select(title, name_source, link)
      
      # Update the output table with the filtered news data
      output$national_news_table <- renderTable({
        National_news
      })
    } else {
      # Handle empty or invalid response (e.g., display a message or fallback behavior)
      output$national_news_table <- renderText({
        "No news articles found."
      })
    }
  })
  
  # UI code for input elements (topic search and action button)
  output$topic_search_input <- renderUI({
    textInput("topic_search", "Enter Search Topic")
  })
  
  output$search_button <- renderUI({
    actionButton("search_and_summarise", "Search and Summarise")
  })
  
  # Display news search UI elements
  output$search_controls <- renderUI({
    tagList(
      uiOutput("topic_search_input"),
      uiOutput("search_button")
    )
  })
  
}

  #If we had time:
# library(gptstudio)
# generate_summary <- function(prompt = prompt) {
#   
#   response <- create_completion_anthropic(
#     prompt = prompt,
#     history = NULL,
#     model = "claude-2",
#     key = anthropic_key
#   )
#   
#   return(response$choices$text)
# }
# userQuestion <- paste0("Please provide a summary of the following survey data that has the columns: date, response, breakdown, category, number, percent, key, question, question_short.\n\n", paste(polling_data_grou, collapse = ","))
# 
# prompt <- paste0("\n\nHuman: ", userQuestion, "\n\nAssistant:")
# 
# q <- polling_data$question[1]
# polling_data_grou <- polling_data %>% filter(question == q)



