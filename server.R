library(shiny)
library(openai) # Assuming you have installed and set up the openai API library
library(stringdist)
library(proxyC)
library(tidyverse)

# Load your data
polling_data <- read.csv("data/first_questions.csv")

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
  response <- openai::create_completion(
    model = "text-davinci-002",
    prompt = paste("Please summarize the following text:\n\n", text),
    max_tokens = 100,
    n = 1,
    stop = NULL,
    temperature = 0.7,
    openai_api_key = openai_key
  )

server <- function(input, output, session) {
  # Reactive function to filter and find top questions
  top_questions <- eventReactive(input$search_and_summarise, {
    # Get user-provided topic and its embedding
    user_topic <- input$topic
    topic_embedding <- get_embeddings(user_topic)
    rownames(topic_embedding) <- 'sim'
    text_embedding <- get_embeddings(polling_data$question)
    
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
  
  output$top_questions <- renderTable({
    top_questions()
  })
}




