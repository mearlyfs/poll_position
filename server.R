library(shiny)
library(openai) # Assuming you have installed and set up the open ai API library
library(stringdist)
text <-  data.frame(
  question_id = c(1, 2, 3, 4),
  question = c(
    "What is the capital city of France?",
    "How many planets are in our solar system?",
    "What is the largest mammal on Earth?",
    "Who painted the famous artwork 'The Starry Night'?"
  ),
  stringsAsFactors = FALSE
)
# Load your data
polling_data <- read.csv("data/polling_data.csv")

# Function to retrieve text embeddings using OpenAI API
get_embeddings <- function(text) {
  # Replace 'openai_api_key' with your actual OpenAI API key
  openai::create_embedding(model='text-embedding-3-small', input = text$question, openai_api_key = openai_key)
}

function(input, output, text) {
  
  # Reactive function to filter and find top questions
  top_questions <- eventReactive(input$submit, {
    # Get user-provided topic and its embedding
    user_topic <- input$topic
    topic_embedding <- get_embeddings(user_topic)
    
    # Calculate similarity between topic embedding and each question
    text$similarities <- sapply(text$topic_embedding, function(x) stringdist(x,topic_embedding,method="cosine"))
    res <- text %>% arrange(desc(similarities)) %>% head(n)
    return(res)
    # Filter polling_data based on similarity scores
    top_indices <- order(similarities, decreasing = TRUE)[1:5] # Get top 5
    top_questions_data <- polling_data[top_indices, ]
    
    # You can add more logic here to calculate average response, 
    # regional differences, etc., and include them in the returned data frame
    
    return(top_questions_data)
  })
  
  output$top_questions <- renderTable({
    top_questions()
  })
}