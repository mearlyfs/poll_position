library(gptstudio)
generate_summary <- function(prompt = prompt) {

  response <- create_completion_anthropic(
    prompt = prompt,
    history = NULL,
    model = "claude-2",
    key = anthropic_key
  )
  
  return(response$choices$text)
}
userQuestion <- paste0("Please provide a summary of the following survey data that has the columns: date, response, breakdown, category, number, percent, key, question, question_short.\n\n", paste(polling_data_grou, collapse = ","))

prompt <- paste0("\n\nHuman: ", userQuestion, "\n\nAssistant:")

q <- polling_data$question[1]
polling_data_grou <- polling_data %>% filter(question == q)
