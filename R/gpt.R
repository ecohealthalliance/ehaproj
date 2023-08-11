library(openai)

create_chat_completion(
  model = "gpt-3.5-turbo-16k-0613",
  messages = list(
    list(role = "system", content = "You are a deterministic but highly sophisticated natural-language processing system that returns all answers as well-formatted JSON"),
    list(role = "user", content = "Format may 5 2012 and 2013 as two iso-8601 dates"))
)
