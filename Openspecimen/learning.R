library(httr)

# Define the URL and token
url <- "https://your-openspecimen-url/api/samples"
token <- "your_api_token"

# Create the data to be sent in the request
data <- list(
  status = "Analyzed",
  analyzedDate = "2023-10-10",
  technician = "John Doe"
)

# Make the API request
response <- PUT(url, add_headers(`X-OS-API-TOKEN` = token), body = data, encode = "json")

# Check the response
content(response)