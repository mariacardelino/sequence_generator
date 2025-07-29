library(httr)
library(jsonlite)
library(dplyr)

base_url <- "https://openspecimen-dev.emory.edu/openspecimen/rest/ng"

username <- 'CLUES_API'
password <- '1518Clifton'

## LOGIN #############################################
res <- POST(
  url = paste0(base_url, "/sessions"),
  content_type_json(),
  body = list(
    loginName = username,
    password = password,
    domainName = "openspecimen"
  ), encode = "json"
)

if (status_code(res) == 200) {
  body <- content(res, "parsed")
  token <- body$Token # Successful login, but token == "NULL"
  #token <- body$resetPasswordToken   # # Successful login and gives a token
  cat("Login successful\n")
} else {
  stop(paste("Login failed:", status_code(res)))
} 


### TRYING USING RESET PASSWORD TOKEN
## CHECK SPECIMEN EXISTS - HEAD #######################
specimen_label <- "blood1" #change to label 

res <- HEAD(
  url = paste0(base_url, "/specimens"),
  query = list(label = specimen_label),
  add_headers(
    `X-OS-API-TOKEN` = token,
    `Content-Type` = "application/json"
  )
)

if (status_code(res) == 200) {
  cat(sprintf("Specimen '%s' exists.\n", specimen_label))
  specimen_exists <- TRUE
} else if (status_code(res) == 404) {
  cat(sprintf("Specimen '%s' not found (HTTP 404).\n", specimen_label))
  specimen_exists <- FALSE
} else {
  cat(sprintf("Unexpected response (HTTP %s).\n", status_code(res)))
  specimen_exists <- FALSE
}


# Manually inspect response content 
raw_body <- content(res, "text", encoding = "UTF‑8")
raw_body

### KEEP GETTING resetPasswordToken in the response. 
# Password needs to be reset?



### token?
do_api_call <- function(req) {
  req <- req %>% add_headers(`X-OS-API-TOKEN` = token())
  res <- req %>% req_perform()
  responseData(list(status=status_code(res), body=content(res, "parsed", simplifyVector = FALSE)))
}

## If CHECK specimen exists (HEAD)
req <- request(paste0(base_url, "/specimens")) %>% req_method("HEAD") %>% req_url_query(label = input$specLabel)
do_api_call(req)

##1226
endpoint <- "https://your-instance/openspecimen/rest/ng/specimens"
label    <- "blood1"

res <- HEAD(url = paste0(base_url, "/openspecimen/rest/ng/specimens"),
            query = list(label = specimen_label),
            add_headers(
              `X-OS-API-TOKEN` = token,
              `Content-Type` = "application/json"
            ))

if (status_code(res) == 200) {
  # For HEAD it may not return a body, but assume 200 → exists
  exists <- TRUE
  message(sprintf("Specimen with label '%s' exists.", label))
} else {
  exists <- FALSE
  message(sprintf(
    "Specimen with label '%s' not found. Status: %s",
    label, status_code(res)
  ))
}

exists