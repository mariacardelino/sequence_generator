library(shiny)
library(httr)
library(jsonlite)
library(dplyr)

# username =  CLUES_API 
# password = 1518Clifton

# Dev server URL - CHANGE when necessary
base_url <- "https://openspecimen-dev.emory.edu/openspecimen/rest/ng"

## UI LOGIC ########################################
ui <- fluidPage(
  titlePanel("OpenSpecimen API Demo (Dev)"),
  sidebarLayout(
    sidebarPanel(
      textInput("username", "Username", value = ""),
      passwordInput("password", "Password", value = ""),
      actionButton("login", "Login"),
      verbatimTextOutput("login_status"),
      hr(),
      numericInput("visitId", "Visit ID", value = 1),
      textInput("specLabel", "Specimen label", value = "lab1"),
      actionButton("do_head", "Check Specimen Exists (HEAD)"),
      actionButton("do_get", "Get Specimen (GET)"),
      actionButton("do_post", "Create Specimen (POST)"),
      actionButton("do_put", "Update Specimen Status (PUT)"),
      verbatimTextOutput("api_response")
    ),
    mainPanel(
      h4("API Response:"),
      verbatimTextOutput("response_pretty")
    )
  )
)

## SERVER LOGIC ########################################
server <- function(input, output, session) {
  token <- reactiveVal(NULL)
  responseData <- reactiveVal(NULL)
  
  ## LOGIN #####
  observeEvent(input$login, {
    res <- POST(
      url = paste0(base_url, "/sessions"),
      content_type_json(),
      body = list(
        loginName = input$username,
        password = input$password,
        domainName = "openspecimen"
      ), encode = "json"
    )
    if (status_code(res) == 200) {
      body <- content(res, "parsed")
      token(body$token)
      output$login_status <- renderText("Login successful")
    } else {
      output$login_status <- renderText(paste("Login failed:", status_code(res)))
    }
  }) ## END Login observe
  
  
  
  
  
  ## JUNK
  ## "DO_api_call" function???
  do_api_call <- function(req) {
    req <- req %>% add_headers(`X-OS-API-TOKEN` = token())
    res <- req %>% req_perform()
    responseData(list(status=status_code(res), body=content(res, "parsed", simplifyVector = FALSE)))
  }
  
  
  ## If CHECK specimen exists (HEAD)
  observeEvent(input$do_head, {
    req <- request(paste0(base_url, "/specimens")) %>% req_method("HEAD") %>% req_url_query(label = input$specLabel)
    do_api_call(req)
  })
  
  # If GET specimen (GET)
  observeEvent(input$do_get, {
    # example: get specimen ID 1 (or from responseData if stored)
    specId <- 1
    req <- request(paste0(base_url, "/specimens/", specId)) %>% req_method("GET")
    do_api_call(req)
  })
  
  # If CREATE specimen (POST)
  observeEvent(input$do_post, {
    body <- list(
      lineage="New",
      visitId=input$visitId,
      status="Collected",
      label=input$specLabel,
      specimenClass="Fluid",
      type="Plasma",
      availableQty="5",
      collectionEvent=list(
        user=list(loginName = input$username),
        time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
        container="Not Specified", procedure="Not Specified"
      ),
      receivedEvent=list(
        user=list(loginName = input$username),
        time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
        receivedQuality="Acceptable"
      )
    )
    req <- request(paste0(base_url, "/specimens")) %>% req_method("POST") %>% req_body_json(body)
    do_api_call(req)
  })
  
  # If UPDATE specimen status (PUT)
  observeEvent(input$do_put, {
    specId <- 1
    body <- list(status="Collected", Reason="Updated via Shiny")
    req <- request(paste0(base_url, "/specimens/", specId, "/status")) %>% req_method("PUT") %>% req_body_json(body)
    do_api_call(req)
  })
  
  output$api_response <- renderPrint({
    rd <- responseData()
    if (is.null(rd)) return(NULL)
    cat("HTTP status:", rd$status, "\n")
    print(rd$body)
  })
}

shinyApp(ui, server)
