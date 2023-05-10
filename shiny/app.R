library(shiny)
library(bslib)
library(plotly)
library(tidyverse)
library(data.table)

load("shiny/params.Rda")
load_url <- function (url, ..., sha1 = NULL) {
  # based very closely on code for devtools::source_url
  stopifnot(is.character(url), length(url) == 1)
  temp_file <- tempfile()
  on.exit(unlink(temp_file))
  request <- httr::GET(url)
  httr::stop_for_status(request)
  writeBin(httr::content(request, type = "raw"), temp_file)
  file_sha1 <- digest::digest(file = temp_file, algo = "sha1")
  if (is.null(sha1)) {
    message("SHA-1 hash of file is ", file_sha1)
  }
  else {
    if (nchar(sha1) < 6) {
      stop("Supplied SHA-1 hash is too short (must be at least 6 characters)")
    }
    file_sha1 <- substr(file_sha1, 1, nchar(sha1))
    if (!identical(file_sha1, sha1)) {
      stop("SHA-1 hash of downloaded file (", file_sha1, 
           ")\n  does not match expected value (", sha1, 
           ")", call. = FALSE)
    }
  }
  load(temp_file, envir = .GlobalEnv)
}
file_url <- getURL("http://github.com/rycktessman/tpt-cost-effectiveness/raw/main/shiny/params.Rda")
load(file_url)
file_url <- ("https://github.com/rycktessman/tpt-cost-effectiveness/raw/main/shiny/params.rda")
load(url(file_url))
load("shiny/country_params.Rda")
load("shiny/pub_targets.Rda")
source("shiny/model_functions.R")

regimens <- c("None", "3HP", "1HP", "6H")
countries <- c("Namibia", "Zambia")

#Pt 1: User Interface (UI) - framework (structure for app's appearance)
ui <- navbarPage(
  title="TB Preventive Treatment Budget Impact Tool",
  theme=bs_theme(version=4, bootswatch="minty"),
  tabPanel(
    title="Main",
    sidebarLayout(
      sidebarPanel(
        width=3, 
        h1("Mandatory Inputs"),
        shiny::selectInput(
          inputId="country",
          label="Select a country",
          choices=countries
          ),
        shiny::selectInput(
          inputId="tpt_plhiv",
          label="Select regimen for PLHIV",
          choices=regimens
        ),
        shiny::numericInput(
          inputId="covg_plhiv",
          label="Select TPT coverage level for PLHIV (0-100%)",
          value=0
        ),
        shiny::selectInput(
          inputId="tpt_child",
          label="Select regimen for household contacts < 5 years",
          choices=regimens
        ),
        shiny::numericInput(
          inputId="covg_child",
          label="Select TPT coverage level for contacts < 5 (0-100%)",
          value=0
        ),
        shiny::selectInput(
          inputId="tpt_adol",
          label="Select regimen for household contacts 5-14 years",
          choices=regimens
        ),
        shiny::numericInput(
          inputId="covg_adol",
          label="Select TPT coverage level for contacts 5-14 (0-100%)",
          value=0
        ),
        shiny::selectInput(
          inputId="tpt_adult",
          label="Select regimen for household contacts 15+ years",
          choices=regimens
        ),
        shiny::numericInput(
          inputId="covg_adult",
          label="Select TPT coverage level for contacts 15+ (0-100%)",
          value=0
        )
      ),
      mainPanel(
        h1("Results"),
        fluidRow(column(5, plotlyOutput("testplot1")),
                 column(5, plotlyOutput("testplot2")))
      )
    )
  ),
  tabPanel(
    title="Optional Inputs",
    mainPanel(
      h1("Specify optional parameters")
    )
  ),
  tabPanel(
    title="Download Results",
    mainPanel(
      h1("Download full set of results")
    )
  )
)
  
#Pt 2: Server - runs R code, graphs
server <- function(input, output) {
  rv <- reactiveValues()
  observe({
    country_code <- country_info %>% filter(country==input$country) %>% pull(code)
    params <- c(plhiv_params, other_params, unlist(cost_params[[country_code]]), 
                "p_ltbi"=ltbi_params %>% filter(iso3==country_code) %>% pull(ltbi_prev), 
                "p_notif_ltfu"=p_notif_adult[[country_code]],
                "p_success"=p_success_plhiv[[country_code]],
                "yrs_new"=2, "yrs"=10)
    rv$output <- run_model_plhiv(input$country, input$tpt_plhiv, input$covg_plhiv, NULL, params)
  })
  output$testplot1 <- renderPlotly({
    plot_ly(rv$output,
            x=~year,
            y=~cum_costs,
            type='bar',
            group=~scenario)
  })
  output$testplot2 <- renderPlotly({
    plot_ly(rv$output,
            x=~year,
            y=~cases,
            type='bar',
            group=~scenario)
  })
}

shinyApp(ui=ui, server=server)

