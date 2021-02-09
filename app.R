#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(rvest)
library(lubridate)
library(cranlogs)
library(glue)
library(scales)
library(colorspace)
library(tidytext)
library(pluralize)
library(kableExtra)
library(igraph)
library(ggraph)
library(ctv)

db <- "http://cran.rstudio.com/web/packages/packages.rds" %>% 
    url() %>% 
    readRDS() %>% 
    as.data.frame()

CRANview <- available.views() %>% 
    pluck("name") %>% 
    unlist()

theme_set(
    theme(panel.background = element_rect(fill = NA),
          panel.grid = element_line(color = "lightgray"),
          axis.text = element_text(color = "black"),
          axis.line = element_line(color = "black", size = 0.7),
          axis.ticks.length = unit(1.4, "mm"),
          axis.ticks = element_line(color = "black", size = 0.7),
          axis.title = element_text(color = "black", face = "bold"),
          strip.background = element_rect(color = "black",
                                          fill = "black"),
          strip.text = element_text(color = "white"),
          plot.title.position = "plot",
          plot.title = element_text(color = "black", face = "bold")))

ui <- dashboardPage(
    dashboardHeader(title = "R package explorer"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Widgets", tabName = "widgets", icon = icon("th"))
        )
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
        ),
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    fluidRow(
                        valueBox(
                            comma(nrow(db)), "Total CRAN Packages", icon = icon("box-open"),
                            color = "purple"
                        ),
                        
                        box(
                            title = "CRAN packages",
                            checkboxGroupInput("taskview", "CRAN Task View", 
                                               choices = CRANview),
                            textOutput("print")
                        )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "widgets",
                    h2("Widgets tab content")
            )
        )
    )
)

server <- function(input, output) { 
    
    output$print <- renderText({
        str(input$taskview)
    })
    
    output$taskviewPkgs <- reactive({
        ctv:::.get_pkgs_from_ctv_or_repos(input$taskview)
    })
    
    output$packages <- reactive({
        taskviewPkgs
    })
    
}

shinyApp(ui, server)