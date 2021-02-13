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
library(DT)

db <- "http://cran.rstudio.com/web/packages/packages.rds" %>% 
    url() %>% 
    readRDS() %>% 
    as.data.frame()

CRANview <- available.views() %>% 
    pluck("name") %>% 
    unlist()

DTcallback <- c(
    "$('table.dataTable.display tbody tr:odd').css('background-color', 'green');",
    "$('table.dataTable.display tbody tr:even').css('background-color', 'red');",
    "$('table.dataTable.display tbody tr:odd')",
    "  .hover(function(){",
    "    $(this).css('background-color', 'yellow');",
    "   }, function(){",
    "    $(this).css('background-color', 'green');",
    "   }",
    "  );",
    "$('table.dataTable.display tbody tr:even')",
    "  .hover(function(){",
    "    $(this).css('background-color', 'orange');",
    "   }, function(){",
    "    $(this).css('background-color', 'red');",
    "   }",
    "  );"
)

singularize2 <- function(x) {
    res <- singularize(x)
    ifelse(res=="bia", "bias", res)
}

stop_words_rex <- c(paste0("^", stop_words$word, "$"), "^doi", "^[0-9.]+$")

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
            selectInput("taskview", "CRAN Task View", 
                        choices = CRANview)
        )
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
        ),
        tabItems(
            tabItem(tabName = "dashboard",
                    htmlOutput("header"),
                    fluidRow(
                        valueBoxOutput("taskBox"),
                        dataTableOutput("pkgtable")
                    ),
                    fluidRow(
                        box(title = "Ngram",
                            numericInput("ngram", "Ngram value", value = 2, min = 1, max = 10, step = 1),
                            selectInput("desc", "Words", choices = c("Title", "Description")),
                            dataTableOutput("ngramtab")
                        )
                    )

            )
        )
    )
)

server <- function(input, output) { 
    
    pkgs <- reactive({
        ctv:::.get_pkgs_from_ctv_or_repos(input$taskview)[[1]]
    })
    
    task_db <- reactive({
        db %>% 
            filter(Package %in% pkgs()) %>% 
            mutate(Description = str_replace_all(Description, "\n", " "),
                   Description = str_squish(Description),
                   Title = str_replace_all(Title, "\n", " "))
    })
    
    ngram_tab <- reactive({
        words <- paste0("word", 1:input$ngram)
        task_db() %>% 
            unnest_tokens(word, as.name(input$desc), token = "ngrams", n = input$ngram) %>% 
            separate(word, words, sep = " ") %>% 
            mutate(across(num_range("word", 1:input$ngram), singularize2)) %>% 
            distinct(!!!map(c("Package", words), as.name)) %>% 
            filter(if_all(num_range("word", 1:input$ngram), 
                          ~!str_detect(., paste0(stop_words_rex, collapse = "|")))) %>% 
            count(!!!map(words, as.name), sort = TRUE) %>% 
            mutate(word = do.call("paste", map(words, ~eval(parse(text = .x))))) %>% 
            select(word, n) %>% 
            # ngrams that appear once is not interesting
            filter(n!=1)
    })
    
    output$ngramtab <- renderDataTable({
        datatable(ngram_tab())
    })
    
    output$pkgtable <- renderDataTable({
        task_db() %>% 
            select(Package, Title) %>% 
            datatable()
    })
    
    output$header <- renderUI({
        h2(paste("Task View:", input$taskview))
    })
    
    output$taskBox <- renderValueBox({
        valueBox(
            paste0(comma(length(pkgs())), " / ", comma(nrow(db))), 
            "CRAN Task View Packages out of total", icon = icon("box-open"),
            color = "maroon"
        )
    })
    
}

shinyApp(ui, server)