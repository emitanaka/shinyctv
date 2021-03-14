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
library(plotly)
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

names(CRANview) <- available.views() %>% 
    pluck("topic") %>% 
    unlist()


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
    dashboardHeader(title = "R package explorer",
                    dropdownMenu(type = "messages",
                                 messageItem(
                                     from = "Contact",
                                     message = "Need help or contact developer?",
                                     icon = icon("envelope"),
                                     href = "mailto:emi.tanaka@monash.edu"
                                 ),
                                 messageItem(
                                     from = "Support",
                                     message = "This app will continue to be updated.",
                                     icon = icon("github"),
                                     time = "2021-03-10",
                                     href = "https://github.com/emitanaka/shinyctv"
                                 ))),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            selectInput("taskview", "CRAN Task View", 
                        choices = CRANview),
            dateRangeInput("cranglog_daterange", "Date range:", 
                           start = Sys.Date() - years(2) - 2, end = Sys.Date() - 2),
            selectInput("cranlog_pkgs", "Selected packages",
                        choices = "MASS", multiple = TRUE)
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
                        valueBoxOutput("taskBox")),
                    fluidRow(
                        tabBox(
                            width = "250px",
                            height = "280px",
                            selected = "Tab3",
                            tabPanel("Tab1", "Tab content 1",
                                     dataTableOutput("pkgtable") 
                                     ),
                            tabPanel("Tab2", "Ngram",
                                     numericInput("ngram", "Ngram value", value = 2, min = 1, max = 10, step = 1),
                                     selectInput("desc", "Words", choices = c("Title", "Description")),
                                     dataTableOutput("ngramtab")
                                     ),
                            tabPanel("Tab3", "Tab content 3",
                                     plotlyOutput("histplot")
                                     ),
                            tabPanel("Tab4", "Tab content 4",
                                     plotlyOutput("timeplot")
                            )
                        
                    )
                    )

            )
        )
    ))

server <- function(input, output, session) { 
    observe({
        top5 <- dldatsum() %>% 
            slice_max(order_by = total, n = 5) %>% 
            pull(package)
        updateSelectInput(session, "cranlog_pkgs", choices = pkgs(),
                          selected = top5)
    })
    
    pkgs <- reactive({
        ctv:::.get_pkgs_from_ctv_or_repos(input$taskview)[[1]]
    })
    

    
    dldat <- reactive({
        start <- input$cranglog_daterange[1]
        end <- input$cranglog_daterange[2]
        cran_downloads(pkgs(), from = start, to = end)
    })
    
    dldatsum <- reactive({
        dldat() %>% 
            group_by(package) %>% 
            summarise(total = sum(count)) %>% 
            ungroup()
    })
    
    dldat_select <- reactive({
        dldat() %>% 
            filter(package %in% input$cranlog_pkgs)
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
        h2(paste("Task View:", names(CRANview)[CRANview==input$taskview]))
    })
    
    output$taskBox <- renderValueBox({
        valueBox(
            paste0(comma(length(pkgs())), " / ", comma(nrow(db))), 
            "CRAN Task View Packages out of total", icon = icon("box-open"),
            color = "maroon"
        )
    })
    
    output$histplot <- renderPlot({
        dldat() %>% 
            group_by(package) %>% 
            summarise(total = sum(count)) %>%
            ggplot(aes(total)) + 
            geom_histogram(color = "white", fill = "#AD0059") + 
            scale_x_log10(label = comma) + 
            labs(x = glue("Total download counts from {input$cranglog_daterange[1]} to {input$cranglog_daterange[2]}"),
                 y = "Number of packages") +
            scale_y_continuous(expand = c(0, 0))
            
    })
    
    output$timeplot <- renderPlot({
        ggplot(dldat_select(), aes(date, count, color = package)) +
            # add shadow lines
            geom_line(data = rename(dldat_select(), package2 = package), 
                      color = "gray", aes(group = package2)) +
            # add date when package was updated
            #geom_vline(data = updates, aes(xintercept = update),
            #           linetype = "dashed", color = "#79003e") + 
            # the trend line
            geom_line() +
            scale_y_log10() +
            facet_grid(package ~ .) + 
            labs(title = glue("Downloaded from {input$cranglog_daterange[1]} to {input$cranglog_daterange[2]}")) + 
            scale_color_discrete_qualitative() +
            guides(color = FALSE)
    })
    
}

shinyApp(ui, server)