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
library(rio)
library(readr)
library(data.table)


db1 <- "http://cran.rstudio.com/web/packages/packages.rds" %>% 
    url() %>% 
    readRDS() %>% 
    as.data.frame()

convert("www/packages.rds", "www/packages.csv") 

db <- as_tibble(fread("www/packages.csv"))


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
            menuItem("About", icon = icon("accusoft"), tabName = "about"),
            menuItem("Source code", icon = icon("file-code-o"), 
                     href = "https://github.com/emitanaka/shinyctv"
            ),
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

            ),
            tabItem(tabName = "about",
                    fluidRow(
                        fluidRow(
                            column(
                                box(
                                    title = div("About this Shiny App", style = "padding-left: 25px", class = "h2"),
                                    column(
                                        "The app contains interactive analysis of CRAN download logs and textual analysis of the title and descriptions of packages by the CRAN Task View ",
                                        tags$a(href = "https://github.com/emitanaka/shinyctv", "here"),".
                       The findings are displayed in two tables and two plots.",
                                        tags$br(),
                                        h3("Motivations"),
                                        "  ",
                                        tags$br(),
                                        tags$br(),
                                        "-  ",
                                        tags$br(),
                                        "-  ",
                                        tags$br(),
                                        "-  ",
                                        tags$br(),
                                        tags$br(),
                                        "Only look at the data, it is hard to interpret how the CRAN download logs throughout the time, and textual analysis of the title and descriptions of packages, I hope it possible for you to get something inspired and have fun.",
                                        tags$br(),
                                        h3("Data Source"),
                                        "The raw dataset is from",
                                        tags$a(href = "http://cran-logs.rstudio.com/", "CRAN package download logs"),
                                        h3("Creator"),
                                        "Emi Tanaka @",
                                        tags$a(href = "https://github.com/emitanaka/shinyctv", "Emi Tanaka's Github"),
                                        h3("References"),
                                        "Bob Rudis and Blake Embrey (2020). pluralize: Pluralize and 'Singularize' Any (English) Word. R package version 0.2.0. https://CRAN.R-project.org/package=pluralize ",
                                        tags$br(),
                                        tags$br(),
                                        "Chang, Winston, and Barbara Borges Ribeiro. 2018. Shinydashboard: Create Dashboards with ’Shiny’. https://CRAN.R-project.org/package=shinydashboard.",
                                        tags$br(),
                                        tags$br(),
                                        "Chang, Winston, Joe Cheng, JJ Allaire, Yihui Xie, and Jonathan McPherson. 2020. Shiny: Web Application Framework for R. https://CRAN.R-project.org/package=shiny.",
                                        tags$br(),
                                        tags$br(),
                                        "Csardi G, Nepusz T: The igraph software package for complex network research, InterJournal, Complex Systems 1695. 2006. https://igraph.org",
                                        tags$br(),
                                        tags$br(),
                                        "Chung-hong Chan, Geoffrey CH Chan, Thomas J. Leeper, and Jason Becker (2021). rio: A Swiss-army knife for data file I/O. R package version 0.5.26.",
                                        tags$br(),
                                        tags$br(),
                                        "Chang, W. (2018, November 06). Themes for Shiny [R package shinythemes version 1.1.2]. Retrieved from https://CRAN.R-project.org/package=shinythemes.",
                                        tags$br(),
                                        tags$br(),
                                        "Gábor Csárdi (2019). cranlogs: Download Logs from the 'RStudio' 'CRAN' Mirror. R package version 2.1.1. https://CRAN.R-project.org/package=cranlogs ",
                                        tags$br(),
                                        tags$br(),
                                        "Garrett Grolemund, Hadley Wickham (2011). Dates and Times Made Easy with lubridate. Journal of Statistical Software, 40(3), 1-25. URL https://www.jstatsoft.org/v40/i03/.",
                                        tags$br(),
                                        tags$br(),
                                        "Hadley Wickham and Jim Hester (2020). readr: Read Rectangular Text Data. R package version 1.4.0. https://CRAN.R-project.org/package=readr",
                                        tags$br(),
                                        tags$br(),
                                        "Hadley Wickham (2020). rvest: Easily Harvest (Scrape) Web Pages. R package version 0.3.6. https://CRAN.R-project.org/package=rvest. ",
                                        tags$br(),
                                        tags$br(),
                                        "Hadley Wickham and Dana Seidel (2020). scales: Scale Functions for Visualization. R package version 1.1.1. https://CRAN.R-project.org/package=scales",
                                        tags$br(),
                                        tags$br(),
                                        "Hao Zhu (2021). kableExtra: Construct Complex Table with 'kable' and Pipe Syntax. http://haozhu233.github.io/kableExtra/, https://github.com/haozhu233/kableExtra.",
                                        tags$br(),
                                        tags$br(),
                                        "Jim Hester (2020). glue: Interpreted String Literals. R package version 1.4.2. https://CRAN.R-project.org/package=glue",
                                        tags$br(),
                                        tags$br(),
                                        "Matt Dowle and Arun Srinivasan (2020). data.table: Extension of `data.frame`. R package version 1.13.2. https://CRAN.R-project.org/package=data.table.",
                                        tags$br(),
                                        tags$br(),
                                        "",
                                        tags$br(),
                                        tags$br(),
                                        " ",
                                        tags$br(),
                                        tags$br(),
                                        " ",
                                        tags$br(),
                                        tags$br(),
                                        " ",
                                        tags$br(),
                                        tags$br(),
                                        " ",
                                        tags$br(),
                                        tags$br(),
                                        "Sievert, Carson. 2020. Interactive Web-Based Data Visualization with R, Plotly, and Shiny. Chapman; Hall/CRC. https://plotly-r.com.",
                                        tags$br(),
                                        tags$br(),
                                        "Silge J, Robinson D (2016). “tidytext: Text Mining and Analysis Using Tidy Data Principles in R.” _JOSS_, *1*(3). doi: 10.21105/joss.00037 (URL:https://doi.org/10.21105/joss.00037), <URL:http://dx.doi.org/10.21105/joss.00037>.",
                                        tags$br(),
                                        tags$br(),
                                        "Thomas Lin Pedersen (2021). ggraph: An Implementation of Grammar of Graphics for Graphs and Networks. R package version 2.0.5. https://CRAN.R-project.org/package=ggraph ",
                                        tags$br(),
                                        tags$br(),
                                        "Wickham, Hadley. 2016. Ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York. https://ggplot2.tidyverse.org.",
                                        tags$br(),
                                        tags$br(),
                                        "Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy D’Agostino McGowan, Romain François, Garrett Grolemund, et al. 2019. “Welcome to the tidyverse.” Journal of Open Source Software 4 (43): 1686. https://doi.org/10.21105/joss.01686.",
                                        tags$br(),
                                        tags$br(),
                                        " ",
                                        tags$br(),
                                        tags$br(),
                                        "Wickham, Hadley, and Dana Seidel. 2020. Scales: Scale Functions for Visualization. https://CRAN.R-project.org/package=scales.",
                                        tags$br(),
                                        tags$br(),
                                        "Yihui Xie, Joe Cheng and Xianying Tan (2020). DT: A Wrapper of the JavaScript Library 'DataTables'. R package version 0.16. https://CRAN.R-project.org/package=DT",
                                        tags$br(),
                                        tags$br(),
                                        "Zeileis A (2005). “CRAN Task Views.” _R News_, *5*(1), 39-40. <URL:https://CRAN.R-project.org/doc/Rnews/>.",
                                        tags$br(),
                                        tags$br(),
                                        "Zeileis A, Fisher JC, Hornik K, Ihaka R, McWhite CD, Murrell P, Stauffer R, Wilke CO (2020). “colorspace: A Toolbox for Manipulating and Assessing Colors and Palettes.” _Journal of Statistical Software_, *96*(1), 1-49. doi:10.18637/jss.v096.i01 (URL: https://doi.org/10.18637/jss.v096.i01).",
                                        width = 12,
                                        style = "padding-left: 20px; padding-right: 20px; padding-bottom: 40px; margin-top: -15px"
                                    ),
                                    width = 12,
                                ),
                                width = 12,
                                style = "padding: 20px"
                            )
                        )
                    ))
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