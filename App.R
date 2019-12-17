library(shinydashboard)
library(shiny)
library(leaflet)
library(htmltools)
library(tidyverse)
library(httr)
library(ggmap)
library(jsonlite)
library(tidyr)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(MASS)
library(plotly)
library(ggmap)
library(car)
library(psych)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(reshape2)
library(kableExtra)
##########################################
yelp_businessfinal <- read.csv("yelp_businessfinal.csv")
yelp_review <- read.csv("review.csv")

set.seed(1234)
data(stop_words)

table1 <- yelp_businessfinal %>% 
    group_by(name) %>%
    summarise(no_rows = length(name)) %>% 
    arrange(desc(no_rows)) %>% 
    slice(1:50)

yelp_review1 <-yelp_review %>%
    mutate(text=as.character(text)) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)

table2 <- yelp_review1 %>%
    count(word, sort = TRUE) %>%
    mutate(word = reorder(word, n)) %>%
    slice(1:50)

yelp_businessfinal2 <-yelp_businessfinal %>% 
    mutate(categories=as.character(categories)) %>% 
    select(business_id, name, city, categories) %>%
    unnest_tokens(word, categories) %>% 
    anti_join(stop_words)

table3 <- yelp_businessfinal2 %>%
    count(word, sort = TRUE) %>%
    mutate(word = reorder(word, n)) %>%
    slice(1:50)

yelp_review1_sentiment <- yelp_review1 %>%
    inner_join(get_sentiments("bing"))%>%
    count(name, sentiment) 

yelp_review1_sentiment2 <- yelp_review1_sentiment %>%
    mutate(new_n=ifelse(sentiment == "negative",-n, n))

yelp_review1_sentiment3 <- yelp_review1_sentiment %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)

##########################################

header <- dashboardHeader(
    title = "615 Final Project Xinci"
)

siderbar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
        menuItem("Mapping", tabName = "Mapping", icon = icon("map")),
        menuItem("Text Analysis on Review ", tabName = "menu3", icon = icon("font"),collapsible = TRUE,
                 menuSubItem('All Stores', tabName = 'All1'),
                 menuSubItem('Seperate Stores', tabName = 'Seperate1')),
        menuItem("Text Analysis on Categories", tabName = "menu4", icon = icon("font")),
        menuItem("Sentiment Analysis", tabName = "SentimentAnalysis", icon = icon("font"),collapsible = TRUE,
                 menuSubItem('All Stores', tabName = 'All2'),
                 menuSubItem('Seperate Stores', tabName = 'Seperate2'))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "Dashboard",
                fluidPage(
                box(plotOutput("myplot1",height = 725),width=1000)
                )
                ),
        tabItem(tabName = "Mapping",
                fluidPage(
                    titlePanel("Map of 50 Stores in Las Vegas"),
                    absolutePanel(top = 210, left = 275, width = 200,
                                  selectInput("stores1","Choose a store:",
                                              choices=c("ALL",unique(as.character(yelp_businessfinal$name))))),
                    leafletOutput("mymap",width = "100%", height = 725)
                    )
                ),
        tabItem(tabName = "All1",
                fluidRow(
                    box(plotOutput("myplot2",height = 750)),
                    box(wordcloud2Output("mywordcloud2"))
                    )
                ),
        tabItem(tabName = "Seperate1",
                fluidRow(
                    box(
                        selectInput("stores2","Choose a store:",
                                    choices=c("ALL",unique(as.character(yelp_review$name)))))),
                fluidRow(
                    box(
                        wordcloud2Output("mywordcloud3"))
                )
                ),
        tabItem(tabName = "menu4",
                fluidRow(
                    box(plotOutput("myplot4",height = 750)),
                    box(wordcloud2Output("mywordcloud4"))
                )
                ),
        tabItem(tabName = "All2",
                fluidRow(
                    box(plotOutput("myplot5",height = 750)),
                    box(plotOutput("myplot6",height = 750))
                )
        ),
        tabItem(tabName = "Seperate2",
                fluidRow(
                    box(selectInput("stores3","Choose a store:",
                                    choices=unique(as.character(yelp_review$name)))),
                    box(tableOutput("mytable1")),
                    box(plotOutput("myplot7"))
                )
        )
    )
)
ui <-dashboardPage(
    header,
    siderbar,
    body
    )

server <- function(input, output, session) {
    output$myplot1 <- renderPlot(
        ggplot(table1) +
            geom_bar(aes(x= reorder(name, no_rows), y=no_rows),stat = "identity",fill="steelblue") +
            geom_text(aes(x=name, y=no_rows,label = no_rows),size=3,color="white",hjust=1.25)+
            labs(x = "Top 50 Stores in Las Vegas",y="Frequency")+
            coord_flip()+
            theme_minimal()
    )
    output$mymap <- renderLeaflet({
        if (input$stores1 != "ALL") {
            yelp_businessfinal <- yelp_businessfinal[yelp_businessfinal$name == input$stores1,]
        }
        leaflet(data = yelp_businessfinal) %>%
            addTiles() %>%
            addAwesomeMarkers(~yelp_businessfinal$longitude, ~yelp_businessfinal$latitude, label = ~htmlEscape(yelp_businessfinal$name))
        })
    output$myplot2 <- renderPlot({
        ggplot(data=table2,aes(word, n)) +
            geom_col(fill="steelblue") +
            geom_text(aes(x=word, y=n,label = n),size=2,color="white",hjust=1.25)+
            labs(x = "Top 50 Common Words in Customer Review",y="Frequency")+
            coord_flip()+
            theme_minimal()
    })
    output$mywordcloud2 <- renderWordcloud2({
        wordcloud2(table2,size = 0.75,shape="circle", color='random-dark')
    })
    output$mywordcloud3 <- renderWordcloud2({
        if (input$stores2 != "ALL") {
            wc2 <- yelp_review1 %>%
                dplyr::filter(grepl(input$stores2, name)) %>%
                count(word, sort = TRUE)%>%
                slice(1:100)
            wordcloud2(wc2,size = 0.75,shape="circle", color='random-dark')
        }
        else if (input$stores2 == "ALL") {
            wc2 <- yelp_review1 %>%
                count(word, sort = TRUE)%>%
                slice(1:100)
            wordcloud2(wc2,size = 0.75,shape="circle", color='random-dark')
        }
    })
    output$myplot4 <- renderPlot({
        ggplot(data=table3,aes(word, n)) +
            geom_col(fill="steelblue") +
            geom_text(aes(x=word, y=n,label = n),size=2,color="white",hjust=1.25)+
            labs(x = "Top 50 Category types",y="Frequency")+
            coord_flip()+
            theme_minimal()
    })
    output$mywordcloud4 <- renderWordcloud2({
        wordcloud2(table3,size = 0.75,shape="circle", color='random-dark')
    })
    output$myplot5 <- renderPlot({
        ggplot(yelp_review1_sentiment2) +
            geom_bar(aes(x=name, y=new_n,fill = (new_n > 0)),stat = "identity") +
            geom_text(aes(x=name, y=new_n,label = new_n),size=2,color="black")+
            coord_flip()+
            scale_fill_brewer(palette="Paired")+
            labs(x = "Stores in Las Vegas",y="Frequency of Negative and Postive Sentiment")+
            theme_minimal()
    })
    output$myplot6 <- renderPlot({
        ggplot(yelp_review1_sentiment3, aes(x = reorder(name, sentiment), y=sentiment,fill = sentiment > 0)) +
            geom_bar(stat = "identity")+
            geom_text(aes(label = sentiment),size=2,color="black",hjust=1)+
            coord_flip()+
            theme_minimal()+
            scale_fill_brewer(palette="Paired")+
            labs(x = "Stores in Las Vegas",y="Frequency of Net Sentiment")
    })
    output$myplot7 <- renderPlot({
            yelp_review1_sentiment4 <- yelp_review1 %>%
                dplyr::filter(grepl(input$stores3, name)) %>% 
                inner_join(get_sentiments("bing")) %>% 
                count(word,sentiment,sort = TRUE) %>% 
                group_by(sentiment) %>%
                top_n(10) %>%
                ungroup() %>%
                mutate(word = reorder(word, n))
            ggplot(data=yelp_review1_sentiment4, aes(word, n,fill = sentiment,width=0.5)) +
                geom_bar(stat="identity")+
                geom_text(aes(x=word, y=n,label = n),size=4,color="white",hjust=1.25)+
                facet_wrap(~sentiment,scales = "free_y") +
                coord_flip()+
                theme_minimal()+
                scale_fill_brewer(palette="Paired")+
                labs(x = "Words",y="Frequency")+
                theme(legend.position = "none")
    })
    output$mytable1 <- function() {
            yelp_review1_sentiment5 <- yelp_review1 %>%
                dplyr::filter(grepl(input$stores3, name)) %>% 
                inner_join(get_sentiments("bing")) %>% 
                count(name,sentiment,sort = TRUE) %>% 
                mutate(Ratio = n/sum(n)) %>% 
                rename(Frequency=n,Sentiment=sentiment,Name=name)
            kable(yelp_review1_sentiment5) %>% 
                kable_styling(bootstrap_options = c("striped", "hover", "condensed"),full_width = F)
    }
}
shinyApp(ui, server)











