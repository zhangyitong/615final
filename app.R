#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggmap)
library(wordcloud)
library(tidytext)
library(tidyverse)
library(tm)
library(stringr)

usdata <- read.csv("twitter_us.csv")
cadata <- read.csv("twitter_la.csv")

tweets<-read.csv("tweets.csv")
tweets.text.corpus <- Corpus(VectorSource(tweets$x))
tweets.text.corpus <- tm_map(tweets.text.corpus, function(x)removeWords(x,stopwords()))


ui <- shinyUI(fluidPage(
  titlePanel("Twitter Project on Trader Joe's"),
  navbarPage(title = "Content",
             
             tabPanel("Brand",
                      h1("Trader Joe's"),
                      hr(),
                      img(src = "store.jpg")
                      
             ),# end of tab

 
             tabPanel("US Map",
                      mainPanel(plotOutput("plotiris"))
                      ),
             
             tabPanel("CA Map",
                      mainPanel(plotOutput("plotca"))
             ),
             
             tabPanel("Wordcloud",
                      mainPanel(plotOutput("wordcloud"))
                      
             )
  )# end of navbar
)# end fluid page
)# end shiny UI








# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {

  output$plotiris <- renderPlot(
    ggmap(get_map(location = "United States",  zoom = 4,
                  source = "google", maptype = "roadmap")) +
      geom_point(aes(x=lon,
                     y = lat), data = usdata, alpha = 0.5, size = 2,
                 color = "red")

  )
  
  output$plotca <- renderPlot(
    ggmap(get_map(location = "Los Angeles",  zoom = 6,
                  source = "google", maptype = "roadmap")) + 
      geom_point(aes(x=lon,
                     y = lat), data = cadata, alpha = 0.5, size = 2,
                 color = "red")
  )
  
  output$wordcloud <- renderPlot(
    wordcloud(tweets.text.corpus,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),
              random.color= TRUE, random.order = FALSE, max.words = 150)
  )
  
  
})

# Run the application 
shinyApp(ui = ui, server = server)

