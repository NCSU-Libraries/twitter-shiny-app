## This is a tutorial to collect Twitter data and create a wordcloud of most frequent hashtags
## from tweets based on a hashtag search

# Step 1. Install and load necessary R libraries. Only do this once.

#install.packages("dplyr") # Packages for working with text
#install.packages("rtweet") # More info on this package: http://rtweet.info/
#install.packages("httpuv") # May be required for authentication, depending on your machine
#install.packages("slam") # Required by wordcloud2
#install.packages("wordcloud2") #Package for creating a wordlcoud
#install.packages('shiny')
#install.packages('DT')
# Load the libraries into the current session. Do this every time you want to run this script.

library(dplyr)
library(rtweet) 
library(httpuv)
library(slam)
library(wordcloud2)
library(shiny)
library(DT)
library(stringr)
library(rebus)

#Define key and secret 

consumer_key <- 'Your Keys'
consumer_secret <- 'Your Secret'

Access_token <-'Your Token'
Access_tokensecret <- 'Your tokensecret'

#Create a token to connect to Twitter's API using your key and secret
token <- create_token(app="APP Name", consumer_key, consumer_secret, 
                      Access_token, Access_tokensecret,set_renv = TRUE)

# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("WordCloud for Twitter Hashtags"),
   
  # Inputs(s) panel
  fluidRow(
        
      # Enter a hashtag that interests you. 
      column(5,textInput(inputId = "word", 
                label = "Hashtag", 
                value='data')),
     
      #Select sample size
      column(5,sliderInput(inputId='n',
                  label='Sample Size',
                  value=200,
                  min=100,
                  max=500))
    ),
  
  #Output tab panel
   tabsetPanel(
     
    # Output(s)
    tabPanel('Wordclouds',wordcloud2Output(outputId = "wordcloud")),
    tabPanel('Frequency Table',DT::dataTableOutput(outputId = "table")),
    tabPanel('Tweets',DT::dataTableOutput(outputId='txt'))
    )
  
   )


# Server
server <- function(input, output) {
  
  #Adding '#' before the word for Twitter searching purpose
  hashtag <-reactive({
    req(input$word)
    paste("#",input$word)
    })
  
  #Collecting data from Twitter using the input word
  tweets <- reactive({
    search_tweets(hashtag(), input$n , include_rts = FALSE)
    })
  
  #Unlist hashtags vector
  hashtags_list <- reactive({tweets()$hashtags %>% 
    unlist()  %>%   #Collapse the hashtag lists into one long list
    tolower() }) #Convert text to lowercase
  
  #Make a frequency table of hashtags 
  hashtags_count <- reactive({table(hashtags_list())})
  
  #Transform to a data frame
  hashtags_df <- reactive({cbind.data.frame(tags=names(hashtags_count()),count=as.integer(hashtags_count()))})
    
  #Sort the table in decending order 
  table_sort<-reactive({
    hashtags_df()[order(hashtags_df()$count,decreasing = T),]
  })
  
  #Transform to lower case for tweets$hashtags as a list 
  tweet_list <-reactive(
    lapply(tweets()$hashtags,tolower)
  )
  
  # Detect if the hashtags contain the most frequent word
  int<-reactive(
    lapply(tweet_list(),str_detect,pattern=START %R% as.character(table_sort()[2,1]) %R% END)
  )
  
  #create the index
  intt<-reactive(
    sapply(int(),sum)
  )
  
  #Output Tweets Text for the most frequent hashtag
  output$txt <-DT::renderDataTable(
    tweets()[which(intt()>0),5]
  )
  
  #Output the frequecy table
  output$table<-DT::renderDataTable({
    DT::datatable(
      table_sort()[2:11,],
      rownames=F,
      colnames=c('Hashtag','Count')
    )
  })
  
  # Create the wordcloud from the hashtag
  output$wordcloud <- renderWordcloud2({
    hashtags_df() %>%  #create a dataframe from the hashtags table
      filter(hashtags_df()$tags != input$word) %>% #filter out the hashtag term itself from the wordcloud
      wordcloud2(.) 
  }) #make the wordcloud 

}
      
# Create the Shiny app object
shinyApp(ui = ui, server = server)   
      
