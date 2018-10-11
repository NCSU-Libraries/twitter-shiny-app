
# Twitter Hashtags Shiny App

## Overview of the app
This web app created by the shiny package in R is designed to visualize Twitter hashtags. It invites users to enter a Twitter hashtag and specify a number of tweets to harvest. From this tweet harvest, the app will generate a wordcloud, a frequency table showing top ten most frequent hashtags, and tweets regarding the most frequent hashtags from that tweet harvest based on a given keyword. This app can give us a general idea on how the keyword related to other topics.

## Data Source
The data used is the latest 7 days’ twitter information obtained from [Twitter’s public API](https://developer.twitter.com/en/docs/basics/getting-started). A table with 88 variables containing user id, tweet text, created time, hashtags included, media type, and other users’ info like follower count, friends count will be created by abstracting data randomly from Twitter’s database and the number of observations is determined by the app user.  This table is what we analyze hashtags based on. Given the sample size, then observations are collected randomly from Twitter’s database. Only hashtags and texts are used in this app.

## Instructions
It is quite easy to use this app:
1. Input the word that you are interested to explore and choose the sample size you want your outputs based on. The larger the sample size, the more info you can get from the outputs, of course, the running time will take longer.

![](https://github.com/April92/Twitter-Hashtags/blob/master/1.png)
2. The wordcould plot will be generated on the screen as below. By moving the mouse to the word on the plot, the count of this word based on the chosen sample size will be shown.

![](https://github.com/April92/Twitter-Hashtags/blob/master/2.png)

4. By clicking the Frequency Table tab button, you can see a frequency table showing the largest ten counts of hashtags in the sample.

![](https://github.com/April92/Twitter-Hashtags/blob/master/3.png)

5. The Tweets tab layer shows all tweet texts regarding the most frequent hashtag.
![](https://github.com/April92/Twitter-Hashtags/blob/master/4.png)

## Technical details
1. A [Twitter developer account](https://twitter.com/login?redirect_after_login=https%3A%2F%2Fdeveloper.twitter.com%2Fen%2Fapply%2Fuser) is needed to obtain Twitter data. Package ‘rtweet’ and ‘httpuv’ will assist us to get data from Twitter by R.
2. The ['shiny'](https://shiny.rstudio.com/package) in R helps create the web app. It provides the framework to build the input and output panels that form the web app. We just need to follow its rules and design our own app by setting the input data and output formats.
3. Functions in the package ['wordclouds2'](https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html) assisted by the package 'slam' can generate the wordcloud plot on a given frequency table.
4. The frequency table and tweets table from the app outputs are created by some data manipulations.
5. Some other packages are applied as well. The filter function in ['dplyr'](https://www.rdocumentation.org/packages/dplyr/versions/0.7.6) is used to create subtables, package ['rebus'](https://www.rdocumentation.org/packages/rebus/versions/0.0-4) is required for building regular expression to filter data and package ['stringr'](https://www.rdocumentation.org/packages/stringr/versions/1.1.0) is used for string detecting. They are quite useful for data manipulations. Package ['DT'](https://www.rdocumentation.org/packages/DT/versions/0.4) provides an R interface to the JavaScript library DataTable, and can create interactive table that can be displayed on HTML pages.

## R script
### Step 1. Install and load necessary R libraries. Only do this once.

```R
install.packages("dplyr") # Packages for manipulating data
install.packages("rtweet") # More info on this package: http://rtweet.info/
install.packages("httpuv") # May be required for authentication, depending on your machine
install.packages("slam") # Required by wordcloud2
install.packages("wordcloud2") # Package for creating a wordlcoud
install.packages('shiny') # Package for building a web app
install.packages('DT') # Create interactive table that can display on HTML pages
install.packages('rebus') # Required for building regular expression to filter data
install.packages('stringr') # Required for string detecting

#Load the libraries into the current session. Do this every time you want to run this script.
library(dplyr)
library(rtweet)
library(httpuv)
library(slam)
library(wordcloud2)
library(shiny)
library(DT)
library(stringr)
library(rebus)
```

### Step 2. Define key and secret and create a token to connect to Twitter's APT

```R
#Define key and secret
consumer_key <- ''
consumer_secret <- ''

Access_token <-''
Access_tokensecret <- ''

#Create a token to connect to Twitter's API using your key and secret
token <- create_token(app="", consumer_key, consumer_secret,
                      Access_token, Access_tokensecret,set_renv = TRUE)
```
### Step 3. Construct web app framework with 'shiny' package

#### Define UI for application

```R
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
```

#### Server

```R
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
```

### Step 4. Create the Shiny app object
```R
shinyApp(ui = ui, server = server)   
```     
