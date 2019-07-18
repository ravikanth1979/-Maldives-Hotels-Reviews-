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
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(stringr)
library(tidyr)
library(scales)
library(broom)
library(purrr)
library(widyr)
library(igraph)
library(ggraph)
library(SnowballC)
library(wordcloud)
library(wordcloud2)
library(reshape2)
library(janeaustenr)
library(devtools)
library(plotly)
library(grid)
library(reshape2)
library(caret)
library(e1071)
library(gridExtra)
library(rstudioapi)
library(plotrix)

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

all_hotels <- data.table::fread("maldives_hotel_reviews.csv")
all_hotels$`Review Date`<- as.Date(all_hotels$`Review Date`, format = "%B %d,%Y")
hotels_list <- unique(all_hotels$`Hotel Name`)
black.bold.italic.text <- element_text(face = "bold.italic", color = "black", size = 10)
black.bold.italic.title <- element_text(face = "bold.italic", color = "black", size = 11)

ui <- dashboardPage(
  dashboardHeader(title = "Maldives Hotel Reviews"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("All Hotels", tabName = "allhotels", icon = icon("dashboard")))
  ),
  dashboardBody(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),

    tabItems(
      tabItem(tabName = "allhotels",
              tabsetPanel(type = "tabs",
                          tabPanel("Maldives Hotels Sentiment Analysis", 
                                   fluidPage(
                                     fluidRow(
                                      column(6, div(style = "height:40px;"), DT::dataTableOutput('allhotels')),
                                      column(6, div(style = "height:40px;"), plotlyOutput("plotTimeSeries"))),
                                     fluidRow(
                                       column(6, div(style = "height:30px;"), plotlyOutput("plotStopwordsCommonWords")),
                                       column(6, div(style = "height:30px;"), plotlyOutput("plotStopStemmedwordsCommonWords"))),
                                     fluidRow(
                                       column(6, div(style = "height:30px;"), plotOutput("plotWordNetwork")),
                                       column(6, div(style = "height:30px;"), plotOutput("plotGrowingWords"))),
                                     fluidRow(
                                       column(6, div(style = "height:30px;"), plotOutput("plotShrinkingWords")),
                                       column(6, div(style = "height:30px;"), plotOutput("plotinterestwords"))),
                                     fluidRow(
                                       column(6, div(style = "height:30px;"), plotOutput("plotPostiveNegativeSentiment")),
                                       column(6, div(style = "height:30px;"), plotOutput("plotWordContributionPostiveNegativeSentiment"))),
                                     fluidRow(
                                       column(6, div(style = "height:30px;"), plotOutput("plotWordSentimentScore")),
                                       column(6, div(style = "height:30px;"), plotOutput("plotNegationWordSentimentScore"))),
                                     fluidRow(
                                       column(6, div(style = "height:50px;"), textOutput('mostpositivereview'),
                                              tags$head(tags$style('#mostpositivereview{color: #2F9587;
                                                                   font-size: 30px;
                                                                   font-weight: bold;
                                                                   font-style: italic;
                                                                   }'))),
                                       column(6, div(style = "height:50px;"), textOutput('mostnegativereview'),
                                              tags$head(tags$style('#mostnegativereview{color: #CB4335;
                                                                   font-size: 30px;
                                                                   font-weight: bold;
                                                                   font-style: italic;
                                                                   }')))),
                                     fluidRow(
                                       column(6,div(style = "height:50px;"), plotOutput('wordCloudpostivenegative')),
                                       column(6, div(style = "height:50px;"), plotOutput("wordCloud"))),
                                     fluidRow(
                                       column(12,div(style = "height:30px;"), plotOutput('positivepercentage')))
                                     )))))))

server <- function(input, output) {
  
  selected_hotel <-reactive({
    select_hotel = subset(all_hotels,all_hotels$`Hotel Name`%in% hotels_list[{input$allhotels_rows_selected}])
    select_hotel$`Total review count` = NULL
    select_hotel$Review_viaMobile = NULL
    select_hotel$`Guest Location` = NULL
    select_hotel$`Review Heading` = NULL
    select_hotel = unique(select_hotel)
    select_hotel <- tibble::rowid_to_column(select_hotel, "ID")
    print(select_hotel)
  })
  
  output$allhotels <- DT::renderDataTable({
    DT::datatable(unique(all_hotels[,c(2)]), selection = 'single', colnames = c('Select A Hotel From Below List'))
  }
  )
  
  output$plotTimeSeries = renderPlotly({
    hotel_new = count(selected_hotel(), Day = round_date(selected_hotel()$`Review Date`, "day"))
    #print(hotel_new)

    ggplot(hotel_new)+
    geom_line(aes(x = hotel_new$Day, y = hotel_new$n)) +
    labs (x = "Time Period", y = "Number of Reviews", title = "Number of Reviews for Period of Time")+
    theme(axis.text = black.bold.italic.text, title = black.bold.italic.title )
 })


output$plotStopwordsCommonWords = renderPlotly({
  #print(selected_hotel())
  hotel_new = selected_hotel()
  hotel_new  <- hotel_new %>%
                       mutate(`Review Date` = as.POSIXct(`Review Date`, origin = "1970-01-01"),
                             month = round_date(`Review Date`, "month"))
   review_words <- hotel_new %>%
    unnest_tokens(word, Review, drop = FALSE) %>%
    distinct(ID, word, .keep_all = TRUE) %>%
    anti_join(stop_words, by = "word") %>%
    filter(str_detect(word, "[^\\d]")) %>%
    group_by(word) %>%
    mutate(word_total = n()) %>%
    ungroup()

  word_counts <- review_words %>%
    count(word, sort = TRUE)
  print(word_counts)
  
  
   
  word_counts %>%
    head(25) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col(fill = "#088A85") +
    scale_y_continuous(labels = comma_format()) +
    coord_flip() +
    labs(title = "Most common words in review text(stop words removed)",
         y = "Number of uses")+
    theme(axis.text = black.bold.italic.text, title = black.bold.italic.title)
})

output$plotStopStemmedwordsCommonWords = renderPlotly({
  hotel_new = selected_hotel()
  hotel_new  <- hotel_new %>%
    mutate(`Review Date` = as.POSIXct(`Review Date`, origin = "1970-01-01"),
           month = round_date(`Review Date`, "month"))
  review_words <- hotel_new %>%
    unnest_tokens(word, Review, drop = FALSE) %>%
    distinct(ID, word, .keep_all = TRUE) %>%
    anti_join(stop_words, by = "word") %>%
    filter(str_detect(word, "[^\\d]")) %>%
    group_by(word) %>%
    mutate(word_total = n()) %>%
    ungroup()
  
  word_counts <- review_words %>%
    count(word, sort = TRUE)
  print(word_counts)
  
  word_counts %>%
    head(25) %>%
    mutate(word = wordStem(word)) %>% 
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col(fill = "#088A85") +
    scale_y_continuous(labels = comma_format()) +
    coord_flip() +
    labs(title = "Most common words in review text(stop words removed and stemmed)",
        y = "Number of uses")+
    theme(axis.text = black.bold.italic.text, title = black.bold.italic.title)
})

output$plotWordNetwork = renderPlot({
  #print(selected_hotel())
  hotel_new = selected_hotel()
  review_subject <- hotel_new %>% 
    unnest_tokens(word, Review) %>% 
    anti_join(stop_words)
  
  my_stopwords <- data_frame(word = c(as.character(1:10)))
  review_subject <- review_subject %>% 
    anti_join(my_stopwords)
  title_word_pairs <- review_subject %>% 
    pairwise_count(word, ID, sort = TRUE, upper = FALSE)
  set.seed(1234)
  
  title_word_pairs %>%
      filter(n >= 10) %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "#01DFD7") +
      geom_node_point(size = 4) +
      geom_node_text(aes(label = name), repel = TRUE, 
                     point.padding = unit(0.2, "lines")) +
      ggtitle(paste("Word Network of ",hotel_new$`Hotel Name`," Reviews")) +
      theme(axis.text = black.bold.italic.text, title = black.bold.italic.title )
   
})

output$plotGrowingWords = renderPlot({
  hotel_new = selected_hotel()
  hotel_new  <- hotel_new %>%
    mutate(`Review Date` = as.POSIXct(`Review Date`, origin = "1970-01-01"),
           month = round_date(`Review Date`, "month"))
  review_words <- hotel_new %>%
    unnest_tokens(word, Review, drop = FALSE) %>%
    distinct(ID, word, .keep_all = TRUE) %>%
    anti_join(stop_words, by = "word") %>%
    filter(str_detect(word, "[^\\d]")) %>%
    group_by(word) %>%
    mutate(word_total = n()) %>% ungroup()
  reviews_per_month <- hotel_new %>%
    group_by(month) %>%
    summarize(month_total = n())
  word_month_counts <- review_words %>%
    filter(word_total >= 10) %>%
    count(word, month) %>%
    complete(word, month, fill = list(n = 0)) %>%
    inner_join(reviews_per_month, by = "month") %>%
    mutate(percent = n / month_total) %>%
    mutate(year = year(month) + yday(month) / 365)
  mod <- ~ glm(cbind(n, month_total - n) ~ year, ., family = "binomial")
  slopes <- word_month_counts %>%
    nest(-word) %>%
    mutate(model = map(data, mod)) %>%
    unnest(map(model, tidy)) %>%
    filter(term == "year") %>%
    arrange(desc(estimate))
  slopes %>% head(9) %>%
    inner_join(word_month_counts, by = "word") %>%
    mutate(word = reorder(word, -estimate)) %>%
    ggplot(aes(month, n / month_total, color = word)) +
    geom_line(show.legend = FALSE) +
    scale_y_continuous(labels = percent_format()) +
    facet_wrap(~ word, scales = "free_y") +
    expand_limits(y = 0) +
    labs(x = "Year",
         y = "Percentage of reviews containing this word",
         title = "9 fastest growing words in Hotel reviews",
         subtitle = "Judged by growth rate over 1 year") +
   theme(axis.text = black.bold.italic.text, title = black.bold.italic.title )
})

output$plotShrinkingWords = renderPlot({
  hotel_new = selected_hotel()
  hotel_new  <- hotel_new %>%
    mutate(`Review Date` = as.POSIXct(`Review Date`, origin = "1970-01-01"),
           month = round_date(`Review Date`, "month"))
  review_words <- hotel_new %>%
    unnest_tokens(word, Review, drop = FALSE) %>%
    distinct(ID, word, .keep_all = TRUE) %>%
    anti_join(stop_words, by = "word") %>%
    filter(str_detect(word, "[^\\d]")) %>%
    group_by(word) %>%
    mutate(word_total = n()) %>%
    ungroup()
  reviews_per_month <- hotel_new %>%
    group_by(month) %>%
    summarize(month_total = n())
  word_month_counts <- review_words %>%
    filter(word_total >= 10) %>%
    count(word, month) %>%
    complete(word, month, fill = list(n = 0)) %>%
    inner_join(reviews_per_month, by = "month") %>%
    mutate(percent = n / month_total) %>%
    mutate(year = year(month) + yday(month) / 365)
  mod <- ~ glm(cbind(n, month_total - n) ~ year, ., family = "binomial")
  slopes <- word_month_counts %>%
    nest(-word) %>%
    mutate(model = map(data, mod)) %>%
    unnest(map(model, tidy)) %>%
    filter(term == "year") %>%
    arrange(desc(estimate))
  slopes %>% tail(9) %>%
    inner_join(word_month_counts, by = "word") %>%
    mutate(word = reorder(word, estimate)) %>%
    ggplot(aes(month, n / month_total, color = word)) +
    geom_line(show.legend = FALSE) +
    scale_y_continuous(labels = percent_format()) +
    facet_wrap(~ word, scales = "free_y") +
    expand_limits(y = 0) +
    labs(x = "Year", y = "Percentage of reviews containing this term",
         title = "9 fastest shrinking words in Maldives hotel reviews",
         subtitle = "Judged by growth rate over 1 year") +
    theme(axis.text = black.bold.italic.text, title = black.bold.italic.title )
})

output$plotinterestwords = renderPlot({
  hotel_new = selected_hotel()
  hotel_new  <- hotel_new %>%
    mutate(`Review Date` = as.POSIXct(`Review Date`, origin = "1970-01-01"),
           month = round_date(`Review Date`, "month"))
  review_words <- hotel_new %>%
    unnest_tokens(word, Review, drop = FALSE) %>%
    distinct(ID, word, .keep_all = TRUE) %>%
    anti_join(stop_words, by = "word") %>%
    filter(str_detect(word, "[^\\d]")) %>%
    group_by(word) %>%
    mutate(word_total = n()) %>%
    ungroup()
  reviews_per_month <- hotel_new %>%
    group_by(month) %>%
    summarize(month_total = n())
  word_month_counts <- review_words %>%
    filter(word_total >= 10) %>%
    count(word, month) %>%
    complete(word, month, fill = list(n = 0)) %>%
    inner_join(reviews_per_month, by = "month") %>%
    mutate(percent = n / month_total) %>%
    mutate(year = year(month) + yday(month) / 365)
    mod <- ~ glm(cbind(n, month_total - n) ~ year, ., family = "binomial")
    
    slopes <- word_month_counts %>%
      nest(-word) %>%
      mutate(model = map(data, mod)) %>%
      unnest(map(model, tidy)) %>%
      filter(term == "year") %>%
      arrange(desc(estimate))
    
     top_word_list = slopes %>% head(3) %>%
     inner_join(word_month_counts, by = "word") %>%
       mutate(word = reorder(word, estimate))

     word_list = as.character(unlist(unique(top_word_list$word)))
     print(word_list[1])
  word_month_counts %>%
    filter(word %in% word_list) %>%
    ggplot(aes(month, n / month_total, color = word)) +
    geom_line(size = 1, alpha = .8) +
    scale_y_continuous(labels = percent_format()) +
    expand_limits(y = 0) +
    labs(x = "Year",
         y = "Percentage of reviews containing this term", title = paste(word_list[1]," vs ",word_list[2], " vs " ,
                                                                         word_list[3], " in terms of reviewers interest")) +
    theme(axis.text = black.bold.italic.text, title = black.bold.italic.title )
})

output$plotPostiveNegativeSentiment = renderPlot({
  hotel_new = selected_hotel()
  
  reviews <- hotel_new %>% 
    filter(!is.na(Review)) %>% 
    select(ID, Review) %>% 
    group_by(row_number()) %>% 
    ungroup()
  tidy_reviews <- reviews %>%
    unnest_tokens(word, Review)
  tidy_reviews <- tidy_reviews %>%
    anti_join(stop_words)
  
  bing_word_counts <- tidy_reviews %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()
  
  bing_word_counts %>%
    group_by(sentiment) %>%
    top_n(18) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free") +
    labs(y = "Contribution to sentiment", x = "Word") +
    coord_flip() + 
    ggtitle('Words that contribute to positive and negative sentiment in the reviews')+
    theme(axis.text = black.bold.italic.text, title = black.bold.italic.title )
},height = "auto")

output$plotWordContributionPostiveNegativeSentiment = renderPlot({
  hotel_new = selected_hotel()
  
  reviews <- hotel_new %>% 
    filter(!is.na(Review)) %>% 
    select(ID, Review) %>% 
    group_by(row_number()) %>% 
    ungroup()
  tidy_reviews <- reviews %>%
    unnest_tokens(word, Review)
  tidy_reviews <- tidy_reviews %>%
    anti_join(stop_words)
  
  contributions <- tidy_reviews %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(word) %>%
    summarize(occurences = n(),
              contribution = sum(score))
  contributions %>%
    top_n(40, abs(contribution)) %>%
    mutate(word = reorder(word, contribution)) %>%
    ggplot(aes(word, contribution, fill = contribution > 0)) +
    ggtitle('Words with the greatest contributions to positive/negative 
          sentiment in reviews') +
    geom_col(show.legend = FALSE) +
    labs(y = "Contribution to sentiment", x = "Word") +
    coord_flip() +
    theme(axis.text = black.bold.italic.text, title = black.bold.italic.title )
},height = "auto")

output$plotWordSentimentScore = renderPlot({
  hotel_new = selected_hotel()
  
  review_bigrams <- hotel_new %>%
    unnest_tokens(bigram, Review, token = "ngrams", n = 2)
  bigrams_separated <- review_bigrams %>%
    separate(bigram, c("word_one", "word_two"), sep = " ")
  bigrams_filtered <- bigrams_separated %>%
    filter(!word_one %in% stop_words$word) %>%
    filter(!word_two %in% stop_words$word)
  bigram_counts <- bigrams_filtered %>% 
    count(word_one, word_two, sort = TRUE)
  bigrams_united <- bigrams_filtered %>%
    unite(bigram, word_one, word_two, sep = " ")
  bigrams_united %>%
    count(bigram, sort = TRUE)
  
  not_words_count <- bigrams_separated %>%
    filter(word_one == "not") %>%
    count(word_one, word_two, sort = TRUE)
  
  print(not_words_count)

  AFINN <- get_sentiments("afinn")
  not_words <- bigrams_separated %>%
    filter(word_one == "not") %>%
    inner_join(AFINN, by = c(word_two = "word")) %>%
    count(word_two, score, sort = TRUE) %>%
    ungroup()
  
  print(not_words)
  
  not_words %>%
    mutate(contribution = n * score) %>%
    arrange(desc(abs(contribution))) %>%
    head(50) %>%
    mutate(word_two = reorder(word_two, contribution)) %>%
    ggplot(aes(word_two, n * score, fill = n * score > 0)) +
    geom_col(show.legend = FALSE) +
    xlab("Words preceded by \"not\"") +
    ylab("Sentiment score * Number of occurrences") +
    ggtitle("The 50 words preceded by \"not\" that had the greatest contribution to 
            sentiment scores, positive or negative direction") +
    coord_flip() +
    theme(axis.text = black.bold.italic.text, title = black.bold.italic.title )
})

output$plotNegationWordSentimentScore = renderPlot({
  hotel_new = selected_hotel()
  
  review_bigrams <- hotel_new %>% unnest_tokens(bigram, Review, token = "ngrams", n = 2)
  bigrams_separated <- review_bigrams %>%
    separate(bigram, c("word_one", "word_two"), sep = " ")
  bigrams_filtered <- bigrams_separated %>%
    filter(!word_one %in% stop_words$word) %>%
    filter(!word_two %in% stop_words$word)
  bigram_counts <- bigrams_filtered %>% 
    count(word_one, word_two, sort = TRUE)
  bigrams_united <- bigrams_filtered %>%
    unite(bigram, word_one, word_two, sep = " ")
  bigrams_united %>% count(bigram, sort = TRUE)
  
  bigrams_separated %>% filter(word_one == "not") %>% count(word_one, word_two, sort = TRUE)
  AFINN <- get_sentiments("afinn")
  not_words <- bigrams_separated %>%
    filter(word_one == "not") %>%
    inner_join(AFINN, by = c(word_two = "word")) %>%
    count(word_two, score, sort = TRUE) %>%
    ungroup()
  negation_words <- c("not", "no", "never", "without")
  negated_words <- bigrams_separated %>%
    filter(word_one %in% negation_words) %>%
    inner_join(AFINN, by = c(word_two = "word")) %>%
    count(word_one, word_two, score, sort = TRUE) %>%
    ungroup()
  negated_words %>% mutate(contribution = n * score,
                    word_two = reorder(paste(word_two, word_one, sep = "__"), contribution)) %>%
    group_by(word_one) %>% top_n(100, abs(contribution)) %>%
    ggplot(aes(word_two, contribution, fill = n * score > 0)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ word_one, scales = "free") +
    scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
    xlab("Words preceded by negation term") +
    ylab("Sentiment score * Number of occurrences") +
    ggtitle('The most common positive or negative words to follow negations 
            such as "no", "not", "never" and "without"') +
    coord_flip()+
    theme(axis.text = black.bold.italic.text, title = black.bold.italic.title )
})


output$mostpositivereview = renderText({
  hotel_new = selected_hotel()
  print(hotel_new)
  
  reviews <- hotel_new %>% 
    filter(!is.na(Review)) %>% 
    select(`Review id`, Review) %>% 
    group_by(row_number()) %>% 
    ungroup()
  tidy_reviews <- reviews %>%
    unnest_tokens(word, Review)
  tidy_reviews <- tidy_reviews %>%
    anti_join(stop_words)
  
  sentiment_messages <- tidy_reviews %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(`Review id`) %>%
    summarize(sentiment = mean(score),
              words = n()) %>%
    ungroup() %>%
    filter(words >= 5)
    print(sentiment_messages)
    print(arrange(sentiment_messages, desc(sentiment))$`Review id`[1])
    
    paste("Most Positive Review and Review ID:",
          hotel_new[hotel_new$`Review id` %in% arrange(sentiment_messages, desc(sentiment))$`Review id`[1], ]$Review[1],
          "(",hotel_new[hotel_new$`Review id` %in% arrange(sentiment_messages, desc(sentiment))$`Review id`[1], ]$`Review id`[1],")"
    )
})

output$mostnegativereview = renderText({
  hotel_new = selected_hotel()
  print(hotel_new)
  
  reviews <- hotel_new %>% 
    filter(!is.na(Review)) %>% 
    select(`Review id`, Review) %>% 
    group_by(row_number()) %>% 
    ungroup()
  tidy_reviews <- reviews %>%
    unnest_tokens(word, Review)
  tidy_reviews <- tidy_reviews %>%
    anti_join(stop_words)
  
  sentiment_messages <- tidy_reviews %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(`Review id`) %>%
    summarize(sentiment = mean(score),
              words = n()) %>%
    ungroup() %>%
    filter(words >= 5)
  
  print(sentiment_messages)
  
  print(arrange(sentiment_messages, sentiment)$`Review id`[1])
  
  paste("Most Negative Review and Review ID:",
        hotel_new[hotel_new$`Review id` %in% arrange(sentiment_messages, sentiment)$`Review id`[1], ]$Review[1],
        "(",hotel_new[hotel_new$`Review id` %in% arrange(sentiment_messages, sentiment)$`Review id`[1], ]$`Review id`[1],")"
  )
})

output$wordCloud = renderPlot({
  require(tm)
  hotel_new = selected_hotel()
  hotel_new <- gsub("http\\S+\\s*", "", hotel_new)
  hotel_new <- gsub("[[:punct:]]", "", hotel_new)
  hotel_new <- gsub("[[:digit:]]", "", hotel_new)
  hotel_new <- gsub("^\\d", "", hotel_new)
  hotel_new <- gsub("\\r\\n", "", hotel_new)
  
  corpus_hotel_review <- Corpus(VectorSource(hotel_new))
  
  corpus_hotel_review = tm_map(corpus_hotel_review, content_transformer(tolower))
  corpus_hotel_review = tm_map(corpus_hotel_review, removeNumbers)
  corpus_hotel_review = tm_map(corpus_hotel_review, removePunctuation)
  corpus_hotel_review = tm_map(corpus_hotel_review, removeWords, stopwords())
  corpus_hotel_review = tm_map(corpus_hotel_review, stripWhitespace)
  corpus_hotel_review = tm_map(corpus_hotel_review, stemDocument)
  wordcloud(corpus_hotel_review,
            min.freq = 3,
            colors=brewer.pal(8, "Dark2"),
            random.color = TRUE,
            max.words = 100)
})

output$wordCloudpostivenegative = renderPlot({
  require(tm)
  hotel_new = selected_hotel()
  print(hotel_new)
  
  reviews <- hotel_new %>% 
    filter(!is.na(Review)) %>% 
    select(`Review id`, Review) %>% 
    group_by(row_number()) %>% 
    ungroup()
  tidy_reviews <- reviews %>%
    unnest_tokens(word, Review)
  tidy_reviews <- tidy_reviews %>%
    anti_join(stop_words)
  
  tidy_reviews %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("#FC7F8A", "#36C0B9"),
                     max.words = 100)
})


output$positivepercentage = renderPlot({
  require(tm)
  source("sentiment.R")
  print("inside positivepercentage...")
  hotel_new = selected_hotel()$Review
  
  print(hotel_new)
  hotel_new <- gsub("http\\S+\\s*", "", hotel_new)
  hotel_new <- gsub("[[:punct:]]", "", hotel_new)
  hotel_new <- gsub("[[:digit:]]", "", hotel_new)
  hotel_new <- gsub("^\\d", "", hotel_new)
  hotel_new <- gsub("\\r\\n", "", hotel_new)
  
  corpus_hotel_review <- Corpus(VectorSource(hotel_new))
  
  corpus_hotel_review = tm_map(corpus_hotel_review, content_transformer(tolower))
  corpus_hotel_review = tm_map(corpus_hotel_review, removeNumbers)
  corpus_hotel_review = tm_map(corpus_hotel_review, removePunctuation)
  corpus_hotel_review = tm_map(corpus_hotel_review, removeWords, stopwords())
  corpus_hotel_review = tm_map(corpus_hotel_review, stripWhitespace)
  corpus_hotel_review = tm_map(corpus_hotel_review, stemDocument)
  
  
  positive_percentage = sentiment(corpus_hotel_review)
  print(positive_percentage)
  negative_percentage = 100.00 - positive_percentage
  
  review_df <- data.frame("SN" = 1:2, "Review_Group" = c("Positive","Negative"),"Review_Percentage"=c(positive_percentage,negative_percentage))
  lbls <- paste(review_df$Review_Group, review_df$Review_Percentage,"%")
  box() 
  pie3D(review_df$Review_Percentage, labels = lbls, col = c("#36C0B9","#FC7F8A"), 
        main = "Positive and Negative Review Analysis", explode=0.5, radius=2.0, theta=0.6, labelcex = 2.0) 

})
}
# Run the application 
shinyApp(ui = ui, server = server)

