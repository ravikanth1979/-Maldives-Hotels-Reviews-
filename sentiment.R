sentiment <- function(stem_corpus)
{
  require(rstudioapi)
  current_path <- getActiveDocumentContext()$path
  setwd(dirname(current_path))
  positive_lexicon <- read.csv("positive-lexicon.txt")
  negative_lexicon <- read.csv("negative-lexicon.txt")
  
  
  #Create variables and vectors
  total_pos_count <- 0
  total_neg_count <- 0
  pos_count_vector <- c()
  neg_count_vector <- c()
  #Calculate the size of the corpus
  size <- length(stem_corpus)
  for(i in 1:size)
  {
    #All the words in current review
    corpus_words<- list(strsplit(stem_corpus[[i]]$content, split = " "))
    #positive words in current review
    pos_count <-length(intersect(unlist(corpus_words), unlist(positive_lexicon)))
    #negative words in current review
    neg_count <- length(intersect(unlist(corpus_words), unlist(negative_lexicon)))
    
    total_pos_count <- total_pos_count + pos_count ## overall positive count
    total_neg_count <- total_neg_count + neg_count ## overall negative count
    
  }
  #Calculating overall percentage of positive and negative words of all the reviews
  total_pos_count ## overall positive count
  total_neg_count ## overall negative count
  total_count <- total_pos_count + total_neg_count
  overall_positive_percentage <- (total_pos_count*100)/total_count
  overall_negative_percentage <- (total_neg_count*100)/total_count
  overall_positive_percentage ## overall positive percentage
  #Create a dataframe with all the positive and negative reviews
  df<-data.frame(Review_Type=c("Postive","Negative"),
                 Count=c(total_pos_count ,total_neg_count ))
  print(df) #Print
  overall_positive_percentage <- round(overall_positive_percentage,2)
  return(overall_positive_percentage)
}
