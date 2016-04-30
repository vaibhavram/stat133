library(DataComputing)
library(data.table)
library(stringr)

# rev <- read.csv("C:\\Users\\Vaibhav\\Documents\\Year 2 - Sophomore\\Semester 2\\Stat 133\\FinalProject\\Reviews.csv")
# 
# ## Cleanup that created the .Rda file
# clean <- function(table) {
#   table$Time <- as.POSIXct(table$Time, origin="1970-01-01")
#   names(table) <- c("Id", "ProductId", "UserId", "ProfileName", "Helpful", "TotalHelpfulRatings", "Score", "Time", "Summary", "Text")
#   table <- table %>%
#     mutate(Unhelpful = TotalHelpfulRatings - Helpful) %>%
#     select(Id, ProductId, UserId, ProfileName, Helpful, Unhelpful, TotalHelpfulRatings, Score, Time, Summary, Text)
#   table <- unique(select(table, -Id))
#   table <- table %>%
#     mutate(Id = 1)
#   table$Id <- seq.int(from=1, to=nrow(table))
#   setcolorder(table, c("Id", "ProductId", "UserId", "ProfileName", "Helpful", "Unhelpful", "TotalHelpfulRatings", "Score", "Time", "Summary", "Text"))
#   return(table)
# }
# 
# reviews <- clean(rev)

# Download and cleaning of existing .Rda file

load("C:\\Users\\Vaibhav\\Documents\\Year 2 - Sophomore\\Semester 2\\Stat 133\\FinalProject\\reviews.rda")
reviews <- unique(select(reviews, -Id))
reviews <- reviews %>%
  mutate(Id = 1)
reviews$Id <- seq.int(from=1, to=nrow(reviews))
setcolorder(reviews, c("Id", "ProductId", "UserId", "ProfileName", "Helpful", "Unhelpful", 
                       "TotalHelpfulRatings", "Score", "Time", "Summary", "Text"))

## Textual analysis of "Summary" and "Text"

    # Creating data table with all the matches

small_reviews <- reviews %>% select(Id, Helpful, Unhelpful, TotalHelpfulRatings, Score, Time, Summary)
ls <- tstrsplit(small_reviews$Summary, split=" ")
mat  <- matrix(unlist(ls), ncol=length(ls), byrow=FALSE)
words <- as.data.frame(mat, stringsAsFactors = FALSE)
wordsplit <- cbind(small_reviews, words)

    # Function to remove punctuation and capitalization from words

wordclean <- function(word) {
  g <- gsub("[^[:alnum:]]", "", word)
  return(tolower(g))
}

for(i in 8:ncol(wordsplit)) {
  wordsplit[,i] <- sapply(wordsplit[,i], wordclean)
}

    # Manipulating the data table to look at most common words

commons <- wordsplit %>%
  gather(key=col, value=word, -Id, -Helpful, -Unhelpful, -TotalHelpfulRatings, -Score, -Time, -Summary) %>%
  group_by(word) %>%
  summarize(count=n()) %>%
  filter(! is.na(word)) %>%
  arrange(desc(count))

avgscores <- wordsplit %>%
  gather(key=col, value=word, -Id, -Helpful, -Unhelpful, -TotalHelpfulRatings, -Score, -Time, -Summary) %>%
  group_by(word) %>%
  summarize(AvgScore=mean(Score)) %>%
  filter(! is.na(word)) %>%
  arrange(desc(AvgScore))

avghelpful <- wordsplit %>%
  mutate(H_percent=Helpful/TotalHelpfulRatings) %>%
  filter(! TotalHelpfulRatings==0) %>%
  gather(key=col, value=word, -Id, -Helpful, -Unhelpful, -TotalHelpfulRatings, -H_percent, -Score, -Time, -Summary) %>%
  group_by(word) %>%
  summarize(avghelpful=mean(H_percent), totratings=sum(TotalHelpfulRatings)) %>%
  filter(! is.na(word)) %>%
  arrange(desc(avghelpful))

allwords <- commons %>%
  left_join(avgscores, by=c("word"="word")) %>%
  left_join(avghelpful, by=c("word"="word"))

popularwords <- allwords %>%
  filter(count > 10000)

popularwords %>% 
  ggplot(aes(x=AvgScore, y=avghelpful)) +
  geom_text(aes(label=word))

negativewords <- allwords %>%
  filter(AvgScore<3, count>25)

positivewords <- allwords %>%
  filter(AvgScore>3, count>500)

## Looking at Food Words

foodwords <- read.csv("C:\\Users\\Vaibhav\\Documents\\Year 2 - Sophomore\\Semester 2\\Stat 133\\FinalProject\\foodwords.csv", header=FALSE)
names(foodwords) <- c("word")
foodwords$word <- gsub(" ", "", foodwords$word)

foodwords_data <- foodwords %>%
  left_join(allwords, by=c("word"="word")) %>%
  filter(count > 20)

foodwords_data %>% 
  ggplot(aes(x=AvgScore, y=avghelpful)) +
  geom_text(aes(label=word))









