install.packages("stringi")
library(stringi)

library(DataComputing)

install.packages("data.table")
library(data.table)

load("/Users/jonathanxu/Downloads/reviews (1).rda")

reviews <- unique(select(reviews, -Id))
reviews <- reviews %>%
  mutate(Id = 1)
reviews$Id <- seq.int(from=1, to=nrow(reviews))
setcolorder(reviews, c("Id", "ProductId", "UserId", "ProfileName", "Helpful", "Unhelpful", 
                       "TotalHelpfulRatings", "Score", "Time", "Summary", "Text"))

text_count <- reviews %>% mutate(count = stri_count(Text, regex = "\\S+"))

text_count %>% ggplot(aes(x = Score, y = count)) + geom_point(size = 0.5, alpha = .3, position = "jitter") + ylim(0,1000)