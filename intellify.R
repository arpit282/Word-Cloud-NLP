
#########################################################################
#####################  TEXT MINING (WORD CLOUD)    #######################


library(wordcloud)
library(tm)
library(SnowballC)

# Data intake
modi <- readLines("modi's new speech.txt")

# Corpus formation
modi_corpus <- Corpus(VectorSource(modi))
inspect(modi_corpus)

# Cleaning of data
modi_clean <- tm_map(modi_corpus,removeNumbers)
inspect(modi_clean[2:6])

modi_clean <- tm_map(modi_corpus,tolower)
modi_clean <- tm_map(modi_corpus,stripWhitespace)
modi_clean <- tm_map(modi_corpus,removePunctuation)
modi_clean <- tm_map(modi_corpus,removeWords,stopwords("en"))
modi_clean <- tm_map(modi_corpus,stemDocument)




# word cloud formation
cloud <- wordcloud(modi_clean,min.freq = 2)
cloud <- wordcloud(modi_clean,min.freq = 2,colors = brewer.pal(8,"Dark2"),random.order = F,
                   rot.per = .50)



