
#===================LOAD PACKAGES=======
library(tidyverse) 
library(dplyr)
library(tidytext)
library(tinytext)
library(tibble)
library(ggplot2)
library(scales)
library(syuzhet)
library(tm)
library(ggplot2)


#FakeNews <- read_csv("Fake.csv")
#TrueNews <- read_csv("True.csv")
#News_Articles <- read_csv("news_articles.csv")



temp <- tempfile()
download.file("https://github.com/ShannaJJ/FakeNews/raw/main/Fake.zip",temp)
FakeNews <- read.csv(unz(temp, "Fake.csv"))
unlink(temp)
FakeNews <- as.data.frame(FakeNews) %>% mutate(title = as.character(title),
                                               text = as.character(text),
                                               date = as.character(date),
                                               subject = as.character(subject))

temp <- tempfile()
download.file("https://github.com/ShannaJJ/FakeNews/raw/main/True.zip",temp)
TrueNews <- read.csv(unz(temp, "True.csv"))
unlink(temp)
TrueNews <- as.data.frame(TrueNews) %>% mutate(title = as.character(title),
                                               text = as.character(text),
                                               date = as.character(date),
                                               subject = as.character(subject))

temp <- tempfile()
download.file("https://github.com/ShannaJJ/FakeNews/raw/main/news_articles.csv.zip",temp)
News_Articles <- read.csv(unz(temp, "news_articles.csv"))
unlink(temp)

News_Articles <- as.data.frame(News_Articles) %>% mutate(title = as.character(title_without_stopwords),
                                                         text = as.character(text_without_stopwords),
                                                         published = as.character(published),
                                                         label = as.character(label))
                                                



## ==== Data Exploration & Preparation  ====
#Fake and True DataSets

as_tibble(head(FakeNews))
nrow(FakeNews)
as_tibble(head(TrueNews))
nrow(TrueNews)
# subject and date is not needed for this analysis so I will remove it.
FakeNews <- subset(FakeNews, select = -c(date,subject) )
TrueNews <- subset(TrueNews, select = -c(date,subject) )

#News_Articles DataSet
as_tibble(head(News_Articles))
News_Articles %>% group_by(type) %>% summarize(NoArticles = n())

#I need to split the News_Article dataset into True and False,
#Change the label of Real to True
#remove all other columns except title & text. 
News_Articles <- subset(News_Articles, select = c(title_without_stopwords,text_without_stopwords,label) )
News_Articles <- News_Articles %>% 
  rename(title = title_without_stopwords,
         text = text_without_stopwords,
         type = label)

TrueNews2 <- filter(News_Articles, type == 'Real')
TrueNews2 <- subset(TrueNews2, select = -c(type) )

# Concatenate TrueNews2 with TrueNews
TrueNews <- rbind(TrueNews, TrueNews2)

FakeNews <- FakeNews[1:22218,]
nrow(FakeNews)
nrow(TrueNews)

# Removed unused dataframes
 rm(TrueNews2, News_Articles)

#==========================
#Text Preprocessing - Finding the most common words in fake & true

library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")

# Add title & text to same field
FakeNews$text <- with(FakeNews, paste(title, text))
TrueNews$text <- with(TrueNews, paste(title, text))

# Load the data as a corpus

#FAKE NEWS WORD CLOUD
fake_corpus = VCorpus(VectorSource(FakeNews$text))
# Convert the text to lower case
fake_corpus = tm_map(fake_corpus, content_transformer(tolower))
# Remove numbers
fake_corpus = tm_map(fake_corpus, removeNumbers)
# Remove punctuation
fake_corpus = tm_map(fake_corpus, removePunctuation)
# Remove english common stopwords
fake_corpus = tm_map(fake_corpus, removeWords, stopwords())
# Eliminate extra white spaces
fake_corpus = tm_map(fake_corpus, stripWhitespace)
# Text stemming - which reduces words to their root form. 
# In my research I have noticed that this is not always the most accurate process 
fake_corpus = tm_map(fake_corpus, stemDocument)


fake_dtm = DocumentTermMatrix(fake_corpus)
fake_dtm = removeSparseTerms(fake_dtm, 0.999)
fake_dataset = as.data.frame(as.matrix(fake_dtm))

#wordCloud
library(wordcloud)

fake_v = sort(colSums(fake_dataset),decreasing=TRUE)
myNames = names(fake_v)
fake_words = data.frame(word=myNames,freq=fake_v,type='fake')

wordcloud(words = fake_words$word, freq = fake_words$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

#TRUE NEWS WORD CLOUD
true_corpus = VCorpus(VectorSource(TrueNews$text))
# Convert the text to lower case
true_corpus = tm_map(true_corpus, content_transformer(tolower))
# Remove numbers
true_corpus = tm_map(true_corpus, removeNumbers)
# Remove punctuation
true_corpus = tm_map(true_corpus, removePunctuation)
# Remove Stop words - They are the most commonly occurring words in a language and 
# have very little value in terms of gaining useful information. 
# They should be removed before performing further analysis.
true_corpus = tm_map(true_corpus, removeWords, stopwords())
# Eliminate extra white spaces
true_corpus = tm_map(true_corpus, stripWhitespace)
# Text stemming - which reduces words to their root form. 
# In my research I have noticed that this is not always the most accurate process 
true_corpus = tm_map(true_corpus, stemDocument)

true_dtm = DocumentTermMatrix(true_corpus)
true_dtm = removeSparseTerms(true_dtm, 0.999)
true_dataset = as.data.frame(as.matrix(true_dtm))

#wordCloud
library(wordcloud)

true_v = sort(colSums(true_dataset),decreasing=TRUE)
myNames_true = names(true_v)
true_words = data.frame(word=myNames_true,freq=true_v,type='true')

wordcloud(words = true_words$word, freq = fake_words$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

Words <- rbind(fake_words, true_words)

# Display the top 5 most frequent words
head(fake_words, 5)
head(true_words, 5)

# I am going to use the top 5 words in the analysis.
# There is a lot more analysis that can be done on the text such as - Word Associations, Term Frequencies etc.
#==========================

## ==== Data Transformation ====
# A fake or truth indicator must be added as this is what we are predicting.
FakeNews$Type <- c('Fake')
TrueNews$Type <- c('True')

#Join Fake & True Data to make a full data set.
NewsData <- rbind(FakeNews, TrueNews)

#Check for any Null Values
anyNA(NewsData)

#True Check which columns contain Null
colnames(NewsData)[colSums(is.na(NewsData)) > 0]

# Considering the text column is the main column we are using for analysis & prediction we will remove the NA values 
NewsData <- na.omit(NewsData)

nrow(NewsData)

#remove unused datasets
rm(FakeNews,TrueNews, true_dtm, true_v, fake_v, myNames, myNames_true,
   true_dataset,fake_corpus,fake_dataset,fake_dtm, true_corpus,true_words,fake_words)

# ==== Addition of extra information for predictions ==== ##
# Add an ID 
NewsData$ID <-seq_len(nrow(NewsData))
NewsData <- select(NewsData, ID, everything())

# Remove Title as we have already added it to the text field
NewsData <- subset(NewsData, select = -c(title) )

#Add number of sentences per article 
library(quanteda)
NewsData$No_of_sentences <- nsentence(NewsData$text)

#Add Number of characters per article
NewsData$TextLength <- nchar(NewsData$text)
summary(NewsData$TextLength)
TextLength <- NewsData$TextLength




# TextLength[1:50]

#======= Preprocessing the data=========
# Text data contains characters, like punctuation, stop words etc, 
# that does not give information and increase the complexity of the analysis. 
# So, in order to simplify our data, we remove all this noise to obtain a clean and analyzable dataset.

# Sapply function to calculate number of exclamation marks
NewsData$No_of_excl <- sapply(NewsData$text, function(x) length(unlist(strsplit(as.character(x), "\\!+"))))

# Sapply function to calculate number of question marks
NewsData$No_of_question <- sapply(NewsData$text, function(x) length(unlist(strsplit(as.character(x), "\\?+"))))

##Count of exclamations & question marks in fake and true news avg in Fake and True

Punctuation <-NewsData %>% group_by(Type) %>% summarise(Avg_Excl=round(mean(No_of_excl),3),
                                                        Avg_Ques=round(mean(No_of_question),3))

Punctuation <- Punctuation %>% gather("Punctuation", "Avg_per_Article", -Type)

ggplot(Punctuation, aes(x = Punctuation, y = Avg_per_Article, fill=Type)) +
  geom_col(position = "dodge") +   geom_text(aes(label=Avg_per_Article), position=position_dodge(width=0.9), vjust=-0.25)



NewsData$text<- gsub('[[:punct:]]', '', NewsData$text)  #remove punctuation

# Make text lower case
NewsData$text <- tolower(NewsData$text)                 #make it lower case

#From the word cloud of Fake News - Add Number of Times 'Trump' Appears 
NewsData$No_of_Wordtrump <- str_count(NewsData$text, "trump")
#From the word cloud of True News - Add Number of Times 'said' Appears 
NewsData$No_of_Wordsaid <- str_count(NewsData$text, "said")


Data_measures <- NewsData %>% group_by(Type) %>%
  summarize(No_articles = n(),
            Avg_no_Sentences = mean(No_of_sentences),
            Avg_TextLength = mean(TextLength),
            Avg_no_excl = mean(No_of_excl),
            Avg_no_question = mean(No_of_question),
            Avg_no_trump = mean(No_of_Wordtrump),
            Avg_no_said = mean(No_of_Wordsaid))

Data_measures

library("ggplot2")
library(gridExtra)

NewsData  %>%  ggplot(aes(No_of_sentences, No_of_question, color=Type)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(labels = scales::comma)+
  ggtitle("TextLength, question")

NewsData  %>%  ggplot(aes(No_of_sentences, No_of_excl, color=Type)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(labels = scales::comma)+
  ggtitle("TextLength, excl")



##===============================
# Stop words removal 

NoStop_text <- removeWords(NewsData$text, stopwords("en"))
NoStop_text <- data.frame(NoStop_text)
NoStop_text$ID <-seq_len(nrow(NoStop_text))
NoStop_text <- select(NewsData2, ID, everything())

NewsData <- left_join(NewsData,NoStop_text,by="ID")
NewsData <- NewsData %>% rename(NoStop_text = NoStop_text)

NewsData$NoStop_text <- str_replace_all(NewsData$NoStop_text, fixed("  "), " ")

#Add number of words per article
NewsData$No_of_words <- sapply(strsplit(NewsData$NoStop_text, " "), length)

###===============================
## Get Emotion of Text


emotion <-get_nrc_sentiment(as.character(NewsData$NoStop_text))
summary(emotion)

#Take only ID and Fake Column and combine with emotion
IDFAke <- NewsData[c(1,3)]
#Take only the emotions - negative & postive will the used in actual News dataset
emotionDF <- cbind(NewsData[c(3)],emotion[c(1,2,3,4,5,6,7,8)])
emotionDF2 <- cbind(NewsData[c(3)],emotion[c(9,10)])

emotionGraph <- emotionDF %>% group_by(Type) %>%
  summarize_all((mean))
emotionGraph2 <- emotionDF2 %>% group_by(Type) %>%
  summarize_all((mean))

emotionGraph <- emotionGraph %>% gather("emotion", "Avg_No_of_Words", -Type)
emotionGraph2 <- emotionGraph2 %>% gather("emotion", "Avg_No_of_Words", -Type)

#Create graph of Emotions Fake vs True Words
ggplot(emotionGraph, aes(x = emotion, y = Avg_No_of_Words, fill=Type)) +
  geom_col(position = "dodge")
ggplot(emotionGraph2, aes(x = emotion, y = Avg_No_of_Words, fill=Type)) +
  geom_col(position = "dodge")

#Interesting Observation is that Trust is the only sentiment on average that is more in true news than in fake news.

#taking only negative and positive and trust for the analysis
emotionNegPos<-emotion[c(8,9,10)]
emotionNegPos$ID <-seq_len(nrow(emotion))
emotionNegPos <- select(emotionNegPos, ID, everything())

NewsData<-left_join(NewsData,emotionNegPos)


Data_measures <- NewsData %>% group_by(Type) %>%
  summarize(No_articles = n(),
            Avg_no_Sentences = mean(No_of_sentences),
            Avg_TextLength = mean(TextLength),
            Max_TextLength = max(TextLength),
            Avg_no_excl = mean(No_of_excl),
            Avg_no_question = mean(No_of_question),
  )

Data_measures2 <- NewsData %>% group_by(Type) %>%
  summarize(No_articles = n(),
            Avg_No_trump = mean(No_of_Wordtrump),
            Avg_No_said = mean(No_of_Wordsaid),
            Avg_No_trust = mean(trust),
            Avg_No_positive = mean(positive),
            Avg_No_negative = mean(negative))

Dataset <- NewsData


#========= MODEL BUILDING & TRAINING ========

anyNA(NewsData)


NewsData <- subset(Dataset, select = -c(ID,text, NoStop_text))

#Encoding categorical data
NewsData$Type = factor(NewsData$Type,
                           levels= c('Fake','True'),
                           labels= c(1,0))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
library(caret)

set.seed(123)

split = sample.split(NewsData$Type, SplitRatio = 0.8)
train_set = subset(NewsData, split == TRUE)
test_set = subset(NewsData, split == FALSE)
# Feature Scaling
train_set[,2:11] = scale(train_set[,2:11])
test_set[,2:11] = scale(test_set[,2:11])

##MODEL 1 - Logistic Regression
# Logistic Regression

# Fitting Logistic Regression to the Training set
classifier = glm(formula = Type ~ No_of_sentences + TextLength,
                 family = binomial,
                 data = train_set)
classifier
plot(classifier, 1)

# Predicting the Test set results
prob_pred = predict(classifier, type = "response", newdata = test_set[2:3])
y_pred = ifelse(prob_pred > 0.5, "1", "0")
y_pred <-as.factor(y_pred)

# Making the Confusion Matrix
require(caret)   

cm = table(test_set[, 1], y_pred > 0.5)

cm = table(test_set[, 1], y_pred )

cm<-confusionMatrix(test_set[, 1] ,y_pred )

cm<-confusionMatrix(data=test_set$Type, 
                    reference=y_pred)

test = factor(test_set$Type)
cm


table(y_pred, test_set[["Type"]])

Accuracy<-round(cm$overall[1],2)
Accuracy


### MODEL 2 - Logistic Regression 2 : No of Words & Sentiment
# Using Logistic Regression again with the sentiment of the words.

# Fitting Logistic Regression to the Training set
classifier2 = glm(formula = Type ~ No_of_words + trust + negative + positive,
                  family = binomial,
                  data = train_set)
classifier2

# Predicting the Test set results
prob_pred2 = predict(classifier2, type = 'response', newdata = test_set[8:11])
y_pred2 = ifelse(prob_pred2 > 0.5, 1, 0)
y_pred2 <-as.factor(y_pred2)

# Making the Confusion Matrix
require(caret)    
cm<-confusionMatrix(data=y_pred2, 
                    reference=test_set$Type)
Accuracy<-cm$overall[1]
Accuracy


##MODEL 3 - Logistic Regression 3
# Logistic Regression

# Fitting Logistic Regression to the Training set
classifier3 = glm(formula = Type ~ .,
                  family = binomial,
                  data = train_set)
classifier3

# Predicting the Test set results
prob_pred3 = predict(classifier3, type = 'response', newdata = test_set[-1])

y_pred3 = ifelse(prob_pred3 > 0.5, 1, 0)
y_pred3 <-as.factor(y_pred3)

# Making the Confusion Matrix
require(caret)    
cm<-confusionMatrix(data=y_pred3, 
                    reference=test_set$Type)
Accuracy<-cm$overall[1]
Accuracy


##MODEL 4 - SUPPORT VECTOR MACHINE

library(e1071)

# Fitting SVM to the Training set and predicting the test set results
SVM_classifier = svm(formula = Type ~ No_of_sentences + TextLength,
                     data = train_set,
                     type = 'C-classification',
                     kernel = 'linear')

# Predicting the Test set results
y_pred = predict(SVM_classifier,  newdata = test_set[2:3])

# Making the Confusion Matrix
require(caret)    
cm<-confusionMatrix(data=y_pred, 
                    reference=test_set$Type)
cm

Accuracy <-round(cm$overall[1],2)
Accuracy



##MODEL 5 - SUPPORT VECTOR MACHINE 2
# Fitting SVM to the Training set and predicting the test set results

SVM2_classifier = svm(formula = Type ~ .,
                      data = train_set,
                      type = 'C-classification',
                      kernel = 'linear')

# Predicting the Test set results
y_pred = predict(SVM2_classifier,  newdata = test_set[-1])

# Making the Confusion Matrix
require(caret)    
cm<-confusionMatrix(data=y_pred, 
                    reference=test_set$Type)
cm
Accuracy<-round(cm$overall[1],2)
Accuracy


##MODEL 6 - DECISION TREE 
split = sample.split(NewsData$Type, SplitRatio = 0.8)
train_set = subset(NewsData, split == TRUE)
test_set = subset(NewsData, split == FALSE)

# Fitting Decision Tree to the Training set
install.packages('rpart')
library(rpart)
# Fitting SVM to the Training set and predicting the test set results
DT_classifier = rpart(formula = Type ~. , 
                      data = train_set)

# Predicting the Test set results
y_pred = predict(DT_classifier, newdata = test_set[-1],type = 'class')

# Making the Confusion Matrix
require(caret) 
cm<-confusionMatrix(data=y_pred, 
                    reference=test_set$Type)
cm

plot(DT_classifier)
text(DT_classifier)

Accuracy<-round(cm$overall[1],2)
Accuracy


##MODEL 7 - RANDOM FOREST 
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
library(caret)
set.seed(123)

split = sample.split(NewsData$Type, SplitRatio = 0.8)
train_set = subset(NewsData, split == TRUE)
test_set = subset(NewsData, split == FALSE)
# Feature Scaling
train_set[,2:11] = scale(train_set[,2:11])
test_set[,2:11] = scale(test_set[,2:11])


#install.packages('randomForest')
library(randomForest)
set.seed(123)

# Fitting SVM to the Training set and predicting the test set results
RF_classifier = randomForest(x = train_set[-1],
                             y = train_set$Type,
                             mtry = 6,
                             ntree = 500,
                             localImp = TRUE)
RF_classifier


# Predicting the Test set results
y_pred = predict(RF_classifier, newdata = test_set[-1])

# Making the Confusion Matrix
require(caret) 
cm<-confusionMatrix(data=y_pred, reference=test_set$Type)

#install.packages("randomForestExplainer")
library(randomForestExplainer)

importance_frame  <- measure_importance(RF_classifier)
save(importance_frame, file = "importance_frame.rda")
load("importance_frame.rda")
knitr::kable(importance_frame, caption = "Importance Frame")
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

Accuracy<-cm$overall[1]
Accuracy

## CONCLUSION

## REFERENCES

 
##--REFERENCES--------------------------------

# https://datascienceplus.com/parsing-text-for-emotion-terms-analysis-visualization-using-r/#:~:text=R%20offers%20the%20get_nrc_sentiment%20function,sadness%2C%20joy%2C%20and%20disgust.
# https://www.cspoerlein.com/files/textanalyse.html
# https://rstudio-pubs-static.s3.amazonaws.com/230437_32a31933273244e48d9723b33b7e6386.html
# http://trinker.github.io/qdap/vignettes/qdap_vignette.html#counts




