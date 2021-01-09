
#===================LOAD PACKAGES=======
library(tidyverse) 
library(dplyr)
library(tidytext)
library(tibble)
library(ggplot2)
library(scales)
library(syuzhet)
library(tm)
library(ggplot2)


FakeNews <- read_csv("Fake.csv")
TrueNews <- read_csv("True.csv")

## ==== Data Exploration & Preparation  ====

head(FakeNews)
head(TrueNews)


FakeNews %>% group_by(subject) %>%   summarize(no_articles = n()) 
TrueNews %>% group_by(subject) %>%   summarize(no_articles = n())
# There are only 2 subjects in the truenews data set - interesting that 100% world news is labeled True.
# I will probably not use this column as there is no mix of subjects.  

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
# But due to time constraints I will move on. 
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

# Remove Title, Date, Subject field
NewsData <- subset(NewsData, select = -c(title,date,subject) )

#Add number of sentences per article 
library(quanteda)
NewsData$No_of_sentences <- nsentence(NewsData$text)

#Add Number of character per article
NewsData$TextLength <- nchar(NewsData$text)
summary(NewsData$TextLength)
TextLength <- NewsData$TextLength
TextLength[1:50]

ggplot(NewsData, aes(x = TextLength, fill = Type)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Text Count", x = "Length of Text",
       title = "Distribution of Text Lengths by Type")

# Preprocessing the data
# Text data contains characters, like punctuations, stop words etc, 
# that does not give information and increase the complexity of the analysis. 
# So, in order to simplify our data, we remove all this noise to obtain a clean and analyzable dataset.

# Sapply function to calculate number of exclamation marks
NewsData$excl <- sapply(NewsData$text, function(x) length(unlist(strsplit(as.character(x), "\\!+"))))

# Sapply function to calculate number of question marks
NewsData$question <- sapply(NewsData$text, function(x) length(unlist(strsplit(as.character(x), "\\?+"))))

##Count of exclamations & question marks in fake and true news avg in Fake and True
NewsData %>% group_by(Type) %>% summarise(Sum_Excl=sum(excl),
                                          Avg_Excl=mean(excl),
                                          Sum_Ques=sum(question),
                                          Avg_Ques=mean(question))

# Make text lower case
NewsData$text <- tolower(NewsData$text) #make it lower case
NewsData$text<- gsub('[[:punct:]]', '', NewsData$text) #remove punctuation


##===============================
  
# Remove Stop words - They are the most commonly occurring words in a language and 
# have very little value in terms of gaining useful information. 
# They should be removed before performing further analysis.
library(tm)
tidy_NewsData <- NewsData %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords())

tidy_NewsData<-tidy_NewsData[c(1,7)]

tidy_NewsData %>%
  count(word, sort = TRUE) 

#Add number of words per article
NewsData$No_of_words <- sapply(strsplit(NewsData$text, " "), length)


###===============================
## Get Emotion of Text
# R offers the get_nrc_sentiment function via the Tidy or Syuzhet packages for analysis of 
# emotion words expressed in text. Both packages implemented Saif Mohammad's NRC Emotion lexicon, 
# comprised of several words for emotion expressions of anger, fear, anticipation, trust, surprise, 
# sadness, joy, and disgust

emotion <-get_nrc_sentiment(as.character(NewsData$text))
emotion

#Take only ID and Fake Column and combine with emotion
IDFAke <- NewsData[c(1,3)]
#Take only the emotions - negative & postive will the used in actual News dataset
emotionDF <- cbind(NewsData[c(3)],emotion[c(1,2,3,4,5,6,7,8)])

emotionGraph <- emotionDF %>% group_by(Type) %>%
  summarize_all(mean)

emotionGraph <- emotionGraph %>% gather("emotion", "Avg_No_of_Words", -Type)

#Create graph of Emotions Fake vs True Words
ggplot(emotionGraph, aes(x = emotion, y = Avg_No_of_Words, fill=Type)) +
  geom_col(position = "dodge")

#Interesting Observation is that Trust is the only sentiment on average that is more in true news than in fake news.
#I will add this observation to the dataset

#taking only negative and positive and trust for the analysis
emotionNegPos<-emotion[c(8,9,10)]
emotionNegPos$ID <-seq_len(nrow(emotion))
emotionNegPos <- select(emotionNegPos, ID, everything())

NewsData<-left_join(NewsData,emotionNegPos)

Dataset <- NewsData

#========= MODEL BUILDING & TRAINING ========

anyNA(NewsData)

NewsData <- subset(Dataset, select = -c(ID,text))

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
train_set[,2:9] = scale(train_set[,2:9])
test_set[,2:9] = scale(test_set[,2:9])


##MODEL 1 - Logistic Regression
# Logistic Regression

# Fitting Logistic Regression to the Training set
classifier = glm(formula = Type ~ No_of_sentences + TextLength,
                 family = binomial,
                 data = train_set)
classifier
summary(classifier)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[2:3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)
y_pred <-as.factor(y_pred)

# Making the Confusion Matrix
require(caret)    
cm<-confusionMatrix(data=y_pred, 
                    reference=test_set$Type)
cm
Accuracy<-round(cm$overall[1],2)
Accuracy


##MODEL 2 - Logistic Regression
# Logistic Regression

# Fitting Logistic Regression to the Training set
classifier2 = glm(formula = Type ~ trust + negative + positive,
                 family = binomial,
                 data = train_set)
classifier2
summary(classifier2)

# Predicting the Test set results
prob_pred2 = predict(classifier2, type = 'response', newdata = test_set[7:9])
y_pred2 = ifelse(prob_pred2 > 0.5, 1, 0)
y_pred2 <-as.factor(y_pred2)

# Making the Confusion Matrix
require(caret)    
cm<-confusionMatrix(data=y_pred2, 
                    reference=test_set$Type)
cm
Accuracy<-round(cm$overall[1],2)
Accuracy


##MODEL 3 - Logistic Regression
# Logistic Regression

# Fitting Logistic Regression to the Training set
classifier3 = glm(formula = Type ~ negative + positive,
                  family = binomial,
                  data = train_set)
classifier3
summary(classifier3)

# Predicting the Test set results
prob_pred3 = predict(classifier3, type = 'response', newdata = test_set[8:9])

y_pred3 = ifelse(prob_pred3 > 0.5, 1, 0)
y_pred3 <-as.factor(y_pred3)

# Making the Confusion Matrix
require(caret)    
cm<-confusionMatrix(data=y_pred3, 
                    reference=test_set$Type)
cm
Accuracy<-round(cm$overall[1],2)
Accuracy

##MODEL 4 - SUPPORT VECTOR MACHINE

# Fitting SVM to the Training set and predicting the test set results
library(e1071)
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
Accuracy<-round(cm$overall[1],2)
Accuracy

##MODEL 4 - SUPPORT VECTOR MACHINE 2

# Fitting SVM to the Training set and predicting the test set results
library(e1071)
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



##--REFERENCES--------------------------------

# https://datascienceplus.com/parsing-text-for-emotion-terms-analysis-visualization-using-r/#:~:text=R%20offers%20the%20get_nrc_sentiment%20function,sadness%2C%20joy%2C%20and%20disgust.


