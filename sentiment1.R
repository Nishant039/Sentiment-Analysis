library(RCurl)
#any chart  to show of confusion matrix
#5 best and 5 worst sentences.
test_data_url <- "https://dl.dropboxusercontent.com/u/8082731/datasets/UMICH-SI650/testdata.txt"
train_data_url <- "https://dl.dropboxusercontent.com/u/8082731/datasets/UMICH-SI650/training.txt"

test_data_file <- "testdata.txt"
train_data_file <- "training.txt"
e_file<-"evaluate.txt"
train_data_df <- read.csv(
  file  = train_data_file, 
  sep='\t', 
  header=FALSE, 
  quote = "",
  stringsAsFactor=F,
  col.names=c("Sentiment", "Text"))
test_data_df <- read.csv(
  file  = test_data_file, 
  sep='\t', 
  header=FALSE, 
  quote = "",
  stringsAsFactor=F,
  col.names=c("Text"))
e_data_df <- read.csv(
  file  = e_file, 
  sep='\t', 
  header=FALSE, 
  quote = "",
  stringsAsFactor=F,
  col.names=c("Text"))

# we need to convert Sentiment to factor
train_data_df$Sentiment <- as.factor(train_data_df$Sentiment)
head(train_data_df)


table(train_data_df$Sentiment)


mean(sapply(sapply(train_data_df$Text, strsplit, " "), length))

library(tm)



corpus <- Corpus(VectorSource(c(train_data_df$Text, test_data_df$Text,e_data_df$Text)))

corpus[1]$content


corpus <- tm_map(corpus, content_transformer(tolower))


corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus<- tm_map(corpus, stemDocument,language="english")

corpus
corpus[1:3]$content

dtm <- DocumentTermMatrix(corpus)
dtm


sparse <- removeSparseTerms(dtm, 0.99)
sparse

important_words_df <- as.data.frame(as.matrix(sparse))
colnames(important_words_df) <- make.names(colnames(important_words_df))
# split into train and test
important_words_train_df <- head(important_words_df, nrow(train_data_df))
important_words_test_df <- important_words_df[nrow(train_data_df)+1:nrow(test_data_df),]  #tail(important_words_df, nrow(test_data_df))
important_words_e_df<-tail(important_words_df, nrow(e_data_df))
# Add to original dataframes
train_data_words_df <- cbind(train_data_df, important_words_train_df)
test_data_words_df <- cbind(test_data_df, important_words_test_df)
e_data_words_df<-cbind(e_data_df,important_words_e_df)

t=train_data_words_df


train_data_words_df$Text <- NULL
test_data_words_df$Text <- NULL
e_data_words_df$Text<-NULL
library(caTools)
set.seed(1234)
# first we create an index with 80% True values based on Sentiment
spl <- sample.split(train_data_words_df$Sentiment, .85)


# now we use it to split our data into train and test
eval_train_data_df <- train_data_words_df[spl==T,]
eval_test_data_df <- train_data_words_df[spl==F,]


log_model <- glm(Sentiment~., data=eval_train_data_df, family=binomial)

#summary(log_model)

log_pred <- predict(log_model, newdata=eval_test_data_df, type="response")
table(eval_test_data_df$Sentiment, log_pred>.5)

log_pred_test <- predict(log_model, newdata=test_data_words_df, type="response")
test_data_df$Sentiment <- log_pred_test>.5

library(dplyr)
a=log_pred[(log_pred>0.65)&log_pred<0.73]
a
a=as.matrix(a)
a=as.data.frame(a)
colnames(a)="Log_values"
a
indices1=as.numeric(rownames(a))
t[indices1,]


b=log_pred[(log_pred>=0.999999)]
b
length(b)
set.seed(123)
c=sample(b,10,replace = F)
c
c=as.data.frame(c)
colnames(c)="Log_values"
c
indices2=as.numeric(rownames(c))
t[indices2,]


barplot(a$Log_values,names.arg = rownames(a),main="Potentialy Worst Sentences",ylab="Log values",xlab="Phrase ids")

par(mfrow=c(1,1))
barplot(c$Log_values,names.arg = rownames(c),main="Randomly Selected Best Sentences",ylab="Log values",xlab="Phrase ids",ann=FALSE)
usr <- par("usr")
par(usr=c(usr[1:2], 0, 1))
axis(2,at=seq(0,1,0.2))


head(test_data_df)
set.seed(1234)

log_pred_e<-predict(log_model, newdata=e_data_words_df,type="response")
e_data_df$sentiment <- log_pred_e>.69

e_data_df

valuer<-function(v)
{
  if (v==TRUE)
  {
    return ("Positive Sentiment")
  }
  else{
    return("Negative sentiment")
  }
}
e_data_df$Value=sapply(e_data_df$sentiment,valuer)
e_data_df

spl_test <- sample.split(test_data_df$Sentiment, .0005)
test_data_sample_df <- test_data_df[spl_test==T,]
test_data_sample_df[test_data_sample_df$Sentiment==T, c('Text')]

test_data_sample_df[test_data_sample_df$Sentiment==F, c('Text')]


