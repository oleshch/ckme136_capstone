# Capstone Project 
# Orest Leshchyshen

# Set Environmental variables to work with MAC
# Java 1.6 will set up the 'Home' link under /Library/Java
Sys.setenv(JAVA_HOME = '/Library/Java//Home') 
Sys.setenv(LD_LIBRARY_PATH = '$LD_LIBRARY_PATH:$JAVA_HOME/lib')

# Install Packages
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("wordcloud")
#install.packages("fpc")
install.packages("rJava", type='source')
install.packages("RWeka")
require(devtools)
install_github('okugami79/sentiment140')
install.packages("qdapRegex")
install.packages("syuzhet")



# Load Packages
library(RSQLite)
library(DT)
library(tm)
library(SnowballC)  
library(ggplot2)
library(wordcloud)   
library(fpc)
library(RWeka)
library(sentiment)
library(qdapRegex)
library(syuzhet)

# ------------------
# Loading the Data
# ------------------

# Create DB Connection
dbconn <- dbConnect(SQLite(), "database.sqlite")

# Separate compliants
all_complaints <- dbGetQuery(dbconn, "SELECT consumer_complaint_narrative 
                             FROM consumer_complaints 
                             WHERE consumer_complaint_narrative IS NOT NULL
                             ORDER BY RANDOM()
                             LIMIT 5000")

# ------------------
# Preprocessing
# ------------------

myCorpus <- Corpus(VectorSource(all_complaints))
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# strip white space
myCorpus <- tm_map(myCorpus, stripWhitespace)
# Converting to lower case
myCorpus <- tm_map(myCorpus, tolower)
# remove stop words
myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
# Remove common word endings
myCorpus <- tm_map(myCorpus, stemDocument, language="english") 
# Remove masked characters
myCorpus <- tm_map(myCorpus, removeWords, c("xx","xxx","xxxx", "xxxxxxxx","xxxxxxxxxxxx","xxxxxxxxxxxxxxxx"))   
# Create plain text documents
myCorpus <- tm_map(myCorpus, PlainTextDocument) 

# ------------------
# Staging the data
# ------------------

#Create Document Term Matrix
dtm <- DocumentTermMatrix(myCorpus)
dtm <- removeSparseTerms(dtm, 0.4)

#Create Term Document Matrix
tdm <- TermDocumentMatrix(myCorpus) 

# ------------------
# Exploring the data
# ------------------

# Sort the most occurring terms
termfreq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   

# Ten of the most frequent terms
head(termfreq,10)

# Word Frequency Bar Graph
wf <- data.frame(word=names(termfreq), freq=termfreq)   
   
p <- ggplot(subset(wf, freq>2000), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   

# Word Cloud of Terms
set.seed(142)   
# Min frequesncy of 3000 occurences
wordcloud(names(termfreq), termfreq, min.freq=300, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))   

# Top 100 words
wordcloud(names(termfreq), termfreq, max.words=100, rot.per=0.2, colors=brewer.pal(6, "Dark2"))



#sort(as.matrix(dtm_tfidf)[1,], decreasing=F)[1:3]

all_complaints_df<-data.frame(text=unlist(sapply(myCorpus, `[`)), stringsAsFactors=F)

all_complaints_small<-data.frame(all_complaints_df[1:10,]) 

# Split Complaint into words and run sentiment analysis on them
for (row in all_complaints_small) {
  splitrow<-strsplit(rm_white_multiple(row), " ")
  sentimentdf<-0
  for (i in splitrow){
    sent = get_sentiment(i)
  }
  newdf<-data.frame(row,sum(sent))
}

c0<-sentimentdf[sentimentdf$sent < 0,]
c0["sentiment"]<- 0

c1<-sentimentdf[sentimentdf$polarity > 0,]
c1["sentiment"]<- 1



c0_50 <- c0[sample(nrow(c0), 50), ]
c1_50 <- c1[sample(nrow(c1), 50), ]
names(c0_50)<- c("complaints","sentiment")
names(c1_50)<- c("complaints","sentiment")

data_100 <- rbind(c0_50, c1_50) 
table(data_100$sentiment)

df1 <- as.data.frame(as.matrix(dtm))
df2 <- as.data.frame(as.matrix(dtm_tfidf))


dtm_tfidf <- DocumentTermMatrix(myCorpus, control = list(weighting = weightTfIdf, minWordLength=3))


df1_c<-cbind(df1,out_put_class=as.factor(data_100$sentiment))
df2_c<-cbind(df2,out_put_class=as.factor(data_100$sentiment))

## Using logistic regression 
formula = as.formula('out_put_class ~ .')
weka_fit1 <- Logistic(formula, data = df1_c)
evaluate_Weka_classifier(weka_fit1, numFolds = 10)

#GLM package for logistic Regresion



## logistic regression with tf-idf
weka_fit2 <- Logistic(formula, data = df2_c)
evaluate_Weka_classifier(weka_fit2, numFolds = 10)
