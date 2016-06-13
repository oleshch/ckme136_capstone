# Capstone Project 
# Orest Leshchyshen

# Install Packages
list.of.packages <- c("RSQLite", "DBI","tm","SnowballC","wordcloud","qdapRegex","fpc", "rJava","RWeka","syuzhet" ,"ggmap", "zipcodes")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) {install.packages(new.packages)}

# Load Packages
library(RSQLite)
library(dplyr)
library(DT)
library(tm)
library(SnowballC)  
library(ggplot2)
library(wordcloud)   
library(fpc)
library(qdapRegex)
library(RWeka)
library(glm2)
library(syuzhet)
library(ggmap)
library(zipcode)

# ------------------
# Loading the Data
# ------------------

# Create DB Connection
dbconn <- dbConnect(SQLite(), "database.sqlite")

data <- dbGetQuery(dbconn, "SELECT * 
                             FROM consumer_complaints 
                             WHERE consumer_complaint_narrative IS NOT NULL")

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

# Create Term frequency - inverse document frequency Matrix
dtm_tfidf <- DocumentTermMatrix(myCorpus, control = list(weighting = weightTfIdf, minWordLength=3))

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
wordfreq <- subset(data.frame(word=names(termfreq), freq=termfreq), freq>2000) 

# Graph of Word Frequency
ggplot(wordfreq, aes(x=reorder(word, freq), y=freq)) +
  geom_bar(stat="identity", fill="#173e43") +
  theme_light(base_size = 16) +
  coord_flip() + 
  xlab("") + 
  ylab("Frequency of Word") 


# Word Cloud of Terms
set.seed(142)   

# Min frequesncy of 3000 occurences
wordcloud(names(termfreq), termfreq, min.freq=1000, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))   

# Top 100 words
wordcloud(names(termfreq), termfreq, max.words=800, rot.per=0.2, colors=brewer.pal(6, "Dark2"))

# Number of Complaints by Product
ggplot(data %>% group_by(product) %>% summarise(num_products=length(product)),
       aes(x=reorder(product, num_products), y=num_products)) +
  geom_bar(stat="identity", fill="#173e43") +
  coord_flip() + 
  xlab("") + 
  ylab("Number of Complaints") +
  theme_light(base_size=16)

# Number of complaints by Company
companies <- data %>%
  group_by(company) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:10)

ggplot(companies, aes(y = count, x = reorder(company, count))) + 
  geom_bar(stat="identity", fill="#173e43") +
  coord_flip() + 
  xlab("") + 
  ylab("Number of complaints by Company") +
  theme_light(base_size = 16) 

company_response_to_consumer <- data %>%
  group_by(company_response_to_consumer) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(company_response_to_consumer, aes(y = count, x = reorder(company_response_to_consumer, count))) + 
  geom_bar(stat="identity", fill="#173e43") +
  coord_flip() + 
  xlab("") + 
  ylab("Number of complaints by Company") +
  theme_light(base_size = 16) 

data("zipcode")
mapzipcode<-data
mapzipcode$zip<- gsub("X","0",data$zipcode)
mapzipcode$zip<- clean.zipcodes(mapzipcode$zip)
mapzipcode<- merge(mapzipcode, zipcode, by.x='zip', by.y='zip')

maping <- mapzipcode %>%
  group_by(zip,longitude,latitude, state.y) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

map<-get_map(location='united states', zoom=4, maptype="roadmap")
ggmap(map) +
  geom_point(aes(x=longitude, y=latitude, size=count, colour=state.y), data=maping, alpha=.2) +
  xlab("") + 
  ylab("") +
  theme_light(base_size = 16) +
  geom_text(data = maping,aes(x=longitude, y=latitude, label = count, size =100),check_overlap = TRUE)

# ------------------
# Structure the data for analysis
# ------------------

all_complaints_df<-data.frame(text=head(unlist(sapply(myCorpus, `[`)),-9), stringsAsFactors=F)

# Split Complaint into words 
for (row in all_complaints_df) {
  splitrow<-strsplit(rm_white_multiple(row), " ")
}

# Run get_sentiment on each row
score<-lapply(lapply(splitrow, function(x) get_sentiment(x) ), function(x) sum(x))

# Create sentiment score
sentimentdf<-data.frame(all_complaints_df,unlist(score))
colnames(sentimentdf) <- c("complaints", "score")

c0<-sentimentdf[sentimentdf$score < 0,]
c0["sentiment"]<- 0

c1<-sentimentdf[sentimentdf$score > 0,]
c1["sentiment"]<- 1


c0_50 <- c0[sample(nrow(c0), 50), ]
c1_50 <- c1[sample(nrow(c1), 50), ]
names(c0_50)<- c("complaints","score","sentiment")
names(c1_50)<- c("complaints","score","sentiment")

data_100 <- rbind(c0_50, c1_50) 
table(data_100$sentiment)

df1 <- as.data.frame(as.matrix(dtm))
df2 <- as.data.frame(as.matrix(dtm_tfidf))


df1_c<-cbind(df1,out_put_class=as.factor(data_100$sentiment), row.names = NULL)
df2_c<-cbind(df2,out_put_class=as.factor(data_100$sentiment), row.names = NULL)


## Using logistic regression 
formula = as.formula('out_put_class ~ .')
weka_fit1 <- Logistic(formula, data = df1_c)
evaluate_Weka_classifier(weka_fit1, numFolds = 10)

## logistic regression with tf-idf
weka_fit2 <- Logistic(formula, data = df2_c)
evaluate_Weka_classifier(weka_fit2, numFolds = 10)
