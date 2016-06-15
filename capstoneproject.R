# ------------------
# Capstone Project 
# Orest Leshchyshen
# ------------------

# Install Packages
list.of.packages <- c("RSQLite", "DBI","tm","SnowballC","wordcloud","qdapRegex","fpc", "rJava","RWeka","syuzhet" ,"ggmap", "zipcode","png", "pROC")
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
library(png)
library(pROC)

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
                             LIMIT 10000")

# ------------------
# Preprocessing
# ------------------

#Remove four or more duplicate letters
for (complaint in all_complaints) { all_complaints <- data.frame(gsub('([[:alpha:]])\\1{3,}', '\\1', complaint)) }
#Make corpus
myCorpus <- Corpus(DataframeSource(all_complaints))
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
myCorpus <- tm_map(myCorpus, removeWords, c("xx","xxx","xxxx","xxxxx", "xxxxxxxx","xxxxxxxxxx","xxxxxxxxxxxx","xxxxxxxxxxxxxxxx"))   
# Create plain text documents
myCorpus <- tm_map(myCorpus, PlainTextDocument) 


# ------------------
# Staging the data
# ------------------

#Create Document Term Matrix
dtm <- DocumentTermMatrix(myCorpus, control = list(minWordLength=3))
dtm <- removeSparseTerms(dtm, 0.8)

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
wordcloud(names(termfreq), termfreq, min.freq=800, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))   

# Top 100 words
wordcloud(names(termfreq), termfreq, max.words=100, rot.per=0.2, colors=brewer.pal(6, "Dark2"))

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
  ylab("Company Response to Complaints") +
  theme_light(base_size = 16) 


# Location of complaints
data("zipcode")
mapzipcode<-data
mapzipcode$zip<- gsub("X","0",data$zipcode)
mapzipcode$zip<- clean.zipcodes(mapzipcode$zip)
mapzipcode<- merge(mapzipcode, zipcode, by.x='zip', by.y='zip')

maping <- mapzipcode %>%
  group_by(zip,longitude,latitude, state.y) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Used to create map file
wd<-getwd()
filename<-paste0(wd,"/ggmapTemp")
mapfile<-paste0(wd,"/map.rda")
#map<-get_map(location='united states', zoom=4, source="osm", color="bw", filename=filename)
#save(map, file=mapfile)

load(mapfile)

ggmap(map) +
  geom_point(aes(x=longitude, y=latitude, size=count, colour=state.y), data=maping, alpha=.7) +
  xlab("") + 
  ylab("") +
  theme_light(base_size = 16) +
  geom_text(data = maping,aes(x=longitude, y=latitude, label = count, size =400),check_overlap = TRUE,vjust=2)

# ------------------
# Structure the data for analysis
# ------------------

all_complaints_df<-data.frame(complaints=unlist(lapply(myCorpus, as.character)), stringsAsFactors=F)

# Split Complaint into words 
for (row in all_complaints_df) {
  splitrow<-strsplit(rm_white_multiple(row), " ")
}

# Run get_sentiment on each word in row
syuzhet_score <- lapply(splitrow, function(x) get_sentiment(x, method="syuzhet") )
bing_score<-lapply(splitrow, function(x) get_sentiment(x, method="bing") )
afinn_score <- lapply(splitrow, function(x) get_sentiment(x, method="afinn") )
nrc_score<-lapply(splitrow, function(x) get_sentiment(x, method="nrc") )

#Add all the scored up
syuzhet_score <- lapply(syuzhet_score, function(x) sum(sign(x)))
bing_score <- lapply(bing_score, function(x) sum(sign(x)))
afinn_score <- lapply(afinn_score, function(x) sum(sign(x)))
nrc_score <- lapply(nrc_score, function(x) sum(sign(x)))


all_scores<-cbind(
  syuzhet_score,
  bing_score,
  afinn_score,
  nrc_score
)

# Create sentiment scores dataframe
sentimentdf<-data.frame(all_complaints_df,all_scores)

# ------------------
# Use Logistic Regression Model
# ------------------

#Document Term Martix as a Dataframe
df1 <- data.frame(as.matrix(dtm), row.names = 1:nrow(dtm))

# Iterate over different sentiment scores to get the different models 
Model_syuzhet_score<-0
Model_bing_score<-0
Model_afinn_score<-0
Model_nrc_score<-0

scores<-c('syuzhet_score', 'bing_score','afinn_score','nrc_score')

for(score in scores)
{
  print(paste("Started", score, "Model"))
  c0<-sentimentdf[sentimentdf[score] > 0,]
  c0["sentiment"]<- 0
  
  c1<-sentimentdf[sentimentdf[score] < 0,]
  c1["sentiment"]<- 1
  
  # Take a random sampling of 200 rows
  data_200 <- rbind(c0[sample(nrow(c0), 100),c(1,6)],c1[sample(nrow(c1),100),c(1,6)])
  
  df1_c<-merge(df1,data_200, by="row.names", all=FALSE)
  df1_c$out_put_class <-factor(df1_c$sentiment)
  df1_c<- subset(df1_c, select = -c(Row.names, sentiment,complaints))
  
  ## Using logistic regression 
  formula = as.formula('out_put_class~.')
  weka_fit1 <- Logistic(formula, data = df1_c)
  assign(paste0("Weka_fit_", score), weka_fit1)
  assign(paste0("Model_", score), evaluate_Weka_classifier(weka_fit1, numFolds = 10, seed = 1, class = TRUE))
  print(paste("Finished", score, "Model"))
  
}

#Display the 10 fold Cross- Validation results
Model_syuzhet_score
Model_bing_score
Model_afinn_score
Model_nrc_score
