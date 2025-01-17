# require(SparkR)
require(tidytext)
require(dplyr)
require(tm)
# require(SnowballC)

# Creating a dataframe of movie_names from the file
movie_names <- read.delim("https://raw.githubusercontent.com/lypf2018/MovieReviewsSearchEngine/master/dataset/movie.metadata.tsv", header = FALSE)
df_movie_names_title <- data.frame(doc_id = movie_names[,1], movie_name = movie_names[,3])

# Creating a corpus from the file
df <- read.delim("https://raw.githubusercontent.com/lypf2018/MovieReviewsSearchEngine/master/dataset/plot_summaries.txt", header = FALSE, sep="\t")
df_title <- data.frame(doc_id = df[,1], text = df[,2])
df_merged <- merge(df_title,df_movie_names_title, by="doc_id")
df_merged <- df_merged %>% 
  mutate(rowIndex=as.numeric(row.names(.))) %>% 
  select(text, doc_id, movie_name, rowIndex)

# Transforming documents into a list
docList <- as.list(df_merged$text)
N.docs <- length(docList)

# store docs in Corpus class which is a fundamental data structure in text mining
my.docs <- VectorSource(docList)


# Pre-processing
# 1.Removing stop words
# 2.Word stemming
# Transform/standaridze docs to get ready for analysis
# 3.Saving pre-processed data for faster processing next time
my.docsCorpus <- VCorpus(my.docs) %>% 
  tm_map(stemDocument) %>%
  tm_map(removeNumbers) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeWords,stopwords("en")) %>%
  tm_map(stripWhitespace)

QrySearch <- function(queryTerm) {
  
  # Record starting time to measure your search engine performance
  start.time <- Sys.time()
  queryList <- as.list(factor(queryTerm))

  # store docs in Corpus class which is a fundamental data structure in text mining
  my.query <- VectorSource(queryList)
  
  # Pre-processing
  # 1.Removing stop words
  # 2.Word stemming
  # Transform/standaridze docs to get ready for analysis
  my.queryCorpus <- VCorpus(my.query) %>% 
    tm_map(stemDocument) %>%
    tm_map(removeNumbers) %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeWords,stopwords("en")) %>%
    tm_map(stripWhitespace)

  my.queryCorpus[[1]]$meta$id <- N.docs + 1
  my.corpus<- c(my.docsCorpus, my.queryCorpus)

  # Store docs into a term document matrix where rows=terms and cols=docs
  # Normalize term counts by applying TDiDF weightings
  term.doc.matrix.stm <- TermDocumentMatrix(my.corpus,
                                            control=list(
                                              weighting=function(x) weightTfIdf(x, normalize = FALSE),
                                              stopwords = TRUE))

  # Transform term document matrix into a dataframe
  term.doc.matrix <- tidy(term.doc.matrix.stm) %>% 
    group_by(document) %>% 
    mutate(vtrLen=sqrt(sum(count^2))) %>% 
    mutate(count=count/vtrLen) %>% 
    ungroup() %>% 
    select(term:count)
  docMatrix <- term.doc.matrix %>% 
    mutate(document=as.numeric(document)) %>% 
    filter(document<N.docs+1)
  qryMatrix <- term.doc.matrix %>% 
    mutate(document=as.numeric(document)) %>% 
    filter(document>=N.docs+1)

  # Calcualte top ten results by cosine similarity
  searchRes <- docMatrix %>% 
    inner_join(qryMatrix,by=c("term"="term"),
               suffix=c(".doc",".query")) %>% 
    mutate(termScore=round(count.doc*count.query,4)) %>% 
    group_by(document.query,document.doc) %>% 
    summarise(Score=sum(termScore)) %>% 
    filter(row_number(desc(Score))<=10) %>% 
    arrange(desc(Score)) %>% 
    left_join(df_merged,by=c("document.doc"="rowIndex")) %>% 
    ungroup() %>% 
    rename(Result=text) %>% 
    select(Result,Score,doc_id,movie_name) %>% 
    data.frame()
  
  # Record when it stops and take the difference
  end.time <- Sys.time()
  time.taken <- round(end.time - start.time,4)
  print(paste("Used",time.taken,"seconds"))
  
  return(searchRes)
  
}

# 4.Reading query phrases
keyWords <- readline(prompt = "Please input key word(s):")

# 5.Showing the top 10 documents matching the user’s queries, quit with “q” key
while(keyWords != "q") {
  top10_doc <- QrySearch(keyWords)
  print(top10_doc)
  keyWords <- readline(prompt = "Please input key word(s):")
}
