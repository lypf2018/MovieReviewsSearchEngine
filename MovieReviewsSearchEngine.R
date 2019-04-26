require(tm)

df <- read.table("dataset/plot_summaries.txt", header = FALSE, sep="\t")
df_title <- data.frame(doc_id = df[,1], text = df[,2])
corpus <- VCorpus(DataframeSource(df_title))
inspect(corpus)
