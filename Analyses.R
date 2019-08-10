library(tidyverse)
library(tidytext)
library(ggplot2)
library(quanteda)
library(quanteda.dictionaries)
library(textreadr)
library(stringr)
library(reshape)
### Analyses  (draft)

Tokenized_Short <-Tokenized_Short %>%
  anti_join(stop_words, by = c("terms" = "word")) #%>% View()


## Set LIWC dictionary - Quanteda
Dict_LIWC<-dictionary(file = "LIWC2015_English.dic", format = "LIWC")

# Transforming the data into Quanteda corpus

Quant_Corp<-corpus(Clean_texts, docid_field = "ID", text_field = "text")

summary(corpus_reshape(Quant_Corp, to = "paragraphs"))
Anal<-liwcalike(Quant_Corp, dictionary = Dict_LIWC)



