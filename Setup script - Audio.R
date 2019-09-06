<<<<<<< HEAD
library(tidyverse)
library(textreadr)
library(stringr)
########### Short vs. Long Experiment - Condition: AUDIO #########
###### Getting the data #####

Doc1<-read_docx(file = "./Raw Data/Cleaned Long vs. Short - Short Transcripts.docx")
Doc2<-read_docx(file = "./Raw Data/Cleaned Long vs. Short - Long Transcripts.docx")

Corpus1<-str_split(Doc1, pattern = ":")
Corpus2<-str_split(Doc2, pattern = ":")

Raw_text1<-data.frame( Pair = vector(mode = "character", length = length(Corpus1)),
                      Person = vector(mode = "character", length = length(Corpus1)),
                      text = vector(mode = "character", length = length(Corpus1)), stringsAsFactors = F)

Raw_text2<-data.frame( Pair = vector(mode = "character", length = length(Corpus2)),
                       Person = vector(mode = "character", length = length(Corpus2)),
                       text = vector(mode = "character", length = length(Corpus2)), stringsAsFactors = F)

## loop to transfer the texts into a data frame ##1
for (i in 1:length(Corpus1)) {
  print(i) #sanity check
  Working<-Corpus1[i]
  ifelse (is.na(Working[[1]][2]),
          Raw_text1[i,1]<-Working[[1]],
          Raw_text1[i,1]<-NA)
  ifelse (is.na(Working[[1]][2]),
          Raw_text1[i,2]<-NA,
          Raw_text1[i,2]<-Working[[1]][1])
  ifelse (is.na(Working[[1]][2]),
          Raw_text1[i,3]<-NA,
          Raw_text1[i,3]<-Working[[1]][2])
}

## loop to transfer the texts into a data frame ##2
for (i in 1:length(Corpus2)) {
  print(i) #sanity check
  Working<-Corpus2[i]
  ifelse (is.na(Working[[1]][2]),
          Raw_text2[i,1]<-Working[[1]],
          Raw_text2[i,1]<-NA)
  ifelse (is.na(Working[[1]][2]),
          Raw_text2[i,2]<-NA,
          Raw_text2[i,2]<-Working[[1]][1])
  ifelse (is.na(Working[[1]][2]),
          Raw_text2[i,3]<-NA,
          Raw_text2[i,3]<-Working[[1]][2])
}


## manual fix of glitches
Raw_text1[125,3]<-str_c(c(Raw_text1[125,3],Raw_text1[126,1]),  collapse = " ")
Raw_text1[885,3]<-str_c(c(Raw_text1[885,3],Raw_text1[886,1]),  collapse = " ")
Raw_text1[995,3]<-Raw_text1[995,1]
Raw_text1[995,2]<-"Person B"
Raw_text1[995,1]<-NA
Raw_text1[1181,3]<-str_c(c(Raw_text1[1181,3],Raw_text1[1182,1]),  collapse = " ")

Raw_text1[-c(126,886, 1182),]-> Raw_text1


###### Cleaning the data ########
## logical vector detecting rows with the intervention of experimenter
stop1<-str_which(Raw_text1$Pair, pattern = "^\\[Experimenter ")
stop2<-str_which(Raw_text2$Pair, pattern = "^\\[Experimenter ")
        
## we drop those
Raw_text1 <-Raw_text1[-stop1,] 
Raw_text2 <-Raw_text2[-stop2,] 

####
index_pairs1<-str_split_fixed(Raw_text1$Pair, pattern = ",", 4) #%>% View()
index_pairs2<-str_split_fixed(Raw_text2$Pair, pattern = ",", 4) #%>% View()

for (i in 1:nrow(index_pairs1)){
  if (index_pairs1[i,1] == "") {
    print(i)
    index_pairs1[i,]<-index_pairs1[i-1,]
  }
  else {
    print(i)
    }
}

for (i in 1:nrow(index_pairs2)){
  if (index_pairs2[i,1] == "") {
    print(i)
    index_pairs2[i,]<-index_pairs2[i-1,]
  }
  else {
    print(i)
  }
}


###### Structuring the data ################
### Putting together pair information and texts 

Clean_texts1<-cbind(index_pairs1[,1:3], Raw_text1[,2:3])
Clean_texts2<-cbind(index_pairs2[,1:3], Raw_text2[,2:3])
# Dropping empty rows
Clean_texts1<-Clean_texts1[!is.na(Clean_texts1$text),]
Clean_texts2<-Clean_texts2[!is.na(Clean_texts2$text),]
# Relabeling Position and ID

Clean_texts1<- Clean_texts1 %>%
  mutate(Position = ifelse(Person == "Person A", "Support", "Oppose")) #%>% View()
Clean_texts1[,7:8]<-str_split_fixed(Clean_texts1[,2], 2, pattern = " P") 
Clean_texts1[,9:10]<-str_split_fixed(Clean_texts1[,3], 2, pattern = " P")
Clean_texts1<- Clean_texts1 %>%
  mutate(ID = ifelse(Position == "Support", V8, V10)) #%>% View()

Clean_texts2<- Clean_texts2 %>%
  mutate(Position = ifelse(Person == "Person A", "Support", "Oppose")) #%>% View()
Clean_texts2[,7:8]<-str_split_fixed(Clean_texts2[,2], 2, pattern = " P") 
Clean_texts2[,9:10]<-str_split_fixed(Clean_texts2[,3], 2, pattern = " P")
Clean_texts2<- Clean_texts2 %>%
  mutate(ID = ifelse(Position == "Support", V8, V10)) #%>% View()


#Selecting the columns of interest
Clean_texts1<-Clean_texts1[,c(1,6,11,5)] #%>% View()
Clean_texts2<-Clean_texts2[,c(1,6,11,5)] #%>% View()

## loop Indexing the # of intervention##

Clean_texts1$Intervention<-vector(mode = "numeric", length = nrow(Clean_texts1))
Clean_texts1$Intervention[1]<-1
Clean_texts1$Intervention[2]<-1
Clean_texts1$Intervention[3]<-2
names(Clean_texts1)[1]<-"Pair"

for (i in 4:nrow(Clean_texts1)){
  if (Clean_texts1$Pair[i] == Clean_texts1$Pair[i-1]){
    
    ifelse(Clean_texts1$Intervention[i-1]==Clean_texts1$Intervention[i-2], 
      Clean_texts1$Intervention[i]<-1+Clean_texts1$Intervention[i-2], 
      Clean_texts1$Intervention[i]<-Clean_texts1$Intervention[i-1])
    }
    else 
      {
      Clean_texts1$Intervention[i]<-1
    }
  }


Clean_texts2$Intervention<-vector(mode = "numeric", length = nrow(Clean_texts2))
Clean_texts2$Intervention[1]<-1
Clean_texts2$Intervention[2]<-1
Clean_texts2$Intervention[3]<-2
names(Clean_texts2)[1]<-"Pair"

for (i in 4:nrow(Clean_texts2)){
  if (Clean_texts2$Pair[i] == Clean_texts2$Pair[i-1]){
    
    ifelse(Clean_texts2$Intervention[i-1]==Clean_texts2$Intervention[i-2], 
           Clean_texts2$Intervention[i]<-1+Clean_texts2$Intervention[i-2], 
           Clean_texts2$Intervention[i]<-Clean_texts2$Intervention[i-1])
  }
  else 
  {
    Clean_texts2$Intervention[i]<-1
  }
}


## Further cleaning ##
Clean_texts1$text<-gsub("\\[name redacted\\]", " ", Clean_texts1$text) 
Clean_texts2$text<-gsub("\\[name redacted\\]", " ", Clean_texts2$text) 

####  Different Data Structures #########
Clean_Sh_Au<-data.frame(Clean_texts1, 
                        Medium = rep("Audio", nrow(Clean_texts1)), 
                        Extension = rep("Short", nrow(Clean_texts1)), stringsAsFactors = F)
Clean_Ln_Au<-data.frame(Clean_texts2,
                        Medium = rep("Audio", nrow(Clean_texts2)), 
                        Extension = rep("Long", nrow(Clean_texts2)), stringsAsFactors = F)

##  Tokenize the texts for frequency analyses #########

Tok_Sh_Au<-unnest_tokens(Clean_texts1, input = text, output = terms ) 
Tok_Ln_Au<-unnest_tokens(Clean_texts2, input = text, output = terms ) 

##  Texts organized by participant ####
All_Ps1<-unique(Clean_texts1$ID)
By_Part_Sh_Au<-data.frame(ID = All_Ps1,
                          Medium = rep("Audio", length(All_Ps1)),
                          Extension = rep("Short", length(All_Ps1)),
                          text = vector(mode = "character", length = length(All_Ps1)), 
                          stringsAsFactors = F)
                 
All_Ps2<-unique(Clean_texts2$ID)
By_Part_Ln_Au<-data.frame(ID = All_Ps2,
                          Medium = rep("Audio", length(All_Ps2)),
                          Extension = rep("Long", length(All_Ps2)),
                          text = vector(mode = "character", length = length(All_Ps2)), 
                          stringsAsFactors = F)


for(i in All_Ps1){
  index<-match(i, All_Ps1)
  By_Part_Sh_Au$text[index]<-str_c(Clean_texts1$text[Clean_texts1$ID == i], collapse =" ")
}


for(i in All_Ps2){
  index<-match(i, All_Ps2)
  By_Part_Ln_Au$text[index]<-str_c(Clean_texts2$text[Clean_texts2$ID == i], collapse =" ")
}


##  Texts organized by pair ####
All_Pairs1<-as.vector(unique(Clean_texts1$Pair))
Convo_Sh_Au<-data.frame(ID = All_Pairs1,
                        Medium = rep("Audio", length(All_Pairs1)),
                        Extension = rep("Short", length(All_Pairs1)),   
                        text = vector(mode = "character", length = length(All_Pairs1)), 
                        stringsAsFactors = F)

All_Pairs2<-as.vector(unique(Clean_texts2$Pair))
Convo_Ln_Au<-data.frame(ID = unique(Clean_texts2$Pair),
                        Medium = rep("Audio", length(All_Pairs2)),
                        Extension = rep("Long", length(All_Pairs2)),   
                        text = vector(mode = "character", length = length(All_Pairs2)), 
                        stringsAsFactors = F)


for(i in All_Pairs1){
  index<-match(i, All_Pairs1)
  Convo_Sh_Au$text[index]<-str_c(Clean_texts1$text[Clean_texts1$Pair == i], collapse =" ")
}


for(i in All_Pairs2){
  index<-match(i, All_Pairs2)
  Convo_Ln_Au$text[index]<-str_c(Clean_texts2$text[Clean_texts2$Pair == i], collapse =" ")
=======
library(tidyverse)
library(textreadr)
library(stringr)
########### Short vs. Long Experiment - Condition: AUDIO #########
###### Getting the data #####

Doc1<-read_docx(file = "./Raw Data/Cleaned Long vs. Short - Short Transcripts.docx")
Doc2<-read_docx(file = "./Raw Data/Cleaned Long vs. Short - Long Transcripts.docx")

Corpus1<-str_split(Doc1, pattern = ":")
Corpus2<-str_split(Doc2, pattern = ":")

Raw_text1<-data.frame( Pair = vector(mode = "character", length = length(Corpus1)),
                      Person = vector(mode = "character", length = length(Corpus1)),
                      text = vector(mode = "character", length = length(Corpus1)), stringsAsFactors = F)

Raw_text2<-data.frame( Pair = vector(mode = "character", length = length(Corpus2)),
                       Person = vector(mode = "character", length = length(Corpus2)),
                       text = vector(mode = "character", length = length(Corpus2)), stringsAsFactors = F)

## loop to transfer the texts into a data frame ##1
for (i in 1:length(Corpus1)) {
  print(i) #sanity check
  Working<-Corpus1[i]
  ifelse (is.na(Working[[1]][2]),
          Raw_text1[i,1]<-Working[[1]],
          Raw_text1[i,1]<-NA)
  ifelse (is.na(Working[[1]][2]),
          Raw_text1[i,2]<-NA,
          Raw_text1[i,2]<-Working[[1]][1])
  ifelse (is.na(Working[[1]][2]),
          Raw_text1[i,3]<-NA,
          Raw_text1[i,3]<-Working[[1]][2])
}

## loop to transfer the texts into a data frame ##2
for (i in 1:length(Corpus2)) {
  print(i) #sanity check
  Working<-Corpus2[i]
  ifelse (is.na(Working[[1]][2]),
          Raw_text2[i,1]<-Working[[1]],
          Raw_text2[i,1]<-NA)
  ifelse (is.na(Working[[1]][2]),
          Raw_text2[i,2]<-NA,
          Raw_text2[i,2]<-Working[[1]][1])
  ifelse (is.na(Working[[1]][2]),
          Raw_text2[i,3]<-NA,
          Raw_text2[i,3]<-Working[[1]][2])
}


## manual fix of glitches
Raw_text1[125,3]<-str_c(c(Raw_text1[125,3],Raw_text1[126,1]),  collapse = " ")
Raw_text1[885,3]<-str_c(c(Raw_text1[885,3],Raw_text1[886,1]),  collapse = " ")
Raw_text1[995,3]<-Raw_text1[995,1]
Raw_text1[995,2]<-"Person B"
Raw_text1[995,1]<-NA
Raw_text1[1181,3]<-str_c(c(Raw_text1[1181,3],Raw_text1[1182,1]),  collapse = " ")

Raw_text1[-c(126,886, 1182),]-> Raw_text1


###### Cleaning the data ########
## logical vector detecting rows with the intervention of experimenter
stop1<-str_which(Raw_text1$Pair, pattern = "^\\[Experimenter ")
stop2<-str_which(Raw_text2$Pair, pattern = "^\\[Experimenter ")
        
## we drop those
Raw_text1 <-Raw_text1[-stop1,] 
Raw_text2 <-Raw_text2[-stop2,] 

####
index_pairs1<-str_split_fixed(Raw_text1$Pair, pattern = ",", 4) #%>% View()
index_pairs2<-str_split_fixed(Raw_text2$Pair, pattern = ",", 4) #%>% View()

for (i in 1:nrow(index_pairs1)){
  if (index_pairs1[i,1] == "") {
    print(i)
    index_pairs1[i,]<-index_pairs1[i-1,]
  }
  else {
    print(i)
    }
}

for (i in 1:nrow(index_pairs2)){
  if (index_pairs2[i,1] == "") {
    print(i)
    index_pairs2[i,]<-index_pairs2[i-1,]
  }
  else {
    print(i)
  }
}


###### Structuring the data ################
### Putting together pair information and texts 

Clean_texts1<-cbind(index_pairs1[,1:3], Raw_text1[,2:3])
Clean_texts2<-cbind(index_pairs2[,1:3], Raw_text2[,2:3])
# Dropping empty rows
Clean_texts1<-Clean_texts1[!is.na(Clean_texts1$text),]
Clean_texts2<-Clean_texts2[!is.na(Clean_texts2$text),]
# Relabeling Position and ID

Clean_texts1<- Clean_texts1 %>%
  mutate(Position = ifelse(Person == "Person A", "Support", "Oppose")) #%>% View()
Clean_texts1[,7:8]<-str_split_fixed(Clean_texts1[,2], 2, pattern = " P") 
Clean_texts1[,9:10]<-str_split_fixed(Clean_texts1[,3], 2, pattern = " P")
Clean_texts1<- Clean_texts1 %>%
  mutate(ID = ifelse(Position == "Support", V8, V10)) #%>% View()

Clean_texts2<- Clean_texts2 %>%
  mutate(Position = ifelse(Person == "Person A", "Support", "Oppose")) #%>% View()
Clean_texts2[,7:8]<-str_split_fixed(Clean_texts2[,2], 2, pattern = " P") 
Clean_texts2[,9:10]<-str_split_fixed(Clean_texts2[,3], 2, pattern = " P")
Clean_texts2<- Clean_texts2 %>%
  mutate(ID = ifelse(Position == "Support", V8, V10)) #%>% View()


#Selecting the columns of interest
Clean_texts1<-Clean_texts1[,c(1,6,11,5)] #%>% View()
Clean_texts2<-Clean_texts2[,c(1,6,11,5)] #%>% View()

## loop Indexing the # of intervention##

Clean_texts1$Intervention<-vector(mode = "numeric", length = nrow(Clean_texts1))
Clean_texts1$Intervention[1]<-1
Clean_texts1$Intervention[2]<-1
Clean_texts1$Intervention[3]<-2
names(Clean_texts1)[1]<-"Pair"

for (i in 4:nrow(Clean_texts1)){
  if (Clean_texts1$Pair[i] == Clean_texts1$Pair[i-1]){
    
    ifelse(Clean_texts1$Intervention[i-1]==Clean_texts1$Intervention[i-2], 
      Clean_texts1$Intervention[i]<-1+Clean_texts1$Intervention[i-2], 
      Clean_texts1$Intervention[i]<-Clean_texts1$Intervention[i-1])
    }
    else 
      {
      Clean_texts1$Intervention[i]<-1
    }
  }


Clean_texts2$Intervention<-vector(mode = "numeric", length = nrow(Clean_texts2))
Clean_texts2$Intervention[1]<-1
Clean_texts2$Intervention[2]<-1
Clean_texts2$Intervention[3]<-2
names(Clean_texts2)[1]<-"Pair"

for (i in 4:nrow(Clean_texts2)){
  if (Clean_texts2$Pair[i] == Clean_texts2$Pair[i-1]){
    
    ifelse(Clean_texts2$Intervention[i-1]==Clean_texts2$Intervention[i-2], 
           Clean_texts2$Intervention[i]<-1+Clean_texts2$Intervention[i-2], 
           Clean_texts2$Intervention[i]<-Clean_texts2$Intervention[i-1])
  }
  else 
  {
    Clean_texts2$Intervention[i]<-1
  }
}


## Further cleaning ##
Clean_texts1$text<-gsub("\\[name redacted\\]", " ", Clean_texts1$text) 
Clean_texts2$text<-gsub("\\[name redacted\\]", " ", Clean_texts2$text) 

####  Different Data Structures #########
Clean_Sh_Au<-data.frame(Clean_texts1, 
                        Medium = rep("Audio", nrow(Clean_texts1)), 
                        Extension = rep("Short", nrow(Clean_texts1)), stringsAsFactors = F)
Clean_Ln_Au<-data.frame(Clean_texts2,
                        Medium = rep("Audio", nrow(Clean_texts2)), 
                        Extension = rep("Long", nrow(Clean_texts2)), stringsAsFactors = F)

##  Tokenize the texts for frequency analyses #########

Tok_Sh_Au<-unnest_tokens(Clean_texts1, input = text, output = terms ) 
Tok_Ln_Au<-unnest_tokens(Clean_texts2, input = text, output = terms ) 

##  Texts organized by participant ####
All_Ps1<-unique(Clean_texts1$ID)
By_Part_Sh_Au<-data.frame(ID = All_Ps1,
                          Medium = rep("Audio", length(All_Ps1)),
                          Extension = rep("Short", length(All_Ps1)),
                          text = vector(mode = "character", length = length(All_Ps1)), 
                          stringsAsFactors = F)
                 
All_Ps2<-unique(Clean_texts2$ID)
By_Part_Ln_Au<-data.frame(ID = All_Ps2,
                          Medium = rep("Audio", length(All_Ps2)),
                          Extension = rep("Long", length(All_Ps2)),
                          text = vector(mode = "character", length = length(All_Ps2)), 
                          stringsAsFactors = F)


for(i in All_Ps1){
  index<-match(i, All_Ps1)
  By_Part_Sh_Au$text[index]<-str_c(Clean_texts1$text[Clean_texts1$ID == i], collapse =" ")
}


for(i in All_Ps2){
  index<-match(i, All_Ps2)
  By_Part_Ln_Au$text[index]<-str_c(Clean_texts2$text[Clean_texts2$ID == i], collapse =" ")
}


##  Texts organized by pair ####
All_Pairs1<-as.vector(unique(Clean_texts1$Pair))
Convo_Sh_Au<-data.frame(ID = All_Pairs1,
                        Medium = rep("Audio", length(All_Pairs1)),
                        Extension = rep("Short", length(All_Pairs1)),   
                        text = vector(mode = "character", length = length(All_Pairs1)), 
                        stringsAsFactors = F)

All_Pairs2<-as.vector(unique(Clean_texts2$Pair))
Convo_Ln_Au<-data.frame(ID = unique(Clean_texts2$Pair),
                        Medium = rep("Audio", length(All_Pairs2)),
                        Extension = rep("Long", length(All_Pairs2)),   
                        text = vector(mode = "character", length = length(All_Pairs2)), 
                        stringsAsFactors = F)


for(i in All_Pairs1){
  index<-match(i, All_Pairs1)
  Convo_Sh_Au$text[index]<-str_c(Clean_texts1$text[Clean_texts1$Pair == i], collapse =" ")
}


for(i in All_Pairs2){
  index<-match(i, All_Pairs2)
  Convo_Ln_Au$text[index]<-str_c(Clean_texts2$text[Clean_texts2$Pair == i], collapse =" ")
>>>>>>> 6eddc7b7a6219867d7a5dbf408dd7f0526475f48
}