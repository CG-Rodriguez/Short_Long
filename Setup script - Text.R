library(tidyverse)
library(textreadr)
library(stringr)
library(stringi)
########### Short vs. Long Experiment - Condition: TEXT #########
###### Getting the data #####

Doc1<-read_docx(file = "./Raw Data/Long vs. Short - Original Short Text.docx")
Doc2<-read_docx(file = "./Raw Data/Long vs. Short - Original Long Text.docx", trim =  F)


### For some reason, the text from the .docx file of the Long/Text condition 
### cannot be extracted using the same algorithm as the others. Hence, the variations in this part of the code



Corpus1<-str_split(Doc1, pattern = ":")
Corpus2<-stri_split_fixed(Doc2,pattern = "Person")

Raw_text1<-data.frame( Pair = vector(mode = "character", length = length(Corpus1)),
                       Person = vector(mode = "character", length = length(Corpus1)),
                       text = vector(mode = "character", length = length(Corpus1)), stringsAsFactors = F)

Raw_text2<-data.frame( Pair = vector(mode = "character", length = sum(lengths(Corpus2))),
                       Person = vector(mode = "character", length = sum(lengths(Corpus2))),
                       text = vector(mode = "character", length = sum(lengths(Corpus2))), stringsAsFactors = F)

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
############# THIS PART NEEDS TO BE DIFFERENT ##################

UL2<-unlist(Corpus2)
DF2<-str_split(UL2, pattern = ":")

## loop to transfer the texts into a data frame ##2
for (i in 1:length(DF2)) {
  print(i) #sanity check
  if (length(DF2[[i]]) == 1){
    if(sum(str_detect(DF2[[i]][1],c("^Pair", "\\[Experimenter"))) == 1) {
      
        DF2[[i]][1]->Raw_text2[i,1]
    }
    else {
      paste(Raw_text2[i-1, 3], DF2[[i]][1], collapse = " ")->Raw_text2[i-1, 3]
    }
  }
    else {
      paste("Person", DF2[[i]][1], sep = "")->Raw_text2[i,2]
      paste(DF2[[i]][2:length(DF2[[i]])], collapse = " ")->Raw_text2[i,3]
      }
  }



## manual fix of glitches
Raw_text1[200,2]<- "Person A"
paste(Raw_text1[448,3], Raw_text1[449,1], sep = "")->Raw_text1[448,3]
Raw_text1[449,1]<-NA
paste(Raw_text1[496,3], Raw_text1[497,1], sep = "")->Raw_text1[496,3]
Raw_text1[497,1]<-NA
paste(Raw_text1[661,3], Raw_text1[662,1], sep = "")->Raw_text1[661,3]
Raw_text1[662,1]<-NA
paste(Raw_text1[683,3], Raw_text1[684,1], sep = "")->Raw_text1[683,3]
Raw_text1[684,1]<-NA

###### Cleaning the data ########
## logical vector detecting rows with the intervention of experimenter
stop1<-str_which(Raw_text1$Pair, pattern = "^\\[Experimenter ")
stop2<-str_which(Raw_text2$Pair, pattern = "^\\[Experimenter ")

## we drop those
Raw_text1 <-Raw_text1[-stop1,] 
Raw_text2 <-Raw_text2[-stop2,] 

####
index_pairs1<-str_split_fixed(Raw_text1$Pair, pattern = " ", 6) #%>% View()

index_pairs2<-str_split_fixed(Raw_text2$Pair, pattern = " ", 6) #%>% View()

for (i in 1:nrow(index_pairs1)){
  if (index_pairs1[i,1] == "") {
    print(i)
    index_pairs1[i,]<-index_pairs1[i-1,]
  }
  else {
    print(i)
  }
}
## merging and selecting the columns we need 

index_pairs1[,1]<-paste(index_pairs1[,1],index_pairs1[,2], sep = " ")
index_pairs1[,3]<-gsub("P", "", index_pairs1[,3])
index_pairs1[,5]<-gsub("P", "", index_pairs1[,5])


for (i in 1:nrow(index_pairs2)){
  if (index_pairs2[i,1] == "") {
    print(i)
    index_pairs2[i,]<-index_pairs2[i-1,]
  }
  else {
    print(i)
  }
}

index_pairs2[,1]<-paste(index_pairs2[,1],index_pairs2[,2], sep = " ")
index_pairs2[,3]<-gsub("P", "", index_pairs2[,3])
index_pairs2[,5]<-gsub("P", "", index_pairs2[,5])



###### Structuring the data ################
### Putting together pair information and texts 

Clean_texts1<-cbind(index_pairs1[,c(1,3,5)], Raw_text1[,2:3])
Clean_texts2<-cbind(index_pairs2[,c(1,3,5)], Raw_text2[,2:3])
# Dropping empty rows
Clean_texts1<-Clean_texts1[!is.na(Clean_texts1$text),]
Clean_texts2<-Clean_texts2[!is.na(Clean_texts2$text),]

names(Clean_texts1)[1:3]<-c("Pair", "PA", "PB")
names(Clean_texts2)[1:3]<-c("Pair", "PA", "PB")

Clean_texts1$PA<-as.character(Clean_texts1$PA)
Clean_texts1$PB<-as.character(Clean_texts1$PB)
Clean_texts2$PA<-as.character(Clean_texts2$PA)
Clean_texts2$PB<-as.character(Clean_texts2$PB)

# Relabeling Position and ID

Clean_texts1<- Clean_texts1 %>%
  mutate(Position = ifelse(Person == "Person A", "Support", "Oppose")) #%>% View()
Clean_texts1<- Clean_texts1 %>%
  mutate(ID = if_else(Position == "Support", PA, PB)) #%>% View()

Clean_texts2<- Clean_texts2 %>%
  mutate(Position = ifelse(Person == "Person A", "Support", "Oppose")) #%>% View()
Clean_texts2<- Clean_texts2 %>%
  mutate(ID = ifelse(Position == "Support", PA, PB)) #%>% View()


#Selecting the columns of interest
Clean_texts1<-Clean_texts1[,c(1,7,6,5)] #%>% View()
Clean_texts2<-Clean_texts2[,c(1,7,6,5)] #%>% View()

## loop Indexing the # of intervention##

Clean_texts1$Intervention<-vector(mode = "numeric", length = nrow(Clean_texts1))
Clean_texts1$Intervention[1]<-1
Clean_texts1$Intervention[2]<-1
Clean_texts1$Intervention[3]<-2

Clean_texts2$Intervention<-vector(mode = "numeric", length = nrow(Clean_texts2))
Clean_texts2$Intervention[1]<-1
Clean_texts2$Intervention[2]<-1
Clean_texts2$Intervention[3]<-2

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


## Further cleaning - interpolated comments ##
Clean_texts1$text<-gsub("\\[.\\]", " ", Clean_texts1$text) 
Clean_texts2$text<-gsub("\\[.\\]", " ", Clean_texts2$text) 

#### Different Data Structures #########
Clean_Sh_Tx<-data.frame(Clean_texts1, 
                        Medium = rep("Text", nrow(Clean_texts1)), 
                        Extension = rep("Short", nrow(Clean_texts1)), stringsAsFactors = F)
Clean_Ln_Tx<-data.frame(Clean_texts2,
                        Medium = rep("Text", nrow(Clean_texts2)), 
                        Extension = rep("Long", nrow(Clean_texts2)), stringsAsFactors = F)
## Tokenize the texts for frequency analyses #########

Tok_Sh_Tx<-unnest_tokens(Clean_texts1, input = text, output = terms ) 
Tok_Ln_Tx<-unnest_tokens(Clean_texts2, input = text, output = terms ) 

## Texts organized by participant ####
All_Ps1<-unique(Clean_texts1$ID)
By_Part_Sh_Tx<-data.frame(ID = All_Ps1,
                          Medium = rep("Text", length(All_Ps1)),
                          Extension = rep("Short", length(All_Ps1)),
                          text = vector(mode = "character", length = length(All_Ps1)), 
                          stringsAsFactors = F)
All_Ps2<-unique(Clean_texts2$ID)
By_Part_Ln_Tx<-data.frame(ID = All_Ps2,
                         Medium = rep("Text", length(All_Ps2)),
                         Extension = rep("Long", length(All_Ps2)),
                         text = vector(mode = "character", length = length(All_Ps2)), 
                         stringsAsFactors = F)


for(i in All_Ps1){
  index<-match(i, All_Ps1)
  By_Part_Sh_Tx$text[index]<-str_c(Clean_texts1$text[Clean_texts1$ID == i], collapse =" ")
}


for(i in All_Ps2){
  index<-match(i, All_Ps2)
  By_Part_Ln_Tx$text[index]<-str_c(Clean_texts2$text[Clean_texts2$ID == i], collapse =" ")
}


##  Texts organized by pair ####
All_Pairs1<-as.vector(unique(Clean_texts1$Pair))
Convo_Sh_Tx<-data.frame(ID = All_Pairs1,
                      Medium = rep("Text", length(All_Pairs1)),
                      Extension = rep("Short", length(All_Pairs1)),
                      text = vector(mode = "character",length = length(All_Pairs1)),
                      stringsAsFactors = F)

All_Pairs2<-as.vector(unique(Clean_texts2$Pair))
Convo_Ln_Tx<-data.frame(ID = All_Pairs2,
                      Medium = rep("Text", length(All_Pairs2)),
                      Extension = rep("Long", length(All_Pairs2)), 
                      text = vector(mode = "character", length = length(All_Pairs2)),
                      stringsAsFactors = F)


for(i in All_Pairs1){
  index<-match(i, All_Pairs1)
  Convo_Sh_Tx$text[index]<-str_c(Clean_texts1$text[Clean_texts1$Pair == i], collapse =" ")
}


for(i in All_Pairs2){
  index<-match(i, All_Pairs2)
  Convo_Ln_Tx$text[index]<-str_c(Clean_texts2$text[Clean_texts2$Pair == i], collapse =" ")
}