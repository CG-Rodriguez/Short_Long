library(tidyverse)
library(tidytext)
library(ggplot2)
library(quanteda)
library(textreadr)
library(stringr)

#### Getting the data #####

Doc<-read_docx(file = "Cleaned Long vs. Short - Short Transcripts.docx")

Corpus<-str_split(Doc, pattern = ":")

Raw_text<-data.frame( Pair = vector(mode = "character", length = 1721),
                      Person = vector(mode = "character", length = 1721),
                      text = vector(mode = "character", length = 1721), stringsAsFactors = F)


## loop to transfer the texts into a data frame
for (i in 1:1721) {
  print(i) #sanity check
  Working<-Corpus[i]
  ifelse (is.na(Working[[1]][2]),
          Raw_text[i,1]<-Working[[1]],
          Raw_text[i,1]<-NA)
  ifelse (is.na(Working[[1]][2]),
          Raw_text[i,2]<-NA,
          Raw_text[i,2]<-Working[[1]][1])
  ifelse (is.na(Working[[1]][2]),
          Raw_text[i,3]<-NA,
          Raw_text[i,3]<-Working[[1]][2])
}

## manual fix of glitches
Raw_text[125,3]<-str_c(c(Raw_text[125,3],Raw_text[126,1]),  collapse = " ")
Raw_text[885,3]<-str_c(c(Raw_text[885,3],Raw_text[886,1]),  collapse = " ")
Raw_text[995,3]<-Raw_text[995,3]
Raw_text[995,2]<-"Person B"
Raw_text[995,1]<-NA
Raw_text[1181,3]<-str_c(c(Raw_text[1181,3],Raw_text[1182,1]),  collapse = " ")

Raw_text[-c(126,886, 1182),]-> Raw_text


###### Cleaning the data ########
## logical vector detecting rows with the intervention of experimenter
stop<-str_which(Raw_text$Pair, pattern = "^\\[Experimenter ")
        
## we drop those
Raw_text <-Raw_text[-stop,] 

####
index_pairs<-str_split_fixed(Raw_text$Pair, pattern = ",", 4) #%>% View()

for (i in 1:nrow(index_pairs)){
  if (index_pairs[i,1] == "") {
    print(i)
    index_pairs[i,]<-index_pairs[i-1,]
  }
  else {
    print(i)
    }
}

#########Structuring the data ################
### Putting together pair information and texts 

Clean_texts<-cbind(index_pairs[,1:3], Raw_text[,2:3])
# Dropping empty rows
Clean_texts<-Clean_texts[!is.na(Clean_texts$text),]
# Relabeling Position and ID

Clean_texts<- Clean_texts %>%
  mutate(Position = ifelse(Person == "Person A", "Support", "Oppose")) #%>% View()
Clean_texts[,7:8]<-str_split_fixed(Clean_texts[,2], 2, pattern = " P") 
Clean_texts[,9:10]<-str_split_fixed(Clean_texts[,3], 2, pattern = " P")

Clean_texts<- Clean_texts %>%
  mutate(ID = ifelse(Position == "Support", V8, V10)) #%>% View()
#Selecting the columns of interest
Clean_texts<-Clean_texts[,c(1,6,11,5)] #%>% View()

## loop Indexing the # of intervention##

Clean_texts$Intervention<-vector(mode = "numeric", length = nrow(Clean_texts))
Clean_texts$Intervention[1]<-1
Clean_texts$Intervention[2]<-1
Clean_texts$Intervention[3]<-2
names(Clean_texts)[1]<-"Pair"

for (i in 4:nrow(Clean_texts)){
  if (Clean_texts$Pair[i] == Clean_texts$Pair[i-1]){
    
    ifelse(Clean_texts$Intervention[i-1]==Clean_texts$Intervention[i-2], 
      Clean_texts$Intervention[i]<-1+Clean_texts$Intervention[i-2], 
      Clean_texts$Intervention[i]<-Clean_texts$Intervention[i-1])
    }
    else 
      {
      Clean_texts$Intervention[i]<-1
    }
  }

## Further cleaning ##
Clean_texts$text<-gsub("\\[name redacted\\]", " ", Clean_texts$text) 

############## Different Data Structures #########

### Tokenize the texts for frequency analyses #########

Tokenized_Short<-unnest_tokens(Clean_texts, input = text, output = terms ) 


### Texts organized by participant ####
All_Ps<-unique(Clean_texts$ID)
By_Participant<-data.frame(ID = unique(Clean_texts$ID),
                 text = vector(mode = "character", length = length(unique(Clean_texts$ID))), 
                 stringsAsFactors = F)

for(i in All_Ps){
  index<-match(i, All_Ps)
  By_Participant$text[index]<-str_c(Clean_texts$text[Clean_texts$ID == i], collapse =" ")
}

###  Texts organized by pair ####
All_Pairs<-as.vector(unique(Clean_texts$Pair))
By_Convo<-data.frame(ID = unique(Clean_texts$Pair),
                           text = vector(mode = "character", length = length(unique(Clean_texts$Pair))), 
                           stringsAsFactors = F)

for(i in All_Pairs){
  index<-match(i, All_Pairs)
  By_Convo$text[index]<-str_c(Clean_texts$text[Clean_texts$Pair == i], collapse =" ")
}