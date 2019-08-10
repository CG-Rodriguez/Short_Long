################################################################
######################## DATA STRUCTURE ########################
################################################################
################## This is what the data looks like ############
################## after running "Setup script - Audio.R" ######
##################         "Setup script - Text.R"##############
################################################################

## Clean_Ln(Sh)_Tx(Au): clean dialogues, each row is an intervention. 
### Labels: Pair ID, Position, Participant ID, Intervention per participant (not in dialogue)
head(Clean_Ln_Tx)
#
## Tok_Ln(Sh)_Tx(Au): tokenized corpus, each row is a term (attention: including stopwords!)
### Labels: Pair ID, Position, Participant ID, Intervention per participant (not in dialogue)
head(Tok_Ln_Tx, 25)
#
## By_Part_Ln(Sh)_Tx(Au): individualized texts, each row n is all that participant n said
### Labels: Participant ID
head(By_Part_Ln_Tx,3)
#
## Convo_Ln(Sh)_Tx(Au): paired dialogues, each row is the whole text of the conversation
### Labels: Pair ID
head(Convo_Ln_Tx)
#
################################################################
################################################################

#####  Compiling all the data in one dataset ###################  

library(readxl)

## Retrieving participants quantitative data
Full_Dataset<-read_xlsx(".\\Raw Data\\political topics pairs short vs long 2.21.19.xlsx")
## Combining texts of all conditions
All_Ps_Ever<-rbind(By_Part_Sh_Au, By_Part_Ln_Au, By_Part_Sh_Tx, By_Part_Ln_Tx)

## Detecting inconsistencies: participants with data but no text (n = 76)
PID<-Full_Dataset$`Participant Number`
PID_T<-All_Ps_Ever$ID
## Excluding textless participants
Trimmed_Dataset<-Full_Dataset[which(PID  %in% PID_T),]

Trimmed_Dataset<-Trimmed_Dataset %>%
  arrange(`Participant Number`) 
All_Ps_Ever<-All_Ps_Ever %>%
  arrange(ID) 

## Manual Fix
# (Run AUDIO script to extract Clean_texts2 and modify)
Clean_texts2[1536:1583,1]<- "Pair 104"
Clean_texts2[1597:1607,1]<- "Pair 110"

Clean_texts2[1536:1583,3]<- c("226","229")
Clean_texts2$ID[1597:1604]<- c("252","237")
Clean_texts2[1605,3]<-"252"
Clean_texts2[1606:1607,3]<- c("237", "252")  
  
Trimmed_Dataset<-Trimmed_Dataset[-251,]

### Creating the huge dataset

Huge<-data.frame(ID1 = All_Ps_Ever$ID, 
                 ID2 = Trimmed_Dataset$`Participant Number`, 
                 Medium = All_Ps_Ever[,2], 
                 Extension = All_Ps_Ever[,3],
                 Pair = Trimmed_Dataset$`Pair number continuous`, 
                 Topic = Trimmed_Dataset$`Topic Talked about`,
                 Text = All_Ps_Ever$text, 
                 Dehum =  Trimmed_Dataset$mind,
                 Conflict =  Trimmed_Dataset$conflict...116, 
                 Respons = Trimmed_Dataset$responsiveness,
                 Pre_Speakers = Trimmed_Dataset$speaker_support_pre, 
                 Pre_Reparations = Trimmed_Dataset$reparations_support_pre,
                 Pre_Drugs = Trimmed_Dataset$drugs_support_pre, 
                 Post_Speakers = Trimmed_Dataset$Current_attitude_speakers,
                 Post_Reparation = Trimmed_Dataset$Current_attitude_speakers, 
                 Post_Drugs = Trimmed_Dataset$Current_attitude_drugs,
                 Agreed = Trimmed_Dataset$Overall_agreement, 
                 Att_Change =  Trimmed_Dataset$end_beg,
                 Att_Begin =  Trimmed_Dataset$begin_opinion, 
                 Att_End = Trimmed_Dataset$end_opinion,
                 Str_Speakers = Trimmed_Dataset$speaker_strength, 
                 Str_Reparations = Trimmed_Dataset$reparations_strength,
                 Str_Drugs = Trimmed_Dataset$drugs_strength, 
                 stringsAsFactors = FALSE)

## Embelishing the huge dataset ##

Huge<-Huge %>%
  mutate (Topic =if_else(Topic == "1", "Speakers",
                  if_else(Topic == "2", "Reparations", "Drugs"))) #%>% View()

Huge <- Huge %>%
  mutate(Pre_Att = if_else(Topic == "Speakers", Pre_Speakers, 
                           if_else(Topic == "Reparations", Pre_Reparations, Pre_Drugs))) %>%
  mutate(Post_Att = if_else(Topic == "Speakers", Post_Speakers, 
                           if_else(Topic == "Reparations", Post_Reparation, Post_Drugs))) %>%
  mutate(Real_Change = abs(Post_Att - Pre_Att)) 

## Alter person ratings
Alter_rating<-data.frame(alt_Dehum = vector(mode = "numeric", length = nrow(Huge)),
                          alt_Conf = vector(mode = "numeric", length = nrow(Huge)),
                          alt_Resp = vector(mode = "numeric", length = nrow(Huge)))
# loop
for (i in 1:nrow(Huge)) {
  Huge$Pair[i]->ind_pair
  which(Huge$Pair==ind_pair)->both
  both[which(both !=i)]->alter
  Huge[alter,8:10]->Alter_rating[i,1:3]
  print(i)
  print(alter)
}

Huge<-cbind(Huge, Alter_rating, stringAsFactors = FALSE)
##### First NLP analyses ############
library(quanteda)
library(quanteda.dictionaries)
library(reshape)
library(tidytext)
Huge$Text <- str_replace(Huge$Text, "\\[name redacted\\]", "")

### LIWC by Conditions ###

LIWC_dic <-dictionary(file = "LIWC2015_English.dic")
Rslts<-liwcalike(Huge$Text, LIWC_dic)
Huge$Conflict<-as.numeric(Huge$Conflict)
Huge$Respons<-as.numeric(Huge$Respons)
Huge$alt_Conf<-as.numeric(Huge$alt_Conf)
Huge$alt_Resp<-as.numeric(Huge$alt_Resp)
Output<-data.frame(Huge[,c(3:6,8:10, 16, 17, 26,29:31)], Rslts)
Output$Agreed[Output$Agreed == 1]<-0
Output$Agreed[Output$Agreed == 2]<-1

save(Output, file = "Output.Rdata")
save(Huge, file = "Huge.Rdata")
#### Descriptive Tables #####
library(sjPlot)
library(stargazer)
library(summarytools)
library(papaja)
## All word categories by Medium

Output[,c(2,6:8, 12:14, 21, 42, 48, 53, 60, 64, 69, 78, 88)] %>%
  group_by(Medium) %>%
  summarize(Dehum = mean(alt_Dehum), Conflict = mean(alt_Conf), Responsiveness = mean(alt_Resp), Function = mean(function..function.words.), 
            Affective = mean(affect..affect.), Social = mean(social..social.), Cognitive = mean(cogproc..cognitive.processes.),
            Perceptual = mean(percept..perceptual.processes.), 
            Biological = mean(bio..biological.processes.), 
            Drives = mean(drives..drives.), Orientation = mean(relativ..relativity.), 
            Informal = mean(informal..informal.language.))%>%
  apa_table(format = "html")


Output[,c(1,17, 38, 44, 49, 56, 60, 65, 71, 74, 84)] %>%
  filter(Medium == "Audio") %>%
  stargazer(type = "html", digits = 2, summary.stat = c("n", "mean", "sd"), out = "desc2.html")

Output[,c(1,17, 38, 44, 49, 56, 60, 65, 71, 74, 84)] %>%
  filter(Medium != "Audio") %>%
  stargazer(type = "html", digits = 2, summary.stat = c("n", "mean", "sd"), out = "desc2.html")
  
Output %>%
  glm(formula = Real_Change ~  informal..informal.language.+
        cogproc..cognitive.processes. + drives..drives.+
        percept..perceptual.processes. + bio..biological.processes., family = "gaussian") %>%
  summary() 
  plot()

##### Descriptive Graphs #####
### One variable
## Liwc categories per topic
Output[,c(1,2,4,5:7,8:10,17, 35+3, 41+3, 46+3, 53+3, 57+3, 62+3, 71+3, 81+3)] %>% #View()
  melt(id.vars = 1:3, measure.vars = 10:18 ) %>%
  ggplot(aes(reorder(variable, value), value, fill = Topic)) + 
  stat_summary(geom = "col",position = "dodge2") + 
  stat_summary(geom = "errorbar", position = "dodge2") + 
  coord_flip() + labs(y = "Proportion", x = "LIWC term categories", title = "Term Frequency, by category and Topic",
                      caption = "n = 406") +
  scale_x_discrete(labels = rev(c("Function words", "Cognitive ('know', 'should', 'think)", 
                              "Social ('mate', 'talk', 'they')",
                              "Movement ('go', 'end', 'in')",
                              "Drives (affiliation, power, etc.)",
                              "Affect ('happy', 'cry', 'sweet')",
                              "Informal (fillers, swear, etc.)",
                              "Perceptual ('see', 'hear', 'feel')",
                              "Biological ('eat', 'blood', 'pain')")))
  
## Liwc categories per medium
Output[,c(1,2,4,5:7,8:10,17, 35+3, 41+3, 46+3, 53+3, 57+3, 62+3, 71+3, 81+3)] %>% #View()
  melt(id.vars = 1:3, measure.vars = 10:18 ) %>%
  ggplot(aes(reorder(variable, value), value, fill = Medium)) + 
  stat_summary(geom = "col",position = "dodge2") + 
  stat_summary(geom = "errorbar", position = "dodge2") + 
  coord_flip() + labs(y = "Proportion", x = "LIWC term categories", title = "Term Frequency, by Medium",
                      caption = "n = 406") +
  scale_x_discrete(labels = rev(c("Function words","Cognitive ('know', 'should', 'think)", 
                                  "Social ('mate', 'talk', 'they')",
                                  "Movement ('go', 'end', 'in')",
                                  "Drives (affiliation, power, etc.)",
                                  "Affect ('happy', 'cry', 'sweet')",
                                  "Informal (fillers, swear, etc.)",
                                  "Perceptual ('see', 'hear', 'feel')",
                                  "Biological ('eat', 'blood', 'pain')")))

## Liwc categories per extension
Output[,c(1,2,4,5:7,8:10,17, 35+3, 41+3, 46+3, 53+3, 57+3, 62+3, 71+3, 81+3)] %>% #View()
  melt(id.vars = 1:3, measure.vars = 10:18 ) %>%
  ggplot(aes(reorder(variable, value), value, fill = Extension)) + 
  stat_summary(geom = "col",position = "dodge2") + 
  stat_summary(geom = "errorbar", position = "dodge2") + 
  coord_flip() + labs(y = "Proportion", x = "LIWC term categories", title = "Term Frequency, by Extension",
                      caption = "n = 406") +
  scale_x_discrete(labels = rev(c("Cognitive ('know', 'should', 'think)", 
                                  "Social ('mate', 'talk', 'they')",
                                  "Movement ('go', 'end', 'in')",
                                  "Drives (affiliation, power, etc.)",
                                  "Affect ('happy', 'cry', 'sweet')",
                                  "Informal (fillers, swear, etc.)",
                                  "Perceptual ('see', 'hear', 'feel')",
                                  "Biological ('eat', 'blood', 'pain')")))

### Two variables
## Medium x Topic
Output[,c(1,2,4,5:7,8:10,17, 35+3, 41+3, 46+3, 53+3, 57+3, 62+3, 71+3, 81+3)] %>% #View()
  melt(id.vars = 1:3, measure.vars = 10:18 ) %>%
  ggplot(aes(reorder(variable, value), value, fill = Medium))+
  stat_summary(geom = "col",position = "dodge2") + 
  stat_summary(geom = "errorbar", position = "dodge2") + 
  facet_wrap(~Topic) +  coord_flip()


## Barplot: 3 outcomes, by categories ####
# Affective terms
A<-ggplot(Output) + 
  #geom_point(aes(affect..affect.,alt_Dehum)) + 
  geom_smooth(aes(affect..affect.,alt_Dehum, color = "red"), method = "lm") +
  #geom_point(aes(affect..affect.,alt_Conf)) + 
  geom_smooth(aes(affect..affect.,alt_Conf, color = "green"), method = "lm") +  
  #geom_point(aes(affect..affect.,alt_Resp)) + 
  geom_smooth(aes(affect..affect.,alt_Resp, color = "blue"), method = "lm") +
  scale_color_discrete(name = "Attributed", labels = c("Dehumanization", "Conflictive", "Responsiveness")) +
  labs(title = "Affective terms",x = "Proportion", y = "Partner attributions")

# Function words
B<-ggplot(Output) + 
  #geom_point(aes(function..function.words.,alt_Dehum)) + 
  geom_smooth(aes(function..function.words.,alt_Dehum, color = "red"), method = "lm") +
  #geom_point(aes(function..function.words.,alt_Conf)) + 
  geom_smooth(aes(function..function.words.,alt_Conf, color = "green"), method = "lm") +  
  #geom_point(aes(function..function.words.,alt_Resp)) + 
  geom_smooth(aes(function..function.words.,alt_Resp, color = "blue"), method = "lm") +
  scale_color_discrete(name = "Attributed", labels = c("Dehumanization", "Conflictive", "Responsiveness")) +
  labs(title = "Function Words",x = "Proportion", y = "Partner attributions")

# Social terms
C<-ggplot(Output) + 
  #geom_point(aes(social..social.,alt_Dehum)) + 
  geom_smooth(aes(social..social.,alt_Dehum, color = "red"), method = "lm") +
  #geom_point(aes(social..social.,alt_Conf)) + 
  geom_smooth(aes(social..social.,alt_Conf, color = "green"), method = "lm") +  
  #geom_point(aes(social..social.,alt_Resp)) + 
  geom_smooth(aes(social..social.,alt_Resp, color = "blue"), method = "lm") +
  scale_color_discrete(name = "Attributed", labels = c("Dehumanization", "Conflictive", "Responsiveness")) +
  labs(title = "Social terms",x = "Proportion", y = "Partner attributions")

# Cognitive terms
D<-ggplot(Output) + 
  #geom_point(aes(cogproc..cognitive.processes.,alt_Dehum)) + 
  geom_smooth(aes(cogproc..cognitive.processes.,alt_Dehum, color = "red"), method = "lm") +
  #geom_point(aes(cogproc..cognitive.processes.,alt_Conf)) + 
  geom_smooth(aes(cogproc..cognitive.processes.,alt_Conf, color = "green"), method = "lm") +  
  #geom_point(aes(cogproc..cognitive.processes.,alt_Resp)) + 
  geom_smooth(aes(cogproc..cognitive.processes.,alt_Resp, color = "blue"), method = "lm") +
  scale_color_discrete(name = "Attributed", labels = c("Dehumanization", "Conflictive", "Responsiveness")) +
  labs(title = "Cognitive terms",x = "Proportion", y = "Partner attributions")


# Perceptual terms
E<-ggplot(Output) + 
  #geom_point(aes(percept..perceptual.processes.,alt_Dehum)) + 
  geom_smooth(aes(percept..perceptual.processes.,alt_Dehum, color = "red"), method = "lm") +
  #geom_point(aes(percept..perceptual.processes.,alt_Conf)) + 
  geom_smooth(aes(percept..perceptual.processes.,alt_Conf, color = "green"), method = "lm") +  
  #geom_point(aes(percept..perceptual.processes.,alt_Resp)) + 
  geom_smooth(aes(percept..perceptual.processes.,alt_Resp, color = "blue"), method = "lm") +
  scale_color_discrete(name = "Attributed", labels = c("Dehumanization", "Conflictive", "Responsiveness")) +
  labs(title = "Perceptual terms",x = "Proportion", y = "Partner attributions")

# Biological terms
F<-ggplot(Output) + 
  #geom_point(aes(bio..biological.processes.,alt_Dehum)) + 
  geom_smooth(aes(bio..biological.processes.,alt_Dehum, color = "red"), method = "lm") +
  #geom_point(aes(bio..biological.processes.,alt_Conf)) + 
  geom_smooth(aes(bio..biological.processes.,alt_Conf, color = "green"), method = "lm") +  
  #geom_point(aes(bio..biological.processes.,alt_Resp)) + 
  geom_smooth(aes(bio..biological.processes.,alt_Resp, color = "blue"), method = "lm") +
  scale_color_discrete(name = "Attributed", labels = c("Dehumanization", "Conflictive", "Responsiveness")) +
  labs(title = "Biological terms",x = "Proportion", y = "Partner attributions")

# Drive terms
G<-ggplot(Output) + 
  #geom_point(aes(drives..drives.,alt_Dehum)) + 
  geom_smooth(aes(drives..drives.,alt_Dehum, color = "red"), method = "lm") +
  #geom_point(aes(drives..drives.,alt_Conf)) + 
  geom_smooth(aes(drives..drives.,alt_Conf, color = "green"), method = "lm") +  
  #geom_point(aes(drives..drives.,alt_Resp)) + 
  geom_smooth(aes(drives..drives.,alt_Resp, color = "blue"), method = "lm") +
  scale_color_discrete(name = "Attributed", labels = c("Dehumanization", "Conflictive", "Responsiveness")) +
  labs(title = "Drive-related terms",x = "Proportion", y = "Partner attributions")

# Informal terms
H<-ggplot(Output) + 
  #geom_point(aes(informal..informal.language.,alt_Dehum)) + 
  geom_smooth(aes(informal..informal.language.,alt_Dehum, color = "red"), method = "lm") +
  #geom_point(aes(informal..informal.language.,alt_Conf)) + 
  geom_smooth(aes(informal..informal.language.,alt_Conf, color = "green"), method = "lm") +  
  #geom_point(aes(informal..informal.language.,alt_Resp)) + 
  geom_smooth(aes(informal..informal.language.,alt_Resp, color = "blue"), method = "lm") +
  scale_color_discrete(name = "Attributed", labels = c("Dehumanization", "Conflictive", "Responsiveness")) +
  labs(title = "Informal terms", x = "Proportion", y = "Partner attributions")

# Relative terms
I<-ggplot(Output) + 
  #geom_point(aes(relativ..relativity.,alt_Dehum)) + 
  geom_smooth(aes(relativ..relativity.,alt_Dehum, color = "red"), method = "lm") +
  #geom_point(aes(relativ..relativity.,alt_Conf)) + 
  geom_smooth(aes(relativ..relativity.,alt_Conf, color = "green"), method = "lm") +  
  #geom_point(aes(relativ..relativity.,alt_Resp)) + 
  geom_smooth(aes(relativ..relativity.,alt_Resp, color = "blue"), method = "lm") +
  scale_color_discrete(name = "Attributed", labels = c("Dehumanization", "Conflictive", "Responsiveness")) +
  labs(title = "Relative terms (space, time, movement)",
       x = "Proportion", y = "Partner attributions")


library(ggpubr)
ggarrange(A, B, C, D, E, F, G, H, I, legend = "bottom", common.legend = TRUE, nrow = 3, ncol = 3)






## Sketchboard (non-consolidated code) #####

mat<-round(cor(Output[,c(6:8, 12:14, 21, 42, 48, 53, 60, 64, 69, 78, 88)]),3)
pmat<-corr.p(mat, n= nrow(Output))
ggcorrplot::ggcorrplot(mat, type = "lower", hc.order = FALSE, p.mat = pmat$p, lab = TRUE, insig = "blank", lab_size = 4)

ggcorr(Output[,c(6:8, 12:14, 21, 42, 48, 53, 60, 64, 69, 78, 88)])

ggplot(Output) + 
  geom_point(aes(cogproc..cognitive.processes.,alt_Dehum)) + 
  geom_smooth(aes(cogproc..cognitive.processes.,alt_Dehum, color = "red"), method = "lm") +
  geom_point(aes(cogproc..cognitive.processes.,alt_Conf)) + 
  geom_smooth(aes(cogproc..cognitive.processes.,alt_Conf, color = "green"), method = "lm") +  
  geom_point(aes(cogproc..cognitive.processes.,alt_Resp)) + 
  geom_smooth(aes(cogproc..cognitive.processes.,alt_Resp, color = "blue"), method = "lm") +
  scale_color_discrete(name = "Attributed", labels = c("Dehumanization", "Conflictive", "Responsiveness")) +
  labs(title = "Cognitive Words",x = "Proportion", y = "Partner attributions") + facet_wrap(~Medium)

ggplot(Output) + 
  #geom_point(aes(function..function.words.,alt_Dehum)) + 
  geom_smooth(aes(function..function.words.,alt_Dehum, color = "red"), method = "lm") +
  #geom_point(aes(function..function.words.,alt_Conf)) + 
  geom_smooth(aes(function..function.words.,alt_Conf, color = "green"), method = "lm") +  
  #geom_point(aes(function..function.words.,alt_Resp)) + 
  geom_smooth(aes(function..function.words.,alt_Resp, color = "blue"), method = "lm") +
  scale_color_discrete(name = "Attributed", labels = c("Dehumanization", "Conflictive", "Responsiveness")) +
  labs(title = "Function Words",x = "Proportion", y = "Partner attributions") + facet_wrap(~Medium)

ggplot(Output) + 
  #geom_point(aes(relativ..relativity.,alt_Dehum)) + 
  geom_smooth(aes(relativ..relativity.,alt_Dehum, color = "red"), method = "lm") +
  #geom_point(aes(relativ..relativity.,alt_Conf)) + 
  geom_smooth(aes(relativ..relativity.,alt_Conf, color = "green"), method = "lm") +  
  #geom_point(aes(relativ..relativity.,alt_Resp)) + 
  geom_smooth(aes(relativ..relativity.,alt_Resp, color = "blue"), method = "lm") +
  scale_color_discrete(name = "Attributed", labels = c("Dehumanization", "Conflictive", "Responsiveness")) +
  labs(title = "Relative Words",x = "Proportion", y = "Partner attributions") + facet_wrap(~Extension)

ggplot(Output) + 
  #geom_point(aes(bio..biological.processes.,alt_Dehum)) + 
  geom_smooth(aes(bio..biological.processes.,alt_Dehum, color = "red"), method = "lm") +
  #geom_point(aes(bio..biological.processes.,alt_Conf)) + 
  geom_smooth(aes(bio..biological.processes.,alt_Conf, color = "green"), method = "lm") +  
  #geom_point(aes(bio..biological.processes.,alt_Resp)) + 
  geom_smooth(aes(bio..biological.processes.,alt_Resp, color = "blue"), method = "lm") +
  scale_color_discrete(name = "Attributed", labels = c("Dehumanization", "Conflictive", "Responsiveness")) +
  labs(title = "Function Words",x = "Proportion", y = "Partner attributions") + facet_wrap(~Medium)


ggplot(Output) + 
  #geom_point(aes(drives..drives.,alt_Dehum)) + 
  geom_smooth(aes(drives..drives.,alt_Dehum, color = "red"), method = "lm") +
  #geom_point(aes(drives..drives.,alt_Conf)) + 
  geom_smooth(aes(drives..drives.,alt_Conf, color = "green"), method = "lm") +  
  #geom_point(aes(drives..drives.,alt_Resp)) + 
  geom_smooth(aes(drives..drives.,alt_Resp, color = "blue"), method = "lm") +
  scale_color_discrete(name = "Attributed", labels = c("Dehumanization", "Conflictive", "Responsiveness")) +
  labs(title = "Function Words",x = "Proportion", y = "Partner attributions") + facet_wrap(~Medium)


### Machine Learning - Stepwise regression
Vars<-paste0(noquote(names(Output)[21:93]), sep = " + ", collapse = "")

## Cross-validation
library(MASS)
resMatR = data.frame(Formula = vector(mode = "character", length = 10),
                    R_Squared = vector(mode = "numeric", length = 10),
                    r_test = vector(mode = "numeric", length = 10), stringsAsFactors = FALSE)

resMatC = data.frame(Formula = vector(mode = "character", length = 10),
                     R_Squared = vector(mode = "numeric", length = 10),
                     r_test = vector(mode = "numeric", length = 10), stringsAsFactors = FALSE)

resMatM = data.frame(Formula = vector(mode = "character", length = 10),
                     R_Squared = vector(mode = "numeric", length = 10),
                     r_test = vector(mode = "numeric", length = 10), stringsAsFactors = FALSE)

for (i in 1:10){
index<-sample(1:nrow(Output), ceiling(nrow(Output)*4/5))
Train<-Output[index,]
Test<- Output[-index,]
fullR<-lm(data=Train, alt_Resp ~ affect..affect..posemo..positive.emotions. + affect..affect..negemo..negative.emotions..anx..anx. + 
           affect..affect..negemo..negative.emotions..anger..anger. + affect..affect..negemo..negative.emotions..sad..sad. + 
           social..social..family..family. + social..social..friend..friends. + 
           social..social..female..female. + social..social..male..male. + cogproc..cognitive.processes..insight..insight. + cogproc..cognitive.processes..cause..causal. + 
           cogproc..cognitive.processes..discrep..discrepancies. + cogproc..cognitive.processes..tentat..tentative. + 
           cogproc..cognitive.processes..certain..certainty. + cogproc..cognitive.processes..differ..differentiation. + 
           percept..perceptual.processes..see..see. + percept..perceptual.processes..hear..hear. + 
           percept..perceptual.processes..feel..feel. + bio..biological.processes..body..body. + 
           bio..biological.processes..health..health. + bio..biological.processes..sexual..sexual. + bio..biological.processes..ingest..ingest. + 
           drives..drives..affiliation..affiliation. + drives..drives..achieve..achievement. + 
           drives..drives..power..power. + drives..drives..reward..reward. + drives..drives..risk..risk. + 
           timeorient..time.orientation..focuspast..past.focus. + timeorient..time.orientation..focuspresent..present.focus. + 
           timeorient..time.orientation..focusfuture..future.focus. + relativ..relativity..motion..motion. + 
           relativ..relativity..space..space. + relativ..relativity..time..time. + persconc..personal.concerns..work..work. + 
           persconc..personal.concerns..leisure..leisure. + persconc..personal.concerns..home..home. + 
           persconc..personal.concerns..money..money. + persconc..personal.concerns..relig..religion. + 
           persconc..personal.concerns..death..death. + informal..informal.language..swear..swear. + informal..informal.language..netspeak..netspeak. + 
           informal..informal.language..assent..assent. + informal..informal.language..nonflu..nonfluencies. + 
           informal..informal.language..filler..filler.words.)

stepR<-stepAIC(fullR, direction = "both", trace = FALSE)
FmlaR<- stepR$call$formula
SumR<-stepR%>% summary()
R2R<-SumR$r.squared
r_testR<-cor(as.numeric(predict(stepR, newdata = Test)),Test$alt_Resp)

fullC<-lm(data=Train, alt_Conf ~ affect..affect..posemo..positive.emotions. + affect..affect..negemo..negative.emotions..anx..anx. + 
            affect..affect..negemo..negative.emotions..anger..anger. + affect..affect..negemo..negative.emotions..sad..sad. + 
            social..social..family..family. + social..social..friend..friends. + 
            social..social..female..female. + social..social..male..male. + cogproc..cognitive.processes..insight..insight. + cogproc..cognitive.processes..cause..causal. + 
            cogproc..cognitive.processes..discrep..discrepancies. + cogproc..cognitive.processes..tentat..tentative. + 
            cogproc..cognitive.processes..certain..certainty. + cogproc..cognitive.processes..differ..differentiation. + 
            percept..perceptual.processes..see..see. + percept..perceptual.processes..hear..hear. + 
            percept..perceptual.processes..feel..feel. + bio..biological.processes..body..body. + 
            bio..biological.processes..health..health. + bio..biological.processes..sexual..sexual. + bio..biological.processes..ingest..ingest. + 
            drives..drives..affiliation..affiliation. + drives..drives..achieve..achievement. + 
            drives..drives..power..power. + drives..drives..reward..reward. + drives..drives..risk..risk. + 
            timeorient..time.orientation..focuspast..past.focus. + timeorient..time.orientation..focuspresent..present.focus. + 
            timeorient..time.orientation..focusfuture..future.focus. + relativ..relativity..motion..motion. + 
            relativ..relativity..space..space. + relativ..relativity..time..time. + persconc..personal.concerns..work..work. + 
            persconc..personal.concerns..leisure..leisure. + persconc..personal.concerns..home..home. + 
            persconc..personal.concerns..money..money. + persconc..personal.concerns..relig..religion. + 
            persconc..personal.concerns..death..death. + informal..informal.language..swear..swear. + informal..informal.language..netspeak..netspeak. + 
            informal..informal.language..assent..assent. + informal..informal.language..nonflu..nonfluencies. + 
            informal..informal.language..filler..filler.words.)

stepC<-stepAIC(fullC, direction = "both", trace = FALSE)
FmlaC<- stepC$call$formula
SumC<-stepC%>% summary()
R2C<-SumC$r.squared
r_testC<-cor(as.numeric(predict(stepC, newdata = Test)),Test$alt_Conf)


fullM<-lm(data=Train, alt_Dehum ~ affect..affect..posemo..positive.emotions. + affect..affect..negemo..negative.emotions..anx..anx. + 
            affect..affect..negemo..negative.emotions..anger..anger. + affect..affect..negemo..negative.emotions..sad..sad. + 
            social..social..family..family. + social..social..friend..friends. + 
            social..social..female..female. + social..social..male..male. + cogproc..cognitive.processes..insight..insight. + cogproc..cognitive.processes..cause..causal. + 
            cogproc..cognitive.processes..discrep..discrepancies. + cogproc..cognitive.processes..tentat..tentative. + 
            cogproc..cognitive.processes..certain..certainty. + cogproc..cognitive.processes..differ..differentiation. + 
            percept..perceptual.processes..see..see. + percept..perceptual.processes..hear..hear. + 
            percept..perceptual.processes..feel..feel. + bio..biological.processes..body..body. + 
            bio..biological.processes..health..health. + bio..biological.processes..sexual..sexual. + bio..biological.processes..ingest..ingest. + 
            drives..drives..affiliation..affiliation. + drives..drives..achieve..achievement. + 
            drives..drives..power..power. + drives..drives..reward..reward. + drives..drives..risk..risk. + 
            timeorient..time.orientation..focuspast..past.focus. + timeorient..time.orientation..focuspresent..present.focus. + 
            timeorient..time.orientation..focusfuture..future.focus. + relativ..relativity..motion..motion. + 
            relativ..relativity..space..space. + relativ..relativity..time..time. + persconc..personal.concerns..work..work. + 
            persconc..personal.concerns..leisure..leisure. + persconc..personal.concerns..home..home. + 
            persconc..personal.concerns..money..money. + persconc..personal.concerns..relig..religion. + 
            persconc..personal.concerns..death..death. + informal..informal.language..swear..swear. + informal..informal.language..netspeak..netspeak. + 
            informal..informal.language..assent..assent. + informal..informal.language..nonflu..nonfluencies. + 
            informal..informal.language..filler..filler.words.)

stepM<-stepAIC(fullM, direction = "both", trace = FALSE)
FmlaM<- stepM$call$formula
SumM<-stepM%>% summary()
R2M<-SumM$r.squared
r_testM<-cor(as.numeric(predict(stepM, newdata = Test)),Test$alt_Dehum)


resMatR[i,1]<-as.character(FmlaR[3])
resMatR[i,2]<-R2R
resMatR[i,3]<-r_testR

resMatC[i,1]<-as.character(FmlaC[3])
resMatC[i,2]<-R2C
resMatC[i,3]<-r_testC


resMatM[i,1]<-as.character(FmlaM[3])
resMatM[i,2]<-R2M
resMatM[i,3]<-r_testM

}



TermMatC<-as.data.frame(t(str_split_fixed( resMatC[,1], pattern = " \\+ ", 17)))
TermListC<-summary(unlist(TermMatC))[-1]

ModelC<-names(TermListC)[TermListC>=7]

TermMatM<-as.data.frame(t(str_split_fixed( resMatM[,1], pattern = " \\+ ", 17)))
TermListM<-summary(unlist(TermMatM))[-1]

ModelM<-names(TermListM)[TermListM>=7]

TermMatR<-as.data.frame(t(str_split_fixed( resMatR[,1], pattern = " \\+ ", 17)))
TermListR<-summary(unlist(TermMatR))[-1]

ModelR<-names(TermListR)[TermListR>=7]


resMatC<-resMatC %>% 
  mutate(Product = sqrt(R_Squared^2+ r_test)* r_test^2/R_Squared) 
BestC<-resMatC[which(resMatC$Product ==max(resMatC$Product)),1]
resMatM<-resMatM %>% 
  mutate(Product = sqrt(R_Squared^2+ r_test)* r_test^2/R_Squared) 
BestM<-resMatC[which(resMatM$Product ==max(resMatM$Product)),1]
resMatR<-resMatR %>% 
  mutate(Product = sqrt(R_Squared^2+ r_test)* r_test^2/R_Squared) 
BestR<-resMatR[which(resMatR$Product ==max(resMatR$Product)),1]


## Running the best regressions ever

BS_R<-lm(data=Output, paste("alt_Resp", "~", BestR, sep = "")) #%>% summary()
BS_C<-lm(data=Output, paste("alt_Conf", "~", BestC, sep = "")) #%>% summary()
BS_M<-lm(data=Output, paste("alt_Dehum", "~", BestM, sep = "")) #%>% summary()

CO_R<-lm(data=Output, paste("alt_Resp", "~", paste(ModelR, collapse = "+"))) #%>% summary()
CO_C<-lm(data=Output, paste("alt_Conf", "~", paste(ModelC, collapse = "+"))) #%>% summary()
CO_M<-lm(data=Output, paste("alt_Dehum", "~", paste(ModelM, collapse = "+"))) #%>% summary()

Probe<-Output[sample(150),]
cor(cbind(predict(BS_R, Probe), predict(CO_R, Probe), Probe$alt_Resp), use = "complete")

Probe<-Output[sample(150),]
cor(cbind(predict(BS_C, Probe), predict(CO_C, Probe), Probe$alt_Conf), use = "complete")

Probe<-Output[sample(150),]
cor(cbind(predict(BS_M, Probe), predict(CO_M, Probe), Probe$alt_Dehum), use = "complete")

library(stargazer)
Reg1<-lm(data=Output, paste("alt_Resp", "~", BestR, sep = "")) 
Reg2<-lm(data=Output, paste("alt_Conf", "~", BestC, sep = ""))
Reg3<-lm(data=Output, paste("alt_Dehum", "~", BestM, sep = ""))

stargazer(Reg1, Reg2, Reg3,type = "html", out = "Test.html", 
          dep.var.labels = c("Responsiveness","Conflict", "Mind"), dep.var.caption = "Attributes perceived by partner",
          initial.zero = FALSE,single.row = TRUE, header = TRUE, title = "Regressions: best models of words predicting partner perception",
          covariate.labels = Terms,  digits = 2, model.numbers = FALSE, no.space = TRUE, align = TRUE, notes.align = "c") 


unique(names(unlist(lapply(list(Reg1, Reg2, Reg3), coef))))


#Terms<-c("Negative Affect: Anxiety", "Negative Affect: Anger", "Negative Affect: Sadness",
#                     "Social words: Friends", "Social words: female", "Positive Affect", "Cognitive: Insight",
#                     "Cognitive: Causal terms", "Perception: Feel", "Biological terms: Ingest", "Drives: power",
#                     "Cognitive: Tentative talk", "Perception: Hear", "Biological terms: Body", "Biological terms: Health",
#                     "Time Focus: Present", "Relative: Motion terms", "Concerns: Home", "Concerns: Money", "Concerns:Death",
#                     "Informal: Swear", "Drives: Affiliation", "Drives: Achievement", "Drives: Reward", "Time Focus: Past",
#                     "Concerns: Religion", "Informal: Netspeak", "Informal: Assent", "Informal: filler words", "Informal:nonfluencies")