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
Output[,c(1,2,21, 42, 48, 53, 60, 64, 69, 78, 88)] %>%
  melt(id.vars = 1:2, measure.vars = 3:11 ) %>%
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


#### Machine Learning - Stepwise regression ####
Vars<-paste0(noquote(names(Output)[21:93]), sep = " + ", collapse = "")
# Straigth-forward model
library(MASS)
library(lm.beta)

fullR<-lm(data=Output, alt_Resp ~ affect..affect..posemo..positive.emotions. + affect..affect..negemo..negative.emotions..anx..anx. + 
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

BestR<-lm(data=Output, FmlaR) %>%
  lm.beta() %>% summary.lm.beta() 

BestR$coefficients %>%
  as.data.frame() %>% 
  mutate(names = row.names(.)) %>%
  dplyr::filter(abs(Standardized) >= .1) %>% 
  dplyr::select(names) %>% unlist() %>%
  paste("alt_Resp ~", ., collapse = "+")  -> Best_F_Res

Best_FitR<-lm(data=Output, formula = alt_Resp ~ affect..affect..negemo..negative.emotions..sad..sad.+ 
                social..social..friend..friends.+
                social..social..female..female.+
                cogproc..cognitive.processes..insight..insight.+
                cogproc..cognitive.processes..cause..causal.+
                drives..drives..affiliation..affiliation.+
                timeorient..time.orientation..focuspresent..present.focus.+
                informal..informal.language..assent..assent.)

fullC<-lm(data=Output, alt_Conf ~ affect..affect..posemo..positive.emotions. + affect..affect..negemo..negative.emotions..anx..anx. + 
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

BestC<-lm(data=Output, FmlaC) %>%
  lm.beta() %>% summary.lm.beta() 

BestC$coefficients %>%
  as.data.frame() %>% 
  mutate(names = row.names(.)) %>%
  dplyr::filter(abs(Standardized) >= .1) %>% 
  dplyr::select(names) %>% unlist() %>%
  paste("alt_Conf ~", ., collapse = "+")  -> Best_C_Res

Best_FitC<-lm(data=Output, formula = alt_Conf ~ affect..affect..posemo..positive.emotions.+
                cogproc..cognitive.processes..insight..insight.+
                drives..drives..achieve..achievement.+
                timeorient..time.orientation..focuspresent..present.focus.+
                persconc..personal.concerns..home..home.+
                informal..informal.language..assent..assent.)



fullH<-lm(data=Output, alt_Dehum ~ affect..affect..posemo..positive.emotions. + affect..affect..negemo..negative.emotions..anx..anx. + 
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

stepH<-stepAIC(fullH, direction = "both", trace = FALSE)
FmlaH<- stepH$call$formula

BestH<-lm(data=Output, FmlaH) %>%
  lm.beta() %>% summary.lm.beta() 

BestH$coefficients %>%
  as.data.frame() %>% 
  mutate(names = row.names(.)) %>%
  dplyr::filter(abs(Standardized) >= .1) %>% 
  dplyr::select(names) %>% unlist() %>%
  paste("alt_Dehum ~", ., collapse = "+")  -> Best_H_Res

Best_FitH<-lm(data=Output, formula = alt_Dehum ~ social..social..friend..friends.+
                bio..biological.processes..health..health.+
                drives..drives..achieve..achievement.+
                drives..drives..reward..reward.+
                timeorient..time.orientation..focuspresent..present.focus.+
                informal..informal.language..assent..assent.)



## Cross-validation (NOT RUN) ####

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


## Running the best regressions ever ####
library(stargazer)

stargazer(Best_FitH, Best_FitC, Best_FitR,type = "html", out = "Test.html", 
          dep.var.labels = c("Mind","Conflict", "Responsiveness"), dep.var.caption = "Attributes perceived by partner",
          initial.zero = FALSE,single.row = TRUE, header = TRUE, title = "Regressions: best models of words predicting partner perception",
          covariate.labels = Terms, digits = 2, model.numbers = FALSE, no.space = TRUE, align = TRUE, notes.align = "c", star.cutoffs = c(.05, .01, .001) 


unique(names(unlist(lapply(list(Reg1, Reg2, Reg3), coef))))


Terms<-c("Negative Affect: Sadness", "Social: Friends", "Biological: Health", "Positive Affect", "Social: Female",
"Cognitive: Insight", "Drives: Achievement", "Drives: Reward", "Cognitive: Causal", "Drive: Affiliation", "Time Orientation: Present",
"Concerns: Home", "Informal Language: Assent")

#### Word categoris by topics #####
## General categories
Output %>%
  melt(id.vars = "Topic", measure.vars =c(21, 42,48, 53,60,64,69, 78, 88)) %>%
  ggplot(aes(variable, value, fill = Topic)) + 
  stat_summary( geom = "col", position = "dodge2") + 
  stat_summary( geom = "errorbar", position = "dodge2") + coord_flip()
  
# Specific Categories 1/2
Output %>%
  melt(id.vars = "Topic", measure.vars =c(22, 30:41, 43, 45:47, 49:52,54:59)) %>%
  ggplot(aes(variable, value, fill = Topic)) + 
  stat_summary( geom = "col", position = "dodge2") + 
  stat_summary( geom = "errorbar", position = "dodge2") + coord_flip()

# Specific Categories 2/2
Output %>%
  melt(id.vars = "Topic", measure.vars =c(61:63,65:68,70:77,79:87, 89:93)) %>%
  ggplot(aes(variable, value, fill = Topic)) + 
  stat_summary( geom = "col", position = "dodge2") + 
  stat_summary( geom = "errorbar", position = "dodge2") + coord_flip()

Output %>%
  #lm(data=., informal..informal.language. ~ Topic) %>% summary()   # NO
  #lm(data=., relativ..relativity. ~ Topic) %>% summary()           # NO
  #lm(data=., drives..drives. ~ Topic) %>% summary()                # Yes, Reparations
  #lm(data=., bio..biological.processes. ~ Topic) %>% summary()     # All three differ
  #lm(data=., percept..perceptual.processes. ~ Topic) %>% summary() # Yes, Speakers
  #lm(data=., cogproc..cognitive.processes. ~ Topic) %>% summary()  # NO
  #lm(data=., social..social. ~ Topic) %>% summary()                 # Yes, Speakers 
  #lm(data=., affect..affect. ~ Topic) %>% summary()                # Yes, Speakers
  lm(data=., function..function.words. ~ Topic) %>% summary()       # Yes, Speakers slightly
  
  
## Differences in Drive words = Power words (slave?)
Output %>%
  melt(id.vars = "Topic", measure.vars =70:74) %>%
  ggplot(aes(variable, value, fill = Topic)) + 
  stat_summary( geom = "col", position = "dodge2") + 
  stat_summary( geom = "errorbar", position = "dodge2") + coord_flip()

LIWC_dic[["drives (Drives)"]][["power (Power)"]]  

## Differences in Bio words = health-related word in drugs (i.e,pain)
Output %>%
  melt(id.vars = "Topic", measure.vars =65:68) %>%
  ggplot(aes(variable, value, fill = Topic)) + 
  stat_summary( geom = "col", position = "dodge2") + 
  stat_summary( geom = "errorbar", position = "dodge2") + coord_flip()

LIWC_dic[["bio (Biological Processes)"]][["health (Health)"]]


## Differences in Perceptual words = hearing-related words (i.e. speaker, list, hush, sound, yelling)
Output %>%
  melt(id.vars = "Topic", measure.vars =61:63) %>%
  ggplot(aes(variable, value, fill = Topic)) + 
  stat_summary( geom = "col", position = "dodge2") + 
  stat_summary( geom = "errorbar", position = "dodge2") + coord_flip()

LIWC_dic[["percept (Perceptual Processes)"]][["hear (Hear)"]]

## Differences in Social words = male-related words in speakers (e.g., he, guy ) and 
## family-related in reparations (e.g., grandsons, relatives, folks)
Output %>%
  melt(id.vars = "Topic", measure.vars =49:52) %>%
  ggplot(aes(variable, value, fill = Topic)) + 
  stat_summary( geom = "col", position = "dodge2") + 
  stat_summary( geom = "errorbar", position = "dodge2") + coord_flip()

LIWC_dic[["social (Social)"]][["male (Male)"]]
LIWC_dic[["social (Social)"]][["family (Family)"]]

## Differences in Affective words = anger-related words in speakers (e.g., asshole, jerk, intimidation, mean, threat)
Output %>%
  melt(id.vars = "Topic", measure.vars =43:47) %>%
  ggplot(aes(variable, value, fill = Topic)) + 
  stat_summary( geom = "col", position = "dodge2") + 
  stat_summary( geom = "errorbar", position = "dodge2") + coord_flip()

LIWC_dic[["affect (Affect)"]][["negemo (Negative Emotions)"]][["anger (Anger)"]]
## Differences in Function words: small and hard to interpret:
# - more pronouns but less adjectives, comparisons, quantifiers  in speakers
# - more auxiliary verbs in drugs, and reparations more than speakers
# - less conjunctions in reparations

Output %>%
  melt(id.vars = "Topic", measure.vars =c(22,30:41)) %>%
  ggplot(aes(variable, value, fill = Topic)) + 
  stat_summary( geom = "col", position = "dodge2") + 
  stat_summary( geom = "errorbar", position = "dodge2") + coord_flip()





###### Context analyses #####
# CR will generate texts with highlighted keywords for 'insight' and ' present' categories (JS will provide LIWC license).
library(reshape)
library(quanteda)
library(tidyverse)

Insight_context<-kwic(Huge$Text, LIWC_dic[["cogproc (Cognitive Processes)"]][["insight (Insight)"]] ) 
knitr::kable(head(Insight_context))
Present_context<-kwic(Huge$Text, LIWC_dic[["timeorient (Time Orientation)"]][["focuspresent (Present Focus)"]])

## Both categories deal with extremely common expressions; so it's hard to interpret what that could mean in terms of difference between conditions.


##### Mediation analyses ####
#CR will run mediation analyses on the three outcomes, using 'assent', 'present', and 'insight' words as mediators.
library(psych)
library(lavaan)
library(semPlot)

df_Meds<-cbind(Output[,c(1:3, 5:8, 12:14, 54, 76, 91)], Output_MFD[,16])
names(df_Meds)[11:14]<-c("Insight", "Present", "Assent", "Moral")
df_Meds$Medium<-dummy.code(df_Meds$Medium)[,1]
save(df_Meds, file= "Meds.Rdata")
#Mediation models

Ins_Hum<-'# direct effect
            alt_Dehum ~ c*Medium
            # mediator
            Insight ~ a*Medium
            alt_Dehum ~ b*Insight
            # indirect effect (a*b)
            indirect := a*b
            # direct effect (c)
            direct := c
            # total effect
            total := c + (a*b)' 

Fit_Ins_Hum<-sem(data=df_Meds, model = Ins_Hum, estimator = "MLM")
summary(Fit_Ins_Hum)

Ins_Conf<-'# direct effect
            alt_Conf ~ c*Medium
            # mediator
            Insight ~ a*Medium
            alt_Conf ~ b*Insight
            # indirect effect (a*b)
            indirect := a*b
            # direct effect (c)
            direct := c
            # total effect
            total := c + (a*b)' 
Fit_Ins_Conf<-sem(data=df_Meds, model = Ins_Conf, estimator = "MLM")
summary(Fit_Ins_Conf)

Ins_Resp<-'# direct effect
            alt_Resp ~ c*Medium
            # mediator
            Insight ~ a*Medium
            alt_Resp ~ b*Insight
            # indirect effect (a*b)
            indirect := a*b
            # direct effect (c)
            direct := c
            # total effect
            total := c + (a*b)' 
Fit_Ins_Resp<-sem(data=df_Meds, model = Ins_Resp, estimator = "MLM")
summary(Fit_Ins_Resp)

Pres_Hum<-'# direct effect
            alt_Dehum ~ c*Medium
            # mediator
            Present ~ a*Medium
            alt_Dehum ~ b*Present
            # indirect effect (a*b)
            indirect := a*b
            # direct effect (c)
            direct := c
            # total effect
            total := c + (a*b)' 

Fit_Pres_Hum<-sem(data=df_Meds, model = Pres_Hum, estimator = "MLM")
summary(Fit_Pres_Hum)

Pres_Conf<-'# direct effect
            alt_Conf ~ c*Medium
            # mediator
            Present ~ a*Medium
            alt_Conf ~ b*Present
            # indirect effect (a*b)
            indirect := a*b
            # direct effect (c)
            direct := c
            # total effect
            total := c + (a*b)' 
Fit_Pres_Conf<-sem(data=df_Meds, model = Pres_Conf, estimator = "MLM")
summary(Fit_Pres_Conf)

Pres_Resp<-'# direct effect
            alt_Resp ~ c*Medium
            # mediator
            Present ~ a*Medium
            alt_Resp ~ b*Present
            # indirect effect (a*b)
            indirect := a*b
            # direct effect (c)
            direct := c
            # total effect
            total := c + (a*b)' 
Fit_Pres_Resp<-sem(data=df_Meds, model = Pres_Resp, estimator = "MLM")
summary(Fit_Pres_Resp)

Asnt_Hum<-'# direct effect
            alt_Dehum ~ c*Medium
            # mediator
            Assent ~ a*Medium
            alt_Dehum ~ b*Assent
            # indirect effect (a*b)
            indirect := a*b
            # direct effect (c)
            direct := c
            # total effect
            total := c + (a*b)' 

Fit_Asnt_Hum<-sem(data=df_Meds, model = Asnt_Hum, estimator = "MLM")
summary(Fit_Asnt_Hum)

Asnt_Conf<-'# direct effect
            alt_Conf ~ c*Medium
            # mediator
            Assent ~ a*Medium
            alt_Conf ~ b*Assent
            # indirect effect (a*b)
            indirect := a*b
            # direct effect (c)
            direct := c
            # total effect
            total := c + (a*b)' 
Fit_Asnt_Conf<-sem(data=df_Meds, model = Asnt_Conf, estimator = "MLM")
summary(Fit_Asnt_Conf)

#### Supression effect??
semPaths(Fit_Asnt_Conf, what = "std" )

Asnt_Resp<-'# direct effect
            alt_Resp ~ c*Medium
            # mediator
            Assent ~ a*Medium
            alt_Resp ~ b*Assent
            # indirect effect (a*b)
            indirect := a*b
            # direct effect (c)
            direct := c
            # total effect
            total := c + (a*b)' 
Fit_Asnt_Resp<-sem(data=df_Meds, model = Asnt_Resp, estimator = "MLM")
summary(Fit_Asnt_Resp)

semPaths(Fit_Asnt_Resp, what = "std")

All_Dehum<-'# direct effect
            alt_Dehum ~ c*Medium
            # mediator 1
            Present ~ a_1*Medium
            alt_Dehum ~ b_1*Present
            # indirect effect_1 (a_1*b_1)
            indirect_1 := a_1*b_1
            # mediator 2
            Insight ~ a_2*Medium
            alt_Dehum ~ b_2*Insight
            #indirect effect_2 (a_2*b_2)
            indirect_2 := a_2*b_2
            # mediator 3
            Assent ~ a_3*Medium
            alt_Dehum ~ b_3*Assent
            #indirect effect_3 (a_3*b_3)
            indirect_3 := a_3*b_3
            # mediator 4
            Moral ~ a_4*Medium
            alt_Dehum ~ b_4*Moral
            # indirect effect_4 (a_4*b_5)
            indirect_4 := a_4*b_4
            # direct effect (c)
            direct := c
            # total effect
            total := c + (a_1*b_1) + (a_2*b_2) + (a_3*b_3) + (a_4*b_4)'

Fit_All_Hum<-sem(data=df_Meds, model = All_Dehum, estimator = "MLM")
summary(Fit_All_Hum)



## SEM Plots ####
lay<-matrix(c(1, 0 , #dehum
              0, -.15, #pres
              0, .05, #insight
              0, -.05, #Asnt
              0, -.1, #Moral
              -1, 0) # Medium)
            , ncol = 2, byrow =TRUE)
Hum_labs = c("Partner \n Humanization", "Time-Present \n words", "Insight \n words","Assent \n words", "Moral", "Medium \n (T = 0; A = 1)")


semPaths(Fit_All_Hum, "par",, layout = lay, sizeMan2 = 8, 
         sizeMan = 10, sizeInt = 12, edge.label.cex=1.2, style = "ram",
         fade=FALSE, nodeLabels = Hum_labs, label.prop = c(1.4,1.4, .8, .8, .8, 1.4), residuals = FALSE)
text(-.9,-.9,"Indirect effects: \n Insight: -.02 \n Present: -.016 \n Assent: .07, \n Moral: .05")

All_Conf<-'# direct effect
            alt_Conf ~ c*Medium
            # mediator 1
            Present ~ a_1*Medium
            alt_Conf ~ b_1*Present
            # indirect effect_1 (a_1*b_1)
            indirect_1 := a_1*b_1
            # mediator 2
            Insight ~ a_2*Medium
            alt_Conf ~ b_2*Insight
            #indirect effect_2 (a_2*b_2)
            indirect_2 := a_2*b_2
            # mediator 3
            Assent ~ a_3*Medium
            alt_Conf ~ b_3*Assent
            #indirect effect_3 (a_3*b_3)
            indirect_3 := a_3*b_3
            # mediator 4
            Moral ~ a_4*Medium
            alt_Conf ~ b_4*Moral
            # indirect effect_4 (a_4*b_5)
            indirect_4 := a_4*b_4
            # direct effect (c)
            direct := c
            # total effect
            total := c + (a_1*b_1) + (a_2*b_2) + (a_3*b_3) + (a_4*b_4)'

Fit_All_Conf<-sem(data=df_Meds, model = All_Conf, estimator = "MLM")
summary(Fit_All_Conf)


            , ncol = 2, byrow =TRUE)
Conf_labs = c("Perceived \n Conflict", "Time-Present \n words", "Insight \n words","Assent \n words", "Moral", "Medium \n (T = 0; A = 1)")

semPaths(Fit_All_Conf, "par",, layout = lay, sizeMan2 = 8, 
         sizeMan = 10, sizeInt = 12, edge.label.cex=1.2, style = "ram",
         fade=FALSE, nodeLabels = Conf_labs, label.prop = c(1.4,1.4, .8, .8, .8, 1.4), residuals = FALSE)
text(-.9, -.9, "Indirect effects: \n Insight: .04 \n Present: .04 \n Assent: -.295***,\n Moral: -.10*")


All_Resp<-'# direct effect
            alt_Resp ~ c*Medium
            # mediator 1
            Present ~ a_1*Medium
            alt_Resp ~ b_1*Present
            # indirect effect_1 (a_1*b_1)
            indirect_1 := a_1*b_1
            # mediator 2
            Insight ~ a_2*Medium
            alt_Resp ~ b_2*Insight
            #indirect effect_2 (a_2*b_2)
            indirect_2 := a_2*b_2
            # mediator 3
            Assent ~ a_3*Medium
            alt_Resp ~ b_3*Assent
            #indirect effect_3 (a_3*b_3)
            indirect_3 := a_3*b_3
            # mediator 4
            Moral ~ a_4*Medium
            alt_Resp ~ b_4*Moral
            # indirect effect_4 (a_4*b_5)
            indirect_4 := a_4*b_4
            # direct effect (c)
            direct := c
            # total effect
            total := c + (a_1*b_1) + (a_2*b_2) + (a_3*b_3) + (a_4*b_4)'
Fit_All_Resp<-sem(data=df_Meds, model = All_Resp, estimator = "MLM")
summary(Fit_All_Resp)

Resp_labs  = c("Perceived \n Responsiveness", "Time-Present \n words", "Insight \n words","Assent \n words","Moral",  "Medium \n (T = 0; A = 1)")

semPaths(Fit_All_Resp, "par",, layout = lay, sizeMan2 = 8, 
         sizeMan = 10, sizeInt = 12, edge.label.cex=1.2, style = "ram",
         fade=FALSE, nodeLabels = Resp_labs, label.prop = c(1.4,1.4, .8, .8, .8, 1.4), residuals = FALSE)
text(-.9, -.9, "Indirect effects: \n Insight: .-02 \n Present: -.05* \n Assent: -.24*** \n Moral: -.09*")



#MFD####
# - Moralization: CR will run the MFD to analyze moralization as a potential mediator
# Analysis
library(quanteda)
MFD_dic<-dictionary(file = "C:/Users/Cristian Rodriguez/Documents/Research/1 Projects - Ongoing/MFD es/Spanish-mfd-master/dicts/mfd2.0.dic")
Results_MFD<-liwcalike(Huge$Text, MFD_dic)

Output_MFD<-data.frame(Huge[,c(3:6,8:10, 27:29)], Results_MFD)
### RESULT! Moralization across topics, higher in Text than Audio.
Output_MFD %>%
  mutate(Care = care.virtue + care.vice, 
         Fairness = fairness.virtue+fairness.vice,
         Authority = authority.virtue+authority.vice,
         Loyalty = loyalty.virtue+loyalty.vice,
         Sanctity = sanctity.virtue+sanctity.vice) %>% 
  melt(measure.vars = 39:43, id.vars = c(1,2,4)) %>%
  ggplot(aes(variable, value, fill = Medium)) + 
  stat_summary(geom = "col", position = "dodge2") + 
  stat_summary(geom = "errorbar", position = "dodge2") + coord_flip() + facet_wrap(~Topic)

## Mediation: does moralization explains the differences between Text-Audio?
Output_MFD$Medium<-dummy.code(Output_MFD$Medium)[,1]


Moral_Hum<-'# direct effect
            alt_Dehum ~ c*Medium
            # mediator
            Dic ~ a*Medium
            alt_Dehum ~ b*Dic
            # indirect effect (a*b)
            indirect := a*b
            # direct effect (c)
            direct := c
            # total effect
            total := c + (a*b)' 

Fit_Moral_Hum<-sem(Moral_Hum, data=Output_MFD)

Sanct_Hum<-'# direct effect
            alt_Dehum ~ c*Medium
            # mediator
            Sanctity ~ a*Medium
            alt_Dehum ~ b*Sanctity
            # indirect effect (a*b)
            indirect := a*b
            # direct effect (c)
            direct := c
            # total effect
            total := c + (a*b)' 


Output_MFD %>%
  mutate(Care = care.virtue + care.vice, 
         Fairness = fairness.virtue+fairness.vice,
         Authority = authority.virtue+authority.vice,
         Loyalty = loyalty.virtue+loyalty.vice,
         Sanctity = sanctity.virtue+sanctity.vice) %>%
  sem(data=., model = Sanct_Hum) %>% summary()

Care_Hum<-'# direct effect
            alt_Dehum ~ c*Medium
            # mediator
            Care ~ a*Medium
            alt_Dehum ~ b*Care
            # indirect effect (a*b)
            indirect := a*b
            # direct effect (c)
            direct := c
            # total effect
            total := c + (a*b)' 
Output_MFD %>%
  mutate(Care = care.virtue + care.vice, 
         Fairness = fairness.virtue+fairness.vice,
         Authority = authority.virtue+authority.vice,
         Loyalty = loyalty.virtue+loyalty.vice,
         Sanctity = sanctity.virtue+sanctity.vice) %>%
  sem(data=., model = Care_Hum) %>% summary()



Moral_Conf<-'# direct effect
            alt_Conf ~ c*Medium
            # mediator
            Dic ~ a*Medium
            alt_Conf ~ b*Dic
            # indirect effect (a*b)
            indirect := a*b
            # direct effect (c)
            direct := c
            # total effect
            total := c + (a*b)' 

Fit_Moral_Conf<-sem(Moral_Conf, data=Output_MFD) %>% summary()

Sanct_Conf<-'# direct effect
            alt_Conf ~ c*Medium
            # mediator
            Sanctity ~ a*Medium
            alt_Conf ~ b*Sanctity
            # indirect effect (a*b)
            indirect := a*b
            # direct effect (c)
            direct := c
            # total effect
            total := c + (a*b)' 


Output_MFD %>%
  mutate(Care = care.virtue + care.vice, 
         Fairness = fairness.virtue+fairness.vice,
         Authority = authority.virtue+authority.vice,
         Loyalty = loyalty.virtue+loyalty.vice,
         Sanctity = sanctity.virtue+sanctity.vice) %>%
  sem(data=., model = Sanct_Conf) %>% summary()

Care_Conf<-'# direct effect
            alt_Conf ~ c*Medium
            # mediator
            Care ~ a*Medium
            alt_Conf ~ b*Care
            # indirect effect (a*b)
            indirect := a*b
            # direct effect (c)
            direct := c
            # total effect
            total := c + (a*b)' 
Output_MFD %>%
  mutate(Care = care.virtue + care.vice, 
         Fairness = fairness.virtue+fairness.vice,
         Authority = authority.virtue+authority.vice,
         Loyalty = loyalty.virtue+loyalty.vice,
         Sanctity = sanctity.virtue+sanctity.vice) %>%
  sem(data=., model = Care_Conf) %>% summary()

Moral_Resp<-'# direct effect
            alt_Resp ~ c*Medium
            # mediator
            Dic ~ a*Medium
            alt_Resp ~ b*Dic
            # indirect effect (a*b)
            indirect := a*b
            # direct effect (c)
            direct := c
            # total effect
            total := c + (a*b)' 

Fit_Moral_Resp<-sem(Moral_Resp, data=Output_MFD) %>% summary()

Sanct_Resp<-'# direct effect
            alt_Resp ~ c*Medium
            # mediator
            Sanctity ~ a*Medium
            alt_Resp ~ b*Sanctity
            # indirect effect (a*b)
            indirect := a*b
            # direct effect (c)
            direct := c
            # total effect
            total := c + (a*b)' 


Output_MFD %>%
  mutate(Care = care.virtue + care.vice, 
         Fairness = fairness.virtue+fairness.vice,
         Authority = authority.virtue+authority.vice,
         Loyalty = loyalty.virtue+loyalty.vice,
         Sanctity = sanctity.virtue+sanctity.vice) %>%
  sem(data=., model = Sanct_Resp) %>% summary()

Care_Resp<-'# direct effect
            alt_Resp ~ c*Medium
            # mediator
            Care ~ a*Medium
            alt_Resp ~ b*Care
            # indirect effect (a*b)
            indirect := a*b
            # direct effect (c)
            direct := c
            # total effect
            total := c + (a*b)' 
Output_MFD %>%
  mutate(Care = care.virtue + care.vice, 
         Fairness = fairness.virtue+fairness.vice,
         Authority = authority.virtue+authority.vice,
         Loyalty = loyalty.virtue+loyalty.vice,
         Sanctity = sanctity.virtue+sanctity.vice) %>%
  sem(data=., model = Care_Resp) %>% summary()

## Summary of Indirect effects from Mediation Analyses ####

Indirect<-data.frame( Mediator = c("Assent", "Present", "Insight", "Moral"),
                        Mindful = vector(mode = "character", length = 4),
                        Conflictive = vector(mode = "character", length = 4),
                        Responsive= vector(mode = "character", length = 4),
                      stringsAsFactors = FALSE)
Ind_effects<-vector(mode = "character", length = 12)

All_fits<-c(Fit_Asnt_Hum, Fit_Asnt_Conf, Fit_Asnt_Resp, 
            Fit_Pres_Hum, Fit_Pres_Conf, Fit_Pres_Resp, 
            Fit_Ins_Hum, Fit_Ins_Conf,Fit_Ins_Resp, 
            Fit_Moral_Hum, Fit_Moral_Conf, Fit_Moral_Resp)

for (i in 1:length(All_fits)) {
  
  est<-as.data.frame(summary(All_fits[[i]], header = FALSE))[7,6]
  pval<-as.data.frame(summary(All_fits[[i]]))[7,9]
  
  if(pval <.001) {
    est<-paste(round(est,2), "***", sep= "")
    }
  else {
    if (pval <.01) {
      est<-paste(round(est,2), "**", sep = "")
    } 
    else {
      if (pval <.05) {
        est<-paste(round(est,2), "*", sep = "")
      }
      else {est<-round(est,2)}
    }
  }
          
  Ind_effects[i]<-est
}

Indirect[1:4, 2:4]<- matrix(Ind_effects, 4, 3, byrow = TRUE)

save(Indirect, file = "Indirect.Rdata")
                      


#Other analyses ####
##- 1st vs 3rd person --> NEEDS measure of Agreement by JS

##- re-run regression by topics


