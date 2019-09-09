library(tidyverse)
library(reshape)
library(sjstats)

load("~/GitHub/Short_Long/Output.Rdata")

### Analyses based on Agreement ####

#Agreement Outcomes
# Dich_Agree: Self Reported Agreement (dichotomous; did you finally agree?)
# Ev_Agree: Self reported evolution of agreement (how much did you agree at the beginning / how much did you agree at the end; 1 Disagree - 5 Agree)
# Approach: pair-level measure of how the reported issue attitudes got closer after the convo ==> (Attitude pre [subjA] - Attitude pre [subj B]) -  (Attitude post [subjA] - Attitude post [subj B])

data_Ag<-Output %>%
  mutate(Agreed = if_else(Agreed == 0, 1, 0),
         Dich_Agree = if_else(Agreed == 0, "Disagreed", "Agreed"),
         Ev_Agree = Att_Change,
         Att_Change = Real_Change)

## Loop for the  Approach measure
data_Ag$Approach<-vector(mode = "numeric", length = nrow(data_Ag))
for (i in 1:nrow(data_Ag)){
  
  Pair_ID<-Huge$Pair[i]
  Subjects<-Huge$ID1[data_Ag$Pair == Pair_ID]
  Pre_Diff<-abs(Huge$Pre_Att[Huge$ID1 == Subjects[1]] - Huge$Pre_Att[Huge$ID1 == Subjects[2]])
  Post_Diff<-abs(Huge$Post_Att[Huge$ID1 == Subjects[1]] - Huge$Post_Att[Huge$ID1 == Subjects[2]])
  Diff_Diff<- Pre_Diff - Post_Diff
  data_Ag$Approach[i]<-Diff_Diff
}

### Exploratory plots and analyses ####
data_Ag %>%
  group_by(Dich_Agree) %>%
  summarise(mean(Ev_Agree), mean(Approach))

means<-data_Ag %>%
  group_by(Dich_Agree)%>%
  summarize(mean(Approach))


data_Ag %>%
  mutate(Approach = as.factor(Approach)) %>%
  melt(id.vars = "Dich_Agree", measure.vars =  "Approach") %>%
  ggplot(aes(value, fill = Dich_Agree)) + geom_bar(stat = "count", position = "dodge") +
  labs(title = "Approach estimates, by Perceived Agreement", y = "Approach", fill = "Perceived Agreement")
  

data_Ag %>%
  mutate(Approach = factor(Approach, labels = -2:4)) %>%
  ggplot(aes(fill = Approach))+
  geom_bar(aes(x = Topic), position = "dodge") + 
  geom_bar(aes(x = Medium), position = "dodge") + 
  geom_bar(aes(x = Extension), position = "dodge")+ 
  scale_fill_brewer(palette = 'Blues' ) + 
  scale_x_discrete(limits = c("Audio", "Text", "Short", "Long", "Drugs", "Reparations", "Speakers"))+
labs(title = "Approach estimates, by Conditions", y = "Proportion", fill = "Approach", 
     x = "Medium                    Extension                                  Topic       ")


data_Ag %>%
  melt(id.vars = "Medium", measure.vars =  "Ev_Agree") %>%
  ggplot(aes(value, color = Medium)) + geom_freqpoly(binwidth = 1.5)

data_Ag %>%
  melt(id.vars = "Medium", measure.vars =  "Dich_Agree") %>%
  ggplot(aes(value, fill = Medium)) + geom_bar(position = "dodge2")

ggplot(data_Ag, aes(Dich_Agree, alt_Dehum, fill = Medium)) + stat_summary(geom = "col", position = "dodge2")
ggplot(data_Ag, aes(Dich_Agree, alt_Conf, fill = Medium)) + stat_summary(geom = "col", position = "dodge2")
ggplot(data_Ag, aes(Dich_Agree, alt_Resp, fill = Medium)) + stat_summary(geom = "col", position = "dodge2")
ggplot(data_Ag, aes(Dich_Agree, Approach, fill = Medium)) + stat_summary(geom = "col", position = "dodge2")

ggplot(data_Ag, aes(Ev_Agree, alt_Dehum, color = Medium)) + geom_point(position = "jitter") + geom_smooth(method = "lm")
ggplot(data_Ag, aes(Ev_Agree, alt_Conf, color = Medium)) + geom_point(position = "jitter") + geom_smooth(method = "lm")
ggplot(data_Ag, aes(Ev_Agree, alt_Resp, color = Medium)) + geom_point(position = "jitter") + geom_smooth(method = "lm")

ggplot(data_Ag, aes(Approach, alt_Dehum, color = Medium)) + geom_point(position = "jitter")+ geom_smooth(method = "lm")
ggplot(data_Ag, aes(Approach, alt_Conf, color = Medium)) + geom_point(position = "jitter") + geom_smooth(method = "lm")
ggplot(data_Ag, aes(Approach, alt_Resp, color = Medium)) + geom_point(position = "jitter") + geom_smooth(method = "lm")



### Dataset by pairs ####
### Real Agreement as outcomes
### Hum, Conf and Resp averaged by participants
### IVs - word categories - averaged by participants

Paired_df<-data.frame( Pair_ID = unique(data_Ag$Pair),
                       Mind = vector(mode = "numeric", length = length(unique(data_Ag$Pair))),
                       Conf = vector(mode = "numeric", length = length(unique(data_Ag$Pair))),
                       Resp = vector(mode = "numeric", length = length(unique(data_Ag$Pair))))
                       
                       
for (i in 1:nrow(data_Ag)){
  if(Paired_df$Pair_ID)
  <-data_Ag$Pair[i]
  Subjects<-data_Ag$ID2[data_Ag$Pair == Pair_ID]
  Paired_df$Mind[i]<- (data_Ag$alt_Dehum[data_Ag$ID2 == Subjects[1]] + data_Ag$alt_Dehum[data_Ag$ID2 == Subjects[2]])/2
  Paired_df$Conf[i]<- (data_Ag$alt_Conf[data_Ag$ID2 == Subjects[1]] + data_Ag$alt_Conf[data_Ag$ID2 == Subjects[2]])/2
  Paired_df$Resp[i]<- (data_Ag$alt_Resp[data_Ag$ID2 == Subjects[1]] + data_Ag$alt_Resp[data_Ag$ID2 == Subjects[2]])/2
}
  
  



### Regressions ####
library(lme4)
Mixed<-glmer(data=data_Ag, Agreed ~ function..function.words. + affect..affect. + social..social. + 
              cogproc..cognitive.processes. + bio..biological.processes. + drives..drives. + relativ..relativity. +
              informal..informal.language. + Topic + (1 | Pair), 
             family = binomial)

se <- sqrt(diag(vcov(Mixed)))

ORs_CI <- cbind(Est = fixef(Mixed), 
              LL = fixef(Mixed) - 1.96 * se, 
              UL = fixef(Mixed) + 1.96 * se) %>%
   exp() %>% round(2) %>% as_tibble() %>%
  mutate(sig = c("", "", "", "", "", "**", "+", "", "**", "", "")) 

Mixed<-lmer(data=data_Ag, Ev_Agree ~ function..function.words. + affect..affect. + social..social. + 
              cogproc..cognitive.processes. + bio..biological.processes. + drives..drives. + relativ..relativity. +
              informal..informal.language. + (1 | Topic))

Mixed2<-lmer(data=data_Ag, Approach ~ function..function.words. + affect..affect. + social..social. + 
         cogproc..cognitive.processes. + bio..biological.processes. + drives..drives. + relativ..relativity. +
         informal..informal.language. + (1 | Topic))

Mixed3<-glmer(data=data_Ag, 
              Agreed ~ (1+Topic|bio..biological.processes.) + function..function.words. + affect..affect. + social..social. +
               cogproc..cognitive.processes. + bio..biological.processes. + drives..drives. + 
               relativ..relativity. + informal..informal.language., family = binomial())

  