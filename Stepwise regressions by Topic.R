## Drugs ####

Vars<-paste0(noquote(names(Output)[21:93]), sep = " + ", collapse = "")

## Cross-validation
library(MASS)


Out_Drugs<-Output[Output$Topic == "Drugs",]

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
  index<-sample(1:nrow(Out_Drugs), ceiling(nrow(Out_Drugs)*4/5))
  Train<-Out_Drugs[index,]
  Test<- Out_Drugs[-index,]
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
BestC<-resMatC[which(resMatC$Product ==max(resMatC$Product[-5.])),1]
resMatM<-resMatM %>% 
  mutate(Product = sqrt(R_Squared^2+ r_test)* r_test^2/R_Squared) 
BestM<-resMatC[which(resMatM$Product ==max(resMatM$Product)),1]
resMatR<-resMatR %>% 
  mutate(Product = sqrt(R_Squared^2+ r_test)* r_test^2/R_Squared) 
BestR<-resMatR[which(resMatR$Product ==max(resMatR$Product)),1]


BS_R<-lm(data=Output, paste("alt_Resp", "~", BestR, sep = "")) %>% summary()
BS_C<-lm(data=Output, paste("alt_Conf", "~", BestC, sep = "")) %>% summary()
BS_M<-lm(data=Output, paste("alt_Dehum", "~", BestM, sep = "")) %>% summary()


### Reparations

Vars<-paste0(noquote(names(Output)[21:93]), sep = " + ", collapse = "")

## Cross-validation
library(MASS)


Out_Reps<-Output[Output$Topic == "Reparations",]

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
  index<-sample(1:nrow(Out_Reps), ceiling(nrow(Out_Reps)*4/5))
  Train<-Out_Reps[index,]
  Test<- Out_Reps[-index,]
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
BestC<-resMatC[which(resMatC$Product ==max(resMatC$Product[c(3,6,8:10)])),1]
resMatM<-resMatM %>% 
  mutate(Product = sqrt(R_Squared^2+ r_test)* r_test^2/R_Squared) 
BestM<-resMatC[which(resMatM$Product ==max(resMatM$Product[c(-1,-6,-9,-10)])),1]
resMatR<-resMatR %>% 
  mutate(Product = sqrt(R_Squared^2+ r_test)* r_test^2/R_Squared) 
BestR<-resMatR[which(resMatR$Product ==max(resMatR$Product[-10])),1]


BS_R<-lm(data=Out_Reps, paste("alt_Resp", "~", BestR, sep = "")) %>% summary()
BS_C<-lm(data=Out_Reps, paste("alt_Conf", "~", BestC, sep = "")) %>% summary()
BS_M<-lm(data=Out_Reps, paste("alt_Dehum", "~", BestM, sep = "")) %>% summary()

### Speakers ####