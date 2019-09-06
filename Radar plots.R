library(tidyverse)
library(reshape)
library(ggradar)
library(scales)

save(Output_MFD, file = "MFD.Rdata")
Output %>%
  select(2, 23, 29:35) %>%
  group_by(Medium) %>%
  summarise( Personal_Pron = mean(function..function.words..pronoun..pronouns..ppron..personal.pronouns.),
             Impersonal_Pron = mean(function..function.words..pronoun..pronouns..ipron..impersonal.pronouns. ),
             Articles = mean(function..function.words..article..articles.), 
             Preopositions = mean(function..function.words..prep..prepositions.), 
             Aux_Verbs = mean(function..function.words..auxverb..auxiliary.verbs.),
             Conjunctions = mean(function..function.words..conj..conjunctions.), 
             Negations = mean(function..function.words..negate..negations.)) %>%
  #mutate_at(vars(-Medium), rescale, to = c(0,100)) %>%
  ggradar(grid.max =  13)

Output_MFD %>%
  mutate( Care = care.virtue+care.vice,
          Fairness = fairness.virtue+fairness.vice,
          Loyalty = loyalty.virtue+loyalty.vice,
          Authority = authority.virtue+authority.vice,
          Sanctity = sanctity.virtue+sanctity.vice,
          Medium = if_else(Medium ==0, "Text", "Audio") ) %>%
  group_by(Topic) %>%
  summarize (Care = mean(Care),
             Fairness = mean(Fairness),
             Loyalty = mean(Loyalty),
             Authority = mean(Authority),
             Sanctity = mean(Sanctity)) %>%
  ggradar(grid.max = 3)

vars<-c(5,42:93)[!c(2,42:93) %in% c(42, 48, 53, 60, 64, 69, 78, 88)]


Output[,c(5,42, 48, 53, 60, 64, 69, 78, 88)] %>% 
  group_by(Topic) %>%
  summarise_all(mean) %>%
  ggradar(grid.max = 18, group.line.width = 1, group.point.size = .6, axis.label.size = 3, values.radar = c(0,10,20))
  
  