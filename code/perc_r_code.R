library(dplyr)
library(tidyr)

perc_data <- read.csv("data/PERC.S2.csv")

perc_final<-perc_data %>%
  #puzzle scores, missing values are coded as incorrect.
  mutate(t1p1_correct = if_else(t1p1 == 1, 1, 0),
         t1p1_correct = coalesce(t1p1_correct, 0),
         t1p2_correct = if_else(t1p2 == 3, 1, 0),
         t1p2_correct = coalesce(t1p2_correct, 0),
         t1p3_correct = if_else(t1p3 == 6, 1, 0),
         t1p3_correct = coalesce(t1p3_correct, 0),
         t1p4_correct = if_else(t1p4 == 6, 1, 0),
         t1p4_correct = coalesce(t1p4_correct, 0),
         t1pp1_correct = if_else(t1pp1 == 4, 1, 0),
         t1pp1_correct = coalesce(t1pp1_correct, 0),
         t1pp2_correct = if_else(t1pp2 == 5, 1, 0),
         t1pp2_correct = coalesce(t1pp2_correct, 0),
         t1pp3_correct = if_else(t1pp3 == 1, 1, 0),
         t1pp3_correct = coalesce(t1pp3_correct, 0),
         t1tp1_correct = if_else(t1tp1 == 8, 1, 0),
         t1tp1_correct = coalesce(t1tp1_correct, 0),
         t1tp2_correct = if_else(t1tp2 == 5, 1, 0),
         t1tp2_correct = coalesce(t1tp2_correct, 0),
         t1tp3_correct = if_else(t1tp3 == 4, 1, 0),
         t1tp3_correct = coalesce(t1tp3_correct, 0),
         t1tp4_correct = if_else(t1tp4 == 8, 1, 0),
         t1tp4_correct = coalesce(t1tp4_correct, 0),
         t1tp5_correct = if_else(t1tp5 == 4, 1, 0),
         t1tp5_correct = coalesce(t1tp5_correct, 0),
         t1tp6_correct = if_else(t1tp6 == 5, 1, 0),
         t1tp6_correct = coalesce(t1tp6_correct, 0),
         t1tp7_correct = if_else(t1tp7 == 8, 1, 0),
         t1tp7_correct = coalesce(t1tp7_correct, 0),
         t1tp8_correct = if_else(t1tp8 == 3, 1, 0),
         t1tp8_correct = coalesce(t1tp8_correct, 0),
         #replacing NAs with 0 in challenge-seeking
         t1difchc = coalesce(t1difchc, 0),
         #baseline puzzle-solving ability.
         set1_scoreraw = (t1p1_correct + t1p2_correct + t1p3_correct + t1p4_correct)) %>%
  rowwise() %>% 
  #effort and persistence sum
  mutate(eiottime = (sum(t1pp1t1_page_submit, t1pp1t2_page_submit, t1pp2t1_page_submit,t1pp2t2_page_submit,t1pp2t1_page_submit_1, t1pp3t2_page_submit, na.rm = TRUE)),  
         perssec = (sum(t1tp2time, t1tp3time, t1tp4time, t1tp5time, na.rm = TRUE))) %>% 
  ungroup() %>% 
  #effort and persistence square root transformation
  mutate(eiottimetrans = sqrt(eiottime),
         perstrans = sqrt(perssec),
         #effort and persistence winsorization
         ewd = replace(eiottimetrans, (eiottimetrans > (mean(eiottimetrans)) + (3*sd(eiottimetrans))), (mean(eiottimetrans))+(3*sd(eiottimetrans))),
         pers_wd = replace(perstrans, perstrans > (mean(perstrans))+(3*sd(perstrans)), (mean(perstrans))+(3*sd(perstrans))),
         #effort and persistence normalization
         effort = (ewd - min(ewd))/(max(ewd)-(min(ewd))),
         persistence = (pers_wd - min(pers_wd))/(max(pers_wd)-(min(pers_wd))),
         #sum resilience
         resilience = (t1tp6_correct + t1tp7_correct + t1tp8_correct)/3,
         #composite perc score
         perc_score = (t1difchc + effort + persistence + resilience))




