library(doParallel)
library(arrow)
library(FNN)
library(tidyverse)
cm <- read_parquet('cm_missing2.parquet')
s <- 1
cm2 <- cm %>%
  group_by(country_id) %>%
  arrange(month_id) %>%
  mutate(target_month_id = month_id + s,
         ln_ged_sb_target = lead(ln_ged_sb,s)) %>%
  ungroup() %>%
  filter(target_month_id>=121,target_month_id<=480) %>%
  select(1:14,18:29,35,33,34,-contains('pop'))
cm2_train <- cm2 %>% filter(target_month_id>=121, target_month_id<445) %>% na.omit()

cm2_test <- cm2 %>% filter(target_month_id>=445,target_month_id<=480) %>% na.omit()

f1 <- as.formula(paste("ln_ged_sb_target ~ ",paste(colnames(cm2)[1:23],collapse='+')))

tictoc::tic()
tt <- run_genetic_knn(f1,train = cm2_train,test=cm2_test,k = 7,gene_dist = seq(from=0, to=1, by=0.05),n_individuals = 100,n_keep = 10,n_discard = 10,n_generations = 10)
tictoc::toc()

