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
ivs <- 1:23
dv <- 24
idvar <- 25:26
tvar <- 26
cvar <- 25
cm2_train <- cm2 %>% filter(target_month_id>=121, target_month_id<445) %>% na.omit()

cm2_test <- cm2 %>% filter(target_month_id>=445,target_month_id<=480) %>% na.omit()


true_mse_knn <- mse_calc(cm2_train[-boot_ids,]$ln_ged_sb_target,
                         knn.reg(as.matrix(cm2_train[boot_ids,ivs]  %>% apply(2,standardize_vector))%*% weights[[1]],as.matrix(cm2_train[-boot_ids,ivs]) %>% apply(2,standardize_vector),y=as.matrix(cm2_train[boot_ids,dv]),k=13)$pred)



true_knn <- knn.reg(as.matrix(cm2_train[,ivs]) %>% apply(2,standardize_vector),as.matrix(cm2_test[,ivs]) %>% apply(2,standardize_vector),y=as.matrix(cm2_train[,dv]),k=13)
true_mse_knn <- mse_calc(cm2_test$ln_ged_sb_target,true_knn$pred)


genetic_weight_best <- run_genetic(n_individuals=10,n_discard=1,n_keep=1,,k=13,ivs=ivs,dv=dv,data=cm2_train,evaluate_best_on = cm2_test,n_gens = 20)

genetic_weight_best_old <- run_genetic(k=13,ivs=ivs,dv=dv,data=cm2_train,evaluate_best_on = cm2_test,n_gens = 100)




