devtools::load_all() ## Load the genetic.invariant package
library(tidyverse)
library(doParallel)


adult <- adult %>% mutate(us_born = case_when(native_country == 'United-States' ~ 1, T ~ 0)) ## Load the 'adult' dataset and make some modifications

trainid <- sample(1:nrow(adult),30000) # Make a random split into test and train data
adult_train <- adult[trainid,]
adult_test <- adult[-trainid,]

## Calculate the MSE for the test data using regular kNN on standardized data
true_mse_adult <- MLmetrics::MSE(knn.reg_formula(wage_class_num ~ age+education_num+relationship+capital_gain+capital_loss+hours_per_week+us_born,
                train = adult_train,
                test = adult_test,
                k=3,
                standardize=TRUE)$pred,adult_test$wage_class_num)

## Calculate the loo-MSE for the training data using regular kNN on standardized data
true_mse_adult_is <- MLmetrics::MSE(knn.reg_formula(wage_class_num ~ age+education_num+relationship+capital_gain+capital_loss+hours_per_week+us_born,
                                                 train = adult_train,
                                                 k=3)$pred,adult_train$wage_class_num)

## Use multiple cores to speed up computation
registerDoParallel(cores=10)

## Run the genetic algorithms. Try plaing around with the arguments, the arguments are documented in ?run_genetic_knn()
gknn_adult <- run_genetic_knn(train = adult_train,test = adult_test,
                              formula = wage_class_num ~ age+education_num+relationship+capital_gain+capital_loss+hours_per_week+us_born,
                              k = 3,
                              gene_dist = 'unif',n_generations = 100,eval_gens = 'loocv')

gknn_adult_oob <- run_genetic_knn(train = adult_train,test = adult_test,
                              formula = wage_class_num ~ age+education_num+relationship+capital_gain+capital_loss+hours_per_week+us_born,
                              k = 3,
                              gene_dist = 'unif',n_generations = 100,eval_gens = 'oob')



### This provides a plot for performance in- and out of sample across generations
tt <- tibble(x = 1:100,
             loocv_train = map_dbl(gknn_adult,'score_ensemble_train'),
             loocv_test = map_dbl(gknn_adult,'score_ensemble_test'),
             oob_train = map_dbl(gknn_adult_oob,'score_ensemble_train'),
             oob_test = map_dbl(gknn_adult_oob,'score_ensemble_test'))

tt %>% pivot_longer(2:5) %>%
  ggplot(aes(x=x,y=value,color=name))+
  geom_line()+
  geom_hline(yintercept = true_mse_adult,lty=2)+
  geom_hline(yintercept = true_mse_adult_is,lty=2)

tt2 <- tibble(x = 1:100,
              cosine_loo = 1:100 %>% map_dbl(~mean(lsa::cosine(reduce(gknn_adult[[.x]]$weights[1:10],cbind))[lower.tri(lsa::cosine(reduce(gknn_adult[[.x]]$weights[1:10],cbind)))])),
              cosine_oob = 1:100 %>% map_dbl(~mean(lsa::cosine(reduce(gknn_adult_oob[[.x]]$weights[1:10],cbind))[lower.tri(lsa::cosine(reduce(gknn_adult_oob[[.x]]$weights[1:10],cbind)))])))


tt2 %>% pivot_longer(2:3) %>%
  ggplot(aes(x=x,y=value,color=name))+
  geom_smooth()


