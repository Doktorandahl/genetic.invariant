library(devtools)

use_package('dplyr')
use_readme_rmd()
use_mit_license()
use_package('MLmetrics')
load_all()
document()
use_package('foreach')
use_package('parallel')
use_package('FNN')
use_package('doParallel')

banknote <- read_csv("data_import/banknote.txt",
                          col_names = FALSE)
banknote <- set_names(banknote,c('variance','skewness','curtosis','entropy','class'))

cancer <- read_csv("data_import/breast-cancer-wisconsin.data",
                        col_names = FALSE)

cancer <- set_names(cancer, c('id','thickness','unif_size','unif_shape','adhesion','cell_size','nuclei','chromatin','nucleoli','mitoses','class'))

cancer <- cancer %>% mutate(nuclei = as.numeric(nuclei),
                            class = factor(class,levels = c(2,4),labels = c('benign','malign')))


heart <- read_csv("~/Downloads/processed.cleveland.data",
                       col_names = FALSE)

heart <- heart %>% set_names(c('age','ca','chol','cp','exang','fbs','num','oldspeak','restecg','sex','slope','thal','thalach','trestbps'))

heart <- heart %>% mutate(num = factor(num))

adult <- adult %>% set_names(c('age','workclass','wage','education','education_num','marital_status','occupation','relationship','race','sex','capital_gain','capital_loss','hours_per_week','native_country','wage_class')) %>%
  mutate(wage_class_num = case_when(wage_class == "<=50K" ~ 0,
                                    T ~ 1))
use_data(adult,overwrite = T)



usethis::use_data(heart)
usethis::use_data(cancer)
usethis::use_data(banknote)

