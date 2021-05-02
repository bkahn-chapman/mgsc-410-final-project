
rm(list = ls())

# Class 2 ------------------------------------------------------------------

#plotting with ggplot2
library(tidyverse)

#scatterplot
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(color="red", shape = 6)+
  geom_smooth()+
  xlab("Engine Displacement")+
  ylab("Highway MPG")

# Try geom_boxplot() and geom_histogram()
# can always type help("geom_histogram")

ggplot(mpg, aes(group=cty, y=hwy))
geom_boxplot(color='red')

# Class 3 -----------------------------------------------------------------

library('tidyverse')

# the here package is very useful, it allows us to select across folders relative to our "home"
# directory of the project
# note here::here allows us to use the here function in the here package without loading it 

# OR you can run the code below to 
fs::dir_create(here::here("datasets"))

# this downloads a file from the net and stores it in your datasets folder
download.file("https://raw.githubusercontent.com/jonhersh/MGSC310/master/datasets/IMDB_movies.csv", 
              here::here("datasets", "movies.csv"), 
              method = "curl", 
              replace = TRUE)

movies <- read.csv(here::here("datasets", "movies.csv"))

# let's summarize the data using the glimpse function
movies %>% glimpse() 

# SLICE: slice to view only the first 10 rows
movies %>% slice(1:10)

#arrange
movies_sub <- movies %>% 
  arrange(-budget) %>% 
  slice(1:10) %>% 
  select(budget, title_year, movie_title)

#using select, rename, and everything
movies_keys <- movies %>%  select(director_name, movie_title)
movies_actors <- movies %>% select(starts_with("actor"))
movies <- movies %>% select(director_name, movie_title, title_year, everything())
movies <- movies %>%  rename(director = director_name)

#filter
movies_big <- movies %>% filter(budget > 100000000) %>% 
  slice(1:10) %>% 
  arrange(-budget) %>% 
  select(budget, movie_title) %>% 
  print()

# is.factor
is.factor(movies_eng$language)

#is NA?
is.na(NA)

# For loop example (From Class 4)
for(i in 1:10){
  print(i)
}

# Class 4 -----------------------------------------------------------------
# select function
movies_keys <- movies %>%  select(director_name, movie_title)

# rename
movies <- movies %>%  rename(director = director_name)

# creating functions
print_names <- function(data_frame){
  print(names(data_frame))
} 

# duplicated rows
movies %>% duplicated() %>% sum()

# Example of cleaning a dataset code
movies_clean <- 
  movies %>% 
  distinct() %>% 
  mutate(budgetM = budget/1000000,
         grossM = gross/1000000,
         profitM = grossM - budgetM) %>%
  rename(director = director_name, 
         title = movie_title,
         year = title_year) %>% 
  relocate(title, year, country, director, budgetM, grossM, imdb_score) %>% 
  filter(budgetM < 400) 

#group by and summarize

director_avg <- 
  movies_clean %>% 
  group_by(director) %>%
    summarize(gross_avg_director = mean(grossM, na.rm = TRUE))

# Class 5 -----------------------------------------------------------------
#using the mutate command (example from Class 3)
library('magrittr')
movies %<>% mutate(budgetM = budget/1000000,
                   grossM = gross/1000000)

# Class 6 -----------------------------------------------------------------
# More helpful libraries
library('dplyr')
library('tidyr')
library('tidyverse')
library('ggridges')
library('gganimate')
library('forcats')

# Another ggplot example
ggplot(movies_clean %>% 
         mutate(country_simple = fct_lump(country, n = 10)), 
       aes(x = imdb_score, y = country_simple, fill = stat(x))) +
  scale_fill_viridis_c(name = "IMDB Score") + 
  geom_density_ridges_gradient()

# Class 7 -----------------------------------------------------------------
# remove all existing objects in memory
rm(list = ls())

# Linear model example
mod1 <- lm(hwy ~ displ + cyl, 
           data = mpg)

# summary example
summary(mod1)

# Linear regression graphics
library('sjPlot')
library('tidymodels')
# output a prettier table of results 
tab_model(mod1)
# output a plot of regression coefficients
plot_model(mod1)
# output a table of coefficients and their p-values, t-stats
tidy(mod1)

# Class 7 also has theoretical discussion in the lab / exercise portion

# Class 8 -----------------------------------------------------------------
# Estimating Regression Model with Factors
mpg <- mpg %>% 
  mutate(class = factor(class))
mod2 <- lm(hwy ~ displ + class,
           data = mpg)
#and other factor manipulation

# predictions
preds_new <- predict(mod4,
                     newdata = newX)

# Finding residuals
resids <- mod4$residuals
resids <- mpg$hwy - preds

# Dataframe and plot examples
results <- data.frame(
  preds = preds,
  true = mpg$hwy,
  resids = resids
)
ggplot(results, 
       aes(x = true, y = preds)) +
  geom_point(alpha = 1/2, size = 4) +
  geom_abline(color = "red") +
  xlim(10,40) + ylim(10,40)

# Class 9 -----------------------------------------------------------------
# log function w/ linear regression
mod1 <- lm(log(hwy) ~ log(displ) + year,
           data = mpg)

# train-test-split
set.seed(1818)
train_prop <- 0.8
mpg_split <- initial_split(mpg, prop = train_prop)

mpg_train <- training(mpg_split)
mpg_test <- testing(mpg_split)

# estimate a model using the training set
mod <- lm(hwy ~ year + displ, 
          data = mpg_train)

# generate in-sample (training) predictions
preds_train <- predict(mod, newdata = mpg_train)

# generate out-of-sample (test set) predictions
preds_test <- predict(mod, newdata = mpg_test)

#helpful tools and Dataframe example
library('yardstick')

# create a df
results_train <- data.frame(
  predicted = preds_train,
  actual = mpg_train %>% 
    filter(complete.cases(hwy, year, displ)) %>% 
    select(hwy),
  type = rep("train", length(preds_train))
) %>% 
  rename(`predicted` = 1, `actual` = 2, `type` = 3)

results_test <- data.frame(
  predicted = preds_test,
  actual = mpg_test %>% 
    filter(complete.cases(hwy, year, displ)) %>% 
    select(hwy),
  type = rep("test", length(preds_test))
) %>% 
  rename(`predicted` = 1, `actual` = 2, `type` = 3)

rmse(results_train, predicted, actual) 
rmse(results_test, predicted, actual)
mae(results_train, predicted, actual)
mae(results_test, predicted, actual)

metrics(results_train, predicted, actual)
metrics(results_test, predicted, actual)

# Class 10 -----------------------------------------------------------------
# Logit examples
# set family = binomial to set logistic function
logit_fit1 <- glm(default ~ student,
                  family = binomial,
                  data = Default)

# rounding thing for scientific notation 
options(scipen = 9)

#exponentiation
exp(logit_fit1$coefficients)

#confusion matrix
conf_mat(two_class_example, 
         truth = truth, 
         estimate = predicted)


results_logit <- data.frame(
  `truth` = as.factor(Default$default),
  `Class1` =  scores,
  `Class2` = 1 - scores,
  `predicted` = as.factor(ifelse(scores > 0.4,
                                 "Yes","No"))
)

cm <- conf_mat(results_logit, 
               truth = truth,
               estimate = predicted)

print(cm)
autoplot(cm, "heatmap")

# ROC Plots
library('plotROC')

p <- ggplot(results_logit, 
            aes(m = Class1, d = truth)) + 
  geom_roc(labelsize = 3.5, 
           cutoffs.at = 
             c(0.99,0.9,0.7,0.5,0.3,0.1,0)) +
  theme_minimal(base_size = 16)
print(p)
calc_auc(p)

roc_auc(results_logit, 
        truth = truth, 
        estimate = predicted)

# Class 11 -----------------------------------------------------------------
# logistic predictions, also in Class 10
logit_fit3 <-  glm(default ~ balance,
                   family = binomial,
                   data = Default)

scores <- predict(logit_fit3,
                  type = "response")

# Downsamping and Upsampling

library('ROSE')
data_rose_down <- ROSE(default ~., data = Default, 
                       N = 666, p = 1/2)

table(data_rose_down$data$default)

data_rose_up <- ROSE(default ~., 
                     data = Default, 
                     N = 12000, 
                     p = 1/2)
table(data_rose_up$data$default)

# logit downsampled model
logit_down <- glm(default ~ balance,
                  data= data_rose_down$data,
                  family = "binomial")
summary(logit_down)

# logit up-sampled
logit_up <- glm(default ~ balance,
                data= data_rose_up$data,
                family = "binomial")
summary(logit_up)

# vanilla logit
logit <- glm(default ~ balance,
             data = Default,
             family = "binomial")

# generate scores and class predictions
scores_down = predict(logit_down,
                      type = "response")

scores_up = predict(logit_up,
                    type = "response")

scores_reg = predict(logit,
                     type = "response")

class_down = ifelse(scores_down > 0.5,1,0)
class_up = ifelse(scores_up > 0.5,1,0)
class_reg = ifelse(scores_reg > 0.5,1,0)

# Class 12 -----------------------------------------------------------------
# Bootstrapping
B = 100 # number of bootstraped datasets
n_boot = 200 # size of each bootstrapped sample
coef_boot = NULL
for(b in 1:B){
  idx <- sample(1:nrow(Auto_sub), 
                size = n_boot, replace = TRUE)
  mod <- lm(mpg ~ displacement, 
            data = Auto_sub %>% slice(idx))
  coef_boot[b] <- mod$coefficients[2]  
}

mod_lm <- lm(mpg ~ displacement, 
             data = Auto_sub)

coef_boot <- data.frame(coef_boot = 
                          coef_boot)

ggplot(coef_boot, aes(x = coef_boot)) + 
  geom_histogram() +
  geom_vline(xintercept = mod_lm$coefficients[2], 
             color = "red")

#Leave one out cross-validation LOOCV
mods_LOOCV <- list()
preds_LOOCV <- NULL
for(i in 1:nrow(Auto)){
  mod = lm(mpg ~ .,
           data = Auto_sub %>% slice(-i))
  preds_LOOCV[i] <- predict(mod, newdata =
                              slice(Auto_sub,i))
  mods_LOOCV[[i]] <- mod
}

head(preds_LOOCV)

mod_insample <- lm(mpg ~ ., data = Auto_sub)

# compute RMSE LOOCV 
preds_DF <- data.frame(
  preds_LOOCV = preds_LOOCV,
  preds_insample = predict(mod_insample),
  true = Auto$mpg
)

# K-Fold cross validation
Auto_sub <- 
  mutate(Auto_sub,
         folds = createFolds(Auto_sub$mpg,
                             k = 10, list = FALSE)
  )

Auto_sub$folds

### K-Fold Cross Validation
nfolds <- 10
preds_10FoldCV_DF <- data.frame(
  folds = Auto_sub$folds,
  preds_10FoldCV = rep(NA,nrow(Auto_sub))
)

for(i in 1:nfolds){
  mod <- lm(mpg ~ ., 
            data = Auto_sub %>% 
              filter(folds != i))
  preds <- predict(mod, 
                   newdata = Auto_sub %>%
                     filter(folds == i))
  preds_10FoldCV_DF[preds_10FoldCV_DF$folds == i,"preds_10FoldCV"]  <- preds
}


preds_DF <- data.frame(
  preds_10FoldCV = preds_10FoldCV_DF$preds_10FoldCV,
  preds_DF  
)

RMSE(preds_DF$preds_10FoldCV,preds_DF$true)
RMSE(preds_DF$preds_LOOCV,preds_DF$true)
RMSE(preds_DF$preds_insample,preds_DF$true)

R2(preds_DF$preds_10FoldCV,preds_DF$true)
R2(preds_DF$preds_LOOCV,preds_DF$true)
R2(preds_DF$preds_insample,preds_DF$true)

# Class 13 -----------------------------------------------------------------
# more combined Libraries
set.seed(1818)
options(scipen = 9)
rm(list=ls())

library('tidyverse')
library('rsample')
library('glmnet')
library('glmnetUtils')
library('forcats')

# Ridge Model
ridge_mod <- cv.glmnet(hwy ~ .,
                       data = mpg_clean,
                       # note alpha = 0 sets ridge!  
                       alpha = 0)

# print the two model sugegsted values of lambda:
print(ridge_mod$lambda.min)
print(ridge_mod$lambda.1se)

# print coefficient using lambda.min
coef(ridge_mod, s = ridge_mod$lambda.min) %>% 
  round(3)

# print coefficient using lambda.1se
coef(ridge_mod, s = ridge_mod$lambda.1se) %>% 
  round(3)

# put into coefficient vector
ridge_coefs <- data.frame(
  `ridge_min` = coef(ridge_mod, s = ridge_mod$lambda.min) %>% 
    round(3) %>% as.matrix() %>% as.data.frame(),
  `ridge_1se` = coef(ridge_mod, s = ridge_mod$lambda.1se) %>% 
    round(3) %>% as.matrix() %>% as.data.frame()
) %>% rename(`ridge_min` = 1, `ridge_1se` = 2)

# use the plot function to see the MSE
# path as we vary lambda (the amount of penalization)
plot(ridge_mod)

### examine coefficient shrinkage path
library('coefplot')
coefpath(ridge_mod)

# Class 14 -----------------------------------------------------------------
# More libraries load
set.seed(1818)
options(scipen = 9)

library('tidyverse')
library('rsample')
library('glmnet')
library('glmnetUtils')
library('forcats')
library('broom') #this one is the one we added

# Lasso Model

# note cv.glmnet automatically performs k-fold cross-validation 
lasso_mod <- cv.glmnet(hwy ~ .,
                       data = mpg_clean,
                       # note alpha = 1 sets Lasso!
                       alpha = 1)


# Note that lasso estimates a series of models, one for 
# every value of lambda -- the amount of shrinkage

# print the two model sugegsted values of lambda:
print(lasso_mod$lambda.min)
print(lasso_mod$lambda.1se)

# plot how the MSE varies as we vary lambda
plot(lasso_mod)

# to examine the coefficients we must say what value of 
# lambda we want to use. 

# coefficients using lambda.1se
coef(lasso_mod, 
     s = lasso_mod$lambda.1se) %>% 
  round(3)

# put into coefficient vector
lasso_coefs <- data.frame(
  `lasso_min` = coef(lasso_mod, s = lasso_mod$lambda.min) %>%
    as.matrix() %>% data.frame() %>% round(3),
  `lasso_1se` = coef(lasso_mod, s = lasso_mod$lambda.1se) %>% 
    as.matrix() %>% data.frame() %>% round(3)
) %>%  rename(`lasso_min` = 1, `lasso_1se` = 2)

print(lasso_coefs)

coefpath(lasso_mod)

# Class 15 -----------------------------------------------------------------
# Elastic Model
enet_mod <- cva.glmnet(hwy ~ .,
                       data = mpg_clean,
                       alpha = seq(0,1, by = 0.05))

plot(enet_mod)

# now enet_mod holds a list with all of the sub models, 
# each with alpha = whatever sequence the model was estimated with

minlossplot(enet_mod, 
            cv.type = "min")


str(enet_mod$modlist)

# Use this function to find the best alpha
get_alpha <- function(fit) {
  alpha <- fit$alpha
  error <- sapply(fit$modlist, 
                  function(mod) {min(mod$cvm)})
  alpha[which.min(error)]
}

# Get all parameters.
get_model_params <- function(fit) {
  alpha <- fit$alpha
  lambdaMin <- sapply(fit$modlist, `[[`, "lambda.min")
  lambdaSE <- sapply(fit$modlist, `[[`, "lambda.1se")
  error <- sapply(fit$modlist, function(mod) {min(mod$cvm)})
  best <- which.min(error)
  data.frame(alpha = alpha[best], lambdaMin = lambdaMin[best],
             lambdaSE = lambdaSE[best], eror = error[best])
}

# extract the best alpha value and model parameters
best_alpha <- get_alpha(enet_mod)
print(best_alpha)
get_model_params(enet_mod)

# extract the best model object
best_mod <- enet_mod$modlist[[which(enet_mod$alpha == best_alpha)]]
