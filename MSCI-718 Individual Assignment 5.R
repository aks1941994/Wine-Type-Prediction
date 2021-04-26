#Calling libraries

library(dplyr)
library(tidyverse)
library(ggplot2)
library(caret)
library(ggm)
library(rcompanion)
#Loading datasets
setwd("E:/PostGrad/UW Winter Term/MSCI 718/Assignments/Individual Assignment-5")
Red_Wine <- read_csv2("winequality-red.csv",skip_empty_rows = TRUE)
White_Wine <- read_csv2("winequality-white.csv",skip_empty_rows = TRUE)

#Wrangling the datasets

summary(is.na(White_Wine))
summary(is.na(Red_Wine))
Red_Wine <- remove_missing(Red_Wine)

Red_Wine <- Red_Wine %>%
  mutate(`volatile acidity` = as.double(`volatile acidity`),
         `citric acid` = as.double(`citric acid`),
         chlorides = as.double(chlorides),
         density = as.double(density),
         sulphates = as.double(sulphates))
Red_Wine <- Red_Wine %>% add_column(Wine_Type = "Red Wine",.before = 1)


White_Wine <- White_Wine %>%
  mutate(`volatile acidity` = as.double(`volatile acidity`),
         `citric acid` = as.double(`citric acid`),
         chlorides = as.double(chlorides),
         density = as.double(density),`residual sugar` = as.double(`residual sugar`),
         sulphates = as.double(sulphates))
White_Wine <- White_Wine %>% add_column(Wine_Type = "White Wine",.before = 1)

Wines_copy <- merge(Red_Wine, White_Wine,all.x = TRUE,all.y = TRUE)
Wines <- merge(Red_Wine, White_Wine,all.x = TRUE,all.y = TRUE)
Wines <- Wines %>% mutate(Wine_Type = as.factor(Wine_Type))

#Visualizing Data

ggplot(Wines_copy, aes(y=Wine_Type, x=`volatile acidity`)) + geom_jitter(width=0.1, height=0.25) 
ggplot(Wines_copy, aes(y=Wine_Type, x=`residual sugar`)) + geom_jitter(width=0.1, height=0.25) 
ggplot(Wines_copy, aes(y=Wine_Type, x=quality)) + geom_jitter(width=0.1, height=0.25)
ggplot(Wines_copy, aes(y=Wine_Type, x=sulphates)) + geom_jitter(width=0.1, height=0.25)

#Removing Outliers
ggplot(data = Wines, aes(x = `fixed acidity`)) + geom_boxplot(outlier.colour = 'Red') + coord_flip()
Wines <- Wines %>% filter(`fixed acidity` < 100 & `fixed acidity` >40)
ggplot(data = Wines, aes(x = `volatile acidity`)) + geom_boxplot(outlier.colour = 'Red') + coord_flip()
Wines <- Wines %>% filter(`volatile acidity` < 0.7)
ggplot(data = Wines, aes(x = `residual sugar`)) + geom_boxplot(outlier.colour = 'Red') + coord_flip()
Wines <- Wines %>% filter(`residual sugar` < 30)
ggplot(data = Wines, aes(x = chlorides)) + geom_boxplot(outlier.colour = 'Red') + coord_flip()
Wines <- Wines %>% filter(chlorides < 0.09)
ggplot(data = Wines, aes(x = `free sulfur dioxide`)) + geom_boxplot(outlier.colour = 'Red') + coord_flip()
Wines <- Wines %>% filter(`free sulfur dioxide` < 80)
ggplot(data = Wines, aes(x = `total sulfur dioxide`)) + geom_boxplot(outlier.colour = 'Red') + coord_flip()
Wines <- Wines %>% filter(`total sulfur dioxide` < 300)
ggplot(data = Wines, aes(x = density)) + geom_boxplot(outlier.colour = 'Red') + coord_flip()
Wines <- Wines %>% filter(density < 1)
ggplot(data = Wines, aes(x = sulphates)) + geom_boxplot(outlier.colour = 'Red') + coord_flip()
Wines <- Wines %>% filter(sulphates < 0.9)

#Computing multiple logistic regression

model1 <- glm( Wine_Type ~ `fixed acidity` + `volatile acidity` + `citric acid`+
              `residual sugar`+ chlorides + `free sulfur dioxide` + `total sulfur dioxide`+
              density + pH + sulphates + alcohol + quality,
              data = Wines,family = binomial())
summary(model1)$coef

  model2 <- glm( Wine_Type ~ `volatile acidity` + `residual sugar`+ 
                 `total sulfur dioxide`+ sulphates + quality+ `free sulfur dioxide` ,
               data = Wines,family = binomial())

summary(model2)
confint(model2)
exp(confint(model2))
contrasts(Wines$Wine_Type)

#Testing assumptions

#Linearity Assumption
probabilities <- predict(model2, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "White", "Red")
head(predicted.classes)
mydata <- Wines %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)


ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#Multicollinearity

car::vif(model2)

#Independence of Errors

car::durbinWatsonTest(model2)

#Model Accuracy
nagelkerke(model2)

