# acceptability response time
# brpl1 group
# statistical analysis of log values

# independent variables
# number: S (singular) e P (plural)
# noun: E (edibles) e U (non-edibles)
# dependent variables
# acceptability scale (ordinal) & response time (continuous)

# response time
# there are two response time:
# the first, is the time taken to read the whole sentence. we cant use it since
# the sentences do not have the same number of words across all of the target ones
# the sencond response time is related to the time participants' took to select
# from 1 to 5 their choice. This is the important response time for this study.

# experimental conditions
# edible + singular:     e + s (banana)
# edible + plural:       e + p (bananas)
# unedible + singular:   u + s (log)
# unedible + plural:     u + p (logs)

# open packages

library(tidyverse)
library(dplyr)
library(png)
library(ggplot2)
library(nortest)
library(som)
library(lattice)
library(effects)
library(sjPlot)
library(car)
library(lme4)
library(lmerTest)
library(rms)
library(ordinal)
library(emmeans)

# in case it did not open, install the function: install.packages("") 

# folder path

setwd("~/Documents/Thesis Dissertation/Data Analysis/AJT (BrPL1) Response Time")

# bringing the treat data

datart <- read.csv("datart3.csv")

str(datart)

# cleaning up unnecessary columns

datart <- datart %>% dplyr::select(-X)

# renaming

datart$Response.time <- as.numeric(datart$log)
datart <- datart %>% mutate_if(sapply(datart, is.character), as.factor)
datart$Item <- as.factor(datart$Item)

# 'S' (singular) as the reference level for the variable 'Number'

datart$Number = relevel(datart$Number, ref = "S")

# 'E' (edible) as the reference value for the variable 'Noun'

datart$Noun = relevel(datart$Noun, ref = "E")

# consulting the reference level of the dependent variables

levels(datart$Noun)
levels(datart$Number)

# model 1 with interaction between the quantifier and noun
# dependent variable log

model1 <- lmer(log ~ Number * Noun + (1|Participant) + (1|Item), data = datart, REML = FALSE)

summary(model1)

model2 <- lmer(log ~ Number + Noun + (1|Participant) + (1|Item), data = datart, REML = FALSE)

summary(model2)

anova (model1, model2)

model3 <- lmer(log ~ Number + (1|Participant) + (1|Item), data = datart, REML = FALSE)

summary(model3)

anova(model1, model3)

anova (model2, model3)

model4 <- lmer(log ~ Noun + (1|Participant) + (1|Item), data = datart, REML = FALSE)

summary(model4)

anova (model1, model4)

model0 <- lmer(log ~ (1|Participant) + (1|Item), data = datart, REML = FALSE)

summary (model0)

anova (model3, model4)

tab_model(model4)

# the best model is 3 over 1. its in the limit of significance

anova (model2,model4)

# the best model is 4 over 1. its in the limit of significance

# post-hoc 

post.hoc = emmeans(model2, ~ Number*Noun)

pairs(post.hoc, adjust="tukey")

pairs(post.hoc, adjust="bonferroni")

# plotting

tab_model(model4)

# ploting the graph with the interaction in log

plot(allEffects(model3), 
     grid = TRUE, 
     multiline = TRUE, 
     main = "Values Predicted by the Model", 
     colors = c("#FA8072", "#26BCC9")) 

# plotting a graph with the full labels

# Check if the column is already a factor

if (!is.factor(datart$Number)) {
  datart$Number <- as.factor(datart$Number)  # Convert to factor if it's not already
}

if (!is.factor(datart$Noun)) {
  datart$Noun <- as.factor(datart$Noun)  # Convert to factor if it's not already
}

# Rename the levels

levels(datart$Number) <- c("Singular", "Plural")
levels(datart$Noun) <- c("Non-edible", "Edible")

# plotting

plot(allEffects(model3), 
     grid = TRUE, 
     main = "Values Predicted by the Model",
     ylab = "Log",
     colors = c("#FA8072", "#26BCC9")) 

