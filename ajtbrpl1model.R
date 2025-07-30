# acceptability scale inferencial analysis
# brpl1 group
# statistical analysis - cumulative link mixed model fitted with the laplace approximation

# independent variables
# number: S (singular) e P (plural)
# noun: E (edibles) e U (non-edibles)
# dependent variables
# acceptability scale (ordinal) & response time (continuous)

# scale from 1 to 5
# 1 totally unacceptable  / totally unspeakable 
# 2 unacceptable          / unspeakable
# 3 neutral               / neutral
# 4 acceptable            / unspeakable
# 5 totally acceptable    / totally unspeakable 

# experimental conditions
# edible + singular:     e + s (banana)
# edible + plural:       e + p (bananas)
# unedible + singular:   u + s (log)
# unedible + plural:     u + p (logs)

# open packages

library(dplyr)
library(effects)
library(ordinal)
library(arm)
library(lattice)

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

# working directory

setwd("~/Documents/Thesis Dissertation/Data Analysis/AJT (BrPL1) Acceptability Scale")

# bringing the treat data

dataajt1 <- read.csv("dataajt1.csv")

str(dataajt1)

# cleaning up unnecessary columns

dataajt1 <- dataajt1 %>% dplyr::select(-X)

# setting 'Value' as a numeric factor

dataajt1$Value <- as.numeric(dataajt1$Value)
dataajt1 <- dataajt1 %>% mutate_if(sapply(dataajt1, is.character), as.factor)
dataajt1$Item <- as.factor(dataajt1$Item)
dataajt1$Value <- factor(dataajt1$Value, ordered = TRUE)

# establishing the reference values

# 'S' (singular) as the reference level for the variable 'Number'

dataajt1$Number = relevel(dataajt1$Number, ref = "S")

# 'E' (edible) as the reference value for the variable 'Noun'

dataajt1$Noun = relevel(dataajt1$Noun, ref = "E")

# E+S (edible+singular) as the reference value for the variable 'Condition'
 
dataajt1$Condition = relevel(dataajt1$Condition, ref = "E+S")

# consulting the reference level of the dependent variables

levels(dataajt1$Noun)
levels(dataajt1$Number)
levels(dataajt1$Condition)

# model1 with the interaction between noun and number
# fixed effects (independent variables)
# random effects (participants and items)
# value is the dependent variable

model0 <- clmm(Value ~ Condition + (1 | Participant) + (1 | Item), data = dataajt1)

summary(model0)

model1 <- clmm(Value ~ Noun * Number + (1 | Participant) + (1 | Item), data = dataajt1)

summary(model1)

model2 <- clmm(Value ~ Noun + Number + (1 | Participant) + (1 | Item), data = dataajt1)

summary(model2)

anova (model1, model2)

# model 1 is better

# tabbing the model

tab_model(model1)

# post.hoc

post.hoc = emmeans(model1, ~ Noun * Number)

pairs(post.hoc, adjust="tukey")

pairs(post.hoc, adjust="bonferroni")

# reoganizing the conditions to display in a certain manner

dataajt1$Condition <- factor(dataajt1$Condition, levels = c("E+S", "U+S", "E+P", "U+P"))

# adding the full labels

dataajt1$Noun <- factor(dataajt1$Noun, 
                        levels = c("E", "U"),  # Original labels
                        labels = c("Edible", "Non-Edible"))  # New labels

dataajt1$Number <- factor(dataajt1$Number, 
                          levels = c("S", "P"),  # Original labels
                          labels = c("Singular", "Plural"))  # New labels

# plotting the graph first graph with the variables

plot(allEffects(model1), multiline = TRUE)

# plotting a graph that highlights the interaction isolating rate 5
# mention in the text that it is to further explore the interaction

eff_inter <- Effect(c("Noun", "Number"), model1)

plot(eff_inter, multiline = TRUE)

eff_df <- as.data.frame(eff_inter)

str(eff_df)

library(ggplot2)

ggplot(eff_df, aes(x = Noun, y = prob.X1, color = Number, group = Number)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  labs(
    x = "Noun Type",
    y = "Probability of getting speakability rate 1",
    color = "Number"
  ) +
  theme_minimal() +
  theme(legend.position = "top")


# further exploring threshold values
# transforming the values (we use the estimate)

invlogit(c(-4.0417, -2.6918, -1.9035, -0.4112)) 

# result

# [1] 0.01726429 0.06345896 0.12971286 0.39862442

# Totally unspeakable 0.8702871
# Unspeakable         0.01726429
# Neutral             0.06345896
# Speakable           0.12971286
# Totally speakable   0.39862442

# to calculate the probability of getting a 'totally speakable' occurence, we
# use the formula: 1 - the number we got for speakable.

1 - 0.12971286

# result
# 0.8702871
