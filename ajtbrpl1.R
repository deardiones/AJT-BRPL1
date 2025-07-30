# acceptability scale analysis
# brpl1 group
# data treatment

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

# two new columns were added in the original results_prod.csv file.
# the 'Judgment' column was added to embrace the scale naming for each number
# the 'Number' column was added to the number variable.

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

# in case it did not open, instal the function: install.packages("") 

# folder path

setwd("~/Documents/Thesis Dissertation/Data Analysis/AJT (BrPL1) Acceptability Scale")

# load function "READ.PCIBEX"

read.pcibex <- function(filepath, auto.colnames=TRUE, fun.col=function(col,cols){cols[cols==col]<-paste(col,"Ibex",sep=".");return(cols)}) {
  n.cols <- max(count.fields(filepath,sep=",",quote=NULL),na.rm=TRUE)
  if (auto.colnames){
    cols <- c()
    con <- file(filepath, "r")
    while ( TRUE ) {
      line <- readLines(con, n = 1, warn=FALSE)
      if ( length(line) == 0) {
        break
      }
      m <- regmatches(line,regexec("^# (\\d+)\\. (.+)\\.$",line))[[1]]
      if (length(m) == 3) {
        index <- as.numeric(m[2])
        value <- m[3]
        if (is.function(fun.col)){
          cols <- fun.col(value,cols)
        }
        cols[index] <- value
        if (index == n.cols){
          break
        }
      }
    }
    close(con)
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=cols))
  }
  else{
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=seq(1:n.cols)))
  }
}

# read the data

dataajt1 <- read.pcibex("results_prod.csv")

# adding columns 

# column named 'Judgment'

dataajt1 <- dataajt1 (Value = c(1, 2, 3, 4, 5))

dataajt1 <- dataajt1 %>%
  mutate(Judgment = case_when(
    Value == 1 ~ "Totally Unspeakable",
    Value == 2 ~ "Unspeakable",
    Value == 3 ~ "Neutral",
    Value == 4 ~ "Speakable",
    Value == 5 ~ "Totally Speakable",
    TRUE ~ NA_character_  # Handles unexpected values
  ))

# column named 'Number'

dataajt1 <- dataajt1(condition = c("CE+P", "CNE+P", "CE+S", "CNE+S"))

dataajt1 <- dataajt1 %>%
  mutate(Number = case_when(
    condition == "CE+P" ~ "P",
    condition == "CNE+P" ~ "P",
    condition == "CE+S" ~ "S",
    condition == "CNE+S" ~ "S",
    TRUE ~ NA_character_  # Handles unexpected values
  ))

# selecting the relevant columns

dataajt1 <- dataajt1 %>%
  filter(Label %in% "target",
         Parameter == "Choice") %>%
  dplyr::select(ParticipantID, group, item, noun, condition, Judgment, Number, Value)

# removing participants X90 & Q171

participant_to_remove <- "ParticipantID_X90"
dataajt1 <- dataajt1[dataajt1$ParticipantID != "X90" , ]
participant_to_remove <- "ParticipantID_Q171"
dataajt1 <- dataajt1[dataajt1$ParticipantID != "Q171" ,]

# check the data

View(dataajt1)

# transforming 

dataajt1 <- dataajt1 %>% mutate_if(sapply(dataajt1, is.character), as.factor)

# inspecting

str(dataajt1)

# renaming

dataajt1 <- rename(dataajt1, Participant = ParticipantID)
dataajt1 <- rename(dataajt1, Item = item)
dataajt1 <- rename(dataajt1, Condition = condition) 
dataajt1 <- rename(dataajt1, Noun = noun)

# recodings

dataajt1$Noun <- recode_factor(dataajt1$Noun, CE = "E", CNE = "U")

dataajt1$Condition <- recode_factor(dataajt1$Condition, 
                                 `CE+S` = "E+S",  # edible singular
                                 `CE+P` = "E+P",  # edible plural
                                 `CNE+P` = "U+P", # non-edible plural
                                 `CNE+S` = "U+S") # non-edible singular

# checking the structure

str(dataajt1)

# reordering the columns

dataajt1 <- dataajt1 %>%
  dplyr::select(Participant, group, Item, Noun, Number, Condition, Value, Judgment)

# checking the levels of the variable 'Judgment'

levels(dataajt1$Judgment) 

# [1] "Neutral"             "Speakable"           "Totally Speakable"   "Totally Unspeakable" "Unspeakable"    

# organizing it in the scale manner

dataajt1$Judgment <- factor(dataajt1$Judgment, levels=c('Totally Unspeakable', 'Unspeakable', 'Neutral', 'Speakable',
                                                      "Totally Speakable"))

# checking the order

levels(dataajt1$Judgment) 

# [1] "Totally Unspeakable" "Unspeakable"         "neutral"             "speakable"           "Totally Speakable" 

# organizing the dependent variable (Value) as a factor 

dataajt1$Value <- factor(dataajt1$Value, levels=c('1', '2', '3', '4', '5')) 

# checking the organization

levels(dataajt1$Value)

# [1] "1" "2" "3" "4" "5"

# summary 

summary(dataajt1)

# Participant  group        Item       Noun    Number  Condition Value                  Judgment  
# A100   : 16   A:240   Min.   : 1.00   E:480   P:480   E+S:240   1: 69   Totally Unspeakable: 69  
# A29    : 16   B:288   1st Qu.: 4.75   U:480   S:480   E+P:240   2: 94   Unspeakable        : 94  
# A32    : 16   C:240   Median : 8.50                   U+P:240   3: 79   Neutral            : 79  
# B123   : 16   D:192   Mean   : 8.50                   U+S:240   4:196   Speakable          :196  
# C16    : 16           3rd Qu.:12.25                             5:522   Totally Speakable  :522  
# C71    : 16           Max.   :16.00                                                              
# (Other):864

summary(dataajt1$Value) 

# results
# 1   2   3   4   5 
# 69  94  79 196 522

# converting the column 'Value' to numberic

dataajt1$Value <- as.numeric(as.character(dataajt1$Value))

# table per condition
# creating the first table with the independent variables and the mean for judgment
# IMPORTANT: this table should be reported

tab1 <- aggregate(Value ~ Condition, data= dataajt1, mean)

# tabling the means and standard deviation
# IMPORTANT: should it be reported?

Value = dataajt1 %>%
  group_by(Condition) %>% 
  summarise(mean = mean(Value),
            standard.deviation = sd(Value))

# table per variable
# noun

aggregate(Value ~ Noun, data = dataajt1, mean)

#       Noun          Value
# 1     Edible        4.254167
# 2     Non-Edible    3.845833

# number

aggregate(Value ~ Number, data = dataajt1, mean)

#         Number      Value
# 1       Plural      4.375
# 2       Singular    3.725

# judgment per participant

dataajt1 %>% 
  group_by(Participant) %>%
  summarise(mean = mean(Value)) %>%
  arrange(desc(mean)) # decrescent order per participant

# judgment per item

dataajt1 %>%
  group_by(Item) %>%
  summarise(mean = mean(Value)) %>%
  arrange(desc(mean)) # decrescent order per item

# table 2

tab2 <- ftable(xtabs(~ Condition + Judgment , data = dataajt1))

tab2

# Judgment            Totally Unspeakable Unspeakable Neutral Speakable Totally Speakable
# Condition                                                                             
# E+S                 13                  24          18      50        135
# E+P                 10                  6           18      54        152
# U+P                 11                  10          15      48        156
# U+S                 35                  54          28      44        79

# table 2 in percentage

tab2_prop <-prop.table(tab2, 1)*100

tab2_prop

# Judgment                      Totally Unspeakable Unspeakable   Neutral   Speakable   Totally Speakable
# Condition                                                                               
# E+S                           5.416667            10.000000     7.500000  20.833333   56.250000
# E+P                           4.166667            2.500000      7.500000  22.500000   63.333333
# U+P                           4.583333            4.166667      6.250000  20.000000   65.000000
# U+S                          14.583333            22.500000     11.666667 18.333333   32.916667

# histogram (there are two pages of graphs per condition)

histogram(~ Value | Condition , data = dataajt1,layout = c(1,3))

# histogram (a better one)

histogram(~ Value | Condition, data = dataajt1, col = c("lightpink", "lightyellow", "lightcyan", "lightblue", "lightgreen"), layout=c(3,3))

# histogram (the best one)

histogram(~ Value | Condition, 
          data = dataajt1, 
          col = c("black"), 
          layout = c(2, 2),  # Adjust the grid layout (2x2 in this case)
          scales = list(
            x = list(at = c(1, 2, 3, 4, 5), labels = c("1", "2", "3", "4", "5")), 
            y = list(relation = "free")  # Use "same" for consistent y-axis across panels
          ), 
          main = "Speakability by Condition", 
          xlab = "Rate", 
          ylab = "Speakability in %")

# save the treated data

write.csv(dataajt1, "~/Documents/Thesis Dissertation/Data Analysis/AJT (BrPL1) Acceptability Scale/dataajt1.csv")

# plotting a better graph with the full labels

# Update the factor levels for Condition
dataajt1$Condition <- factor(dataajt1$Condition, 
                             levels = c("U+P", "U+S", "E+P", "E+S"),  # Original labels
                             labels = c("Non-edible+Plural", "Non-edible+Singular", "Edible+Plural", "Edible+Singular"))  # New labels

dataajt1$Condition <- factor(dataajt1$Condition, levels = c("Edible+Plural", "Non-edible+Plural", "Edible+Singular", "Non-edible+Singular"))

histogram(~ Value | Condition, 
          data = dataajt1, 
          col = c("black"), 
          layout = c(2, 2),  # Adjust the grid layout (2x2 in this case)
          scales = list(
            x = list(at = c(1, 2, 3, 4, 5), labels = c("1", "2", "3", "4", "5")), 
            y = list(relation = "same")  # Use "same" for consistent y-axis across panels
          ), 
          main = "Speakability by Condition", 
          xlab = "Rate", 
          ylab = "Speakability in %")

# save the final graph

