# acceptability response time
# brpl1 group
# data treatment

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

# in case it did not open, instal the function: install.packages("") 

# folder path

setwd("~/Documents/Thesis Dissertation/Data Analysis/AJT (BrPL1) Response Time")

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

datart <- read.pcibex("results_prod.csv")

# removing participants X90 & Q171

participant_to_remove <- "ParticipantID_X90"
datart <- datart[datart$ParticipantID != "X90" , ]
participant_to_remove <- "ParticipantID_Q171"
datart <- datart[datart$ParticipantID != "Q171" ,]

# check the data

View(datart)

# check the structure

str(datart)

# column named 'Number'

datart <- datart(condition = c("CE+P", "CNE+P", "CE+S", "CNE+S"))

datart <- datart %>%
  mutate(Number = case_when(
    condition == "CE+P" ~ "P",
    condition == "CNE+P" ~ "P",
    condition == "CE+S" ~ "S",
    condition == "CNE+S" ~ "S",
    TRUE ~ NA_character_  # Handles unexpected values
  ))

# selecting the relevant columns

datart <- datart %>%
  filter(Label == "target",
         Parameter %in% c("Click", "Choice") |
           PennElementName == "respostapractice") %>%
  dplyr::select(ParticipantID, group, Label, item, Parameter, condition, Number, noun, Value, EventTime) %>%
  mutate(EventTime = as.numeric(EventTime)) %>%  # Convert EventTime to numeric
  group_by(ParticipantID, item) %>%
  mutate(event = case_when(Parameter == "Choice" ~ "T1",
                           Parameter == "Click" ~ "T2"),
         selection = case_when(Value == "1" ~ "Click",
                               Value == "2" ~ "Click",
                               Value == "3" ~ "Click",
                               Value == "4" ~ "Click",
                               Value == "5" ~ "Click")) %>% 
  fill(selection, .direction = "up") %>%
  ungroup() %>%
  dplyr::select(-Parameter, -Value, -selection) %>% 
  pivot_wider(names_from = event, values_from = EventTime, values_fn = list(EventTime = first)) %>%  # Handle duplicates
  mutate(T1 = as.numeric(T1),    # Convert T1 to numeric
         T2 = as.numeric(T2),    # Convert T2 to numeric
         RT = T1 - T2)

# renaming

datart <- rename(datart, Participant = ParticipantID)
datart <- rename(datart, Item = item)
datart <- rename(datart, Condition = condition) 
datart <- rename(datart, Noun = noun)

# recodings

datart$Noun <- recode_factor(datart$Noun, CE = "E", CNE = "U")

datart$Condition <- recode_factor(datart$Condition, 
                                    `CE+S` = "E+S",  # edible singular
                                    `CE+P` = "E+P",  # edible plural
                                    `CNE+P` = "U+P", # non-edible plural
                                    `CNE+S` = "U+S") # non-edible singular

# response time mean
# IMPORTANT: it should be reported 

MeanRTdatart = datart %>%
  group_by(Condition) %>% 
  summarise(MeanRT = mean(RT))

# initial raw treatment finished, saving the files

write.csv(datart, "~/Documents/Thesis Dissertation/Data Analysis/AJT (BrPL1) Response Time/dataajtrt.csv")

write.csv(MeanRTdatart, "~/Documents/Thesis Dissertation/Data Analysis/AJT (BrPL1) Response Time/meanRTajtrt.csv")

################################################################################

# renaming

datart <- rename(datart, Response.time = RT)

# mutate columns

datart <- datart %>%
  dplyr::select(- T1, - T2, - Label)

# transforming

datart$Response.time <- as.numeric(datart$Response.time)
datart <- datart %>% mutate_if(sapply(datart, is.character), as.factor)
datart$Item <- as.factor(datart$Item)

# reordering the columns

datart <- datart %>%
  dplyr::select(Participant, group, Item, Noun, Number, Condition, Response.time)

# summary

summary(datart)

# Participant  group        Item     Noun    Number  Condition Response.time  
# A100   : 16   A:240   1      : 60   E:480   P:480   E+S:240   Min.   :  768  
# A29    : 16   B:288   2      : 60   U:480   S:480   E+P:240   1st Qu.: 1704  
# A32    : 16   C:240   3      : 60                   U+P:240   Median : 2632  
# B123   : 16   D:192   4      : 60                   U+S:240   Mean   : 3793  
# C16    : 16           5      : 60                             3rd Qu.: 4395  
# C71    : 16           6      : 60                             Max.   :65106  
# (Other):864           (Other):600                                    

# summary of response time

summary(datart$Response.time) 

#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 768    1704    2632    3793    4395   65106 

# reading time mean and standard deviation 
# IMPORTNT: report this raw means and sd)

ResponseTime = datart %>%
  group_by(Condition) %>% 
  summarise(Mean = mean(Response.time),
            StandardDeviation = sd(Response.time))

# mean and by variable

aggregate(Response.time ~ Number, data = datart, mean)

#         Number            Response.time
# 1       Plural            3727.298
# 2       Singular          3859.662

aggregate(Response.time ~ Noun, data = datart, mean)

#       Noun                Response.time
# 1     Edible              3785.10
# 2     U non-edible        3801.86

# mean per participant

datart %>%
  group_by(Participant) %>%
  summarise(Mean = mean(Response.time)) %>%
  arrange(desc(Mean)) # decescent order

# mean per item

datart %>%
  group_by(Item) %>%
  summarise(Mean = mean(Response.time)) %>%
  arrange(desc(Mean))

# plotting

# boxplot

ggplot(datart, aes(x = Condition, y = Response.time)) +
  geom_boxplot(fill = "lightblue") +
  stat_summary(color = "red", size = 0.2, shape = 3) + 
  labs(x = "Condition", y = "Response Time") + 
  theme_classic() 

# histogram

ggplot(datart, aes(x = Response.time)) + 
  geom_histogram(bins = 30, fill = "lightblue",
                 color = "blue") +
  labs(x = "Response Time",
       y = "Frequency") +
  theme_classic()

# qq 

ggplot(datart, aes(sample = Response.time)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "", y = "Response Time") +
  facet_wrap(~Condition) +
  theme_light()

# qq per participant

ggplot(datart, aes(sample = Response.time)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Participant) +
  labs(x = "", y = "") +
  theme_light()

# qq per item

ggplot(datart, aes(sample = Response.time)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Item) +
  labs(x = "", y = "") +
  theme_light()

# normality test

lillie.test(datart$Response.time) 

# D = 0.23631, p-value < 2.2e-16

shapiro.test(datart$Response.time)

# the data is not well distributed and neither normal. Proceeding for the first
# outlier cut with the interqualitic range

# calculating the superior limit

summary(datart$Response.time)  

#   Min.    1st Qu.   Median    Mean    3rd Qu.     Max. 
#   768     1704      2632      3793    4395        65106 

# formula: limitesuperior <- Q3 + 3*(Q3 - Q1) 

limitesuperior <- 4395 + 3*(4395 - 1704)

limitesuperior

# [1] 12468

# filtering the data with the upper limit 12468

datart2 <- datart %>% filter(Response.time < 12468)

summary(datart2)

# Participant  group        Item     Noun    Number  Condition Response.time  
# A100   : 16   A:235   4      : 60   E:466   P:466   E+S:233   Min.   :  768  
# A32    : 16   B:276   10     : 60   U:468   S:468   E+P:233   1st Qu.: 1688  
# B123   : 16   C:236   16     : 60                   U+P:233   Median : 2582  
# C16    : 16   D:187   1      : 59                   U+S:235   Mean   : 3292  
# C71    : 16           3      : 59                             3rd Qu.: 4176  
# D145   : 16           5      : 59                             Max.   :12091  
# (Other):838           (Other):577                                            

# plotting

# boxplot

ggplot(datart2, aes(x = Condition, y = Response.time)) +
  geom_boxplot(fill = "lightblue") +
  stat_summary(color = "red", size = 0.2, shape = 3) + 
  labs(x = "Condition", y = "Response Time") + 
  theme_classic() 

# histogram

ggplot(datart2, aes(x = Response.time)) + 
  geom_histogram(bins = 30, fill = "lightblue",
                 color = "blue") +
  labs(x = "Response Time",
       y = "Frequency") +
  theme_classic()

# qq 

ggplot(datart2, aes(sample = Response.time)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "", y = "Response Time") +
  facet_wrap(~Condition) +
  theme_light()

# qq per participant

ggplot(datart2, aes(sample = Response.time)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Participant) +
  labs(x = "", y = "") +
  theme_light()

# qq per item

ggplot(datart2, aes(sample = Response.time)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Item) +
  labs(x = "", y = "") +
  theme_light()

# normality test

lillie.test(datart2$Response.time) 

# D = 0.15329, p-value < 2.2e-16

shapiro.test(datart2$Response.time)

# W = 0.83793, p-value < 2.2e-16

# once again, the graphs show that there are still many outliers in the boxplot.
# the data is not well distributed. thus, and the normality tests does not show 
# that the data follows a normal distribution. 

# removing more outliers

# formula: limitesuperior <- Q3 + 1.5*(Q3 - Q1)

limitesuperior2 <- 4395 + 1.5*(4395 - 1704)

limitesuperior2

# [1] 8431.5

datart3 <- datart %>% filter(Response.time < 8431.5)

summary(datart3)

#   Participant  group        Item     Noun    Number  Condition Response.time 
# C16    : 16   A:229   10     : 59   E:444   P:444   E+S:223   Min.   : 768  
# C71    : 16   B:261   1      : 58   U:448   S:448   E+P:221   1st Qu.:1654  
# D145   : 16   C:225   3      : 58                   U+P:223   Median :2478  
# D185   : 16   D:177   5      : 57                   U+S:225   Mean   :2984  
# F108   : 16           8      : 57                             3rd Qu.:3905  
# G100   : 16           9      : 57                             Max.   :8386  
# (Other):796           (Other):546 

# plotting

# boxplot

ggplot(datart3, aes(x = Condition, y = Response.time)) +
  geom_boxplot(fill = "lightblue") +
  stat_summary(color = "red", size = 0.2, shape = 3) + 
  labs(x = "Condition", y = "Response Time") + 
  theme_classic() 

# histogram

ggplot(datart3, aes(x = Response.time)) + 
  geom_histogram(bins = 30, fill = "lightblue",
                 color = "blue") +
  labs(x = "Response Time",
       y = "Frequency") +
  theme_classic()

# qq 

ggplot(datart3, aes(sample = Response.time)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "", y = "Response Time") +
  facet_wrap(~Condition) +
  theme_light()

# qq per participant

ggplot(datart3, aes(sample = Response.time)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Participant) +
  labs(x = "", y = "") +
  theme_light()

# qq per item

ggplot(datart3, aes(sample = Response.time)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Item) +
  labs(x = "", y = "") +
  theme_light()

# normality test

lillie.test(datart3$Response.time) 

# D = 0.12796, p-value < 2.2e-16

shapiro.test(datart3$Response.time)

# W = 0.88313, p-value < 2.2e-16

# once again, the graphs show that there are still many outliers in the boxplot.
# the data is not well distributed. thus, and the normality tests does not show 
# that the data follows a normal distribution. 

# logarithmic transformation

datart3$log <- log(datart3$Response.time)

# plotting

# boxplot

ggplot(datart3, aes(x = Condition, y = log)) +
  geom_boxplot(fill = "lightblue") +
  stat_summary(color = "red", size = 0.2, shape = 3) + 
  labs(x = "Condition", y = "Response Time") + 
  theme_classic() 

# histogram

ggplot(datart3, aes(x = log)) + 
  geom_histogram(bins = 30, fill = "lightblue",
                 color = "blue") +
  labs(x = "Response Time",
       y = "Frequency") +
  theme_classic()

# qq 

ggplot(datart3, aes(sample = log)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "", y = "Response Time") +
  facet_wrap(~Condition) +
  theme_light()

# qq per participant

ggplot(datart3, aes(sample = log)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Participant) +
  labs(x = "", y = "") +
  theme_light()

# qq per item

ggplot(datart3, aes(sample = Response.time)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Item) +
  labs(x = "", y = "") +
  theme_light()

# normality test

lillie.test(datart3$log) 

# D = 0.057039, p-value = 3.163e-07

shapiro.test(datart3$log)

# W = 0.97792, p-value = 2.25e-10

# data improves with the log transformation. the distribution is better with no 
# outliers in the box plot. The test is still not normal with the W values below 1
# therefore i am preserving the log transformation.

# new means with after the log

ResponseTimeLog = datart3 %>%
  group_by(Condition) %>% 
  summarise(Mean = mean(Response.time),
            StandardDeviation = sd(Response.time))

# summary of the data after log

summary(datart3)

# Participant  group        Item     Noun    Number  Condition Response.time       log       
# C16    : 16   A:229   10     : 59   E:444   P:444   E+S:223   Min.   : 768   Min.   :6.644  
# C71    : 16   B:261   1      : 58   U:448   S:448   E+P:221   1st Qu.:1654   1st Qu.:7.411  
# D145   : 16   C:225   3      : 58                   U+P:223   Median :2478   Median :7.815  
# D185   : 16   D:177   5      : 57                   U+S:225   Mean   :2984   Mean   :7.851  
# F108   : 16           8      : 57                             3rd Qu.:3905   3rd Qu.:8.270  
# G100   : 16           9      : 57                             Max.   :8386   Max.   :9.034  
# (Other):796           (Other):546

# plotting the final graphs

grafico <- datart3 %>%  
  group_by(Number, Noun) %>%  
  summarise(media = mean(Response.time), se = sd(Response.time)/sqrt(n()))

ggplot(grafico, aes(x = Number, y = media, fill = Noun)) + 
  geom_col(alpha = 0.8, position = "dodge") + 
  geom_errorbar(position = position_dodge(width = 0.9),
                aes(ymax = media + se, ymin = pmax(0, media - se)), width = 0.25, alpha = 0.8) + 
  coord_cartesian(ylim = c(0, 4000)) +  
  scale_x_discrete(labels = c("S" = "Singular", "P" = "Plural")) +  # Map S and P to descriptive labels
  scale_fill_discrete(labels = c("E" = "Edible", "U" = "Non-edible")) +  # Map E and U to descriptive labels
  labs(x = "Number", y = "Response Time (ms)", fill = "Noun Type") +
  theme_light()

# linear model to check the residues

modelOFF = lm(log ~ Number + Noun, datart3)

summary(modelOFF)

# Call:
# lm(formula = log ~ Number + Noun, data = datart3)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.19232 -0.41324 -0.03985  0.40843  1.25784 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  7.77648    0.03141 247.594   <2e-16 ***
#   NumberS      0.05963    0.03616   1.649   0.0995 .  
#   NounU        0.08970    0.03616   2.481   0.0133 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.54 on 889 degrees of freedom
# Multiple R-squared:  0.009883,	Adjusted R-squared:  0.007656 
# F-statistic: 4.437 on 2 and 889 DF,  p-value: 0.01209

lillie.test(modelOFF$residuals)

# D = 0.060973, p-value = 2.576e-08

shapiro.test(modelOFF$residuals)

# W = 0.97901, p-value = 4.971e-10

# the residues are not normal

# analysing the residues

head(modelOFF$fitted.values)

ajustados = modelOFF$fitted.values
residuos = modelOFF$residuals

aj.residuos = data.frame(ajustados, residuos)

ggplot(aj.residuos, aes(y=residuos)) +
  geom_boxplot()

ggplot(aj.residuos, aes(x = residuos)) +
  geom_histogram()

ggplot(aj.residuos, aes(sample=residuos)) +
  stat_qq() +
  stat_qq_line()

ggplot(aj.residuos, aes(x = ajustados, y = residuos)) +
  geom_point(size=2)

# the residues are somewhat well distributed, but they are not normal

# saving the transformed data

write.csv(datart3, "~/Documents/Thesis Dissertation/Data Analysis/AJT (BrPL1) Response Time/datart3.csv")

# further exploration

# summary
summary(datart)
summary(datart3)
# sets of data excluded per condition
# E+S = 17
# E+P = 19
# U+P = 17
# U+S = 15