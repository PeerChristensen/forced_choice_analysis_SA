# DUAL FORCED-CHOICE TASK: PREPARE DATA
# PEER CHRISTENSEN
# DATA FROM STELLENBOSCH, MAY 2018
# OCTOBER 2018

# 1 Clean and add variables
# 2 write CSV

# -------------------------------------------
####### LOAD PACKAGES AND DATA ##############

library(tidyverse)
library(magrittr)

files <- list.files("choice_logs_stellenbosch",pattern="*.csv",full.names = T)

df    <- files   %>%
  map(read_csv2) %>%
  reduce(rbind)

theme_set(theme_bw())

# -------------------------------------------
####### CLEAN VARIABLES #####################

files = list.files("Choice_logs_stellenbosch",pattern="csv",full.names = T)

df = files %>%
  map(read_csv2) %>%
  reduce(rbind)

df$language[df$language=="Xh"] = "Xhosa"
df$participant <- factor(df$participant)
df$language    <- factor(df$language)
df$gender      <- factor(df$gender)
df$voice       <- factor(df$voice)
df$left        <- factor(df$left)
df$right       <- factor(df$right)
df$choice      <- factor(df$choice)
df$choiceName  <- factor(df$choiceName)
df$RT          <- as.numeric(df$RT)

# -------------------------------------------
####### ADD VARIABLES #######################

df %<>%
  mutate(
    targetBetween = ifelse(
      (voice =="low" & (left =="low" | left=="big")  & (right =="low" | right=="big")) |
        (voice =="high" & (left =="high" | left=="small") & (right =="high" | right=="small")),T,F),
    
    targetWithin = ifelse(
      (voice =="low" & (left =="low" | left=="high")  & (right =="low" | right=="high")) |
        (voice =="low" & (left =="big" | left=="small")  & (right =="big" | right=="small")) |
        (voice =="high" & (left =="high" | left=="low") & (right =="high" | right=="low")) | 
        (voice =="high" & (left =="small" | left=="big") & (right =="small" | right=="big")),T,F),
    
    choiceDimension = factor(ifelse(choiceName =="high" | choiceName =="low","height","size")))

df$condition <- "mixed"
df$condition[(df$left=="high" | df$left=="low") & (df$right=="high" | df$right=="low")]   <- "height"
df$condition[(df$left=="small" | df$left=="big") & (df$right=="small" | df$right=="big")] <- "size"

df$condition <- factor(df$condition)

df$congruent <- NA
df$congruent= ifelse(df$condition=="mixed" & df$voice=="high" &
                      (df$voice=="high" & df$left=="high" & df$right=="small") |
                      (df$voice=="high" & df$left=="small" & df$right=="high") |
                      (df$voice=="low" & df$left=="low" & df$right=="big") |
                      (df$voice=="low" & df$left=="nig" & df$right=="low"),TRUE,FALSE)

# -------------------------------------------
####### REMOVE FAST RTs #####################

df <- df %>% filter(RT > 0.2)

# -------------------------------------------
####### WRITE CSV ###########################

write_csv(df, "task_3_datafile.csv")
