# ANALYSIS OF TWO-ALTERNATIVE FORCED-CHOICE TASK DATA
# PEER CHRISTENSEN
# STELLENBOSCH, MAY 2018

####### LOAD PACKAGES ###################################################

library(viridis)
library(tidyverse)
library(lme4)
library(lmerTest)
library(janitor)
library(sjPlot)
library(effects)
library(jtools)

###### LOAD AND CLEAN DATA ###############################################
files = list.files("Choice_logs_stellenbosch",pattern="csv",full.names = T)

df = files %>%
  map(read_csv2) %>%
  reduce(rbind)

df$participant <- factor(df$participant)
df$language    <- factor(df$language)
df$gender      <- factor(df$gender)
df$voice       <- factor(df$voice)
df$left        <- factor(df$left)
df$right       <- factor(df$right)
df$choice      <- factor(df$choice)
df$choiceName  <- factor(df$choiceName)
df$RT          <- as.numeric(df$RT)

###### CREATE VARIABLES ###################################################
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

df$condition = "mixed"
df$condition[(df$left=="high" | df$left=="low") & (df$right=="high" | df$right=="low")] = "height"
df$condition[(df$left=="small" | df$left=="big") & (df$right=="small" | df$right=="big")] = "size"
df$condition=factor(df$condition)

###### RT ####################################################################
#distribution of RT - set upper RT limit to 3 secs?
df %>% ggplot(aes(x=RT)) + 
  geom_density()

#reaction time - choosing between SIZE and HEIGHT (when "congruent")
df %>%
  filter(targetBetween==T) %>% 
  group_by(voice) %>%
  dplyr::summarise(n=n(),mean = mean(RT), sd = sd(RT))

df %>%
  filter(targetBetween==T) %>%
  ggplot(aes(x=voice,y=RT,fill=voice)) +
  geom_jitter(alpha=.5) +
  geom_boxplot(alpha=.8,colour="snow") +
  scale_fill_viridis(discrete = T) +
  theme_apa()

 #reaction time - choosing between spatial opposites
df %>%
  filter(targetWithin==T) %>% 
  group_by(voice,condition) %>%
  dplyr::summarise(n=n(),mean = mean(RT), sd = sd(RT))

fit = lmer(RT~condition+voice+(1|participant),df)
summary(fit)

###### DIMENSION PREFERENCES ###################################################
df %>% 
  filter(targetBetween==T) %>%
  group_by(voice,choiceName) %>%
  dplyr::summarise(n=n())

dfBetween = df %>% filter(targetBetween==T)
fit1 = glmer(choiceDimension ~ voice + (1|participant),family="binomial",data=dfBetween)
summary(fit1) #significant preference for size going from high to low sound

ef <- effect("voice",fit1)
x <- as.data.frame(ef)
ggplot(x, aes(voice, fit)) + 
  geom_bar(stat="identity", position="dodge")+ 
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.4)

plot(allEffects(fit1)) #probability of choosing "size" vs. "height"

# are random effects normally distributed?
plot_model(fit1, type = "diag")

###### POLARITY PREFERENCES ###################################################
#height
df %>% 
  filter(targetWithin==T & condition=="height") %>%
  tabyl(voice,choiceName) %>% adorn_percentages()

###polarity preferences - size
df %>% 
  filter(targetWithin==T & condition=="size") %>%
  tabyl(voice,choiceName) %>% adorn_percentages()














#### WITHIN-DIMENSIONS ASSOCIATIONS WITH PITCH ###

df2 = files %>%
  map(read_csv2) %>%
  reduce(rbind)

df2 = df2 %>%
  mutate(RT = as.numeric(RT),
         target = ifelse((voice =="low" & (left =="low" | left=="high")  & (right =="low" | right=="high")) |
                           (voice =="low" & (left =="big" | left=="small")  & (right =="big" | right=="small")) |
                           (voice =="high" & (left =="high" | left=="low") & (right =="high" | right=="low")) |
                           (voice =="high" & (left =="small" | left=="big") & (right =="small" | right=="big")),T,F)) %>%
  mutate(choiceCongruent = ifelse(((choiceName =="high" | choiceName =="small")  & voice=="high") |
                                    ((choiceName=="low" | choiceName =="big") & voice=="low"),T,F)) %>%
  mutate(choiceDimension = ifelse(choiceName =="high" | choiceName =="low","height","size")) %>%
  filter(RT<6)

#reaction time - congruency
dfTarget = df %>% filter(target==T)
rt = df %>% 
  group_by(choiceCongruent,choiceName) %>%
  summarise(n=n(),mean = mean(RT), sd = sd(RT))
rt

#rt - pitch
rt = dfTarget %>% 
  group_by(voice,choiceDimension) %>%
  summarise(n=n(),mean = mean(RT), sd = sd(RT))
rt

#dfTarget %>% group_by(voice,choiceName) %>% summarise(n=n())



#for (file in files){
#  p = read_csv2(file)
#  df = rbind(df,p)
#}
#mutate(congruent = ifelse((voice =="low" & (left =="low" | left=="big")  & (right =="low" | right=="big")) |
#                            (voice =="high" & (left =="high" | left=="small") & (right =="high" | right=="small")),T,F)) %>%
#  mutate(choiceCongruent = ifelse(((choiceName =="high" | choiceName =="small")  & voice=="high") |
#                                    ((choiceName=="low" | choiceName =="big") & voice=="low"),T,F)) %>%
#  mutate(choiceDimension = ifelse(choiceName =="high" | choiceName =="low","height","size")) %>%
  
  
