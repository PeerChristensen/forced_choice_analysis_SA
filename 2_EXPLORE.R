# DUAL FORCED-CHOICE TASK: EXPLORE
# PEER CHRISTENSEN
# DATA FROM STELLENBOSCH, MAY 2018
# OCTOBER 2018

# MIXED CONDITION
# HEIGHT CONDITION
# SIZE CONDITION
# RT ANALYSES

# -------------------------------------------
####### LOAD PACKAGES AND DATA ##############

library(tidyverse)
library(magrittr)
library(lme4)
library(stellenbosch)
library(gridExtra)
library(lmerTest)
library(broom)
library(sjPlot)

df <- read.csv("task_3_datafile.csv") %>% 
  as_tibble()

theme_set(theme_bw())

cols  <- unname(stellenbosch_colours[c("green","shiraz","khaki")])
dodge <- position_dodge(width=0.1)

# -------------------------------------------
####### MIXED CONDITION #####################

df_m <- df %>% 
  filter(condition=="mixed" & congruent ==TRUE)

mixed <- df_m %>%
  group_by(language,participant,voice,choiceDimension) %>%
  summarise(n = n()) %>%
  complete(choiceDimension, 
           nesting(participant), 
           fill = list(n = 0)) %>%
  mutate(freq = n / sum(n), 
         wt=sum(n)) %>%
  group_by(language,voice,choiceDimension) %>%
  summarise(Freq = weighted.mean(freq,wt),
            se   = sqrt(Hmisc::wtd.var(freq,wt))/sqrt(length(unique(participant)))) %>%
  ungroup()

# lines
mixed %>% 
  ggplot(aes(x=choiceDimension,y=Freq,colour=language)) +
  geom_line(aes(group = language),
            size      = 1,
            position  = dodge) +
  geom_point(position = dodge) +
  geom_errorbar(aes(ymin = Freq-se,ymax = Freq+se),
                width    = .3,
                position = dodge) +
  scale_colour_manual(values = c(cols[1],cols[2])) +
  facet_wrap(~voice)

# bars
mixed %>% 
  ggplot(aes(x=choiceDimension,y=Freq,fill=language)) +
  geom_bar(stat     = "identity",
           position = position_dodge()) +
  geom_errorbar(aes(ymin = Freq-se,ymax = Freq+se),
                width    = .3,
                position = position_dodge(.9)) +
  scale_fill_manual(values = c(cols[1],cols[2])) +
  facet_wrap(~voice)

fit1 = glmer(choiceDimension ~ voice * language + (1+voice|participant),family="binomial",data=df_m)
summary(fit1)

# -------------------------------------------
####### HEIGHT CONDITION #####################

df_h <- df %>% 
  filter(condition=="height")

height <- df_h %>%
  group_by(language,participant,voice,choiceName) %>%
  summarise(n = n()) %>%
  complete(choiceName, 
           nesting(participant), 
           fill = list(n = 0)) %>%
  mutate(freq = n / sum(n), 
         wt=sum(n)) %>%
  group_by(language,voice,choiceName)      %>%
  summarise(Freq = weighted.mean(freq,wt),
            se = sqrt(Hmisc::wtd.var(freq,wt))/sqrt(length(unique(participant)))) %>%
  ungroup() %>%
  filter(Freq>0)
  
# lines
height %>% 
  ggplot(aes(x=choiceName,y=Freq,colour=language)) +
  geom_line(aes(group = language),
            size = 1,
            position = dodge) +
  geom_point(position = dodge) +
  geom_errorbar(aes(ymin = Freq-se,ymax = Freq+se),
                width    =.3,
                position = dodge) +
  scale_colour_manual(values=c(cols[1],cols[2])) +
  facet_wrap(~voice)

fit2 = glmer(choiceName ~ voice * language + (1+voice|participant),family="binomial",data=df_h)
summary(fit2)

# -------------------------------------------
####### SIZE CONDITION ######################

df_s <- df %>% 
  filter(condition=="size")

size <- df_s %>%
  group_by(language,participant,voice,choiceName) %>%
  summarise(n = n()) %>%
  complete(choiceName, 
           nesting(participant), 
           fill = list(n = 0)) %>%
  mutate(freq = n / sum(n), 
         wt=sum(n)) %>%
  group_by(language,voice,choiceName) %>%
  summarise(Freq = weighted.mean(freq,wt),
            se = sqrt(Hmisc::wtd.var(freq,wt))/sqrt(length(unique(participant)))) %>%
  ungroup() %>%
  filter(Freq>0)

# lines
size %>% 
  ggplot(aes(x=choiceName,y=Freq,colour=language)) +
  geom_line(aes(group = language),
            size = 1,
            position = dodge) +
  geom_point(position = dodge) +
  geom_errorbar(aes(ymin = Freq-se,ymax = Freq+se),
                width    =.3,
                position = dodge) +
  scale_colour_manual(values=c(cols[1],cols[2])) +
  facet_wrap(~voice)

fit3 = glmer(choiceName ~ voice * language + (1+voice|participant),family="binomial",data=df_s)
summary(fit3)

# -------------------------------------------
####### RT ANALYSES #########################

df %<>% filter(RT < 60)

### INSPECT QQPLOTS

RT_cor    = qqnorm(df$RT,plot=F)
RTinv_cor = qqnorm(1/df$RT,plot=F)
RTlog_cor = qqnorm(log(df$RT),plot=F)

RT_plot <- df %>% 
  ggplot(aes(sample=RT))+
  stat_qq() +
  stat_qq_line(colour = stellenbosch_colours["shiraz"]) +
  ggtitle("RT") +
  annotate("text",
           x     = -3,
           y     = max(df$RT),
           size  = 8,
           label = paste("italic(r) ==",
                         round(cor(RT_cor$x,RT_cor$y),3)),parse=T)

RTinv_plot <- df  %>%
  ggplot(aes(sample=1/RT))+
  stat_qq() +
  stat_qq_line(colour = stellenbosch_colours["shiraz"]) +
  ggtitle("Inverse Gaussian transform") +
  annotate("text",
           x     = -3,
           y     = max(1/df$RT),
           size  = 8,
           label = paste("italic(r) ==",
                         round(cor(RTinv_cor$x,RTinv_cor$y),3)),parse=T)

RTlog_plot <- df %>% 
  ggplot(aes(sample=log(RT)))+
  stat_qq() +
  stat_qq_line(colour = stellenbosch_colours["shiraz"]) +
  ggtitle("Log-Normal transform") +
  annotate("text",
           x     = -3,
           y     = max(log(df$RT)),
           size  = 8,
           label = paste("italic(r) ==",
                         round(cor(RTlog_cor$x,RTlog_cor$y),3)),parse=T)

grid.arrange(RT_plot,RTinv_plot,RTlog_plot,ncol=3)

### REGRESSION MODEL: ALL CONDITIONS
contrasts(df$condition) <- contr.treatment(3, base = 2)

fitRT <- lmer(log(RT) ~ language*voice*condition + (1+condition+voice|participant), data =df)
summary(fitRT)

# MODEL CRITICISM

fitRT_au=augment(fitRT)

# R-squared
cor(fitRT_au$log.RT.,fitRT_au$.fitted) ^2

# Diagnostic plots
plot_model(fitRT,type="diag")

# Remove residuals > 2.5. sd
df <- df[abs(scale(fitRT_au$.resid)) < 2.5, ]

# refit model
fitRT2 <- lmer(log(RT) ~ language*voice*condition + (1+condition+voice|participant), data =df)
summary(fitRT2)

# Check new model
fitRT2_au <- augment(fitRT2)

# R-squared
cor(fitRT2_au$log.RT.,fitRT2_au$.fitted) ^2

plot_model(fitRT2,type="diag")[[1]]

### REGRESSION MODEL: MIXED CONDITION
df_m %<>% filter(RT < 60)

fitRTm <- lmer(log(RT) ~ language*voice + (1+voice|participant), data =df_m)
summary(fitRTm)

# MODEL CRITICISM

fitRTm_au=augment(fitRTm)

# R-squared
cor(fitRTm_au$log.RT.,fitRTm_au$.fitted)^2

# Diagnostic plots
plot_model(fitRTm,type="diag")

# Remove residuals > 2.5. sd
df_m <- df_m[abs(scale(fitRTm_au$.resid)) < 2.5, ]

# refit model
fitRTm2 <- lmer(log(RT) ~ language*voice + (1+voice|participant), data =df_m)
summary(fitRTm2)

# Check new model
fitRTm2_au <- augment(fitRTm2)

# R-squared
cor(fitRTm2_au$log.RT.,fitRTm2_au$.fitted) ^2

plot_model(fitRTm2,type="diag")[[1]]

### REGRESSION MODEL: HEIGHT CONDITION
df_h %<>% filter(RT < 60)

fitRTh <- lmer(log(RT) ~ language*voice + (1+voice|participant), data =df_h)
summary(fitRTh)

# MODEL CRITICISM

fitRTh_au=augment(fitRTh)

# R-squared
cor(fitRTh_au$log.RT.,fitRTh_au$.fitted)^2

# Diagnostic plots
plot_model(fitRTh,type="diag")

# Remove residuals > 2.5. sd
df_h <- df_h[abs(scale(fitRTh_au$.resid)) < 2.5, ]

# refit model
fitRTh2 <- lmer(log(RT) ~ language*voice + (1+voice|participant), data =df_h)
summary(fitRTh2)

# Check new model
fitRTh2_au <- augment(fitRTh2)

# R-squared
cor(fitRTh2_au$log.RT.,fitRTh2_au$.fitted) ^2

plot_model(fitRTh2,type="diag")[[1]]

### REGRESSION MODEL: MIXED CONDITION
df_s %<>% filter(RT < 60)

fitRTs <- lmer(log(RT) ~ language*voice + (1+voice|participant), data =df_s)
summary(fitRTs)

# MODEL CRITICISM

fitRTs_au=augment(fitRTs)

# R-squared
cor(fitRTs_au$log.RT.,fitRTs_au$.fitted)^2

# Diagnostic plots
plot_model(fitRTs,type="diag")

# Remove residuals > 2.5. sd
df_s <- df_s[abs(scale(fitRTs_au$.resid)) < 2.5, ]

# refit model
fitRTs2 <- lmer(log(RT) ~ language*voice + (1+voice|participant), data =df_s)
summary(fitRTs2)

# Check new model
fitRTs2_au <- augment(fitRTs2)

# R-squared
cor(fitRTs2_au$log.RT.,fitRTs2_au$.fitted) ^2

plot_model(fitRTs2,type="diag")[[1]]




