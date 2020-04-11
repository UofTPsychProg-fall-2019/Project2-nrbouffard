# BOTB analysis April 10, 2020
# Author: Nichole Bouffard
# This script runs the anovas and t tests for single item recognition and associative memory. It also plots the results using ggplot

#--------------------------------------#

# Load in the three packages below. (Note, if you don't already have them downloaded you can uncomment the "install.package" lines and run those first, then load them using the library() line.)
    # tidyverse - useful datawrangling functions, piping, and ggplot 
    # afex - this is package I like to use to run my stats. It has a function "aov_ez" that I use instead of "aov". It takes a more intutive input compared the aov formulas 
    # emmeans - for running the pairwise comparisons (t tests) after the anova
# 
## libraries -----
# install.pacakges(tidyverse)
library(tidyverse)
# install.packages(afex)
library(afex)
#install.packages(emmeans)
library(emmeans)
#
# For raincloud plots:
#install.packages(cowplot)
library(cowplot)
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
#
# This is also something I like to run at the top of my scripts. It basically sets up your environment to do contrast coding so your contrasts sum to zero (the default in R is not to do this)
set_sum_contrasts()

#--------------------------------------#

### SINGLE ITEM RECOGNITION ###

## Read in data -----
sirdat <-read_csv('/Users/nicholebouffard/Documents/Github/Project2-nrbouffard/SngItmRec.csv') # I'm using read_csv here instead of read.csv. You must have the tidyverse loaded first before you can use read_csv, but it reads your data into a tibble way quicker and more efficient than reading your data into a data.frame using read.csv

# Wrangle the data into long format
sirdat <- sirdat %>% 
  gather("condition", "hit_minus_fa", 4:6)

# First compute means and sd  make sure it matches what was computed in excel
sirdat_summary<- sirdat %>% 
  group_by(condition, stimulus, group) %>% 
  summarise(mean = mean(hit_minus_fa), sd = sd(hit_minus_fa))
sirdat_summary

# check that the se matches
YA <- sirdat %>% 
  filter(group == 'YA') 
nYA <- length(unique(YA$subid)) #31

OA <- sirdat %>% 
  filter(group == 'OA') 
nOA <- length(unique(OA$subid)) #28

sirdat_summary <- sirdat_summary %>% 
  mutate(n = ifelse(group == 'YA', nYA, nOA)) %>% 
  mutate(se = sd/sqrt(n))
sirdat_summary

# Ok cool, so the sirdat_summary values seem to match the excel. Now I will plot to see if the graphs looks the same

# Barplot
sirPlot1<-sirdat %>% 
  ggplot(aes(x = condition, y = hit_minus_fa, fill = group)) +
  stat_summary(fun.y=mean,geom="bar", position=position_dodge(width=.9)) +
  stat_summary(fun.data=mean_se, geom="errorbar", position=position_dodge(width=.9), width=.3)+
  scale_fill_manual(values=c("#66CC99","#9999CC"))+
  facet_wrap(~stimulus)+
  theme_bw() +
  ggtitle('Single Item Recognition')

# View plot
sirPlot1

# Rain cloud plot
sirPlot2<-sirdat %>% 
  ggplot(aes(x = condition, y = hit_minus_fa, fill = group, color = group))+
    geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust =2, alpha = .3)+
    scale_fill_manual(values=c("#66CC99","#9999CC"))+
    geom_point(position = position_jitter(width = .15), size = .5)+
    stat_summary(fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange") +
    scale_color_manual(values=c("#66CC99","#9999CC"))+
    facet_wrap(~c(stimulus))+
    theme_set(theme_cowplot())+
    ggtitle('Single Item Recognition')

# View plot
sirPlot2

# Ok so these plots looks the same
# Save these out to send to Rena
#ggsave(plot = sirPlot2, '/Users/nicholebouffard/Documents/Github/Project2-nrbouffard/single_item_recog_raincloud.png')

## Now running anova and t tests
mdl1<-aov_ez(id = 'subid',
             dv = 'hit_minus_fa',
             data = sirdat,
             between = 'group',
             within = c('condition', 'stimulus'))

# print the output of the anova
mdl1
# you can also print it a nicer anova table
anova(mdl1)

# Run the post hoc t tests.
# You can play around with this and add different variables to contrast by. I included the full code, with all the contrasts, but the output is long so I figured you could play around with these as you need.
pairs(emmeans(mdl1, 'condition'))
#pairs(emmeans(mdl1, c('condition', 'stimulus', 'group')))


#--------------------------------------#

### ASSOCIATIVE MEMORY ###
assocdat<-read_csv('/Users/nicholebouffard/Documents/Github/Project2-nrbouffard/AssocMem.csv') 

# Wrangle the data into long format
assocdat <- assocdat %>% 
  gather("condition", "fa", 4:6)

# Compute means and sd  make sure it matches the excel
assocdat_summary<- assocdat %>% 
  group_by(condition, stimulus, group) %>% 
  summarise(mean = mean(fa), sd = sd(fa))
assocdat_summary

# check that the se matches
assocdat_summary <- assocdat_summary %>% 
  mutate(n = ifelse(group == 'YA', nYA, nOA)) %>% 
  mutate(se = sd/sqrt(n))
assocdat_summary
# I spot checked these and they match the excel

# BarPlot
assocPlot1 <- assocdat %>% 
  ggplot(aes(x = condition, y = fa, group = group, fill = group)) +
  stat_summary(fun.y=mean,  geom="bar", position=position_dodge(width=.9)) +
  stat_summary(fun.data=mean_se, geom="errorbar", position=position_dodge(width=.9), width=.3)+
  scale_fill_manual(values=c("#66CC99","#9999CC"))+
  # geom_jitter(alpha =.5)+
  facet_wrap(~stimulus)+
  #ylim(0,0.35)+
  theme_bw() +
  ylab('False Alarms')+
  ggtitle('Associative Memory')

#View plot
assocPlot1


# Point range Plot
assocPlot2 <- assocdat %>% 
  ggplot(aes(x = condition, y = fa, color = group, fill = group)) +
  geom_jitter(position=position_dodge(width=.9), size = .5)+
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange",position=position_dodge(width=.9)) +
  scale_color_manual(values=c("#66CC99","#9999CC"))+
  facet_wrap(~stimulus)+
  theme_bw() +
  ylab('False Alarms')+
  ggtitle('Associative Memory')

#View plot
assocPlot2


# These also look similar to the excel plots
#ggsave(plot = assocPlot1, '/Users/nicholebouffard/Documents/Github/Project2-nrbouffard/assoc_mem.png')


## Now running anova and t tests
mdl2 <- aov_ez(id = 'subid',
             dv = 'fa',
             data = assocdat,
             between = 'group',
             within = c('condition', 'stimulus'))

# print the output of the anova
mdl2
# you can also print it a nicer anova table
anova(mdl2)

# Run the post hoc t tests.
pairs(emmeans(mdl2, 'condition'))
#pairs(emmeans(mdl2, c('condition', 'stimulus', 'group')))



