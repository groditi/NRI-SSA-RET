library(seasonal)
library(readr)
library(dplyr)
library(zoo)
library(lubridate)
library(ggplot2)
library(forecast)
library(tibble)

#new monthly benefit awards and average benefit by sex
new_benes <- read_csv(file.path("etc", "ssa.ret.bene.new.csv")) 
#all existing benefits and average benefit by sex
total_benes <- read_csv(file.path("etc", "ssa.ret.bene.total.csv"))

#calculate how many benefits were extinguished
#the assumption here is the diff between benefit delta and new awards is deaths
transitions <- total_benes %>% filter(date >= ymd('1974-12-01')) %>% 
  left_join(new_benes, by = 'date') %>%
  mutate(
    exits.ret.men = (lag(ret.men.count) - ret.men.count) + new.ret.men.count,
    exits.ret.women = (lag(ret.women.count) - ret.women.count) + new.ret.women.count,
    exits.spouse.men = (lag(spouse.men.count) - spouse.men.count) + new.spouse.men.count,
    exits.spouse.women = (lag(spouse.women.count) - spouse.women.count) + new.spouse.women.count
  )

#there is a definite seasonlity pattern to new awards so correct for that
#start in 2001 because there is something odd in may of 2000 data that would
#imply a lot of resusitations or maybe more likely than a massive wave of zombies
#signing up for benefits there was some sort of methodology adjustment. 
# Personally, I don't care but if you do you can call +1-410-965-0090 or email
# statistics@ssa.gov and get to the bottom of this zombie pensioner mystery.
model_new_men <- new_benes %>%  
  filter(date >= ymd("2001-01-01")) %>% 
  select(new.ret.men.count) %>%
  ts(frequency = 12, start = c(2001,1)) %>% seas()
model_new_women <- new_benes %>%   
  filter(date >= ymd("2001-01-01")) %>%
  select(new.ret.women.count) %>%
  ts(frequency = 12, start = c(2001,1)) %>% seas()

#So I expected a little seasonality from exits from, like, you know, #FluSeason
#but it turns out there is a shitton (actual statistical term) of seasonality
#in extinguishments. again.. idk why, but if you wanna know, you can call
model_exits_men <- transitions %>%  
  filter(date >= ymd("2001-01-01")) %>%
  select(exits.ret.men) %>%
  ts(frequency = 12, start = c(2001,1)) %>% seas()
model_exits_women <- transitions %>%  
  filter(date >= ymd("2001-01-01")) %>%
  select(exits.ret.women) %>%
  ts(frequency = 12, start = c(2001,1)) %>% seas()

#trend decompsition visuals
new_men_plot <- model_new_men %>% autoplot + 
  xlab("") + ylab("New SSA Retirement Beneficiaries: men") +
  scale_y_continuous(labels = scales::comma)
new_women_plot <- model_new_women %>% autoplot + 
  xlab("") + ylab("New SSA Retirement Beneficiaries: women") +
  scale_y_continuous(labels = scales::comma)

exits_men_plot <- model_exits_men %>% autoplot + xlab("") + 
  ylab("Extinguished SSA Retirement Beneficiaries: men") +
  scale_y_continuous(labels = scales::comma) 

exits_women_plot <- model_exits_women %>% autoplot + xlab("") + 
  ylab("Extinguished SSA Retirement Beneficiaries: women") +
  scale_y_continuous(labels = scales::comma)


ggsave(file.path("etc", "ret.new.men.png"), new_men_plot)
ggsave(file.path("etc", "ret.new.women.png"), new_women_plot)
ggsave(file.path("etc", "ret.exits.men.png"), exits_men_plot)
ggsave(file.path("etc", "ret.exits.women.png"), exits_women_plot)

#write out the merged file for the boners who dont know how to use R
write_csv(transitions, file.path("etc", "ssa.ret.bene.merged.csv"))
