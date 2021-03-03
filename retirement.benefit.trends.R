library(seasonal)
library(readr)
library(dplyr)
library(zoo)
library(lubridate)
library(ggplot2)
library(forecast)
library(tibble)
library(seasonal)

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
    exits.ret.total = (lag(ret.total.count) - ret.total.count) + new.ret.total.count,
    exits.spouse.men = (lag(spouse.men.count) - spouse.men.count) + new.spouse.men.count,
    exits.spouse.women = (lag(spouse.women.count) - spouse.women.count) + new.spouse.women.count,
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

zombie_plot <- transitions %>% 
  filter(date >= ymd('1999-01-01') & date < ymd('2002-01-01')) %>% 
  select(exits.ret.men, exits.ret.women, exits.spouse.men, exits.spouse.women) %>%
  ts(frequency = 12, start = c(1999,1)) %>% autoplot + xlab("") + 
  ylab("Extinguished SSA Retirement Beneficiaries") +
  scale_y_continuous(labels = scales::comma) +
    theme( legend.position="bottom" )

ggsave(file.path("etc", "ret.resusitations.png"), zombie_plot)

#############################################################
#the charts from the medium post
transitions %>% 
  filter(date >= ymd('1980-01-01') & date < ymd('2000-01-01')) %>% 
  select(exits.ret.men, exits.ret.women, exits.spouse.men, exits.spouse.women) %>%
  ts(frequency = 12, start = c(1980,1)) %>% autoplot + xlab("") + 
  ylab("Extinguished SSA Retirement Beneficiaries") +
  scale_y_continuous(labels = scales::comma) +
  theme( legend.position="bottom" )

transitions %>% 
  filter(date >= ymd('1980-01-01') & date < ymd('2000-01-01')) %>% 
  select(exits.ret.total) %>%
  ts(frequency = 12, start = c(1980,1)) %>%
  seas() %>% autoplot + xlab("") + 
  ylab("Extinguished SSA Retirement Beneficiaries") +
  scale_y_continuous(labels = scales::comma) 

transitions %>% 
  filter(date >= ymd('2001-01-01') & date < ymd('2020-01-01')) %>% 
  select(exits.ret.total) %>%
  ts(frequency = 12, start = c(2001,1)) %>%
  seas() %>% autoplot + xlab("") + 
  ylab("Extinguished SSA Retirement Beneficiaries") +
  scale_y_continuous(labels = scales::comma) 

predictor_model <- transitions %>% 
  filter(date >= ymd('2001-01-01') & date < ymd('2020-01-01')) %>% 
  select(exits.ret.total) %>%
  ts(frequency = 12, start = c(2001,1)) %>%
  seas( forecast.save = "fct" ) 

forecasts_ts <- predictor_model %>% series('fct')
exits_forecasts <- tibble(
  date = as.Date(as.yearmon(index(forecasts_ts))),
  forecast = forecasts_ts[,1],
  lower.ci = forecasts_ts[,2],
  upper.ci = forecasts_ts[,3]
) 

thisyearsucks <- transitions %>% 
  select(date, exits.ret.total) %>%
  filter(date >= ymd('2020-01-01')) %>% 
  inner_join(exits_forecasts, by = 'date') %>%
  mutate(
    forecast_error = exits.ret.total-forecast,
    cumulative_error = cumsum(forecast_error)
  )
   
 
ci_plot <- thisyearsucks %>% 
  ggplot(aes(x = date)) +
  geom_ribbon(
    aes(ymin = lower.ci, ymax = upper.ci),
     alpha = 0.15, fill = '#0072B2'
  ) +
  geom_line(aes(y = forecast), color = "#666666", alpha = 0.5) +
  geom_line(aes(y = exits.ret.total), color ="#D55E00", alpha = 0.7) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  
err_plot <- thisyearsucks %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = forecast_error), alpha = 0.5, color ="#D55E00") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

cum_err_plot <- thisyearsucks %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = cumulative_error), color ="#D55E00") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.title.x = element_blank())

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(ci_plot), ggplotGrob(err_plot), ggplotGrob(cum_err_plot), size = "last"))

