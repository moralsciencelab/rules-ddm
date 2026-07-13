library(tidyverse)
library(lmerTest)
library(emmeans)
library(posterior)
library(dplyr)

dt = read.csv('data/rules_allstudies.csv')
glimpse(dt)

## speed- accuracy
dt$spd_acc_mode = relevel(factor(dt$spd_acc_mode), ref = 'spd')

resp_sa = glmer(response ~ spd_acc_mode * (text + purpose) + (1 | rule) + (1 | subj_code), 
                subset(dt, study == 'speed_accuracy'), family = 'binomial')

car::Anova(resp_sa)

jtools::summ(resp_sa, digits = 3, exp = TRUE, confint = TRUE)

emmeans(resp_sa, pairwise ~ spd_acc_mode | text * purpose)

rt_sa = lmer(log(rt) ~ spd_acc_mode * (text * purpose) + (1 | rule) + (1 | subj_code), 
             subset(dt, study == 'speed_accuracy'))

car::Anova(rt_sa)

jtools::summ(rt_sa, digits = 3, confint = TRUE)

emmeans(resp_sa, pairwise ~ spd_acc_mode | text * purpose)

## proportion congruency

dt = dt %>% 
  mutate(congprop2 = if_else(congprop == 'low', 'high', 
                             if_else(congprop == "high", "low", NA)))

dt$congprop = relevel(factor(dt$congprop2), ref = 'low')
resp_cp = glmer(response ~ congprop * (text + purpose) + (1 | rule) + (1 | subj_code), 
                subset(dt, study == 'congruency_proportions'), family = 'binomial')

car::Anova(resp_cp)

jtools::summ(resp_cp, digits = 3, exp = TRUE, confint = TRUE)

emmeans(resp_cp, pairwise ~ congprop | text * purpose, type = 'response')

rt_cp = lmer(log(rt) ~ congprop * (text * purpose) + (1 | rule) + (1 | subj_code), 
             subset(dt, study == 'congruency_proportions'))

jtools::summ(rt_cp, digits = 3, confint = TRUE)


sa_ddm <- read_csv("results/ddm/model_speed_accuracy/model_speed_accuracy_output.csv") %>%
  rename(term = `...1`) %>%
  filter(!str_detect(term, "_subj."), !str_detect(term, "std")) %>%
  select(term, mean, `2.5q`, `97.5q`)

print(sa_ddm)

cp_ddm <- read_csv("results/ddm/model_congruency_proportions/model_congruency_proportions_output.csv") %>%
  rename(term = `...1`) %>%
  filter(!str_detect(term, "_subj."), !str_detect(term, "std")) %>%
  select(term, mean, `2.5q`, `97.5q`)

print(cp_ddm)