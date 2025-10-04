library(tidyverse)
library(emmeans)
library(lmerTest)
library(logistf)

learn1 = read.csv("learning/data/learn1_raw.csv")
cases = read.csv("learning/pilot/unique_cases.csv")
View(learn1)
View(cases)

learn1_trials = learn1 %>%
  filter(task == 'response') %>%
  mutate(response_num = case_when(
    str_detect(key_assignment, "Press e for no") & response == "e" ~ 0,
    str_detect(key_assignment, "Press e for no") & response == "i" ~ 1,
    str_detect(key_assignment, "Press i for no") & response == "i" ~ 0,
    str_detect(key_assignment, "Press i for no") & response == "e" ~ 1
  )
  ) %>%
  left_join(., cases, by = "stimulus") %>%
  mutate(text = case_when(type %in% c('c', 'o') ~ 1,
                          type %in% c('u', 'n') ~ 0),
         purpose = case_when(type %in% c('c', 'u') ~ 1,
                          type %in% c('o', 'n') ~ 0)) %>%
  group_by(subj_code) %>%
  mutate(trial = dense_rank(trial_index)/96)
  
mod1 = glmer(response_num ~ trial * (text + purpose) + 
               (1 | subj_code) + (1 | rule), family = 'binomial',
             data = learn1_trials)
car::Anova(mod1)

xtabs(~ trial + condition_assignment, learn1_trials)
View(learn1_trials)

learn1_trials = learn1_trials %>%
  mutate(block = case_when(
    condition_assignment == 'Experimental' ~ 'Block3X',
    condition_assignment == 'Control' & trial <= 0.5 ~ 'Block1C',
    condition_assignment == 'Control'& trial > 0.5  ~ 'Block2C',
  ), 
  tXp = text * purpose)

speeders = learn1_trials %>%
  group_by(subj_code) %>% 
  summarise(speed = sum(as.numeric(rt) < 1000, na.rm = TRUE)) %>%
  arrange(-speed) %>% filter(speed > 24)

mod1 = glmer(response_num ~ trial * (text + purpose) + 
               (1 | subj_code) + (1 | rule), family = 'binomial', 
             subset(learn1_trials,  
                    condition_assignment == 'Control' & as.numeric(rt) > 500))
jtools::summ(mod1, digits = 3, confint = TRUE, exp = TRUE)

interactions::interact_plot(mod1, pred = 'trial', modx = 'purpose',
                            mod2 = 'text', interval = TRUE)


mean_resps = learn1_trials %>%
  ungroup() %>%
  filter(as.numeric(rt) > 500) %>%
  group_by(trial, condition_assignment, text, purpose, type) %>%
  summarise(response_num = mean(response_num, na.rm = TRUE))

ggplot(data = mean_resps, aes(x = trial, y = response_num)) + 
  geom_vline(xintercept = 0.5, linetype = 3) +
  geom_line(aes(group = type, color = as.factor(text), 
                linetype = text != purpose), alpha = .4) +
  geom_smooth(data = subset(learn1_trials, as.numeric(rt) > 500), 
                            aes(group = type, fill = as.factor(text), 
                                color = as.factor(text), 
                  linetype = text != purpose), linewidth = .5,
              method = "glm", 
              method.args = list(family = "binomial")) + 
  facet_grid(. ~ condition_assignment, scales = 'free_x', space = 'free_x') +
  theme_classic() + 
  theme(legend.position = 'none') + 
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 1, .25), 
                     labels = c('0', 'Block 1', '', 'Block 2', '1'))

mod2 = glmer(response_num ~ block * (text + purpose) + 
               (1 | subj_code) + (1 | rule), family = 'binomial', 
             subset(learn1_trials, as.numeric(rt) > 500))
jtools::summ(mod2, digits = 3, confint = TRUE, exp = TRUE)

temp1 = interactions::sim_slopes(mod2, pred = 'text', modx = 'block', 
                         confint = TRUE, digits = 3)
temp2 = interactions::sim_slopes(mod2, pred = 'purpose', modx = 'block',
                         confint = TRUE, digits = 3)

coefs5c = bind_rows(
  data.frame(temp1$slopes) %>% mutate(predictor = 'text'),
  data.frame(temp2$slopes) %>% mutate(predictor = 'purpose')
)  %>%
  mutate(Est. = exp(Est.), 
         X2.5. = exp(X2.5.), 
         X97.5. = exp(X97.5.))


ggplot(coefs5c, aes(x = Value.of.block, y = `Est.`, color = predictor)) +
  geom_hline(data = subset(coefs5c, Value.of.block == 'Block1C'),
             aes(yintercept = `Est.`, color = predictor), linetype = 3) +
  geom_rect(data = subset(coefs5c, Value.of.block == 'Block1C'),
              aes(ymin = `X2.5.`, ymax = `X97.5.`, fill = predictor), 
            xmin = -Inf, xmax = Inf, alpha = .1, color = NA) +
  geom_linerange(aes(ymin = `X2.5.`, ymax = `X97.5.`)) +
         geom_point(shape = 21, fill = 'white', size = 2, stroke = 1.5) + 
  coord_flip() + 
  theme_classic() + 
  scale_x_discrete(labels = c('Rules0', 'RulesL', 'RulesC'), 
                   name = NULL) + 
  scale_y_continuous(name = 'Odds ratio (OR)', limits = c(2.9, 13.2),
                     trans = 'log', breaks = seq(3, 13), expand = c(0, 0)) + 
  theme(axis.ticks = element_blank(),
        legend.position = 'none')

mod1 = glmer(response_num ~ group * (text + purpose) + 
               (1 | subj_code), family = 'binomial',
             subset(learn1, group != "Control Block 2"))
car::Anova(mod1)
emtrends(mod1, pairwise ~ group, var = 'text')
emtrends(mod1, pairwise ~ group, var = 'purpose')
