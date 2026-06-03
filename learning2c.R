library(tidyverse)
library(emmeans)
library(lmerTest)
library(logistf)

learn3 =  bind_rows(read.csv("study4c/data/rules-colors-arrows_batch1.csv"),
                    read.csv("study4c/data/rules-colors-arrows_batch2c.csv"),
                    read.csv("study4c/data/rules-colors-arrows_batch3.csv"),
                    read.csv("study4c/data/rules-colors-arrows_batch4b.csv"))

#learn3 = read.csv("study4c/data/rules-colors-arrows_batch3.csv")
glimpse(learn3)

learn3 %>%
  filter(task == "response") %>%
  distinct(stimulus) %>%
  write.csv("stim2learn2c.csv", row.names = FALSE)

learn3 %>%
  filter(task == 'fixation') %>%
  group_by(run_id, PROLIFIC_PID, response) %>%
  tally() 

lf3 = learn3 %>%
  filter(task == 'flanker_response') %>%
  mutate(response = as.numeric(correct == "true")) 

lf3 %>%
  group_by(task, condition_assignment, flanker_stim_type) %>%
  summarise(n(), mean(response, na.rm = TRUE))

ls3 = learn3 %>%
  filter(task == 'stroop_response') %>%
  mutate(response = as.numeric(correct == "true"), 
         congruent_stim = color == text) 

ls3 %>%
  group_by(task, condition_assignment, congruent_stim) %>%
  summarise(n(), mean(response, na.rm = TRUE))

lf3 %>%
  group_by(task, condition_assignment) %>%
  summarise(n(), mean(as.numeric(rt), na.rm = TRUE))

ls3 %>%
  group_by(task, condition_assignment) %>%
  summarise(n(), mean(as.numeric(rt), na.rm = TRUE))


learn3 %>%
  group_by(run_id, PROLIFIC_PID, condition_assignment) %>%
  tally() %>%
  mutate(complete = n > 330) %>%
  group_by(condition_assignment, complete) %>%
  summarise(n())

completes = learn3 %>%
  filter(!is.na(PROLIFIC_PID)) %>%
  group_by(run_id, PROLIFIC_PID) %>%
  mutate(max_trial = max(trial_index)) %>%
  filter(max_trial == trial_index) %>% 
  filter(str_detect(stimulus, "Thank you for taking part in this experiment!")) %>%
  mutate(complete_time_elapsed = time_elapsed/1000/60) %>%
  select(run_id, PROLIFIC_PID, complete_time_elapsed)

failed_check = learn3 %>%
  filter(str_detect(stimulus, 'too many questions incorrectly')) %>%
  mutate(screen_out_time_elapsed = time_elapsed/1000/60) %>%
  select(run_id, PROLIFIC_PID, screen_out_time_elapsed)

id_list = full_join(completes, failed_check, by = c('run_id', 'PROLIFIC_PID')) 
#View(id_list)

learn3_clean = full_join(learn3_clean, id_list)


learn3_clean %>%
  group_by(rule, text, purpose) %>%
  summarise(mean(response_num, na.rm = TRUE), n()) 

id_list %>%
  filter(!is.na(complete_time_elapsed), is.na(screen_out_time_elapsed)) %>%
  write.csv('completes_learn3_batch1.csv')

learn3_clean %>%
  group_by(PROLIFIC_PID, text, purpose) %>%
  tally() 

learn3_clean = learn3 %>%
  filter(task == 'response') %>%
  mutate(condition = case_when(
    str_detect(condition_assignment, "Stroop") ~ 'Stroop'),
    congruency = case_when(
      str_detect(condition_assignment, "\"Congruent") & condition != "Control" ~ 'Congruent',
      str_detect(condition_assignment, "Incongruent") & condition != "Control" ~ 'Incongruent',
      condition == "Control" ~ 'Control'), 
    yes_side = case_when(
      str_detect(key_assignment, 'e for yes') ~ 'e',
      str_detect(key_assignment, 'i for yes') ~ 'i',
    ), 
    no_side = case_when(
      str_detect(key_assignment, 'e for no') ~ 'e',
      str_detect(key_assignment, 'i for no') ~ 'i',
    ), 
    response_num = if_else(response == yes_side, 1, 
                           if_else(response == no_side, 0, NA))
  ) %>%
  group_by(run_id, PROLIFIC_PID) %>%
  mutate(trial = dense_rank(trial_index)) %>%
  select(run_id, PROLIFIC_PID, congruency, condition, trial, stimulus, response_num, rt) %>%
  full_join(., read.csv('study4c/data/stimulus_coding.csv'), by = 'stimulus') %>%
  mutate(text = if_else(case %in% c('v', 'o'), 1, 0),
         purpose = if_else(case %in% c('v', 'u'), 1, 0),
         block = 
           case_when(trial <= 24 ~ 'B1',
                     condition == 'Control' & trial > 24 ~ 'B2',
                     condition == 'Rules' & trial > 72 ~ 'B2',
                     condition == 'Stroop' & trial > 24 ~ 'B2'),
         z_trial = case_when(
           condition == 'Control' ~ trial/48,
           condition == 'Rules'  ~ trial/96,
           condition == 'Stroop'  ~ trial/48
         ))

# Analysis ----

pass_list = id_list %>%
  filter(!is.na(complete_time_elapsed), is.na(screen_out_time_elapsed)) %>%
  pull(PROLIFIC_PID)

learn3_clean  %>%
  filter(PROLIFIC_PID %in% pass_list)

## T and P effects ----
model0 = glmer(response_num ~ (text * purpose) + (1 | PROLIFIC_PID) + (1 | rule), 
               subset(learn3_clean, !is.na(block) & PROLIFIC_PID %in% pass_list), 
               family = 'binomial')
car::Anova(model0)
jtools::summ(model0)
emmeans(model0, ~ text * purpose, type = 'response')

learn3_clean %>% 
  group_by(PROLIFIC_PID %in% pass_list, text, purpose) %>%
  summarise(n(),
    median(as.numeric(rt), na.rm = TRUE))
  
## Baseline effect (block) ----
model0_base = glmer(response_num ~ block * (text + purpose) + 
                      (1 | PROLIFIC_PID) + (1 | rule), 
                    subset(learn3_clean, !is.na(block) &
                             PROLIFIC_PID %in% pass_list), 
                    family = 'binomial',
                    control = glmerControl(optimizer= "optimx",
                                           optCtrl  = list(method="nlminb")))
car::Anova(model0_base)
emtrends(model0_base, pairwise ~ block , var = 'text')
emtrends(model0_base, pairwise ~ block , var = 'purpose')

anova(model0_base, model0)

emmeans(model0_base, pairwise ~ block | text * purpose, type = 'response')

model0_base2 = glmer(response_num ~ congruency * block * (text + purpose) + 
                       (1 | PROLIFIC_PID) + (1 | rule), 
                     subset(learn3_clean, !is.na(block) & 
                              PROLIFIC_PID %in% pass_list), 
                     family = 'binomial',
                     control = glmerControl(optimizer= "optimx",
                                            optCtrl  = list(method="nlminb")))
anova(model0_base2, model0_base)

car::Anova(model0_base2)
emtrends(model0_base2, pairwise ~ block | congruency, var = 'purpose')
emtrends(model0_base2, pairwise ~ block | congruency, var = 'text')

emtrends(model0_base2, pairwise ~ congruency | block, var = 'purpose')
emtrends(model0_base2, pairwise ~ congruency | block, var = 'text')

emtrends(model0_base2, pairwise ~ congruency, var = 'purpose')
emtrends(model0_base2, pairwise ~ congruency, var = 'text')

learn3_clean = learn3_clean %>%
  arrange(run_id, PROLIFIC_PID, block, rule, trial) %>%
  group_by(run_id, PROLIFIC_PID, block, rule) %>%
  mutate(w_trial = (dense_rank(trial)-1)/11,
         block_n = as.factor(ceiling(trial/12)))

learn3_clean$post_blocks = as.numeric(learn3_clean$block == "B2")

model0_base3 = glmer(response_num ~ (w_trial + block_n) * (text + purpose) + 
                       (1 | PROLIFIC_PID) + (1 | rule), 
                     subset(learn3_clean, !is.na(block) & 
                              PROLIFIC_PID %in% pass_list), 
                     family = 'binomial',
                     control = glmerControl(optimizer= "optimx",
                                            optCtrl  = list(method="nlminb")))

car::Anova(model0_base3)

# more text from block to block, resets during break
emtrends(model0_base3, pairwise ~ block_n, var = 'text')
# more text over trials in block
emtrends(model0_base3, pairwise ~ text , var = 'w_trial')

# less purpose in post blocks
emtrends(model0_base3, pairwise ~ block_n, var = 'purpose')
# slightly less purpose over trials in block
emtrends(model0_base3, pairwise ~ purpose , var = 'w_trial')


learn3_clean = learn3_clean %>%
  mutate(carry_over = case_when(
    block_n %in% c("1", "3") ~ 0,
    block_n %in% c("2", "4") ~ 1
  ))

model0_base4 = glmer(response_num ~ (w_trial + carry_over + post_blocks) * (text + purpose) + 
                       (1 | PROLIFIC_PID) + (1 | rule), 
                     subset(learn3_clean, !is.na(block) & 
                              PROLIFIC_PID %in% pass_list), 
                     family = 'binomial',
                     control = glmerControl(optimizer= "optimx",
                                            optCtrl  = list(method="nlminb")))

anova(model0_base4, model0_base3)
car::Anova(model0_base4)

# more text over trials in block
emtrends(model0_base4, pairwise ~ text , var = 'w_trial')
# more text from block to block, carries over
emtrends(model0_base4, pairwise ~ carry_over, var = 'text')
# stroop treatment leaves text unaffected
emtrends(model0_base4, pairwise ~ post_blocks, var = 'text')

# small effect of less purpose over trial
emtrends(model0_base4, pairwise ~ purpose, var = 'w_trial')
# no carry-over effect of purpose
emtrends(model0_base4, pairwise ~ carry_over, var = 'purpose')
# stroop treatment VASTLY reduces purpose
emtrends(model0_base4, pairwise ~ post_blocks, var = 'purpose')


# more text over trials in block
emtrends(model0_base4, pairwise ~ text * purpose, var = 'w_trial')
# more text from block to block, carries over
emtrends(model0_base4, pairwise ~ carry_over, var = 'text')
# stroop treatment leaves text unaffected
emtrends(model0_base4, pairwise ~ post_blocks, var = 'text')

model0_base5 = glmer(response_num ~ congruency * (w_trial + carry_over + post_blocks) * (text + purpose) + 
                       (1 | PROLIFIC_PID) + (1 | rule), 
                     subset(learn3_clean, !is.na(block) & 
                              PROLIFIC_PID %in% pass_list), 
                     family = 'binomial',
                     control = glmerControl(optimizer= "optimx",
                                            optCtrl  = list(method="nlminb")))

anova(model0_base5, model0_base4)
car::Anova(model0_base5)

jtools::summ(model0_base5, digits = 3)

emtrends(model0_base5, pairwise ~ congruency | post_blocks, var = 'text')
emtrends(model0_base5, pairwise ~ congruency | post_blocks, var = 'purpose')

emtrends(model0_base5, pairwise ~ congruency | carry_over, var = 'text')
emtrends(model0_base5, pairwise ~ congruency | carry_over, var = 'purpose')

emtrends(model0_base5, pairwise ~ congruency | text , var = 'w_trial')
emtrends(model0_base5, pairwise ~ congruency | purpose , var = 'w_trial')


# more text over trials in block
emtrends(model0_base4, pairwise ~ text * purpose, var = 'w_trial')
# more text from block to block, carries over
emtrends(model0_base4, pairwise ~ carry_over, var = 'text')
# stroop treatment leaves text unaffected
emtrends(model0_base4, pairwise ~ post_blocks, var = 'text')


learn3_clean$trial_factor = as.factor(learn3_clean$trial)
learn3_clean$tXp = learn3_clean$text * learn3_clean$purpose

model0_base6 = glmer(response_num ~ trial_factor * (text + purpose + tXp) + 
                       (1 | PROLIFIC_PID) + (1 | rule), 
                     , 
                     
                     control = glmerControl(optimizer= "optimx",
                                            optCtrl  = list(method="nlminb")))

model0_base6  = fixest::feglm(response_num ~ trial_factor * (text + purpose), 
              data = subset(learn3_clean, !is.na(block) & PROLIFIC_PID %in% pass_list),
              family = 'binomial',
              cluster = ~ PROLIFIC_PID + rule)

summ6 = summary(model0_base6)
summ6values = summ6$coeftable %>%
  data.frame() %>% 
  mutate(term = row.names(.), 
         effect = case_when(str_detect(term, 'purpose') ~ 'purpose',
                            str_detect(term, 'text') ~ 'text',
                            str_detect(term, 'tXp') ~ 'tXp'),
         trial = as.numeric(gsub("\\D", "", term)),
         trial = if_else(is.na(trial), 1, trial)) 

full_join(summ6values %>% filter(trial == 1) %>% rename(base = Estimate) %>%
            select(effect, base), 
          summ6values) %>% 
  mutate(pred = if_else(trial != 1, base + Estimate, base),
         block = ceiling(trial/12)) %>%
  ggplot(aes(x = trial, y = pred, color = effect, group = paste(block, effect))) + 
  geom_vline(xintercept = 12.5) +
  geom_vline(xintercept = 24.5) +
  geom_vline(xintercept = 36.5) +
  geom_smooth(method = 'lm') +
  geom_point(shape = 21) + 
  theme_classic() +
  scale_x_continuous(limits = c(0.5, 48.5), expand = c(0, 0)) + 
  theme(legend.position = "top")


mod = fixest::feglm(save ~  (trial + appear) * (lag_outcome + within_diff) +
trial * mean_save,
data = subset(cumul, appear > 1),
family = "binomial",
cluster = ~ id + species)

model_fail = glmer(response_num ~ congruency * block * ( text + purpose) + (1 | PROLIFIC_PID) + (1 | rule), 
               learn3_clean,
               family = 'binomial')
car::Anova(model_fail)
jtools::summ(model_fail)

model_fail = glmer(response_num ~ congruency * block * case + (1 | PROLIFIC_PID) + (1 | rule), 
                   learn3_clean,
                   family = 'binomial')

emmeans(model_fail, pairwise ~ block | case *congruency, type = 'response')

emmeans(model_fail, pairwise ~ congruency | case * block, type = 'response')


glimpse(learn3_clean)

learn3_clean %>% 
  filter(case == "u", PROLIFIC_PID %in% pass_list) %>%
  arrange(block, rule, stimulus) %>%
  group_by( block, rule, stimulus, text, purpose, case, congruency) %>%
  summarise(freq = n(), 
            resp = mean(response_num, na.rm = TRUE), 
            low = DescTools::BinomCI(sum(response_num, na.rm = TRUE), n())[2],
            hi = DescTools::BinomCI(sum(response_num, na.rm = TRUE), n())[3]) %>%
 # pivot_wider(names_from = 'congruency', values_from = c('freq', 'resp')) %>%
  #mutate(DescTools::MeanDiffCI(resp_Incongruent, resp_Congruent))
  ggplot(aes(x = stimulus, y = resp, color = congruency)) + 
  geom_hline(yintercept = 0) +
  geom_linerange(aes(ymin = low, ymax = hi), 
                 position = position_dodge(width = .3)) +
  geom_point(shape = 21, 
             position = position_dodge(width = .3)) + coord_flip() +
  facet_grid(rule ~ block, scales = 'free_y') +
  scale_x_discrete() + 
  theme(legend.position = 'top')

emtrends(model_fail, pairwise ~ congruency | block, var = 'purpose')
emtrends(model_fail, pairwise ~ congruency | block, var = 'text')

learn3_clean %>%
  filter(PROLIFIC_PID %in% pass_list) %>%
  mutate(block_trial = ceiling(trial/12)) %>%
  group_by(block_trial, trial, text, purpose, case, congruency, block) %>%
  summarise(resp = mean(response_num, na.rm = TRUE)) %>% 
  ggplot(aes(x = trial, y = resp, linetype = congruency, 
             color = case, group = paste(congruency, case, block_trial))) + 
  geom_vline(xintercept = 12.5) +
  geom_vline(xintercept = 24.5) +
  geom_vline(xintercept = 36.5) +
#  facet_grid(. ~ block_trial, scales = 'free_x') +
  geom_point(aes(shape = congruency)) + 
  geom_smooth(aes(group = paste(congruency, case, block_trial)), 
              method = 'lm') +
  scale_shape_manual(values = c(16, 21))



model_bayes = brm(response_num ~ congruency * block * (text + purpose) + 
                    (1 | PROLIFIC_PID) + (1 | rule), 
                  data = subset(learn3_clean, !is.na(block) & 
                                  PROLIFIC_PID %in% completes$PROLIFIC_PID), 
                  family = 'bernoulli', 
                  prior = c(
                    prior(normal(0, 0.5), class = "b"),       # interactions (or specific coef)
                    prior(exponential(1), class = "sd")
                  ), 
                  iter = 4000, warmup = 1000, chains = 4, cores = 4,
                  save_pars = save_pars(all = TRUE))

model_bayes_null = brm(
  response_num ~ congruency * block * (text + purpose)
  - congruency:block:text +
    (1 | PROLIFIC_PID) + (1 | rule),
  data = subset(learn3_clean, !is.na(block) & 
                  PROLIFIC_PID %in% completes$PROLIFIC_PID),
  family = bernoulli(),
  prior = c(
    prior(normal(0, 0.5), class = "b"),        # interactions (or specific coef)
    prior(exponential(1), class = "sd")
  ),
  iter = 4000, warmup = 1000, chains = 4, cores = 4,
  save_pars = save_pars(all = TRUE)
)

summary(model_bayes)
summary(model_bayes_null)

hypothesis(
  model_bayes,
  "congruencyIncongruent:blockB2:text > 0"
)

bayes_factor(model_bayes_null, model_bayes)

