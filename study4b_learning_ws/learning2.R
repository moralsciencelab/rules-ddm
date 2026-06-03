library(tidyverse)
library(emmeans)
library(lmerTest)

learn2 = bind_rows(read.csv("study4b_learning_ws/data/learning2_batch1.csv"), 
                read.csv("study4b_learning_ws/data/learning2_batch2.csv"))
glimpse(learn2)

# Payment and exclusions ----

demog4b = learn2 %>%
  distinct(subj_code, PROLIFIC_PID, response) %>%
  filter(str_detect(response, "age|gender"), 
         response != 'pagedown') %>%
  mutate(
    key   = map_chr(response, ~names(fromJSON(.x))),
    value = map_chr(response, ~as.character(fromJSON(.x)))
  ) %>%
  select(-response) %>%
  pivot_wider(names_from = key, values_from = value)


## white text instructions
learn2 %>%
  filter(task == 'fixation') %>%
  group_by(run_id, PROLIFIC_PID, response) %>%
  tally() 

completes = learn2 %>%
  filter(!is.na(PROLIFIC_PID)) %>%
  group_by(run_id, PROLIFIC_PID) %>%
  mutate(max_trial = max(trial_index)) %>%
  filter(max_trial == trial_index) %>% 
  filter(str_detect(stimulus, "Thank you for taking part in this experiment!")) %>%
  mutate(complete_time_elapsed = time_elapsed/1000/60) %>%
  select(run_id, PROLIFIC_PID, complete_time_elapsed)

failed_check = learn2 %>%
  filter(str_detect(stimulus, 'too many questions incorrectly')) %>%
  mutate(screen_out_time_elapsed = time_elapsed/1000/60) %>%
  select(run_id, PROLIFIC_PID, screen_out_time_elapsed)

id_list = full_join(completes, failed_check, by = c('run_id', 'PROLIFIC_PID')) 
View(id_list)

id_list_completes = id_list %>%
  filter(is.na(screen_out_time_elapsed), !is.na(complete_time_elapsed)) %>% 
  pull(PROLIFIC_PID)

id_list %>%
  filter(!is.na(complete_time_elapsed), is.na(screen_out_time_elapsed)) %>%
  write.csv('completes_learn2_batch2.csv')

demog4b %>%
  left_join(., completes, by = 'PROLIFIC_PID') %>% 
  distinct(PROLIFIC_PID, age) %>%
  summarise(n(), mean(as.numeric(age), na.rm = TRUE),
            sd(as.numeric(age), na.rm = TRUE))

demog4b %>%
  left_join(., completes, by = 'PROLIFIC_PID') %>% 
  distinct(PROLIFIC_PID, gender) %>%
  mutate(total = n()) %>%
  group_by(gender, total) %>%
  tally() %>% mutate(n/total*100)

# Cleaning ----

learn2_clean = learn2 %>%
  filter(task == 'response') %>%
  mutate(condition = case_when(
    str_detect(condition_assignment, "Control") ~ 'Control',
    str_detect(condition_assignment, "Stroop") ~ 'Stroop',
    str_detect(condition_assignment, "Rules") ~ 'Rules'),
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

## T and P effects ----
model0 = glmer(response_num ~ (text + purpose) + (1 | PROLIFIC_PID) + (1 | rule), 
              subset(learn2_clean, PROLIFIC_PID %in% completes$PROLIFIC_PID), family = 'binomial')
car::Anova(model0)
jtools::summ(model0)
emmeans(model0, ~ text * purpose, type = 'response')

## Baseline effect (block) ----

model0_base = glmer(response_num ~ block * (text + purpose) + 
                 (1 | PROLIFIC_PID) + (1 | rule), 
                 subset(learn2_clean, !is.na(block) &
                      PROLIFIC_PID %in% completes$PROLIFIC_PID), 
               family = 'binomial')
car::Anova(model0_base)
jtools::summ(model0_base, digits = 3, exp = TRUE)

model0_noblock = glmer(response_num ~ (text + purpose) + (1 | PROLIFIC_PID) + (1 | rule), 
               subset(learn2_clean, !is.na(block) &
                      PROLIFIC_PID %in% completes$PROLIFIC_PID), family = 'binomial')
anova(model0_base, model0_noblock)

# Block effect (by treatment) ----

model1_control = glmer(response_num ~ block * (text + purpose) + 
                      (1 | PROLIFIC_PID) + (1 | rule), 
                    subset(learn2_clean, !is.na(block) &
                             condition == 'Control' &
                             PROLIFIC_PID %in% id_list_completes & 
                             as.numeric(rt) > 200), 
                    family = 'binomial')
car::Anova(model1_control)
# approach sig
jtools::summ(model1_control, digits = 3, exp = TRUE)
emtrends(model1_control, pairwise ~ block, var = "text")
emtrends(model1_control, pairwise ~ block, var = "purpose")

model1_stroop = glmer(response_num ~ block * (text + purpose) + 
                          (1 | PROLIFIC_PID) + (1 | rule), 
                        subset(learn2_clean, !is.na(block) &
                                 condition == 'Stroop' &
                                 PROLIFIC_PID %in% id_list_completes), 
                        family = 'binomial')
car::Anova(model1_stroop)
# both sig
jtools::summ(model1_stroop, digits = 3, exp = TRUE)

model1_rules = glmer(response_num ~ block * (text * purpose) + 
                        (1 | PROLIFIC_PID) + (1 | rule), 
                      subset(learn2_clean, !is.na(block) &
                               condition == 'Rules' &
                               PROLIFIC_PID %in% id_list_completes), 
                      family = 'binomial')
car::Anova(model1_rules)
# -purp sig
jtools::summ(model1_rules, digits = 3, exp = TRUE)

# Congruency effects (by treatment) ----

model2_stroop = glmer(response_num ~ congruency * block * (text + purpose) + 
                        (1 | PROLIFIC_PID) + (1 | rule), 
                      subset(learn2_clean, !is.na(block) &
                               condition == 'Stroop' &
                               PROLIFIC_PID %in% completes$PROLIFIC_PID), 
                      family = 'binomial')
car::Anova(model2_stroop)
# increase in T for incongruent
emtrends(model2_stroop, pairwise ~ block | congruency, var = "text")
# decrease in P about the same
emtrends(model2_stroop, pairwise ~ block | congruency, var = "purpose")

model2_rules = glmer(response_num ~ congruency * block * (text + purpose) + 
                        (1 | PROLIFIC_PID) + (1 | rule), 
                      subset(learn2_clean, !is.na(block) &
                               condition == 'Rules' &
                               PROLIFIC_PID %in% completes$PROLIFIC_PID), 
                      family = 'binomial')
car::Anova(model2_rules)
# no increase in T 
emtrends(model2_rules, pairwise ~ block | congruency, var = "text")
emtrends(model2_rules, pairwise ~ block, var = "text")
# decrease in P about the same
emtrends(model2_rules, pairwise ~ block | congruency, var = "purpose")

model1_rules = glmer(response_num ~ block * (text + purpose) + 
                       (1 | PROLIFIC_PID) + (1 | rule), 
                     subset(learn2_clean, !is.na(block) &
                              condition == 'Rules' &
                              PROLIFIC_PID %in% completes$PROLIFIC_PID), 
                     family = 'binomial')
car::Anova(model1_rules)
# -purp sig
jtools::summ(model1_rules, digits = 3, exp = TRUE)

### exploratory congruency model ----

model2_cong = glmer(response_num ~ (condition + congruency) * block * (text + purpose) + 
                       (1 | PROLIFIC_PID) + (1 | rule), 
                     subset(learn2_clean, !is.na(block) &
                              condition != 'Control' &
                              PROLIFIC_PID %in% completes$PROLIFIC_PID), 
                     family = 'binomial')
car::Anova(model2_cong)
emtrends(model2_cong, pairwise ~ block | congruency, var = "purpose")
emtrends(model2_cong, pairwise ~ block | congruency, var = "text")
emtrends(model2_cong, pairwise ~ block | condition, var = "text")

### exploratory rules model ----

learn2_clean = learn2_clean %>%
  mutate(rules_block = paste('Block', ceiling(trial/24)))

model2_rule4 = glmer(response_num ~ rules_block * (text + purpose) + 
                      (1 | PROLIFIC_PID) + (1 | rule), 
                    subset(learn2_clean, 
                             condition == 'Rules' &
                             PROLIFIC_PID %in% completes$PROLIFIC_PID), 
                    family = 'binomial')
car::Anova(model2_rule4)

# the unlearning of purpose is pretty immediate
emtrends(model2_rule4, pairwise ~ rules_block, var = 'text')
emtrends(model2_rule4, pairwise ~ rules_block, var = 'purpose')


model2_rule4c = glmer(response_num ~ congruency * rules_block * (text + purpose) + 
                       (1 | PROLIFIC_PID) + (1 | rule), 
                     subset(learn2_clean, 
                            condition == 'Rules' &
                              PROLIFIC_PID %in% completes$PROLIFIC_PID), 
                     family = 'binomial')
car::Anova(model2_rule4c)

emtrends(model2_rule4c, pairwise ~ rules_block | congruency, var = 'text')
emtrends(model2_rule4c, pairwise ~ rules_block | congruency, var = 'purpose')

bind_rows(
  emtrends(model2_rule4c, ~ rules_block | congruency, var = 'text') %>%
    as.data.frame() %>% mutate(effect = 'text', 
                               task = 'Rules') %>% rename(trend = text.trend),
  emtrends(model2_rule4c, ~ rules_block  | congruency, var = 'purpose') %>%
    as.data.frame() %>% mutate(effect = 'purpose', 
                               task = 'Rules') %>% rename(trend = purpose.trend),
  emtrends(model2_stroop, ~ block | congruency, var = "text")  %>%
    as.data.frame() %>% mutate(effect = 'text', 
                               task = 'Stroop', 
                               rules_block = case_when(block == 'B1' ~ 'Block 1',
                                                       block == 'B2' ~ 'Block 2')) %>% rename(trend = text.trend),
  emtrends(model2_stroop, ~ block | congruency, var = "purpose") %>%
    as.data.frame() %>% mutate(effect = 'purpose', 
                               task = 'Stroop', 
                               rules_block = case_when(block == 'B1' ~ 'Block 1',
                                                       block == 'B2' ~ 'Block 2')) %>% rename(trend = purpose.trend)) %>%
  ggplot(aes(x = rules_block, y = trend, color = effect)) + 
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL, group = paste(effect, task)), 
                 position = position_dodge(width = .5)) +
  geom_line(aes(group = paste(effect, task), linetype = task), 
            position = position_dodge(width = .5)) +
  geom_point(aes(shape = congruency, group = paste(effect, task)), size = 2, 
             position = position_dodge(width = .5)) +
  facet_grid(. ~ congruency)

# Single subject regression ----

ss_data = data.frame(
  PROLIFIC_PID = character(),
  congruency = character(),
  condition = character(),
  pre.text = numeric(),
  post.text = numeric(),
  pre.purpose = numeric(),
  post.purpose = numeric()
)

for (i in levels(as.factor(completes$PROLIFIC_PID))) {
  temp_dt = learn2_clean %>%
    filter(PROLIFIC_PID == i)
  
  cond_dt = temp_dt %>%
    select(congruency, condition) %>%
    distinct() %>%
    suppressMessages()
  
  pre_model = lm(response_num ~ text + purpose, 
                 subset(temp_dt, block == 'B1'))
  post_model = lm(response_num ~ text + purpose, 
                 subset(temp_dt, block == 'B2'))
  
  new_row = data.frame(PROLIFIC_PID = i, 
              congruency = cond_dt$congruency,
              condition = cond_dt$condition, 
              pre.text = as.numeric(pre_model$coefficients[2]),
              post.text = as.numeric(post_model$coefficients[2]),
              pre.purpose = as.numeric(pre_model$coefficients[3]),
              post.purpose = as.numeric(post_model$coefficients[3]))
  
  ss_data = rbind(ss_data, new_row)
  
  cat(nrow(ss_data), ' of ', length(completes$PROLIFIC_PID), '\n')
}

#learning: 1 if Rules-Congruent or Rules-Incongruent
#conflict: 1 if Stroop-Incongruent or Rules-Incongruent
#reinforcement: 1 if Rules-Congruent
#heuristic: 1 if Rules-Incongruent

ss_data = ss_data %>%
  mutate(diff_text = post.text - pre.text, 
         diff_purpose = post.purpose - pre.purpose, 
         learning = if_else(condition == 'Rules', 1, 0),
         conflict = if_else(congruency == 'Incongruent', 1, 0),
         reinforcement = if_else(condition == 'Rules' & congruency == 'Congruent', 1, 0),
         heuristic = if_else(condition == 'Rules' & congruency == 'Incongruent', 1, 0))

shapiro.test(ss_data$diff_text)
shapiro.test(ss_data$diff_purpose)

model0_constant = lm(diff_text ~ 1, ss_data)
AIC(model0_constant)

model1_l = lm(diff_text ~ learning, ss_data)
jtools::summ(model1_l, digits = 3)
AIC(model1_l)

model1_c = lm(diff_text ~ conflict, ss_data)
jtools::summ(model1_c, digits = 3)
AIC(model1_c)

model1_r = lm(diff_text ~ reinforcement, ss_data)
jtools::summ(model1_r, digits = 3)
AIC(model1_r)

model1_h = lm(diff_text ~ heuristic, ss_data)
jtools::summ(model1_h, digits = 3)
AIC(model1_h)

model1_both = lm(diff_text ~ learning + conflict, ss_data)
jtools::summ(model1_both, digits = 3)
AIC(model1_both)

model1_all = lm(diff_text ~ reinforcement + heuristic + conflict, ss_data)
jtools::summ(model1_all, digits = 3)
AIC(model1_all)


model0_constant = lm(diff_purpose ~ 1, ss_data)
jtools::summ(model0_constant, digits = 3)
AIC(model0_constant)

model1_l = lm(diff_purpose ~ learning, ss_data)
jtools::summ(model1_l, digits = 3)
AIC(model1_l)

model1_c = lm(diff_purpose ~ conflict, ss_data)
jtools::summ(model1_c, digits = 3)
AIC(model1_c)

model1_r = lm(diff_purpose ~ reinforcement, ss_data)
jtools::summ(model1_r, digits = 3)
AIC(model1_r)

model1_h = lm(diff_purpose ~ heuristic, ss_data)
jtools::summ(model1_h, digits = 3)
AIC(model1_h)

model1_both = lm(diff_purpose ~ learning + conflict, ss_data)
jtools::summ(model1_both, digits = 3)
AIC(model1_both)

model1_all = lm(diff_purpose ~ reinforcement + heuristic + conflict, ss_data)
jtools::summ(model1_all, digits = 3)
AIC(model1_all)

flip_model = glm(conflict ~ diff_text, ss_data, 
                 family = 'binomial')
jtools::summ(flip_model, digits = 3)
AIC(flip_model)

flip_model2 = glm(conflict ~ diff_text + diff_purpose, ss_data, 
                 family = 'binomial')
jtools::summ(flip_model2, digits = 3)
AIC(flip_model2)

anova(flip_model2, flip_model)

## Learning (trial effects) ----
ggplot(learn2_clean, aes(x = trial, y = response_num, color = case)) + 
  geom_smooth(aes(linetype = congruency), 
              method = 'lm') + facet_grid(. ~ condition)

model1t = glmer(response_num ~ condition * z_trial * (text + purpose) + 
                (1 | PROLIFIC_PID) + (1 | rule), 
              subset(learn2_clean, PROLIFIC_PID %in% completes$PROLIFIC_PID), 
              family = 'binomial')
car::Anova(model1t)

emtrends(model1t, pairwise ~ text, var = 'z_trial')
emtrends(model1t, pairwise ~ purpose, var = 'z_trial')

