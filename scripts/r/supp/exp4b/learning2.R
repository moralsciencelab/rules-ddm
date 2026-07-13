library(tidyverse)
library(emmeans)
library(lmerTest)
library(logistf)

learn2 = bind_rows(read.csv("learning2/data/learning2_batch1.csv"), 
                read.csv("learning2/data/learning2_batch2.csv"))
View(learn2)

# Payment and exclusions ----

## white text instructions
learn2 %>%
  filter(task == 'fixation') %>%
  group_by(run_id, PROLIFIC_PID, response) %>%
  tally() %>%
  View()

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

id_list %>%
  filter(!is.na(complete_time_elapsed), is.na(screen_out_time_elapsed)) %>%
  write.csv('completes_learn2_batch2.csv')

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
  full_join(., read.csv('learning2/data/stimulus_coding.csv'), by = 'stimulus') %>%
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

model0_base2 = glmer(response_num ~ condition * block * (text + purpose) + 
                      (1 | PROLIFIC_PID) + (1 | rule), 
                    subset(learn2_clean, !is.na(block) &
                             PROLIFIC_PID %in% completes$PROLIFIC_PID), 
                    family = 'binomial')
car::Anova(model0_base2)
emtrends(model0_base2, pairwise ~ block | condition, var = 'text')
emtrends(model0_base2, pairwise ~ block | condition, var = 'purpose')

learn2_clean = learn2_clean %>%
  mutate(treatment = paste(congruency, condition))

model0_base3 = glmer(response_num ~ treatment* block * (text + purpose) + 
                       (1 | PROLIFIC_PID) + (1 | rule), 
                     subset(learn2_clean, !is.na(block) & 
                              PROLIFIC_PID %in% completes$PROLIFIC_PID), 
                     family = 'binomial')
car::Anova(model0_base3)
emtrends(model0_base3, pairwise ~ block | treatment, var = 'purpose')
emtrends(model0_base3, pairwise ~ block | treatment, var = 'text')


anova(model0_base2, model0_base)

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
                             PROLIFIC_PID %in% completes$PROLIFIC_PID), 
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
                                 PROLIFIC_PID %in% completes$PROLIFIC_PID), 
                        family = 'binomial')
car::Anova(model1_stroop)
# both sig
jtools::summ(model1_stroop, digits = 3, exp = TRUE)

model1_rules = glmer(response_num ~ block * (text + purpose) + 
                        (1 | PROLIFIC_PID) + (1 | rule), 
                      subset(learn2_clean, !is.na(block) &
                               condition == 'Rules' &
                               PROLIFIC_PID %in% completes$PROLIFIC_PID), 
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
                 subset(temp_dt, block == 'B1'), family = 'binomial')
  post_model = lm(response_num ~ text + purpose, 
                 subset(temp_dt, block == 'B2'), family = 'binomial')
  
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

id_diffs = learn2_clean %>% 
  filter(PROLIFIC_PID %in% completes$PROLIFIC_PID) %>%
  group_by(PROLIFIC_PID, condition, congruency, block, text, purpose) %>%
  summarise(resp = mean(response_num,na.rm = TRUE)) %>%
  pivot_wider(id_cols = c('condition', 'congruency', 'PROLIFIC_PID', 'block'), names_from = c('text', 'purpose'),
              values_from = resp) %>%
  mutate(text = `1_0` - `0_1`, 
         puni = `1_1` - `0_0`) %>%
  select(condition, congruency, PROLIFIC_PID, block, text, puni) %>%
  pivot_wider(id_cols = c('condition', 'congruency', 'PROLIFIC_PID'), 
              names_from = 'block',
              values_from = c('text', 'puni')) %>%
  select(-text_NA, -puni_NA) %>%
  mutate(text_diff = text_B2 - text_B1, 
         puni_diff = puni_B2 - puni_B1)

ggplot(id_diffs, aes(x = reorder(PROLIFIC_PID, -text_diff), y = text_diff)) + 
  geom_col(alpha = .3, width = 1, aes(fill = text_diff > 0)) + coord_flip() + 
  theme(axis.text.y = element_blank()) + 
  facet_wrap(~ reorder(condition, -text_diff), nrow = 1, scales = 'free_y')

ggplot(id_diffs, aes(x = reorder(PROLIFIC_PID, -puni_diff), y = puni_diff)) + 
  geom_col(alpha = .3, width = 1, aes(fill = puni_diff > 0)) + coord_flip() + 
  theme(axis.text.y = element_blank()) + 
  facet_wrap(~ reorder(condition, -puni_diff), nrow = 1, scales = 'free_y')

ss_data = ss_data %>%
  mutate(diff_text = post.text - pre.text, 
         diff_purpose = post.purpose - pre.purpose, 
         work = if_else(condition != "Control", 1, 0),
         learning = if_else(condition == 'Rules', 1, 0),
         conflict = if_else(congruency == 'Incongruent', 1, 0),
         reinforcement = if_else(condition == 'Rules' & congruency == 'Congruent', 1, 0),
         heuristic = if_else(condition == 'Rules' & congruency == 'Incongruent', 1, 0))

shapiro.test(ss_data$diff_text)
shapiro.test(ss_data$diff_purpose)

ss_means = ss_data %>%
  group_by(condition) %>%
  summarise(diff_text = median(diff_text), 
            diff_purpose = median(diff_purpose))

ggplot(ss_data, aes(x = diff_text, y = diff_purpose)) + 
#  geom_jitter(alpha = .3, aes(fill = condition), shape = 21) + 
  geom_point(data = ss_means, size = 3, fill = 'white', aes(color = condition))

ggplot(ss_data, aes(x = reorder(PROLIFIC_PID, -diff_text), y = diff_text)) + 
  geom_col(alpha = .3, width = 1) + coord_flip() + 
  theme(axis.text.y = element_blank()) + 
  facet_wrap(~ condition, nrow = 1, scales = 'free_y')

ggplot(ss_data, aes(x = reorder(PROLIFIC_PID, -diff_purpose), y = diff_purpose)) + 
  geom_col(alpha = .3, width = 1) + coord_flip() + 
  theme(axis.text.y = element_blank()) + 
  facet_wrap(~ condition, nrow = 1, scales = 'free_y')

ggplot(ss_data, aes(x = diff_text, y= diff_purpose, fill = conflict)) + 
  geom_jitter() + stat_ellipse(aes(group = conflict)) + 
  geom_smooth()

ggplot(ss_data, aes(x = diff_text, y= diff_purpose, color = conflict)) + 
  geom_jitter() + stat_ellipse(aes(group = conflict)) + 
  geom_smooth()


model0_constant = ordinal::clm(formula = as.factor(diff_text) ~ condition, 
                               data = ss_data, threshold = 'equidistant')
summary(model0_constant)
AIC(model0_constant)
jtools::summ(model0_constant, digits = 3)

model1_l = lm(diff_purpose ~ condition, ss_data)
emmeans(model1_l, pairwise ~ condition)

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

model0_constant = lm(diff_purpose ~ work, ss_data)
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

learn2_clean %>%
  select(PROLIFIC_PID, rule, text, purpose, condition, congruency, rt, response_num) %>%
  write.csv(., 'learning2/learning2ddm.csv', row.names = FALSE)


learn2_clean = learn2_clean %>%
  mutate(trial_norm = if_else(
    condition == 'Rules' & trial > 24, trial - 48, trial))

mean_resps2 = learn2_clean %>%
  ungroup() %>%
  filter(as.numeric(rt) > 500, !is.na(block)) %>%
  group_by(trial_norm, block, condition, text, purpose, case) %>%
  summarise(response_num = mean(response_num, na.rm = TRUE))
  
  
ggplot(data = mean_resps2, aes(x = trial_norm, y = response_num)) + 
  annotate(geom = 'rect', ymin = -Inf, ymax = .5, xmin = -Inf, xmax = Inf, 
           fill = 'lightgrey', alpha = .2) +
  geom_vline(xintercept = 24.5, linetype = 3) +
  geom_line(aes(group = case, color = as.factor(text), 
                linetype = text != purpose), alpha = .4) +
  geom_smooth(data = subset(learn2_clean, as.numeric(rt) > 500 & !is.na(block)), 
              aes(group = paste(case, block), fill = as.factor(text), 
                  color = as.factor(text), 
                  linetype = text != purpose), linewidth = .5,
              method = "glm", 
              method.args = list(family = "binomial")) + 
  facet_grid(condition ~ block, scales = 'free_x', space = 'free_x') +
  theme_classic() + 
  theme(legend.position = 'none', axis.ticks = element_blank(), 
        strip.text.y = element_text(angle = 0), 
        strip.background = element_blank()) + 
  scale_y_continuous(name = NULL, limits = c(0, 1), expand = c(0, 0), 
                     breaks = seq(0, 1, .50), labels = c('No', '', 'Yes')) +
  scale_x_continuous(expand = c(0, 0))

emtrends(model0_base3, pairwise ~ block | treatment, var = 'purpose')
emtrends(model0_base3, pairwise ~ block | treatment, var = 'text')

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



ddm_control = read.csv('~/Documents/ddm_rules/model_learn2_control/model_learn2_control_within_traces.csv') %>%
  select(-contains(c('_subj', '_std')), -X) %>%
  mutate(z = exp(z_trans)/(1 + exp(z_trans))) %>%
  select(-z_trans) %>%
  gather(key = 'item', value = 'value', na.rm = TRUE) %>%
  filter(item %in% c('a_C.block..T.B2.', 't_C.block..T.B2.', 
                     'v_C.text..T.1..C.block..T.B2.', 'v_C.purpose..T.1..C.block..T.B2.')) %>%
  mutate(parameter = 
           case_when(item == "a_C.block..T.B2." ~ "Δa",
                     item == "t_C.block..T.B2." ~ "Δt",
                     item == "v_C.text..T.1..C.block..T.B2." ~ "Δv-text",
                     item == "v_C.purpose..T.1..C.block..T.B2." ~ "Δv-purpose"), 
         treatment = 'Passive Control') %>%
  select(-item)

ddm_stroop = read.csv('~/Documents/ddm_rules/model_learn2_stroop/model_learn2_stroop_within_traces.csv') %>%
  select(-contains(c('_subj', '_std')), -X) %>%
  mutate(z = exp(z_trans)/(1 + exp(z_trans))) %>%
  select(-z_trans) %>%
  gather(key = 'item', value = 'value', na.rm = TRUE) %>%
  filter(item %in% c('a_C.block..T.B2.', 't_C.block..T.B2.', 
                     'v_C.text..T.1..C.block..T.B2.', 'v_C.purpose..T.1..C.block..T.B2.')) %>%
  mutate(parameter = 
           case_when(item == "a_C.block..T.B2." ~ "Δa",
                     item == "t_C.block..T.B2." ~ "Δt",
                     item == "v_C.text..T.1..C.block..T.B2." ~ "Δv-text",
                     item == "v_C.purpose..T.1..C.block..T.B2." ~ "Δv-purpose"), 
         treatment = 'Conflict') %>%
  select(-item)

ddm_rules = read.csv('~/Documents/ddm_rules/model_learn2_rules/model_learn2_rules_within_traces.csv') %>%
  select(-contains(c('_subj', '_std')), -X) %>%
  mutate(z = exp(z_trans)/(1 + exp(z_trans))) %>%
  select(-z_trans) %>%
  gather(key = 'item', value = 'value', na.rm = TRUE) %>%
  filter(item %in% c('a_C.block..T.B2.', 't_C.block..T.B2.', 
                     'v_C.text..T.1..C.block..T.B2.', 'v_C.purpose..T.1..C.block..T.B2.')) %>%
  mutate(parameter = 
           case_when(item == "a_C.block..T.B2." ~ "Δa",
                     item == "t_C.block..T.B2." ~ "Δt",
                     item == "v_C.text..T.1..C.block..T.B2." ~ "Δv-text",
                     item == "v_C.purpose..T.1..C.block..T.B2." ~ "Δv-purpose"), 
         treatment = 'Learning') %>%
  select(-item)

ddm_fig4 = bind_rows(ddm_control, ddm_stroop, ddm_rules)

ddm_fig4_mdn = ddm_fig4 %>%
  group_by(parameter, treatment) %>%
  summarise(value = median(value,na.rm = TRUE))

ggplot(ddm_fig4, aes(x = treatment, y = value)) +
  geom_violin(trim = FALSE, draw_quantiles = c(.25, .75)) + 
  facet_wrap(~ parameter, scales = 'free_x', nrow = 2) + 
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(data = ddm_fig4_mdn, aes(y = value)) +
  coord_flip() + 
  theme_classic() +
  theme(strip.background = element_blank(), 
        axis.ticks = element_blank())



