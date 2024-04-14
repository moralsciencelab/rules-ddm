library(tidyverse)
library(emmeans)
library(lavaan)

### Part 1. Behavioral results

# stfl1 = read.csv('colors-arrows-rules_backup_v1.csv')
stfl = read.csv('colors-arrows-rules.csv')
glimpse(stfl)

stroop = stfl %>%
  filter(color != '', text != '') %>%
  mutate(congruent = color == text, 
         response = case_when(correct == 'true' ~ 1, 
                                 correct == 'false' ~ 0)) %>%
  select(subj_code, response, congruent, rt, trial_index)

stroop %>% 
  group_by(response, congruent) %>%
  summarise(median(as.numeric(rt), na.rm = TRUE), 
            n())

glimpse(stroop)

ggplot(subset(stroop, as.numeric(rt) > 0 & subj_code %in% IDlist), aes(x = trial_index, y = as.numeric(rt), color = congruent)) + 
  geom_hline(yintercept = 300) + 
  geom_jitter() + 
  geom_smooth()

acc_stroop = glmer(response ~ congruent + (1 | subj_code), data = stroop, family = 'binomial')
car::Anova(acc_stroop)
emmeans(acc_stroop, pairwise ~ congruent, type = 'response')

rt_stroop = lmer(rt ~ congruent + (1 | subj_code), data = flanker)
car::Anova(rt_stroop)
emmeans(rt_stroop, pairwise ~ congruent, type = 'response')

flanker = stfl %>%
  filter(flanker_stim_type %in% c('congruent', 'incongruent')) %>%
  mutate(response = case_when(correct == 'true' ~ 1, 
                              correct == 'false' ~ 0), 
         rt = as.numeric(rt)) %>%
  select(subj_code, congruent = flanker_stim_type, response, rt, trial_index)

ggplot(subset(flanker, as.numeric(rt) > 0 & subj_code %in% IDlist), aes(x = trial_index, y = as.numeric(rt), color = congruent)) + 
  geom_hline(yintercept = 250) + 
  geom_jitter() + 
  geom_smooth()

acc_flanker = glmer(response ~ congruent + (1 | subj_code), data = flanker, family = 'binomial')
car::Anova(acc_flanker)
emmeans(acc_flanker, pairwise ~ congruent, type = 'response')

rt_flanker = lmer(rt ~ congruent + (1 | subj_code), data = flanker)
car::Anova(rt_flanker)
emmeans(rt_flanker, pairwise ~ congruent, type = 'response')


rules = left_join(stfl, coding, by = 'stimulus') %>%
  filter(!is.na(purpose)) %>%
  mutate(resp = 
           case_when(
             response == 'e' & str_detect(key_assignment, 'e for yes') ~ 1, 
             response == 'e' & str_detect(key_assignment, 'i for yes') ~ 0,
             response == 'i' & str_detect(key_assignment, 'i for yes') ~ 1, 
             response == 'i' & str_detect(key_assignment, 'e for yes') ~ 0
           ), 
         rt = as.numeric(rt), 
         rule =
           case_when(str_detect(stimulus, 'university post') ~ 'noise',
                     str_detect(stimulus, 'train station') ~ 'sleep',
                     str_detect(stimulus, 'deer population') ~ 'shoot',
                     str_detect(stimulus, 'traffic accident') ~ 'drink',
                     str_detect(stimulus, 'cars are allowed') ~ 'cars',
                     str_detect(stimulus, 'headmaster') ~ 'phones',
                     str_detect(stimulus, 'avoid accidents') ~ 'lab',
                     str_detect(stimulus, 'A  restaurant') ~ 'dogs',
                     str_detect(stimulus, 'house clean') ~ 'shoes')) %>%
  filter(rule != 'noise') %>%
  mutate(congruent = text.y == purpose) %>%
  select(subj_code, trial_index, text = text.y, purpose, congruent, response = resp, rt, rule)

ggplot(subset(rules, as.numeric(rt) > 0), 
       aes(x = trial_index, y = as.numeric(rt), color = congruent)) + 
  geom_hline(yintercept = 500) + 
  geom_jitter() + 
  geom_smooth()

acc_rules = glmer(response ~ text*purpose + (1 | subj_code) + (1 | rule), 
                  data = rules, family = 'binomial')
car::Anova(acc_rules)
emmeans(acc_rules, pairwise ~ purpose | text, type = 'response')

rt_rules = lmer(rt ~ text*purpose  + (1 | subj_code) + (1 | rule), 
                data = rules)
car::Anova(rt_rules)
emmeans(rt_rules, pairwise ~ purpose | text, type = 'response')

theory = stfl %>% filter(str_detect(response, "theory_question")) %>%
  mutate(theory = case_when(str_detect(response, "letter") ~ 't_theory',
                            str_detect(response, "spirit") ~ 'p_theory')) %>%
  select(subj_code, theory)

rules = full_join(rules, theory, by = 'subj_code')


stroop_clean = stroop %>%
  mutate(response = case_when(
    correct == 'true' ~ 1,
    correct == 'false' ~ 0), 
    rt = as.numeric(rt), 
    stim_type = case_when(
      congruent == TRUE ~ 'Congruent',
      congruent == FALSE ~ 'Incongruent'
    )) %>%
  select(subj_code, stim_type, response, rt)

flanker_clean = flanker %>%
  mutate(response = case_when(
    correct == 'true' ~ 1,
    correct == 'false' ~ 0), 
    rt = as.numeric(rt), 
    stim_type = case_when(
      flanker_stim_type == "congruent" ~ 'Congruent',
      flanker_stim_type == "incongruent" ~ 'Incongruent'
    )) %>%
  select(subj_code, stim_type, response, rt)


mod1t = lmer(rt ~ text*purpose*theory + (1 | rule) + (1 | subj_code), 
            subset(rules, rt > 200))
car::Anova(mod1t)
emmeans(mod1t, pairwise ~ text | theory, type = 'response')
emmeans(mod1t, pairwise ~ theory | text * purpose, type = 'response')

mod2t = glmer(response ~ text*purpose*theory + (1 | rule) + (1 | subj_code), 
             subset(rules, rt > 200), family = 'binomial')
car::Anova(mod2t)
emmeans(mod2t, pairwise ~ text | theory, type = 'response')
emmeans(mod2t, pairwise ~ purpose | theory, type = 'response')
emmeans(mod2t, pairwise ~ theory | text * purpose, type = 'response')

write.csv(rules, 'rules_clean.csv', row.names = FALSE)
write.csv(flanker_clean, 'flanker_clean.csv', row.names = FALSE)
write.csv(stroop_clean, 'stroop_clean.csv', row.names = FALSE)


### Part 2. DDM correlations

st1 = read.csv('ddm_output/model1stroop_output.csv') %>%
  rename(stroop = mean)  %>%
  filter(str_detect(X, '_subj.'))
fl1 = read.csv('ddm_output/model1flanker_output.csv') %>%
  rename(flanker = mean) %>%
  filter(str_detect(X, '_subj.'))

r1 = read.csv('ddm_output/model3rulesFS_output.csv') %>%
  rename(rules = mean) %>%
  filter(str_detect(X, '_subj.')) %>%
  mutate(
    parameter = str_sub(X, 1, 1), 
    subj_code = str_sub(X, -12, -1),
    intercept = str_detect(X, 'Intercept'), 
    conflict = str_detect(X, '(conflict)'), 
    text = str_detect(X, 'text'), 
    term = case_when(
      intercept == TRUE & conflict == FALSE & text == FALSE ~ 'intercept',
      intercept == FALSE & conflict == FALSE & text == FALSE ~ '',
      intercept == FALSE & conflict == TRUE & text == FALSE ~ 'conflict',
      intercept == FALSE & conflict == FALSE & text == TRUE ~ 'text',
      intercept == FALSE & conflict == TRUE & text == TRUE ~ 'conflictXtext')
  ) %>%
  select(rules, parameter, subj_code, term)

r1wide = r1 %>%
  pivot_wider(names_from = c('term', 'parameter'), values_from = 'rules', 
              id_cols = c('subj_code')) %>%
  rename(rules_a = `_a`, rules_t = `_t`, rules_z = `_z`) %>%
  unnest()

stfl1 = full_join(st1, fl1, by = 'X') %>%
  mutate(parameter = str_sub(X, 1, 1), 
         subj_code = str_sub(X, -12, -1),
         term = case_when(
           str_detect(X, 'Intercept') ~ 'intercept',
           str_detect(X, 'conflict') ~ 'conflict'
         )) %>%
  select(stroop, flanker, parameter, subj_code, term) 

stfl1wide = stfl1 %>%
  pivot_wider(names_from = c('term', 'parameter'), values_from = c('stroop', 'flanker'), 
              id_cols = 'subj_code')%>%
  rename(stroop_a = stroop_NA_a, stroop_t = stroop_NA_t, 
         flanker_a = flanker_NA_a, flanker_t = flanker_NA_t)

cortable_parameters = full_join(r1wide, stfl1wide, by = 'subj_code') 

### mahalanobis distance (multivariate outlier detection)

mahal_vector = cortable_parameters %>%
  select(contains('rules'))
cortable_parameters$mahalanobisR = mahalanobis(mahal_vector, colMeans(mahal_vector), cov(mahal_vector))
cortable_parameters$pvalueR <- pchisq(cortable_parameters$mahalanobisR, df=ncol(mahal_vector) - 1, lower.tail=FALSE)

mahal_vector = cortable_parameters %>%
  select(contains('stroop'))
cortable_parameters$mahalanobisS = mahalanobis(mahal_vector, colMeans(mahal_vector), cov(mahal_vector))
cortable_parameters$pvalueS <- pchisq(cortable_parameters$mahalanobisS, df=ncol(mahal_vector) - 1, lower.tail=FALSE)

mahal_vector = cortable_parameters %>%
  select(contains('flanker'))
cortable_parameters$mahalanobisF = mahalanobis(mahal_vector, colMeans(mahal_vector), cov(mahal_vector))
cortable_parameters$pvalueF <- pchisq(cortable_parameters$mahalanobisF, df=ncol(mahal_vector) - 1, lower.tail=FALSE)

cutoff_mahal = .001

cortable_parameters = cortable_parameters %>%
  mutate(outlier = case_when(
    pvalueR < cutoff_mahal ~ 'Rules',
    pvalueF < cutoff_mahal ~ 'Flanker',
    pvalueS < cutoff_mahal ~ 'Stroop'
  ))

xtabs(~ outlier, cortable_parameters)

cortable_parameters %>%
  select(contains("_a"), contains("_t"),contains("_v"), contains("_z")) %>%
  apaTables::apa.cor.table(., 'DDM_CorrTable.doc')

### Part 3. Bifactor analysis

st2 = read.csv('ddm_output/model2stroop_output.csv') %>%
  rename(stroop = mean)  %>%
  filter(str_detect(X, '_subj.')) %>%
  mutate(parameter = str_sub(X, 1, 1), 
         subj_code = str_sub(X, -12, -1), 
         block = case_when(str_detect(X, '\\(1') ~ '1',
                           str_detect(X, '\\(2') ~ '2',
                           str_detect(X, '\\(3') ~ '3',
                           str_detect(X, '\\(4') ~ '4'), 
         conflict = case_when(
           str_detect(X, 'True') ~ 'incongruent',
           str_detect(X, 'False') ~ 'congruent'
         )) %>%
  select(subj_code, stroop, parameter, block, conflict) %>%
  pivot_wider(names_from = 'block', values_from = 'stroop', names_prefix = 'stroop') 

fl2 = read.csv('ddm_output/model2flanker_output.csv') %>%
  rename(flanker = mean) %>%
  filter(str_detect(X, '_subj.')) %>%
  mutate(parameter = str_sub(X, 1, 1), 
         subj_code = str_sub(X, -12, -1),
         block = case_when(str_detect(X, '\\(1') ~ '1',
                           str_detect(X, '\\(2') ~ '2',
                           str_detect(X, '\\(3') ~ '3',
                           str_detect(X, '\\(4') ~ '4'), 
         conflict = case_when(
           str_detect(X, 'True') ~ 'incongruent',
           str_detect(X, 'False') ~ 'congruent'
         )) %>%
  select(subj_code, flanker, parameter, block, conflict) %>%
  pivot_wider(names_from = 'block', values_from = 'flanker', names_prefix = 'flanker')

r2 = read.csv('ddm_output/model4rulesFS_output.csv') %>%
  rename(rules = mean) %>%
  filter(str_detect(X, '_subj.')) %>%
  mutate(parameter = str_sub(X, 1, 1), 
         subj_code = str_sub(X, -12, -1),
         block = case_when(str_detect(X, '\\(1') ~ '1',
                           str_detect(X, '\\(2') ~ '2',
                           str_detect(X, '\\(3') ~ '3',
                           str_detect(X, '\\(4') ~ '4'), 
         conflict = case_when(
           str_detect(X, 'True') ~ 'incongruent',
           str_detect(X, 'False') ~ 'congruent'
         )) %>%
  select(subj_code, rules, parameter, block, conflict) %>%
  pivot_wider(names_from = 'block', values_from = 'rules', names_prefix = 'rules')

r2 %>%
  filter(parameter == 'a') %>%
  select(rules1:rules4) %>%
  psych::alpha(., check.keys = TRUE)

r2 %>%
  filter(parameter == 't') %>%
  select(rules1:rules4) %>%
  psych::alpha(., check.keys = TRUE)

r2 %>%
  filter(parameter == 'v', conflict == 'congruent') %>%
  select(rules1:rules4) %>%
  psych::alpha(., check.keys = TRUE)

r2 %>%
  filter(parameter == 'v', conflict == 'incongruent') %>%
  select(rules1:rules4) %>%
  psych::alpha(., check.keys = TRUE)

bifactor_data = full_join(r2, full_join(st2, fl2, by = c('subj_code', 'parameter', 'conflict')), 
                  by = c('subj_code', 'parameter', 'conflict'))

glimpse(bifactor_data)

bi_model <- 'flank =~ flanker1 + flanker2 + flanker3 + flanker4
                stroop =~ stroop1 + stroop2 + stroop3 + stroop4 
                rules =~ rules1 + rules2 + rules3 + rules4
                common =~ flanker1 + flanker2 + flanker3 + flanker4 + stroop1 + stroop2 + stroop3 + stroop4 + rules1 + rules2 + rules3 + rules4 
                '

### For non-decision time (remove stroop1)
bifactor_fit_t <- cfa(model = bi_model, data = subset(bifactor_data, parameter == 't'), 
                      std.lv = TRUE, orthogonal = TRUE)
summary(bifactor_fit_t, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

### Boundary separation
bifactor_fit_a <- cfa(model = bi_model, data = subset(bifactor_data, parameter == 'a'), 
                      std.lv = TRUE, orthogonal = TRUE)
summary(bifactor_fit_a, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

## congruent drift rate
bifactor_fit_congruent <- cfa(model = bi_model, 
                              data = subset(bifactor_data, parameter == 'v' & conflict == 'congruent'), 
                              std.lv = TRUE, orthogonal = TRUE)
summary(bifactor_fit_congruent, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

bifactor_fit_incongruent <- cfa(model = bi_model, 
                                data = subset(bifactor_data, parameter == 'v' & conflict == 'incongruent'), 
                                std.lv = TRUE, orthogonal = TRUE)
summary(bifactor_fit_incongruent, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
