library(tidyverse)
library(emmeans)
library(lavaan)

### Part 1. Behavioral results

# stfl1 = read.csv('colors-arrows-rules_backup_v1.csv')
stfl1 = read.csv('stroopflanker_data/strata1.csv')
stfl2 = read.csv('stroopflanker_data/strata2.csv')
stfl3 = read.csv('stroopflanker_data/strata3.csv')
stfl4 = read.csv('stroopflanker_data/strata4.csv')
stfl5 = read.csv('stroopflanker_data/strata5.csv')
stfl6 = read.csv('stroopflanker_data/strata6.csv')
stfl = bind_rows(stfl1, stfl2, stfl3, stfl4, stfl5, stfl6)
glimpse(stfl)

stroop = stfl %>%
  filter(color != '', text != '') %>%
  mutate(congruent = color == text, 
         response = case_when(correct == 'true' ~ 1, 
                                 correct == 'false' ~ 0),
         rt = as.numeric(rt)) %>%
  select(subj_code, response, congruent, rt , trial_index)

stroop %>% 
  group_by(response, congruent) %>%
  summarise(median(as.numeric(rt), na.rm = TRUE), 
            n())

ggplot(subset(stroop, as.numeric(rt) > 300), aes(x = trial_index, y = as.numeric(rt), color = congruent)) + 
  geom_hline(yintercept = 300) + 
  geom_jitter() + 
  geom_smooth()

acc_stroop = glmer(response ~ congruent + (1 | subj_code), 
                   data = subset(stroop, rt > 300), family = 'binomial')
car::Anova(acc_stroop)
emmeans(acc_stroop, pairwise ~ congruent, type = 'response')

rt_stroop = lmer(rt ~ congruent + (1 | subj_code), data = subset(stroop, rt > 300))
car::Anova(rt_stroop)
emmeans(rt_stroop, pairwise ~ congruent, type = 'response')

stroopMeans = stroop %>%
  filter(rt > 300) %>%
  group_by(congruent) %>%
  summarise(rt = median(rt, na.rm = TRUE))

ggplot() + 
  geom_histogram(data=subset(stroop, rt > 300 & as.factor(response) == '1'), fill = 'white', binwidth = 100,
                 aes(rt, color=as.factor(response), y= ..count../100), linewidth = .2) +
  geom_histogram(data=subset(stroop, rt > 300 & as.factor(response) == '0'), fill = 'white', binwidth = 100,
                 aes(rt, color=as.factor(response), y= -..count../100), linewidth = .2) +
  facet_grid(reorder(congruent, rt) ~ ., 
             # labeller = labeller(`paste(text, purpose)` = case_labels, version = study_labels)
             ) +
  geom_density(data=subset(stroop, as.factor(response)=="1" & rt > 300), color = NA,
               aes(rt, fill="0",  y= ..count..), alpha = .3) +
  geom_density(data=subset(stroop,  as.factor(response)=="0" & rt > 300), color = NA,
               aes(rt, fill="1",  y= -..count..), alpha = .3) +
  theme_minimal() + 
  geom_vline(data = stroopMeans, aes(xintercept = rt), linetype = 2, color = "black", size = 0.7) + 
  scale_y_continuous(name = '', limits = c(-3, 22), breaks = NULL) + 
  scale_x_continuous(name = 'Reaction time (seconds)', expand = c(0.05, 0.05),
                     limits = c(0, 3500), breaks = seq(0, 3000, 1000), 
                     labels = seq(0, 3, 1)) + 
  theme(legend.position = 'top', 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        text = element_text(size = 14),
        strip.text.y.right = element_blank(),
        legend.title = element_blank(),
        panel.spacing.x = unit(5, "lines"),
        plot.margin = margin(.1,0,.1,0, "cm")) + 
  scale_fill_manual(values=c("brown", "#4E79A7"), 
                    labels = c('Correct', 'Incorrect')) +
  scale_color_manual(values=c("brown", "#4E79A7"),
                     labels = c('Incorrect', 'Correct')) +
  guides(color = 'none')

ggsave('stroop1.jpg', width = 6, height = 7)


rulesMeans = rules %>%
  filter(rt > 500) %>%
  group_by(text, purpose) %>%
  summarise(rt = median(rt, na.rm = TRUE))


ggplot() + 
  geom_histogram(data=subset(rules, rt > 500 & as.factor(response) == '1'), fill = 'white', binwidth = 100,
                 aes(rt, color=as.factor(response), y= ..count../105), linewidth = .2) +
  geom_histogram(data=subset(rules, rt > 500 & as.factor(response) == '0'), fill = 'white', binwidth = 100,
                 aes(rt, color=as.factor(response), y= -..count../105), linewidth = .2) +
  facet_grid(purpose ~ text 
             # labeller = labeller(`paste(text, purpose)` = case_labels, version = study_labels)
  ) +
  geom_density(data=subset(rules, as.factor(response)=="1" & rt > 500), color = NA,
               aes(rt, fill="0",  y= ..count..), alpha = .3) +
  geom_density(data=subset(rules,  as.factor(response)=="0" & rt > 500), color = NA,
               aes(rt, fill="1",  y= -..count..), alpha = .3) +
  theme_minimal() + 
  geom_vline(data = rulesMeans, aes(xintercept = rt), linetype = 2, color = "black", size = 0.7) + 
  scale_y_continuous(name = '', limits = c(-3.4, 3.4), breaks = NULL) + 
  scale_x_continuous(name = 'Reaction time (seconds)', expand = c(0.05, 0.05),
                     limits = c(0, 8500), breaks = seq(0, 8000, 2000), 
                     labels = seq(0, 8, 2)) + 
  theme(legend.position = 'top', 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        text = element_text(size = 14),
        strip.text= element_blank(),
        legend.title = element_blank(),
        panel.spacing.x = unit(2, "lines"),
        plot.margin = margin(.1,0,.1,0, "cm")) + 
  scale_fill_manual(values=c("brown", "#4E79A7"), 
                    labels = c('Yes', 'No')) +
  scale_color_manual(values=c("brown", "#4E79A7"),
                     labels = c('Yes', 'No')) +
  guides(color = 'none')

ggsave('rules1.jpg', width = 10, height = 7)

rt_stroop2 = lmer(rt ~ congruent*response + (1 | subj_code), data = subset(stroop, rt > 300))
car::Anova(rt_stroop2)
emmeans(rt_stroop2, pairwise ~ congruent | response, type = 'response')

flanker = stfl %>%
  filter(flanker_stim_type %in% c('congruent', 'incongruent')) %>%
  mutate(response = case_when(correct == 'true' ~ 1, 
                              correct == 'false' ~ 0), 
         rt = as.numeric(rt)) %>%
  select(subj_code, congruent = flanker_stim_type, response, rt, trial_index)

ggplot(subset(flanker, as.numeric(rt) > 250), aes(x = trial_index, y = as.numeric(rt), color = congruent)) + 
  geom_hline(yintercept = 250) + 
  geom_jitter() + 
  geom_smooth()

acc_flanker = glmer(response ~ congruent + (1 | subj_code), 
                    data = subset(flanker, rt > 250), family = 'binomial')
car::Anova(acc_flanker)
emmeans(acc_flanker, pairwise ~ congruent, type = 'response')

rt_flanker = lmer(rt ~ congruent + (1 | subj_code), data = subset(flanker, rt > 250))
car::Anova(rt_flanker)
emmeans(rt_flanker, pairwise ~ congruent, type = 'response')

rt_flanker2 = lmer(rt ~ congruent * response + (1 | subj_code), data = subset(flanker, rt > 250))
car::Anova(rt_flanker2)
emmeans(rt_flanker2, pairwise ~ congruent | response, type = 'response')


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

ggplot(subset(rules, as.numeric(rt) > 500), 
       aes(x = trial_index, y = as.numeric(rt), color = congruent)) + 
  geom_hline(yintercept = 500) + 
  geom_jitter() + 
  geom_smooth()

ggplot(subset(rules, as.numeric(rt) > 500), 
       aes(x = trial_index, y = as.numeric(rt), color = paste(text, purpose))) + 
  geom_hline(yintercept = 500) + 
  geom_jitter(alpha = .1) + 
  geom_smooth()

acc_rules = glmer(response ~ text*purpose + (1 | subj_code) + (1 | rule), 
                  data = subset(rules, rt > 500), family = 'binomial')
car::Anova(acc_rules)
emmeans(acc_rules, pairwise ~ purpose | text, type = 'response')

rt_rules = lmer(rt ~ text*purpose  + (1 | subj_code) + (1 | rule), 
                data = subset(rules, rt > 500))
jtools::summ(rt_rules)
car::Anova(rt_rules)
emmeans(rt_rules, pairwise ~ purpose | text, type = 'response')

theory = stfl %>% filter(str_detect(response, "theory_question")) %>%
  mutate(theory = case_when(str_detect(response, "letter") ~ 't_theory',
                            str_detect(response, "spirit") ~ 'p_theory', 
                            str_detect(response, "bipolar") ~ str_sub(response, -2, -2)), 
         dv = if_else(str_detect(response, "bipolar"), 'bipolar', 'endorsement')) %>%
  select(subj_code, theory, dv) %>%   filter(complete.cases(.)) %>%
  pivot_wider(names_from = 'dv', values_from = 'theory') %>%
  mutate(bipolar = 1 + as.numeric(bipolar), 
         bi_ctr = bipolar - 4, 
         bi_sqr = bi_ctr^2)


rules = full_join(rules, theory, by = 'subj_code')

stroop_clean = stroop %>%
  mutate(stim_type = case_when(
      congruent == TRUE ~ 'Congruent',
      congruent == FALSE ~ 'Incongruent'
    )) %>%
  select(subj_code, stim_type, response, rt)

flanker_clean = flanker %>%
  mutate(
    stim_type = case_when(
      congruent == "congruent" ~ 'Congruent',
      congruent == "incongruent" ~ 'Incongruent'
    )) %>%
  select(subj_code, stim_type, response, rt)


mod1t = lmer(rt ~ text*purpose*endorsement + (1 | rule) + (1 | subj_code), 
            subset(rules, rt > 500))
car::Anova(mod1t)
emmeans(mod1t, pairwise ~ endorsement | text * purpose, type = 'response')

mod2t = glmer(response ~ text*purpose*bipolar + (1 | rule) + (1 | subj_code), 
             subset(rules, rt > 500), family = 'binomial')
car::Anova(mod2t)
emmeans(mod2t, pairwise ~ endorsement | text, type = 'response')
emmeans(mod2t, pairwise ~ endorsement | purpose, type = 'response')
emmeans(mod2t, pairwise ~ text * purpose, var = 'bipolar')

ggplot(subset(rules, rt > 500), aes(x = bipolar, y = rt, color = paste(text, purpose))) + 
  geom_smooth(method = 'loess')

write.csv(rules, 'rules_clean.csv', row.names = FALSE)
write.csv(flanker_clean, 'flanker_clean.csv', row.names = FALSE)
write.csv(stroop_clean, 'stroop_clean.csv', row.names = FALSE)

mod2b = lmer(rt ~ text*purpose*(bi_ctr + bi_sqr) + (1 | rule) + (1 | subj_code), 
              subset(rules, rt > 500))
car::Anova(mod2b)

emtrends(mod2b, pairwise ~ purpose | text, var = 'bi_ctr')
emtrends(mod2b, pairwise ~ purpose | text, var = 'bi_sqr')
interactions::sim_slopes(mod2b, pred = 'bi_sqr', modx = 'text', mod2 = 'purpose')

mod2b = lmer(rt ~ text*purpose*(bi_ctr + bi_sqr + endorsement) + (1 | rule) + (1 | subj_code), 
             subset(rules, rt > 500))
car::Anova(mod2b)

ggplot( subset(rules, rt > 500), aes(x = bipolar, y = rt, color = text == purpose)) + 
 # geom_jitter( alpha = .05) + 
  geom_smooth(method = 'lm', formula =  y ~ x + I(x^2)) + 
  stat_smooth(aes(group = bipolar), geom = 'point', size = 3) + 
  theme_classic() 

### Part 2. DDM correlations

st1 = read.csv('model1stroop/model1stroop_output.csv') %>%
  rename(stroop = mean)  %>%
  filter(str_detect(X, '_subj.'))
fl1 = read.csv('model1flanker/model1flanker_output.csv') %>%
  rename(flanker = mean) %>%
  filter(str_detect(X, '_subj.'))

r1 = read.csv('model2rulesFS/model2rulesFS_output.csv') %>%
  rename(rules = mean) %>%
  filter(str_detect(X, '_subj.')) %>%
  mutate(
    parameter = str_sub(X, 1, 1), 
    subj_code = str_sub(X, -12, -1),
    term = case_when(
      str_detect(X, 'Intercept') ~ 'intercept',
      str_detect(X, 'conflict') ~ 'conflict'
    )
  ) %>%
  select(rules, parameter, subj_code, term)

r1wide = r1 %>%
  pivot_wider(names_from = c('term', 'parameter'), values_from = 'rules', 
              id_cols = c('subj_code')) %>%
  rename(rules_a = `NA_a`, rules_t = `NA_t`, rules_z = `NA_z`) %>%
  unnest()

r1 = read.csv('model2rulesFS/model2rulesFS_output.csv') %>%
  rename(rules = mean) %>%
  filter(str_detect(X, '_subj.'))

stflr1 = full_join(r1, full_join(st1, fl1, by = 'X'), by = 'X') %>%
  mutate(parameter = str_sub(X, 1, 1), 
         subj_code = str_sub(X, -12, -1),
         term = case_when(
           str_detect(X, 'Intercept') ~ 'intercept',
           str_detect(X, 'conflict') ~ 'conflict'
         )) %>%
  select(stroop, flanker, rules, parameter, subj_code, term) 

ggplot(stflr1, aes(x = stroop, y = rules, color = parameter)) + 
  geom_jitter() + facet_wrap(~ parameter + term, scales = 'free')

cortable_parameters = stflr1 %>%
  pivot_wider(names_from = c('term', 'parameter'), values_from = c('stroop', 'flanker', 'rules'), 
              id_cols = 'subj_code') %>%
  select(-stroop_NA_z, -flanker_NA_z)

# cortable_parameters = full_join(r1wide, stfl1wide, by = 'subj_code') 

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
  filter(is.na(outlier)) %>%
  select(contains("_a"), contains("_t"),contains("_v")) %>%
  apaTables::apa.cor.table(., 'DDM_CorrTableExp.doc')

cor.test(~ rules_NA_a + stroop_NA_a, subset(cortable_parameters, is.na(outlier)))
cor.test(~ rules_NA_a + flanker_NA_a, subset(cortable_parameters, is.na(outlier)))

cortable_fig = cortable_parameters %>%
  select(outlier, subj_code) %>%
  right_join(., stflr1, by = 'subj_code') %>%
  filter(is.na(outlier), parameter != 'z') %>%
  gather(stroop:flanker, value = 'predictor', key = 'task') %>%
  mutate(panel = case_when(parameter == 'a' ~ '(a) Boundary\nSeparation', 
                           parameter == 't' ~ '(t) Non-Decision\nTime',
                           parameter == 'v' & term == 'conflict' ~ '(v) Drift Rate\n[conflict]',
                           parameter == 'v' & term == 'intercept' ~ '(v) Drift Rate \n[intercept]'), 
         panel_order = case_when(parameter == 'a' ~ 2, 
                                 parameter == 't' ~ 1,
                                 parameter == 'v' & term == 'conflict' ~ 4,
                                 parameter == 'v' & term == 'intercept' ~ 3))

cortable_fig = cortable_fig %>%
  group_by(task, panel) %>%
  mutate(
    rules_rank = (rank(rules) - 1) / (n() - 1) * (max(rules) - min(rules)) + min(rules),
    predictor_rank = (rank(predictor) - 1) / (n() - 1) * (max(predictor) - min(predictor)) + min(predictor)
  ) %>%
  ungroup()

write.csv(cortable_fig, 'study2_cor_ddm.csv', row.names = FALSE)


library(dplyr)
library(broom)

cors <- cortable_fig %>%
  group_by(task, panel) %>%
  summarise(
    cor_res = list(cor.test(rules, predictor, method = 'spearman')),
    .groups = "drop"
  ) %>%
  mutate(
    tidy = lapply(cor_res, broom::tidy)
  ) %>%
  unnest(tidy) %>%
  select(task, panel, estimate, p.value)

cors <- cors %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ ""
    ),
    label = sprintf("ρ = %.2f%s", estimate, stars)
  )

x_limits <- cortable_fig %>%
  group_by(task, panel) %>%
  summarise(min_x = min(rules), .groups = "drop")

labels_df <- cortable_fig %>%
  group_by(task, panel, panel_order) %>%
  summarise(
    x = mean(range(rules)),                 # midpoint of x-axis
    y = max(predictor),             # just below top of y-axis
    estimate = cor(rules, predictor, method = "spearman"),
    p.value = cor.test(rules, predictor, method = "spearman")$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    coef_text = sub("(-?)0\\.", "\\1.", sprintf("%.2f", estimate)),
    label = sprintf("ρ = %s%s", coef_text,
                    case_when(
                      p.value < 0.001 ~ "***",
                      p.value < 0.01  ~ "**",
                      p.value < 0.05  ~ "*",
                      TRUE            ~ ""
                    ))
  )


ggplot(cortable_fig, aes(x = rules, y = predictor, color = task, fill = task)) + 
  geom_jitter(alpha = .5, shape = 21, fill = 'white') + 
  geom_smooth(aes(x = rules_rank, y = predictor_rank), method = 'lm') + 
  geom_label(
    data = labels_df,
    aes(x = x, y = y, label = label),
    fill = '#cecece', alpha = .5, border.color = NA, 
    inherit.aes = FALSE,
    size = 4
  ) +
  facet_wrap(task ~ reorder(panel, panel_order), scales = 'free', nrow = 2) + 
  scale_color_manual(values = c('firebrick', 'navy')) + 
  scale_fill_manual(values = c('firebrick', 'navy')) + 
  scale_y_continuous(name = 'Basic Conflict Task (Stroop/Flanker)') + 
  scale_x_continuous(name = 'Rules Task') + 
  theme_classic() + 
  theme(strip.background = element_blank(), 
        legend.position = 'none', legend.title = element_blank(),
        legend.text = element_text(size = 11),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 12, face = 'bold'), 
        text = element_text(size = 12))

fig3a_ddm_cor = ggplot(subset(cortable_fig, task == 'flanker'), aes(x = rules, y = predictor, color = task, fill = task)) + 
  geom_jitter(alpha = .5, shape = 21, fill = 'white') + 
  geom_smooth(aes(x = rules_rank, y = predictor_rank), method = 'lm') + 
  geom_label(
    data = subset(labels_df, task == 'flanker'),
    aes(x = x, y = y, label = label),
    fill = '#cecece', alpha = .5, border.color = NA, 
    inherit.aes = FALSE,
    size = 4
  ) +
  facet_wrap(~ reorder(panel, panel_order), scales = 'free', nrow = 1) + 
  scale_color_manual(values = c('firebrick', 'navy')) + 
  scale_fill_manual(values = c('firebrick', 'navy')) + 
  scale_y_continuous(name = 'Flanker Task') + 
  scale_x_continuous(name = NULL) + 
  theme_classic() + 
  theme(strip.background = element_blank(), 
        legend.position = 'none', legend.title = element_blank(),
        legend.text = element_text(size = 11),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(size = 11, face = 'bold'), 
        text = element_text(size = 12))

fig3b_ddm_cor = ggplot(subset(cortable_fig, task == 'stroop'), aes(x = rules, y = predictor, color = task, fill = task)) + 
  geom_jitter(alpha = .5, shape = 21, fill = 'white') + 
  geom_smooth(aes(x = rules_rank, y = predictor_rank), method = 'lm') + 
  geom_label(
    data = subset(labels_df, task == 'stroop'),
    aes(x = x, y = y, label = label),
    fill = '#cecece', alpha = .5, border.color = NA, 
    inherit.aes = FALSE,
    size = 4
  ) +
  facet_wrap(~ reorder(panel, panel_order), scales = 'free', nrow = 1) + 
  scale_color_manual(values = c('navy', 'firebrick')) + 
  scale_fill_manual(values = c('navy', 'firebrick')) + 
  scale_y_continuous(name = 'Stroop Task') + 
  scale_x_continuous(name = 'Rules Task') + 
  theme_classic() + 
  theme(strip.background = element_blank(), 
        legend.position = 'none', legend.title = element_blank(),
        legend.text = element_text(size = 11),
        axis.ticks = element_blank(),
        strip.text = element_blank(), 
        text = element_text(size = 12))

ggpubr::ggarrange(fig3a_ddm_cor, fig3b_ddm_cor, nrow = 2, align = 'v')

ggsave('fig3cors.jpg', dpi = 300, width = 20, height = 12, units = 'cm')



rules %>%
  select(subj_code, endorsement) %>%
  distinct() %>%
  full_join(., cortable_parameters, by = 'subj_code') %>%
  filter(is.na(outlier), endorsement == 't_theory') %>%
  select(contains("_a"), contains("_t"),contains("_v")) %>%
  apaTables::apa.cor.table(., 'DDM_CorrTableExpT.doc')

rules %>%
  select(subj_code, endorsement) %>%
  distinct() %>%
  full_join(., cortable_parameters, by = 'subj_code') %>%
  filter(is.na(outlier), endorsement == 'p_theory') %>%
  select(contains("_a"), contains("_t"),contains("_v")) %>%
  apaTables::apa.cor.table(., 'DDM_CorrTableExpP.doc')

rules %>%
  select(subj_code, bipolar) %>%
  distinct() %>%
  full_join(., cortable_parameters, by = 'subj_code') %>%
  filter(is.na(outlier), bipolar < 3) %>%
  select(contains("_a"), contains("_t"),contains("_v")) %>%
  apaTables::apa.cor.table(., 'DDM_CorrTableExpT2.doc')

####

ddmSFR = rules %>%
  select(subj_code, bipolar, endorsement) %>%
  unique() %>% full_join(., cortable_parameters, by = 'subj_code') %>%
  mutate(bipolar_sq = (bipolar - 4)^2)

ggplot(subset(ddmSFR, is.na(outlier)), aes(x = bipolar, y = rules_conflict_v)) + 
  geom_jitter(color = 'firebrick', alpha = .3) + geom_smooth(color = 'firebrick', 
                                                 method = 'lm', formula =  y ~ x + I(x^2)) + 
  theme_classic()

glimpse(ddmSFR)

library(dplyr)

ciDDM = ddmSFR %>%
  filter(is.na(outlier)) %>%
  group_by(bipolar) %>%
  summarise(
    rules_conflict_mean = ci(rules_conflict_v, na.rm = TRUE)["mean"],
    rules_conflict_lower = ci(rules_conflict_v, na.rm = TRUE)["lower"],
    rules_conflict_upper = ci(rules_conflict_v, na.rm = TRUE)["upper"],
    rules_intercept_mean = ci(rules_intercept_v, na.rm = TRUE)["mean"],
    rules_intercept_lower = ci(rules_intercept_v, na.rm = TRUE)["lower"],
    rules_intercept_upper = ci(rules_intercept_v, na.rm = TRUE)["upper"]
  ) %>%
  gather(-bipolar, key = 'key', value = 'response') 

ggplot(data = subset(ciDDM, str_detect(key, 'mean') & bipolar < 7), 
                aes(x = bipolar, y = response, color = key)) + 
  geom_point(alpha = .3) + geom_line() + 
  geom_line(data = subset(ciDDM, str_detect(key, 'mean', negate = TRUE) & bipolar < 7), linetype = 2) + 
  theme_classic() +
  facet_wrap(~ str_detect(key, 'intercept'), scales = 'free_y')

  
ciV = ddmSFR %>%
  mutate(congruentR = rules_intercept_v, incongruentR = rules_intercept_v + rules_conflict_v,
         congruentF = flanker_intercept_v, incongruentF = flanker_intercept_v + flanker_conflict_v,
         congruentS = stroop_intercept_v, incongruentS = stroop_intercept_v + stroop_conflict_v) %>%
  select(bipolar, congruentR:incongruentS) %>%
  gather(-bipolar, key = 'predictor', value = 'response') %>%
  mutate(bipolar = if_else(bipolar == 7, 6, bipolar)) %>%
  group_by(bipolar, predictor) %>%
  reframe(lower = ci(response, na.rm = TRUE)['lower'],
          mean = ci(response, na.rm = TRUE)['mean'],
          upper = ci(response, na.rm = TRUE)['upper']) %>%
  mutate(domain = case_when(str_sub(predictor, -1, -1) == 'F' ~ 'Flanker',
                            str_sub(predictor, -1, -1) == 'R' ~ 'Rules',
                            str_sub(predictor, -1, -1) == 'S' ~ 'Stroop'), 
         predictor = str_sub(predictor, 1, -2))

area_ciV = ciV %>%
  select(-lower, -upper) %>%
  pivot_wider(names_from = predictor, values_from = mean)

ggplot(ciV, aes(x = bipolar, y = mean)) + 
  geom_linerange(aes(ymin = lower, ymax = upper, group = predictor, fill = predictor), 
                 alpha = .6, color = 'grey', linewidth = .5) + 
  geom_ribbon(data = area_ciV, aes(ymax = congruent, ymin = incongruent, y = 0), alpha = .7, fill = 'lightgrey') + 
  geom_hline(yintercept = 0, linetype = 3) + geom_line(aes(group = predictor, linetype = predictor)) + 
  geom_point(shape = 21, size = 2, fill = 'white') + jtools::theme_apa() +
  scale_x_continuous(name = 'Letter vs. Spirit Endorsement') + 
  scale_y_continuous(name = 'Drift Rate', expand = c(0, 0, .02, .02), breaks = seq(0, 6, 1)) + 
  facet_wrap(~ domain, scales = 'free') + 
  theme(legend.position = c(.85, .2)) 

ggplot(subset(ddmSFR, is.na(outlier)), aes(x = bipolar, y = rules_intercept_v)) + 
  geom_jitter(color = 'firebrick', alpha = .3) + geom_smooth(color = 'firebrick', 
                                                             method = 'lm', formula =  y ~ x + I(x^2)) + 
  theme_classic()

ggplot(subset(ddmSFR, is.na(outlier)), aes(x = bipolar, y = rules_NA_t)) + 
  geom_jitter() + geom_smooth()

cor.test(~ rules_intercept_v + bipolar, ddmSFR)  
cor.test(~ rules_intercept_v + bipolar, ddmSFR)  

mod1 = lm(rules_intercept_v ~ endorsement + bipolar + bipolar_sq, subset(ddmSFR, is.na(outlier)))
jtools::summ(mod1, digits = 3)

mod1 = lm(rules_conflict_v ~ endorsement + bipolar + bipolar_sq, subset(ddmSFR, is.na(outlier)))
jtools::summ(mod1, digits = 3)

mod1 = lm(rules_NA_a ~ endorsement + bipolar + bipolar_sq, subset(ddmSFR, is.na(outlier)))
jtools::summ(mod1, digits = 3)

mod1 = lm(rules_NA_t ~ endorsement + bipolar + bipolar_sq, subset(ddmSFR, is.na(outlier)))
jtools::summ(mod1, digits = 3)

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

View(bifactor_data)

bi_model <- 'flank =~ flanker1 + flanker2 + flanker3 + flanker4
                stroop =~ stroop1 + stroop2 + stroop3 + stroop4 
                rules =~ rules1 + rules2 + rules3 + rules4
                common =~ flanker1 + flanker2 + flanker3 + flanker4 + stroop1 + stroop2 + stroop3 + stroop4 + rules1 + rules2 + rules3 + rules4 
                '

bi_model_t <- 'flank =~ flanker1 + flanker2 + flanker3 + flanker4
                stroop =~ stroop2 + stroop3 + stroop4 
                rules =~ rules1 + rules2 + rules3 + rules4
                common =~ flanker1 + flanker2 + flanker3 + flanker4 + stroop1 + stroop2 + stroop3 + stroop4 + rules1 + rules2 + rules3 + rules4 
                '

### For non-decision time (remove stroop1)
bifactor_fit_t <- cfa(model = bi_model_t, data = subset(bifactor_data, parameter == 't'), 
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

# bifactor_data = left_join(bifactor_data, theory) 



bifactor_fit_t <- cfa(model = bi_model, data = subset(bifactor_data, parameter == 'a' & theory == 'p_theory'), 
                      std.lv = TRUE, orthogonal = TRUE)
summary(bifactor_fit_t, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

### Boundary separation
bifactor_fit_a <- cfa(model = bi_model, data = subset(bifactor_data, parameter == 'v'  & conflict == 'incongruent'), 
                      std.lv = TRUE, orthogonal = TRUE)
summary(bifactor_fit_a, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
