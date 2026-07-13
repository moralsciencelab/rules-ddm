library(tidyverse)
library(emmeans)
library(lavaan)
library(lmerTest)
library(broom)
library(grid)
library(patchwork)
library(magick)
library(grid)
library(ggplot2)
library(ggpubr)

theme_update(text = element_text(family = "Helvetica Neue"))


### Part 1. Behavioral results
stfl = read.csv('data/study2.csv')

stroop = stfl %>%
  filter(color != '', text != '') %>%
  mutate(congruent = color == text, 
         response = case_when(correct == 'true' ~ 1, 
                                 correct == 'false' ~ 0),
         rt = as.numeric(rt)) %>%
  select(subj_code, trial_index, response, congruent, rt)

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

rt_stroop = lmer(log(rt) ~ congruent + (1 | subj_code), 
                 data = subset(stroop, rt > 300))
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

rt_stroop2 = lmer(log(rt) ~ congruent*response + (1 | subj_code), data = subset(stroop, rt > 300))
car::Anova(rt_stroop2)
emmeans(rt_stroop2, pairwise ~ congruent | response, type = 'response')

flanker = stfl %>%
  filter(flanker_stim_type %in% c('congruent', 'incongruent')) %>%
  mutate(response = case_when(correct == 'true' ~ 1, 
                              correct == 'false' ~ 0), 
         rt = as.numeric(rt)) %>%
  select(subj_code, trial_index, congruent = flanker_stim_type, response, rt)

ggplot(subset(flanker, as.numeric(rt) > 250), aes(x = trial_index, y = as.numeric(rt), color = congruent)) + 
  geom_hline(yintercept = 250) + 
  geom_jitter() + 
  geom_smooth()

acc_flanker = glmer(response ~ congruent + (1 | subj_code), 
                    data = subset(flanker, rt > 250), family = 'binomial')
car::Anova(acc_flanker)
emmeans(acc_flanker, pairwise ~ congruent, type = 'response')

rt_flanker = lmer(log(rt) ~ congruent + (1 | subj_code), data = subset(flanker, rt > 250))
car::Anova(rt_flanker)
emmeans(rt_flanker, pairwise ~ congruent, type = 'response')

rt_flanker2 = lmer(log(rt) ~ congruent * response + (1 | subj_code), data = subset(flanker, rt > 250))
car::Anova(rt_flanker2)
emmeans(rt_flanker2, pairwise ~ congruent | response, type = 'response')

coding <- read.csv("data/codingsheet_120.csv")

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
  select(subj_code, trial_index, stimulus, text = text.y, purpose, congruent, response = resp, rt, rule)

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
  group_by(subj_code) %>%
  mutate(index = dense_rank(trial_index), 
         block = as.factor(ceiling(index/24))) %>%
  select(subj_code, block, stim_type, response, rt) 

flanker_clean = flanker %>%
  mutate(
    stim_type = case_when(
      congruent == "congruent" ~ 'Congruent',
      congruent == "incongruent" ~ 'Incongruent'
    )) %>%
  group_by(subj_code) %>%
  mutate(index = dense_rank(trial_index), 
         block = as.factor(ceiling(index/24))) %>%
  select(subj_code, block, stim_type, response, rt)


mod1t = lmer(rt ~ text*purpose*endorsement + (1 | rule) + (1 | subj_code), 
            subset(rules, rt > 500))
car::Anova(mod1t)
emmeans(mod1t, pairwise ~ endorsement | text * purpose, type = 'response')

mod2t = glmer(response ~ text*purpose*endorsement + (1 | rule) + (1 | subj_code), 
             subset(rules, rt > 500), family = 'binomial')
car::Anova(mod2t)
emmeans(mod2t, pairwise ~ endorsement | text, type = 'response')
emmeans(mod2t, pairwise ~ endorsement | purpose, type = 'response')

write.csv(rules, 'data/study2_rules.csv', row.names = FALSE)
write.csv(flanker_clean, 'data/study2_flanker.csv', row.names = FALSE)
write.csv(stroop_clean, 'data/study2_stroop.csv', row.names = FALSE)

### Part 2. DDM correlations

st1 = read.csv('results/ddm/model_stroop/model1stroop_output.csv') %>%
  rename(stroop = mean)  %>%
  filter(str_detect(X, '_subj.'))
fl1 = read.csv('results/ddm/model_flanker/model1flanker_output.csv') %>%
  rename(flanker = mean) %>%
  filter(str_detect(X, '_subj.'))
r1 = read.csv('results/ddm/model_rulesFS/model2rulesFS_output.csv') %>%
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

r1 = read.csv('results/ddm/model_rulesFS/model2rulesFS_output.csv') %>%
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
  apaTables::apa.cor.table(., 'results/Study2_correlation_table.doc')

cor.test(~ rules_NA_a + stroop_NA_a, subset(cortable_parameters, is.na(outlier)))
cor.test(~ rules_NA_a + flanker_NA_a, subset(cortable_parameters, is.na(outlier)))

cortable_fig = cortable_parameters %>%
  select(outlier, subj_code) %>%
  right_join(., stflr1, by = 'subj_code') %>%
  filter(is.na(outlier), parameter != 'z') %>%
  gather(stroop:flanker, value = 'predictor', key = 'task') %>%
  mutate(panel = case_when(parameter == 'a' ~ 'Threshold (a)',
                           parameter == 't' ~ 'Non-decision time (t)',
                           parameter == 'v' & term == 'conflict'  ~ 'Drift rate-incongruent (v)',
                           parameter == 'v' & term == 'intercept' ~ 'Drift rate-intercept (v)'), 
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

cors <- cortable_fig %>%
  group_by(panel, task) %>%
  summarise(
    cor_res = list(cor.test(rules, predictor, method = 'spearman')),
    .groups = "drop"
  ) %>%
  mutate(
    tidy = lapply(cor_res, broom::tidy)
  ) %>%
  unnest(tidy) %>%
  select(panel, task, estimate, p.value)

cors <- cors %>%
  mutate(
    stars = case_when(
      p.value < .001 ~ "***",
      p.value < .01  ~ "**",
      p.value < .05  ~ "*",
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
    x = min(rules),                 # midpoint of x-axis
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

fig3a_ddm_cor = ggplot(subset(cortable_fig, task == 'flanker'), aes(x = rules, y = predictor, color = panel, fill = panel)) + 
  geom_jitter(alpha = .5, shape = 21, fill = 'white') + 
  geom_smooth(aes(x = rules_rank, y = predictor_rank), method = 'lm') + 
  geom_label(
    data = subset(labels_df, task == 'flanker'),
    aes(x = x, y = y, label = label),
    fill = '#cecece', alpha = .5, border.color = NA, 
    inherit.aes = FALSE, hjust = 0, vjust = 1,
    size = 4
  ) +
  facet_wrap(~ reorder(panel, panel_order), scales = 'free', nrow = 1, space = 'fixed',
             #labeller = label_parsed
             ) + 
  scale_color_manual(values = c(scales::muted(v_color), v_color, t_color,  a_color)) + 
  scale_fill_manual(values =  c(scales::muted(v_color), v_color, t_color,  a_color)) + 
  scale_y_continuous(name = 'Flanker Task') + 
  scale_x_continuous(name = NULL) + 
  theme_classic() + 
  theme(strip.background = element_blank(), 
        legend.position = 'none', legend.title = element_blank(),
        legend.text = element_text(size = 11),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(size = 11, face = 'bold', hjust = 0), 
        text = element_text(size = 12))

fig3b_ddm_cor = ggplot(subset(cortable_fig, task == 'stroop'), aes(x = rules, y = predictor, color = panel, fill = panel)) + 
  geom_jitter(alpha = .5, shape = 21, fill = 'white') + 
  geom_smooth(aes(x = rules_rank, y = predictor_rank), method = 'lm') + 
  geom_label(
    data = subset(labels_df, task == 'stroop'),
    aes(x = x, y = y, label = label), 
    fill = '#cecece', alpha = .5, border.color = NA, 
    inherit.aes = FALSE, hjust = 0, vjust = 1,
    size = 4
  ) +
  facet_wrap(~ reorder(panel, panel_order), scales = 'free', nrow = 1, space = "fixed") + 
  scale_color_manual(values = c(scales::muted(v_color), v_color, t_color,  a_color)) + 
  scale_fill_manual(values =  c(scales::muted(v_color), v_color, t_color,  a_color)) + 
  scale_y_continuous(name = 'Stroop Task') + 
  scale_x_continuous(name = 'Rules Task') + 
  theme_classic() + 
  theme(strip.background = element_blank(), 
        legend.position = 'none', legend.title = element_blank(),
        legend.text = element_text(size = 11),
        axis.ticks = element_blank(),
        strip.text = element_blank(), 
        text = element_text(size = 12))


# read first page of PDF
img <- image_read_pdf("figures/bifactor.pdf", density = 300)

# convert to raster
img_raster <- as.raster(img)

# grob
icons_bf_grob <- rasterGrob(img_raster, interpolate = TRUE)

# wrap as ggplot
icons_plot <- ggplot() +
  annotation_custom(
    icons_bf_grob,
    xmin = -Inf, xmax = Inf,
    ymin = -Inf, ymax = Inf
  ) +
  theme_void() +
  theme(
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 0,
      l = 0
    )
  )

(fig3a_ddm_cor / fig3b_ddm_cor) /
  icons_plot +
  plot_layout(heights = c(1, 1, 2.5)) +
  plot_annotation(tag_levels = list(c("A", "", "B"))) + 
   theme(plot.margin = margin(0, 0, 0, 0))


ggsave('figures/Figure3.png', width = 25, height = 20, dpi = 300, units = "cm")



### Part 3. Bifactor analysis

st2 = read.csv("results/ddm/model2stroop/model2stroop_output.csv") %>%
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

fl2 = read.csv('results/ddm/model2flanker/model2flanker_output.csv') %>%
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

r2 = read.csv('results/ddm/model4rulesFS/model4rulesFS_output.csv') %>%
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
  filter(parameter != 'z') %>%
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

bi_model_t <- 'flank =~ flanker1 + flanker2 + flanker4
                stroop =~ stroop1 + stroop2 + stroop3 + stroop4 
                rules =~ rules1 + rules2 + rules3 + rules4
                common =~ flanker1 + flanker2 + flanker3 + flanker4 + stroop1 + stroop2 + stroop3 + stroop4 + rules1 + rules2 + rules3 + rules4 
                '

bi_model_int <- 'flank =~  flanker2 + flanker3 + flanker4
                stroop =~ stroop1 + stroop2 + stroop3 + stroop4 
                rules =~ rules1 + rules2 + rules3 + rules4
                common =~ flanker2 + flanker3 + flanker4 + stroop1 + stroop2 + stroop3 + stroop4 + rules1 + rules2 + rules3 + rules4 
                '
bifactor_data_interf = bifactor_data %>%
  filter(parameter == 'v') %>%
  pivot_longer(cols = c(stroop1:stroop4, flanker1:flanker4, rules1:rules4), 
               names_to = 'task_block', values_to = 'value') %>%
  pivot_wider(names_from = "conflict", 
              values_from = 'value', names_prefix = 'conflict_')  %>%
  mutate(interference = conflict_congruent - conflict_incongruent) %>%
  pivot_wider(names_from = 'task_block', values_from = 'interference',
              id_cols = 'subj_code')


### For non-decision time (remove stroop1)
bifactor_fit_t <- cfa(model = bi_model, data = subset(bifactor_data, parameter == 't'), 
                      std.lv = TRUE, orthogonal = TRUE,
                      estimator = "MLR",       # more robust
                      optim.method = "BFGS",   # try different optimizer
                      control = list(iter.max = 1000))
summary(bifactor_fit_t, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

### Boundary separation
bifactor_fit_a <- cfa(model = bi_model, data = subset(bifactor_data, parameter == 'a'), 
                      std.lv = TRUE, orthogonal = TRUE,
                      estimator = "MLR",       # more robust
                      optim.method = "BFGS",   # try different optimizer
                      control = list(iter.max = 1000))
summary(bifactor_fit_a, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

## congruent drift rate
bifactor_fit_congruent <- cfa(model = bi_model, 
                              data = subset(bifactor_data, parameter == 'v' & conflict == 'congruent'), 
                              std.lv = TRUE, orthogonal = TRUE,
                              estimator = "MLR",       # more robust
                              optim.method = "BFGS",   # try different optimizer
                              control = list(iter.max = 1000))
summary(bifactor_fit_congruent, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#bifactor_fit_incongruent <- cfa(model = bi_model, 
  #                              data = bifactor_data_interf, 
 #                               std.lv = TRUE, orthogonal = TRUE)
#summary(bifactor_fit_incongruent, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

bifactor_fit_incongruent <- cfa(model = bi_model,
                                data = bifactor_data_interf,
                                std.lv = TRUE, orthogonal = TRUE,
                                estimator = "MLR",       # more robust
                                optim.method = "BFGS",   # try different optimizer
                                control = list(iter.max = 1000))

summary(bifactor_fit_incongruent, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
# bifactor_data = left_join(bifactor_data, theory) 

orthogonal_model <- '
  flank  =~ flanker1 + flanker2 + flanker3 + flanker4
  stroop =~ stroop1 + stroop2 + stroop3 + stroop4
  rules  =~ rules1 + rules2 + rules3 + rules4
'
fit_interf_orthogonal <- cfa(orthogonal_model,
                             data = bifactor_data_interf,
                             std.lv = TRUE, orthogonal = TRUE, 
                             estimator = "MLR",       # more robust
                             optim.method = "BFGS",   # try different optimizer
                             control = list(iter.max = 1000))
summary(fit_interf_orthogonal, fit.measures = TRUE, standardized = TRUE)

fit_interf_oblique <- cfa(orthogonal_model,
                          data = bifactor_data_interf,
                          std.lv = TRUE)  # allows correlations

lavTestLRT(fit_interf_orthogonal, fit_interf_oblique)

lavTestLRT(fit_interf_orthogonal, bifactor_fit_incongruent)
