library(tidyverse)
library(lmerTest)
library(emmeans)

dt = read.csv('rules_allstudies.csv')
glimpse(dt)

# to store the results
results_path = paste(getwd(), '/results', sep = "")
dir.create(file.path(results_path))

## Violation, compliance and punishment ----
### Summary stats ----
dt %>%
  filter(study %in% c('violate', 'comply', 'punishment')) %>%
  group_by(study, text, purpose, condition) %>%
  summarise(rt = median(rt, na.rm = TRUE), 
            resp = mean(response, na.rm = TRUE)) %>%
  pivot_wider(names_from = 'study', values_from = c('rt', 'resp'), id_cols = 'condition') %>%
  write.csv('results/summary_stats1.csv', row.names = FALSE)

### Responses ----
resp_v = glmer(response ~ text + purpose + (1 | rule) + (1 | subj_code), 
              subset(dt, study == 'violate'), family = 'binomial')

car::Anova(resp_v)
jtools::summ(resp_v, exp = TRUE, digits = 3, confint = TRUE)

resp_c = glmer(response ~ text + purpose + (1 | rule) + (1 | subj_code), 
               subset(dt, study == 'comply'), family = 'binomial')

car::Anova(resp_c)
jtools::summ(resp_c, exp = TRUE, digits = 3, confint = TRUE)

resp_p = glmer(response ~ text + purpose + (1 | rule) + (1 | subj_code), 
               subset(dt, study == 'punishment'), family = 'binomial')

car::Anova(resp_p)
jtools::summ(resp_p, exp = TRUE, digits = 3, confint = TRUE) 

bind_rows(
  jtools::summ(resp_v, exp = TRUE, digits = 3, confint = TRUE)$coeftable %>% 
    data.frame() %>% mutate(iv = row.names(.), dv = 'violation'),
  jtools::summ(resp_c, exp = TRUE, digits = 3, confint = TRUE)$coeftable %>% 
    data.frame() %>% mutate(iv = row.names(.), dv = 'compliance'),
  jtools::summ(resp_p, exp = TRUE, digits = 3, confint = TRUE)$coeftable %>% 
    data.frame() %>% mutate(iv = row.names(.), dv = 'punishment')
) %>% rename(coefficient = exp.Est.., lowerCI = X2.5., upperCI = X97.5.) %>%
  mutate(across(coefficient:p, ~round(., 3))) %>%
  relocate(dv, iv) %>%
  write.csv('results/responses.csv', row.names = FALSE)

### Reaction Times ----
rt_v = lmer(rt ~ text * purpose + (1 | rule) + (1 | subj_code), 
               subset(dt, study == 'violate'))

car::Anova(rt_v)
jtools::summ(rt_v, digits = 3, confint = TRUE)

rt_c = lmer(rt ~ text * purpose + (1 | rule) + (1 | subj_code), 
               subset(dt, study == 'comply'))

car::Anova(rt_c)
jtools::summ(rt_c, digits = 3, confint = TRUE)

rt_p = lmer(rt ~ text * purpose + (1 | rule) + (1 | subj_code), 
               subset(dt, study == 'punishment'))

car::Anova(rt_p)
jtools::summ(rt_p, digits = 3, confint = TRUE) 

bind_rows(
  jtools::summ(rt_v, digits = 3, confint = TRUE)$coeftable %>% 
    data.frame() %>% mutate(iv = row.names(.), dv = 'violation'),
  jtools::summ(rt_c, digits = 3, confint = TRUE)$coeftable %>% 
    data.frame() %>% mutate(iv = row.names(.), dv = 'compliance'),
  jtools::summ(rt_p, digits = 3, confint = TRUE)$coeftable %>% 
    data.frame() %>% mutate(iv = row.names(.), dv = 'punishment')
) %>% rename(coefficient = Est., lowerCI = X2.5., upperCI = X97.5.) %>%
  mutate(across(coefficient:p, ~round(., 3))) %>%
  relocate(dv, iv) %>%
  write.csv('results/rts.csv', row.names = FALSE)

# Effect of response on rt

rt_v2 = lmer(rt ~ text * purpose * response + (1 | rule) + (1 | subj_code), 
            subset(dt, study == 'violate'))
car::Anova(rt_v2)
emmeans(rt_v2, pairwise ~ response | text * purpose)

rt_c2 = lmer(rt ~ text * purpose * response + (1 | rule) + (1 | subj_code), 
            subset(dt, study == 'comply'))
car::Anova(rt_c2)
emmeans(rt_c2, pairwise ~ response | text * purpose)

rt_p2 = lmer(rt ~ text * purpose * response + (1 | rule) + (1 | subj_code), 
            subset(dt, study == 'punishment'))
car::Anova(rt_p2)
emmeans(rt_p2, pairwise ~ response | text * purpose)

bind_rows(
  emmeans(rt_v2, pairwise ~ response | text * purpose)$contrasts %>% 
    data.frame() %>% mutate(dv = 'violation'),
  emmeans(rt_c2, pairwise ~ response | text * purpose)$contrasts %>% 
    data.frame() %>% mutate(dv = 'compliance'),
  emmeans(rt_p2, pairwise ~ response | text * purpose)$contrasts %>% 
    data.frame() %>% mutate( dv = 'punishment')
) %>% rename(coefficient = estimate) %>%
  mutate(across(coefficient:p.value, ~round(., 3))) %>%
  relocate(dv) %>%
  write.csv('results/rts2.csv', row.names = FALSE)

rt_v3 = lmer(rt ~ text * purpose + response + (1 | rule) + (1 | subj_code), 
             subset(dt, study == 'violate'))
jtools::summ(rt_v3, digits = 3, confint = TRUE)

rt_c3 = lmer(rt ~ text * purpose + response + (1 | rule) + (1 | subj_code), 
             subset(dt, study == 'comply'))
jtools::summ(rt_c3, digits = 3, confint = TRUE)

rt_p3 = lmer(rt ~ text * purpose + response + (1 | rule) + (1 | subj_code), 
             subset(dt, study == 'punishment'))
jtools::summ(rt_p3, digits = 3, confint = TRUE)

bind_rows(
  jtools::summ(rt_v3, digits = 3, confint = TRUE)$coeftable %>% 
    data.frame() %>% mutate(iv = row.names(.), dv = 'violation'),
  jtools::summ(rt_c3, digits = 3, confint = TRUE)$coeftable %>% 
    data.frame() %>% mutate(iv = row.names(.), dv = 'compliance'),
  jtools::summ(rt_p3, digits = 3, confint = TRUE)$coeftable %>% 
    data.frame() %>% mutate(iv = row.names(.), dv = 'punishment')
) %>% rename(coefficient = Est., lowerCI = X2.5., upperCI = X97.5.) %>%
  mutate(across(coefficient:p, ~round(., 3))) %>%
  relocate(dv) %>% filter(iv == 'response') %>%
  write.csv('results/rts3.csv', row.names = FALSE)


## Figure 1 ----

case_labels <- c(
  '0 0' = "Compliance",
  '0 1' = "Literal\ncompliance",
  '1 0' = "Literal\nviolation",
  '1 1' = "Violation"
)

study_labels <- c(
  'violate' = "Experiment 1a: Violation",
  'comply' =  "Experiment 1b: Compliance",
  'punishment' =  "Experiment 1c: Punishment"
)

dt$study = factor(dt$study)
dt$study = relevel(dt$study, ref = "violate")

fig1mdn = dt %>%
  filter( study %in% c('violate', 'comply', 'punishment'), rt > 400) %>%
  group_by(study, as.factor(response), text, purpose) %>%
  summarise(rt = median(rt, na.rm = TRUE))

fig1iqr = dt %>%
  filter( study %in% c('violate', 'comply', 'punishment'), rt > 400) %>%
  group_by(study, as.factor(response), text, purpose) %>%
  summarise(q1 = fivenum(rt, na.rm = TRUE)[2],
            q3 = fivenum(rt, na.rm = TRUE)[4]) %>%
  gather(q1:q3, key = 'quartile', value = 'rt')

color_no = "#be983f"
color_yes = "#3f8d97"

ggplot() + 
  geom_histogram(data=subset(dt, study %in% c('violate', 'comply', 'punishment') &
                               as.factor(response)=="1" & rt > 400), fill = 'white', binwidth = 200,
                 aes(rt, color=as.factor(response), y= ..count../220), linewidth = .2) +
  geom_histogram(data=subset(dt, study %in% c('violate', 'comply', 'punishment') &
                               as.factor(response)=="0" & rt > 400), fill = 'white', binwidth = 200,
                 aes(rt, color=as.factor(response), y= -..count../220), linewidth = .2) +
  facet_grid(reorder(paste(text, purpose), -((text * 1.1) + purpose)) ~ study, 
             labeller = labeller(`paste(text, purpose)` = case_labels, 
                                 study = study_labels)) +
  geom_density(data=subset(dt, study %in% c('violate', 'comply', 'punishment') & as.factor(response)=="1" & rt > 400), color = NA,
               aes(rt, fill="0",  y= ..count..), alpha = .3) +
  geom_density(data=subset(dt, study %in% c('violate', 'comply', 'punishment') & as.factor(response)=="0" & rt > 400), color = NA,
               aes(rt, fill="1",  y= -..count..), alpha = .3) +
  theme_minimal() + 
  geom_segment(data = subset(fig1mdn, `as.factor(response)` == '0'), aes(x = rt, xend = rt), 
               y = -Inf, yend = 0,  linetype = 1, color = color_no, size = 0.7) + 
  geom_segment(data = subset(fig1mdn, `as.factor(response)` == '1'), aes(x = rt, xend = rt), 
               y = 0, yend = Inf,  linetype = 1, color = color_yes, size = 0.7) +
  geom_segment(data = subset(fig1iqr, `as.factor(response)` == '0'), aes(x = rt, xend = rt), 
               y = -Inf, yend = 0,  linetype = 3, color = color_no, size = 0.4, alpha = .7) + 
  geom_segment(data = subset(fig1iqr, `as.factor(response)` == '1'), aes(x = rt, xend = rt), 
               y = 0, yend = Inf,  linetype = 3, color = color_yes, size = 0.4, alpha = .7) +
  scale_y_continuous(name = '', limits = c(-1.4, 1.4), breaks = NULL) + 
  scale_x_continuous(name = 'Reaction time (s)', expand = c(0, 0),
                     limits = c(0, 8000), breaks = seq(0, 8000, 2000), 
                     labels = c(' 0', '2', '4', '6', '8 ')) + 
  theme(legend.position = 'bottom', 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        text = element_text(size = 14, family = 'Helvetica Neue'),
        axis.text = element_text(size = 10, family = 'Helvetica Neue'),
        axis.title = element_text(size = 12, family = 'Helvetica Neue'),
        legend.text = element_text(size = 11, family = 'Helvetica Neue'),
        strip.text.y.right = element_blank(),
        legend.title = element_blank(),
        panel.spacing.x = unit(.8, "lines"),
        plot.margin = margin(.1,.3,.1,.3, "cm"),
        panel.border = element_rect(colour = "black", size=0.5, fill = NA)) + 
  scale_fill_manual(values=c(color_yes, color_no), 
                    labels = c('Yes', 'No')) +
  scale_color_manual(values=c(color_yes, color_no), 
                     labels = c('Yes', 'No')) +
  guides(color = 'none')


fig1a_rt = ggplot() + 
  geom_histogram(data=subset(dt, study %in% c('violate') &
                               as.factor(response)=="1" & rt > 400), fill = 'white', binwidth = 200,
                 aes(rt, color=as.factor(response), y= ..count../220), linewidth = .2) +
  geom_histogram(data=subset(dt, study %in% c('violate') &
                               as.factor(response)=="0" & rt > 400), fill = 'white', binwidth = 200,
                 aes(rt, color=as.factor(response), y= -..count../220), linewidth = .2) +
  facet_grid(reorder(paste(text, purpose), -((text * 1.1) + purpose)) ~ study, 
             labeller = labeller(`paste(text, purpose)` = case_labels, 
                                 study = study_labels)) +
  geom_density(data=subset(dt, study %in% c('violate') & as.factor(response)=="1" & rt > 400), color = NA,
               aes(rt, fill="0",  y= ..count..), alpha = .3) +
  geom_density(data=subset(dt, study %in% c('violate') & as.factor(response)=="0" & rt > 400), color = NA,
               aes(rt, fill="1",  y= -..count..), alpha = .3) +
  theme_minimal() + 
  geom_segment(data = subset(fig1mdn, `as.factor(response)` == '0' & study == 'violate'), aes(x = rt, xend = rt), 
               y = -Inf, yend = 0,  linetype = 1, color = color_no, size = 0.7) + 
  geom_segment(data = subset(fig1mdn, `as.factor(response)` == '1' & study == 'violate'), aes(x = rt, xend = rt), 
               y = 0, yend = Inf,  linetype = 1, color = color_yes, size = 0.7) +
  geom_segment(data = subset(fig1iqr, `as.factor(response)` == '0' & study == 'violate'), aes(x = rt, xend = rt), 
               y = -Inf, yend = 0,  linetype = 3, color = color_no, size = 0.4, alpha = .7) + 
  geom_segment(data = subset(fig1iqr, `as.factor(response)` == '1' & study == 'violate'), aes(x = rt, xend = rt), 
               y = 0, yend = Inf,  linetype = 3, color = color_yes, size = 0.4, alpha = .7) +
  scale_y_continuous(name = '', limits = c(-1.4, 1.4), breaks = NULL) + 
  scale_x_continuous(name = '', expand = c(0, 0),
                     limits = c(0, 8000), breaks = seq(0, 8000, 2000), 
                     labels = c(' 0', '2', '4', '6', '8 ')) + 
  theme(legend.position = 'none', 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        strip.text = element_blank(),
        strip.text.y.right = element_blank(),
        legend.title = element_blank(),
        panel.spacing.x = unit(.8, "lines"),
        plot.margin = margin(.7, .2, .7, .2, "cm"),
        panel.border = element_rect(colour = "black", size=0.5, fill = NA)) + 
  scale_fill_manual(values=c(color_yes, color_no), 
                    labels = c('Yes', 'No')) +
  scale_color_manual(values=c(color_yes, color_no), 
                     labels = c('Yes', 'No')) +
  guides(color = 'none')

### appendix 11

lrn1_ddm = read.csv('~/Documents/ddm_rules/model_learn/model_learn_output.csv') %>%
  rename(term = X) %>%
  filter(!str_detect(term, "_subj."), !str_detect(term, "std")) %>%
  select(term, mean, `X2.5q`, `X97.5q`) 



read.csv('~/Documents/ddm_rules/model_learn2_control/model_learn2_control_output.csv') %>%
  rename(term = X) %>%
  filter(!str_detect(term, "_subj."), !str_detect(term, "std")) %>%
  select(term, mean, `X2.5q`, `X97.5q`) 

read.csv('~/Documents/ddm_rules/model_learn2_stroop/model_learn2_stroop_output.csv') %>%
  rename(term = X) %>%
  filter(!str_detect(term, "_subj."), !str_detect(term, "std")) %>%
  select(term, mean, `X2.5q`, `X97.5q`) 

read.csv('~/Documents/ddm_rules/model_learn2_rules/model_learn2_rules_output.csv') %>%
  rename(term = X) %>%
  filter(!str_detect(term, "_subj."), !str_detect(term, "std")) %>%
  select(term, mean, `X2.5q`, `X97.5q`) 
