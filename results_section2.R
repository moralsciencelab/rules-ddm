library(tidyverse)
library(lmerTest)
library(emmeans)

dt = read.csv('rules_allstudies.csv')
glimpse(dt)

dt %>%
  distinct(subj_code, study) %>%
  group_by(study) %>%
  tally()


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

resp_v_int = glmer(response ~ text * purpose + (1 | rule) + (1 | subj_code), 
                   subset(dt, study == 'violate'), family = 'binomial')
jtools::summ(resp_v_int, exp = TRUE, digits = 3, confint = TRUE)

resp_v_slope = glmer(response ~ text + purpose + (1 | rule) + (1 + text + purpose| subj_code), 
                     subset(dt, study == 'violate'), family = 'binomial')
jtools::summ(resp_v_slope, exp = TRUE, digits = 3, confint = TRUE)

resp_c = glmer(response ~ text * purpose + (1 | rule) + (1 | subj_code), 
               subset(dt, study == 'comply'), family = 'binomial')
resp_c = glmer(response ~ text + purpose + (1 | rule) + (1 | subj_code), 
               subset(dt, study == 'comply'), family = 'binomial')

car::Anova(resp_c)
jtools::summ(resp_c, exp = TRUE, digits = 3, confint = TRUE)

resp_p = glmer(response ~ text * purpose + (1 | rule) + (1 | subj_code), 
               subset(dt, study == 'punishment'), family = 'binomial')
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


results = data.frame()

for (j in c('violate', 'comply', 'punishment')) {
  temp_dt = subset(dt, study == j)
  for (i in levels(as.factor(temp_dt$subj_code))) {
    temp_dt2 = subset(dt, subj_code == i)
    
    temp_model <- try(
      logistf::logistf(response ~ text + purpose, 
                       data = temp_dt2, 
                       family = 'binomial'),
      silent = TRUE
    )
    
    # Skip participant if model failed
    if (inherits(temp_model, "try-error")) {
      message("Skipping participant due to model error.")
      next
    }
    
    new_row = c(j, i, temp_model$prob)
    
    results = rbind(results, new_row)
    cat(".")
  }
}

names(results) = c('study', 'subject_id', 'intercept', 'text', 'purpose')
glimpse(results)

results %>%
  group_by(study, text = as.numeric(text) < .05, purpose = as.numeric(purpose) < .05) %>%
  summarise(prop = n()/120) %>%
  ggplot(aes(x = paste(text, purpose), y = prop)) + 
  geom_col(aes(fill = paste(text, purpose))) + 
  facet_wrap(. ~ study)

results %>%
  gather(intercept:purpose, key = 'term', value = 'pval') %>%
  group_by(term, study) %>%
  summarise(mean(as.numeric(pval) < .05, na.rm = TRUE), 
            n())

### Reaction Times ----
rt_v = lmer(log(rt) ~ text * purpose + (1 | rule) + (1 | subj_code), 
            subset(dt, study == 'violate'))

car::Anova(rt_v, type = "III")
jtools::summ(rt_v, digits = 3, confint = TRUE)

rt_c = lmer(log(rt) ~ text * purpose + (1 | rule) + (1 | subj_code), 
            subset(dt, study == 'comply'))

car::Anova(rt_c, type = "III")
jtools::summ(rt_c, digits = 3, confint = TRUE)

rt_p = lmer(log(rt) ~ text * purpose + (1 | rule) + (1 | subj_code), 
            subset(dt, study == 'punishment'))

car::Anova(rt_p, type = "III")
jtools::summ(rt_p, digits = 3, confint = TRUE) 

bind_rows(
  jtools::summ(rt_v, digits = 3, confint = TRUE)$coeftable %>% 
    data.frame() %>% mutate(iv = row.names(.), dv = 'violation'),
  jtools::summ(rt_c, digits = 3, confint = TRUE)$coeftable %>% 
    data.frame() %>% mutate(iv = row.names(.), dv = 'compliance'),
  jtools::summ(rt_p, digits = 3, confint = TRUE)$coeftable %>% 
    data.frame() %>% mutate(iv = row.names(.), dv = 'punishment')
) %>% rename(coefficient = Est., lowerCI = X2.5., upperCI = X97.5.) %>%
  mutate(across(coefficient:p, ~round(., 2))) %>%
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

###

ddm_results = bind_rows(
  read.csv("~/Documents/ddm_rules/model_violate/model_violate_output.csv") %>%
    mutate(study = "study1a"),
  read.csv("~/Documents/ddm_rules/model_comply/model_comply_output.csv") %>%
    mutate(study = "study1b"),
  read.csv("~/Documents/ddm_rules/model_punishment/model_punishment_output.csv") %>%
    mutate(study = "study1c")) %>%
  rename(parameter = X) %>% 
  filter(!str_detect(parameter, '_std|subj')) %>%
  mutate(condition = case_when(
    str_detect(parameter, "0.0.0.0") ~ "(compliance)",
    str_detect(parameter, "0.0.1.0") ~ "(literal compliance)",
    str_detect(parameter, "1.0.0.0") ~ "(literal violation)",
    str_detect(parameter, "1.0.1.0") ~ "(violation)"
  ), 
  term = if_else(is.na(condition), parameter,
                 paste(str_sub(parameter, 1, 1), condition, sep = " "))) %>%
  select(study, term, mean, X2.5q, X97.5q) %>%
  mutate(across(c(mean, `X2.5q`, `X97.5q`), ~round(.x, 2)))

write.csv(ddm_results, "results/ddm_estimates.csv", row.names = FALSE)

#### Rhat etc

library(posterior)
library(dplyr)

traces <- read_csv("~/Documents/ddm_rules/model_violate/model_violate_within_traces.csv")
# Convert to draws_df format
draws <- as_draws_df(traces)

# Get R-hat, ESS bulk, ESS tail for all parameters
summary_1a <- summarise_draws(draws, 
                              mean, 
                              sd,
                              ~quantile(.x, probs = c(0.025, 0.975)),
                              rhat,
                              ess_bulk,
                              ess_tail)  %>% 
  filter(!str_detect(variable, '_std|subj')) %>%
  mutate(condition = case_when(
    str_detect(variable, "0.0.0.0") ~ "(compliance)",
    str_detect(variable, "0.0.1.0") ~ "(literal compliance)",
    str_detect(variable, "1.0.0.0") ~ "(literal violation)",
    str_detect(variable, "1.0.1.0") ~ "(violation)"
  ), 
  term = if_else(is.na(condition), variable,
                 paste(str_sub(variable, 1, 1), condition, sep = " "))) %>%
  select(variable, rhat, ess_bulk, ess_tail) %>%
  mutate(across(c(rhat, ess_bulk, ess_tail), ~round(.x, 2))) %>%
  filter(variable != "...1")

print(summary_1a)

traces <- read_csv("~/Documents/ddm_rules/model_comply/model_comply_within_traces.csv")
# Convert to draws_df format
draws <- as_draws_df(traces)

# Get R-hat, ESS bulk, ESS tail for all parameters
summary_1b <- summarise_draws(draws, 
                              mean, 
                              sd,
                              ~quantile(.x, probs = c(0.025, 0.975)),
                              rhat,
                              ess_bulk,
                              ess_tail)  %>% 
  filter(!str_detect(variable, '_std|subj')) %>%
  mutate(condition = case_when(
    str_detect(variable, "0.0.0.0") ~ "(compliance)",
    str_detect(variable, "0.0.1.0") ~ "(literal compliance)",
    str_detect(variable, "1.0.0.0") ~ "(literal violation)",
    str_detect(variable, "1.0.1.0") ~ "(violation)"
  ), 
  term = if_else(is.na(condition), variable,
                 paste(str_sub(variable, 1, 1), condition, sep = " "))) %>%
  select(variable, rhat, ess_bulk, ess_tail) %>%
  mutate(across(c(rhat, ess_bulk, ess_tail), ~round(.x, 2))) %>%
  filter(variable != "...1")

print(summary_1b)

traces <- read_csv("~/Documents/ddm_rules/model_punishment/model_punishment_within_traces.csv")
# Convert to draws_df format
draws <- as_draws_df(traces)

# Get R-hat, ESS bulk, ESS tail for all parameters
summary_1c <- summarise_draws(draws, 
                              mean, 
                              sd,
                              ~quantile(.x, probs = c(0.025, 0.975)),
                              rhat,
                              ess_bulk,
                              ess_tail)  %>% 
  filter(!str_detect(variable, '_std|subj')) %>%
  mutate(condition = case_when(
    str_detect(variable, "0.0.0.0") ~ "(compliance)",
    str_detect(variable, "0.0.1.0") ~ "(literal compliance)",
    str_detect(variable, "1.0.0.0") ~ "(literal violation)",
    str_detect(variable, "1.0.1.0") ~ "(violation)"
  ), 
  term = if_else(is.na(condition), variable,
                 paste(str_sub(variable, 1, 1), condition, sep = " "))) %>%
  select(variable, rhat, ess_bulk, ess_tail) %>%
  mutate(across(c(rhat, ess_bulk, ess_tail), ~round(.x, 2))) %>%
  filter(variable != "...1")

print(summary_1c)


summary_1a <- summary_1a %>% mutate(study = "1a")
summary_1b <- summary_1b %>% mutate(study = "1b")
summary_1c <- summary_1c %>% mutate(study = "1c")

diagnostics <- bind_rows(summary_1a, summary_1b, summary_1c) %>%
  filter(variable != "...1") %>%
  mutate(across(c(rhat, ess_bulk, ess_tail), ~round(.x, 2))) %>%
  select(study, variable, rhat, ess_bulk, ess_tail) %>%
  arrange(variable, study)

write.csv(diagnostics, "results/diagnostics.csv", row.names = FALSE)

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

xtabs(~ condition + congprop2, dt)

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


sa_ddm <- read_csv("~/Documents/ddm_rules/model_speed_accuracy/model_speed_accuracy_output.csv") %>%
  rename(term = `...1`) %>%
  filter(!str_detect(term, "_subj."), !str_detect(term, "std")) %>%
  select(term, mean, `2.5q`, `97.5q`)

View(sa_ddm)

cp_ddm <- read_csv("~/Documents/ddm_rules/model_congruency_proportions/model_congruency_proportions_output.csv") %>%
  rename(term = `...1`) %>%
  filter(!str_detect(term, "_subj."), !str_detect(term, "std")) %>%
  select(term, mean, `2.5q`, `97.5q`)

View(cp_ddm)

## 

learning_mega_analysis <- dt %>%
  filter(task == "rules") %>%
  group_by(study, subj_code) %>%
  mutate(index = dense_rank(trial_index)) %>%
  ungroup() %>% 
  mutate(convict = if_else(study == "comply", 
                           1 - response, response), 
         limit = case_when(spd_acc_mode == "acc" ~ 10, 
                           spd_acc_mode == "spd" ~ 5, 
                           .default = 8), 
         proportion = case_when(congprop == "low" ~ 1/6, 
                                congprop == "high" ~ 5/6, 
                                .default = 1/2), 
         dv = case_when(study == "comply" ~ "comply",
                        study == "punishment" ~ "punishment",
                        .default = "violate"), 
         z_index = index/96) 

xtabs(~ limit, learning_mega_analysis)

study_dt = dt %>%
  filter(task == "rules") %>%
  group_by(study, subj_code) %>%
  mutate(index = dense_rank(trial_index), 
         convict = if_else(study == "comply", 
                           1 - response, response)) %>%
  group_by(study, text, purpose, index) %>%
  summarise(convict = mean(convict, na.rm = TRUE))

#glimpse(study_dt)

ggplot(learning_mega_analysis, aes(x = index, y = convict, group = condition)) + 
  annotate(geom = 'rect', xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = .5, 
           fill = 'grey', alpha = .2)+
  geom_line(data = study_dt, aes(group = paste(study, text, purpose), 
                                 linetype = text != purpose, color = as.factor(text)), 
            linewidth = .2, alpha = .6) +
  geom_smooth(aes(linetype = text != purpose, color = as.factor(text), 
                  fill = as.factor(text)),
              method = "gam", linewidth = .7,
              method.args = list(family = binomial),
              formula = y ~ s(x)) +
  theme_classic() + 
  theme(legend.position = 'none', 
        strip.background = element_blank(), 
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 15, face = 'bold'),
        panel.spacing = unit(0.5, "cm"), 
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = 'transparent', color = 'black', 
                                        linewidth = .5)) + 
  scale_y_continuous(name = "Probability of conviction (%)",
                     limits = c(0, 1), expand = c(0, 0), 
                     breaks = NULL, 
                     sec.axis = dup_axis(name = NULL, 
                                          breaks = c(.02, .25, .50, .75, .98), 
                                         labels = c("0%", "25%", "50%", "75%", "100%"))) +
  scale_x_continuous(name = "Trial number", labels = c("   1", "", "48", "", "96    "),
                     expand = c(0, 0), breaks = c(1, 24, 48, 72, 96)) +
  scale_color_manual(values = c("#be983f", "#3f8d97")) +
  scale_fill_manual(values = c("#be983f", "#3f8d97"))

ggsave("results/learning_curve_4a.png", width = 15, height = 15, dpi = 300, units = 'cm')

xtabs(~ index, learning_mega_analysis)


learning_model = glmer(convict ~ z_index * (text + purpose) + limit + proportion + dv + (1 | subj_code) + 
                         (1 | rule), learning_mega_analysis, family = 'binomial', 
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))) 

jtools::summ(learning_model, digits = 3, exp = TRUE, confint = TRUE)

learning_model2 = glmer(convict ~ (z_index + limit + proportion + dv) * (text + purpose) + (1 | subj_code) + 
                          (1 | rule), learning_mega_analysis, family = 'binomial', 
                        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))) 

jtools::summ(learning_model2, digits = 3, exp = TRUE, confint = TRUE)



learning_model3 = glmer(convict ~ (z_index + limit + proportion + dv) * (text + purpose) + (1 | study/subj_code) + 
                          (1 | rule), learning_mega_analysis, family = 'binomial', 
                        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))) 

jtools::summ(learning_model3, digits = 3, exp = TRUE, confint = TRUE)



