library(tidyverse)
library(lmerTest)
library(emmeans)
library(posterior)
library(dplyr)

color_no = "#0072B2"
color_yes = "#E69F00"
color_text = "#009E73"
color_purpose = "#CC79A7"
#color_a = "#D55E00"
#color_v = "#56B4E9"
#color_t = "#000000"

z_color = "#7B2FBE"
a_color = "#2171B5"
v_color = "#D55E00"
t_color = "#999999"
violation_color = "#D55E00"
compliance_color = "#009E73"

# set base family ggplot
theme_set(theme_minimal(base_size = 12, base_family = 'Helvetica Neue'))


dt = read.csv('data/rules_allstudies.csv')
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
  write.csv('results/study1_models.csv', row.names = FALSE)


byparticipant = data.frame()

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
    
    byparticipant = rbind(byparticipant, new_row)
    cat(".")
  }
}

names(byparticipant) = c('study', 'subject_id', 'intercept', 'text', 'purpose')
glimpse(byparticipant)

byparticipant %>%
  group_by(study, text = as.numeric(text) < .05, purpose = as.numeric(purpose) < .05) %>%
  summarise(prop = n()/120) %>%
  ggplot(aes(x = paste(text, purpose), y = prop)) + 
  geom_col(aes(fill = paste(text, purpose))) + 
  facet_wrap(. ~ study)

byparticipant %>%
  gather(intercept:purpose, key = 'term', value = 'pval') %>%
  group_by(study, term) %>%
  summarise(prop_sig = mean(as.numeric(pval) < .05, na.rm = TRUE), 
            n = n())

### Reaction Times ----
rt_v = lmer(log(rt) ~ text * purpose + (1 | rule) + (1 | subj_code), 
            subset(dt, study == 'violate'))

car::Anova(rt_v)
jtools::summ(rt_v, digits = 3, confint = TRUE)

rt_c = lmer(log(rt) ~ text * purpose + (1 | rule) + (1 | subj_code), 
            subset(dt, study == 'comply'))

car::Anova(rt_c)
jtools::summ(rt_c, digits = 3, confint = TRUE)

rt_p = lmer(log(rt) ~ text * purpose + (1 | rule) + (1 | subj_code), 
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
  mutate(across(coefficient:p, ~round(., 2))) %>%
  relocate(dv, iv) %>%
  write.csv('results/study1_rt_models.csv', row.names = FALSE)

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
  write.csv('results/study1_rt_models2.csv', row.names = FALSE)


## Figure 1 ----

dt = dt %>%
  mutate(
    case_var = paste(text, purpose),
    case_var = factor(case_var,
                      levels = c("1 1", "1 0", "0 1", "0 0"),
                      labels = c("Violation", "Literal\nviolation", 
                                 "Literal\ncompliance", "Compliance"))
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
  group_by(study, as.factor(response), text, purpose, case_var) %>%
  summarise(rt = median(rt, na.rm = TRUE))

fig1iqr = dt %>%
  filter( study %in% c('violate', 'comply', 'punishment'), rt > 400) %>%
  group_by(study, as.factor(response), text, purpose, case_var) %>%
  summarise(q1 = fivenum(rt, na.rm = TRUE)[2],
            q3 = fivenum(rt, na.rm = TRUE)[4]) %>%
  gather(q1:q3, key = 'quartile', value = 'rt')

ggplot() + 
  geom_histogram(data=subset(dt, study %in% c('violate', 'comply', 'punishment') &
                               as.factor(response)=="1" & rt > 400), fill = 'white', binwidth = 200,
                 aes(rt, color=as.factor(response), y= ..count../220), linewidth = .2) +
  geom_histogram(data=subset(dt, study %in% c('violate', 'comply', 'punishment') &
                               as.factor(response)=="0" & rt > 400), fill = 'white', binwidth = 200,
                 aes(rt, color=as.factor(response), y= -..count../220), linewidth = .2) +
  facet_grid(case_var ~ study, labeller = labeller(study = study_labels)) +
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
        #strip.text.y.right = element_blank(),
        legend.title = element_blank(),
        panel.spacing.x = unit(.8, "lines"),
        plot.margin = margin(.1,.3,.1,.3, "cm"),
        panel.border = element_rect(colour = "black", size=0.5, fill = NA)) + 
  scale_fill_manual(values=c(color_yes, color_no), 
                    labels = c('Yes', 'No')) +
  scale_color_manual(values=c(color_yes, color_no), 
                     labels = c('Yes', 'No')) +
  guides(color = 'none')

ggsave("figures/SuppFigure1.png", 
       width = 18, height = 15, dpi = 300, units = 'cm')

###

study1_ddm = bind_rows(
  read.csv("results/ddm/model_violate/model_violate_output.csv") %>%
    mutate(study = "study1a"),
  read.csv("results/ddm/model_comply/model_comply_output.csv") %>%
    mutate(study = "study1b"),
  read.csv("results/ddm/model_punishment/model_punishment_output.csv") %>%
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

write.csv(study1_ddm, "results/study1_ddm.csv", row.names = FALSE)

#### Rhat etc

traces <- read_csv("results/ddm/model_violate/model_violate_within_traces.csv")
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

traces <- read_csv("results/ddm/model_comply/model_comply_within_traces.csv")
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

traces <- read_csv("results/ddm/model_punishment/model_punishment_within_traces.csv")
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

write.csv(diagnostics, "results/study1_ddm_diagnostics.csv", row.names = FALSE)
