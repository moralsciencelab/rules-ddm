library(tidyverse)
library(emmeans)
library(lmerTest)
library(logistf)
library(jsonlite)

learn1 = read.csv("study4a_learning_bs/data/learn1_raw.csv")
cases = read.csv("study4a_learning_bs/pilot/unique_cases.csv")
glimpse(learn1)
# View(cases)


demog4a = learn1 %>%
  distinct(subj_code, response) %>%
  filter(str_detect(response, "age|gender")) %>%
  mutate(
    key   = map_chr(response, ~names(fromJSON(.x))),
    value = map_chr(response, ~as.character(fromJSON(.x)))
  ) %>%
  select(-response) %>%
  pivot_wider(names_from = key, values_from = value)

demog4a %>%
  summarise(mean(as.numeric(age), na.rm = TRUE),
            sd(as.numeric(age), na.rm = TRUE))

prop.table(xtabs(~ demog4a$gender)) * 100

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


learn1_trials %>%
  group_by(rule, stimulus, text, purpose) %>%
  summarise(mean(response_num, na.rm = TRUE)) %>%
  View()

mod1 = glmer(response_num ~ trial * (text + purpose) + 
               (1 | subj_code) + (1 | rule), family = 'binomial',
             data = learn1_trials)
car::Anova(mod1)

jtools::summ(mod1, digits = 3, confint = TRUE, exp = TRUE)
emmeans(mod1, ~ text * purpose * trial, type = "response",
        at = list(trial = c(0.25, 0.5, 0.75)))
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
             data = subset(learn1_trials, as.numeric(rt) >= 500))
car::Anova(mod1)

jtools::summ(mod1, digits = 3, confint = TRUE, exp = TRUE)


mod1 = glmer(response_num ~ trial * (text + purpose) + 
               (1 | subj_code) + (1 | rule), family = 'binomial', 
             subset(learn1_trials,  
                    condition_assignment == 'Control' & as.numeric(rt) > 500))
jtools::summ(mod1, digits = 3, confint = TRUE, exp = TRUE)

interactions::interact_plot(mod1, pred = 'trial', modx = 'purpose',
                            mod2 = 'text', interval = TRUE)

learn1_trials$log_trial_c <- log(as.numeric(learn1_trials$trial * 96)) - 
  mean(log(as.numeric(learn1_trials$trial * 96)), na.rm = TRUE)

mod1_log <- glmer(response_num ~ log_trial_c * (text + purpose) + 
                    (1 | subj_code) + (1 | rule), family = 'binomial',
                  data = subset(learn1_trials, as.numeric(rt) >= 500))

jtools::summ(mod1_log, digits = 3, confint = TRUE, exp = TRUE)

mean_resps = learn1_trials %>%
  ungroup() %>%
  filter(as.numeric(rt) > 500) %>%
  group_by(trial, condition_assignment, text, purpose, type) %>%
  summarise(response_num = mean(response_num, na.rm = TRUE))

ggplot(data = mean_resps, aes(x = trial, y = response_num)) + 
  annotate(geom = 'rect', xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = .5, 
           fill = 'grey', alpha = .3)+
  geom_vline(xintercept = 0.5, linetype = 3) +
  geom_line(aes(group = type, color = as.factor(text), 
                linetype = text != purpose), 
            alpha = .4, linewidth = .3) +
  geom_smooth(data = subset(learn1_trials, as.numeric(rt) > 500), 
                            aes(group = type, fill = as.factor(text), 
                                color = as.factor(text), 
                  linetype = text != purpose), linewidth = .5,
              method = "glm", 
              method.args = list(family = "binomial")) + 
  facet_grid(. ~ condition_assignment, scales = 'free_x', space = 'free_x', 
             labeller = labeller(condition_assignment = c(
               "Control" = "Task-Specific Learning",
               "Experimental" = "Domain-General Control"
             ))) +
  theme_classic() + 
  theme(legend.position = 'none', 
        strip.background = element_blank(), 
        strip.text = element_text(size = 8, face = 'bold'),
        panel.spacing = unit(0.5, "cm"), 
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = 'transparent', color = 'black', 
                                        linewidth = .5)) + 
  scale_y_continuous(name = "Violation judgment",
                     limits = c(0, 1), expand = c(0, 0), 
                     breaks = c(0, .5, 1), labels = c("No\n", "", "\nYes")) +
  scale_x_continuous(name = "Trial number", labels = c("1", "", "48", "", "96"),
                     expand = c(0, 0), breaks = c(1/96, .25, .5, .75, 1)) +
  scale_color_manual(values = c("#be983f", "#3f8d97")) +
  scale_fill_manual(values = c("#be983f", "#3f8d97"))

ggsave('learning1_a.jpg', dpi = 600, width = 12, height = 8, units = 'cm')


mod2 = glmer(response_num ~ block * (text + purpose) + 
               (1 | subj_code) + (1 | rule), family = 'binomial', 
             data = subset(learn1_trials, as.numeric(rt) >= 500))
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

color_text = "#3d4a5c"
color_purpose = "#b05c6a"

coef_plot = droplevels(subset(coefs5c, Value.of.block != 'Block1C'))
levels(coef_plot$Value.of.block)

ref_lines = droplevels(subset(coefs5c, Value.of.block == 'Block1C')) %>%
  mutate(Value.of.block = 0.5)

ggplot(data = coef_plot, 
       aes(x = Value.of.block, y = `Est.`, color = predictor)) +
  geom_hline(data = ref_lines,
             aes(yintercept = `Est.`, color = predictor, x = ), 
             linetype = 3) +
  geom_rect(data = ref_lines,
              aes(ymin = `X2.5.`, ymax = `X97.5.`, fill = predictor), 
            xmin = -Inf, xmax = Inf, alpha = .2, color = NA) +
  geom_linerange(data = coef_plot, 
                 aes(ymin = `X2.5.`, ymax = `X97.5.`)) +
  geom_point(data = coef_plot, 
                    shape = 21, fill = 'white', size = 2, stroke = 1.5) + 
  coord_flip() + 
  theme_classic() + 
  scale_x_discrete(labels =  c(expression(Rules[E]),
                   expression(Rules[D])), 
                   name = NULL) + 
  scale_y_continuous(name = 'Odds ratio (OR)', limits = c(2.9, 13.2),
                     trans = 'log', breaks = seq(3, 13, 2), expand = c(0, 0)) + 
  scale_color_manual(values = c(color_purpose, color_text)) +
  scale_fill_manual(values = c(color_purpose, color_text)) +
  theme(axis.ticks = element_blank(),
        text = element_text(family = "Helvetica Neue"),
        axis.text = element_text(size = 12, color = 'black'),
        axis.title.x = element_text(size = 13, face = 'bold'),
        legend.position = 'none')

ggsave('learning1_c.jpg', dpi = 600, width = 14, height = 6, units = 'cm')


mod1 = glmer(response_num ~ group * (text + purpose) + 
               (1 | subj_code), family = 'binomial',
             subset(learn1, group != "Control Block 2"))
car::Anova(mod1)
emtrends(mod1, pairwise ~ group, var = 'text')
emtrends(mod1, pairwise ~ group, var = 'purpose')
