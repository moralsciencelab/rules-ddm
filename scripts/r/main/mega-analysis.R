library(tidyverse)

dt = read.csv('data/rules_allstudies.csv')
df = read.csv('data/rules_allstudies.csv')

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

study_dt = df %>%
  filter(task == "rules") %>%
  group_by(study, subj_code) %>%
  mutate(index = dense_rank(trial_index), 
         convict = if_else(study == "comply", 
                           1 - response, response)) %>%
  group_by(study, condition, text, purpose, index) %>%
  summarise(convict = mean(convict, na.rm = TRUE))


fig4a = ggplot(learning_mega_analysis, aes(x = index, y = convict, group = reorder(condition, convict))) + 
  annotate(geom = 'rect', xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = .5, 
           fill = 'grey', alpha = .2)+
  geom_line(data = study_dt, aes(group = paste(study, condition), 
                                 linetype = text != purpose, color = condition), 
            linewidth = .2, alpha = .6) +
  geom_smooth(aes(linetype = text != purpose, color = condition, 
                  fill = condition),
              method = "gam", linewidth = .7,
              method.args = list(family = binomial),
              formula = y ~ s(x)) +
  theme_classic() + 
  theme(legend.position = 'none', 
        text = element_text(family = 'Helvetica Neue'),
        strip.background = element_blank(), 
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
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
  scale_color_manual(values = c("full violation" = violation_color,
                                "purpose violation" = scales::muted(compliance_color), 
                                "text violation" = scales::muted(violation_color),
                                "no violation" = compliance_color)) +
  scale_fill_manual(values =  c("full violation" = violation_color,
                                "purpose violation" = scales::muted(compliance_color), 
                                "text violation" = scales::muted(violation_color),
                                "no violation" = compliance_color)) + 
  theme(plot.margin = margin(15, 15, 5, 5))

learning_model_rt = lmer(log(rt) ~ z_index + (text * purpose) + dv + (1 | subj_code) + 
                         (1 | rule), learning_mega_analysis) 
car::Anova(learning_model_rt)
jtools::summ(learning_model_rt, digits = 3, confint = TRUE)

learning_model = glmer(convict ~ z_index * (text * purpose) + limit + proportion + dv + (1 | subj_code) + 
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

interactions::sim_slopes(learning_model, pred = 'z_index', modx = 'text', mod2 = 'purpose', 
                         by = 'dv', type = 'response')
