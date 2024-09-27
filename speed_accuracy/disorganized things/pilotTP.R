library(tidyverse)
library(emmeans)
library(lmerTest)
library(car)

pilotTP = read.csv("rules-speed-and-accuracy.csv")
coding = read.csv('codingsheet.csv')
glimpse(coding)

tp1 = full_join(pilotTP, coding, by = 'stimulus') %>%
  mutate(rule = 
           case_when(str_detect(stimulus, 'university post') ~ 'noise',
                     str_detect(stimulus, 'train station') ~ 'sleep',
                     str_detect(stimulus, 'deer population') ~ 'shoot',
                     str_detect(stimulus, 'traffic accident') ~ 'drink',
                     str_detect(stimulus, 'cars are allowed') ~ 'cars',
                     str_detect(stimulus, 'headmaster') ~ 'phones',
                     str_detect(stimulus, 'A restaurant') ~ 'dogs',
                     str_detect(stimulus, 'house clean') ~ 'shoes'), 
         yesKey = case_when(
           str_detect(key_assignment, 'i for yes') ~ 'i',
           str_detect(key_assignment, 'e for yes') ~ 'e'), 
         resp = if_else(response == yesKey, 1, 0), 
         Order = case_when(
           condition == 1 ~ 'SpeedFirst', 
           condition == 2 ~ 'AccuracyFirst'), 
         rt = as.numeric(rt), 
         block = if_else(trial_index < 185, 'Block1', 'Block2'), 
         Condition = case_when(
           Order == 'SpeedFirst' & block == 'Block1' ~ 'Speed',
           Order == 'AccuracyFirst' & block == 'Block1' ~ 'Accuracy',
           Order == 'SpeedFirst' & block == 'Block2' ~ 'Accuracy',
           Order == 'AccuracyFirst' & block == 'Block2' ~ 'Speed'))


tp1 %>%
  filter(!is.na(text), !is.na(purpose), rule != 'noise') %>%
  group_by(trial_index, text, purpose, rule, Order) %>%
  summarise(rt = max(rt, na.rm = TRUE)) %>%
  ggplot(aes(x = trial_index, y = rt, color = paste(text, purpose))) + 
  geom_point() + facet_grid(. ~ Order)

tp1 = tp1 %>%
  filter(!is.na(text), !is.na(purpose), rule != 'noise')

mod1 = glmer(resp ~ Condition*(text + purpose) + (1 | rule) + (1 | subj_code), 
           tp1, family = 'binomial')
car::Anova(mod1)
emmeans(mod1, pairwise ~ Condition | text * purpose, type = 'response')

mod2 = lmer(rt ~ Condition*(text * purpose) + (1 | rule) + (1 | subj_code), 
             tp1)
car::Anova(mod2)
emmeans(mod2, pairwise ~ text * purpose | Condition, adjust = 'none')

tp1ddm = tp1 %>%
  filter(!is.na(text), !is.na(purpose), rule != 'noise') %>%
  select(subj_code, rule, rt, resp, text, purpose, Condition)

write.csv(tp1ddm, 'tp1ddm.csv', row.names = FALSE)

ddmFigTP = read.csv('model1tp/model1tp_within_traces.csv')
glimpse(ddmFigTP)

ddmFigTP = ddmFigTP %>%
  select(-X) %>%
  gather(key = 'item', value = 'value', na.rm = TRUE) %>%
  mutate(parameter = str_sub(item, 1, 1),
         mode = str_sub(item, 3, 5),
         type = str_sub(item, -26, -1)) %>%
  select(-item)
glimpse(ddmFigTP)

ddmFigTP$type = as.factor(ddmFigTP$type)
levels(ddmFigTP$type) = c('', '', 'over', 
                       'under', 'core', 'over', 
                       'under', 'off', 'core', 'core')

ddmFigTP = ddmFigTP %>%
  mutate(case_type = case_when(
    type == 'core' ~ 'Violates T & P',
    type == 'over' ~ 'Violates T, Complies with P',
    type == 'under' ~ 'Complies with T, Violates P',
    type == 'off' ~ 'Complies with T & P', 
    type == '' ~ 'Aggregate'), 
    Mode = case_when(mode == 'Spe' ~ 'Speed', 
                      mode == 'Acc' ~ 'Accuracy', 
                      mode == '' ~ 'Aggregate'))

ddmFigTP$parameter = as.factor(ddmFigTP$parameter)
levels(ddmFigTP$parameter) = c("Threshold (a)", "Non-decision time (t)", 
                            "Drift rate (abs(v))")

ddmTP = ddmFigTP %>%
  group_by(Mode, case_type, parameter) %>%
  summarise(value = median(value, na.rm = TRUE))

pcA = ggplot(subset(ddmFigTP, case_type != 'Aggregate'), aes(x = Mode, y = value)) + 
  geom_violin(aes(fill = Mode), linewidth = 1, color = NA, alpha = .5) + 
  geom_point(data = subset(ddmTP, case_type != 'Aggregate'), 
             shape = 3) + 
  facet_grid(reorder(case_type, -value) ~ parameter, scales = 'free') + 
  theme_classic() + 
  scale_y_continuous(name = NULL) + 
  scale_x_discrete(name = NULL) +
  theme_bw() + coord_flip() + 
  scale_color_brewer(palette = 'Dark2') +
  scale_fill_brewer(palette = 'Dark2') + 
  theme(legend.position = 'none', 
        strip.text.y = element_text(angle = 0),
        panel.background = element_rect(fill = 'white'),
        strip.background = element_blank(),
        plot.background = element_rect(fill = "white", color = NULL),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "white"),
        text = element_text(family = 'serif', size = 12, face = 'bold'),
        plot.margin = margin(.1, .1, .1, .1, "cm"),
        axis.text.x = element_text(face = 'plain', color = 'black')) +
  guides(linetype = 'none')

pcB = ggplot(subset(ddmFigTP, case_type == 'Aggregate'), 
             aes(x = Mode, y = value)) + 
  geom_violin(aes(fill = Mode), linewidth = 1, color = NA, alpha = .5) + 
  geom_point(data = subset(ddmTP, case_type == 'Aggregate'), 
             shape = 3) + 
  facet_grid(. ~ parameter, scales = 'free') + 
  theme_classic() + 
  scale_y_continuous(name = NULL) + 
  scale_x_discrete(name = NULL) +
  theme_bw() + coord_flip() + 
  scale_color_brewer(palette = 'Dark2') +
  scale_fill_brewer(palette = 'Dark2') + 
  theme(legend.position = 'none', 
        plot.margin = margin(.1, 4.55, .1, .1, "cm"),
        strip.text.y = element_text(angle = 0),
        panel.background = element_rect(fill = 'white'),
        strip.background = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "white"),
        text = element_text(family = 'serif', size = 12, face = 'bold'),
        axis.text.x = element_text(face = 'plain', color = 'black')) +
  guides(linetype = 'none')

ggpubr::ggarrange(pcB, pcA, ncol = 1, heights = c(.45, 1))

### Power Analysis

## Set target N and resampling N
sampleSizeL = 15
sampleSizeH = 150
numSamples = 10
iterat = sampleSizeH - sampleSizeL + 1

## Empty data frames
pValues = matrix(ncol=6, nrow=numSamples*iterat)
colnames(pValues) = c("N", 'Time Pressure', 'Text', 'Purpose',
                       'Time Pressure × Text', 'Time Pressure × Purpose')


## Bootstrap loop
for (i in 1:iterat) {
  for (j in 1:numSamples) {
    obs = (sampleSizeL + i - 1)*100
    
    Resampled = sample_n(tp1ddm, obs, replace=TRUE)
    rowNum = (i*numSamples + j) - numSamples
   # bootModel = suppressMessages(glmer(resp ~ Condition*(text + purpose) + (1 | rule) + (1 | subj_code), 
    #                                   Resampled, family = 'binomial'))
    
    bootModel = suppressMessages(glm(resp ~ Condition*(text + purpose), 
                                       Resampled, family = 'binomial'))
    output = car::Anova(bootModel, type = 2)
    pValues[rowNum, ] = c(obs/85, output$`Pr(>Chisq)`[1:5])
  } 
  message('Iter ', i, ' of ', iterat)
}

pValues = as.data.frame(pValues) 

## Calculate power and create figures
plotPwr = pValues %>%
  gather(-N, key = "term", value = "result") %>%
  mutate(sig = case_when(result <=.05  ~ "Significant",
                         result >.05  ~ "Not Significant"), 
         sig_num = case_when(result <=.05  ~ 1,
                             result >.05  ~ 0))

summaryPwr = plotPwr %>%
  group_by(term, N) %>% 
  summarize(power = mean(sig_num, na.rm = T)*100)

ggplot(summaryPwr, aes(x = N, y = power, fill = term, color = term)) + 
  geom_hline(yintercept = 90, linetype = 2) +
  geom_point(alpha = .5, shape = 21, 
             size = 1.2, fill = 'white') + 
  geom_smooth(linewidth = .8, se = FALSE)+
  scale_x_continuous(name = 'Sample Size')+
  scale_y_continuous(name = 'Power (%)', 
                     limits = c(-3, 103), expand = c(0, 0),
                     breaks = seq(0, 100, 20))+
  theme_bw()+
  theme(legend.position = 'top', panel.spacing = unit(1.5, "lines"),
        plot.margin = unit(c(0.8, 0.8, 0.8, 0.8), "lines"),
        legend.title = element_blank(), 
        axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2")

ggsave('pwrFigTP.jpg', width = 6, height = 4)
