library(tidyverse)
library(emmeans)
library(lmerTest)
library(logistf)
library(jsonlite)
library(magick)
library(grid)
library(ggplot2)
library(ggpubr)
library(scales)


learn1 = read.csv("data/exp4a/learn1_raw.csv")
cases = read.csv("data/unique_cases.csv")
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
             subset(learn1_trials,  
                    condition_assignment == 'Control' & as.numeric(rt) >= 500))
jtools::summ(mod1, digits = 3, confint = TRUE, exp = TRUE)

learn1_trials$log_trial_c <- log(as.numeric(learn1_trials$trial * 96)) - 
  mean(log(as.numeric(learn1_trials$trial * 96)), na.rm = TRUE)

mod1_log <- glmer(response_num ~ log_trial_c * (text + purpose) + 
                    (1 | subj_code) + (1 | rule), family = 'binomial',
                  data = subset(learn1_trials, as.numeric(rt) >= 500))

jtools::summ(mod1_log, digits = 3, confint = TRUE, exp = TRUE)

mean_resps = learn1_trials %>%
  ungroup() %>%
  filter(as.numeric(rt) >= 500) %>%
  group_by(trial, condition_assignment, text, purpose, type) %>%
  summarise(response_num = mean(response_num, na.rm = TRUE))

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

coef_plot = droplevels(subset(coefs5c, Value.of.block != 'Block1C'))
levels(coef_plot$Value.of.block)

ref_lines = droplevels(subset(coefs5c, Value.of.block == 'Block1C')) %>%
  mutate(Value.of.block = 0.5)

fig4c = ggplot(data = coef_plot, 
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
  scale_color_manual(values = c(muted(compliance_color), muted(violation_color))) +
  scale_fill_manual(values = c(muted(compliance_color), muted(violation_color))) +
  theme(axis.ticks = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        text = element_text(family = "Helvetica Neue"),
        axis.text = element_text(size = 12, color = 'black'),
        axis.title.x = element_text(size = 13),
        legend.position = 'none')

# read first page of PDF
img <- image_read_pdf("figures/figure4b.pdf", density = 300)

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
      t = 0,
      r = 0,
      b = 5,
      l = 0
    )
  )

fig4bc <-  icons_plot / fig4c +
  plot_annotation(tag_levels = list(c("B", "C"))) +
  plot_layout(heights = c(1, 1))


final_plot <-
  wrap_plots(fig4a, fig4bc) +
  plot_annotation(tag_levels = list(c("A", "B", "C"))) +
  plot_layout(widths = c(1.1, 1)) +
  theme(
    plot.tag.position = c(0, 1)  # top-left (x, y)
  ) 

ggsave(
  filename = "figures/Figure4.png",
  plot = final_plot,
  width = 26,
  height = 12,
  dpi = 300, 
  units = "cm"
)

learn1_ddm <- read_csv("results/ddm/model_learn/model_learn_output.csv") %>%
  rename(term = `...1`) %>%
  filter(!str_detect(term, "_subj."), !str_detect(term, "std")) %>%
  select(term, mean, `2.5q`, `97.5q`)

print(learn1_ddm)
