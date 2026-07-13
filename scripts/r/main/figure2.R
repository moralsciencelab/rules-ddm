library(tidyverse)
library(ggplot2)
library(png)
library(grid)
library(ggpubr)
library(magick)
library(scales)

z_color = "#7B2FBE"
a_color = "#2171B5"
v_color = "#D55E00"
t_color = "#999999"
violation_color = "#D55E00"
compliance_color = "#009E73"


# globally set Helvetica Neue as ggplot font
stat_list = c("accuracy",
              "mean_ub",
              "std_ub",
              "10q_ub",
              "30q_ub",
              "50q_ub",
              "70q_ub",
              "90q_ub",
              "mean_lb",
              "std_lb",
              "10q_lb",
              "30q_lb",
              "50q_lb",
              "70q_lb",
              "90q_lb")


theme_set(
  theme_minimal(base_family = "Helvetica Neue")
)

base_family <- "Helvetica Neue"

df = read.csv('data/rules_allstudies.csv')
# Your DDM parameters
a <- 3.2      
z <- 0.54     
t0 <- 1.1   
v_conditions <- c(1.18, 0.75, -0.97, -1.24)  
v_model <- c(1.1, 0.25, -0.55, -1.15) 
label_conditions <- c('Violation', 'Literal Violation', 
                      'Literal Compliance', 'Compliance')
s <- 1        

dt <- 0.0001  
max_time <- 8 
n_noisy_per_condition <- 10

set.seed(123)

# Simulate with controllable noise
simulate_ddm <- function(v_drift, noise_scale = 1) {
  evidence <- z * a
  time <- t0
  trajectory <- data.frame(time = time, evidence = evidence)
  
  while (evidence > 0 && evidence < a && time < max_time) {
    evidence <- evidence + v_drift * dt + rnorm(1, 0, s * sqrt(dt) * noise_scale)
    time <- time + dt
    trajectory <- rbind(trajectory, c(time, evidence))
  }
  return(trajectory)
}

# Generate noisy trajectories (background layer)
noisy_trajectories <- lapply(seq_along(v_model), function(cond) {
  v_drift <- v_model[cond]
  cond_trajs <- lapply(1:n_noisy_per_condition, function(i) {
    traj <- simulate_ddm(v_drift, noise_scale = 0.8)
    traj$trial <- i
    traj$condition <- label_conditions[cond]
    traj$type <- "noisy"
    traj
  })
  do.call(rbind, cond_trajs)
})
noisy_df <- do.call(rbind, noisy_trajectories)
#noisy_df <- noisy_df[seq(1, nrow(noisy_df), by = 50), ]  # subsample

# Generate clean trajectories (foreground layer)
clean_trajectories <- lapply(seq_along(v_conditions), function(cond) {
  v_drift <- v_conditions[cond]
  traj <- simulate_ddm(v_drift, noise_scale = 0.1)  # Very low noise
  traj$condition <- label_conditions[cond]
  traj$trial <- 0
  traj$type <- "clean"
  traj
})
clean_df <- do.call(rbind, clean_trajectories)
# clean_df <- clean_df[seq(1, nrow(clean_df), by = 50), ]

# Combine dataframes
all_trajectories <- rbind(noisy_df, clean_df)

# Plot with noisy beneath, clean on top
walks_fig = ggplot() +
  # Boundaries
  geom_hline(yintercept = a, linetype = 1, linewidth = 0.8, color = "gray30") +
  geom_hline(yintercept = a/2, linetype = 2, linewidth = 0.4, color = "gray30") +
  geom_hline(yintercept = 0, linetype = 1, linewidth = 0.8, color = "gray30") +
  
  ## draw z and t
  annotate(geom = 'rect', xmin = -Inf, xmax = 1.01, ymin = a/2, ymax = (a * z), 
           fill = z_color, alpha = .1) +
  annotate(geom = 'rect', xmin = -Inf, xmax = 1.17, ymin = -Inf, ymax = Inf, 
           fill = muted(t_color), alpha = .1) +
  annotate(geom = 'rect', xmin = -Inf, xmax = 1.01, ymin = -Inf, ymax = Inf, 
           fill = t_color, alpha = .1) +
  geom_segment(aes(x = t0/2, xend = t0/2, y = a/2, yend = a * z), 
               linewidth = .3, alpha = .9,
               arrow = arrow(length = unit(0.15, "cm"), angle = 50, 
                             ends = 'both', type = 'closed'), color = z_color) +
  geom_segment(aes(x = 0, xend = 1.02, y = a*.75), 
               linewidth = .3, alpha = .9,
               arrow = arrow(length = unit(0.15, "cm"), angle = 50, 
                             ends = 'both', type = 'closed'), color = t_color) +
  geom_segment(aes(x = 0, xend = 1.17, y = a * .75 + .15), 
               linewidth = .3, alpha = .9, linetype = 2,
               arrow = arrow(length = unit(0.15, "cm"), angle = 50, 
                             ends = 'both', type = 'closed'), color = t_color) +
  geom_segment(aes(x = 7.8, xend = 7.8, y = .01, yend = a - .01), 
               linewidth = .3, alpha = .9,
               arrow = arrow(length = unit(0.15, "cm"), angle = 50, 
                             ends = 'both', type = 'closed'), color = a_color) +
  # Noisy trajectories (drawn first = beneath)
  geom_line(data = noisy_df, 
            aes(x = time, y = evidence, linetype = condition,
                group = paste(trial, condition), color = condition),
            alpha = 0.15, linewidth = 0.25) +
  # Clean trajectories (drawn second = on top)
  geom_line(data = clean_df,
            aes(x = time, y = evidence, group = condition,
                linetype = condition, color = condition),
            alpha = 0.9) +
  # Starting point
  geom_point(x = t0, y = z * a, size = 4, color = "black", shape = 21, fill = "white", stroke = 1.5) +
  # Labels
  annotate("text", x = max_time * 0.9, y = a - 0.10,
           label = "Upper boundary",
           hjust = 1, size = 3.5,
           family = base_family) +
  annotate("text", x = max_time * 0.9, y = 0.10,
           label = "Lower boundary",
           hjust = 1, size = 3.5,
           family = base_family) +
  annotate("text", x = t0/2, y = z * a + 0.1,
           label = "z", fontface = 'bold',
           hjust = 1, size = 4,
           color = z_color,
           family = base_family) +
  annotate("text", x = t0/2, y = a * .75 + 0.25, label = "t", 
           hjust = 1, size = 4, color = t_color, fontface = 'bold',
           family = base_family) +
  annotate("text", x = 7.5, y = a * .75 , label = "a", 
           hjust = 1, size = 4, color = a_color, fontface = 'bold',
           family = base_family) +
  labs(x = "Time (seconds)", 
       y = NULL) +
  scale_x_continuous(limits = c(0, 8), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, a), expand = c(0, 0)) +
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none', 
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(), 
        plot.margin = margin(.7, .2, .2, .2, "cm"),
        panel.background = element_blank(),
        #  panel.border = element_rect(colour = "black", linewidth=0.5, fill = NA),
        panel.border = element_blank(),
        panel.grid.major.x = element_line(linetype = 1, linewidth = .1, color = 'grey'),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_color_manual(values = c(compliance_color,
                                scales::muted(compliance_color), 
                                scales::muted(violation_color), 
                                violation_color)) +
  scale_linetype_manual(values = c(1, 3, 3, 1))

ppc_data = 
  bind_rows(bind_rows(
    read.csv("~/Documents/ddm_rules/study1a_ppc_stats_text-0.0_purpose-1.0.csv") %>%
      mutate(text = 0, purpose = 1, stat = stat_list, case = "Purpose violation"),
    read.csv("~/Documents/ddm_rules/study1a_ppc_stats_text-1.0_purpose-1.0.csv") %>%
      mutate(text = 1, purpose = 1, stat = stat_list, case = "Violation"),
    read.csv("~/Documents/ddm_rules/study1a_ppc_stats_text-0.0_purpose-0.0.csv") %>%
      mutate(text = 0, purpose = 0, stat = stat_list, case = "Compliance"),
    read.csv("~/Documents/ddm_rules/study1a_ppc_stats_text-1.0_purpose-0.0.csv") %>%
      mutate(text = 1, purpose = 0, stat = stat_list, case = "Text violation")) %>%
      mutate(study = "Study 1a"),
    bind_rows(read.csv("~/Documents/ddm_rules/study1b_ppc_stats_text-0.0_purpose-1.0.csv") %>%
                mutate(text = 0, purpose = 1, stat = stat_list, case = "Purpose violation"),
              read.csv("~/Documents/ddm_rules/study1b_ppc_stats_text-1.0_purpose-1.0.csv") %>%
                mutate(text = 1, purpose = 1, stat = stat_list, case = "Violation"),
              read.csv("~/Documents/ddm_rules/study1b_ppc_stats_text-0.0_purpose-0.0.csv") %>%
                mutate(text = 0, purpose = 0, stat = stat_list, case = "Compliance"),
              read.csv("~/Documents/ddm_rules/study1b_ppc_stats_text-1.0_purpose-0.0.csv") %>%
                mutate(text = 1, purpose = 0, stat = stat_list, case = "Text violation")) %>%
      mutate(study = "Study 1b"),
    bind_rows(read.csv("~/Documents/ddm_rules/study1c_ppc_stats_text-0.0_purpose-1.0.csv") %>%
                mutate(text = 0, purpose = 1, stat = stat_list, case = "Purpose violation"),
              read.csv("~/Documents/ddm_rules/study1c_ppc_stats_text-1.0_purpose-1.0.csv") %>%
                mutate(text = 1, purpose = 1, stat = stat_list, case = "Violation"),
              read.csv("~/Documents/ddm_rules/study1c_ppc_stats_text-0.0_purpose-0.0.csv") %>%
                mutate(text = 0, purpose = 0, stat = stat_list, case = "Compliance"),
              read.csv("~/Documents/ddm_rules/study1c_ppc_stats_text-1.0_purpose-0.0.csv") %>%
                mutate(text = 1, purpose = 0, stat = stat_list, case = "Text violation")) %>%
      mutate(study = "Study 1c")) %>%
  mutate(quantile = if_else(str_detect(stat, "0q_"), 'Quantile', ''), 
         level = if_else(str_detect(stat, "0q_"), as.numeric(str_sub(stat, 1, 2)), NA), 
         bound = if_else(str_detect(stat, "0q_"), 
                         if_else(str_detect(stat, "_ub"), "upper", "lower"), ""),
         directed_level = if_else(quantile == "Quantile" & bound == "lower", 
                                  -level, level),
         directed_mean = if_else(str_detect(stat, '_ub'), mean, -mean),
         directed_observed = if_else(str_detect(stat, '_ub'), observed, -observed), 
         lower = mean - std,
         upper = mean + std, 
         error = case_when(
           bound == 'lower' & text == 1 & purpose == 1 & study != "Study 1b" ~ 'error',
           bound == 'upper' & text == 0 & purpose == 0  & study != "Study 1b" ~ 'error',
           bound == 'upper' & text == 1 & purpose == 1 & study == "Study 1b" ~ 'error',
           bound == 'lower' & text == 0 & purpose == 0  & study == "Study 1b" ~ 'error',
           .default = 'non-error'
         ))

ggplot(subset(ppc_data, quantile == 'Quantile'), aes(x = level, y = mean)) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_ribbon(aes(ymin = lower, ymax = upper, group = paste(text, purpose, bound)), 
              fill = "orange", alpha = 0.2) +
  geom_line(aes(group = paste(text, purpose, bound)), linetype = 2, color = 'orange') + 
  geom_line(aes(group = paste(text, purpose, bound), y = observed, 
                linetype = error), 
            color = 'navy') + 
  geom_point(aes(group = paste(text, purpose, bound), y = observed), 
             color = 'navy', shape = 21, fill = 'white') + 
  facet_grid(fct_rev(case) ~ study + bound) +
  scale_linetype_manual(values = c(3, 1)) +
  theme_classic() + 
  scale_y_continuous(name = 'Time (seconds)', breaks = seq(0, 6, 2), 
                     limits = c(0, 6), expand = c(0, 0)) +
  scale_x_continuous(name = 'Percentile', breaks = seq(10, 90, 20), 
                     limits = c(5, 95), expand = c(0, 0)) +
  theme(strip.background = element_blank(), 
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linetype = 1, linewidth = .05),
        legend.position = "none",
        panel.spacing.x = unit(0.5, "cm"),
        panel.spacing.y = unit(0.5, "cm"),
        panel.border = element_rect(color = 'black', linewidth = .5))

ggsave('figures/SuppFigure2.png', dpi = 300, width = 16, height = 12, units = 'cm')

ppc_violate = ppc_data %>%
  filter(study == "Study 1a")

ppc_violate$case = factor(ppc_violate$case, levels = c("Violation", "Text violation", 
                                                       "Purpose violation", "Compliance"))

# relabel literal compliance and literal violation
case_labels = c("Violation" = "Violation", 
                "Purpose violation" = "Literal compliance", 
                "Text violation" = "Literal violation", 
                "Compliance" = "Compliance")

fig1b = ggplot(subset(ppc_violate, quantile == 'Quantile'), aes(x = directed_level, y = mean)) +
  geom_vline(xintercept = 0, linetype = 1, linewidth = .2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, group = paste(text, purpose, bound), 
                  fill = case, alpha = error)) +
  geom_line(aes(group = paste(text, purpose, bound)), 
            color = 'white', linetype = 1, linewidth = .5, alpha = .8) + 
  geom_line(aes(group = paste(text, purpose, bound), y = observed), 
            color = 'black', linetype = 2) + 
  geom_point(aes(group = paste(text, purpose, bound), y = observed), 
             color = 'black', shape = 21, fill = 'white', size = 1) + 
  facet_grid(case ~ ., 
             labeller = labeller(case = case_labels)
  ) +
  scale_x_continuous(name = NULL, breaks = NULL, limits = c(-92, 92),
                     expand = c(0, 0),
                     labels = NULL) +
  theme_classic() + 
  scale_y_continuous(name = "", limits = c(0, 8), expand = c(0, 0)) +
  coord_flip() +
  scale_color_manual(values = c(violation_color, muted(violation_color), muted(compliance_color), compliance_color)) +
  scale_fill_manual(values = c(violation_color, muted(violation_color), muted(compliance_color), compliance_color)) +
  scale_alpha_manual(values = c(.1, .5)) +
  theme(strip.background = element_blank(), 
        #strip.text = element_blank(), 
        axis.ticks = element_blank(), axis.line = element_blank(),
        legend.position = 'none',
        panel.spacing.x = unit(.8, "lines"),
        plot.margin = margin(.7, .2, .2, .2, "cm"),
        panel.border = element_rect(colour = "black", size=0.5, fill = NA),
        panel.grid.major.x = element_line(linetype = 1, linewidth = .1, color = 'grey'))



fig1a = subset(df, study %in% c('violate'))

fig1a$case = factor(fig1a$condition, levels = c("full violation", "text violation", 
                                                       "purpose violation", "no violation"))

# relabel literal compliance and literal violation
case_labels = c("full violation" = "Violation", 
                "purpose violation" = "Literal compliance", 
                "text violation" = "Literal violation", 
                "no violation" = "Compliance")

fig1mdn = fig1a %>%
  group_by(case, response) %>%
  summarise(rt = median(rt, na.rm = TRUE))

fig1a_rt = ggplot() + 
  geom_histogram(data=subset(fig1a,
                               as.factor(response)=="1" & rt > 400), fill = 'white', binwidth = 200,
                 aes(rt, color=case, y= ..count../220), linewidth = .2) +
  geom_histogram(data=subset(fig1a,
                               as.factor(response)=="0" & rt > 400), fill = 'white', binwidth = 200,
                 aes(rt, color=case, y= -..count../220), linewidth = .2) +
  facet_grid(case ~ ., 
             labeller = labeller(case = case_labels)) +
  geom_density(data=subset(fig1a, as.factor(response)=="1" & rt > 400), color = NA,
               aes(rt, fill=case,  y= ..count..), alpha = .3) +
  geom_density(data=subset(fig1a, as.factor(response)=="0" & rt > 400), color = NA,
               aes(rt, fill=case,  y= -..count..), alpha = .3) +
  theme_classic() + 
  geom_segment(data = subset(fig1mdn, response == 0), aes(x = rt, xend = rt, 
                                                             color = case), 
               y = -Inf, yend = 0,  linetype = 1, size = 0.7) + 
  geom_segment(data = subset(fig1mdn, response == 1), aes(x = rt, xend = rt,
                                                             color = case), 
               y = 0, yend = Inf,  linetype = 1,  size = 0.7) +
  scale_y_continuous(name = '', limits = c(-1.4, 1.4), breaks = NULL) + 
  scale_x_continuous(name = '', expand = c(0, 0),
                     limits = c(0, 8000), breaks = seq(0, 8000, 2000), 
                     labels = c(' 0', '2', '4', '6', '8 ')) + 
  theme(legend.position = 'none', 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        strip.text.y.right = element_blank(),
       # plot.margin = margin(.7, .2, .2, l = -1, "cm"),
        axis.ticks = element_blank(), axis.line = element_blank(),
        panel.spacing.x = unit(.8, "lines"),
        panel.border = element_rect(colour = "black", size=0.5, fill = NA)) + 
  scale_color_manual(values = c(violation_color, muted(violation_color), muted(compliance_color), compliance_color)) +
  scale_fill_manual(values = c(violation_color, muted(violation_color), muted(compliance_color), compliance_color)) +
  guides(color = 'none')

# import figures/icons.png
icons = png::readPNG('figures/icons.png')
# convert to raster grob
icons_grob <- rasterGrob(icons, interpolate = TRUE)

# wrap grob as ggplot
icons_plot <- ggplot() +
  annotation_custom(icons_grob, xmin = -Inf, xmax = Inf,
                    ymin = -Inf, ymax = Inf) +
  theme_void() +
  theme(
    plot.margin = margin(
      t = 22,
      r = -100,
      b = 30,   # increase this
      l = 0
    )
  )

figure2 = icons_plot + fig1a_rt + walks_fig + fig1b +
  plot_annotation(tag_levels = list(c("", "A", "B", "C"))) +
  plot_layout(widths = c(0.65, 1, 1, 1)) 

ggsave(
  filename = "figures/Figure2.png",
  plot = figure2,
  width = 24,
  height = 14,
  dpi = 300, 
  units = "cm"
)

ggsave('figures/Figure2.png', dpi = 300, width = 20, height = 14, units = 'cm')
