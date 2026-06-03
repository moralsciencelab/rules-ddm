library(ggplot2)

# globally set Helvetica Neue as ggplot font

theme_set(
  theme_minimal(base_family = "Helvetica Neue")
)


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
           fill = 'firebrick', alpha = .1) +
  annotate(geom = 'rect', xmin = -Inf, xmax = 1.17, ymin = -Inf, ymax = Inf, 
           fill = 'lightgrey', alpha = .1) +
  annotate(geom = 'rect', xmin = -Inf, xmax = 1.01, ymin = -Inf, ymax = Inf, 
           fill = 'grey', alpha = .1) +
  geom_segment(aes(x = t0/2, xend = t0/2, y = a/2, yend = a * z), 
               linewidth = .3, alpha = .9,
               arrow = arrow(length = unit(0.15, "cm"), angle = 50, 
                             ends = 'both', type = 'closed'), color = 'firebrick') +
  geom_segment(aes(x = 0, xend = 1.02, y = a*.75), 
               linewidth = .3, alpha = .9,
               arrow = arrow(length = unit(0.15, "cm"), angle = 50, 
                             ends = 'both', type = 'closed'), color = 'darkgrey') +
  geom_segment(aes(x = 0, xend = 1.17, y = a * .75 + .15), 
               linewidth = .3, alpha = .9, linetype = 2,
               arrow = arrow(length = unit(0.15, "cm"), angle = 50, 
                             ends = 'both', type = 'closed'), color = 'darkgrey') +
  geom_segment(aes(x = 7.8, xend = 7.8, y = .01, yend = a - .01), 
               linewidth = .3, alpha = .9,
               arrow = arrow(length = unit(0.15, "cm"), angle = 50, 
                             ends = 'both', type = 'closed'), color = 'black') +
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
  annotate("text", x = max_time * 0.9, y = a - 0.15, label = "Upper boundary", 
           hjust = 1, size = 4) +
  annotate("text", x = max_time * 0.9, y = 0.15, label = "Lower boundary", 
           hjust = 1, size = 4) +
  annotate("text", x = t0/2, y = z * a + 0.1, label = " z", 
           hjust = 1, size = 3.5, color = 'firebrick') +
  annotate("text", x = t0/2, y = a * .75 + 0.25, label = "t", 
           hjust = 1, size = 3.5, color = 'darkgrey') +
  annotate("text", x = 7.7, y = a * .75 , label = "a", 
           hjust = 1, size = 3.5, color = 'black') +
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
        plot.margin = margin(.7, .2, .7, .2, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", linewidth=0.5, fill = NA),
        panel.grid.major.x = element_line(linetype = 1, linewidth = .1, color = 'grey'),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_color_manual(values = c(scales::muted("#be983f"), 
                                "#be983f",
                                "#3f8d97",
                                scales::muted("#3f8d97"))) +
  scale_linetype_manual(values = c(1, 3, 3, 1))

