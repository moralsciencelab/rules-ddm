library(tibble)
library(dplyr)
library(purrr)

simulate_ddm <- function(v, a, z, t0, n_steps = 1000, dt = 0.01, noise = .08) {
  x <- numeric(n_steps)
  x[1] <- z
  for (i in 2:n_steps) {
    x[i] <- x[i - 1] + v * dt + rnorm(1, 0, noise * sqrt(dt))
    if (x[i] >= a) break
    if (x[i] <= 0) break
  }
  tibble(
    time = seq(t0, t0 + length(x) * dt, by = dt)[1:length(x)],
    evidence = x
  )
}

simulate_ddm_multiple <- function(v, a, z, t0, n_steps = 1000, dt = 0.01, noise = .5, n_walks = 20) {
  # Generate a list of n_walks random walks
  walks_list <- lapply(1:n_walks, function(i) {
    x <- numeric(n_steps)
    x[1] <- z
    for (j in 2:n_steps) {
      x[j] <- x[j - 1] + v * dt + rnorm(1, 0, noise * sqrt(dt))
      if (x[j] >= a) break
      if (x[j] <= 0) break
    }
    tibble(
      time = seq(t0, t0 + n_steps * dt, by = dt)[1:length(x)],
      evidence = x,
      walk_id = i  # identify individual trajectories
    )
  })
  bind_rows(walks_list)
}

faint_walks <- params %>%
  mutate(
    data = pmap(list(v, a, z * a, t0), simulate_ddm_multiple, n_walks = 10)
  ) %>%
  unnest(data)

fixed_a = 3.16  # boundary separation
fixed_z = .54  # bias (as fraction of a)

params <- tibble(
  text = c(1, 1, 0, 0),
  purpose = c(1, 0, 1, 0),
  v = c(1.02, 0.23, -0.52, -1.18),  # drift rate
  a = fixed_a,  # boundary separation
  z = fixed_z,  # bias (as fraction of a)
  t0 = c(1.03, 1.01, 1.17, 1.05) # non-decision time
)

walks <- params %>%
  mutate(
    data = pmap(list(v, a, z * a, t0), simulate_ddm)
  ) %>%
  unnest(data)


walks_fig = ggplot(walks, aes(x = time, y = evidence - fixed_a/2, color = v, group = interaction(text, purpose))) +
  geom_hline(yintercept = 0, linetype = 2, color = 'grey', linewidth = .3) +
  annotate(geom = 'rect', xmin = -Inf, xmax = 1.01, ymin = 0, ymax = fixed_a * fixed_z - (fixed_a/2), 
           fill = 'firebrick', alpha = .1) +
  geom_segment(aes(x = 0.5, xend = 0.5, y = 0, yend = fixed_a * fixed_z - (fixed_a/2)), 
               linewidth = .3, alpha = .1,
               arrow = arrow(length = unit(0.15, "cm"), angle = 50, 
                             ends = 'both', type = 'closed'), color = 'firebrick') +
  annotate(geom = 'rect', xmin = -Inf, xmax = 1.17, ymin = -Inf, ymax = Inf, 
           fill = 'lightgrey', alpha = .1) +
  annotate(geom = 'rect', xmin = -Inf, xmax = 1.01, ymin = -Inf, ymax = Inf, 
           fill = 'grey', alpha = .1) +
  geom_segment(aes(x = 0, xend = 1.02, y = fixed_a/4), 
               linewidth = .3, alpha = .1,
               arrow = arrow(length = unit(0.15, "cm"), angle = 50, 
                             ends = 'both', type = 'closed'), color = 'darkgrey') +
  geom_segment(aes(x = 0, xend = 1.17, y = fixed_a/4 + .15), 
               linewidth = .3, alpha = .1, linetype = 2,
               arrow = arrow(length = unit(0.15, "cm"), angle = 50, 
                             ends = 'both', type = 'closed'), color = 'darkgrey') +
  geom_segment(aes(x = 7.8, xend = 7.8, y = -fixed_a/2 +.01, yend = fixed_a/2 - .01), 
               linewidth = .3, alpha = .1,
               arrow = arrow(length = unit(0.15, "cm"), angle = 50, 
                             ends = 'both', type = 'closed'), color = 'black') +
  geom_line(data = faint_walks, size = 1, 
            aes(linetype = text != purpose, group = paste(walk_id, text, purpose)), 
            linewidth = .2, alpha = .2) +
  geom_line(size = 1, aes(linetype = text != purpose), linewidth = .5) +
  
  # Add boundary lines
 # geom_hline(data = params, aes(yintercept = fixed_a/2), color = "black", linetype = 1, linewidth = .3) +
  #geom_hline(data = params, aes(yintercept = -fixed_a/2), color = "black", linetype = 1, linewidth = .3) +
  
  # Label the boundary separation
  geom_text(
    data = params,
    aes(x = max(walks$time) * 0.9, y = a/2 - 0.05,
        label = paste0("a = ", round(a, 2))),
    color = "black",
    size = 3
  ) +
  
  # Nice color scale for drift rates
  scale_color_gradient2(
    low = "#be983f", mid = "gray80", high = "#3f8d97",
    midpoint = mean(params$v)
  ) +
  theme_classic() + 
  scale_x_continuous(name = 'Reaction time (s)', limits = c(0, 8), expand = c(0.001, 0.001)) +
  scale_y_continuous(limits = c(-fixed_a/2, fixed_a/2), name = NULL, 
                     breaks = NULL, expand = c(0, 0)) + 
  theme(legend.position = 'none', 
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(), 
        plot.margin = margin(.7, .2, .7, .2, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", size=0.5, fill = NA),
        panel.grid.major.x = element_line(linetype = 1, linewidth = .1, color = 'grey'))


ggsave('randomwalks.jpg', dpi = 300, width = 10, height = 10, units = 'cm')
