table2 <- read_csv("output/table2_groups.csv")

overall <- read_csv("output/table1_overall.csv") |>
  filter(metric == "Agreement rate") |>
  pull(estimate)

fig1 <- table2 |>
  mutate(group = factor(group, levels = c("4+", "3", "2", "other", "both high"))) |>
  ggplot(aes(x = rate, y = group)) +
  geom_vline(xintercept = overall, linetype = "dashed") +
  geom_pointrange(aes(xmin = lower, xmax = upper)) +
  facet_wrap(vars(grouping), ncol = 1, scales = "free_y") +
  scale_x_continuous(labels = scales::percent, limits = c(0.5, 1)) +
  labs(
    x = "Agreement rate (95% CI)",
    y = NULL,
    title = "Coder agreement by confidence and number of candidate groups",
    caption = "Dashed line: overall agreement (n = 485)"
  ) +
  theme_minimal()

ggsave("output/fig1_agreement_by_group.png", fig1, width = 7, height = 4, dpi = 300)

























