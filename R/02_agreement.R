library(tidyverse)

# A166 and A440 were worked examples shown to both coders,
# so they are excluded from the agreement statistics.

coding_merged <- read_csv("output/coding_merged.csv") |>
  filter(!demo)
stopifnot(nrow(coding_merged) == 485, sum(coding_merged$agree) == 373)

po <- coding_merged |>
  pull(agree) |>
  mean()

n <- nrow(coding_merged)

se_po <- sqrt(po*(1-po)/n)

share1 <- coding_merged |>
  count(group = decision_1, name = "n1") |>
  mutate(share1 = n1/sum(n1))

share2 <- coding_merged |>
  count(group = decision_2, name = "n2") |>
  mutate(share2 = n2/sum(n2))

pe <- share1 |>
  inner_join(share2, by = "group") |>
  summarise(pe = sum(share1*share2)) |>
  pull(pe)

kappa_ci <- function(po, pe, n){
  k <- (po - pe)/(1 - pe)
  se <- sqrt(po * (1 - po)/(n * (1 - pe)^2))
  tibble(kappa = k, lower = k - 1.96 * se, upper = k + 1.96 * se)
}

pe_adj <- mean(1/coding_merged$n_cand_1)

k_adj <- kappa_ci(po, pe_adj, n)

k_cohen <- kappa_ci(po, pe, n)

kappa_cohen <- function(df){
  po <- mean(df$agree)
  share1 <- df |> count(group = decision_1, name = "n1") |> mutate(share1 = n1/sum(n1))
  share2 <- df |> count(group = decision_2, name = "n2") |> mutate(share2 = n2/sum(n2))
  pe <- share1 |> inner_join(share2, by = "group") |> summarise(pe = sum(share1*share2)) |> pull(pe)
  k <- (po - pe)/(1 - pe)
  k
}

set.seed(20261110)
boot_cohen <- replicate(2000, coding_merged |> slice_sample(prop = 1, replace = TRUE) |> kappa_cohen())
ci_cohen <- quantile(boot_cohen, c(0.025, 0.975))

kappa_adj <- function(df){
  po <- mean(df$agree)
  pe <- mean(1/df$n_cand_1)
  k <- (po - pe)/(1 - pe)
  k
}

set.seed(20261110)
boot_adj <- replicate(2000, coding_merged |> slice_sample(prop = 1, replace = TRUE) |> kappa_adj())
ci_adj <- quantile(boot_adj, c(0.025, 0.975))

set.seed(20261110)
boot_po <- replicate(2000, coding_merged |> slice_sample(prop = 1, replace = TRUE) |> pull(agree) |> mean())
ci_po <- quantile(boot_po, c(0.025, 0.975))

table1_overall <- tibble(
  metric = c("Agreement rate", "Cohen's kappa", "Adjusted kappa"),
  estimate = c(po, k_cohen$kappa, k_adj$kappa),
  lower_formula = c(po - 1.96*se_po, k_cohen$lower, k_adj$lower),
  upper_formula = c(po + 1.96*se_po, k_cohen$upper, k_adj$upper),
  lower_boot = c(ci_po[1], ci_cohen[1], ci_adj[1]),
  upper_boot = c(ci_po[2], ci_cohen[2], ci_adj[2])
) |>
  mutate(across(-metric, function(x) round(x, digits = 3)))


coding_merged <- coding_merged |>
  mutate(
    conf_group = if_else(confidence_1 == "high" & confidence_2 == "high", "both high", "other"),
    cand_group = case_when(
      n_cand_1 == 2 ~ "2",
      n_cand_1 == 3 ~ "3",
      .default = "4+"
    )
  )

conf_group_sum <- coding_merged |>
  summarise(n = n(),
            n_agree = sum(agree),
            rate = mean(agree),
            .by = conf_group) |>
  mutate(rate = n_agree/n,
         se = sqrt(rate*(1-rate)/n),
         lower = rate - 1.96*se,
         upper = rate + 1.96*se) |>
  rename(group = conf_group) |>
  mutate(grouping = "confidence")

cand_group_sum <- coding_merged |>
  summarise(n = n(),
            n_agree = sum(agree),
            rate = mean(agree),
            .by = cand_group) |>
  mutate(rate = n_agree/n,
         se = sqrt(rate*(1-rate)/n),
         lower = rate - 1.96*se,
         upper = rate + 1.96*se) |>
  rename(group = cand_group) |>
  mutate(grouping = "candidate")

table2_groups <- conf_group_sum |>
  bind_rows(cand_group_sum) |>
  mutate(across(c(rate, se, lower, upper), function(x) round(x, digits = 3)))

pairs_all <- coding_merged |>
  filter(!agree) |>
  mutate(
    group_a = pmin(decision_1, decision_2),
    group_b = pmax(decision_1, decision_2)
  ) |>
  count(group_a, group_b, sort = TRUE) |>
  mutate(share = n/sum(n))
stopifnot(sum(pairs_all$n) == 112)

table3_pairs <- pairs_all |>
  slice_max(n, n = 10)

write_csv(table1_overall, "output/table1_overall.csv")
write_csv(table2_groups, "output/table2_groups.csv")
write_csv(table3_pairs, "output/table3_pairs.csv")
