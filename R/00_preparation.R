library(tidyverse)
library(readxl)

# =============================================================
# 00_preparation.R  Identify FoR2020 projects whose FoR2008 group is ambiguous
# Input : data/for20_projects.csv (extracted from the main ARC project table;
#         the extraction block below runs only on the author's machine)
#         data/anzsrc2020_anzsrc2008_correspondences.xlsx (official ABS crosswalk)
#         data/step3_item_key.csv (item ID <-> project code)
# Output: output/crosswalk_status.csv (one row = one project; 4,658 rows)
# =============================================================

if(!file.exists("data/for20_projects.csv")){
  arc_projects_base <- read_csv("/Users/weisuogongjue/ARC_data/step2_arc_projects_base.csv", col_types = cols(.default = col_character())) |>
    select(code, ver, for_primary, for_codes, for_version)
  
  for20_projects <- arc_projects_base |>
    filter(ver == "FOR20") |>
    select(-ver)
  stopifnot(nrow(for20_projects) == 4658, !anyDuplicated(for20_projects$code))
  
  write_csv(for20_projects, "data/for20_projects.csv")
}

for20_projects <- read_csv("data/for20_projects.csv", col_types = cols(.default = col_character()))
stopifnot(nrow(for20_projects) == 4658)

for_crosswalk <- read_excel("data/anzsrc2020_anzsrc2008_correspondences.xlsx", sheet = "2020 FoR - 2008 FoR", col_names = FALSE, col_types = "text") |>
  select(1:5) |>
  set_names(c("c20", "n20", "c08", "partial", "n08")) |>
  mutate(across(everything(), str_trim)) |>
  mutate(
    c20 = if_else(str_detect(c20, "^\\d{5}$"), str_c("0", c20), c20),
    c08 = if_else(str_detect(c08, "^\\d{5}$"), str_c("0", c08), c08)
  ) |>
  filter(
    str_detect(c20, "^\\d{6}$"),
    str_detect(c08, "^\\d{6}$")
  ) |>
  mutate(
    g08 = str_sub(c08, start = 1, end = 4)
  )
stopifnot(nrow(for_crosswalk) == 2460)

for20_projects_split <- for20_projects |>
  separate_longer_delim(c(for_codes, for_version), delim = ";") |>
  mutate(across(c(for_codes, for_version), str_trim)) |>
  filter(
    for_version == "FOR20",
    str_detect(for_codes, "^\\d{6}$"),
    str_sub(for_codes, start = 1, end = 4) == for_primary
  ) |>
  rename(c20 = for_codes) |>
  distinct(code, c20)
stopifnot(n_distinct(for20_projects_split$code) == 4658)

for20_projects_join <- for20_projects_split |>
  left_join(for_crosswalk, by = "c20", relationship = "many-to-many") |>
  select(-n20, -partial, -n08)
stopifnot(for20_projects_join |> pull(g08) |> is.na() |> sum() == 0)

max_grp <- for20_projects_join |>
  summarise(
    n = n_distinct(g08),
    .by = c(code, c20)
  ) |>
  summarise(
    max_grp = max(n),
    .by = code
  )

code_summary <- for20_projects_join |>
  summarise(
    n_cand = n_distinct(g08),
    candidates = str_c(sort(unique(g08)), collapse = ";"),
    .by = code
  ) |>
  left_join(max_grp, by = "code") |>
  mutate(
    category = case_when(
      n_cand == 0 ~ "none",
      n_cand == 1 ~ "unique",
      max_grp > 1 ~ "multi_within_code",
      .default = "diverging_codes"
    )
  )
stopifnot(
  nrow(code_summary) == 4658,
  sum(code_summary$category == "unique") == 4171,
  sum(code_summary$category == "multi_within_code") == 353,
  sum(code_summary$category == "diverging_codes") == 134,
  code_summary |> filter(code == "DP230102856") |> pull(candidates) == "0803;0805"
)

item_key <- read_csv("data/step3_item_key.csv", col_types = cols(.default = col_character()))

item_key_codes <- item_key |>
  filter(sheet == "487人工判断表") |>
  pull(code)

code_summary_code <- code_summary |>
  filter(category %in% c("multi_within_code", "diverging_codes")) |>
  pull(code)

stopifnot(
  length(item_key_codes) == 487,
  length(code_summary_code) ==487,
  setequal(item_key_codes, code_summary_code)
)

write_csv(code_summary, "output/crosswalk_status.csv")




