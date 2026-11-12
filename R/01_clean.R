# 01_clean.R clean and combine coders judgemental sheets 
# input: data/step3_coding_sheet_487_coder1.xlsx
#        data/step3_coding_sheet_487_coder2.xlsx
# output: output/coding_merged.csv

library(tidyverse)
library(readxl)

# reading and cleaning the data -------------------------------------------------------

raw1 <- read_excel("data/step3_coding_sheet_487_coder1.xlsx", sheet = "Classification Table", col_types = "text") 

coder1 <- raw1 |>
  set_names(c("item", "abstract", "for20_primary", "for20_6digit", "other_for", "candidates_text", "decision", "joint", "reason", "confidence")) |>
  filter(!is.na(item))

raw2 <- read_excel("data/step3_coding_sheet_487_coder2.xlsx", sheet = "Classification Table", col_types = "text")
coder2 <- raw2 |>
  set_names(c("item", "abstract", "for20_primary", "for20_6digit", "other_for", "candidates_text", "decision", "joint", "reason", "confidence")) |>
  filter(!is.na(item)) |>
  mutate(decision = str_remove_all(decision, "[’']"))

stopifnot(all(str_detect(coder2$decision, "^\\d{4}$")))

coder1 <- coder1 |>
  mutate(confidence = confidence |> str_trim() |> str_to_lower(),
         n_cand = str_count(candidates_text, "\\b\\d{4}\\b"),
         in_cand = str_detect(candidates_text, str_c("\\b", decision, "\\b")))

coder2 <- coder2 |>
  mutate(confidence = confidence |> str_trim() |> str_to_lower(),
         n_cand = str_count(candidates_text, "\\b\\d{4}\\b"),
         in_cand = str_detect(candidates_text, str_c("\\b", decision, "\\b")))
  

coding_merged <- coder1 |>
  inner_join(coder2, by = "item", relationship = "one-to-one", suffix = c("_1", "_2")) |>
  mutate(
    agree = decision_1 == decision_2,
    demo = item %in% c("A166", "A440")) |>
  select(item, for20_primary_1, n_cand_1, decision_1, decision_2, confidence_1, confidence_2, in_cand_1, in_cand_2, agree, demo)
stopifnot(
  nrow(coding_merged) == 487,
  sum(coding_merged$agree) == 375,
  sum(coding_merged$demo) == 2,
  identical(coder1 |> filter(!in_cand) |> pull(item), "A396"),
  all(coder2$in_cand)
)

write_csv(coding_merged, "output/coding_merged.csv")












