library(readr)
library(tidyr)
library(dplyr)
library(here)

df_bsa_consultation_validation <- read_csv(
  here("lib", "nhs_comparison_data", "pf_consultation_validation_data_full.csv")
) %>%
  rename(count_100pct = count) |>
  mutate(count_40pct = round(as.numeric(count_100pct * .4), digits = 0)) %>%
  mutate(data_source = "nhs_bsa") |>
  pivot_longer(
    cols = c(count_100pct, count_40pct),
    names_to = "count_method",
    values_to = "count"
  ) |>
  mutate(
    data_desc = "pf_consultation",
    count_desc = "consultation_type",
  ) |>
  select(year_month, data_source, data_desc, count_desc, count_group = consultation_type, count_method, count) |>
  mutate(count_group = factor(count_group,
    levels = c(
      "sinusitis",
      "infected_insect_bites",
      "uncomplicated_uti",
      "acute_otitis_media",
      "acute_sore_throat",
      "shingles",
      "impetigo",
      "community_pharmacy_clinic_blood_pressure_checks",
      "community_pharmacy_contraceptive_ongoing_consultations",
      "community_pharmacy_contraceptive_initiation_consultations",
      "community_pharmacy_contraceptive_emergency_consultations"
    ),
    labels = c(
      "Acute Sinusitis",
      "Infected Insect Bite",
      "UTI",
      "Acute Otitis Media",
      "Acute Pharyngitis",
      "Herpes Zoster",
      "Impetigo",
      "Blood Pressure Check Service",
      "Contraception Service (Ongoing)",
      "Contraception Service (Initiation)",
      "Contraception Service (Emergency)"
    )
  ))

# df_bsa_medication_validation <- read_csv(here("lib", "validation", "data", "pf_medication_validation_data.csv")) %>%
#   rename(count_100pct = count) |>
#   mutate(count_40pct = round(as.numeric(count_100pct * .4), digits = 0)) %>%
#   mutate(data_source = "nhs_bsa") |>
#   pivot_longer(
#     cols = c(count_100pct, count_40pct),
#     names_to = "count_method",
#     values_to = "count"
#   ) |>
#   mutate(
#     data_desc = "pf_medication",
#     count_desc = "bnf_paragraph",
#   ) |>
#   select(date, data_source, data_desc, count_desc, count_group = bnf_paragraph, count_method, count)

# df_bsa_validation <- bind_rows(df_bsa_consultation_validation, df_bsa_medication_validation) %>%
#   filter(date >= "2024-02-01")

df_bsa_validation <- df_bsa_consultation_validation %>% 
  filter(year_month >= "2024-02-01")
write_csv(df_bsa_validation, here("lib", "nhs_comparison_data", "df_bsa_validation_full.csv"))
rm(df_bsa_consultation_validation)
