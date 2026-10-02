library(httr)
library(here)
library(rvest)
library(dplyr)
library(lubridate)
library(tidyverse)
library(readr)
library(jsonlite)

#' Get ICB code to region lookup data
get_icb_region_lookup <- function() {
  query_url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/SICBL22_ICB22_NHSER22_EN_LU/FeatureServer/0/query"

  query_params <- list(
    where = "1=1",
    outFields = "NHSER22NM,ICB22CDH",
    outSR = "4326",
    f = "json"
  )

  response <- GET(query_url, query = query_params)

  json_text <- content(response, as = "text", encoding = "UTF-8")

  fromJSON(json_text, flatten = TRUE)$features |>
    bind_rows() |>
    as_tibble() |>
    rename(region = attributes.NHSER22NM, icb_code = attributes.ICB22CDH) |>
    distinct()
}

#' Extract dates from dispensing data URL
#'
#' @param urls List of URLs
#' @return String, of date in YYYY-MM-DD format if date can be found, otherwise NA.
extract_dates <- function(urls) {
  urls %>%
    map(~ {
      match <- str_match(
        .x,
        ".*dispensing_data_(\\d{4})(\\d{2})"
      )[, 2:3]

      if (all(!is.na(match))) {
        year <- match[1]
        month <- match[2]

        return(
          format(
            as.Date(paste(year, month, "01", sep = "-")),
            "%Y-%m-%d"
          )
        )
      }

      return(NA)
    })
}

get_dispensing_urls <- function(start_date = "2024-02-01", end_date = NULL) {
  # This is the URL where the data is linked from
  url <- "https://opendata.nhsbsa.net/dataset/pharmacy-and-appliance-contractor-dispensing-data"

  # The URL to the data we are interested in follows this URL structure
  # https://www.nhsbsa.nhs.uk/sites/default/files/2024-05/Dispensing%20Data%20Jan%2024%20-%20CSV.csv
  base_url <- "https://opendata.nhsbsa.net"

  response <- GET(url)
  html_content <- content(response, "text", encoding = "UTF-8")

  csv_links <- read_html(html_content) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    .[grepl("dispensing_data_.*\\.csv", .)]
  
  df <- tibble(
    date = as.Date(extract_dates(csv_links) |> unlist()),
    url = csv_links
  )

  if (is.null(start_date)) {
    start_date <- as.Date(min(df$date))
  }

  if (is.null(end_date)) {
    end_date <- as.Date(max(df$date))
  }

  df <- df |>
    filter(between(date, as.Date(start_date), as.Date(end_date)))

  setNames(as.list(df$url), as.character(df$date))
}

get_dispensing_data <- function(start_date = "2024-02-01", end_date = NULL) {
  dispensing_urls <- get_dispensing_urls(start_date = "2024-02-01", end_date = NULL)

  icb_var_list <- c(
    "ICB_CODE",
    "ICB_NAME"
  )

 pf_var_list <- c(
  "pharmacy_first_clinical_pathways_consultations_acute_otitis_media",
  "pharmacy_first_clinical_pathways_consultations_acute_sore_throat",
  "pharmacy_first_clinical_pathways_consultations_impetigo",
  "pharmacy_first_clinical_pathways_consultations_infected_insect_bites",
  "pharmacy_first_clinical_pathways_consultations_shingles",
  "pharmacy_first_clinical_pathways_consultations_sinusitis",
  "pharmacy_first_clinical_pathways_consultations_uncomplicated_uti",
  "pharmacy_first_urgent_medicine_supply_consultations",
  "pharmacy_first_minor_illness_referral_consultations",
  "community_pharmacy_clinic_blood_pressure_checks",
  "community_pharmacy_contraceptive_ongoing_consultations",
  "community_pharmacy_contraceptive_initiation_consultations",
  "community_pharmacy_contraceptive_emergency_consultations"
  )


clean_content <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_|_$", "")
  }
  
  df <- dispensing_urls |>
    map(read_csv, name_repair = janitor::make_clean_names) |>
    bind_rows(.id = "year_month") |>
    mutate(content = clean_content(content)) |>
    filter(content %in% pf_var_list) |>
    select(year_month, icb_code, content, value) |>
    pivot_wider(
      names_from = content,
      values_from = value,
      values_fn = sum,
      values_fill = 0
    )


  df |>
    rename_with(~ str_replace(., "^numberof_pharmacy_first", "n_pf")) |>
    rename_with(~ str_replace(., "clinical_pathways_consultations", "consultation"))
}

# Calculate summary of counts
df_dispensing_data <- get_dispensing_data(start_date = "2024-02-01")
View(df_dispensing_data)
df_dispensing_data_clean <- df_dispensing_data %>%
  janitor::clean_names()
df_dispensing_data_summary <- df_dispensing_data |>
  group_by(year_month) |>
  summarise(
    pharmacy_first_consultation_acute_otitis_media = sum(pharmacy_first_consultation_acute_otitis_media, na.rm = TRUE),
    pharmacy_first_consultation_acute_sore_throat = sum(pharmacy_first_consultation_acute_sore_throat, na.rm = TRUE),
    pharmacy_first_consultation_impetigo = sum(pharmacy_first_consultation_impetigo, na.rm = TRUE),
    pharmacy_first_consultation_infected_insect_bites = sum(pharmacy_first_consultation_infected_insect_bites, na.rm = TRUE),
    pharmacy_first_consultation_shingles = sum(pharmacy_first_consultation_shingles, na.rm = TRUE),
    pharmacy_first_consultation_sinusitis = sum(pharmacy_first_consultation_sinusitis, na.rm = TRUE),
    pharmacy_first_consultation_uncomplicated_uti = sum(pharmacy_first_consultation_uncomplicated_uti, na.rm = TRUE),
    pharmacy_first_urgent_medicine_supply_consultations = sum(pharmacy_first_urgent_medicine_supply_consultations, na.rm = TRUE),
    pharmacy_first_minor_illness_referral_consultations = sum(pharmacy_first_minor_illness_referral_consultations, na.rm = TRUE),
    community_pharmacy_clinic_blood_pressure_checks = sum(community_pharmacy_clinic_blood_pressure_checks, na.rm = TRUE),
    community_pharmacy_contraceptive_ongoing_consultations = sum(community_pharmacy_contraceptive_ongoing_consultations, na.rm = TRUE),
    community_pharmacy_contraceptive_initiation_consultations = sum(community_pharmacy_contraceptive_initiation_consultations, na.rm = TRUE),
    community_pharmacy_contraceptive_emergency_consultations = sum(community_pharmacy_contraceptive_emergency_consultations, na.rm = TRUE)
    # n_pf_urgent_medicine_supply_consultations = sum(n_pf_urgent_medicine_supply_consultations, na.rm = TRUE),
    # n_pf_minor_illness_referral_consultations = sum(n_pf_minor_illness_referral_consultations, na.rm = TRUE)
  ) |>
  pivot_longer(
    cols = c(
      pharmacy_first_consultation_acute_otitis_media,
      pharmacy_first_consultation_acute_sore_throat,
      pharmacy_first_consultation_impetigo,
      pharmacy_first_consultation_infected_insect_bites,
      pharmacy_first_consultation_shingles,
      pharmacy_first_consultation_sinusitis,
      pharmacy_first_consultation_uncomplicated_uti,
      pharmacy_first_urgent_medicine_supply_consultations,
      pharmacy_first_minor_illness_referral_consultations,
      community_pharmacy_clinic_blood_pressure_checks,
      community_pharmacy_contraceptive_ongoing_consultations,
      community_pharmacy_contraceptive_initiation_consultations,
      community_pharmacy_contraceptive_emergency_consultations
    ),
    names_to = "consultation_type",
    values_to = "count"
  ) |>
  mutate(consultation_type = str_replace(consultation_type, "^pharmacy_first_consultation_", ""))

fs::dir_create(here("lib", "nhs_comparison_data"))
write_csv(df_dispensing_data_summary, here("lib", "nhs_comparison_data", "pf_consultation_validation_data_full.csv"))

# # Get counts by region
# icb_region_lookup <- get_icb_region_lookup()

# df_dispensing_data_by_region <- df_dispensing_data |>
#   left_join(icb_region_lookup, by = "icb_code")

# df_dispensing_data_summary_by_region <- df_dispensing_data_by_region |>
#   group_by(date, region) |>
#   summarise(
#     pharmacy_first_clinical_pathways_consultations_acute_otitis_media = sum(pharmacy_first_clinical_pathways_consultations_acute_otitis_media, na.rm = TRUE),
#     pharmacy_first_clinical_pathways_consultations_acute_sore_throat = sum(pharmacy_first_clinical_pathways_consultations_acute_sore_throat, na.rm = TRUE),
#     pharmacy_first_clinical_pathways_consultations_impetigo = sum(pharmacy_first_clinical_pathways_consultations_impetigo, na.rm = TRUE),
#     pharmacy_first_clinical_pathways_consultations_infected_insect_bites = sum(pharmacy_first_clinical_pathways_consultations_infected_insect_bites, na.rm = TRUE),
#     pharmacy_first_clinical_pathways_consultations_shingles = sum(pharmacy_first_clinical_pathways_consultations_shingles, na.rm = TRUE),
#     pharmacy_first_clinical_pathways_consultations_sinusitis = sum(pharmacy_first_clinical_pathways_consultations_sinusitis, na.rm = TRUE),
#     pharmacy_first_clinical_pathways_consultations_uncomplicated_uti = sum(pharmacy_first_clinical_pathways_consultations_uncomplicated_uti, na.rm = TRUE),
#     # n_pf_urgent_medicine_supply_consultations = sum(n_pf_urgent_medicine_supply_consultations, na.rm = TRUE),
#     # n_pf_minor_illness_referral_consultations = sum(n_pf_minor_illness_referral_consultations, na.rm = TRUE)
#   ) |>
#   pivot_longer(
#     cols = c(
#       pharmacy_first_clinical_pathways_consultations_acute_otitis_media,
#       pharmacy_first_clinical_pathways_consultations_acute_sore_throat,
#       pharmacy_first_clinical_pathways_consultations_impetigo,
#       pharmacy_first_clinical_pathways_consultations_infected_insect_bites,
#       pharmacy_first_clinical_pathways_consultations_shingles,
#       pharmacy_first_clinical_pathways_consultations_sinusitis,
#       pharmacy_first_clinical_pathways_consultations_uncomplicated_uti,
#       # n_pf_urgent_medicine_supply_consultations,
#       # n_pf_minor_illness_referral_consultations
#     ),
#     names_to = "consultation_type",
#     values_to = "count"
#   ) |>
#   mutate(consultation_type = str_replace(consultation_type, "^pharmacy_first_clinical_pathways_consultations_", ""))

# write_csv(df_dispensing_data_summary_by_region, here("lib", "nhs_comparison_data", "pf_consultation_validation_data_by_region.csv"))
