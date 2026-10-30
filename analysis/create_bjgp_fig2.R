library(tidyverse)
library(here)
library(paletteer)

df_measures <- read_csv(
  here("released_output", "pf_breakdown_measures.csv")
)

df_pf_consultations_total <- df_measures |>
  filter(measure == "count_pf_consultation_services_combined") |>
  filter(interval_start >= "2024-02-01") |>
  select(interval_start, pf_consultation_total = numerator) |>
  mutate(measure = "Pharmacy First Consultation") |>
  unique()

source(here("lib", "functions", "combine_os_nhsbsa_comparison_data.R"))
source(here("lib", "functions", "load_comparison_data.R"))

# The NHS BSA data now also has blood pressure, contraception and referral
# consultations. The submitted figure only counts the seven clinical pathways.
df_bsa_validation_seven_pathways <- df_bsa_validation |>
  filter(
    count_group %in%
      c(
        "Acute Sinusitis",
        "Infected Insect Bite",
        "UTI",
        "Acute Otitis Media",
        "Acute Pharyngitis",
        "Herpes Zoster",
        "Impetigo"
      )
  )

df_pf_consultation_validation_plot <- generate_validation_comparison_dataset(
  df_bsa_validation_seven_pathways,
  df_pf_consultations_total
) |>
  filter(date >= "2024-02-01", date <= "2025-01-01")

colour_group_1 <- paletteer_c("viridis::inferno", n = 10)[6]
colour_group_2 <- paletteer_c("viridis::inferno", n = 10)[2]

pf_validation_consultation_plot <- df_pf_consultation_validation_plot |>
  ggplot(aes(x = date)) +
  geom_point(
    aes(
      y = nhs_bsa_count_100pct,
      colour = "NHS BSA",
      shape = "NHS BSA"
    ),
    size = 2.6
  ) +
  geom_line(
    aes(y = nhs_bsa_count_100pct, colour = "NHS BSA"),
    alpha = .3,
    linewidth = .5
  ) +
  geom_point(
    aes(
      y = opensafely_opensafely_tpp,
      colour = "OpenSAFELY-TPP",
      shape = "OpenSAFELY-TPP"
    ),
    size = 2.6
  ) +
  geom_line(
    aes(
      y = opensafely_opensafely_tpp,
      colour = "OpenSAFELY-TPP"
    ),
    alpha = .3,
    linewidth = .5
  ) +
  geom_label(
    aes(
      y = opensafely_opensafely_tpp,
      label = scales::percent(ratio, accuracy = .1),
    ),
    colour = colour_group_2,
    vjust = -.5,
    size = 3.5
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    labels = scales::label_number()
  ) +
  scale_x_date(
    date_breaks = "1 month",
    labels = scales::label_date_short()
  ) +
  scale_colour_manual(
    values = c(
      "NHS BSA" = colour_group_1,
      "OpenSAFELY-TPP" = colour_group_2
    ),
    name = NULL
  ) +
  scale_shape_manual(
    values = c(
      "NHS BSA" = 15,
      "OpenSAFELY-TPP" = 16
    ),
    name = NULL
  ) +
  labs(
    x = NULL,
    y = "Number of consultations",
    colour = NULL,
    shape = NULL
  ) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey50"),
    legend.position = "bottom",
    text = element_text(size = 14)
  )

ggsave(
  filename = here("output", "bjgp_fig2.png"),
  pf_validation_consultation_plot,
  height = 4,
  width = 8,
  create.dir = TRUE
)
