# libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(here)
library(janitor)
library(lubridate)

# cleaning functions ------------------------------------------------------
race_fix <- function(df, column, ...) {
  df <- df %>%
    mutate(
      !!column := case_when(
        !!sym(column) == "White Hispanic/Latino" ~ "Hispanic or Latino",
        !!sym(column) == "White Hispanic or Latino" ~ "Hispanic or Latino",
        !!sym(column) == "Black or African American" ~ "Black",
        !!sym(column) ==
          "Asian (of Far East, Southeast Asian, or Indian Subcontinent Origins)" ~
          "Asian",
        !!sym(column) == "White (European)" ~ "White",
        !!sym(column) == "OTHR" ~ "Unspecified",
        !!sym(column) == "Black Hispanic /Latino" ~ "Hispanic or Latino",
        !!sym(column) == "WHTE" ~ "White",
        !!sym(column) == "Hispanic" ~ "Hispanic or Latino",
        !!sym(column) == "BLAA" ~ "Black",
        !!sym(column) == "Middle Eastern, Arab, North African" ~
          "Middle Eastern, Arab, or North African",
        TRUE ~ !!sym(column)
      )
    )
  return(df)
}


# loading data ------------------------------------------------------------

closures <- read_xlsx(
  here("inputs", "jan_june_2025_closures.xlsx"),
  sheet = "JAN-JUNE"
) |>
  clean_names() |>
  race_fix("demographics_race") |>
  select(
    -client_information_age,
    -po:-reason_for_unsuccessful_prob_supv,
    -age_at_closure,
    -los
  )

closures <- closures |>
  rename(
    client_id = identifiers_client_number,
    gender = demographics_gender,
    race = demographics_race,
    zip_code = current_address_zip_code,
    dob = client_information_date_of_birth_date,
    supervision_type = supervision_type_code,
    plan_start_date = supervision_plan_start_date_date,
    sub_result = supervision_sub_result,
    date_of_closure = supervision_close_date_date,
    term_length = supervision_term_length,
    days_supervised = supervision_days_supervised,
    case_number = case_case_number,
    court_room = supervision_court_department
  )


closures <- closures |>
  mutate(
    age_at_sup_start = year(as.period(interval(
      start = closures$dob,
      end = closures$plan_start_date
    ))),
    age_at_closure = year(as.period(interval(
      start = closures$dob,
      end = closures$date_of_closure
    )))
  )


closures <- closures |>
  mutate(
    supervision_type = case_when(
      supervision_type == "SUPV" ~ "Supervision",
      .default = "Probation"
    )
  )

closures <- closures |>
  mutate(risk_level = str_replace_all(risk_level, "Low", "LOW"))

closures |>
  ggplot(aes(x = risk_level)) +
  geom_bar(aes(fill = as.factor(zip_code), stat = 'count'))
