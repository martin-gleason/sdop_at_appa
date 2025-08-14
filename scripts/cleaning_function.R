#cleaning functions
#Cleaning functions

#!!column fix functions
library(tidyverse)

# the !! de-quotes? the masked name. !!sym is not something I know.
race_fix <- function(df, column, ...){
  df <- df %>%
    mutate(!!column := case_when(!!sym(column) == "White Hispanic/Latino" ~"Hispanic or Latino",
                                 !!sym(column) == "White Hispanic or Latino" ~"Hispanic or Latino",
                                 !!sym(column) == "Black or African American" ~"Black",
                                 !!sym(column) == "Asian (of Far East, Southeast Asian, or Indian Subcontinent Origins)" ~"Asian",
                                 !!sym(column)== "White (European)" ~"White",
                                 !!sym(column) == "OTHR" ~"Unspecified",
                                 !!sym(column) == "Black Hispanic /Latino" ~ "Hispanic or Latino",
                                 !!sym(column)== "WHTE" ~ "White",
                                 !!sym(column)== "Hispanic" ~"Hispanic or Latino",
                                 !!sym(column) == "BLAA" ~"Black",
                                 !!sym(column)== "Middle Eastern, Arab, North African" ~ "Middle Eastern, Arab, or North African",
                                 TRUE ~ !!sym(column)
    ))
  return(df)
}


## Find duplicates
find_duplicates <- function(df, column, ...){
  dupes = df %>%
    group_by(!!sym(column)) %>%
    filter(n() > 1) %>%
    arrange(desc(!!sym(column)))
  return(dupes)
}

## last five digits of case number
case_year <- function(df, column, ...){
  df %>% 
    filter(str_detect(!!sym(column), "^\\d{2}"))
}

#need to build in error correction for this one.
#if right column is null, then there must be matching data
# if right column is not null, then there must be matching data.

remove_duplicates <- function(left_df, right_df, left_column, right_column = NULL,...){
  if(is.null(right_column))
  {right_column = left_column
  
  no_dupes = left_df %>% 
    anti_join(right_df, by = {{left_column}}) 
  } else {
    no_dupes = left_df %>% 
      anti_join(right_df, by = c({{left_column}}, {{right_column}}))
  }
  
  return(no_dupes)
}

get_files <- function(path, ...){
  list.files(here("inputs"), pattern = "^[^~]")
}

get_age <- function(dob, date, ...){
  age = interval(start = unique_youth$client_information_date_of_birth, 
                 end = unique_youth$supervision_plan_start_date) /
    duration(num = 1, units = "years") 
  
  floor(age)
  return(age)
}