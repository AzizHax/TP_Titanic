
#' Calculate Survival Rate by Class
#'
#' This function calculates the survival rate for each passenger class on the Titanic.
#'
#' @param titanic_data A data frame containing the Titanic passenger data.
#' @return A data frame with the total passengers, survived passengers, and survival rate for each class.
#' @export
calculate_survival_rate_by_class <- function(data) {library(usethis)
  library(dplyr)
  library(devtools)
  # Convertir "yes" en 1 et "no" en 0 dans la colonne survived
  data <- data %>%
    mutate(survived = ifelse(survived == "yes", 1, ifelse(survived == "no", 0, NA)))



  # Calculer le taux de survie par classe
  data %>%
    group_by(passengerClass) %>%
    summarize(
      total = n(),
      survived = sum(survived, na.rm = TRUE),
      survival_rate = round((survived / total) * 100, 2)
    )
}




# Calculate Survival Rate by Sex
#'
#' This function calculates the survival rate for each sex on the Titanic.
#'
#' @param titanic_data A data frame containing the Titanic passenger data.
#' @return A data frame with the total passengers, survived passengers, and survival rate for each sex.
#' @export
calculate_survival_rate_by_sex <- function(data) {library(usethis)
  library(dplyr)
  library(devtools)
  # Convertir "yes" en 1 et "no" en 0 dans la colonne survived, gérer les autres valeurs inattendues
  data <- data %>%
    mutate(survived = case_when(
      survived == "yes" ~ 1,
      survived == "no" ~ 0,
      TRUE ~ NA_real_  # Remplacer toutes les autres valeurs par NA
    ))

  # Calculer le taux de survie par sexe
  data %>%
    group_by(sex) %>%
    summarize(
      total = n(),
      survived = sum(survived, na.rm = TRUE),
      survival_rate = round((survived / total) * 100, 2)
    ) %>%
    arrange(desc(survival_rate))
}






