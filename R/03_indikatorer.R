library(dplyr)


# DÖDSTAL (MEDEL)


dödstal <- döda_clean %>%
  filter(år_num >= 2001) %>%
  inner_join(
    befolkning_clean %>%
      rename(befolkning = Antal) %>%
      select(ålder_num, kön, år_num, befolkning),
    by = c("ålder_num", "kön", "år_num")
  ) %>%
  mutate(dödstal = Antal / befolkning) %>%
  group_by(ålder_num, kön) %>%
  summarise(
    dödstal_medel = mean(dödstal, na.rm = TRUE),
    .groups = "drop"
  )



# FERTILITETSTAL (MEDEL)


fertilitetstal <- Födda_df %>%
  filter(`moderns ålder` != "totalt ålder",
         `moderns ålder` != "uppgift saknas") %>%
  group_by(`moderns ålder`, år) %>%
  summarise(Antal = sum(Antal, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    år_num = as.numeric(år),
    ålder_num = as.numeric(gsub("[^0-9]", "", `moderns ålder`))
  ) %>%
  inner_join(
    befolkning_clean %>%
      filter(kön == "kvinnor") %>%
      rename(befolkning = Antal) %>%
      select(ålder_num, år_num, befolkning),
    by = c("ålder_num", "år_num")
  ) %>%
  mutate(fertilitetstal = Antal / befolkning) %>%
  group_by(ålder_num) %>%
  summarise(
    fertilitetstal_medel = mean(fertilitetstal, na.rm = TRUE),
    .groups = "drop"
  )



# NETTOMIGRATION (MEDEL)


nettomigration_medel <- nettomigration_comb %>%
  group_by(ålder_num, kön) %>%
  summarise(
    nettomigration_medel = mean(nettomigration, na.rm = TRUE),
    .groups = "drop"
  )



# KÖNSKVOT VID FÖDSEL


könskvot <- Födda_df %>%
  filter(`moderns ålder` == "totalt ålder") %>%
  group_by(kön) %>%
  summarise(totalt = sum(Antal, na.rm = TRUE), .groups = "drop") %>%
  mutate(andel = totalt / sum(totalt))



# BEFOLKNING 2024


befolkning_2024 <- befolkning_clean %>%
  filter(år_num == 2024) %>%
  select(ålder_num, kön, Antal)



# FÖRSÖRJNINGSKVOTER


total_försorjningskvot <- function(df) {
  df %>%
    group_by(år_num) %>%
    summarise(
      unga = sum(Antal[ålder_num < 20], na.rm = TRUE),
      äldre = sum(Antal[ålder_num >= 65], na.rm = TRUE),
      arbetande = sum(Antal[ålder_num >= 20 & ålder_num < 65], na.rm = TRUE),
      total_ratio = (unga + äldre) / arbetande,
      .groups = "drop"
    )
}


ungdomsförsörjningskvot <- function(df) {
  df %>%
    group_by(år_num) %>%
    summarise(
      ung = sum(Antal[ålder_num < 20], na.rm = TRUE),
      arbetande = sum(Antal[ålder_num >= 20 & ålder_num < 65], na.rm = TRUE),
      ung_ratio = ung / arbetande,
      .groups = "drop"
    )
}


äldrekvot <- function(df) {
  df %>%
    group_by(år_num) %>%
    summarise(
      äldre = sum(Antal[ålder_num >= 65], na.rm = TRUE),
      arbetande = sum(Antal[ålder_num >= 20 & ålder_num < 65], na.rm = TRUE),
      äldre_ratio = äldre / arbetande,
      .groups = "drop"
    )
}


försörjningsindikatorer <- function(df, name) {
  df %>%
    group_by(år_num) %>%
    summarise(
      ung = sum(Antal[ålder_num < 20], na.rm = TRUE),
      äldre = sum(Antal[ålder_num >= 65], na.rm = TRUE),
      arbetande = sum(Antal[ålder_num >= 20 & ålder_num < 65], na.rm = TRUE),
      ung_ratio = ung / arbetande,
      äldre_ratio = äldre / arbetande,
      .groups = "drop"
    ) %>%
    mutate(scenario = name)
}
