library(dplyr)


# 1. STÄDNING AV RÅDATA


befolkning_clean <- befolkning_df %>%
  filter(ålder != "totalt ålder") %>%
  mutate(
    ålder_num = as.numeric(gsub("[^0-9]", "", ålder)),
    år_num = as.numeric(år)
  ) %>%
  filter(år_num >= 2000)

döda_clean <- Döda_df %>%
  filter(ålder != "totalt ålder") %>%
  mutate(
    ålder_num = as.numeric(gsub("[^0-9]", "", ålder)),
    år_num = as.numeric(år)
  ) %>%
  filter(år_num >= 2000)



# 2. BEFOLKNING LAGG (t-1)


bef_lagged <- befolkning_clean %>%
  rename(bef_t_minus1 = Antal) %>%
  mutate(
    ålder_num = ålder_num + 1,
    år_num = år_num + 1
  ) %>%
  select(ålder_num, kön, år_num, bef_t_minus1)



# 3. NETTOMIGRATION (1+ år)


nettomigration <- befolkning_clean %>%
  filter(år_num >= 2001) %>%
  rename(bef_t = Antal) %>%
  inner_join(bef_lagged, by = c("ålder_num", "kön", "år_num")) %>%
  inner_join(
    döda_clean %>%
      rename(döda = Antal) %>%
      select(ålder_num, kön, år_num, döda),
    by = c("ålder_num", "kön", "år_num")
  ) %>%
  mutate(
    nettomigration = bef_t - bef_t_minus1 + döda
  )



# 4. FÖDDA (0-åringar)


Födda_totalt <- Födda_df %>% 
  filter(`moderns ålder` == "totalt ålder") %>% 
  rename(födda = Antal) %>%
  mutate(år_num = as.numeric(år)) %>% 
  select(kön, år_num, födda)


nettomigration_0 <- befolkning_clean %>%
  filter(ålder_num == 0, år_num >= 2001) %>%
  rename(bef_t = Antal) %>%
  inner_join(Födda_totalt, by = c("kön", "år_num")) %>%
  inner_join(
    döda_clean %>%
      filter(ålder_num == 0) %>%
      rename(döda = Antal) %>%
      select(kön, år_num, döda),
    by = c("kön", "år_num")
  ) %>%
  mutate(
    nettomigration = bef_t - födda + döda
  )



# 5. SAMMANSLAGNING MIGRATION


nettomigration_comb <- bind_rows(
  nettomigration %>%
    select(ålder, kön, år, ålder_num, år_num, bef_t, döda, nettomigration),
  nettomigration_0 %>%
    select(ålder, kön, år, ålder_num, år_num, bef_t, döda, nettomigration)
) %>%
  arrange(kön, ålder_num, år_num)



# 6. DÖDSTAL (MEDEL)


Dödstal <- döda_clean %>%
  filter(år_num >= 2001) %>%
  inner_join(
    befolkning_clean %>%
      rename(befolkning = Antal) %>%
      select(ålder_num, kön, år_num, befolkning),
    by = c("ålder_num", "kön", "år_num")
  ) %>%
  mutate(dödstal = Antal / befolkning) %>%
  group_by(ålder_num, kön) %>%
  summarise(dödstal_medel = mean(dödstal, na.rm = TRUE), .groups = "drop")



# 7. FERTILITETSTAL (MEDEL)


Fertilitetstal <- Födda_df %>%
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
  summarise(fertilitetstal_medel = mean(fertilitetstal, na.rm = TRUE), .groups = "drop")



# 8. KÖNSKVOT


könskvot <- Födda_df %>%
  filter(`moderns ålder` == "totalt ålder") %>%
  group_by(kön) %>%
  summarise(totalt = sum(Antal, na.rm = TRUE), .groups = "drop") %>%
  mutate(andel = totalt / sum(totalt))



# 9. NETTOMIGRATION MEDEL


nettomigration_medel <- nettomigration_comb %>%
  group_by(ålder_num, kön) %>%
  summarise(
    nettomigration_medel = mean(nettomigration, na.rm = TRUE),
    .groups = "drop"
  )



# 10. BASPOPULATION 2024


befolkning_2024 <- befolkning_clean %>%
  filter(år_num == 2024) %>%
  select(ålder_num, kön, Antal)

