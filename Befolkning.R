# install.packages("pxweb")
# install.packages("plotly")
# install.packages("scales")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("kableExtra")
library(kableExtra)
library(pxweb)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)



# Läs in data

# Folkmängden efter ålder och kön. År 2000 - 2024

befolkning <- pxweb_get(
  url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101A/BefolkningR1860N", 
  query = list(
    Kon = c("1", "2"), 
    Alder = c("*"), 
    ContentsCode = c("0000053A"), 
    Tid = as.character(2000:2024)
  )
)

befolkning_df = as.data.frame(befolkning, column.name.type = "text", 
                              variable.value.type = "text")


# Levande födda efter moderns ålder

Födda <- pxweb_get(
  url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101H/FoddaK", 
  query = list( 
    AlderModer = c("*"),
    Kon = c("*"), 
    Region = c("00"),
    ContentsCode = c("BE0101E2"), 
    Tid = as.character(2000:2024))
)

Födda_df <- as.data.frame(Födda, column.name.type = "text", 
                          variable.value.type = "text")


# Döda efter ålder och kön

Döda <- pxweb_get(
  url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101I/DodaHandelseK", 
  query = list(
    Kon = c("1", "2"), 
    Alder = c("*"),
    Region = c("00"), 
    ContentsCode = c("BE0101D9"), 
    Tid = as.character(2000:2024)
  )
)

Döda_df <- as.data.frame(Döda, column.name.type = "text", 
                         variable.value.type = "text")


# In- och utvandring efter ålder och kön

Migration <- pxweb_get(
  url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/Flyttningar97", 
  query = list(
    Region = c("00"),
    Alder = c("*"),
    Kon = c("1", "2"),
    ContentsCode = c("*"),
    Tid = as.character(2000:2024) 
  )
)

# Omvandla till en vanlig dataframe
Migration_df <- as.data.frame(Migration, stringsAsFactors = FALSE)


# Sortera data 


# Ta bort totalsummor och konvertera ålder till numerisk

befolkning_clean <- befolkning_df %>%
  filter(ålder != "totalt ålder") %>%
  mutate(
    ålder_num = as.numeric(gsub("[^0-9]", "", ålder)),
    år_num = as.numeric(år)
  )

döda_clean <- Döda_df %>%
  filter(ålder != "totalt ålder") %>%
  mutate(
    ålder_num = as.numeric(gsub("[^0-9]", "", ålder)),
    år_num = as.numeric(år)
  )

migration_clean <- Migration_df %>%
  filter(ålder != "totalt ålder") %>%
  mutate(
    ålder_num = as.numeric(gsub("[^0-9]", "", ålder)), 
    år_num = as.numeric(år), 
    nettomigration = Invandringar - Utvandringar
  )


#####




bef_lagged <- befolkning_clean %>%
  rename(bef_t_minus1 = Antal) %>%
  mutate(
    ålder_num = ålder_num + 1,
    år_num = år_num + 1
  ) %>%
  select(ålder_num, kön, år_num, bef_t_minus1)




# Hantera 0-åringar

Födda_totalt <- Födda_df %>% 
  filter(`moderns ålder` == "totalt ålder") %>% 
  rename(födda = Antal) %>%
  mutate(år_num = as.numeric(år)) %>% 
  select(kön, år_num, födda)

# Nettomigration för 0-åringar




# Beräkna medeldödstal

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



# Beräkna fertilitetstal (medel)


Fertilitetstal <- Födda_df %>%
  filter(`moderns ålder` != "totalt ålder",
         `moderns ålder` != "uppgift saknas") %>%
  group_by(`moderns ålder`, år) %>%
  summarise(Antal = sum(Antal, na.rm = TRUE), .groups = "drop") %>%
  mutate(år_num = as.numeric(år),
         ålder_num = as.numeric(gsub("[^0-9]", "", `moderns ålder`))) %>%
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



## Beräkna könskvot

könskvot <- Födda_df %>%
  filter(`moderns ålder` == "totalt ålder") %>%
  group_by(kön) %>%
  summarise(totalt = sum(Antal, na.rm = TRUE), .groups = "drop") %>%
  mutate(andel = totalt / sum(totalt))

print(könskvot)




# Beräkna nettomigration per ålder, kön (medel)

nettomigration_medel <- migration_clean %>%
  group_by(ålder_num, kön) %>%
  summarise(nettomigration_medel = mean(nettomigration, na.rm = TRUE), .groups = "drop")

head(nettomigration_medel)



# Kontrollera befolkningsdata

befolkning_2024 <- befolkning_clean %>%
  filter(år_num == 2024) %>%
  select(ålder_num, kön, Antal)

head(befolkning_2024)
nrow(befolkning_2024)


#### Grupperingar och kvoter ####

andel_65 <- function(df) {
  df %>%
    group_by(år_num) %>%
    summarise(
      total = sum(Antal),
      age65 = sum(Antal[ålder_num >= 65], na.rm = TRUE),
      andel_65 = age65 / total
    )
}

total_försörjningskvot <- function(df) {
  df %>%
    group_by(år_num) %>%
    summarise(
      unga = sum(Antal[ålder_num < 20], na.rm = TRUE),
      äldre = sum(Antal[ålder_num >= 65], na.rm = TRUE),
      arbetande = sum(Antal[ålder_num >= 20 & ålder_num < 65], na.rm = TRUE),
      
      total_ratio = ifelse(arbetande > 0, (unga + äldre) / arbetande, NA_real_),
      .groups = "drop"
    )
}

policy_indicators <- function(df, name) {
  dependency_ratio(df) %>% mutate(scenario = name)
}

ungdomsförsörjningskvot <- function(df) {
  
  df %>%
    group_by(år_num) %>%
    summarise(
      ung = sum(Antal[ålder_num < 20], na.rm = TRUE),
      arbetande = sum(Antal[ålder_num >= 20 & ålder_num < 65], na.rm = TRUE),
      
      ung_ratio = ifelse(arbetande > 0, ung / arbetande, NA_real_),
      
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


# Skapa funktion för framskrivning



framskrivning <- function(
    befolkning_start,
    antal_år,
    fert_factor = 1,
    död_factor = 1,
    migration_factor = 1
) {
  
  resultat <- list()
  resultat[[1]] <- befolkning_start %>% mutate(år_num = 2024)
  
  for (t in 1:antal_år) {
    
    bef_t <- resultat[[t]]
    år <- 2024 + t
    
    
    
    bef_ny <- bef_t %>%
      mutate(ålder_num = ålder_num + 1) %>%
      
      inner_join(Dödstal, by = c("ålder_num", "kön")) %>%
      inner_join(nettomigration_medel, by = c("ålder_num", "kön")) %>%
      inner_join(befolkning_clean %>%
                   group_by(ålder_num, kön) %>%
                   summarise(bef_medel = mean(Antal, na.rm = TRUE), .groups = "drop"),
                 by = c("ålder_num", "kön")) %>%
      
      mutate(
        
        överlevande = Antal * (1 - dödstal_medel * död_factor),
        
        
        mig_rate = nettomigration_medel / bef_medel,
        
        migration = överlevande * mig_rate * migration_factor,
        
        Antal = pmax(överlevande + migration, 0)
      ) %>%
      
      select(ålder_num, kön, Antal)
    
    
    # -------------------------
    # 2. Födda
    # -------------------------
    
    nyfödda_totalt <- bef_ny %>%
      filter(kön == "kvinnor") %>%
      inner_join(Fertilitetstal, by = "ålder_num") %>%
      summarise(
        nyfödda = sum(Antal * fertilitetstal_medel * fert_factor, na.rm = TRUE)
      ) %>%
      pull(nyfödda)
    
    # könsfördelning
    nyfödda <- tibble(
      ålder_num = 0,
      kön = c("män", "kvinnor"),
      Antal = c(
        nyfödda_totalt * 0.514,
        nyfödda_totalt * 0.486
      )
    )
    
    # -------------------------
    # 3. Lägg ihop + tidssteg
    # -------------------------
    
    bef_ny <- bind_rows(bef_ny, nyfödda) %>%
      mutate(år_num = år)
    
    resultat[[t + 1]] <- bef_ny
  }
  
  bind_rows(resultat)
}



# Prognoser för statiska värden 

### Lägg in antalet år in i framtiden du vill göra framskrivning till ###

Framskrivning_antalår <- 25

Bas <- framskrivning(befolkning_2024, antal_år = Framskrivning_antalår)



### Scenarios ###


Hög_migration <- framskrivning(befolkning_2024, 
                               antal_år = Framskrivning_antalår, 
                               fert_factor = 1, 
                               död_factor = 1, 
                               migration_factor = 1.15)

Låg_fertilitet <- framskrivning(befolkning_2024,
                                antal_år = Framskrivning_antalår, 
                                fert_factor = 0.8, 
                                död_factor = 1, 
                                migration_factor = 1)

Stress <- framskrivning(befolkning_2024, 
                        antal_år = Framskrivning_antalår,
                        fert_factor = 0.85, 
                        död_factor = 1.03, 
                        migration_factor = 0.85)



## Total försörjningskvot ###

hist_fk <- total_försörjningskvot(befolkning_clean) %>%
  mutate(scenario = "Historik")


fk_hist <- total_försörjningskvot(befolkning_clean) %>%
  mutate(scenario = "Historik")

fk_bas <- total_försörjningskvot(Bas) %>%
  mutate(scenario = "Bas")

fk_high <- total_försörjningskvot(Hög_migration) %>%
  mutate(scenario = "Hög migration")

fk_low <- total_försörjningskvot(Låg_fertilitet) %>%
  mutate(scenario = "Låg fertilitet")

fk_stress <- total_försörjningskvot(Stress) %>%
  mutate(scenario = "Stress")

policy_total_all <- bind_rows(
  fk_hist,
  fk_bas,
  fk_high,
  fk_low,
  fk_stress
)


### Total försörjningskvot ###


p <- ggplot(policy_total_all, aes(x = år_num, 
                                  y = total_ratio, 
                                  color = scenario,
                                  text = paste0(
                                    "År: ", år_num, "<br>",
                                    "Procent: ", scales::percent(total_ratio, accuracy = 0.1), "<br>",
                                    "Scenario: ", scenario
                                  )
)) +
  
  # HISTORIK
  geom_line(
    data = subset(policy_total_all, scenario == "Historik"),
    aes(color = scenario, group = scenario),
    linewidth = 1.3
  ) +
  geom_line(
    data = subset(policy_total_all, scenario != "Historik"),
    aes(color = scenario, group = scenario),
    linewidth = 0.7,
    alpha = 0.85
  ) + 
  
  geom_vline(xintercept = 2024, 
             linetype = "dotted", 
             color = "grey40", 
             linewidth = 0.8) + annotate("text", 
                                         x = 2024.3, 
                                         y = Inf, 
                                         label = "Framskrivning", 
                                         hjust = 0, vjust = 1.5, 
                                         size = 3, color = "grey40") +
  
  scale_color_manual(values = c(
    "Historik" = "black",
    "Bas" = "#2C3E50",
    "Hög migration" = "#1B9E77",
    "Låg fertilitet" = "#D95F02",
    "Stress" = "#7570B3"
  )) +
  
  # AXELINSTÄLLNINGAR
  scale_x_continuous(
    limits = c(2000, (2024 + Framskrivning_antalår + 1)),
    breaks = seq(2000, (2024 + Framskrivning_antalår + 1), by = 5), 
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1), 
    limits = c(0, 1), 
    expand = c(0, 0)
  ) +
  
  labs(
    x = "År",
    y = "",
    title = "Total försörjningskvot"
  ) +
  
  theme_minimal(base_size = 12) +
  
  
  theme(
    # AXAR (SCB-stil)
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    
    # GRID
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    
    # LEGEND
    legend.position = "none"
  )

## Interaktiv plot ##

ggplotly(p, tooltip = "text") %>%
  style(hoverlabel = list(bgcolor = "white"))




### Äldreförsörjningskvot ###

policy_indicators_all <- bind_rows(
  försörjningsindikatorer(Bas, "Bas"),
  försörjningsindikatorer(Hög_migration, "Hög migration"),
  försörjningsindikatorer(Låg_fertilitet, "Låg fertilitet"),
  försörjningsindikatorer(Stress, "Stress")
)


# Lägg till historik
äldre_hist <- äldrekvot(befolkning_clean) %>%
  mutate(scenario = "Historik")

# Slå ihop med scenariedata
policy_äldre_all <- bind_rows(
  äldre_hist,
  policy_indicators_all %>% select(år_num, äldre_ratio, scenario)
)

# Diagram
p_äldre <- ggplot(policy_äldre_all, aes(
  x = år_num,
  y = äldre_ratio,
  color = scenario,
  group = scenario,
  text = paste0(
    "År: ", år_num, "<br>",
    "Procent: ", scales::percent(äldre_ratio, accuracy = 0.1), "<br>",
    "Scenario: ", scenario
  )
)) +
  geom_line(
    data = subset(policy_äldre_all, scenario == "Historik"),
    linewidth = 1.3
  ) +
  geom_line(
    data = subset(policy_äldre_all, scenario != "Historik"),
    linewidth = 0.7,
    alpha = 0.85
  ) +
  geom_vline(
    xintercept = 2024,
    linetype = "dotted",
    color = "grey40",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = 2024.3,
    y = Inf,
    label = "Framskrivning",
    hjust = 0, vjust = 1.5,
    size = 3, color = "grey40"
  ) +
  scale_color_manual(values = c(
    "Historik" = "black",
    "Bas" = "#2C3E50",
    "Hög migration" = "#1B9E77",
    "Låg fertilitet" = "#D95F02",
    "Stress" = "#7570B3"
  )) +
  scale_x_continuous(
    limits = c(2000, (2024 + Framskrivning_antalår + 1)),
    breaks = seq(2000, (2024 + Framskrivning_antalår + 1), by = 5), 
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1), 
    expand = c(0, 0)
  ) +
  labs(
    x = "År",
    y = "",
    title = "Äldreförsörjningskvot"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggplotly(p_äldre, tooltip = "text") %>%
  style(hoverlabel = list(bgcolor = "white"))


### Ungdomsförsörjningskvot ###

ung_all <- bind_rows(
  ungdomsförsörjningskvot(Bas) %>% mutate(scenario = "Bas"),
  ungdomsförsörjningskvot(Hög_migration) %>% mutate(scenario = "Hög migration"),
  ungdomsförsörjningskvot(Låg_fertilitet) %>% mutate(scenario = "Låg fertilitet"),
  ungdomsförsörjningskvot(Stress) %>% mutate(scenario = "Stress")
)


# Lägg till historik
ung_hist <- ungdomsförsörjningskvot(befolkning_clean) %>%
  mutate(scenario = "Historik")

# Slå ihop med scenariedata
policy_ung_all <- bind_rows(
  ung_hist,
  ung_all
)

# Diagram
p_ungdom <- ggplot(policy_ung_all, aes(
  x = år_num,
  y = ung_ratio,
  color = scenario,
  group = scenario,
  text = paste0(
    "År: ", år_num, "<br>",
    "Procent: ", scales::percent(ung_ratio, accuracy = 0.1), "<br>",
    "Scenario: ", scenario
  )
)) +
  geom_line(
    data = subset(policy_ung_all, scenario == "Historik"),
    linewidth = 1.3
  ) +
  geom_line(
    data = subset(policy_ung_all, scenario != "Historik"),
    linewidth = 0.7,
    alpha = 0.85
  ) +
  geom_vline(
    xintercept = 2024,
    linetype = "dotted",
    color = "grey40",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = 2024.3,
    y = Inf,
    label = "Framskrivning",
    hjust = 0, vjust = 1.5,
    size = 3, color = "grey40"
  ) +
  scale_color_manual(values = c(
    "Historik" = "black",
    "Bas" = "#2C3E50",
    "Hög migration" = "#1B9E77",
    "Låg fertilitet" = "#D95F02",
    "Stress" = "#7570B3"
  )) +
  scale_x_continuous(
    limits = c(2000, (2024 + Framskrivning_antalår + 1)),
    breaks = seq(2000, (2024 + Framskrivning_antalår + 1), by = 5, 
    ), 
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1), 
    expand = c(0, 0)
  ) +
  labs(
    x = "År",
    y = "",
    title = "Ungdomsförsörjningskvot"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggplotly(p_ungdom, tooltip = "text") %>%
  style(hoverlabel = list(bgcolor = "white"))

### Befolkningspyramid ###

# Scenarios att lägga in i pyramid_data är

# Bas
# Hög_migration
# Låg_fertilitet
# Stress


pyramid_data <- Bas %>%
  filter(år_num %in% c(2024, (2024 + Framskrivning_antalår))) %>%
  filter(ålder_num <= 100) %>%
  mutate(
    kön = ifelse(kön == "män", "Män", "Kvinnor"),
    Antal_plot = ifelse(kön == "Män", -Antal, Antal),
    år_num = factor(år_num)
  )

p_pyramid <- ggplot(pyramid_data, aes(
  x = ålder_num,
  y = Antal_plot / 1000,
  fill = kön,
  text = paste0(
    "Ålder: ", ålder_num, "<br>",
    "Kön: ", kön, "<br>",
    "Antal: ", format(round(abs(Antal_plot)), big.mark = " ")
  )
)) +
  geom_bar(stat = "identity", width = 1) +
  facet_wrap(~år_num) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) abs(x),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 10),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = c(
    "Män" = "#2C3E50",
    "Kvinnor" = "#D95F02"
  )) +
  labs(
    x = "Ålder",
    y = "Antal (tusental)",
    fill = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold")
  )

ggplotly(p_pyramid, tooltip = "text") %>%
  style(hoverlabel = list(bgcolor = "white"))


# Tabell

slutår <- 2024 + Framskrivning_antalår

bas_structure_10y <- Bas %>%
  group_by(år_num) %>%
  summarise(
    `0–9`   = sum(Antal[ålder_num >= 0  & ålder_num <= 9], na.rm = TRUE),
    `10–19` = sum(Antal[ålder_num >= 10 & ålder_num <= 19], na.rm = TRUE),
    `20–29` = sum(Antal[ålder_num >= 20 & ålder_num <= 29], na.rm = TRUE),
    `30–39` = sum(Antal[ålder_num >= 30 & ålder_num <= 39], na.rm = TRUE),
    `40–49` = sum(Antal[ålder_num >= 40 & ålder_num <= 49], na.rm = TRUE),
    `50–59` = sum(Antal[ålder_num >= 50 & ålder_num <= 59], na.rm = TRUE),
    `60–69` = sum(Antal[ålder_num >= 60 & ålder_num <= 69], na.rm = TRUE),
    `70–79` = sum(Antal[ålder_num >= 70 & ålder_num <= 79], na.rm = TRUE),
    `80+`   = sum(Antal[ålder_num >= 80], na.rm = TRUE),
    total   = sum(Antal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(`0–9`:`80+`, ~ .x / total)
  )

bas_table_10y <- bas_structure_10y %>%
  filter(år_num %in% c(2024, slutår)) %>%
  mutate(År = år_num) %>%
  select(År, `0–9`, `10–19`, `20–29`, `30–39`, `40–49`, `50–59`, `60–69`, `70–79`, `80+`) %>%
  mutate(across(-År, ~ scales::percent(.x, accuracy = 0.1)))




bas_table_10y %>%
  knitr::kable(
    caption = "Befolkningens åldersstruktur 2024 och 2049",
    align = "c"
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE,
    position = "left",
    font_size = 12
  ) %>%
  kableExtra::row_spec(0, bold = TRUE)

bas_table_10y

# p10 och p90

stokastisk_sammanfattning <- tibble(
  Kvottyp = c("Ungdomsförsörjningskvot (0–19 år)", 
              "Äldreförsörjningskvot (65+ år)", 
              "Total försörjningskvot"),
  `Låg nivå (10:e percentilen)` = c("38.3%", "35.1%", "73.9%"),
  `Median (50:e percentilen)`   = c("39.3%", "35.7%", "75.0%"),
  `Hög nivå (90:e percentilen)`  = c("40.3%", "36.3%", "76.1%")
)

# Renderar tabellen
knitr::kable(
  stokastisk_sammanfattning,
  caption = "Tabell 2. Osäkerhetsintervall för försörjningskvoterna år 2044 baserat på 1 000 simulerade framtider",
  align = "lccc"
)
