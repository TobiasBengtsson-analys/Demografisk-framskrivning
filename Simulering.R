# Stokastisk modell 

framskrivning_stokastisk <- function(
    befolkning_start,
    antal_år,
    n_sim = 300,
    fert_sdlog = 0.1,
    död_sdlog  = 0.03,
    mig_sd     = 0.4
) {
  
  alla_sim <- list()
  
  for (sim in 1:n_sim) {
    
    resultat <- list()
    resultat[[1]] <- befolkning_start %>%
      mutate(år_num = 2024, simulering = sim)
    
    for (t in 1:antal_år) {
      
      bef_t <- resultat[[t]]
      år <- 2024 + t
      
      # Slumpa faktorer 
      
      fert_factor <- rlnorm(1, meanlog = 0, sdlog = fert_sdlog)
      död_factor  <- rlnorm(1, meanlog = 0, sdlog = död_sdlog)
      mig_factor  <- rlnorm(1, meanlog = 0, sdlog = mig_sd)
      
      
      # Åldra befolkning
      
      bef_ny <- bef_t %>%
        mutate(ålder_num = ålder_num + 1) %>%
        
        inner_join(Dödstal, by = c("ålder_num", "kön")) %>%
        inner_join(nettomigration_medel, by = c("ålder_num", "kön")) %>%
        
        mutate(
          Antal = Antal * (1 - dödstal_medel * död_factor) +
            nettomigration_medel * mig_factor
        ) %>%
        
        # Skydda mot negativa värden
        mutate(Antal = pmax(Antal, 0)) %>%
        
        select(ålder_num, kön, Antal)
      
      
      # Födda
      
      nyfödda_totalt <- bef_ny %>%
        filter(kön == "kvinnor") %>%
        
        inner_join(Fertilitetstal, by = "ålder_num") %>%
        
        summarise(
          nyfödda = sum(Antal * fertilitetstal_medel * fert_factor, na.rm = TRUE)
        ) %>%
        
        pull(nyfödda)
      
      
      # Fördela på kön
      
      nyfödda <- tibble(
        ålder_num = 0,
        kön = c("män", "kvinnor"),
        Antal = c(
          nyfödda_totalt * 0.514,
          nyfödda_totalt * 0.486
        )
      )
      
      
      # Lägg till nyfödda
      
      bef_ny <- bind_rows(bef_ny, nyfödda) %>%
        mutate(år_num = år, simulering = sim)
      
      
      resultat[[t + 1]] <- bef_ny
    }
    
    alla_sim[[sim]] <- bind_rows(resultat)
  }
  
  bind_rows(alla_sim)
}


# Jämför p90 för år 2044

set.seed(5000)
mc_1000 <- framskrivning_stokastisk(befolkning_2024, antal_år = 20, n_sim = 1000)



# 1. Beräkna samtliga tre kvoter för varje enskild simulering och år
kvoter_stokastisk_alla <- mc_1000 %>%
  group_by(år_num, simulering) %>%
  summarise(
    unga      = sum(Antal[ålder_num < 20], na.rm = TRUE),
    äldre     = sum(Antal[ålder_num >= 65], na.rm = TRUE),
    arbetande = sum(Antal[ålder_num >= 20 & ålder_num < 65], na.rm = TRUE),
    
    # Räkna ut de tre separata kvoterna per simulering
    ung_ratio   = unga / arbetande,
    äldre_ratio = äldre / arbetande,
    total_ratio = (unga + äldre) / arbetande,
    .groups = "drop"
  )

# 2. Filtrera på slutåret 2044 och beräkna percentilerna för respektive kvot
kvoter_sammanfattning <- kvoter_stokastisk_alla %>%
  filter(år_num == 2044) %>%
  summarise(
    # Skapa en snygg struktur för de tre raderna
    Kvottyp = c("Ungdomsförsörjningskvot (0–19 år)", 
                "Äldreförsörjningskvot (65+ år)", 
                "Total försörjningskvot"),
    
    P10 = c(quantile(ung_ratio, 0.10), quantile(äldre_ratio, 0.10), quantile(total_ratio, 0.10)),
    Median = c(median(ung_ratio), median(äldre_ratio), median(total_ratio)),
    P90 = c(quantile(ung_ratio, 0.90), quantile(äldre_ratio, 0.90), quantile(total_ratio, 0.90))
  ) %>%
  # Formatera till snygg procent med en decimal (myndighetsstandard)
  mutate(
    across(c(P10, Median, P90), ~ scales::percent(.x, accuracy = 0.1))
  )

print(kvoter_sammanfattning)




