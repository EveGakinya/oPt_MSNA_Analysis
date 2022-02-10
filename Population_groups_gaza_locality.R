library(readr)
library(dplyr)
dap <- analysisplan
dap <- dap %>% dplyr::select("Indicator.Group...Sector","research.question", "sub.research.question",dependent.variable)
dap <- dap %>% dplyr::rename("dependent.var" = "dependent.variable")
df_1 <- read_csv("Output/raw_results/raw_results_oPt_vulnerability_strata_30012022_conditions_filtered.csv")
df_2 <- read_csv("Output/raw_results/raw_results_oPt_vulnerability_strata_gender_disagg_10022022_conditions_filtered.csv")
df_3 <- read_csv("Output/raw_results/raw_results_oPt_vulnerability_strata_disability_disagg_10022022_conditions_filtered.csv")
df_4 <- read_csv("Output/raw_results/raw_results_oPt_vulnerability_strata_refugee_10022022_conditions_filtered.csv")


results <- rbind(df_1, df_2, df_3,df_4)
results_gov <- results %>% dplyr::select(dependent.var,independent.var.value, numbers,repeat.var,repeat.var.value) %>%
  dplyr::filter(repeat.var.value == "abasan_jadida"| repeat.var.value == "abasan_kabira"| repeat.var.value == "al_bureij"| repeat.var.value == "al_fukhari"|
                  repeat.var.value == "al_maghazi" | repeat.var.value == "al_mughraqa" | repeat.var.value == "al_musaddar" | repeat.var.value == "al_qarara"
                | repeat.var.value == "an_naser" | repeat.var.value == "an_nuseirat" | repeat.var.value == "ash_shoka" | repeat.var.value == "az_zawayda" 
                | repeat.var.value == "bani_suheila"| repeat.var.value == "beit_hanun"| repeat.var.value == "beit_lahiya"| repeat.var.value == "bureij_camp"
                | repeat.var.value == "deir_balah"| repeat.var.value == "deir_balah_camp"| repeat.var.value == "gaza"| repeat.var.value == "jabalya"
                | repeat.var.value == "jabalya_camp"| repeat.var.value == "juhor_deik"| repeat.var.value == "khan_yunis"| repeat.var.value == "khan_yunis_camp"
                | repeat.var.value == "khuza"| repeat.var.value == "madinat_ezahra"| repeat.var.value == "maghazi_camp"| repeat.var.value == "nuseirat_camp"
                | repeat.var.value == "rafah"| repeat.var.value == "rafah_camp"| repeat.var.value == "shati_camp"| repeat.var.value == "umm_naser" | repeat.var.value == "wadi_salqa")

df_gov <- merge(x = results_gov, y= dap , by = "dependent.var", all.x = TRUE)


df_gov <- df_gov %>% 
  dplyr::mutate(Percent = round(numbers *100,0))

df_gov$Percent_groups <- case_when(
  df_gov$Percent >= 0 & df_gov$Percent < 10~ "0% - 9%",
  df_gov$Percent >= 10 & df_gov$Percent < 20 ~ "10% - 19%",
  df_gov$Percent >= 20 & df_gov$Percent < 30 ~ "20% - 29%",
  df_gov$Percent >= 30 & df_gov$Percent < 40 ~ "30% - 39%",
  df_gov$Percent >= 40 & df_gov$Percent < 50 ~ "40% - 49%",
  df_gov$Percent >= 50 & df_gov$Percent < 60 ~ "50% - 59%",
  df_gov$Percent >= 60 & df_gov$Percent < 70 ~ "60% - 69%",
  df_gov$Percent >= 70 & df_gov$Percent < 80 ~ "70% - 79%",
  df_gov$Percent >= 80 & df_gov$Percent < 90 ~ "80% - 89%",
  df_gov$Percent >= 90 & df_gov$Percent <= 100 ~ "90% - 100%")



df_gov[df_gov == "WASH Vulnerabilities"] <- "Water, Sanitation & Hygiene Vulnerability"

df_gov[df_gov == "Health Vulnerabilities"] <- "Health Vulnerability"

df_gov[df_gov == "Protection Vulnerabilities"] <- "Protection Vulnerability"



df_gov <- df_gov %>% dplyr::rename("Indicator Group / Sector" = "Indicator.Group...Sector")

# df_gov <- df_gov %>% dplyr::filter(`Indicator Group / Sector` != "NA")

df_gov <- df_gov %>% dplyr::rename("Vulnerability Dimension" = "Sector")

# df_gov <- df_gov %>% dplyr::rename("Indicator" = "research.question")

df_gov <- df_gov %>% dplyr::rename("Indicator" = "sub.research.question")

library(readxl)
Population_groups <- read_excel("Output/raw_results/dap_population_groups.xlsx")


Population_groups <- Population_groups %>% dplyr::rename("dependent.var" = "dependent.variable")

Population_groups<- Population_groups %>%  dplyr::select("dependent.var","Population Reference")

df_gov_new <- merge(x = df_gov, y= Population_groups , by = "dependent.var", all.x = TRUE)


df_gov_new <- df_gov_new %>% 
  dplyr::mutate(repeat.var.value = ifelse(repeat.var.value == "abasan_jadida", "'Abasan al Jadida (as Saghira)", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "abasan_kabira", "'Abasan al Kabira", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "al_bureij", "Al Bureij", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "al_fukhari", "Al Fukhkhari", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "al_maghazi", "Al Maghazi", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "al_mughraqa", "Al Mughraqa", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "al_musaddar", "Al Musaddar", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "al_qarara", "Al Qarara", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "an_naser", "An Naser", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "an_nuseirat", "An Nuseirat", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "ash_shoka", "Shokat as Sufi", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "az_zawayda", "Az Zawayda", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "bani_suheila", "Bani Suheila", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "beit_hanun", "Beit Hanoun", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "beit_lahiya", "Beit Lahiya", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "bureij_camp", "Al Bureij Camp", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "deir_balah", "Deir al Balah", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "deir_balah_camp", "Deir al Balah Camp", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "gaza", "Gaza", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "jabalya", "Jabalya", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "jabalya_camp", "Jabalya Camp", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "juhor_deik", "Juhor ad Dik", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "khan_yunis", "Khan Yunis", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "khan_yunis_camp", "Khan Yunis Camp", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "khuza", "Khuza'a", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "madinat_ezahra", "Madinat Ezahra", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "maghazi_camp", "Al Maghazi Camp", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "nuseirat_camp", "An Nuseirat Camp", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "rafah", "Rafah", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "rafah_camp", "Rafah Camp", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "shati_camp", "Ash Shati' Camp", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "umm_naser", "Umm an Naser", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "wadi_salqa", "Wadi as Salqa", repeat.var.value)
  )
                
df_gov_new <- df_gov_new %>% dplyr::rename("Area" = "repeat.var.value")

df_gov_new <- df_gov_new %>% dplyr::filter(`Vulnerability Dimension` != "NA")

df_gov_new <- df_gov_new %>% 
  dplyr::mutate(independent.var.value = ifelse(independent.var.value == "male_headed", "Male headed households", independent.var.value),
                independent.var.value = ifelse(independent.var.value == "female_headed", "Female headed households", independent.var.value),
                independent.var.value = ifelse(independent.var.value == "with_disability", "Households with disability", independent.var.value),
                independent.var.value = ifelse(independent.var.value == "without_disability", "Households without disability", independent.var.value),
                independent.var.value = ifelse(independent.var.value == "non_refugee", "Non Refugee households", independent.var.value),
                independent.var.value = ifelse(independent.var.value == "out_camp_refugee", "Out-of Camp Refugee households", independent.var.value),
                independent.var.value = ifelse(independent.var.value == "in_camp_refugee", "In Camp Refugee households", independent.var.value))
                
df_gov_new<-df_gov_new %>% 
  dplyr::mutate(independent.var.value = case_when(is.na(independent.var.value) ~ "All households",
                 TRUE ~ independent.var.value))


name <- "locality_population_groups_vul_analysis"
write.csv(df_gov_new, sprintf("output/raw_results/Results_%s_filtered.csv", name), row.names=F)

                