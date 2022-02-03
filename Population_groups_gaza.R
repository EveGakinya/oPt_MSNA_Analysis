library(readr)
library(dplyr)
dap <- analysisplan
dap <- dap %>% dplyr::select("Indicator.Group...Sector","research.question", "sub.research.question",dependent.variable)
dap <- dap %>% dplyr::rename("dependent.var" = "dependent.variable")
df_1 <- read_csv("Output/raw_results/raw_results_oPt_vulnerability_hno_strata_disability_disagg_25012022_conditions_filtered.csv")
df_2 <- read_csv("Output/raw_results/raw_results_oPt_vulnerability_hno_strata_gender_disagg_25012022_conditions_filtered.csv")
df_3 <- read_csv("Output/raw_results/raw_results_oPt_vulnerability_hno_strata_camp_refugee_disagg_25012022_conditions_filtered.csv")
df_4 <- read_csv("Output/raw_results/raw_results_oPt_vulnerability_hno_strata_25012022_conditions_filtered.csv")


results <- rbind(df_1, df_2, df_3,df_4)
results_gov <- results %>% dplyr::select(dependent.var, numbers,repeat.var,repeat.var.value) %>%
                dplyr::filter(repeat.var.value == "khan_yunis" | repeat.var.value == "deir_balah" | repeat.var.value == "gaza" |
                                repeat.var.value == "north_gaza" | repeat.var.value == "rafah" )

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

df_gov <- df_gov %>% dplyr::rename("Sector" = "Indicator Group / Sector")

# df_gov <- df_gov %>% dplyr::rename("Indicator" = "research.question")

df_gov <- df_gov %>% dplyr::rename("Indicator" = "sub.research.question")

library(readxl)
Population_groups <- read_excel("Output/raw_results/dap_population_groups.xlsx")


Population_groups <- Population_groups %>% dplyr::rename("dependent.var" = "dependent.variable")

Population_groups<- Population_groups %>%  dplyr::select("dependent.var","Population Reference")

df_gov_new <- merge(x = df_gov, y= Population_groups , by = "dependent.var", all.x = TRUE)


df_gov_new <- df_gov_new %>% 
  dplyr::mutate(repeat.var.value = ifelse(repeat.var.value == "deir_balah", "Deir Al-Balah", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "gaza", "Gaza", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "north_gaza", "North Gaza", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "rafah", "Rafah", repeat.var.value),
                repeat.var.value = ifelse(repeat.var.value == "khan_yunis", "Khan Younis", repeat.var.value))
                
df_gov_new <- df_gov_new %>% dplyr::rename("Area" = "repeat.var.value")

df_gov_new <- df_gov_new %>% dplyr::filter(`Sector` != "NA")

name <- "gaza_population_groups_vul_analysis"
write.csv(df_gov_new, sprintf("output/raw_results/Results_%s_filtered.csv", name), row.names=F)

                