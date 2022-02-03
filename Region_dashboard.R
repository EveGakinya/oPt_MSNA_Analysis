library(readr)
library(dplyr)
dap <- analysisplan
dap <- dap %>% dplyr::select("Indicator.Group...Sector","research.question", "sub.research.question",dependent.variable)
dap <- dap %>% dplyr::rename("dependent.var" = "dependent.variable")

results <- read_csv("Output/raw_results/raw_results_oPt_vulnerability_region_25012022_conditions.csv")
results <- results %>% dplyr::select(dependent.var, numbers,repeat.var,repeat.var.value) 
df_west <- merge(x = results, y= dap , by = "dependent.var", all.x = TRUE)

df_west <- df_west %>% 
  dplyr::mutate(Percent = round(numbers *100,0))

df_west$Percent_groups <- case_when(
  df_west$Percent >= 0 & df_west$Percent < 10~ "0% - 9%",
  df_west$Percent >= 10 & df_west$Percent < 20 ~ "10% - 19%",
  df_west$Percent >= 20 & df_west$Percent < 30 ~ "20% - 29%",
  df_west$Percent >= 30 & df_west$Percent < 40 ~ "30% - 39%",
  df_west$Percent >= 40 & df_west$Percent < 50 ~ "40% - 49%",
  df_west$Percent >= 50 & df_west$Percent < 60 ~ "50% - 59%",
  df_west$Percent >= 60 & df_west$Percent < 70 ~ "60% - 69%",
  df_west$Percent >= 70 & df_west$Percent < 80 ~ "70% - 79%",
  df_west$Percent >= 80 & df_west$Percent < 90 ~ "80% - 89%",
  df_west$Percent >= 90 & df_west$Percent <= 100 ~ "90% - 100%")



df_west[df_west == "WASH Vulnerabilities"] <- "Water, Sanitation & Hygiene Vulnerability"

df_west[df_west == "Health Vulnerabilities"] <- "Health Vulnerability"

df_west[df_west == "Protection Vulnerabilities"] <- "Protection Vulnerability"



df_west <- df_west %>% dplyr::rename("Indicator Group / Sector" = "Indicator.Group...Sector")

# df_strata <- df_strata %>% dplyr::filter(`Indicator Group / Sector` != "NA")

df_west <- df_west %>% dplyr::rename("Sector" = "Indicator Group / Sector")

# df_strata <- df_strata %>% dplyr::rename("Indicator" = "research.question")

df_west <- df_west %>% dplyr::rename("Indicator" = "sub.research.question")

library(readxl)
Population_groups <- read_excel("Output/raw_results/dap_population_groups.xlsx")


Population_groups <- Population_groups %>% dplyr::rename("dependent.var" = "dependent.variable")

Population_groups<- Population_groups %>%  dplyr::select("dependent.var","Population Reference")

df_west_new <- merge(x = df_west, y= Population_groups , by = "dependent.var", all.x = TRUE)




df_west_new <- df_west_new %>% 
  dplyr::rename("Region" = "repeat.var.value")



df_west_new <- df_west_new %>% dplyr::filter(`Sector` != "NA")

name <- "Region_vul_analysis"
write.csv(df_west_new, sprintf("output/raw_results/Results_%s_filtered.csv", name), row.names=F)

