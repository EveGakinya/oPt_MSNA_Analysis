library(readr)
library(dplyr)
#dap <- dap_oPt_preliminary
#dap <- dap %>% dplyr::select("Indicator Group / Sector","research.question", "sub.research.question",dependent.variable)
#dap <- dap %>% dplyr::rename("dependent.var" = "dependent.variable")

#dap <- dap %>% group_by(dependent.variable) %>% 
        #summarise(n()) 

## working with the tableau dap sent by michael

dap <- analysisplan
dap <- dap %>% dplyr::select("Indicator.Group...Sector","research.question", "sub.research.question",dependent.variable)
dap <- dap %>% dplyr::rename("dependent.var" = "dependent.variable")

results <- read_csv("Output/raw_results/raw_results_oPt_strata_analysis_msna_filtered.csv")
results <- results %>% select(dependent.var, numbers,repeat.var,repeat.var.value) %>% 
            filter(repeat.var.value == "Area_AB"| repeat.var.value == "bethlehem_c"| repeat.var.value == "hebron_c"| repeat.var.value =="jenin_c" |
                      repeat.var.value == "jericho_c" | repeat.var.value == "jerusalem_2_c" | repeat.var.value == "nablus_c" | repeat.var.value == "qalqiliya_c"
                   | repeat.var.value == "ramallah_c" | repeat.var.value == "salfit_c" | repeat.var.value == "tubas_c" | repeat.var.value == "tulkarm_c" | repeat.var.value == "H2")

summary_gov <- summary_gov %>% select(dependent.var, numbers,repeat.var,repeat.var.value)

df <- merge(x = summary_gov, y= dap , by = "dependent.var", all.x = TRUE)

df_strata <- merge(x = results, y= dap , by = "dependent.var", all.x = TRUE)

df_strata <- df_strata %>% 
              dplyr::mutate(Percent = round(numbers *100,2))

df2<-df1[!(df1$Name=="George" | df1$Name=="Andrea"),]

df_strata <- df_strata[!(df_strata$dependent.var == "s2"),]


df_strata$Percent_groups <- case_when(
  df_strata$Percent >= 0 & df_strata$Percent < 10~ "0% - 9%",
  df_strata$Percent >= 10 & df_strata$Percent < 20 ~ "10% - 19%",
  df_strata$Percent >= 20 & df_strata$Percent < 30 ~ "20% - 29%",
  df_strata$Percent >= 30 & df_strata$Percent < 40 ~ "30% - 39%",
  df_strata$Percent >= 40 & df_strata$Percent < 50 ~ "40% - 49%",
  df_strata$Percent >= 50 & df_strata$Percent < 60 ~ "50% - 59%",
  df_strata$Percent >= 60 & df_strata$Percent < 70 ~ "60% - 69%",
  df_strata$Percent >= 70 & df_strata$Percent < 80 ~ "70% - 79%",
  df_strata$Percent >= 80 & df_strata$Percent < 90 ~ "80% - 89%",
  df_strata$Percent >= 90 & df_strata$Percent <= 100 ~ "90% - 100%")

#replacing "AAP" with Accountability to Affected Populations (AAP)
df_strata[df_strata == "AAP"] <- "Accountability to Affected Populations (AAP)"

#replacing "HH Displacement" with Households Displacement

df_strata[df_strata == "HH Displacement"] <- "Households Displacement"

#replacing "HH Profile" with Households Profile

df_strata[df_strata == "HH Profile"] <- "Households Profile"

#replacing WASH with Water, Sanitation & Hygiene

df_strata[df_strata == "WASH"] <- "Water, Sanitation & Hygiene"

df_strata <- df_strata %>% dplyr::rename("Indicator Group / Sector" = "Indicator.Group...Sector")

df_strata <- df_strata %>% dplyr::filter(`Indicator Group / Sector` != "NA")

df_strata <- df_strata %>% dplyr::rename("Sector" = "Indicator Group / Sector")

df_strata <- df_strata %>% dplyr::rename("Indicator" = "research.question")

df_strata <- df_strata %>% dplyr::rename("Sub-Indicator" = "sub.research.question")

library(readxl)
Population_groups <- read_excel("Output/raw_results/Population_groups.xlsx")

Population_groups <- Population_groups %>% dplyr::rename("dependent.var" = "Indicator Code")
                      
Population_groups<- Population_groups %>%  select("dependent.var","Population Reference")

df_strata_new <- merge(x = df_strata, y= Population_groups , by = "dependent.var", all.x = TRUE)


df_strata_new <- df_strata_new %>% 
  mutate(repeat.var.value = ifelse(repeat.var.value == "qalqiliya_c", "Qalqilya Area C", repeat.var.value),
         repeat.var.value = ifelse(repeat.var.value == "jerusalem_2_c", "Jerusalem_2 Area C", repeat.var.value),
         repeat.var.value = ifelse(repeat.var.value == "bethlehem_c", "Bethlehem Area C", repeat.var.value),
         repeat.var.value = ifelse(repeat.var.value == "hebron_c", "Hebron Area C", repeat.var.value),
         repeat.var.value = ifelse(repeat.var.value == "jenin_c", "Jenin Area C", repeat.var.value),
         repeat.var.value = ifelse(repeat.var.value == "jericho_c", "Jericho Area C", repeat.var.value),
         repeat.var.value = ifelse(repeat.var.value == "nablus_c", "Nablus Area C", repeat.var.value),
         repeat.var.value = ifelse(repeat.var.value == "ramallah_c", "Ramallah Area C", repeat.var.value),
         repeat.var.value = ifelse(repeat.var.value == "salfit_c", "Salfit Area C", repeat.var.value),
         repeat.var.value = ifelse(repeat.var.value == "tubas_c", "Tubas Area C", repeat.var.value),
         repeat.var.value = ifelse(repeat.var.value == "tulkarm_c", "Tulkarm Area C", repeat.var.value)
  )

                  
name <- "westbank_strata_analysis"
write.csv(df_strata_new, sprintf("output/raw_results/Results_%s_filtered.csv", name), row.names=F)

name <- "westbank_gov_analysis"
write.csv(df, sprintf("output/raw_results/df_results_%s_filtered.csv", name), row.names=F)



gov_WB <- st_read("~/2. Data cleaning/MSNA_Data_Cleaning_Opt/shapefile_out.shp") %>% st_transform(crs = 4326) %>%
 
  mutate(strata = ifelse(strata == "qalqiliya_c", "Qalqilya Area C", strata),
         strata = ifelse(strata == "jerusalem_2_c", "Jerusalem_2 Area C", strata),
         strata = ifelse(strata == "bethlehem_c", "Bethlehem Area C", strata),
         strata = ifelse(strata == "hebron_c", "Hebron Area C", strata),
         strata = ifelse(strata == "jenin_c", "Jenin Area C", strata),
         strata = ifelse(strata == "jericho_c", "Jericho Area C", strata),
         strata = ifelse(strata == "nablus_c", "Nablus Area C", strata),
         strata = ifelse(strata == "ramallah_c", "Ramallah Area C", strata),
         strata = ifelse(strata == "salfit_c", "Salfit Area C", strata),
         strata = ifelse(strata == "tubas_c", "Tubas Area C", strata),
         strata = ifelse(strata == "tulkarm_c", "Tulkarm Area C", strata)
         )

st_write(gov_WB, "shapefile_out.shp", driver="ESRI Shapefile")  # create to a shapefile 


tableau_table_strata <- read_csv("Input/dap/tableau_table_strata.csv")


Pop_groups<- Population_groups %>%  select("Indicator Code","Population Reference")%>% 
  rename("dependent.variable" = "Indicator Code")


tableau_table_strata_new <- merge(x = tableau_table_strata, y = Pop_groups, by= c("dependent.variable"), all.x = TRUE)

name <- "tableau_table_strata_new"
write.csv(tableau_table_strata_new, sprintf("output/raw_results/df_results_%s_filtered.csv", name), row.names=F)

