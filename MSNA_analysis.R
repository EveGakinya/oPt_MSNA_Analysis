#SETUP
rm(list=ls(all=T))
R.version
  library(rlang)
  library(xlsx)
  library(plyr) # rbind.fill
  library(dplyr)
  library(expss)
  library(reshape)
  library(data.table)
  library(miceadds)
  library(questionr)
  library(koboquest) # manage kobo questionnairs
  library(kobostandards) # check inputs for inconsistencies
  library(xlsformfill) # generate fake data for kobo
  library(surveyweights) # calculate weights from samplingframes
  library(hypegrammaR) # simple stats 4 complex samples
  library(composr) # horziontal operations

  
  source("R/functions/postprocessing_functions.R")
  source("R/functions/to_alphanumeric_lowercase.R")
  source("R/functions/analysisplan_factory.R")
  source("R/functions/HNO_Recoding.R")
  source("R/functions/Binary_Recoding.R")
  source("R/functions/MSNI_Recoding.R")
  source("R/functions/HNO_Recoding_hum_condition.R")
  source("R/functions/Population_groups_Recoding.R")
  #source("R/functions/Additional_indicators.R")

  #source("R/functions/presentation_recoding.R")
  #source("R/functions/gimac_recoding.R")

#LOAD INPUT FILES 
  source("R/1_load_inputs.R",local = T)
#  names(response)[names(response) == 'ï..X_uuid'] <- "X_uuid"
  #' creates objects:
  #' 
  #'    response representative clean
  #'    response indicative clean
  #'    analysisplan
  #'    choices
  #'    questions
  #'    cluster_lookup_table
  #'    loop
  #'    samplingframe
  #'    samplingframe_in_camp

  
#PREPARE SAMPLING FRAMES AND STRATAS
  source("R/2_prepare_samplingframe.R", local = T)
  #' Prepare sampling frames and Strata names:
  #'     3.1 prepare columns in out of camp cluster level sampling frame
  #'     3.2 aggregate out-of-camp to stratum level
  #'     3.3.make strata id for in-camp sampling frame
  #'     3.4.combine the stratum sampling frames
  #'     3.5.add strata ids to the dataset
  #'     3.6. throw error if any don't match

  

#CREATE NEW FUNCTION FOR WEIGHTING
#Gov level aggregation
response <- response %>% drop_na(weights)
response$weights <- ifelse(response$strata == "camps_wb", 1, 
                           response$weights)

 weight_fun<-function(df){
   df$weights
 }
  

#RECODING OF INDICATORS
response <- recoding_pop_groups(response)

response_with_composites <- recoding_preliminary(response, loop)

response_with_composites <- recoding_hno_hum_conditions(response, loop)


#DISAGGREGATE MALE AND FEMALE HEADED HHs
#female_headed <- response_with_composites[which(response_with_composites$X_uuid %in% loop$X_uuid[which(loop$sex == "female" & loop$relationship == "head")]),]
#male_headed <- response_with_composites[which(response_with_composites$X_uuid %in% loop$X_uuid[which(loop$sex == "male" & loop$relationship == "head")]),]
#DISAGGREGATED HH WITH DISABILITY AND THOSE THAT DON'T
#response_with_composites <- count_difficulty_level(response_with_composites)
#response_with_composites_disab <- subset(response_with_composites, response_with_composites$lot_diff > 0 | 
#                                          response_with_composites$cannot_diff > 0)
#response_with_composites_nodisab <- subset(response_with_composites, response_with_composites$lot_diff == 0 & 
#                                          response_with_composites$cannot_diff == 0)


#LOAD ANALYSISPLAN
#dap_name <- "oPt_hno_hum_conditions"
dap_name <- "oPt_preliminary"
# analysisplan <- read.csv(sprintf("input/dap/dap_%s.csv",dap_name), stringsAsFactors = F)
analysisplan <- read.csv(sprintf("input/dap/dap_%s.csv",dap_name), stringsAsFactors = F, sep = ";")
# response_with_composites$male_headed <-  case_when(response_with_composites$gender_disagg == "male_headed" ~ "male_headed",
#                             TRUE ~ NA_character_)
# response_with_composites$female_headed <-  case_when(response_with_composites$gender_disagg == "female_headed" ~ "female_headed",
#                           TRUE ~ NA_character_)
# 
# response_with_composites$refugees <- case_when(response_with_composites$refugee_disagg == "refugee" ~ "refugee", 
#                                                TRUE ~ NA_character_)
# response_with_composites$non_refugees <- case_when(response_with_composites$refugee_disagg == "non_refugee" ~ "non_refugee", 
#                                                TRUE ~ NA_character_)
# response_with_composites$in_camp_refugee <- case_when(response_with_composites$camp_refugee_disagg == "in_camp_refugee"~ "in_camp_refugee",
#                                                       TRUE ~ NA_character_)
# response_with_composites$out_camp_refugee <- case_when(response_with_composites$camp_refugee_disagg == "out_camp_refugee"~"out_camp_refugee",
#                                                       TRUE ~ NA_character_)
# analysisplan$independent.variable <-  "female_headed"
# analysisplan$independent.variable <-  "non_refugees"
# analysisplan$independent.variable <-  "one"
response_with_composites$REGION <- case_when(response_with_composites$region == "gaza" ~ "Gaza", TRUE~ "West Bank")

analysisplan$repeat.for.variable <- "hno_strata"
analysisplan$independent.variable.type <- ""


#AGGREGATE ACROSS DISTRICTS OR/AND POPULATION GROUPS
analysisplan <- analysisplan_nationwide(analysisplan)
analysisplan <- analysisplan_pop_group_aggregated(analysisplan)
#analysisplan$hypothesis.type <- "group_difference"
response_with_composites$cluster_id <- ifelse(response_with_composites$region == "ej" |
                                                response_with_composites$region == "west_bank", 
                                                      response_with_composites$locality_code, 
                                              response_with_composites$X_uuid)
response_with_composites$l7_iii <- NULL

result <- from_analysisplan_map_to_output(response_with_composites, analysisplan = analysisplan,
                                          weighting = weight_fun, cluster_variable_name = "cluster_id",
                                          questionnaire = questionnaire, confidence_level = 0.95)


# name <- "oPt_hno_conditions_msna_hno_strata_conditions"

name <- "oPt_preliminary_hno_strata"
saveRDS(result,paste(sprintf("output/RDS/result_%s.RDS", name)))
#summary[which(summary$dependent.var == "g51a"),]
response_with_composites$strata <- ifelse(response_with_composites$strata == "area_a_b", "area_ab", 
                                          response_with_composites$strata)
summary <- bind_rows(lapply(result[[1]], function(x){x$summary.statistic}))
write.csv(summary, sprintf("output/raw_results/raw_results_%s.csv", name), row.names=F)
summary <- read.csv(sprintf("output/raw_results/raw_results_%s.csv", name), stringsAsFactors = F)
summary <- correct.zeroes(summary)
summary <- summary %>% filter(dependent.var.value %in% c(NA,1))
#summary$max <- ifelse(summary$max > 1, 1, summary$max)
#summary$min <- ifelse(summary$min < 0, 0, summary$min)

write.csv(summary, sprintf("output/raw_results/raw_results_%s_filtered.csv", name), row.names=F)
if(all(is.na(summary$independent.var.value))){summary$independent.var.value <- "all"}
groups <- unique(summary$independent.var.value)
groups <- groups[!is.na(groups)]
library(plyr)
for (i in 1:length(groups)) {
  df <- pretty.output(summary, groups[i], analysisplan, cluster_lookup_table, lookup_table, severity = name == "severity", camp = F)
  write.csv(df, sprintf("output/summary_sorted/summary_sorted_%s_%s.csv", name, groups[i]), row.names = F)
  if(i == 1){
    write.xlsx(df, file=sprintf("output/summary_sorted/summary_sorted_%s.xlsx", name), sheetName=groups[i], row.names=FALSE)
  } else {
    write.xlsx(df, file=sprintf("output/summary_sorted/summary_sorted_%s.xlsx", name), sheetName=groups[i], append=TRUE, row.names=FALSE)
  }
}
summary_gov<- summary
#summary_refugee <- summary
#summary_female <- summary
 #summary_male <- summary

df2 <- rbind(summary_female, summary_male, summary_refugee, summary_non_refugee, summary_in_camp_refugee, summary_out_camp_refugee, summary_governorate)
write.csv(df, sprintf("output/raw_results/df_results_%s_filtered.csv", name), row.names=F)
response$gaggi <- ifelse(response$remote_learning > response$tot_school_aged, 1,0)
response$tot_school_aged <- response$school_girls_5_10 + response$school_girls_11_15 + 
  response$school_girls_16_17 + response$school_boys_5_10 + response$school_boys_11_15 +
  response$school_boys_16_17
table(response$gaggi)

