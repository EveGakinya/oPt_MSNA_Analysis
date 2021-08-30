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
response_with_composites <- recoding_hno(response, loop)

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
dap_name <- "oPt_hno"
analysisplan <- read.csv(sprintf("input/dap/dap_%s.csv",dap_name), stringsAsFactors = F, sep = ";")
response_with_composites$one <- "one"

#Disaggregation Refugee Status
response_with_composites$refugee_disagg <- ifelse(response_with_composites$refugee_status == "yes", "refugee", 
                                                  "non_refugee")
analysisplan$independent.variable <- "refugee_disagg"
#Disaggregation by region (WB, Gaza, EJ, H2)
analysisplan$repeat.for.variable <- "region"


#AGGREGATE ACROSS DISTRICTS OR/AND POPULATION GROUPS
#analysisplan <- analysisplan_nationwide(analysisplan)
analysisplan <- analysisplan_pop_group_aggregated(analysisplan)
#analysisplan$hypothesis.type <- "group_difference"


result <- from_analysisplan_map_to_output(response_with_composites, analysisplan = analysisplan,
                                          weighting = weight_fun,
                                          questionnaire = questionnaire, confidence_level = 0.95)


name <- "oPt_hno_strata_refugee_disagg"
saveRDS(result,paste(sprintf("output/RDS/result_%s.RDS", name)))
#summary[which(summary$dependent.var == "g51a"),]

summary <- bind_rows(lapply(result[[1]], function(x){x$summary.statistic}))
write.csv(summary, sprintf("output/raw_results/raw_results_%s.csv", name), row.names=F)
summary <- read.csv(sprintf("output/raw_results/raw_results_%s.csv", name), stringsAsFactors = F)
summary <- correct.zeroes(summary)
summary <- summary %>% filter(dependent.var.value %in% c(NA,1))
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



