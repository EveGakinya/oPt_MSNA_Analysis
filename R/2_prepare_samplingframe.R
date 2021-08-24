#PREPARE SAMPLINGFRAMES

##CLUSTER SAMPLING FRAME: MAKE VALUES THAT CAN BE MATCHED WITH DATA
samplingframe$stratum <- to_alphanumeric_lowercase(samplingframe$stratum)

#ADD STRATA NAMES TO DATA 
response$strata <- to_alphanumeric_lowercase(response$strata)
response$strata <- gsub('tulkarm_c', 'tulkarem_c', response$strata)
table(response$strata)
table(samplingframe$stratum)

##CHECK IF ALL MATCH SAMPLINGFRAME:
`%find those not in%`<-function(x,y){x[!(x%in%y)] %>% unique}

if(any(!(response$strata %in% samplingframe$stratum))){
  warning("some strata not found in samplingframe")
  warning(which(!(response$strata %in% samplingframe$stratum)) %>% length)
}
response$strata %find those not in% samplingframe$stratum


if(any(is.na(response$strata))){
  warning("strata can not be NA")
}

# MERGE QUESTIONNAIRES
questionnaire <- load_questionnaire(response,questions,choices, choices.label.column.to.use = "name")
