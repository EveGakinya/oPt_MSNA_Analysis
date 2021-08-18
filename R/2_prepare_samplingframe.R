#PREPARE SAMPLINGFRAMES

##CLUSTER SAMPLING FRAME: MAKE VALUES THAT CAN BE MATCHED WITH DATA
samplingframe$stratum <- to_alphanumeric_lowercase(samplingframe$stratum)

#ADD STRATA NAMES TO DATA 
response <-  response %>% mutate(strata = case_when(
    response$location == "H2" ~ "h2",
    response$location== "ej" ~  "ej",
    response$location == "gaza" ~ response$hh_location_gaza,
    response$location == "west_bank" & response$oslo_area == "area_c" 
    ~ paste(hh_location_wb,oslo_area,sep = "_"), 
    response$location == "west_bank" & (response$oslo_area == "area_a" |
    response$oslo_area == "area_b") ~ "area_a_b"
    ))


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
