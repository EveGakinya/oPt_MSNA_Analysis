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

response <- response %>% drop_na(strata)

if(any(is.na(response$strata))){
  warning("strata can not be NA")
}


#IDENTIFY ANY FURTHER PROBLEMS WITH THE SAMPLING FRAMES MATCHING
strata_samplingframe_issues <- as.data.frame(response[which(!response$strata %in% samplingframe$stratum), c("X_uuid", "strata")])
if(nrow(strata_samplingframe_issues)!=0){
  print(strata_samplingframe_issues)
  warning("something's not right with the strata id matching!")
}


#STRATA WEIGHTING
#strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe,
#                                      sampling.frame.population.column = "population",
#                                      sampling.frame.stratum.column = "stratum",
#                                      data.stratum.column = "strata",
#                                      data = response)

# weight_fun <- combine_weighting_functions(strata_weight_fun, clusters_weight_fun)
#weight_fun <-strata_weight_fun

#response$weights<- weight_fun(response)
