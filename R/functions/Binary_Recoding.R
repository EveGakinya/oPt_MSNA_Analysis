#update.packages(checkBuilt=TRUE, ask=FALSE)

#r <- read_csv("output/cleaned_data/parent.csv")
#r <- read.csv("hh_dataset.csv", sep = ";")
#loop <- read.csv("ind_dataset.csv", sep = ";")
#r <- read_excel("output/cleaned_data/msna_data_clean_all_2021-08-16.xlsx", sheet = "MSNA_2021_OPT", col_types = "text")
#setwd("~/Documents/WORK/REACH2021/5. Data Analysis/oPt_MSNA_Analysis/Input/datasets/cleaned")
#r <- read.csv("hh_dataset.csv", sep = ";")

#recoding_preliminary <- function(r, loop) {
r <- response

#% HH with at least one individual with a disability
##getting the hh with at least one individual with a disability in the loop
loop$hh_wd <- case_when(loop$difficulty_communicating %in% c("a_lot_of_difficulty","cannot_do_at_all")|
                        loop$difficulty_hearing %in% c("a_lot_of_difficulty","cannot_do_at_all")|
                        loop$difficulty_remembering %in% c("a_lot_of_difficulty","cannot_do_at_all")|
                        loop$difficulty_washing %in% c("a_lot_of_difficulty","cannot_do_at_all")|
                        loop$difficulty_walking %in% c("a_lot_of_difficulty","cannot_do_at_all")|
                        loop$difficulty_seeing %in% c("a_lot_of_difficulty","cannot_do_at_all") ~ 1,
                        loop$difficulty_communicating %in% c("some_difficulty","no_difficulty") &
                          loop$difficulty_hearing %in% c("some_difficulty","no_difficulty") &
                          loop$difficulty_remembering %in% c("some_difficulty","no_difficulty") &
                          loop$difficulty_washing %in% c("some_difficulty","no_difficulty") &
                          loop$difficulty_walking %in% c("some_difficulty","no_difficulty") &
                          loop$difficulty_seeing %in% c("some_difficulty","no_difficulty") ~ 0,
                       TRUE ~ 0)

### mutating the variable hh_wd in the parent data 
r$hh_with_disability <- loop$hh_wd[match(r$X_uuid, loop$X_uuid)]

### mutating indicator hp1_a :% HH with at least one individual with a disability
r$hp1_a <- case_when(r$hh_with_disability == 1 ~ 1,
                     TRUE ~ 0)

###% of girls with a disability
loop$hh_with_girls_wd <- case_when(loop$difficulty_communicating %in% c("a_lot_of_difficulty","cannot_do_at_all") & loop$difficulty_sex == "female" & loop$difficulty_age < 18|
                          loop$difficulty_hearing %in% c("a_lot_of_difficulty","cannot_do_at_all") & loop$difficulty_sex == "female" & loop$difficulty_age < 18|
                          loop$difficulty_remembering %in% c("a_lot_of_difficulty","cannot_do_at_all") & loop$difficulty_sex == "female" & loop$difficulty_age < 18|
                          loop$difficulty_washing %in% c("a_lot_of_difficulty","cannot_do_at_all") & loop$difficulty_sex == "female" & loop$difficulty_age < 18|
                          loop$difficulty_walking %in% c("a_lot_of_difficulty","cannot_do_at_all") & loop$difficulty_sex == "female" & loop$difficulty_age < 18|
                          loop$difficulty_seeing %in% c("a_lot_of_difficulty","cannot_do_at_all") & loop$difficulty_sex == "female" & loop$difficulty_age < 18 ~ 1,
                        loop$difficulty_communicating %in% c("some_difficulty","no_difficulty") & loop$difficulty_sex == "female" & loop$difficulty_age < 18 &
                          loop$difficulty_hearing %in% c("some_difficulty","no_difficulty") & loop$difficulty_sex == "female" & loop$difficulty_age < 18 &
                          loop$difficulty_remembering %in% c("some_difficulty","no_difficulty") & loop$difficulty_sex == "female" & loop$difficulty_age < 18 &
                          loop$difficulty_washing %in% c("some_difficulty","no_difficulty") & loop$difficulty_sex == "female" & loop$difficulty_age < 18 &
                          loop$difficulty_walking %in% c("some_difficulty","no_difficulty") & loop$difficulty_sex == "female" & loop$difficulty_age < 18 &
                          loop$difficulty_seeing %in% c("some_difficulty","no_difficulty") & loop$difficulty_sex == "female" & loop$difficulty_age < 18 ~ 0,
                        TRUE ~ 0)


### mutating the variable count_of_gwd  in the parent data 
r$hh_with_girls_wd <- loop$hh_with_girls_wd[match(r$X_uuid, loop$X_uuid)]

### mutating indicator hp1_b :% HH with at least one girl with a disability
r$hp1_b <- case_when(r$hh_with_girls_wd == 1 ~ 1,
                     TRUE ~ 0)
###% of boys with a disability
loop$hh_with_boys_wd <- case_when(loop$difficulty_communicating %in% c("a_lot_of_difficulty","cannot_do_at_all") & loop$difficulty_sex == "male" & loop$difficulty_age < 18|
                                     loop$difficulty_hearing %in% c("a_lot_of_difficulty","cannot_do_at_all") & loop$difficulty_sex == "male" & loop$difficulty_age < 18|
                                     loop$difficulty_remembering %in% c("a_lot_of_difficulty","cannot_do_at_all") & loop$difficulty_sex == "male" & loop$difficulty_age < 18|
                                     loop$difficulty_washing %in% c("a_lot_of_difficulty","cannot_do_at_all") & loop$difficulty_sex == "male" & loop$difficulty_age < 18|
                                     loop$difficulty_walking %in% c("a_lot_of_difficulty","cannot_do_at_all") & loop$difficulty_sex == "male" & loop$difficulty_age < 18|
                                     loop$difficulty_seeing %in% c("a_lot_of_difficulty","cannot_do_at_all") & loop$difficulty_sex == "male" & loop$difficulty_age < 18 ~ 1,
                                   loop$difficulty_communicating %in% c("some_difficulty","no_difficulty") & loop$difficulty_sex == "male" & loop$difficulty_age < 18 &
                                     loop$difficulty_hearing %in% c("some_difficulty","no_difficulty") & loop$difficulty_sex == "male" & loop$difficulty_age < 18 &
                                     loop$difficulty_remembering %in% c("some_difficulty","no_difficulty") & loop$difficulty_sex == "male" & loop$difficulty_age < 18 &
                                     loop$difficulty_washing %in% c("some_difficulty","no_difficulty") & loop$difficulty_sex == "male" & loop$difficulty_age < 18 &
                                     loop$difficulty_walking %in% c("some_difficulty","no_difficulty") & loop$difficulty_sex == "male" & loop$difficulty_age < 18 &
                                     loop$difficulty_seeing %in% c("some_difficulty","no_difficulty") & loop$difficulty_sex == "male" & loop$difficulty_age < 18 ~ 0,
                                   TRUE ~ 0)


### mutating the variable count_of_gwd  in the parent data 
r$hh_with_boys_wd <- loop$hh_with_boys_wd[match(r$X_uuid, loop$X_uuid)]

### mutating indicator hp1_c :% HH with at least one boy with a disability
r$hp1_c <- case_when(r$hh_with_boys_wd == 1 ~ 1,
                     TRUE ~ 0)
### mutating indicator hp1_d :% HH with at least one child with a disability
r$hp1_d <- case_when(r$hh_with_boys_wd == 1 |r$hh_with_girls_wd == 1 ~ 1,
                     TRUE ~ 0)

##% of interviews conducted with the head of household
r$hh1 <- ifelse(r$hhh == "yes", 1, 0)


##% of interviews conducted with male or female participants
r$hh5_i <- ifelse(r$gender_respondent == "female", 1, 0)
r$hh5_ii <- ifelse(r$gender_respondent == "male", 1, 0)


##% of female headed households
r$hh6_i <- case_when( r$gender_hhh == "female" ~ 1,
                    r$gender_hhh == "male" ~ 0,
                    TRUE ~ NA_real_)

##% of male headed households
r$hh6_i <- case_when( r$gender_hhh == "male" ~ 1,
                      r$gender_hhh == "female" ~ 0,
                      TRUE ~ NA_real_)

##% of refugee households
r$hh8 <- ifelse(r$refugee_status == "yes", 1, 0)

##% of households with at least one member who's pregnant or lactating
r$hh10 <- ifelse(r$preg_lactating == "yes", 1, 0)

##% of households with at least one member with a chronic disease
r$hh12 <- ifelse(r$chronic_illness == "yes", 1, 0)


##% of HHs displaced as a result of the most recent conflict in Gaza (starting on the 11th of May 2021)
r$hhd1 <- case_when(r$permanent_location_g == "no" ~ 1,
                    r$permanent_location_g %in% c("yes", "do_not_know", "decline_to_answer") ~ 0,
                    TRUE ~ NA_real_)


##% of HHs that have been displaced as a result of the recent conflict and have since returned to their previous location
r$hhd2 <- case_when(r$permanent_location_g == "yes" & r$displacement_status_g == "yes" ~ 1,
                    r$displacement_status_g %in% c("no", "do_not_know", "decline_to_answer") |
                    r$permanent_location_g %in% c("no", "do_not_know", "decline_to_answer")   ~ 0,
                    TRUE ~ NA_real_)


##% of HHs currently hosting displaced individuals
r$hhd3 <- case_when(r$currently_hosting_displaced_g == "yes" ~ 1,
                    r$currently_hosting_displaced_g %in% c("no", "do_not_know", "decline_to_answer") ~ 0,
                    TRUE ~ NA_real_)
 

##Food Security
r$fcs <- 
  (as.numeric(r$cereals)*2) +(as.numeric(r$nuts_seed)*3) +(as.numeric(r$milk_dairy)*4) + (as.numeric(r$meat)*4)+ 
  as.numeric(r$vegetables) + as.numeric(r$fruits) + (as.numeric(r$oil_fats)*0.5) + (as.numeric(r$sweets))


r$poor_fcs <- ifelse(r$fcs <= 21, 1,0)
r$borderline_fcs <- ifelse(r$fcs > 21 & r$fcs <=35,1,0)
r$acceptable_fcs <- ifelse(r$fcs > 35,1,0)

#% of HHs by Food Consumption Score
r$f1_i <- ifelse(r$fcs <= 21, 1,0)
r$f1_ii <- ifelse(r$fcs > 21 & r$fcs <=35,1,0)
r$f1_iii <- ifelse(r$fcs > 35,1,0)


### F2
r$worried <- ifelse(r$lack_enough_food == "yes", 1,0)
r$healthy <- ifelse(r$lack_healthy_food == "yes", 1,0)
r$fewfoods <- ifelse(r$lack_food_variety == "yes", 1,0)
r$skipped <- ifelse(r$skipped_meals == "yes", 1,0)
r$ate_less <- ifelse(r$ate_less == "yes", 1,0)
r$ranout <- ifelse(r$out_of_food == "yes", 1,0)
r$hungry <- ifelse(r$hungry_ddnt_eat == "yes", 1,0)
r$wholeday <- ifelse(r$day_without_eating == "yes", 1,0)



##% HH relying on stress / crisis / emergency strategies to cope with a lack of food or money to buy it
r$stress <-
  ifelse(
    r$coping_selling_properties %in% c("no_already_did", "yes") |
    r$coping_food_credit %in% c("no_already_did", "yes") |
    r$coping_reducing_expenditure %in% c("no_already_did", "yes"), 
    1,
    0  
  )

r$crisis <-
  ifelse(
    r$coping_selling_tranport %in% c("no_already_did", "yes") |
    r$coping_changing_residency %in% c("no_already_did", "yes") |
    r$coping_child_labour %in% c("no_already_did", "yes"),
    1,
    0
  )
r$emergency <-
  ifelse(
    r$coping_children_dropout %in% c("no_already_did", "yes") |
    r$coping_risky_behaviour %in% c("no_already_did", "yes") |
    r$coping_migration %in% c("no_already_did", "yes") |
    r$coping_forced_marriage %in% c("no_already_did", "yes"),
    1,
    0
  )

r$fl1_i <- ifelse(r$stress == 1, 1,0)
r$fl1_ii <- ifelse(r$crisis == 1, 1,0)
r$fl1_iii <- ifelse(r$emergency == 1, 1,0)



####Health
##% HH that can access a hospital within a 30min walk from dwellings
r$h1 <- ifelse(r$distance_hospital <= 30, 1, 0)


#% of HH that needed to access health services in the past 3 months
r$h2 <- ifelse( r$health_accessed == "yes", 1, 0) 

#% of HH that needed to access health services in the past 3 months by type of treatment
r$h2_i    <- r$type_treatment.covid_19_testing
r$h2_ii   <- r$type_treatment.covid_19_treatment
r$h2_iii  <- r$type_treatment.examination_or_non_surgical_treatment
r$h2_iv   <- r$type_treatment.regular_check_up_treatment
r$h2_v    <- r$type_treatment.elective_surgery
r$h2_vi   <- r$type_treatment.emergency_surgery
r$h2_vii  <- r$type_treatment.giving_birth
r$h2_bi   <-r$facility_attempt_care.primary
r$h2_bii  <-r$facility_attempt_care.secondary


#% of HHs reporting access barriers when trying to access health services in the past 3 months
r$h3 <- case_when(r$health_barriers == "yes" ~ 1,
                    r$health_barriers %in% c("no", "do_not_know", "decline_to_answer") ~ 0,
                    TRUE ~ NA_real_)

#% of HHs reporting access barriers when trying to access health services in the past 3 months by type of barrier
r$h3_i    <- r$type_health_barriers.cost
r$h3_ii   <- r$type_health_barriers.authorities_denied_request
r$h3_iii  <- r$type_health_barriers.civil_docs_problems
r$h3_iv   <- r$type_health_barriers.no_referral_phc
r$h3_v    <- r$type_health_barriers.phc_closed
r$h3_vi   <- r$type_health_barriers.movement_to_facility_restricted
r$h3_vii  <- r$type_health_barriers.distance_to_treatmentcenter
r$h3_viii <- r$type_health_barriers.no_medicine
r$h3_ix   <- r$type_health_barriers.no_fem_staff
r$h3_x    <- r$type_health_barriers.unqualified_staff
r$h3_xi   <- r$type_health_barriers.quality_of_services
r$h3_xii  <- r$type_health_barriers.family_imposed_barrier
r$h3_xiii <- r$type_health_barriers.lack_of_awareness
r$h3_xiv  <- r$type_health_barriers.refugee_status
r$h3_xv   <- r$type_health_barriers.refused_treatment
r$h3_xvi  <- r$type_health_barriers.no_offered_treatment
r$h3_xvii <- r$type_health_barriers.not_inclusive


##% of HHs reporting that NOT all members in their household are willing to be vaccinated against COVID-19
r$h4 <- ifelse(r$vaccine_willingness == "no", 1, 0)

##% of HHs reporting that NOT all members in their household are willing to be vaccinated against COVID-19 by reason for not wanting to be vaccinated
r$h4_i    <- r$vaccine_why_not.not_safe
r$h4_ii   <- r$vaccine_why_not.not_effective
r$h4_iii  <- r$vaccine_why_not.health_condition
r$h4_iv   <- r$vaccine_why_not.covid_no_threat
r$h4_v    <- r$vaccine_why_not.wait_other_vaccinated
r$h4_vi   <- r$vaccine_why_not.lack_knowledge_access
r$h4_vii  <- r$vaccine_why_not.cannot_afford
r$h4_viii <- r$vaccine_why_not.general_opposition_vaccine
r$h4_ix   <- r$vaccine_why_not.too_young


#% HH with members unable to access one or more services due to disability
r$hp7 <- case_when(r$disability_access_barriers == "yes" ~ 1,
                  r$disability_access_barriers %in% c("no", "do_not_know", "decline_to_answer") ~ 0,
                  TRUE ~ NA_real_)
#Primary reason why services are inaccesible to persons with disabilities 
r$hp8_i     <- ifelse(r$disability_access_barriers_reason == "costs_access", 1,0)
r$hp8_ii    <- ifelse(r$disability_access_barriers_reason == "distance", 1,0)
r$hp8_iii   <- ifelse(r$disability_access_barriers_reason == "costs_service", 1,0)
r$hp8_iv    <- ifelse(r$disability_access_barriers_reason == "lack_specialized_center", 1,0)
r$hp8_v     <- ifelse(r$disability_access_barriers_reason == "inaccessible_environment", 1,0)
r$hp8_vi    <- ifelse(r$disability_access_barriers_reason == "discrimination", 1,0)
r$hp8_vii   <- ifelse(r$disability_access_barriers_reason == "not_adapted", 1,0)
r$hp8_viii  <- ifelse(r$disability_access_barriers_reason == "movement_restricted", 1,0)
r$hp8_ix    <- ifelse(r$disability_access_barriers_reason == "information_missing", 1,0)
r$hp8_x     <- ifelse(r$disability_access_barriers_reason == "stigma", 1,0)
r$hp8_xi    <- ifelse(r$disability_access_barriers_reason == "lack_permit", 1,0)
r$hp8_xii   <- ifelse(r$disability_access_barriers_reason == "other", 1,0)


#% of HH where at least one member is reporting signs of psychosocial distress (self-diagnosed)
r$hp9 <- ifelse(r$hh_member_distress == "yes", 1,0)

#% of HH where at least one child is reporting signs of psychosocial distress (self-diagnosed)
r$hp10 <- case_when(r$child_distress_number > 0 ~ 1, 
                   r$child_distress_number == 0 | r$hh_member_distress == "no" ~ 0,
                   TRUE ~ NA_real_)

#% of HH where at least one adult is reporting signs of psychosocial distress (self-diagnosed)
r$hp11 <- case_when(r$adult_distress_number > 0 ~ 1, 
                   r$adult_distress_number == 0 | r$hh_member_distress == "no" ~ 0,
                   TRUE ~ NA_real_)

#% of HHs reporting availability of PSS services in case of GBV
r$hp12 <- ifelse(r$availability_pss_women == "yes", 1, 0)

#% of HHs reporting availability of PSS services in case of GBV by type of service
r$hp12_i   <-r$type_of_pss.legal_services
r$hp12_ii  <-r$type_of_pss.medical_services
r$hp12_iii <-r$type_of_pss.psychosocial_services

#% HH where women and girls of reproductive age (15-49) have access to specialized reproductive health services
r$hp13 <- ifelse(r$availability_reproductive_services == "yes", 1, 0)

##% of households with access to an improved water source for drinking purposes
r$w1 <- ifelse(
  r$drinking_water_source %in%
    c(
      "borehole",
      "prot_well",
      "prot_spring",
      "bottled_water",
      "network_private",
      "network_comm"
    ),
  1,
  0
)

##% of households (in Gaza) whose primary drinking water source has changed as a result of the recent escalation
r$w1a <- case_when(r$changed_drinking_source_g == "yes" ~ 1,
                   r$changed_drinking_source_g %in% c("no", "do_not_know", "decline_to_answer") ~ 0,
                   TRUE ~ NA_real_)

#% of households who are using an improved water source for domestic purposes
r$w2 <- ifelse(
  r$domestic_water_source %in%
    c(
      "borehole",
      "prot_well",
      "prot_spring",
      "bottled_water",
      "network_private",
      "network_comm"
    ),
  1,
  0
)

#% of households with access to an improved sanitation facility
r$ws1 <- ifelse(r$latrine_type %in% c("vip_pit", "flush"), 1, 0)

#% of households who are sharing their sanitation facilities with other households
r$ws1a <- case_when(r$shared_sanitation == "yes" ~ 1,
                   r$shared_sanitation == "no" ~ 0,
                   TRUE ~ NA_real_)

# % of households reporting the availability of all listed sanitation items (toilet sear, bidet, niagara, handwashing station, soap, toilet paper)
r$ws1b <- case_when(r$latrine_items.toilet_seat == 1 &
                  r$latrine_items.bidet == 1 &
                  r$latrine_items.toilet_Niagara == 1 &
                  r$latrine_items.handwashing_station == 1 &
                    r$latrine_items.soap == 1 &
                  r$latrine_items.toilet_paper == 1 ~ 1,
                  TRUE ~ 0)

#% of households who are reporting that their latrines are lockable from the inside
r$ws1c <- case_when(r$latrines_lockable_inside == "yes" ~ 1,
                    r$latrines_lockable_inside == "no" ~ 0,
                    TRUE ~ NA_real_)

#% of households by type of waste drainage system
r$ws1b_i <- ifelse(r$latrine_waste_drainage == "sewage_system", 1, 0)
r$ws1b_ii <- ifelse(r$latrine_waste_drainage == "handdug_hole", 1, 0)
r$ws1b_iii <- ifelse(r$latrine_waste_drainage == "covered_septic", 1, 0)
r$ws1b_iv <- ifelse(r$latrine_waste_drainage == "open_area", 1, 0)
r$ws1b_v <- ifelse(r$latrine_waste_drainage == "other", 1, 0)

#% of households with access to a sufficient quantity of water for drinking and domestic purposes
r$w3 <- ifelse(
  r$sufficient_water_drinking == "yes" &
    r$sufficient_water_cooking == "yes" &
    r$sufficient_water_hygiene_personal == "yes" &
    r$sufficient_water_hygiene_domestic == "yes" &
    r$sufficient_water_other_water == "yes",
  1,
  0
)

#% of households with access to a sufficient quantity of water for drinking  purposes
r$w3a <- ifelse(
  r$sufficient_water_drinking == "yes",
  1,
  0
)
#% of households reporting relying on coping strategies to adapt to a lack of water
r$w4 <- case_when(r$water_coping_mechanism_g.no_coping_needed_used == 1 ~ 0, 
                  r$water_coping_mechanism_g.no_coping_needed_used == 0 & r$water_coping_mechanism_g.do_not_know == 1 ~ 0,
                  r$water_coping_mechanism_g.no_coping_needed_used == 0 & r$water_coping_mechanism_g.decline_to_answer == 1 ~ 0, 
                  is.na(r$water_coping_mechanism_g.no_coping_needed_used) ~ NA_real_,
                  TRUE ~ 1)

#% of households reporting relying on coping strategies to adapt to a lack of water by type of coping strategy
r$w4_i <- r$water_coping_mechanism_g.no_coping_needed_used
r$w4_ii <- r$water_coping_mechanism_g.spent_more_on_water
r$w4_iii <- r$water_coping_mechanism_g.water_on_credit
r$w4_iv <- r$water_coping_mechanism_g.drank_stored_water
r$w4_v <- r$water_coping_mechanism_g.reduced_drinking_water
r$w4_vi <- r$water_coping_mechanism_g.modified_hygiene
r$w4_vii <- r$water_coping_mechanism_g.drank_cleaning_water
r$w4_viii <- r$water_coping_mechanism_g.other


#% of households affected by floods
r$w9 <- case_when(r$num_of_floods > 0 ~ 1, 
                    r$num_of_floods == 0 ~ 0,
                    TRUE ~ NA_real_)

#% of households whose daily activities have been affected by floods 
r$w9_a <- case_when(r$floods_activities_effects.none == 1 ~ 0, 
                  r$floods_activities_effects.none == 0 & r$floods_activities_effects.do_not_know == 1 ~ 0,
                  r$floods_activities_effects.none == 0 & r$floods_activities_effects.decline_to_answer == 1 ~ 0, 
                  is.na(r$floods_activities_effects.none) ~ NA_real_,
                  TRUE ~ 1)



#% of households whose shelter has been affected by floods (as a percentage of those households who have experienced floods
r$w9_b <- case_when(r$floods_shelter_effects.none == 1 ~ 0, 
                    r$floods_shelter_effects.none == 0 & r$floods_shelter_effects.do_not_know == 1 ~ 0,
                    r$floods_shelter_effects.none == 0 & r$floods_shelter_effects.decline_to_answer == 1 ~ 0, 
                    is.na(r$floods_shelter_effects.none) ~ NA_real_,
                    TRUE ~ 1)

#% % of households covered by solid waste services of households
r$w10 <- ifelse(r$solid_waste_disposal == "municipality_waste_system", 1, 0)


#% of households reporting waste accumulation for more than 3 days in their area
r$w11 <- case_when(r$waste_accumulation_days > 3 ~ 1, 
                  r$waste_accumulation_days <= 3 ~ 0,
                  TRUE ~ NA_real_)

#% of households living in critical shelter
r$s1 <- ifelse(
  r$shelter_type %in%
    c(
      "unfinished_abandoned_building",
      "damaged_building",
      "tent",
      "collective_shelter",
      "container",
      "makeshift_shelter"
    ),
  1,
  0
)


#% of households where at least one member is sleeping in the living room
r$s2_a <- case_when(r$sleeping_living_room == "yes" ~ 1,
                    r$sleeping_living_room %in% c("no","do_not_know")  ~ 0,
                    TRUE ~ NA_real_)

#Average number of household members per room
r$s2 <- as.numeric(r$hh_size) / as.numeric(r$num_of_rooms)
r$s2 <- ifelse(r$s2 <= 0, NA, r$s2)

#% of households by occupancy status
r$sp1_i <- ifelse(r$occupancy_status == "ownership", 1, 0)
r$sp1_ii <- ifelse(r$occupancy_status == "rented", 1, 0)
r$sp1_iii <- ifelse(r$occupancy_status == "hosted_without_rent", 1, 0)
r$sp1_iv <- ifelse(r$occupancy_status == "squatting", 1, 0)
r$sp1_v <- ifelse(r$occupancy_status == "other", 1, 0)

#% of households reporting risk of eviction
r$sp2 <- ifelse(r$hh_risk_eviction == "yes" , 1, 0)

#% of households reporting risk of eviction by reason
r$sp3_i <- r$hh_risk_eviction_reason.authorities_request 
r$sp3_ii <- r$hh_risk_eviction_reason.lack_funds 
r$sp3_iii <- r$hh_risk_eviction_reason.no_longer_hosted 
r$sp3_iv <- r$hh_risk_eviction_reason.unaccepted_by_community 
r$sp3_v <- r$hh_risk_eviction_reason.owner_request 
r$sp3_vi <- r$hh_risk_eviction_reason.no_agreement 
r$sp3_vii <- r$hh_risk_eviction_reason.inadequate 
r$sp3_viii <- r$hh_risk_eviction_reason.occupied 
r$sp3_ix <- r$hh_risk_eviction_reason.confiscation 
r$sp3_x <- r$hh_risk_eviction_reason.dispute 
r$sp3_xi <- r$hh_risk_eviction_reason.other

#% of households whose shelter has any kind of damage or defects
r$s3 <- case_when(r$shelter_issues.none == 1 ~ 0, 
                    r$shelter_issues.none == 0 & r$shelter_issues.do_not_know == 1 ~ 0,
                    r$shelter_issues.none == 0 & r$shelter_issues.decline_to_answer == 1 ~ 0, 
                    is.na(r$shelter_issues.none) ~ NA_real_,
                    TRUE ~ 1)


#% of households that report having experienced threats or violent/destructive acts by non-Palestinian communities in their area in the past 30 days
r$sp4 <- case_when(r$settler_threats_wb == "yes" ~ 1,
                  r$settler_threats_wb %in% c("no","do_not_know","decline_to_answer")  ~ 0,
                  TRUE ~ NA_real_)

#% of households that report having experienced threats or violent/destructive acts by non-Palestinian communities in their area in the past 30 days by type of threat (as a percentage of those households who have experienced threats/violence)
r$sp4_i <- r$settler_threats_type_wb.disrupting_activities 
r$sp4_ii <- r$settler_threats_type_wb.menacing_behavior 
r$sp4_iii <- r$settler_threats_type_wb.verbal_abuse
r$sp4_iv <- r$settler_threats_type_wb.sexual_harassment 
r$sp4_v <- r$settler_threats_type_wb.obstructed_shelter
r$sp4_vi <- r$settler_threats_type_wb.obstructed_vicinity 
r$sp4_vii <- r$settler_threats_type_wb.physical_violence
r$sp4_viii <- r$settler_threats_type_wb.non_shelter_items_damage 
r$sp4_ix <- r$settler_threats_type_wb.shelter_damage 
r$sp4_x <- r$settler_threats_type_wb.economic_damage 
r$sp4_xi <- r$settler_threats_type_wb.theft 
r$sp4_xii <- r$settler_threats_type_wb.occupation_of_shelter 
r$sp4_xiii <- r$settler_threats_type_wb.forced_eviction 
r$sp4_xiv <- r$settler_threats_type_wb.detention
r$sp4_xv <- r$settler_threats_type_wb.other 


#% of households adopting coping strategies in the last 30 days to avoid threats or violent/destructive acts by non-Palestinian communities living in their area
r$p1 <- case_when(r$coping_settler_threats_wb.none == 1 ~ 0, 
                  r$coping_settler_threats_wb.none == 0 & r$coping_settler_threats_wb.do_not_know == 1 ~ 0,
                  r$coping_settler_threats_wb.none == 0 & r$coping_settler_threats_wb.decline_to_answer == 1 ~ 0, 
                  is.na(r$coping_settler_threats_wb.none) ~ NA_real_,
                  TRUE ~ 1)


#% of households adopting coping strategies in the last 30 days to avoid threats or violent/destructive acts by non-Palestinian communities living in their area by type of coping strategy
r$p1_i <- r$coping_settler_threats_wb.change_livelihoods 
r$p1_ii <- r$coping_settler_threats_wb.none
r$p1_iii <- r$coping_settler_threats_wb.withheld_children_school
r$p1_iv <-  r$coping_settler_threats_wb.withheld_children_travel
r$p1_v <-  r$coping_settler_threats_wb.withheld_adult_travel
r$p1_vi <-  r$coping_settler_threats_wb.security_measures
r$p1_vii <-  r$coping_settler_threats_wb.moved_property
r$p1_viii <- r$coping_settler_threats_wb.sent_children_away 
r$p1_ix <-  r$coping_settler_threats_wb.sent_adults_away 
r$p1_x <- r$coping_settler_threats_wb.hh_moved 
r$p1_xi <- r$coping_settler_threats_wb.other 


#% of households with school-aged children that are currently enrolled in basic or secondary education
r$e1 <- case_when(r$total_enrolled_children > 0 ~ 1, 
                  r$total_enrolled_children == 0 & 
                    r$hh_size_boys_5_10 != 0 &
                    r$hh_size_boys_11_15 != 0 &
                    r$hh_size_boys_16_17 != 0 &
                    r$hh_size_girls_5_10 != 0 &
                    r$hh_size_girls_11_15 != 0 &
                    r$hh_size_girls_16_17 != 0 ~ 0,
                  TRUE ~ NA_real_)

#% of households with school-aged children (who were previously attending school) who are continuing teaching and learning activities remotely
r$e2 <- case_when(r$remote_learning > 0 ~ 1, 
                  r$remote_learning == 0 ~ 0,
                  TRUE ~ NA_real_)


#% of households with school-aged children, where at least one child dropped out of school during the current school year
r$e4 <- case_when(r$dropout_num > 0 ~ 1, 
                   r$dropout_num == 0 ~ 0,
                   TRUE ~ NA_real_)


#% of households with a least one child with a disability that is not attending school regularly (as a percentage of households with at least one child with a disability)
r$e5 <- case_when(r$disabled_school_attendance == "no" ~ 1,
                   r$disabled_school_attendance %in% c("no","do_not_know","decline_to_answer")  ~ 0,
                   TRUE ~ NA_real_)


#% of households with children with a disability reporting challenges to accessing education services
##households reporting
r$e6 <- case_when( r$disabled_school_challenges.none == 1 ~ 0, 
                   r$disabled_school_challenges.none == 0 & r$disabled_school_challenges.do_not_know == 1 ~ 0,
                   r$disabled_school_challenges.none == 0 & r$disabled_school_challenges.decline_to_answer == 1 ~ 0, 
                   is.na(r$disabled_school_challenges.none) ~ NA_real_,
                   TRUE ~ 1)

#% of households that are in need of catch-up learning programmes
r$e7 <- case_when(r$catch_up_learning == "yes" ~ 1,
                  r$catch_up_learning %in% c("no","do_not_know","decline_to_answer")  ~ 0,
                  TRUE ~ NA_real_)


#% of households in Gaza that are NOT planning to enroll all school-aged children at the start of the next school year
r$e8 <- case_when(r$planning_school_enrollment %in% c("none","some") ~ 1,
                  r$planning_school_enrollment %in% c("not_applicable","do_not_know", "all")  ~ 0,
                  TRUE ~ NA_real_)


#% of households reporting the availability of PSS services in schools
r$ep1 <- case_when(r$school_pss_services.no == 1 ~ 0, 
                   r$school_pss_services.no == 0 & r$school_pss_services.not_sure == 1 ~ 0,
                   r$school_pss_services.no == 0 & r$school_pss_services.decline_to_answer == 1 ~ 0, 
                   is.na(r$school_pss_services.no) ~ NA_real_,
                   TRUE ~ 1)


#% of households reporting the availability of PSS services in school by type of service
r$ep1_i <- r$school_pss_services.trained_cousellors
r$ep1_ii <- r$school_pss_services.trained_teachers
r$ep1_iii <- r$school_pss_services.external_pss
r$ep1_iv <- r$school_pss_services.other


#% of households reporting barriers to accessing education 
r$ep2 <- case_when(r$edu_barriers.none != 1 ~ 1, 
                   r$edu_barriers.none == 1 ~ 0,
                   r$edu_barriers.none == 0 & r$edu_barriers.do_not_know == 1 ~ 0,
                   r$edu_barriers.none == 0 & r$edu_barriers.decline_to_answer == 1 ~ 0, 
                   is.na(r$edu_barriers.none) ~ NA_real_,
                   TRUE ~ 1)

#% of households reporting barriers to school by type of barriers
r$ep2_i <- r$edu_barriers.school_stopped
r$ep2_ii <- r$edu_barriers.commuting_not_safe_girls 
r$ep2_iii <- r$edu_barriers.commuting_not_safe_boys
r$ep2_iv <- r$edu_barriers.lack_fees 
r$ep2_v <- r$edu_barriers.unable_to_enroll
r$ep2_vi <- r$edu_barriers.lack_of_schools 
r$ep2_vii <- r$edu_barriers.cannot_physically_go
r$ep2_viii <- r$edu_barriers.overcrowed_schools
r$ep2_ix <- r$edu_barriers.lack_of_staff
r$ep2_x <- r$edu_barriers.poor_infrastructure
r$ep2_xi <- r$edu_barriers.curriculum_issues 
r$ep2_xii <- r$edu_barriers.child_working 
r$ep2_xiii <- r$edu_barriers.parental_refusal
r$ep2_xiv <- r$edu_barriers.lack_interest


## households that can access a functional basic and secondary school within a 30min walk from dwellings
r$e11 <- ifelse(r$primary_school_distance %in% c("less_15", "less_30") &
                   r$secondary_school_distance %in% c("less_15", "less_30"),1,0)

##% of households reporting safety concerns in relation to their childrens' education (traveling to or studying in education facilities reported as being unsafe or very unsafe)
r$ep3 <-ifelse(r$school_safety %in% c("unsafe", "very_unsafe"),1,0)


##% of households reporting safety concerns in relation to their childrens' education by type of concern
r$ep4_i <- r$school_safety_concerns.firing_tear_gas_wb
r$ep4_ii <- r$school_safety_concerns.students_detention_wb 
r$ep4_iii <- r$school_safety_concerns.checkpoints_delays_wb
r$ep4_iv <- r$school_safety_concerns.school_military_entry_wb 
r$ep4_v <- r$school_safety_concerns.military_presence_wb
r$ep4_vi <- r$school_safety_concerns.demolition_threat_wb 
r$ep4_vii <- r$school_safety_concerns.contracting_covid_19_wb
r$ep4_viii <- r$school_safety_concerns.violence_at_school_wb
r$ep4_ix <- r$school_safety_concerns.violence_commuting_wb
r$ep4_x <- r$school_safety_concerns.sexual_abuse_at_schl_wb
r$ep4_xi <- r$school_safety_concerns.sexual_abuse_commuting_wb 
r$ep4_xii <- r$school_safety_concerns.crossing_roads_wb 
r$ep4_xiii <- r$school_safety_concerns.attacks_settlers_wb
r$ep4_xiv <- r$school_safety_concerns.attacks_school_wb
r$ep4_xv <- r$school_safety_concerns.environmental_hazards_wb 
r$ep4_xvi <- r$school_safety_concerns.other_wb

r$ep4a_i <- r$school_safety_concerns.firing_tear_gas_ej
r$ep4a_ii <- r$school_safety_concerns.students_detention_ej 
r$ep4a_iii <- r$school_safety_concerns.checkpoints_delays_ej
r$ep4a_iv <- r$school_safety_concerns.school_military_entry_ej 
r$ep4a_v <- r$school_safety_concerns.military_presence_ej
r$ep4a_vi <- r$school_safety_concerns.demolition_threat_ej 
r$ep4a_vii <- r$school_safety_concerns.contracting_covid_19_ej
r$ep4a_viii <- r$school_safety_concerns.violence_at_school_ej
r$ep4a_ix <- r$school_safety_concerns.violence_commuting_ej
r$ep4a_x <- r$school_safety_concerns.sexual_abuse_at_schl_ej
r$ep4a_xi <- r$school_safety_concerns.sexual_abuse_commuting_ej 
r$ep4a_xii <- r$school_safety_concerns.crossing_roads_ej
r$ep4a_xiii <- r$school_safety_concerns.attacks_settlers_ej
r$ep4a_xiv <- r$school_safety_concerns.attacks_school_ej
r$ep4a_xv <- r$school_safety_concerns.environmental_hazards_ej 
r$ep4a_xvi <- r$school_safety_concerns.other_ej


r$ep4b_i <- r$school_safety_concerns.firing_tear_gas_h2
r$ep4b_ii <- r$school_safety_concerns.students_detention_h2 
r$ep4b_iii <- r$school_safety_concerns.checkpoints_delays_h2
r$ep4b_iv <- r$school_safety_concerns.school_military_entry_h2 
r$ep4b_v <- r$school_safety_concerns.military_presence_h2
r$ep4b_vi <- r$school_safety_concerns.demolition_threat_h2
r$ep4b_vii <- r$school_safety_concerns.contracting_covid_19_h2
r$ep4b_viii <- r$school_safety_concerns.violence_at_school_h2
r$ep4b_ix <- r$school_safety_concerns.violence_commuting_h2
r$ep4b_x <- r$school_safety_concerns.sexual_abuse_at_schl_h2
r$ep4b_xi <- r$school_safety_concerns.sexual_abuse_commuting_h2 
r$ep4b_xii <- r$school_safety_concerns.crossing_roads_h2
r$ep4b_xiii <- r$school_safety_concerns.attacks_settlers_h2
r$ep4b_xiv <- r$school_safety_concerns.attacks_school_h2
r$ep4b_xv <- r$school_safety_concerns.environmental_hazards_h2 
r$ep4b_xvi <- r$school_safety_concerns.other_h2

r$ep4c_i <- r$school_safety_concerns.firing_tear_gas_gs
r$ep4c_ii <- r$school_safety_concerns.students_detention_gs 
r$ep4c_iii <- r$school_safety_concerns.checkpoints_delays_gs
r$ep4c_iv <- r$school_safety_concerns.demolition_threat_gs
r$ep4c_v <- r$school_safety_concerns.contracting_covid_19_gs
r$ep4c_vi <- r$school_safety_concerns.violence_at_school_gs
r$ep4c_vii <- r$school_safety_concerns.violence_commuting_gs
r$ep4c_viii <- r$school_safety_concerns.sexual_abuse_at_schl_gs
r$ep4c_ix <- r$school_safety_concerns.sexual_abuse_commuting_gs 
r$ep4c_x <- r$school_safety_concerns.crossing_roads_gs
r$ep4c_xi <- r$school_safety_concerns.attacks_settlers_gs
r$ep4c_xii <-  r$school_safety_concerns.attacks_school_gs
r$ep4c_xiii <- r$school_safety_concerns.environmental_hazards_gs
r$ep4c_xiv <-r$school_safety_concerns.other_gs

##HHs whose monthly income has decreased as a result of COVID-19
r$l2 <- ifelse(r$income_change_covid == "yes" ,1,0)

##% of households reporting that their typical monthly income has changed since the recent escalation
r$l3 <- case_when(r$income_change_conflict_g %in% c("decreased_lot","decreased_little","increased_little","increased_lot") ~ 1,
                  r$income_change_conflict_g %in% c("no_change","do_not_know") ~ 0,
                  TRUE ~ NA_real_)

##% of households reporting an impact of the recent conflict on their livelihood assets or resources
r$l3a <- case_when(r$finances_change_conflict_g.none == 1 ~ 0, 
                  r$finances_change_conflict_g.none == 0 & r$finances_change_conflict_g.do_not_know == 1 ~ 0,
                  r$finances_change_conflict_g.none == 0 & r$finances_change_conflict_g.decline_to_answer == 1 ~ 0, 
                  is.na(r$finances_change_conflict_g.none) ~ NA_real_,
                  TRUE ~ 1)

##% of HHs with a debt value of more than NIS 5,000
r$l4a<- ifelse(r$how_much_debt > 5000, 1,
                ifelse(r$how_much_debt <= 5000, 0, NA_real_))


##% of HHs with a debt value of more than NIS 10,000 
r$l4b<- ifelse(r$how_much_debt > 10000, 1,
               ifelse(r$how_much_debt <= 10000, 0, NA_real_))

##% HHs unable to afford basic needs (% HH taking on debt due to healthcare, food, education, or basic household expenditures)
r$l5 <-
  ifelse(
    r$reasons_for_debt %in% c("healthcare", "food", "education","basic_household_expenditures"),
    1,
    0
  )

##% of HHs by primary reason for taking on debt
r$l5_i <- ifelse(r$reasons_for_debt == "healthcare" ,1,0)
r$l5_ii <- ifelse(r$reasons_for_debt == "food" ,1,0)
r$l5_iii <- ifelse(r$reasons_for_debt == "education" ,1,0)
r$l5_iv <- ifelse(r$reasons_for_debt == "basic_household_expenditure" ,1,0)
r$l5_v <- ifelse(r$reasons_for_debt == "business_related" ,1,0)
r$l5_vi <- ifelse(r$reasons_for_debt == "clothing_or_NFI" ,1,0)
r$l5_vii <- ifelse(r$reasons_for_debt == "income_generating_activities" ,1,0)
r$l5_viii <- ifelse(r$reasons_for_debt == "major_purchase" ,1,0)
r$l5_ix <- ifelse(r$reasons_for_debt == "reconstruction" ,1,0)
r$l5_x <- ifelse(r$reasons_for_debt == "weddings" ,1,0)


##% of HHs whose debt has increased as a result of COVID-19
r$l6 <- case_when(r$debt_due_to_covid == "yes" ~ 1,
                  r$debt_due_to_covid %in% c("no","do_not_know","decline_to_answer")  ~ 0,
                  TRUE ~ NA_real_)


##Average HH expenditure share by type of expenditure
r$food_share <- round((as.numeric(r$food_exp)/ as.numeric(r$tot_expenses)), 1)
r$l7_i <- ifelse(r$food_share > 100, NA, 
               r$food_share)

r$water_share <- round((as.numeric(r$water_exp)/ as.numeric(r$tot_expenses)), 1)

r$l7_ii <- ifelse(r$water_share > 100, NA, 
                 r$water_share)

r$rent_share <- round((as.numeric(r$rent_exp)/ as.numeric(r$tot_expenses)), 1)
r$l7_iii <- ifelse(r$rent_share > 100, NA, 
                  r$rent_share)

r$medical_share <- round((as.numeric(r$medical_exp)/ as.numeric(r$tot_expenses)), 1)
r$l7_iv <- ifelse(r$medical_share > 100, NA, 
                   r$medical_share)

r$debt_share <- round((as.numeric(r$debt_repayment)/ as.numeric(r$tot_expenses)), 1)
r$l7_v <- ifelse(r$debt_share > 100, NA, 
                  r$debt_share)

r$fuel_share <- round((as.numeric(r$fuel_exp)/ as.numeric(r$tot_expenses)), 1)
r$l7_vi <- ifelse(r$fuel_share > 100, NA, 
                  r$fuel_share)

##% of HHs spending more than 50% of total expenditure on food
r$l7a <- ifelse(r$l7_i > 50, 1,
                ifelse(r$l7_i <=50, 0, NA ))


## HH with at least one adult (18+) unemployed and seeking work
r$l9 <- ifelse(r$unemployed_adults > 0, 1,0)


##Main barriers to employment
r$l10_i <- r$barriers_to_employment.job_competition
r$l10_ii <- r$barriers_to_employment.jobs_are_too_far_away  
r$l10_iii <- r$barriers_to_employment.low_pay_degrading_jobs
r$l10_iv <- r$barriers_to_employment.unqualified
r$l10_v <- r$barriers_to_employment.lack_family_connection
r$l10_vi <- r$barriers_to_employment.lack_women_opportunities
r$l10_vii <- r$barriers_to_employment.lack_disability_opportunities
r$l10_viii <- r$barriers_to_employment.restrictions
r$l10_ix <- r$barriers_to_employment.other


##% of  HH with at least one person under (<18) working
r$lp1 <- case_when(r$under_18_working == "yes" ~ 1,
                  r$under_18_working %in% c("no","do_not_know","decline_to_answer")  ~ 0,
                  TRUE ~ NA_real_)


##% of HH reporting members losing their job permanently or temporarily as a result of the COVID-19 outbreak
r$l12 <- ifelse(r$covid_loss_job == "yes", 1,0)


##% of HH reporting members losing jobs permanently or temporarily as a result of the recent conflict
r$l12b <- case_when(r$conflict_loss_job == "yes" ~ 1,
                  r$conflict_loss_job %in% c("no","do_not_know","decline_to_answer")  ~ 0,
                  TRUE ~ NA_real_)


##% of HHs relying on humanitarian assistance as a primary source of income
r$l13     <- ifelse(r$primary_livelihood.charity_assistance  == 1, 1, 0)

##% of HH reporting challenges in obtaining enough money to meet its needs over the last 30 days
r$l14 <- case_when( r$afford_communication_needs == "yes" |
                    r$afford_education_needs == "yes" |
                    r$afford_health_needs == "yes"|
                    r$afford_shelter_needs == "yes"|
                    r$afford_transport_services == "yes"|
                    r$afford_utilities == "yes" ~ 1,
                    TRUE ~ 0)

##% of HHs reporting challenges in obtaining enough money to meet their basic needs over the past 30 days by type of need
r$l14_i <- ifelse(r$afford_communication_needs == "yes" ,1,0)
r$l14_ii <- ifelse(r$afford_education_needs == "yes" ,1,0)
r$l14_iii <- ifelse(r$afford_health_needs == "yes" ,1,0)
r$l14_iv <- ifelse(r$afford_shelter_needs == "yes" ,1,0)
r$l14_v <- ifelse(r$afford_transport_services == "yes" ,1,0)
r$l14_vi <- ifelse(r$afford_utilities == "yes" ,1,0)


##% HHs where the primary income-earner is below the age of 18 
r$p2a <- case_when(r$income_earner%in% c("male_child_14_17", "male_child_13", "female_child_14_18", "female_child_13") ~ 1,
                 TRUE ~ 0)


##% HHs where the primary decision-maker is below the age of 18 
r$p2b <- case_when( r$decision_maker.female_child_14 == "1"|
                    r$decision_maker.female_child_14_18 == "1"|
                    r$decision_maker.male_child_13 == "1" |
                    r$decision_maker.male_child_14_17 == "1" ~ 1,
                  TRUE ~ 0)

##% of HHs where women and girls avoid areas because they feel unsafe there 
r$p3 <- ifelse(r$women_feel_unsafe == "yes" , 1, 0)
  

##% of HHs reporting safety or security concerns for girls
r$p4 <- case_when(r$security_concerns_girls.none == 1 ~ 0, 
                  r$security_concerns_girls.none == 0 & r$security_concerns_girls.do_not_know == 1 ~ 0,
                  r$security_concerns_girls.none == 0 & r$security_concerns_girls.decline_to_answer == 1 ~ 0, 
                  is.na(r$security_concerns_girls.none) ~ NA_real_,
                  TRUE ~ 1)

##% of HHs reporting safety or security concerns for boys
r$p5 <- case_when(r$security_concerns_boys.none == 1 ~ 0, 
                  r$security_concerns_boys.none == 0 & r$security_concerns_boys.do_not_know == 1 ~ 0,
                  r$security_concerns_boys.none == 0 & r$security_concerns_boys.decline_to_answer == 1 ~ 0, 
                  is.na(r$security_concerns_boys.none) ~ NA_real_,
                  TRUE ~ 1)

##% of HHs reporting safety or security concerns for children with a disability (as a percentage of those HHs where at least one child member of the HH has a disability)
r$p6 <- case_when(r$security_concerns_disabled.none == 1 ~ 0, 
                  r$security_concerns_disabled.none == 0 ~ 1,
                  r$security_concerns_disabled.do_not_know == 1 ~ 0, 
                  r$security_concerns_disabled.decline_to_answer == 1 ~ 0,
                                  TRUE ~ NA_real_)


##% of HHs reporting safety or security concerns for women
r$p7 <- case_when(r$security_concerns_women.none == 1 ~ 0, 
                    r$security_concerns_women.none == 0 & r$security_concerns_women.do_not_know == 1 ~ 0,
                    r$security_concerns_women.none == 0 & r$security_concerns_women.decline_to_answer == 1 ~ 0, 
                    is.na(r$security_concerns_women.none) ~ NA_real_,
                    TRUE ~ 1)


##% of households that have a standing demolition order
r$sp5 <-
  case_when(
    r$demolition_order_wb == "yes" ~ 1,
    r$demolition_order_wb %in% c("decline_to_answer", 'no', "do_not_know") ~ 0,
    TRUE ~ NA_real_
  )


##% of HH with members who have received information or training on the risks of ERW
r$p8 <- ifelse(r$explosives_training == "yes" , 1, 0)


##% of HHs in Gaza whose home has been damaged or destroyed by bombardment since 2014
r$p9 <-
  case_when(
    r$building_damage_g == "yes" ~ 1,
    r$building_damage_g %in% c("decline_to_answer", 'no', "do_not_know") ~ 0,
    TRUE ~ NA_real_
  )

##% of HHs whose house has been damage or destroyed as a result of the 2014 conflict
r$sp6_i <- ifelse(r$building_damage_level_2021_g == "major_damage" | 
                  r$building_damage_level_2021_g == "minor_damage", 1, 0)


##% of HHs reporting damage to their current shelter as a result of the recent conflict 
r$sp6_ii <- ifelse(r$building_damage_level_2021_g == "major_damage" | 
                  r$building_damage_level_2021_g == "minor_damage", 1, 0)


#% of HHs whose house is currently still damaged as a result of bombardment since 2014
r$sp6_iii <-
  case_when(
    r$building_damage_level_current_g %in% c("minor", "major", "moderate", "completely") ~ 1,
    r$building_damage_level_current_g  %in% c("decline_to_answer", "do_not_know", "none") ~ 0,
    TRUE ~ NA_real_
  )

##% of households without any capacity to repair and rehabilitate the shelter that has been damaged or destroyed
r$sp7 <-
  case_when(
    r$building_damage_repair_capacity_g == "none" ~ 1,
          is.na(r$building_damage_repair_capacity_g) ~ NA_real_, 
    TRUE ~ 0)


##% HH reporting to have received aid in the past 6 months
r$aap1 <- ifelse(r$aid_received == "yes", 1, 0)


##% HH reporting to have received aid since the beginning of the escalation in Gaza
r$aap1_a <-
  case_when(
    r$aid_received_escalation_g == "yes" ~ 1,
    r$aid_received_escalation_g  %in% c("decline_to_answer", "do_not_know", "no") ~ 0,
    TRUE ~ NA_real_
  )


##% HH reporting to have received in the past 6 months by type of aid received
r$aap2_i    <- r$aid_type.cash
r$aap2_ii   <- r$aid_type.food
r$aap2_iii  <- r$aid_type.water
r$aap2_iv   <- r$aid_type.fuel
r$aap2_v    <- r$aid_type.shelter
r$aap2_vi   <- r$aid_type.seasonal_items
r$aap2_vii  <- r$aid_type.health_services
r$aap2_viii <- r$aid_type.education_services
r$aap2_ix   <- r$aid_type.other_non_food_items
r$aap2_x    <- r$aid_type.protection


##% HH satisfied with the aid that they have received in the past 6 months
r$aap3 <-
  case_when(
    r$aid_satisfaction == "yes"  ~ 1,
    r$aid_satisfaction  %in% c("decline_to_answer", "do_not_know", "no") ~ 0,
    TRUE ~ NA_real_
  )


##Most commonly reported reasons for dissatisfaction with the aid received
r$aap4_i    <- r$aid_not_satisfied.delays_in_delivery
r$aap4_ii   <- r$aid_not_satisfied.quality_not_good
r$aap4_iii  <- r$aid_not_satisfied.quantity_not_enough


##% HH not satisfied with aid worker behavior
r$aap5      <- case_when(r$aid_workers_satisfied == "no" ~ 1, 
                        r$aid_received =="yes" &  r$aid_workers_satisfied == "yes" ~ 0,
                        TRUE ~  NA_real_)


##% of households reporting access barriers to aid as a result of the recent conflict in Gaza
r$aap6 <- case_when(r$barriers_to_aid_g.did_not_apply == 1 ~ 0, 
                    r$barriers_to_aid_g.did_not_apply == 0 & r$barriers_to_aid_g.do_not_know == 1 ~ 0,
                    r$barriers_to_aid_g.did_not_apply == 0 & r$barriers_to_aid_g.decline_to_answer == 1 ~ 0, 
                    is.na(r$barriers_to_aid_g.did_not_apply) ~ NA_real_,
                    TRUE ~ 1)


##% of households reporting type of humanitarian aid preferred in future distributions by type of aid
r$aap7_i <- r$aid_preferred_g.none
r$aap7_ii <- r$aid_preferred_g.bank_tranfers
r$aap7_iii <- r$aid_preferred_g.in_kind_food
r$aap7_iv <- r$aid_preferred_g.in_kind_nfi
r$aap7_v <- r$aid_preferred_g.mobile_money
r$aap7_vi <- r$aid_preferred_g.physical_cash
r$aap7_vii <- r$aid_preferred_g.prepaid_cards
r$aap7_viii <- r$aid_preferred_g.services
r$aap7_ix <- r$aid_preferred_g.vouchers


##% HH with access/knowledge of complaint mechanisms 
r$aap10 <- case_when(r$complaint_mechanisms == "yes" ~ 1, 
                        r$complaint_mechanisms %in% c("no","do_not_know", "decline_to_answer") ~ 0,
                        TRUE ~  NA_real_)

return(r)
}
