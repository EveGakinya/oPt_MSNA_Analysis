recoding_hno_coping <- function(r, loop) {
  
  r <- response
  cols.nam <- c("unsafe_locations.latrines_bathing_facilities", "unsafe_locations.water_points", "unsafe_locations.distribution_areas",
                "unsafe_locations.settlements_checkpoints", "unsafe_locations.markets", "unsafe_locations.at_the_workplace", "unsafe_locations.social_community_areas",
                "unsafe_locations.public_transport", "unsafe_locations.route_to_school", "unsafe_locations.route_to_communty_centres",
                "unsafe_locations.seeking_humanitarian_aid", "under_18_working_boys", "under_18_working_girls", "shelter_issues.roof_opening_cracks", "shelter_issues.broken_windows",
                "shelter_issues.roof_partially_collapsed", "shelter_issues.collapsed_walls", "shelter_issues.damaged_floors","shelter_issues.doors_broken","shelter_issues.doors_windows_missing",
                "shelter_issues.walls_small_cracks", "shelter_issues.walls_large_cracks","shelter_issues.structural_damage", "shelter_issues.total_structural_collapse",
                "shelter_issues.system_damage", "shelter_issues.foundation_shifted", "shelter_issues.electricity_damage", "hh_size_boys_5_10", "hh_size_boys_11_15",
                "hh_size_boys_16_17","hh_size_girls_5_10", "hh_size_girls_11_15", "hh_size_girls_16_17")
  
  
  r <- r %>% mutate_at(cols.nam, as.numeric)
  r <- r %>% 
    rowwise() %>%
    mutate(unsafe_locations_tot = sum(unsafe_locations.latrines_bathing_facilities,unsafe_locations.water_points,unsafe_locations.distribution_areas,
                                      unsafe_locations.settlements_checkpoints,unsafe_locations.markets,unsafe_locations.at_the_workplace,
                                      unsafe_locations.social_community_areas,unsafe_locations.public_transport,unsafe_locations.route_to_school,
                                      unsafe_locations.route_to_communty_centres,unsafe_locations.seeking_humanitarian_aid, na.rm = F),
           tot_under_18_working = sum(under_18_working_boys,under_18_working_girls, na.rm = F),
           shelter_issues_tot = sum(shelter_issues.roof_opening_cracks,shelter_issues.roof_partially_collapsed,shelter_issues.broken_windows,
                                    shelter_issues.doors_broken,shelter_issues.doors_windows_missing,shelter_issues.walls_small_cracks,
                                    shelter_issues.walls_large_cracks,shelter_issues.collapsed_walls,shelter_issues.damaged_floors,shelter_issues.structural_damage,
                                    shelter_issues.total_structural_collapse,shelter_issues.foundation_shifted,shelter_issues.system_damage,
                                    shelter_issues.electricity_damage,na.rm = F),
           school_aged_children = sum(hh_size_boys_5_10,hh_size_boys_11_15,hh_size_boys_16_17,hh_size_girls_5_10,hh_size_girls_11_15,hh_size_girls_16_17, na.rm = F))
  
  
  # ^- olivier -/- do we really wnat to omit the missing values? if all missing the row is zero, i would suggest to review
  # ^- Evelyn -/- Changed remove NAs to False
  
  #S_1 DISABILITY
  #############
  count_difficulty_level <- function(df) {
    diff <-  df[c(which(startsWith(names(df), "difficulty_")))]                   
    diff$no_diff <- rowSums(diff == "no_difficulty", na.rm = T)
    diff$some_diff <- rowSums(diff == "some_difficulty", na.rm = T)
    diff$lot_diff <- rowSums(diff == "a_lot_of_difficulty", na.rm = T)
    diff$cannot_diff <- rowSums(diff == "cannot_do_at_all", na.rm = T)
    diff <- diff[, c("no_diff", "some_diff", "lot_diff", "cannot_diff")]
    df <- cbind(df, diff)
    return(df)
  }
  loop <- count_difficulty_level(loop)
  #difficulty <- difficulty[, c("no_diff", "some_diff", "lot_diff", "cannot_diff")]
  #r <- cbind(r, difficulty)
  
  ### mutating the variable h1 in the parent data 
  r$no_diff   <- loop$no_diff[match(r$X_uuid, loop$X_uuid)]
  r$some_diff <- loop$some_diff[match(r$X_uuid, loop$X_uuid)]
  r$lot_diff  <- loop$lot_diff[match(r$X_uuid, loop$X_uuid)]
  r$cannot_diff <- loop$cannot_diff[match(r$X_uuid, loop$X_uuid)]
  
  
  r$s_1 <- case_when(r$some_diff == 0 & r$lot_diff == 0 & r$cannot_diff == 0 ~ 1,
                     r$lot_diff == 0 & r$cannot_diff == 0 & r$some_diff > 0 & 
                       r$some_diff <= 3 ~ 2,
                     (r$cannot_diff == 0 & r$lot_diff > 0 & r$lot_diff <= 3) |
                       (r$lot_diff == 0 & r$cannot_diff == 0 & r$some_diff >= 4) ~ 3,
                     (r$cannot_diff > 0 & r$cannot_diff <= 3) | 
                       r$lot_diff >= 4 ~ 4,
                     r$cannot_diff >= 4 ~ 5, 
                     is.na(r$no_diff) & is.na(r$some_diff) & is.na(r$lot_diff) & is.na(r$cannot_diff) ~ 1)
  
  # ^- olivier -/- a mistake in the excel table, so i am not sure that it is accurate
  # ^- michael -/- The mistake is in the excel table. This is actually a JIAF indicator, so these calculations here are aligned with the JIAF threholds
  
  #S_2 AAP
  #% HH satisfied with aid received 
  #% HH with access/knowledge of complaint mechanisms 
  ##############
  r$s_2 <- case_when(
    r$aid_satisfaction == "yes" & r$complaint_mechanisms == "yes" ~ 1,
    r$aid_satisfaction == "yes" & r$complaint_mechanisms == "no" ~ 2,
    r$aid_satisfaction == "no" & r$complaint_mechanisms == "yes" ~ 4,
    r$aid_satisfaction == "no" & r$complaint_mechanisms == "no" ~ 3,
  )
  # ^- olivier -/- can you be satisfied and not aware?
  # ^- evelyn -/- yes, one can be satisfied with the received and not aware of complaint mechanisms
  
  ###Education
  ###S_3 calculating the percentage of the school aged children who are out of school
  r$es2a <- round((as.numeric(r$dropout_num)/r$school_aged_children)*100,1)
  r$s_3 <- case_when(r$es2a == 0 ~ 1,
                     r$es2a > 0 & r$es2a < 100 ~ 3,
                     r$es2a == 100 ~ 5 )
  
  
  ###S_4 Average time needed by school-enrolled children to access the nearest education facility (primary and secondary) / % of households reporting safety concerns in relation to their children’s education
  count_issues_safety <- function(df) {
    diff <-  df[c(which(startsWith(names(df), "school_safety_concerns.")))]                   
    diff$nr_issues <- rowSums(diff, na.rm = T)
    diff <- diff[, c("nr_issues")]
    df <- cbind(df, diff)
    return(df)
  }
  r <- count_issues_safety(r)
  
  r$s_4 <- case_when(r$school_safety == "very_safe" | r$school_safety == "safe"  ~ 1,
                     r$school_safety == "unsafe" & r$nr_issues == 1 ~ 2,
                     r$school_safety == "unsafe" & r$nr_issues > 1 ~ 3,
                     r$school_safety == "very_unsafe"  ~ 4)
  
  
  #S_5 % of HH school-aged children (who were previously attending school) NOT continuing teaching and learning activities remotely and in need of catch-up learning programs. 
  r$remote_learning <- ifelse(r$remote_learning > r$school_aged_children, NA, r$remote_learning)
  
  r$es3a <- round((as.numeric(r$remote_learning)/r$school_aged_children)*100,1)
  r$s_5 <- case_when(r$es3a == 100 & r$catch_up_learning == "no" ~ 1,
                     r$es3a == 100 & r$catch_up_learning == "yes" ~ 2,
                     r$es3a < 100 & r$catch_up_learning == "no" ~ 3,
                     r$es3a < 100 & r$catch_up_learning == "yes" ~ 4,
                     TRUE ~ NA_real_)
  
  
  r$es3a[which(r$es3a > 100)]
  # ^- olivier -/- one is more than 100, please check back the data
  # ^- evelyn -/- changed all remote_learning greater than school_aged-children to NA
  
  
  #FOOD SECURITY
  ##############
  #S_6 Livelihood coping strategy (food) - 30 day recall")
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
  
  r$emergency_1 <- ifelse(r$coping_children_dropout %in% c("no_already_did", "yes"), 1, 0)
  r$emergency_2 <- ifelse(r$coping_risky_behaviour %in% c("no_already_did", "yes"), 1, 0)
  r$emergency_3 <- ifelse(r$coping_migration %in% c("no_already_did", "yes"), 1, 0)
  r$emergency_4 <- ifelse(r$coping_forced_marriage %in% c("no_already_did", "yes"), 1, 0)
  r$emergency_count <- rowSums(r[, c("emergency_1", "emergency_2", "emergency_3", "emergency_4")], na.rm = T)
  
  r$s_6 <- case_when(
    r$stress == 0 & r$crisis == 0 & r$emergency == 0 ~ 1,
    r$stress == 1 & r$crisis == 0 & r$emergency == 0 ~ 2,
    r$crisis == 1 & r$emergency == 0 ~ 3,
    r$emergency == 1 ~ 4,
    r$emergency_count > 1 ~ 5
  )
  
  # ^- olivier -/- near exaustion is missing
  # ^- evelyn -/- thanks, variable added.
  
  ##S_7 Food Expenditure share
  r$food_exp <-ifelse(r$food_exp >= r$tot_expenses, NA, r$food_exp)
  r$food_share <- round((as.numeric(r$food_exp)/ as.numeric(r$tot_expenses))*100 , 1)
  r$food_share <- ifelse(r$food_share > 100, NA, 
                         r$food_share)
  
  # ^- olivier -/- 46 over 100, 136 are 100 or more please check back teh data
  # ^- evelyn -/- changed all food_exp greater than total expenses to NA
  
  
  r$s_7 <- case_when(
    r$food_share < 50 ~ 1,
    r$food_share >= 50 & r$food_share < 65 ~ 2,
    r$food_share >= 65 & r$food_share < 75 ~ 3,
    r$food_share >= 75 & r$food_share < 85 ~ 4,
    r$food_share >= 85 & r$food_share <= 100 ~ 5
  )
  # ^- olivier -/- 100 is not included
  # ^- evelyn -/- updated, thanks.
  
  ##Food Consumption Score
  ##S_8 Food Security
  r$fcs <- 
    (as.numeric(r$cereals)*2) +(as.numeric(r$nuts_seed)*3) +(as.numeric(r$milk_dairy)*4) + (as.numeric(r$meat)*4)+ 
    as.numeric(r$vegetables) + as.numeric(r$fruits) + (as.numeric(r$oil_fats)*0.5) + (as.numeric(r$sweets)*0.5)
  
  # ^- olivier -/- there is mistake here the max should be 112 - it goes up  to 115.5
  # ^- evelyn -/- calculation updated
  
  r$poor_fcs <- ifelse(r$fcs <= 21, 1,0)
  r$borderline_fcs <- ifelse(r$fcs > 21 & r$fcs <=35,1,0)
  r$acceptable_fcs <- ifelse(r$fcs > 35,1,0)
  
  r$s_8 <- case_when(
    r$acceptable_fcs == 1 ~ 1,
    r$borderline_fcs == 1 ~ 3,
    r$poor_fcs == 1 ~ 4
  )
  
  
  ###HEALTH
  #S_9 Percentage of population that can access primary healthcare within one hour’s walk from dwellings
  #% of HHs facing access barriers when trying to access health services
  r$s_9 <- case_when(r$distance_hospital < 30 & r$health_barriers != "yes" ~ 1,
                     r$distance_hospital >= 30  & r$health_barriers != "yes" ~ 2,
                     r$distance_hospital < 30 & r$health_barriers == "yes"  ~ 3,
                     r$distance_hospital >= 30  & r$health_barriers == "yes" ~ 4)
  
  
  #S_10 % of HH where at least one member (SADD) is reporting signs of distress (self-diagnosed)
  r$tot_distressed <- as.numeric(r$child_distress_number) + as.numeric(r$adult_distress_number)
  
  r$per_distressed <- round((r$tot_distressed/as.numeric(r$hh_size))*100, 1)
  
  r$s_10 = case_when(
    r$per_distressed >= 0 & r$per_distressed < 20 ~ 2,
    r$per_distressed >= 20 & r$per_distressed < 40 ~ 3,
    r$per_distressed >= 40 & r$per_distressed < 60 ~ 4,
    r$per_distressed >= 60 ~ 5,
    TRUE ~ 1)
  
  # ^- olivier -/- there is a gap between 19-20, 39-40, 59-60 change for   r$per_distressed >= 0 & r$per_distressed < 20 ~ 2, etc.
  # ^- evelyn -/- recoding updated
  
  #S_11 % of girls / women who avoid areas because they feel unsafe")
  r$s_11 <- case_when(
    r$women_feel_unsafe == "no" ~ 1,
    r$unsafe_locations_tot == 0 ~ 1,
    r$unsafe_locations_tot == 1 ~ 2,
    r$unsafe_locations_tot == 2 ~ 3,
    r$unsafe_locations_tot == 3 ~ 4,
    r$unsafe_locations_tot >= 4 ~ 5
  )
  
  # ^- olivier -/- i think you should remove other from the unsafe location?
  # ^- evelyn -/- I have removed the variable.
  
  
  #S_12 % of households reporting risk of eviction
  r$sp2 <- ifelse(r$hh_risk_eviction == "yes" , 1, 0)
  
  r$s_12 <- case_when(
    r$sp2 == 0 ~ 1,
    r$sp2 == 1 ~ 3
  )
  
  
  ##checking if total number of children in the hh equal the total number of children working
  r$p13a <- ifelse(r$tot_children == r$tot_under_18_working, 1, 0)
  
  
  #S_13 % of girls / boys engaged in child labour
  r$s_13 <- case_when(
    is.na(r$tot_under_18_working) & as.numeric(r$tot_children) > 0 ~ 1,
    r$tot_under_18_working == 0 ~ 1,
    r$tot_under_18_working == 1 ~ 3,
    r$tot_under_18_working > 1 ~ 4,
    r$tot_under_18_working >= 1 & r$p13a == 1 ~ 5)
  
  
  #S_14 % of HHs having access to a sufficient quantity of water for drinking, cooking, bathing, washing or other domestic use")
  #r$s_14 <- case_when(
  #  (r$sufficient_water_cooking == "yes" & r$sufficient_water_drinking == "yes" &
  #     r$sufficient_water_hygiene_personal == "yes" & r$sufficient_water_other_water == "yes") ~ 1,
  #  (r$sufficient_water_cooking == "yes" & r$sufficient_water_drinking == "yes" &
  #     r$sufficient_water_hygiene_personal == "yes" & r$sufficient_water_other_water == "no") ~ 2,
  #  (r$sufficient_water_drinking == "yes" & (r$sufficient_water_cooking == "yes" & 
  #                                             r$sufficient_water_hygiene_personal == "no") | 
  #     (r$sufficient_water_cooking == "no" & r$sufficient_water_hygiene_personal == "yes")) ~ 3,
  #  (r$sufficient_water_drinking == "yes" & r$sufficient_water_cooking == "no" & 
  #     r$sufficient_water_hygiene_personal == "no") ~ 4,
  #  (r$sufficient_water_drinking == "no") ~ 5)
  
  #S_14 % of households with access to an improved water source for drinking and domestic purposes
  r$s_14 <- case_when(
    r$drinking_water_source == "network_private" | r$drinking_water_source == "network_comm" | 
      r$drinking_water_source == "illegal_connection" ~ 1,
    r$drinking_water_source == "borehole" | r$drinking_water_source == "prot_well" | 
      r$drinking_water_source == "prot_tank" | r$drinking_water_source == "prot_spring"  ~ 2,
    r$drinking_water_source == "bottled_water" | r$drinking_water_source == "water_trucking" ~ 3,
    r$drinking_water_source == "unprot_well" | r$drinking_water_source == "unprot_spring" ~ 4,
    r$drinking_water_source == "surface_water" ~ 5)
  
  
  
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
  
  
  #S_15 % of households reporting relying on coping strategies to adapt to a lack of water
  #% of households reporting relying on coping strategies to adapt to a lack of water
  r$w4 <- case_when(r$water_coping_mechanism_g.no_coping_needed_used == 1 ~ 0, 
                    r$water_coping_mechanism_g.no_coping_needed_used == 0 & r$water_coping_mechanism_g.do_not_know == 1 ~ 0,
                    r$water_coping_mechanism_g.no_coping_needed_used == 0 & r$water_coping_mechanism_g.decline_to_answer == 1 ~ 0, 
                    is.na(r$water_coping_mechanism_g.no_coping_needed_used) ~ NA_real_,
                    TRUE ~ 1)
  
  ## 1 rep hh relying on coping strategies to adapt to a lack of water and 0 hh not relying on coping strategies
  r$w4a <- ifelse(r$w4 == 1, 1,0)
  
  ##% of households reporting relying on coping strategies to adapt to a lack of water
  r$s_15 <- case_when(r$w3 == 1 ~ 1,
                      r$w4 == 0 & r$w4a == 0 ~ 2,
                      r$water_coping_mechanism_g.modified_hygiene == 1 | r$water_coping_mechanism_g.drank_stored_water == 1 |
                        r$water_coping_mechanism_g.spent_more_on_water == 1 ~ 3,
                      r$water_coping_mechanism_g.water_on_credit == 1 ~ 4,
                      r$water_coping_mechanism_g.reduced_drinking_water ==1 | r$water_coping_mechanism_g.drank_cleaning_water == 1 ~ 5)
  
  
  #S_16 % of people with inadequate access to sanitation services
  r$s_16 <- case_when(r$latrine_waste_drainage == "sewage_system" ~ 1,
                      r$latrine_waste_drainage == "covered_septic" ~ 2,
                      r$latrine_waste_drainage == "handdug_hole" ~ 3,
                      r$latrine_waste_drainage == "open_area" ~ 4)
  
  
  #LIVELIHOODS##########################
  ##S_17 percentage of adults unemployed and seeking work
  r$s17a <- round((as.numeric(r$unemployed_adults)/ as.numeric(r$tot_adults))*100, 1)
  
  ##
  r$s_17 <- case_when(r$s17a<= 40 & r$s17a > 0 ~ 2,
                      r$s17a > 40 & r$s17a<= 60 ~ 3,
                      r$s17a > 60 & r$s17a <= 80 ~ 4,
                      r$s17a > 80 ~ 5,
                      TRUE ~ 1)
  
  
  
  
  #S_18 % of HHs unable to afford basic needs
  r$s_18 <- case_when(r$how_much_debt == 0 ~ 1,
                      r$reasons_for_debt == "income_generating_activities" |r$reasons_for_debt == "business_related" |
                        r$reasons_for_debt == "clothing_or_NFI"| r$reasons_for_debt == "major_purchase" ~ 2,
                      r$reasons_for_debt == "weddings"| r$reasons_for_debt == "reconstruction" ~ 3,
                      r$reasons_for_debt == "education"| r$reasons_for_debt == "basic_household_expenditures" ~ 4,
                      r$reasons_for_debt == "healthcare" | r$reasons_for_debt == "food" ~ 5)
  
  # ^- olivier -/- there is nto any 5 in the framework
  # ^- evelyn -/- i have updated the framework.
  
  #S_19 % of HHs whose average monthly HH income per HH member was less than __ NIS
  r$l19a <- round((as.numeric(r$tot_income)/as.numeric(r$hh_size)),2)
  r$s_19 <- case_when(r$l19a > 2000 ~1,
                      r$l19a > 1450 & r$l19a <= 2000 ~2,
                      r$l19a > 700 & r$l19a <= 1450 ~ 3,
                      r$l19a > 300 & r$l19a <= 700 ~ 4,
                      r$l19a <= 300 ~ 5)
  
  
  #S_20 Average number of household members per room
  r$s2 <- round((as.numeric(r$hh_size) / as.numeric(r$num_of_rooms)),1)
  r$s_20 <- case_when(r$s2 <= 1 ~ 1,
                      r$s2 > 1 & r$s2 <= 1.99 ~2,
                      r$s2 > 2 & r$s2 <= 2.99 ~ 3,
                      r$s2 > 3  ~ 4,
                      TRUE ~ NA_real_)
  
  #S_21 % of HHs whose shelter has any kind of damage or defects
  r$s_21 <- case_when(r$shelter_issues_tot == 0 ~ 1,
                      r$shelter_issues_tot == 1 ~ 2,
                      r$shelter_issues_tot == 2 ~ 3,
                      r$shelter_issues_tot == 3 ~ 4,
                      r$shelter_issues_tot >= 4 ~ 5)
  
  #Humanitarian Condition - COPING
  #MEAN OF MAX 50% CALCULATION
  hno_coping <-  r[c("s_2", "s_6", "s_15", "s_16")]      
  #Normal rounding
  hno$final_severity_coping <-  apply(hno, 1, function(y) {
    round2(mean(tail(sort(y), (floor(ncol(hno)/2)))))
  })
  #HNO FINAL SEVERITY | COPING
  r$final_severity_coping <- hno$final_severity_coping
  r$hno_severity_coping_1 <- ifelse(r$final_severity_coping == 1, 1,0)
  r$hno_severity_coping_2 <- ifelse(r$final_severity_coping == 2, 1,0)
  r$hno_severity_coping_3 <- ifelse(r$final_severity_coping == 3, 1,0)
  r$hno_severity_coping_4 <- ifelse(r$final_severity_coping == 4, 1,0)
  r$hno_severity_coping_5 <- ifelse(r$final_severity_coping == 5, 1,0)
  
  
  
  #Humanitarian Condition - LIVING STANDARDS AND MENTAL WELL-BEING
  #MEAN OF MAX 50% CALCULATION
  hno_living <-  r[c("s_1", "s_11", "s_12", "s_13")]      
  #Normal rounding
  hno$final_severity_living <-  apply(hno, 1, function(y) {
    round2(mean(tail(sort(y), (floor(ncol(hno)/2)))))
  })
  
  
  #Rounding up
  #hno$mean <-  apply(hno, 1, function(y) {
  #  ceiling(mean(tail(sort(y), (floor(ncol(hno)/2)))))
  #})
  
  #d <- density(hno$mean_unrounded) 
  #plot(d)
  #abline(v=c(2,2.5), col=c("black", "black"), lty=c(2,2), lwd=c(1, 1))
  
  
  #CRITICAL INDICATORS
 # hno$critical <-  apply(hno, 1, function(y) {
#    max(y[c("s_8", "s_15")])
#  })
#  hno$critical <- ifelse(is.na(hno$critical),0, hno$critical)
#  hno$final_severity <- ifelse(hno$critical > hno$mean, hno$critical, hno$mean)
  
  
  #hno$final_severity <- as.character(as.numeric(hno$final_severity))
  r$final_severity <- hno$final_severity
  r$hno_severity_1 <- ifelse(r$final_severity == 1, 1,0)
  r$hno_severity_2 <- ifelse(r$final_severity == 2, 1,0)
  r$hno_severity_3 <- ifelse(r$final_severity == 3, 1,0)
  r$hno_severity_4 <- ifelse(r$final_severity == 4, 1,0)
  r$hno_severity_5 <- ifelse(r$final_severity == 5, 1,0)
  
  return(r)
}