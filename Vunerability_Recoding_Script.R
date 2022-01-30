library(tidyverse)

recoding_vul <- function(r) {
  
  r <- response
  
  
  
  ######Education Vulnerability###############
  # % of HHs with at least one child not enrolled in school during the 2020-2021 school year
  # At least one child in the HH was not in school during 2020-2021 year
  
  
  r <- r %>%  
    mutate(school_aged_children = hh_size_boys_5_10 + hh_size_boys_11_15 + hh_size_boys_16_17 + hh_size_girls_5_10 + hh_size_girls_11_15 + hh_size_girls_16_17) 
  
  r$e_1 <- case_when(r$total_enrolled_children  < r$school_aged_children ~ 1,
                     TRUE ~ 0)
  
  # % of HHs that cannot access a functional basic and secondary school within a 30min walk from dwellings
  # At least one type of school (basic or secondary) takes more than 30 min to reach
  
  r$e_2 <- ifelse(r$primary_school_distance %in% c("less_hour", "less_3hours","more_3hours") &
                    r$secondary_school_distance %in% c("less_hour", "less_3hours","more_3hours"),1,0)
  
  
  # % of HHs reporting safety concerns in relation to their childrens' education
  # The HH reports traveling to or studying in school is either safe or unsafe
  
  r$e_3 <-ifelse(r$school_safety %in% c("unsafe", "very_unsafe"),1,0)
  
  r$edu_vul <- case_when(r$e_1 == 1 | r$e_2 == 1 | r$e_3 == 1 ~ 1,
                         TRUE ~ 0)
  ######Employment Vulnerability#####
  # ## HH with at least one adult (18+) unemployed and seeking work
  
  r$l_1 <- ifelse(r$unemployed_adults > 0, 1,0)
  # 
  ##Age dependency ratio (i.e. ratio of the number of household members aged 15 or below and above and aged 60 or above 
  ##to the number of household members between the ages of 16 and 59)
  # r$adp <- (r$tot_girls + r$tot_boys)/ r$tot_adults
  # 
  # r$l_1 <- case_when(r$adp <= 0.8 ~ 0,
  #                       r$adp > 0.8 ~ 1,
  #                       TRUE ~ NA_real_)
  # 
  
  # % of HHs who earned income from precarious or unstable sources in the 30 days prior to data collection
  # HH reports it did not earn income from the following sources in the 30 days prior to data collection:
  
  
  r$l2_i <-  r$primary_livelihood.agriculture
  r$l2_ii <- r$primary_livelihood.cash_assistance
  r$l2_iii <- r$primary_livelihood.charity_assistance
  r$l2_iv <- r$primary_livelihood.community_support
  r$l2_v <- r$primary_livelihood.daily_work
  r$l2_vi <- r$primary_livelihood.illegal_activites
  r$l2_vii <- r$primary_livelihood.loans
  r$l2_viii <- r$primary_livelihood.remittances
  r$l2_ix<- r$primary_livelihood.sale_of_hh_assets
  r$l2_x <- r$primary_livelihood.selling_assistance
  r$l2_xi <- r$primary_livelihood.savings
  r$l2_xii <- r$primary_livelihood.zakat
  r$l2_xiii <- r$primary_livelihood.social_service
  
  
  r$l_2 <- case_when(r$l2_i == 1| r$l2_ii == 1 | r$l2_iii == 1 | r$l2_iv == 1 | r$l2_v == 1 | r$l2_vi == 1 | r$l2_vii == 1 | r$l2_viii == 1 |
                       r$l2_ix == 1| r$l2_x == 1 | r$l2_xi == 1 | r$l2_xii == 1 | r$l2_xiii == 1 ~ 1,
                     r$l2_i == 0| r$l2_ii == 0 | r$l2_iii == 0 | r$l2_iv == 0 | r$l2_v == 0 | r$l2_vi == 0 | r$l2_vii == 0 | r$l2_viii == 0 |
                       r$l2_ix == 0| r$l2_x == 0 | r$l2_xi == 0 | r$l2_xii == 0 | r$l2_xiii == 0 ~ 0,
                     TRUE ~ NA_real_)
  
  
  r$liv_vul <- case_when(r$l_1 == 1 | r$l_2 == 1 ~ 1,
                         TRUE ~ 0)
  
  ####### Food Security Vulnerability¨########
  # HHs who reported they either:
  #   - Ate less OR
  # - Went the entire day without eating
  
  
  r$f_1 <- case_when(r$ate_less == "yes" | r$day_without_eating == "yes" ~ 1,
                     TRUE ~ 0)
  
  r$food_vul <- r$f_1
  ########################Protection Vulnerabilities#####################
  # % of HHs with any member reportedly experiencing psychosocial distress (self-diagnosed)
  # HHs who reported at least one member has shown signs of psychosocial distress
  
  r$p_1 <- ifelse(r$hh_member_distress == "yes", 1,0)
  
  
  # % of HHs reporting no awareness of medical, legal or mental health and psychosocial services in case of GBV
  # HHs who reported they are not aware of medical, legal or mental health and psychosocial services in case of GBV
  #% of HHs reporting availability of PSS services in case of GBV by type of service
  
  # r$p_2 <- case_when(r$type_of_pss.legal_services == 1 | r$type_of_pss.medical_services == 1 |r$type_of_pss.psychosocial_services == 1 ~ 0,
  #                   r$type_of_pss.legal_services == 0 | r$type_of_pss.medical_services == 0 |r$type_of_pss.psychosocial_services == 0 ~ 1,
  #                   TRUE ~ NA_real_)
  
  # % of HHs with at least one child engaged in labor
  # HHs who reported at least one child has been engaged in labor outside the home in the 30 days prior to data collection
  
  r$p_3 <- case_when(r$under_18_working == "yes" ~ 1,
                     r$under_18_working == "no"  ~ 0,
                     TRUE ~ NA_real_)
  
  # % of HHs in which women and girls avoid areas because they feel unsafe there
  # HHs who reported women and girls avoid certain areas in their area because they feel unsafe
  
  r$p_4 <- ifelse(r$women_feel_unsafe == "yes" , 1, 0)
  
  
  r$prot_vul <- case_when(r$p_1 == 1 |r$p_3 == 1 | r$p_4 == 1 ~ 1,
                          TRUE ~ 0)
  
  # r$prot_vul <- case_when(r$p_1 == 1 | r$p_2 == 1 | r$p_3 == 1 | r$p_4 == 1 ~ 1,
  #                         TRUE ~ 0)
  ##########################Health Vulnerabilities#####################
  # % of HHs with at least one member who has a chronic disease
  # HH reports at least one member has a chronic disease
  
  r$h_1 <- case_when(r$chronic_illness == "yes" ~ 1,
                     r$chronic_illness == "no" ~ 0,
                     TRUE ~ NA_real_)
  
  # % of HHs with at least one member who faced difficulties in accessing health services in the 3 months prior to data collection
  r$h_2 <- case_when(r$health_barriers == "yes" ~ 1,
                     r$health_barriers == "no" ~ 0,
                     TRUE ~ NA_real_)
  # % of HHs with women and girls of reproductive age (15-49) who do not have access to specialized reproductive and health services 
  # r$h_3 <- case_when(r$availability_pss_women == "yes" ~ 0,
  #                   r$availability_pss_women == "no" ~ 1,
  #                   TRUE ~ NA_real_)
  
  r$hlth_vul <- case_when(r$h_1 == 1 | r$h_2 == 1 ~ 1,
                          TRUE ~ 0)
  
  #################################Shelter / Housing Vulnerability###############
  # % of HHs living under critical shelter conditions
  # HHs who reported their shelter is:
  #   - Tent
  # - Unfinished abandoned building
  # - Collective shelter
  # - Makeshift shelter
  
  r$s_1 <- ifelse(
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
  
  # % of HHs experiencing overcrowding
  # HHs who reported more than 3 members / bedroom or sleeping area
  
  # r <-within(r, {s2_i<-ifelse(num_of_rooms!=0,hh_size/num_of_rooms,0) })
  # 
  # r$s_2 <- case_when(r$s2_i <= 3 ~ 0,
  #                     r$s2_i > 3  ~ 1,
  #                     TRUE ~ NA_real_)
  
  
  r$s_2 <- case_when(r$sleeping_living_room == "yes" ~ 1, TRUE ~ 0)
  
  # % of HHs reporting risk of eviction
  
  r$s_3 <- case_when(r$hh_risk_eviction == "yes" ~ 1,
                     r$hh_risk_eviction == "no" ~ 0,
                     TRUE ~ NA_real_)
  
  # % of HHs without a secure occupancy arrangement for their current shelter
  # HHs who reported their current occupancy arrangement is:
  #   - Hosted without rent (by family, friends, institution)
  # - No occupancy agreement / squatting
  
  r$s_4 <- case_when( r$occupancy_status %in% c("hosted_without_rent","squatting") ~ 1,
                      r$occupancy_status %in% c("ownership","rented") ~ 0,
                      TRUE ~ NA_real_)
  
  r$shltr_vul <- case_when(r$s_1 == 1 | r$s_2 == 1 | r$s_3 == 1 | r$s_4 == 1 ~ 1,
                           TRUE ~ 0)
  ##################################WASH Vulnerabilities##########################################
  # % of HHs with limited access to a sufficient quantity of water for drinking and domestic purposes
  # Not enough water for any of the five purposes (e.g. drinking, cooking, personal hygiene, domestic hygiene, other domestic purposes)
  r$w_1 <- ifelse(
    r$sufficient_water_drinking == "no" |
      r$sufficient_water_cooking == "no" |
      r$sufficient_water_hygiene_personal == "no" |
      r$sufficient_water_hygiene_domestic == "no" |
      r$sufficient_water_other_water == "no",
    1,
    0
  )
  
  # % of HHs affected by flooding
  # HHs who reported that their area has seen flooding at least once in the past three years
  
  r$w_2 <- case_when(r$num_of_floods > 0 ~ 1, 
                     r$num_of_floods == 0 ~ 0,
                     TRUE ~ NA_real_)
  
  # % of HHs with limited access to  improved solid waste services
  # "HHs who report that their solid waste is disposed by:
  # - Openly dumped on premises OR
  # - Dumped in the area OR
  # - Openly dumped on premises OR 
  # - Burned on premise"
  r$w_3 <- case_when(r$solid_waste_disposal %in% c("openly_dumped_premises", "dumped_in_area", "burned_on_premises") ~ 1,
                     TRUE ~ 0)
  r$wash_vul <- case_when(r$w_1 == 1 | r$w_2 == 1 | r$w_3 == 1 ~ 1,
                          TRUE ~ 0)
  
  #############################Monetary Resources Vulnerability########################################
  # % of HHs experiencing poverty 
  # Any HH spending 80% or more of their expenses on food or water should be considered vulnerable 
  
  ##Average HH expenditure share by type of expenditure
  r$food_exp <-ifelse(r$food_exp >= r$tot_expenses, NA, r$food_exp)
  r$food_share <- round((as.numeric(r$food_exp)/ as.numeric(r$tot_expenses))*100, 1)
  r$m_i <- ifelse(r$food_share > 100, NA, 
                  r$food_share)
  
  
  r$water_exp <-ifelse(r$water_exp >= r$tot_expenses, NA, r$water_exp)
  r$water_share <- round((as.numeric(r$water_exp)/ as.numeric(r$tot_expenses))*100, 1)
  
  r$m_ii <- ifelse(r$water_share > 100, NA, 
                   r$water_share)
  
  
  r$m_1 <- case_when(r$m_i >= 80 | r$m_ii >= 80 ~ 1,
                     TRUE ~ 0)
  r$pov_vul <- r$m_1
  
  # % of HHs who have accrued debt to meet household needs
  # HH reporting one of the primary reasons for taking on debt: 
  #   - education
  # - basic household expenditure
  # - healthcare
  # - food
  
  r$m_2 <- case_when(r$reasons_for_debt %in% c("healthcare", "food", "education", "basic_household_expenditures") ~ 1, 
                     r$reasons_for_debt %in% c("business_related","major_purchase","income_generating_activites","clothing_or_NFI", "reconstruction", "weddings") ~ 0,
                     TRUE ~ NA_real_)
  
  r$pov_vul <- case_when(r$m_1 == 1 | r$m_2 == 1 ~ 1,
                         TRUE ~ 0)
  
  
  
  # Calculate what percent of HHs are assessed to be vulnerable in at least one dimension
  r <- r %>% 
    mutate(total_vul = rowSums(select(., ends_with('_vul'))))
  
  r$vul <- case_when( r$total_vul >= 1 ~ 1,  TRUE ~ 0)
  
  
  return(r)
}


# library(lubridate)
# library(openxlsx)
# # write.xlsx(df_new, (file = sprintf("output/df_new_%s.xlsx", today())))
# write.xlsx(response_with_composites, (file = sprintf("output/df_new_r_%s.xlsx", today())))
