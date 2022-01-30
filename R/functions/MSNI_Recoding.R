library(readr)
library(magrittr)
library(tidyverse)

recoding_msni <- function(df, loop) {

df <- read_delim("Input/datasets/cleaned/hh_dataset.csv",
                         ";", escape_double = FALSE, trim_ws = TRUE)
loop <- read_delim("Input/datasets/cleaned/ind_dataset.csv",
                               ";", escape_double = FALSE, trim_ws = TRUE)


########################### Capacity Gap #####################################

df$cg_1 <- case_when(df$reasons_for_debt %in% c("healthcare", "food") ~ 1, T ~ 0)

df$cg_2 <- case_when(df$primary_livelihood.charity_assistance == 1 ~ 1, T ~ 0)


##% HH relying on stress / crisis / emergency strategies to cope with a lack of food or money to buy it
df$stress <-
  ifelse(
    df$coping_selling_properties %in% c("no_already_did", "yes") |
      df$coping_food_credit %in% c("no_already_did", "yes") |
      df$coping_reducing_expenditure %in% c("no_already_did", "yes"), 
    1,
    0  
  )

df$crisis <-
  ifelse(
    df$coping_selling_tranport %in% c("no_already_did", "yes") |
      df$coping_changing_residency %in% c("no_already_did", "yes") |
      df$coping_child_labour %in% c("no_already_did", "yes"),
    1,
    0
  )

df$emergency <-
  ifelse(
    df$coping_children_dropout %in% c("no_already_did", "yes") |
      df$coping_risky_behaviour %in% c("no_already_did", "yes") |
      df$coping_migration %in% c("no_already_did", "yes") |
      df$coping_forced_marriage %in% c("no_already_did", "yes"),
    1,
    0
  )

df$cg_3 <- case_when(df$crisis == 1 & df$emergency == 0 ~ 1, T ~ 0)



# Livelihoods Coping Strategy (LCS
df$cg_4 <- case_when(df$water_coping_mechanism_g.spent_more_on_water == 1 | df$water_coping_mechanism_g.water_on_credit == 1 |
                     df$water_coping_mechanism_g.reduced_drinking_water == 1 | df$water_coping_mechanism_g.modified_hygiene == 1 |
                     df$water_coping_mechanism_g.drank_cleaning_water == 1 ~ 1,
                     df$water_coping_mechanism_g.no_coping_needed_used == 1 | df$water_coping_mechanism_g.drank_stored_water == 1 ~ 0,
                     TRUE ~ NA_real_)



df$cg_any <-case_when(df$cg_1 == 1 | df$cg_2 == 1 | df$cg_3 == 1 | df$cg_4 == 1 ~ 1, TRUE ~ 0)
df$cg_all <-case_when(df$cg_1 == 1 & df$cg_2 == 1 & df$cg_3 == 1 & df$cg_4 == 1 ~ 1, TRUE ~ 0)


########################### Vulnerabilities ##################################

df$vul_1 <- case_when(df$hhh == "yes" & df$gender_respondent == "female"| df$gender_hhh == "female" ~ 1,
                                TRUE ~ 0)

#% of households with at least one member with a chronic illness
df$vul_2 <- case_when(df$chronic_illness == "yes" ~ 1,
                      df$chronic_illness == "no" ~ 0,
                      TRUE ~ NA_real_)

#of households with at least one pregnant or lactating female
df$vul_3 <- case_when(df$preg_lactating == "yes" ~ 1,
                      df$preg_lactating == "no" ~ 0,
                      TRUE ~ NA_real_)

##Age dependency ratio (i.e. ratio of the number of household members aged 15 or below and above and aged 60 or above 
##to the number of household members between the ages of 16 and 59)
df$adp <- (df$tot_girls + df$tot_boys)/ df$tot_adults

df$vul_4 <- case_when(df$adp <= 0.8 ~ 0,
                      df$adp > 0.8 ~ 1,
                      TRUE ~ NA_real_)

#% of households without access to soap at home
df$vul_5 <- case_when(df$latrine_items.soap == 0 ~ 1,
                      df$latrine_items.soap == 1 ~ 0,
                      TRUE ~ NA_real_)

#% HH that can access a hospital within a 30min walk from dwellings
df$vul_6 <- case_when(df$distance_hospital < 30 ~ 0,
                      df$distance_hospital >= 30 ~ 1,
                      TRUE ~ NA_real_)

#% of HHs whose house is currently still damaged as a result of bombardment since 2014
df$vul_7 <- case_when(df$building_damage_level_2021_g %in% c("major_damage","minor_damage")~ 1,
                      is.na(df$building_damage_level_2021_g) ~ NA_real_,
                      TRUE ~ 0)

#% of households that have a standing demolition order
df$vul_8 <- case_when(df$demolition_order_wb == "yes" ~ 1,
                      df$demolition_order_wb == "no" ~ 0,
                      TRUE ~ NA_real_)

#% of households that report being at risk of eviction
df$vul_9 <- case_when(df$hh_risk_eviction == "yes" ~ 1,
                      df$hh_risk_eviction == "no" ~ 0,
                      TRUE ~ NA_real_)

#At least one member in the HH reporting lots of difficulties or cannot do at all in at least one domain
loop$hh_wd_new <- case_when(loop$difficulty_communicating %in% c("a_lot_of_difficulty","cannot_do_at_all")|
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
df$hh_with_disability <- loop$hh_wd_new[match(df$X_uuid, loop$X_uuid)]


### mutating indicator hp1_a :% HH with at least one individual with a disability
df$vul_10 <- case_when(df$hh_with_disability == 1 ~ 1,
                       TRUE ~ 0)

df$perc_non_critical_vul <- rowSums(df[, paste0("vul_", 2:10)] == 1, na.rm = T)/rowSums(!is.na(df[, paste0("vul_", 2:10)]), na.rm = T)

df$vul_score <- case_when(
  df$vul_1 == 1 | df$perc_non_critical_vul > 0.66 ~ 3,
  df$perc_non_critical_vul > 0.33 ~ 2,
  T ~ 1
)

df$lsg_vul <- case_when(df$vul_score >= 3 ~ 1, TRUE ~ 0)

########################### Education ########################################

df$edu_1 <- case_when( df$dropout_num == 0 ~ 0,
                       df$dropout_num >= 1 ~ 1,
                       TRUE ~ NA_real_)

# df$child_distress_per <- (df$child_distress_number / df$tot_children)*100

df$edu_2 <- case_when(df$child_distress_number == 0 ~ 0,
                      df$child_distress_number >= 1 ~ 1,
                      TRUE ~ NA_real_)

df$edu_3 <- case_when(df$disabled_school_challenges.no_teachers == 1| df$disabled_school_challenges.classrons_not_adapted == 1 |
                        df$disabled_school_challenges.infrastructure_not_adapted == 1 | df$disabled_school_challenges.no_access_to_distance_learning == 1 |
                        df$disabled_school_challenges.unsafe_at_school == 1 | df$disabled_school_challenges.unsafe_commuting_to_schl == 1 ~ 1,
                      df$disabled_school_challenges.curriculum_not_adapted == 1 | df$disabled_school_challenges.no_separation_of_diasabled == 1 ~1,
                      df$disabled_school_challenges.no_support_to_cwd == 1 | df$disabled_school_challenges.bullying == 1 |
                      df$disabled_school_challenges.social_stigma == 1 ~ 0,
                      TRUE ~ NA_real_)
library(dplyr)

df <- df %>% 
  rowwise() %>% 
  mutate(school_aged_children = sum(hh_size_boys_5_10,hh_size_boys_11_15,hh_size_boys_16_17,hh_size_girls_5_10,hh_size_girls_11_15,hh_size_girls_16_17, na.rm = F))

df$remote_learning_per <- df$remote_learning/df$school_aged_children

df$edu_4 <- case_when(df$remote_learning_per < 100 ~ 0,
                      df$remote_learning_per >= 100 ~ 1,
                      TRUE ~ NA_real_)


df$edu_5 <- case_when(df$school_safety == "very_safe" | df$school_safety == "safe" ~ 0,
                   df$school_safety == "unsafe" | df$school_safety == "very_unsafe"  ~ 1,
                   TRUE ~ 0)

df$perc_non_critical_edu <- rowSums(df[, paste0("edu_", 1:5)] == 1, na.rm = T)/rowSums(!is.na(df[, paste0("edu_", 1:5)]), na.rm = T)

df$edu_score <- case_when(
                        df$perc_non_critical_edu > 0.66 ~ 3,
                        df$perc_non_critical_edu > 0.33 ~ 2,
                        T ~ 1
                        )

df$lsg_edu <- case_when(df$school_aged_children == 0 ~ NA_real_,
                        df$edu_score >= 3 ~ 1, T ~ 0)

########################### Livelihoods ######################################

# "HH reporting one of the primary reasons for taking on debt: 
# - education
# - basic household expenditure
# - healthcare
# - food"


df$liv_1 <- case_when(df$reasons_for_debt %in% c("healthcare", "food", "education") ~ 1, 
                      df$reasons_for_debt %in% c("business_related","major_purchase","income_generating_activites","clothing_or_NFI", "reconstruction", "weddings",
                                                 "decline_to_answer", "do_not_know", "other", "basic_household_expenditures") ~ 0,
                      df$how_much_debt == 0 ~ 0,
                      TRUE ~ NA_real_)


df$liv_2 <- case_when(df$tot_income <= 500 ~ 1,
                      df$tot_income > 500 ~ 0,
                      TRUE ~ NA_real_)

df<-within(df, {debt_per_person <-ifelse(hh_size!=0,how_much_debt/hh_size,0) })



df$liv_3 <- case_when(df$debt_per_person >= 3000 ~ 1,
                      df$debt_per_person < 3000 ~ 0,
                      TRUE ~ NA_real_)

df$liv_4 <- case_when(df$unemployed_adults >= 1 ~ 1,
                      df$unemployed_adults == 0 ~ 0,
                      TRUE ~ NA_real_)

df$perc_non_critical_liv <- rowSums(df[, paste0("liv_", 2:4)] == 1, na.rm = T)/rowSums(!is.na(df[, paste0("liv_", 2:4)]), na.rm = T)


df$liv_score <- case_when(df$liv_1 == 1 ~ 4,
                          df$perc_non_critical_liv > 0.66 ~ 3,
                          df$perc_non_critical_liv > 0.33 ~ 2,
                          T ~ 1)


df$lsg_liv <- case_when(df$liv_score >= 3 ~ 1, T ~ 0)


########################### Food Security ####################################

##Food Security
df$fcs <- 
  (as.numeric(df$cereals)*2) +(as.numeric(df$nuts_seed)*3) +(as.numeric(df$milk_dairy)*4) + (as.numeric(df$meat)*4)+ 
  as.numeric(df$vegetables) + as.numeric(df$fruits) + (as.numeric(df$oil_fats)*0.5) + (as.numeric(df$sweets)*0.5)

df$fcs_category <- case_when(df$fcs <= 21 ~ "Poor",
                             df$fcs >21 & df$fcs <= 35 ~ "Borderline",
                             T ~ "Acceptable")

df$fs_1 <- case_when(df$fcs_category == "Poor" |df$fcs_category == "Borderline" ~ 1, TRUE ~ 0)


##% HH relying on stress / crisis / emergency strategies to cope with a lack of food or money to buy it
df$stress <-
  ifelse(
    df$coping_selling_properties %in% c("no_already_did", "yes") |
      df$coping_food_credit %in% c("no_already_did", "yes") |
      df$coping_reducing_expenditure %in% c("no_already_did", "yes"), 
    1,
    0  
  )

df$crisis <-
  ifelse(
    df$coping_selling_tranport %in% c("no_already_did", "yes") |
      df$coping_changing_residency %in% c("no_already_did", "yes") |
      df$coping_child_labour %in% c("no_already_did", "yes"),
    1,
    0
  )

df$emergency <-
  ifelse(
    df$coping_children_dropout %in% c("no_already_did", "yes") |
      df$coping_risky_behaviour %in% c("no_already_did", "yes") |
      df$coping_migration %in% c("no_already_did", "yes") |
      df$coping_forced_marriage %in% c("no_already_did", "yes"),
    1,
    0
  )

df$fs_2 <- case_when(df$crisis == 1| df$emergency == 1 ~ 1, TRUE ~ 0)


##Average HH expenditure share by type of expenditure
df$food_exp <-ifelse(df$food_exp >= df$tot_expenses, NA, df$food_exp)
df$food_share <- round((as.numeric(df$food_exp)/ as.numeric(df$tot_expenses)), 1)
df$fs_3 <- case_when(df$food_share < 65 ~ 0,
                     df$food_share <= 65 ~ 1,
                     TRUE ~ NA_real_)

df$perc_non_critical_fs <- rowSums(df[, paste0("fs_", 2:3)] == 1, na.rm = T)/rowSums(!is.na(df[, paste0("fs_", 2:3)]), na.rm = T)

df$fs_score <- case_when(df$fs_1 == 1 ~ 4,
                         df$perc_non_critical_fs > 0.66 ~ 3,
                         df$perc_non_critical_fs > 0.33 ~ 2,
                         T ~ 1)


df$lsg_fs <- case_when(df$fs_score >= 3 ~ 1, T ~ 0)


########################### Protection #######################################
df$tot_distress <- df$adult_distress_number + df$child_distress_number


df$per_distress <- round((df$tot_distress/df$hh_size) *100,1)

df$prt_1 <- case_when(df$per_distress <= 40 ~ 0,
                      df$hh_member_distress == "no" ~ 0,
                      df$per_distress > 40 ~ 1,
                      TRUE ~ NA_real_)


df$unsafe_locations_tot <- as.numeric(df$unsafe_locations.latrines_bathing_facilities)+ as.numeric(df$unsafe_locations.water_points) + as.numeric(df$unsafe_locations.distribution_areas) + 
                           as.numeric(df$unsafe_locations.settlements_checkpoints) + as.numeric(df$unsafe_locations.markets) + as.numeric(df$unsafe_locations.at_the_workplace) + 
                           as.numeric(df$unsafe_locations.social_community_areas) + as.numeric(df$unsafe_locations.public_transport) + as.numeric(df$unsafe_locations.route_to_school) + 
                           as.numeric(df$unsafe_locations.route_to_communty_centres) + as.numeric(df$unsafe_locations.seeking_humanitarian_aid)


df$prt_2 <- case_when(
                    df$unsafe_locations_tot >= 2 ~ 1,
                    TRUE ~ 0)



df$prt_3 <- case_when(df$hh_risk_eviction == "yes" ~ 1,
                      df$hh_risk_eviction == "no" ~ 0,
                      TRUE ~ NA_real_)

# df$prt_4 <- case_when(df$security_concerns_disabled.physical_harassment == 1 | df$security_concerns_disabled.verbal_harassment == 1 | df$security_concerns_disabled.sexual_harassment == 1 |
#                         df$security_concerns_disabled.forced_marriage == 1 | df$security_concerns_disabled.exploited_labor == 1 | df$security_concerns_disabled.exploited_sex == 1 |
#                         df$security_concerns_disabled.robberies == 1 | df$security_concerns_disabled.discrimination == 1 | df$security_concerns_disabled.detained == 1 |
#                         df$security_concerns_disabled.fgm == 1 | df$security_concerns_disabled.settler_violence == 1 | df$security_concerns_disabled.tear_gas == 1 | 
#                         df$security_concerns_disabled.checkpoints == 1 | df$security_concerns_disabled.kidnapped ==1 | df$security_concerns_disabled.killed == 1 | 
#                         df$security_concerns_disabled.mines == 1 | df$security_concerns_disabled.explosive_hazard == 1 ~ 1,
#                         df$security_concerns_disabled.none == 1 | df$security_concerns_disabled.do_not_know == 1 | df$security_concerns_disabled.threatened_with_violence == 1 |
#                         df$security_concerns_disabled.abroad_work == 1| df$security_concerns_disabled.bullying == 1 | df$security_concerns_disabled.corporal_punishment ==1 |
#                         df$security_concerns_disabled.begging == 1 ~ 0,
#                         TRUE ~ NA_real_)

# Shelter not impacted by bombardment OR 
# not reporting any current damage from bombardment
# HH reporting damage from bombardment
##% of HHs reporting damage to their current shelter as a result of the recent conflict 

df$sp6_ii <- case_when(df$building_damage_level_2021_g %in% c("major_damage","minor_damage")~ 1,
                        is.na(df$building_damage_level_2021_g) ~ NA_real_,
                      TRUE ~ 0)
#% of HHs whose house is currently still damaged as a result of bombardment since 2014
df$sp6_iii <-
  case_when(
    df$building_damage_level_current_g %in% c("minor", "major", "moderate", "completely") ~ 1,
    df$building_damage_level_current_g == "none" ~ 0,
    TRUE ~ NA_real_)


df$prt_4 <- case_when(df$sp6_ii == 1 | df$sp6_iii == 1 ~ 1,
                      df$sp6_ii == 0 | df$sp6_iii == 0 ~ 0,
                      TRUE ~ NA_real_)


# HH reports having experienced threats or violent/destructive acts by non-Palestinian communities living in their area in the past 30 days
# HH does NOT report having experienced threats or violent/destructive acts by non-Palestinian communities living in their area in the past 30 days

# % of households that report having experienced threats or violent/destructive acts by non-Palestinian communities in their area in the past 30 days
# df$prt_5 <- case_when(df$settler_threats_wb == "yes" ~ 1,
#                       df$settler_threats_wb == "no" ~ 0,
#                       TRUE ~ NA_real_)

# HH was never displaced and is currently not hosting displaced individuals	"HH temporarily or permanently displaced OR
# hosting displaced individuals"

##% of HHs displaced as a result of the most recent conflict in Gaza (starting on the 11th of May 2021)
df$hhd1 <- case_when(df$permanent_location_g == "no" ~ 1,
                     df$permanent_location_g == "yes" ~ 0, 
                    TRUE ~ NA_real_)
##% of HHs currently hosting displaced individuals
df$hhd3 <- case_when(df$currently_hosting_displaced_g == "yes" ~ 1,
                    df$currently_hosting_displaced_g == "no" ~ 0,
                    TRUE ~ NA_real_)

# df$prt_6 <- case_when(df$hhd1 == 1 | df$hhd3 == 1 ~ 1,
#                       df$hhd1 == 0 | df$hhd3 == 0 ~ 0,
#                       TRUE ~ NA_real_)
# "All domains are no difficulties OR
# No domain is a lot of difficulties or cannot do at all and 1, 2 or 3 domains are some difficulties"	"More than 3 domains are some difficulties OR 
# at least one domain is a lot of difficulty or cannot do at all"

#At least one member in the HH reporting lots of difficulties or cannot do at all in at least one domain
# loop$hh_wd <- case_when(loop$difficulty_communicating %in% c("a_lot_of_difficulty","cannot_do_at_all")|
#                           loop$difficulty_hearing %in% c("a_lot_of_difficulty","cannot_do_at_all")|
#                           loop$difficulty_remembering %in% c("a_lot_of_difficulty","cannot_do_at_all")|
#                           loop$difficulty_washing %in% c("a_lot_of_difficulty","cannot_do_at_all")|
#                           loop$difficulty_walking %in% c("a_lot_of_difficulty","cannot_do_at_all")|
#                           loop$difficulty_seeing %in% c("a_lot_of_difficulty","cannot_do_at_all") ~ 1,
#                         loop$difficulty_communicating %in% c("some_difficulty","no_difficulty") &
#                           loop$difficulty_hearing %in% c("some_difficulty","no_difficulty") &
#                           loop$difficulty_remembering %in% c("some_difficulty","no_difficulty") &
#                           loop$difficulty_washing %in% c("some_difficulty","no_difficulty") &
#                           loop$difficulty_walking %in% c("some_difficulty","no_difficulty") &
#                           loop$difficulty_seeing %in% c("some_difficulty","no_difficulty") ~ 0,
#                         TRUE ~ 0)
# 
# ### mutating the variable hh_wd in the parent data 
# df$hh_with_disability <- loop$hh_wd[match(df$X_uuid, loop$X_uuid)]
# 

### mutating indicator hp1_a :% HH with at least one individual with a disability
df$prt_5 <- case_when(df$hh_with_disability == 1 ~ 1,
                       TRUE ~ 0)


# HH reported that women of reproductive age have access to specialized reproductive health services	
# HH reported that women of reproductive age do NOT have access to specialized reproductive health services


df$prt_6 <- case_when(df$availability_reproductive_services == "yes" ~ 0, 
                      df$availability_reproductive_services == "no" ~ 1,
                      TRUE ~ NA_real_)

df$perc_non_critical_prt <- rowSums(df[, paste0("prt_", 2:6)] == 1, na.rm = T)/rowSums(!is.na(df[, paste0("prt_", 2:6)]), na.rm = T)

df$prt_score <- case_when(
                          df$prt_1 == 1 | df$perc_non_critical_prt > 0.66 ~ 3,
                          df$perc_non_critical_prt > 0.33 ~ 2,
                          T ~ 1)

df$lsg_prt <- case_when(df$prt_score >= 3 ~ 1, T ~ 0)

########################### Health ###########################################
# HH not facing any access barriers when trying to access health services	HH facing access barriers when trying to access health services
df$hlth_1 <- case_when(df$health_barriers == "yes" ~ 1,
                       df$health_barriers == "no" ~ 0,
                          TRUE ~ NA_real_)

# < 20% of HH members are showing signs of psychosocial distress	> = 20% of HH members are showing signs of psychosocial distress

df$hlth_2 <- case_when(df$per_distress <= 40 ~ 0,
                       df$hh_member_distress == "no" ~ 0,
                       df$per_distress > 40 ~ 1,
                       TRUE ~ NA_real_)

# < 20% of HH members are not willing to be vaccinated against COVID-19	> = 20% of HH members are not willing to be vaccinated against COVID-19

df$per_unvaccinated <- round((df$vaccine_num_not_willing/df$tot_adults)*100,1)
df$per_unvaccinated <- ifelse(df$per_unvaccinated > 100, NA,df$per_unvaccinated)


df$hlth_3 <- case_when(df$per_unvaccinated < 20 ~ 0,
                       df$per_unvaccinated >= 20 ~ 1,
                       df$vaccine_willingness == "yes" ~ 0,
                       TRUE ~ NA_real_)
# HH can access primary healthcare within 30min walk from dwellings	HH cannot access primary healthcare within 30min walk from dwellings
df$hlth_4 <- case_when(df$distance_hospital <= 30 ~ 0,
                       df$distance_hospital > 30 ~ 1,
                       TRUE ~ 0)

df$perc_non_critical_hlth <- rowSums(df[, paste0("hlth_", 1:4)] == 1, na.rm = T)/rowSums(!is.na(df[, paste0("hlth_", 1:4)]), na.rm = T)

df$hlth_score <- case_when(
                          df$perc_non_critical_hlth > 0.66 ~ 3,
                          df$perc_non_critical_hlth > 0.33 ~ 2,
                          T ~ 1)

df$lsg_hlth <- case_when(df$hlth_score >= 3 ~ 1, T ~ 0)

########################### SNFI #############################################
# "House
# Apartment"	0"Tent
# Unfinished abandoned building
# Collective shelter
# Makeshift shelter"



df$snfi_1 <- case_when(df$shelter_type %in% c("unfinished_abandoned_building","tent","collective_shelter","makeshift_shelter") ~ 1,
                   df$shelter_type %in% c("house", "apartment") ~ 0,
                   TRUE ~ NA_real_)

# Number of persons per bedroom =< 3	Number of persons per bedroom > 3
#Average number of household members per room
df<-within(df, {avg_person_per_bedroom <-ifelse(num_bedrooms!=0,hh_size/num_bedrooms,0) })

df$snfi_2 <- case_when(df$avg_person_per_bedroom <= 3 ~ 0,
                       df$avg_person_per_bedroom > 3 ~ 1,
                       TRUE ~ NA_real_)                  

# Less than three types of shelter damages reported	More than three types of shelter damage reported
count_issues_shelter <- function(df) {
  diff <-  df[c(which(startsWith(names(df), "shelter_issues.")))]                   
  diff$ns_issues <- rowSums(diff, na.rm = F)
  diff <- diff[, c("ns_issues")]
  df <- cbind(df, diff)
  return(df)
}
df <- count_issues_shelter(df)

df$snfi_3 <- case_when(df$ns_issues <= 3 ~ 0,
                       df$ns_issues > 3 ~ 1,
                       TRUE ~ NA_real_)


# "No damage
# Moderate damage
# Minor damage"	Major damage or complete destruction
##% of HHs reporting damage to their current shelter as a result of the recent conflict 

df$snfi_4 <- case_when(df$building_damage_level_2021_g %in% c("major_damage","minor_damage")~ 1,
                       is.na(df$building_damage_level_2021_g) ~ NA_real_,
                       TRUE ~ 0)

# "No repair needed
# Some capacity
# A lot of capacity"	"Very limited capacity
# No capacity"
df$snfi_5 <-
  case_when(
    df$building_damage_repair_capacity_g %in% c("none","very_limited") ~ 1,
    df$building_damage_repair_capacity_g %in% c("no_repair_needed","some_capacity","alot_of_capacity") ~ 0,
    TRUE ~ NA_real_)

df$perc_non_critical_snfi <- rowSums(df[, paste0("snfi_", 2:5)] == 1, na.rm = T)/rowSums(!is.na(df[, paste0("snfi_", 2:5)]), na.rm = T)

df$snfi_score <- case_when(
  df$snfi_1 == 1 ~ 4,
  df$perc_non_critical_snfi > 0.66 ~ 3,
  df$perc_non_critical_snfi > 0.33 ~ 2,
  T ~ 1
)

df$lsg_snfi <- case_when(df$snfi_score >= 3 ~ 1, T ~ 0)

########################### WASH #############################################
#110 % of HHs with access to an improved water source
df$water_source <- case_when(df$drinking_water_source %in% 
                               c("water_trucking",
                                 "unprot_well",
                                 "unprot_tank",
                                 "unprot_spring",
                                 "surface_water") ~ "unimproved",
                                  T ~ "improved")

df$wash_1 <- case_when(df$water_source == "unimproved" ~ 1, TRUE ~ 0)

df$wash_2 <- ifelse(
    df$sufficient_water_drinking == "yes" &
    df$sufficient_water_cooking == "yes" &
    df$sufficient_water_hygiene_personal == "yes" &
    df$sufficient_water_hygiene_domestic == "yes" &
    df$sufficient_water_other_water == "yes",
  0,
  1
)
# Improved sanitation facility	Unimproved sanitation facility
df$wash_3 <- case_when(
  df$latrine_type %in% c(
    "vip_pit",
    "flush",
    "pit_slab"
  ) ~ 0,
  TRUE ~ 1
)
# #116 % of settlements with water management services available
# df$wash_4 <- case_when(df$floods_shelter_effects.none == 1 | df$floods_shelter_effects.shelter_leaking == 1 ~ 0,
#                     df$floods_shelter_effects.shelter_surroundings_damage == 1| df$floods_shelter_effects.furniture_damage == 1|
#                     df$floods_shelter_effects.shelter_items_damage == 1 | df$floods_shelter_effects.complete_destruction == 1 ~ 1,
#                     TRUE ~ NA_real_)
#115 % of settlements with waste management services available

df$wash_4 <- case_when(df$solid_waste_disposal %in% c("municipality_waste_system","dumping_location") ~ 0,
                       df$solid_waste_disposal %in% c("openly_dumped_premises", "dumped_in_area","burned_on_premises","burned_covered_premises")~1,
                       TRUE ~ NA_real_)
# of people with limited access to  improved to sanitation services

# df$wash_6 <- case_when(df$latrine_waste_drainage %in% c("sewage_system","covered_septic") ~ 0,
#                        df$latrine_waste_drainage %in% c("handdug_hole","open_area") ~ 1,
#                        TRUE ~ NA_real_)

df$perc_non_critical_wash <- rowSums(df[, paste0("wash_", 1:4)] == 1, na.rm = T)/rowSums(!is.na(df[, paste0("wash_", 1:4)]), na.rm = T)

df$wash_score <- case_when(
                           df$perc_non_critical_wash > 0.66 ~ 3,
                           df$perc_non_critical_wash > 0.33 ~ 2,
                           T ~ 1)


df$lsg_wash <- case_when(df$wash_score %in% c("3", "4", "4+") ~ 1,
                         T ~ 0)

########################### Combination of LSGs ##############################

df$lsg_edu_vul <- case_when(df$lsg_edu == 1 & df$lsg_vul == 1 ~ 1, is.na(df$lsg_edu) ~ NA_real_,T ~ 0)
df$lsg_liv_vul <- case_when(df$lsg_liv == 1 & df$lsg_vul == 1 ~ 1, is.na(df$lsg_liv) ~ NA_real_,T ~ 0)
df$lsg_fs_vul <- case_when(df$lsg_fs == 1 & df$lsg_vul == 1 ~ 1, is.na(df$lsg_fs) ~ NA_real_,T ~ 0)
df$lsg_prt_vul <- case_when(df$lsg_prt == 1 & df$lsg_vul == 1 ~ 1, is.na(df$lsg_prt) ~ NA_real_,T ~ 0)
df$lsg_hlth_vul <- case_when(df$lsg_hlth == 1 & df$lsg_vul == 1 ~ 1, is.na(df$lsg_hlth) ~ NA_real_,T ~ 0)
df$lsg_snfi_vul <- case_when(df$lsg_snfi == 1 & df$lsg_vul == 1 ~ 1, is.na(df$lsg_snfi) ~ NA_real_,T ~ 0)
df$lsg_wash_vul <- case_when(df$lsg_wash == 1 & df$lsg_vul == 1 ~ 1, is.na(df$lsg_wash) ~ NA_real_,T ~ 0)

df$msni_score <- case_when(
  df$wash_score == "4+" ~ "4+",
  rowSums(df[, c("edu_score", "liv_score", "fs_score", "prt_score", "hlth_score", "snfi_score")] == 4, na.rm = T) > 0 | df$wash_score == "4" ~ "4",
  rowSums(df[, c("edu_score", "liv_score", "fs_score", "prt_score", "hlth_score", "snfi_score")] == 3, na.rm = T) > 0 | df$wash_score == "3" ~ "3",
  rowSums(df[, c("edu_score", "liv_score", "fs_score", "prt_score", "hlth_score", "snfi_score")] == 2, na.rm = T) > 0 | df$wash_score == "2" ~ "2",
  T ~ "1" 
)


df$severity_score_1 <- case_when(df$msni_score == 1 ~ 1, TRUE ~ 0)
df$severity_score_2 <- case_when(df$msni_score == 2 ~ 1, TRUE ~ 0)
df$severity_score_3 <- case_when(df$msni_score == 3 ~ 1, TRUE ~ 0)
df$severity_score_4 <- case_when(df$msni_score == 4 ~ 1, TRUE ~ 0)
df$severity_score_5 <- case_when(df$msni_score == "4+" ~ 1, TRUE ~ 0)


df$lsg_all <- case_when(df$msni_score %in% c("3", "4", "4+") ~ 1, T ~ 0)

return(df)
}
