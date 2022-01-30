
recoding_pop_groups <- function(r,loop) {

r <- response
### Female headed household
r$gender_disagg <- case_when(r$hhh == "yes" & r$gender_respondent == "female"| 
                               r$gender_hhh == "female" ~ "female_headed",
                             TRUE ~ "male_headed")

r$chronic_disagg <- case_when( r$chronic_illness == "yes" ~ "with_chronic", TRUE ~ "no_chronic")

###Gazans displaced by most recent escalation
r$gazans_displaced <- case_when(r$permanent_location_g == "no" ~ "displaced",
                                r$permanent_location_g %in% c("yes", "do_not_know", "decline_to_answer") ~ "non_displaced",
                                is.na(r$permanent_location_g) ~ NA_character_)
###Non refugees
r$refugee_disagg <- ifelse(r$refugee_status == "no", "non_refugee", "refugee")




###HHs whose primary source of income is agriculture, livestock or herding
r$agricultural_hh <- case_when(r$primary_livelihood.agriculture == 1 ~ "agricultural",
                               TRUE ~ "non_agricultural")

### HHs whose shelter has been damaged or destroyed in the recent escalation
r$recent_shelter_damage <- case_when(r$building_damage_level_2021_g %in% c("major_damage","minor_damage")~ "damaged",
                                     r$region == "gaza" & is.na(r$building_damage_level_2021_g) ~ "not_damaged",
                                     TRUE ~ NA_character_)


###In-camp refugees and Out-camp refugee
r$camp_refugee_disagg <- case_when(r$refugee_status == "yes" & grepl("camp", r$strata) ~ "in_camp_refugee",
                               r$refugee_status == "yes" & !grepl("camp", r$strata) ~ "out_camp_refugee",
                               TRUE ~ "non_refugee")

r$location <- ifelse(r$region == "gaza", "Gaza", "West_Bank")

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
r$disability_disagg <- case_when(r$hh_with_disability == 1 ~ "with_disability",
                     TRUE ~ "without_disability")


return(r)
}
