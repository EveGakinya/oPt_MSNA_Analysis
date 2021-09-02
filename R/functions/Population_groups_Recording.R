
recoding_pop_groups <- function(r) {

### Female headed household
r$female_headed <- case_when(r$hhh == "yes" & r$gender_respondent == "female"| 
                               r$gender_hhh == "female" ~ "female_headed",
                             TRUE ~ "male_headed")
###Gazans displaced by most recent escalation
r$gazans_displaced <- case_when(r$permanent_location_g == "no" ~ "displaced",
                                r$permanent_location_g %in% c("yes", "do_not_know", "decline_to_answer") ~ "non_displaced",
                                is.na(r$permanent_location_g) ~ NA_character_)
###Non refugees
r$refugee_status <- ifelse(r$refugee_status == "no", "non_refugee", "refugee")

###HHs whose primary source of income is agriculture, livestock or herding
r$agricultural_hh <- case_when(r$primary_livelihood.agriculture == 1 ~ "agricultural",
                               TRUE ~ "non_agricultural")

### HHs whose shelter has been damaged or destroyed in the recent escalation
r$recent_shelter_damage <- case_when(r$building_damage_level_2021_g %in% c("major_damage","minor_damage")~ "damaged",
                                     r$region == "gaza" & is.na(r$building_damage_level_2021_g) ~ "not_damaged",
                                     TRUE ~ NA_character_)

###In-camp refugees and Out-camp refugee
r$in_camp_refugee <- case_when(r$refugee_status == "yes" & grepl("camp", r$strata) ~ "in_camp_refugee",
                               r$refugee_status == "yes" & !grepl("camp", r$strata) ~ "out_camp_refugee",
                               TRUE ~ NA_character_)

return(r)
}
