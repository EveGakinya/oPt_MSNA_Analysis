# LOAD QUESTIONNAIRE
questions <- read.csv("input/questionnaire/kobo_questions.csv", 
                      stringsAsFactors=T, check.names=T)
colnames(questions)[1] <- "type"


choices <- read.csv("input/questionnaire/kobo_choices.csv", 
                    stringsAsFactors=F, check.names=T)
colnames(choices)[1] <- "list_name"

# LOAD SAMPLINGFRAMES AND LOOKUP-TABLES
cluster_lookup_table <- read.csv("input/lookup_tables/combined_sample_ids.csv", 
                                 stringsAsFactors=F, check.names=F)
cluster_lookup_table <- dplyr::distinct(cluster_lookup_table)
#write.csv(cluster_lookup_table, "input/combined_sample_ids.csv")
#cluster_lookup_table <- read.csv("input/combined_sample_ids.csv", 
#                                 stringsAsFactors=F, check.names=F)

lookup_table <- read.csv("input/lookup_tables/lookup_table_names.csv", stringsAsFactors = F, sep = ";")


samplingframe <- load_samplingframe("./input/sampling_frame/strata_population.csv")


# LOAD DATA AND MERGE REPRESENTATIVE AND INDICATIVE DATA
#data <- xlsform_fill(questions, choices, 500)
response <- read.csv("Input/datasets/cleaned/hh_dataset.csv", sep = ";",
                              stringsAsFactors=F, check.names=T,
                              na.strings = c("", " ", "NA", "#N/A", "N/A"))
loop <- read.csv("Input/datasets/cleaned/ind_dataset.csv", sep = ";",
                 stringsAsFactors=F, check.names=T,
                 na.strings = c("", " ", "NA", "#N/A", "N/A"))

names(response)[names(response) == 'ï..X_uuid'] <- "X_uuid"
names(loop)[names(loop) == "ï..X_uuid"] <- "X_uuid"

#add HNO stratas and governorates to dataset
response$governorate_gaza <- case_when(response$locality_gaza == "beit_hanun" | 
                                  response$locality_gaza == "beit_lahiya" | 
                                  response$locality_gaza == "jabalya" | 
                                  response$locality_gaza == "jabalya_camp" | 
                                  response$locality_gaza == "umm_naser" ~ "north_gaza",
                                  response$locality_gaza == "gaza" | 
                                  response$locality_gaza == "shati_camp" | 
                                  response$locality_gaza == "al_mughraqa" |
                                  response$locality_gaza == "madinat_ezahra" |
                                  response$locality_gaza == "juhor_deik" ~ "gaza",
                                  response$locality_gaza == "deir_balah" | 
                                  response$locality_gaza == "deir_balah_camp" |
                                  response$locality_gaza == "an_nuseirat" | 
                                  response$locality_gaza == "nuseirat_camp" | 
                                  response$locality_gaza == "az_zawayda" | 
                                  response$locality_gaza == "al_bureij" | 
                                  response$locality_gaza == "bureij_camp" |
                                  response$locality_gaza == "al_maghazi" |
                                  response$locality_gaza == "maghazi_camp" |
                                  response$locality_gaza == "al_musaddar" |
                                  response$locality_gaza == "wadi_salqa" ~ "deir_balah",
                                  response$locality_gaza == "khan_yunis" |
                                  response$locality_gaza == "khan_yunis_camp" |
                                  response$locality_gaza == "al_qarara" |
                                  response$locality_gaza == "bani_suheila" |
                                  response$locality_gaza == "abasan_kabira" |
                                  response$locality_gaza == "abasan_jadida" |
                                  response$locality_gaza == "khuza" |
                                  response$locality_gaza == "al_fukhari" ~ "khan_yunis",
                                  response$locality_gaza == "rafah" |
                                  response$locality_gaza == "rafah_camp" |
                                  response$locality_gaza == "an_naser" |
                                  response$locality_gaza == "ash_shoka" ~ "rafah",
                                )


response$hno_strata <- case_when(response$region == "ej" ~ "ej",
                                 response$region == "H2" ~ "h2",
                                 response$oslo_area == "A" & response$strata != "camps_wb" ~ "area_ab",
                                 response$oslo_area == "B" & response$strata != "camps_wb" ~ "area_ab",
                                 response$oslo_area == "C" & response$strata != "camps_wb" ~ "area_c",
                                 response$governorate_gaza == "deir_balah" ~ "deir_balah",
                                 response$governorate_gaza == "gaza" ~ "gaza",
                                 response$governorate_gaza == "khan_yunis" ~ "khan_yunis",
                                 response$governorate_gaza == "north_gaza" ~ "north_gaza",
                                 response$governorate_gaza == "rafah" ~ "rafah"
)

response$governorate <- case_when(response$region == "ej" ~ "ej",
                                  response$region == "H2" ~ "h2",
                                  response$region == "west_bank" & response$oslo_area == "C" ~ response$governorate_wb,
                                  response$region == "west_bank" & response$oslo_area != "C" ~ "area_ab",
                                  response$region == "gaza" ~ response$governorate_gaza
)


response <- response[moveme(names(response), "governorate_gaza before governorate_wb")]
response <- response[moveme(names(response), "hno_strata after strata")]
#write.xlsx(response, 'output/response_governorate.xlsx')

# MERGE QUESTIONNAIRES
questionnaire <- load_questionnaire(response,questions,choices, choices.label.column.to.use = "name")

