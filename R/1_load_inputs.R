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

lookup_table <- read.csv("input/lookup_tables/lookup_table_names.csv", stringsAsFactors = F)


samplingframe <- load_samplingframe("./input/sampling_frame/strata_population.csv")


# LOAD DATA AND MERGE REPRESENTATIVE AND INDICATIVE DATA
#data <- xlsform_fill(questions, choices, 500)
response <- read.csv("Input/datasets/cleaned/hh_dataset.csv", sep = ";",
                              stringsAsFactors=F, check.names=T,
                              na.strings = c("", " ", "NA", "#N/A", "N/A"))
loop <- read.csv("Input/datasets/cleaned/ind_dataset.csv", sep = ";",
                 stringsAsFactors=F, check.names=T,
                 na.strings = c("", " ", "NA", "#N/A", "N/A"))

#names(response)[names(response) == 'ï..X_uuid'] <- "X_uuid"
#names(loop)[names(loop) == "ï..X_uuid"] <- "X_submission__uuid"


