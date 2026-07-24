#Data Science 1859 Team Project
#Team 6 - Heather Craig, Amanda Illeperuma, Edward Li & Jayati Mishra

# Reading in data and basic info
given_data <- read.csv("project_data.csv")
head(given_data)
summary(given_data)
dim(given_data)
colnames(given_data)

# Making new DF with only variables of interest
key_variables<- given_data[c("Pittsburgh.Sleep.Quality.Index.Score","Epworth.Sleepiness.Scale", "Berlin.Sleepiness.Scale","Athens.Insomnia.Scale","SF36.PCS","SF36.MCS","Age","Gender","BMI","Time.from.transplant","Liver.Diagnosis","Recurrence.of.disease","Rejection.graft.dysfunction","Any.fibrosis","Renal.Failure","Depression","Corticoid")]

# Renaming column names
colnames(key_variables) <- c("PSQI", "ESS", "BSS", "AIS", "SF36_PCS", "SF36_MCS","Age", "Gender", "BMI", "TransplantTime", "LiverDiagnosis", "DiseaseRecurrence",  "Rejection", "Fibrosis", "RenalFailure", "Depression", "Corticosteroid")


# Derived Variables
#converting PSQI (score of 4+), ESS (10+) and AIS (5+) to binary (sleep disturbance =1)


# Checking missing data
colSums(is.na(key_variables))


# attach the Subject ID from raw data
key_variables$Subject <- given_data$Subject
key_variables <- key_variables[c("Subject", setdiff(names(key_variables), "Subject"))]


write.csv(key_variables, "key_variables.csv", row.names = FALSE)


