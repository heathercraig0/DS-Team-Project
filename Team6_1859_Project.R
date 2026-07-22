#Data Science 1859 Team Project
#Team 6 - Heather Craig, Amanda Illeperuma, Edward Li & Jayati Mishra

# Reading in data and basic info
given_data <- read.csv("project_data.csv")
head(given_data)
summary(given_data)
dim(given_data)
colnames(given_data)

# Making new DF with only variables of interest
key_variables<- data[c("Pittsburgh.Sleep.Quality.Index.Score","Epworth.Sleepiness.Scale","Stanford.Sleepiness.Scale","Athens.Insomnia.Scale","SF36.PCS","SF36.MCS","Age","Gender","BMI","Time.from.transplant","Liver.Diagnosis","Recurrence.of.disease","Rejection.graft.dysfunction","Any.fibrosis","Renal.Failure","Depression","Corticoid")]

# Renaming column names
colnames(key_variables) <- c("PSQI", "Epworth", "Stanford", "AIS", "SF36_PCS", "SF36_MCS"," Age", "Gender", "BMI", "TransplantTime", "LiverDiagnosis", "DiseaseRecurrence",  "Rejection", "Fibrosis", "RenalFailure", "Depression", "Corticosteroid")

# Chekcing missing data
colSums(is.na(key_variables))

# Checking levels for categorical variables with <2 levels
write.csv(key_variables, "key_variables.csv", row.names = FALSE)


# Converting continuous sleep deprevation tests to categorical (sleep deprevation = 1) 

# Derived variables 

#total number of tests

#
