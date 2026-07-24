#Data Science 1859 Team Project
#Team 6 - Heather Craig, Amanda Illeperuma, Edward Li & Jayati Mishra

# Reading in data and basic info
given_data <- read.csv("project_data.csv")
head(given_data)
summary(given_data)
dim(given_data)
colnames(given_data)

# Making new DF with only variables of interest
key_variables<- given_data[c("Subject","Pittsburgh.Sleep.Quality.Index.Score","Epworth.Sleepiness.Scale", "Berlin.Sleepiness.Scale","Athens.Insomnia.Scale","SF36.PCS","SF36.MCS","Age","Gender","BMI","Time.from.transplant","Liver.Diagnosis","Recurrence.of.disease","Rejection.graft.dysfunction","Any.fibrosis","Renal.Failure","Depression","Corticoid")]

# Renaming column names
colnames(key_variables) <- c("Subject","PSQI", "ESS", "BSS", "AIS", "SF36_PCS", "SF36_MCS","Age", "Gender", "BMI", "TransplantTime", "LiverDiagnosis", "DiseaseRecurrence",  "Rejection", "Fibrosis", "RenalFailure", "Depression", "Corticosteroid")


# Derived Variables
#converting PSQI (score of 4+), ESS (10+) and AIS (5+) to binary (sleep disturbance =1)
key_variables$PSQI_binary <- ifelse(key_variables$PSQI > 4, 1, 0)
key_variables$ESS_binary <- ifelse(key_variables$ESS > 10, 1, 0)
key_variables$AIS_binary <- ifelse(key_variables$AIS > 5, 1, 0)


# Checking total counts
sleep_counts <- data.frame(
  Variable = c("PSQI", "AIS", "ESS", "BSS"),
  No = c(
    sum(key_variables$PSQI_binary == 0, na.rm = TRUE),
    sum(key_variables$AIS_binary == 0, na.rm = TRUE),
    sum(key_variables$ESS_binary == 0, na.rm = TRUE),
    sum(key_variables$BSS == 0, na.rm = TRUE)
  ),
  Yes = c(
    sum(key_variables$PSQI_binary == 1, na.rm = TRUE),
    sum(key_variables$AIS_binary == 1, na.rm = TRUE),
    sum(key_variables$ESS_binary == 1, na.rm = TRUE),
    sum(key_variables$BSS == 1, na.rm = TRUE)
  ),
  Missing = c(
    sum(is.na(key_variables$PSQI_binary)),
    sum(is.na(key_variables$AIS_binary)),
    sum(is.na(key_variables$ESS_binary)),
    sum(is.na(key_variables$BSS))
  )
)

# Add row and column totals
sleep_counts$Total <- rowSums(
  sleep_counts[, c("No", "Yes", "Missing")]
)
sleep_counts <- rbind(
  sleep_counts,
  data.frame(
    Variable = "Total",
    No = sum(sleep_counts$No),
    Yes = sum(sleep_counts$Yes),
    Missing = sum(sleep_counts$Missing),
    Total = sum(sleep_counts$Total)
  )
)

print(sleep_counts)


write.csv(key_variables, "key_variables.csv", row.names = FALSE)
