#Data Science 1859 Team Project
#Team 6 - Heather Craig, Amanda Illeperuma, Edward Li & Jayati Mishra

# ------------------------------------------------------------------------------
# Reading in data and basic info
# ------------------------------------------------------------------------------
given_data <- read.csv("project_data.csv")
head(given_data)
summary(given_data)
dim(given_data)
colnames(given_data)

# ------------------------------------------------------------------------------
# Making new DF with only variables of interest
# ------------------------------------------------------------------------------
key_variables<- given_data[c("Subject","Pittsburgh.Sleep.Quality.Index.Score",
                             "Epworth.Sleepiness.Scale", 
                             "Berlin.Sleepiness.Scale","Athens.Insomnia.Scale",
                             "SF36.PCS","SF36.MCS","Age","Gender","BMI",
                             "Time.from.transplant","Liver.Diagnosis",
                             "Recurrence.of.disease",
                             "Rejection.graft.dysfunction","Any.fibrosis",
                             "Renal.Failure","Depression","Corticoid")]

# Renaming column names
colnames(key_variables) <- c("Subject","PSQI", "ESS", "BSS", "AIS", "SF36_PCS", 
                             "SF36_MCS","Age", "Gender", "BMI", "TransplantTime", 
                             "LiverDiagnosis", "DiseaseRecurrence",  "Rejection", 
                             "Fibrosis", "RenalFailure", "Depression", 
                             "Corticosteroid")

# ------------------------------------------------------------------------------
# Deriving binary variables
# ------------------------------------------------------------------------------
# converting PSQI (score of 4+), ESS (10+) and AIS (5+) to 
# binary (sleep disturbance =1)
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

# ------------------------------------------------------------------------------
# Data quality checks & description of relevant data
# ------------------------------------------------------------------------------
head(key_variables)
summary(key_variables)
dim(key_variables)
colnames(key_variables)

# variable to hold continuous variables for simplicity
continuous_vars <- c("Age", "BMI", "TransplantTime", "SF36_PCS", "SF36_MCS", 
                     "PSQI", "ESS", "AIS")

# empty vectors to collect results from each loop iteration
mean_vals   <- c()
sd_vals     <- c()
median_vals <- c()
iqr_vals    <- c()
na_vals     <- c()

# looping through each variable for descriptive statistics  
for (v in continuous_vars) {
  x <- key_variables[[v]]   # pull out the column by name
  mean_vals   <- c(mean_vals,   mean(x, na.rm = TRUE))
  sd_vals     <- c(sd_vals,     sd(x, na.rm = TRUE))
  median_vals <- c(median_vals, median(x, na.rm = TRUE))
  iqr_vals    <- c(iqr_vals,    IQR(x, na.rm = TRUE))
  na_vals     <- c(na_vals,     sum(is.na(x)))
}

continuous_summary <- data.frame(
  Variable = continuous_vars,
  Mean     = round(mean_vals, 2),
  SD       = round(sd_vals, 2),
  Median   = round(meCI fordian_vals, 2),
  IQR      = round(iqr_vals, 2),
  Missing  = na_vals,
  N_valid  = nrow(key_variables) - na_vals
)

print(continuous_summary)

#---------------------------------------------------

#---------------------------------------------------
# Define the labels for each categorical variable
# (names match the actual numeric codes in the data, as text)
labels_list <- list(
  Gender             = c("1" = "Male", "2" = "Female"),
  LiverDiagnosis     = c("1" = "Hepatitis C", "2" = "Hepatitis B", "3" = 
                           "PSC/PBC/AIH",
                         "4" = "Alcohol-related", "5" = "Other/heterogeneous"),
  DiseaseRecurrence  = c("0" = "No", "1" = "Yes"),
  Rejection          = c("0" = "No", "1" = "Yes"),
  Fibrosis           = c("0" = "No", "1" = "Yes"),
  RenalFailure       = c("0" = "No", "1" = "Yes"),
  Depression         = c("0" = "No", "1" = "Yes"),
  Corticosteroid     = c("0" = "No", "1" = "Yes"),
  BSS                = c("0" = "Negative", "1" = "Positive")
)

categorical_vars <- names(labels_list)

# empty data frame to build up row by row across all variables
categorical_summary <- data.frame(Variable = character(),
                                  Category = character(),
                                  n = integer(),
                                  Percent = numeric(),
                                  stringsAsFactors = FALSE)

for (v in categorical_vars) {
  x <- key_variables[[v]]
  
  counts <- table(x)                          # valid (non-missing) counts only
  pct    <- round(prop.table(counts) * 100, 1)
  
# translate the numeric codes (as text) into labels using the lookup above
  category_labels <- labels_list[[v]][names(counts)]
  
 # build this variable's rows
  var_rows <- data.frame(
    Variable = c(v, rep("", length(counts) - 1)),  # only show variable name once
    Category = category_labels,
    n        = as.integer(counts),
    Percent  = as.numeric(pct),
    stringsAsFactors = FALSE
  )
  
  categorical_summary <- rbind(categorical_summary, var_rows)
}

print(categorical_summary)


# ----------------------------------------------------------------------------
# Q1 Prevalence for each of the 4 sleep instruments and overall
# ----------------------------------------------------------------------------
# Prevalence of each of the instruments
# Denominator = No + Yes (i.e. Not Total since Missing is excluded from the denominator)
# Using binom.test() for 95% CI.

# --- PSQI ---
psqi_row <- sleep_counts[sleep_counts$Variable == "PSQI", ]
psqi_n <- psqi_row$Yes + psqi_row$No
psqi_test <- binom.test(psqi_row$Yes, psqi_n)
psqi_test 

# --- AIS ---
ais_row <- sleep_counts[sleep_counts$Variable == "AIS", ]
ais_n <- ais_row$Yes + ais_row$No
ais_test <- binom.test(ais_row$Yes, ais_n)
ais_test

# --- ESS ---
ess_row <- sleep_counts[sleep_counts$Variable == "ESS", ]
ess_n <- ess_row$Yes + ess_row$No
ess_test <- binom.test(ess_row$Yes, ess_n)
ess_test

# --- BSS ---
bss_row <- sleep_counts[sleep_counts$Variable == "BSS", ]
bss_n <- bss_row$Yes + bss_row$No
bss_test <- binom.test(bss_row$Yes, bss_n)
bss_test

# --- Prevalence summary table ---
prevalence_table <- data.frame(
  Variable    = c("PSQI", "AIS", "ESS", "BSS"),
  n_valid     = c(psqi_n, ais_n, ess_n, bss_n),
  n_disturbed = c(psqi_row$Yes, ais_row$Yes, ess_row$Yes, bss_row$Yes),
  Prevalence  = round(c(psqi_test$estimate, ais_test$estimate, ess_test$estimate, 
                        bss_test$estimate) * 100, 1),
  CI_Lower    = round(c(psqi_test$conf.int[1], ais_test$conf.int[1], 
                        ess_test$conf.int[1], bss_test$conf.int[1]) * 100, 1),
  CI_Upper    = round(c(psqi_test$conf.int[2], ais_test$conf.int[2], 
                        ess_test$conf.int[2], bss_test$conf.int[2]) * 100, 1)
)

print(prevalence_table)

# Overall (composite) prevalence
# A subject classified as "disturbed" if >=50% of their COMPLETED sleep instruments flagged disturbance. 
# Denominator per subject = number of instruments they actually completed
# (missing instruments are excluded, not counted as "not disturbed").

# Pull the 4 binary sleep flags together (BSS is already binary, no _binary suffix)
sleep_binary_matrix <- key_variables[c("PSQI_binary", "ESS_binary", "AIS_binary", 
                                       "BSS")]

# Total tests completed per subject (count of non-missing instruments, out of 4)
key_variables$Total_tests_done <- rowSums(!is.na(sleep_binary_matrix))

# Number of completed tests that showed disturbance
key_variables$Tests_disturbed <- rowSums(sleep_binary_matrix, na.rm = TRUE)

# Percentage of completed tests showing disturbance
# (subjects with 0 completed tests get NA, not 0 - percentage is undefined, not zero)
key_variables$Pct_disturbed <- ifelse(
  key_variables$Total_tests_done == 0,
  NA,
  key_variables$Tests_disturbed / key_variables$Total_tests_done
)

# Final composite binary: >=50% cutoff (ties at exactly 50% count as disturbed)
key_variables$Sleep_disturbed_composite <- ifelse(
  is.na(key_variables$Pct_disturbed), NA,
  ifelse(key_variables$Pct_disturbed >= 0.5, 1, 0)
)

# Check number of completed instruments and composite classification 
table(key_variables$Total_tests_done, useNA = "ifany")
table(key_variables$Sleep_disturbed_composite, useNA = "ifany")

# Calcualte overall prevelence 
composite_valid <- key_variables[!is.na(key_variables$Sleep_disturbed_composite), ]
composite_n <- nrow(composite_valid)
composite_x <- sum(composite_valid$Sleep_disturbed_composite)

composite_test <- binom.test(composite_x, composite_n)
composite_test



# ----------------------------------------------------------------------------
# Q1 Predictive Models 
# ----------------------------------------------------------------------------
# Convert categorical predictors to factors
key_variables$Gender <- factor(key_variables$Gender, 
                               labels = c("Male", "Female"))
key_variables$LiverDiagnosis <- factor(
  key_variables$LiverDiagnosis,
  labels = c("Hepatitis C", "Hepatitis B", "PSC/PBC/AIH",
             "Alcohol-related", "Other/heterogeneous")
)
key_variables$DiseaseRecurrence <- factor(key_variables$DiseaseRecurrence, 
                                          labels = c("No", "Yes"))
key_variables$Rejection <- factor(key_variables$Rejection, 
                                  labels = c("No", "Yes"))
key_variables$Fibrosis <- factor(key_variables$Fibrosis, 
                                 labels = c("No", "Yes"))
key_variables$RenalFailure <- factor(key_variables$RenalFailure, 
                                     labels = c("No", "Yes"))
key_variables$Depression <- factor(key_variables$Depression, 
                                   labels = c("No", "Yes"))
key_variables$Corticosteroid <- factor(key_variables$Corticosteroid, 
                                       labels = c("No", "Yes"))

# Restricting the Number of Predictors;  LiverDiagnosis has 5 levels = 4 DoF, all other categorical and continuous predictors contribute 1 DoF
# Total candidate predictor DoF = 14.

# PSQI Model  - Linear Regression w Stepwise Backward Model Selection
# ----------------------------------------------------------------------------
library(MASS)
#Max DoF is 12 (183/15 = 12.2), so LiverDiagnosis was removed to reduce the candidate model from 14 to 10 DoF.# Removing missing data 
PSQI_data <- na.omit(key_variables[, c(
  "PSQI",
  "Age",
  "Gender",
  "BMI",
  "TransplantTime",
  "DiseaseRecurrence",
  "Rejection",
  "Fibrosis",
  "RenalFailure",
  "Depression",
  "Corticosteroid"
)])
# Full model creation 
model_PSQI_full <- lm(PSQI ~ Age + Gender + BMI + TransplantTime
                      + DiseaseRecurrence + Rejection + Fibrosis 
                      + RenalFailure +Depression + Corticosteroid,
                 data = PSQI_data)
# Stepback best model generation 
PSQI_stepback_model <- stepAIC(
  model_PSQI_full,
  direction = "backward",
  trace = FALSE
)
summary(PSQI_stepback_model)


# ESS Model  - Linear Regression w Stepwise Backward Model Selection
# ----------------------------------------------------------------------------
# All 14 candidate predictor DoF can be included in the initial full model
ESS_data <- na.omit(key_variables[, c(
  "ESS",
  "Age",
  "Gender",
  "BMI",
  "TransplantTime",
  "LiverDiagnosis",
  "DiseaseRecurrence",
  "Rejection",
  "Fibrosis",
  "RenalFailure",
  "Depression",
  "Corticosteroid"
)])
model_ESS_full <- lm(ESS ~ Age + Gender + BMI + TransplantTime + LiverDiagnosis +
                  DiseaseRecurrence + Rejection + Fibrosis + RenalFailure +
                  Depression + Corticosteroid,
                data = ESS_data)
ESS_stepback_model <- stepAIC(
  model_ESS_full,
  direction = "backward",
  trace = FALSE
)
summary(ESS_stepback_model)


# AIS Model  - Linear Regression w Stepwise Backward Model Selection
# ----------------------------------------------------------------------------
# All 14 candidate predictor DoF can be included in the initial full model
AIS_data <- na.omit(key_variables[, c(
  "AIS",
  "Age",
  "Gender",
  "BMI",
  "TransplantTime",
  "LiverDiagnosis",
  "DiseaseRecurrence",
  "Rejection",
  "Fibrosis",
  "RenalFailure",
  "Depression",
  "Corticosteroid"
)])
model_AIS_full <- lm(AIS ~ Age + Gender + BMI + TransplantTime + LiverDiagnosis +
                  DiseaseRecurrence + Rejection + Fibrosis + RenalFailure +
                  Depression + Corticosteroid,
                data = AIS_data)
AIS_stepback_model <- stepAIC(
  model_AIS_full,
  direction = "backward",
  trace = FALSE
)
summary(AIS_stepback_model)


# BSS Model  - Logistic Regression w Stepwise Backward Model Selection
# ----------------------------------------------------------------------------
# Max predictor DoF is 6 (102/15=6.8) so need to remove
BSS_data <- na.omit(key_variables[, c(
  "BSS",
  "Age",
  "Gender",
  "BMI",
  "DiseaseRecurrence",
  "Depression",
  "Corticosteroid"
)])

# Full candidate model using 6 predictor DoF
model_BSS_full <- glm(BSS ~ Age + Gender + BMI + DiseaseRecurrence +
                      Depression + Corticosteroid, data = BSS_data,
                      family = binomial
)

# Backward AIC selection
BSS_stepback_model <- stepAIC(
  model_BSS_full,
  direction = "backward",
  trace = FALSE
)

summary(BSS_stepback_model)
round(exp(coef(BSS_stepback_model)), 3)


# Overall Disturbance Model  - Logistic Regression w Stepwise Backward Model Selection
# ----------------------------------------------------------------------------
# Max 8 DoF (123/15), so need to remove some predictors for full model
Composite_data <- na.omit(key_variables[, c(
  "Sleep_disturbed_composite",
  "Age",
  "Gender",
  "BMI",
  "DiseaseRecurrence",
  "Rejection",
  "Fibrosis",
  "Depression",
  "Corticosteroid"
)])

model_Composite_full <- glm(
  Sleep_disturbed_composite ~ Age + Gender + BMI + DiseaseRecurrence + 
    Rejection + Fibrosis + Depression + Corticosteroid,
  data = Composite_data,
  family = binomial
)

Composite_stepback_model <- stepAIC(
  model_Composite_full,
  direction = "backward",
  trace = FALSE
)

summary(Composite_stepback_model)
round(exp(coef(Composite_stepback_model)), 3)

# Finding sample size used per model (after missing data was ommitted)
nrow(PSQI_data)
nrow(ESS_data)
nrow(AIS_data)
nrow(BSS_data)
nrow(Composite_data)


# Summary: predictor significance
# ----------------------------------------------------------------------------

psqi_coef <- round(summary(PSQI_stepback_model)$coefficients, 3)
print(psqi_coef)

ess_coef <- round(summary(ESS_stepback_model)$coefficients, 3)
print(ess_coef)

ais_coef <- round(summary(AIS_stepback_model)$coefficients, 3)
print(ais_coef)

bss_coef <- round(summary(BSS_stepback_model)$coefficients, 3)
print(bss_coef)
bss_or <- exp(coef(BSS_stepback_model))
print(round(bss_or, 3))

composite_coef <- round(summary(Composite_stepback_model)$coefficients, 3)
print(composite_coef)
composite_or <- round(exp(coef(Composite_stepback_model)), 3)
print(composite_or)


#sorry i have this here it was just easier for me to visualize, Ill remove before we submit - HC
write.csv(key_variables, "key_variables.csv", row.names = FALSE)



# ==============================================================================
# QUESTION 2: RELATIONSHIP BETWEEN SLEEP DISTURBANCE AND QUALITY OF LIFE
# ==============================================================================

# Identify the two quality-of-life outcomes.
# Higher SF36 scores indicate better quality of life.
qol_vars <- c("SF36_PCS", "SF36_MCS")

# PSQI, ESS and AIS are continuous sleep scores.
# Higher values indicate worse sleep.
continuous_sleep_vars <- c("PSQI", "ESS", "AIS")

# BSS is binary:
# 0 = low likelihood of sleep-disordered breathing
# 1 = high likelihood of sleep-disordered breathing
primary_sleep_vars <- c("PSQI", "ESS", "AIS", "BSS")

# These clinically defined binary variables will be used later for secondary
# comparisons between disturbed and non-disturbed patients.
binary_sleep_vars <- c(
  "PSQI_binary",
  "ESS_binary",
  "AIS_binary",
  "BSS"
)

# Select the variables needed for the initial Question 2 data check.
q2_variables <- c(
  "SF36_PCS",
  "SF36_MCS",
  "PSQI",
  "ESS",
  "AIS",
  "BSS"
)

# Verify that the selected variables have the expected data types and values.
str(key_variables[q2_variables])
summary(key_variables[q2_variables])

# Count valid and missing observations for each Question 2 variable.
q2_missing <- data.frame(
  Variable = q2_variables,
  Valid_n = sapply(
    key_variables[q2_variables],
    function(x) sum(!is.na(x))
  ),
  Missing_n = sapply(
    key_variables[q2_variables],
    function(x) sum(is.na(x))
  )
)

# Calculate the percentage missing out of all 268 patients.
q2_missing$Missing_Percent <- round(
  q2_missing$Missing_n / nrow(key_variables) * 100,
  1
)

print(q2_missing)

# ------------------------------------------------------------------------------
# Q2 SECTION 2: AVAILABLE SAMPLE SIZE FOR EACH SLEEP-QOL RELATIONSHIP
# ------------------------------------------------------------------------------

# Not every patient completed every sleep assessment. Therefore, each
# sleep-QoL analysis will have its own sample size rather than using all
# 268 patients.

pair_sample_sizes <- data.frame()

# Repeat the calculation for each of the four sleep measures and both
# quality-of-life outcomes.
for (sleep_var in primary_sleep_vars) {
  
  for (qol_var in qol_vars) {
    
    # Count patients who have valid values for both the selected sleep
    # measure and the selected quality-of-life outcome.
    available_n <- sum(
      complete.cases(
        key_variables[, c(sleep_var, qol_var)]
      )
    )
    
    # Add the result to the sample-size table.
    pair_sample_sizes <- rbind(
      pair_sample_sizes,
      data.frame(
        Sleep_Variable = sleep_var,
        QoL_Outcome = qol_var,
        Available_n = available_n
      )
    )
  }
}

print(pair_sample_sizes)

# ------------------------------------------------------------------------------
# Q2 SECTION 3: DESCRIBE PHYSICAL AND MENTAL QUALITY OF LIFE
# ------------------------------------------------------------------------------

# Create a function to calculate the main descriptive statistics for each
# quality-of-life outcome.
summarize_qol <- function(x) {
  
  data.frame(
    Valid_n = sum(!is.na(x)),
    Missing_n = sum(is.na(x)),
    Mean = round(mean(x, na.rm = TRUE), 2),
    SD = round(sd(x, na.rm = TRUE), 2),
    Median = round(median(x, na.rm = TRUE), 2),
    IQR = round(IQR(x, na.rm = TRUE), 2),
    Minimum = round(min(x, na.rm = TRUE), 2),
    Maximum = round(max(x, na.rm = TRUE), 2)
  )
}

# Apply the function separately to physical and mental quality of life.
qol_summary <- rbind(
  SF36_PCS = summarize_qol(key_variables$SF36_PCS),
  SF36_MCS = summarize_qol(key_variables$SF36_MCS)
)

print(qol_summary)

# ------------------------------------------------------------------------------
# Q2 SECTION 4: EXAMINE THE DISTRIBUTIONS OF PCS AND MCS
# ------------------------------------------------------------------------------

# Display the two histograms side by side so their shapes can be compared.
par(mfrow = c(1, 2))

# Examine the distribution of physical quality-of-life scores.
hist(
  key_variables$SF36_PCS,
  main = "Distribution of Physical Quality of Life",
  xlab = "SF-36 PCS Score",
  ylab = "Number of Patients",
  col = "lightblue",
  border = "white",
  breaks = 10
)

# Add a vertical line showing the mean PCS score.
abline(
  v = mean(key_variables$SF36_PCS, na.rm = TRUE),
  col = "red",
  lwd = 2,
  lty = 2
)

# Examine the distribution of mental quality-of-life scores.
hist(
  key_variables$SF36_MCS,
  main = "Distribution of Mental Quality of Life",
  xlab = "SF-36 MCS Score",
  ylab = "Number of Patients",
  col = "lightgreen",
  border = "white",
  breaks = 10
)

# Add a vertical line showing the mean MCS score.
abline(
  v = mean(key_variables$SF36_MCS, na.rm = TRUE),
  col = "red",
  lwd = 2,
  lty = 2
)

# Return the graphics window to the normal one-plot layout.
par(mfrow = c(1, 1))
# ------------------------------------------------------------------------------
# Q2 SECTION 5: VISUALIZE CONTINUOUS SLEEP SCORES AND QUALITY OF LIFE
# ------------------------------------------------------------------------------

# Create a reusable function for plotting one continuous sleep score against
# one quality-of-life outcome.
plot_sleep_qol <- function(sleep_var, qol_var, sleep_label, qol_label) {
  
  # Retain only patients with valid values for both variables being plotted.
  # This allows each relationship to use all available observations without
  # incorrectly requiring patients to have completed all four sleep measures.
  plot_data <- key_variables[
    complete.cases(key_variables[, c(sleep_var, qol_var)]),
    c(sleep_var, qol_var)
  ]
  
  # Create the scatterplot.
  plot(
    plot_data[[sleep_var]],
    plot_data[[qol_var]],
    main = paste(sleep_label, "and", qol_label),
    xlab = sleep_label,
    ylab = qol_label,
    pch = 19,
    col = rgb(0.2, 0.4, 0.7, 0.45)
  )
  
  # Fit an unadjusted linear relationship for visualization only.
  visual_model <- lm(
    plot_data[[qol_var]] ~ plot_data[[sleep_var]]
  )
  
  # Add the fitted line to show the direction of the relationship.
  abline(
    visual_model,
    col = "red",
    lwd = 2
  )
}

# Display the six continuous sleep-QoL relationships together.
par(mfrow = c(2, 3))

# Physical quality-of-life relationships.
plot_sleep_qol(
  "PSQI", "SF36_PCS",
  "PSQI Score", "SF-36 PCS"
)

plot_sleep_qol(
  "ESS", "SF36_PCS",
  "ESS Score", "SF-36 PCS"
)

plot_sleep_qol(
  "AIS", "SF36_PCS",
  "AIS Score", "SF-36 PCS"
)

# Mental quality-of-life relationships.
plot_sleep_qol(
  "PSQI", "SF36_MCS",
  "PSQI Score", "SF-36 MCS"
)

plot_sleep_qol(
  "ESS", "SF36_MCS",
  "ESS Score", "SF-36 MCS"
)

plot_sleep_qol(
  "AIS", "SF36_MCS",
  "AIS Score", "SF-36 MCS"
)

# Return to the standard one-plot layout.
par(mfrow = c(1, 1))

# ------------------------------------------------------------------------------
# Q2 SECTION 6: UNADJUSTED CORRELATIONS BETWEEN SLEEP AND QUALITY OF LIFE
# ------------------------------------------------------------------------------

# Create an empty data frame to collect the six correlation results:
# three continuous sleep measures examined against two QoL outcomes.
correlation_results <- data.frame()

for (sleep_var in continuous_sleep_vars) {
  
  for (qol_var in qol_vars) {
    
    # Keep only patients with valid values for the specific sleep measure
    # and quality-of-life outcome being analyzed.
    analysis_data <- key_variables[
      complete.cases(key_variables[, c(sleep_var, qol_var)]),
      c(sleep_var, qol_var)
    ]
    
    # Use Pearson correlation because both variables are numeric and the
    # scatterplots showed approximately linear relationships.
    correlation_test <- cor.test(
      analysis_data[[sleep_var]],
      analysis_data[[qol_var]],
      method = "pearson"
    )
    
    # Store the sample size, correlation estimate, 95% confidence interval
    # and original unadjusted p-value for each relationship.
    correlation_results <- rbind(
      correlation_results,
      data.frame(
        Sleep_Variable = sleep_var,
        QoL_Outcome = qol_var,
        N = nrow(analysis_data),
        Correlation = unname(correlation_test$estimate),
        CI_Lower = correlation_test$conf.int[1],
        CI_Upper = correlation_test$conf.int[2],
        P_Value = correlation_test$p.value
      )
    )
  }
}

# Adjust the six original p-values using the Holm method.
# This reduces the risk of false-positive conclusions from conducting several
# sleep-QoL correlation tests.
correlation_results$Holm_P <- p.adjust(
  correlation_results$P_Value,
  method = "holm"
)

# Round the correlation estimates and confidence intervals for presentation.
correlation_results$Correlation <- round(
  correlation_results$Correlation,
  3
)

correlation_results$CI_Lower <- round(
  correlation_results$CI_Lower,
  3
)

correlation_results$CI_Upper <- round(
  correlation_results$CI_Upper,
  3
)

# Create display versions of the p-values.
# Values below 0.0001 are reported as "<0.0001" rather than incorrectly
# appearing as exactly zero after rounding.
correlation_results$P_Value_Display <- ifelse(
  correlation_results$P_Value < 0.0001,
  "<0.0001",
  formatC(
    correlation_results$P_Value,
    format = "f",
    digits = 4
  )
)

correlation_results$Holm_P_Display <- ifelse(
  correlation_results$Holm_P < 0.0001,
  "<0.0001",
  formatC(
    correlation_results$Holm_P,
    format = "f",
    digits = 4
  )
)

# Select only the report-ready columns.
# The unrounded numeric p-values remain stored in correlation_results.
correlation_table <- correlation_results[, c(
  "Sleep_Variable",
  "QoL_Outcome",
  "N",
  "Correlation",
  "CI_Lower",
  "CI_Upper",
  "P_Value_Display",
  "Holm_P_Display"
)]

print(correlation_table)

# ------------------------------------------------------------------------------
# Q2 SECTION 7: VISUAL COMPARISON OF QOL BY SLEEP-DISTURBANCE STATUS
# ------------------------------------------------------------------------------

# Create a function to compare a QoL outcome between patients classified
# as not disturbed (0) and disturbed (1) for each sleep instrument.
plot_binary_qol <- function(
    group_var,
    qol_var,
    sleep_label,
    qol_label
) {
  
  # Use the available-case approach taught in Tutorial 10.
  # Only patients with valid values for this specific sleep classification
  # and QoL outcome are included in the plot.
  plot_data <- key_variables[
    complete.cases(key_variables[, c(group_var, qol_var)]),
    c(group_var, qol_var)
  ]
  
  # Create the boxplot using the approach demonstrated in Tutorial 9.
  # The plot shows the median, spread and possible unusual observations
  # within each sleep-disturbance group.
  boxplot(
    plot_data[[qol_var]] ~ plot_data[[group_var]],
    names = c("Not Disturbed", "Disturbed"),
    main = paste(qol_label, "by", sleep_label),
    xlab = sleep_label,
    ylab = qol_label,
    col = c("lightgrey", "salmon")
  )
}

# Display the eight comparisons together:
# four sleep classifications examined against two QoL outcomes.
par(mfrow = c(2, 4))

# Physical quality-of-life comparisons.
plot_binary_qol(
  "PSQI_binary",
  "SF36_PCS",
  "PSQI Status",
  "SF-36 PCS"
)

plot_binary_qol(
  "ESS_binary",
  "SF36_PCS",
  "ESS Status",
  "SF-36 PCS"
)

plot_binary_qol(
  "AIS_binary",
  "SF36_PCS",
  "AIS Status",
  "SF-36 PCS"
)

plot_binary_qol(
  "BSS",
  "SF36_PCS",
  "BSS Risk",
  "SF-36 PCS"
)

# Mental quality-of-life comparisons.
plot_binary_qol(
  "PSQI_binary",
  "SF36_MCS",
  "PSQI Status",
  "SF-36 MCS"
)

plot_binary_qol(
  "ESS_binary",
  "SF36_MCS",
  "ESS Status",
  "SF-36 MCS"
)

plot_binary_qol(
  "AIS_binary",
  "SF36_MCS",
  "AIS Status",
  "SF-36 MCS"
)

plot_binary_qol(
  "BSS",
  "SF36_MCS",
  "BSS Risk",
  "SF-36 MCS"
)

# Return to the normal single-plot layout.
par(mfrow = c(1, 1))

# ------------------------------------------------------------------------------
# Q2 SECTION 8: UNADJUSTED COMPARISON OF MEAN QOL BETWEEN SLEEP GROUPS
# ------------------------------------------------------------------------------

# This function compares mean QoL between patients classified as disturbed
# and not disturbed for one sleep instrument.
compare_qol_groups <- function(qol_var, group_var) {
  
  # Use the available-case approach from Tutorial 10.
  # Only patients with valid values for this particular sleep classification
  # and QoL outcome are included.
  analysis_data <- key_variables[
    complete.cases(key_variables[, c(qol_var, group_var)]),
    c(qol_var, group_var)
  ]
  
  # Separate QoL values according to sleep-disturbance status.
  not_disturbed <- analysis_data[
    analysis_data[[group_var]] == 0,
    qol_var
  ]
  
  disturbed <- analysis_data[
    analysis_data[[group_var]] == 1,
    qol_var
  ]
  
  # Use Welch's two-sample t-test.
  # Unlike the pooled t-test, Welch's test does not assume equal variances
  # between the disturbed and not-disturbed groups.
  test_result <- t.test(
    disturbed,
    not_disturbed,
    var.equal = FALSE
  )
  
  # The difference is calculated as disturbed minus not disturbed.
  # A negative value means that the disturbed group has lower mean QoL.
  data.frame(
    QoL_Outcome = qol_var,
    Sleep_Group = group_var,
    N_Not_Disturbed = length(not_disturbed),
    N_Disturbed = length(disturbed),
    Mean_Not_Disturbed = mean(not_disturbed),
    SD_Not_Disturbed = sd(not_disturbed),
    Mean_Disturbed = mean(disturbed),
    SD_Disturbed = sd(disturbed),
    Mean_Difference =
      mean(disturbed) - mean(not_disturbed),
    CI_Lower = test_result$conf.int[1],
    CI_Upper = test_result$conf.int[2],
    P_Value = test_result$p.value
  )
}

# Create an empty data frame to collect all eight comparisons.
group_comparison_results <- data.frame()

for (group_var in binary_sleep_vars) {
  
  for (qol_var in qol_vars) {
    
    group_comparison_results <- rbind(
      group_comparison_results,
      compare_qol_groups(
        qol_var,
        group_var
      )
    )
  }
}

# Adjust the eight original p-values using the Holm method demonstrated in
# Tutorial 9. This accounts for conducting several related comparisons.
group_comparison_results$Holm_P <- p.adjust(
  group_comparison_results$P_Value,
  method = "holm"
)

# Round descriptive statistics, estimated differences and confidence intervals
# only after all statistical calculations are complete.
columns_to_round <- c(
  "Mean_Not_Disturbed",
  "SD_Not_Disturbed",
  "Mean_Disturbed",
  "SD_Disturbed",
  "Mean_Difference",
  "CI_Lower",
  "CI_Upper"
)

group_comparison_results[columns_to_round] <- round(
  group_comparison_results[columns_to_round],
  2
)

# Create report-ready p-value columns.
# Very small p-values are shown as "<0.0001" rather than zero.
group_comparison_results$P_Value_Display <- ifelse(
  group_comparison_results$P_Value < 0.0001,
  "<0.0001",
  formatC(
    group_comparison_results$P_Value,
    format = "f",
    digits = 4
  )
)

group_comparison_results$Holm_P_Display <- ifelse(
  group_comparison_results$Holm_P < 0.0001,
  "<0.0001",
  formatC(
    group_comparison_results$Holm_P,
    format = "f",
    digits = 4
  )
)

# Select the columns needed for the report-ready comparison table.
group_comparison_table <- group_comparison_results[, c(
  "QoL_Outcome",
  "Sleep_Group",
  "N_Not_Disturbed",
  "N_Disturbed",
  "Mean_Not_Disturbed",
  "SD_Not_Disturbed",
  "Mean_Disturbed",
  "SD_Disturbed",
  "Mean_Difference",
  "CI_Lower",
  "CI_Upper",
  "P_Value_Display",
  "Holm_P_Display"
)]

print(group_comparison_table)

# ------------------------------------------------------------------------------
# Q2 SECTION 9: CHECK SAMPLE SIZE RELATIVE TO THE NUMBER OF PREDICTORS
# ------------------------------------------------------------------------------

# Define the demographic and clinical variables specified in the assignment.
covariate_names <- c(
  "Age",
  "Gender",
  "BMI",
  "TransplantTime",
  "LiverDiagnosis",
  "DiseaseRecurrence",
  "Rejection",
  "Fibrosis",
  "RenalFailure",
  "Depression",
  "Corticosteroid"
)

# Create an empty table to store the model-size assessment.
model_size_check <- data.frame()

for (sleep_var in primary_sleep_vars) {
  
  for (qol_var in qol_vars) {
    
    # Identify every variable required for this particular adjusted model.
    required_vars <- c(
      qol_var,
      sleep_var,
      covariate_names
    )
    
    # Use complete observations for all variables in the model, following the
    # complete-case approach discussed in Tutorials 8 and 10.
    model_data <- key_variables[
      complete.cases(key_variables[, required_vars]),
      required_vars
    ]
    
    # Construct the proposed full-model formula.
    candidate_formula <- reformulate(
      termlabels = c(sleep_var, covariate_names),
      response = qol_var
    )
    
    # Create the model matrix to count the actual number of coefficients.
    # This is important because a categorical variable such as liver diagnosis
    # creates several indicator coefficients.
    design_matrix <- model.matrix(
      candidate_formula,
      data = model_data
    )
    
    # Exclude the intercept when counting regression coefficients.
    number_of_coefficients <- ncol(design_matrix) - 1
    
    # Also record the number of named predictor terms.
    number_of_terms <- length(
      c(sleep_var, covariate_names)
    )
    
    # Store the sample size and evaluate the p < m/15 guideline from
    # Tutorial 9 using both the named terms and actual coefficients.
    model_size_check <- rbind(
      model_size_check,
      data.frame(
        QoL_Outcome = qol_var,
        Sleep_Variable = sleep_var,
        Complete_Case_N = nrow(model_data),
        Predictor_Terms = number_of_terms,
        Estimated_Coefficients = number_of_coefficients,
        N_Divided_By_15 = round(nrow(model_data) / 15, 2),
        Meets_Term_Rule =
          number_of_terms < (nrow(model_data) / 15),
        Meets_Coefficient_Rule =
          number_of_coefficients < (nrow(model_data) / 15)
      )
    )
  }
}

print(model_size_check)

# ------------------------------------------------------------------------------
# Q2 SECTION 10: SELECT THE ADJUSTED LINEAR REGRESSION MODELS
# ------------------------------------------------------------------------------

# Tutorial 10 used the stepAIC() function from the MASS package to remove
# predictors that do not improve a model enough.
if (!requireNamespace("MASS", quietly = TRUE)) {
  stop("Install the MASS package before running this section.")
}

# This list will store the final models so we can examine them later.
selected_qol_models <- list()

# This table will summarize how each model changed during selection.
selection_summary <- data.frame()

# Run the analysis separately for each sleep measure.
for (sleep_var in primary_sleep_vars) {
  
  # Analyze physical and mental quality of life separately.
  for (qol_var in qol_vars) {
    
    # List every variable needed for this model.
    required_vars <- c(
      qol_var,
      sleep_var,
      covariate_names
    )
    
    # Keep only patients who have values for the outcome, sleep measure
    # and every possible adjustment variable.
    #
    # Using one fixed dataset ensures that changes in AIC are caused by
    # changes in the predictors, not by changes in the patients included.
    model_data <- key_variables[
      complete.cases(key_variables[, required_vars]),
      required_vars
    ]
    
    # Create the full model.
    # It contains the sleep measure and all demographic and clinical variables.
    full_formula <- reformulate(
      termlabels = c(
        sleep_var,
        covariate_names
      ),
      response = qol_var
    )
    
    # Create the smallest model that is allowed.
    # The sleep measure must remain because it is the main variable needed
    # to answer Question 2.
    minimum_formula <- reformulate(
      termlabels = sleep_var,
      response = qol_var
    )
    
    # Fit the full linear regression model.
    full_model <- lm(
      full_formula,
      data = model_data
    )
    
    # Start with the full model and remove adjustment variables one at a time.
    # stepAIC() keeps a smaller model when removing variables improves or
    # sufficiently maintains its AIC.
    #
    # The sleep measure cannot be removed because it is included in the
    # minimum model.
    selected_model <- MASS::stepAIC(
      full_model,
      scope = list(
        lower = minimum_formula,
        upper = full_formula
      ),
      direction = "backward",
      trace = FALSE
    )
    
    # Give the model a clear name, such as SF36_PCS_PSQI.
    model_name <- paste(
      qol_var,
      sleep_var,
      sep = "_"
    )
    
    # Save the full model, selected model and data used.
    # We will need these later for results and assumption checks.
    selected_qol_models[[model_name]] <- list(
      Full_Model = full_model,
      Selected_Model = selected_model,
      Model_Data = model_data
    )
    
    # Identify the predictors remaining in the selected model.
    selected_terms <- attr(
      terms(selected_model),
      "term.labels"
    )
    
    # Add a row describing this model to the summary table.
    selection_summary <- rbind(
      selection_summary,
      data.frame(
        Model = model_name,
        
        # Number of patients used in the model.
        Complete_Case_N = nrow(model_data),
        
        # Number of predictors before backward selection.
        Full_Terms = length(
          attr(terms(full_model), "term.labels")
        ),
        
        # Number of predictors remaining afterward.
        Selected_Terms = length(selected_terms),
        
        # Tutorial 9 used N/15 as a guide for the allowed model size.
        N_Divided_By_15 = round(
          nrow(model_data) / 15,
          2
        ),
        
        # TRUE means the selected model meets the Tutorial 9 guideline.
        Meets_Term_Rule =
          length(selected_terms) < (nrow(model_data) / 15),
        
        # Record AIC before and after selection.
        # A lower AIC indicates a preferable balance between fit and complexity.
        Full_AIC = round(AIC(full_model), 2),
        Selected_AIC = round(AIC(selected_model), 2),
        
        # Show the final model formula so we know which variables remained.
        Selected_Formula = paste(
          deparse(formula(selected_model)),
          collapse = " "
        ),
        
        stringsAsFactors = FALSE
      )
    )
  }
}

print(selection_summary)

# ------------------------------------------------------------------------------
# Q2 SECTION 11: SUMMARIZE THE ADJUSTED SLEEP-QOL RELATIONSHIPS
# ------------------------------------------------------------------------------

# Create an empty table for the results from the eight selected models.
adjusted_sleep_results <- data.frame()

for (sleep_var in primary_sleep_vars) {
  
  for (qol_var in qol_vars) {
    
    # Recreate the name used to save the model in Section 10.
    model_name <- paste(
      qol_var,
      sleep_var,
      sep = "_"
    )
    
    # Retrieve the full and selected versions of this model.
    full_model <- selected_qol_models[[model_name]]$Full_Model
    
    selected_model <- selected_qol_models[[model_name]]$Selected_Model
    
    # Extract the regression coefficients from the selected model.
    coefficient_table <- summary(selected_model)$coefficients
    
    # Obtain the 95% confidence interval for the sleep coefficient.
    sleep_confidence_interval <- confint(
      selected_model
    )[sleep_var, ]
    
    # Compare the smaller selected model with the original full model.
    # A p-value above 0.05 suggests that removing the excluded variables
    # did not make the selected model significantly worse.
    model_comparison <- anova(
      selected_model,
      full_model
    )
    
    # Add the main sleep result from this model to the results table.
    adjusted_sleep_results <- rbind(
      adjusted_sleep_results,
      data.frame(
        Model = model_name,
        QoL_Outcome = qol_var,
        Sleep_Variable = sleep_var,
        
        # Number of complete patients used in the model.
        N = nobs(selected_model),
        
        # Expected change in QoL associated with the sleep measure,
        # after adjustment for the variables retained in the model.
        Adjusted_Estimate = unname(
          coefficient_table[sleep_var, "Estimate"]
        ),
        
        # Estimated uncertainty around the adjusted coefficient.
        Standard_Error = unname(
          coefficient_table[sleep_var, "Std. Error"]
        ),
        
        # Lower and upper limits of the 95% confidence interval.
        CI_Lower = unname(
          sleep_confidence_interval[1]
        ),
        
        CI_Upper = unname(
          sleep_confidence_interval[2]
        ),
        
        # P-value testing whether the adjusted sleep coefficient is zero.
        P_Value = unname(
          coefficient_table[sleep_var, "Pr(>|t|)"]
        ),
        
        # Proportion of QoL variation explained by the selected model,
        # adjusted for the number of predictors.
        Adjusted_R_Squared =
          summary(selected_model)$adj.r.squared,
        
        # P-value comparing the selected model with the full model.
        Full_vs_Selected_P =
          model_comparison$`Pr(>F)`[2],
        
        stringsAsFactors = FALSE
      )
    )
  }
}

# Reset the row labels to simple numbers.
rownames(adjusted_sleep_results) <- NULL

# Adjust the eight sleep-effect p-values using the Holm method from Tutorial 9.
adjusted_sleep_results$Holm_P <- p.adjust(
  adjusted_sleep_results$P_Value,
  method = "holm"
)

# Round estimates, standard errors, confidence intervals and adjusted R-squared
# after completing all calculations.
columns_to_round <- c(
  "Adjusted_Estimate",
  "Standard_Error",
  "CI_Lower",
  "CI_Upper",
  "Adjusted_R_Squared"
)

adjusted_sleep_results[columns_to_round] <- round(
  adjusted_sleep_results[columns_to_round],
  3
)

# Create readable versions of the original sleep-effect p-values.
adjusted_sleep_results$P_Value_Display <- ifelse(
  adjusted_sleep_results$P_Value < 0.0001,
  "<0.0001",
  formatC(
    adjusted_sleep_results$P_Value,
    format = "f",
    digits = 4
  )
)

# Create readable versions of the Holm-adjusted p-values.
adjusted_sleep_results$Holm_P_Display <- ifelse(
  adjusted_sleep_results$Holm_P < 0.0001,
  "<0.0001",
  formatC(
    adjusted_sleep_results$Holm_P,
    format = "f",
    digits = 4
  )
)

# Create readable p-values for the selected-versus-full model comparisons.
adjusted_sleep_results$Model_Comparison_P_Display <- ifelse(
  adjusted_sleep_results$Full_vs_Selected_P < 0.0001,
  "<0.0001",
  formatC(
    adjusted_sleep_results$Full_vs_Selected_P,
    format = "f",
    digits = 4
  )
)

# Select the results needed for the final summary table.
adjusted_sleep_table <- adjusted_sleep_results[, c(
  "Model",
  "N",
  "Adjusted_Estimate",
  "Standard_Error",
  "CI_Lower",
  "CI_Upper",
  "Adjusted_R_Squared",
  "P_Value_Display",
  "Holm_P_Display",
  "Model_Comparison_P_Display"
)]

# Ensure the final table also has simple row numbers.
rownames(adjusted_sleep_table) <- NULL

print(adjusted_sleep_table)

# ------------------------------------------------------------------------------
# Q2 SECTION 12: CHECK THE LINEAR REGRESSION ASSUMPTIONS
# ------------------------------------------------------------------------------

# Tutorial 6 examined three main diagnostic plots:
# 1. Histogram of residuals
# 2. Residuals plotted against fitted values
# 3. Normal Q-Q plot of residuals
#
# These plots will be produced separately for each of the eight selected models.

for (model_name in names(selected_qol_models)) {
  
  # Retrieve the selected model.
  selected_model <- selected_qol_models[[model_name]]$Selected_Model
  
  # Extract its residuals and fitted values.
  model_residuals <- resid(selected_model)
  model_fitted_values <- fitted(selected_model)
  
  # Display three diagnostic plots for this model.
  par(
    mfrow = c(1, 3),
    mar = c(4, 4, 3, 1)
  )
  
  # Plot 1: Histogram of residuals.
  # The residuals should be reasonably centred around zero and should not
  # show extreme skewness.
  hist(
    model_residuals,
    main = paste("Residuals:", model_name),
    xlab = "Residual",
    ylab = "Frequency",
    col = "lightblue",
    border = "white"
  )
  
  # Add a vertical line at zero to show the expected residual centre.
  abline(
    v = 0,
    col = "red",
    lwd = 2,
    lty = 2
  )
  
  # Plot 2: Residuals against fitted values.
  # We look for a random scatter around zero without a curve or funnel shape.
  plot(
    model_fitted_values,
    model_residuals,
    main = paste("Residuals vs Fitted:", model_name),
    xlab = "Fitted Values",
    ylab = "Residuals",
    pch = 19,
    col = rgb(0.2, 0.4, 0.7, 0.5)
  )
  
  # Add a horizontal reference line at zero.
  abline(
    h = 0,
    col = "red",
    lwd = 2,
    lty = 2
  )
  
  # Plot 3: Normal Q-Q plot.
  # Points close to the reference line support approximate residual normality.
  qqnorm(
    model_residuals,
    main = paste("Normal Q-Q:", model_name),
    pch = 19,
    col = rgb(0.2, 0.4, 0.7, 0.5)
  )
  
  qqline(
    model_residuals,
    col = "red",
    lwd = 2
  )
}

# Return to the normal plotting layout and margins.
par(
  mfrow = c(1, 1),
  mar = c(5.1, 4.1, 4.1, 2.1)
)

# ------------------------------------------------------------------------------
# Q2 SECTION 13: CHECK MULTICOLLINEARITY USING VIF
# ------------------------------------------------------------------------------

# Install the car package only if it is not already installed.
# This prevents R from reinstalling it every time the script is run.
if (!requireNamespace("car", quietly = TRUE)) {
  install.packages(
    "car",
    dependencies = TRUE
  )
}

# Load the package after confirming it is installed.
library(car)

# Create an empty table to store VIF results from every selected model.
vif_results <- data.frame()

for (model_name in names(selected_qol_models)) {
  
  # Retrieve the selected model.
  selected_model <- selected_qol_models[[model_name]]$Selected_Model
  
  # Calculate VIF values for the predictors in this model.
  model_vif <- car::vif(selected_model)
  
  # Factors with several categories may produce generalized VIF values.
  if (is.matrix(model_vif)) {
    
    # Use the adjusted generalized VIF so factors with different numbers
    # of categories can be compared.
    if ("GVIF^(1/(2*Df))" %in% colnames(model_vif)) {
      
      vif_values <- model_vif[, "GVIF^(1/(2*Df))"]
      
    } else {
      
      vif_values <- model_vif[, "GVIF"]^(
        1 / (2 * model_vif[, "Df"])
      )
    }
    
  } else {
    
    # Ordinary VIF values are returned when each predictor uses one coefficient.
    vif_values <- model_vif
  }
  
  # Add this model's results to the combined table.
  vif_results <- rbind(
    vif_results,
    data.frame(
      Model = model_name,
      Predictor = names(vif_values),
      VIF = as.numeric(vif_values),
      stringsAsFactors = FALSE
    )
  )
}

# Round the VIF values for presentation.
vif_results$VIF <- round(
  vif_results$VIF,
  3
)

# Add a general interpretation guide.
vif_results$Interpretation <- ifelse(
  vif_results$VIF < 5,
  "No major concern",
  ifelse(
    vif_results$VIF < 10,
    "Review carefully",
    "High multicollinearity"
  )
)

# Reset the table row numbers.
rownames(vif_results) <- NULL

print(vif_results)

# ============================================================
# Q2 SECTION 14: ADJUSTED MODELS USING CLINICAL THRESHOLDS
# ============================================================

# The primary adjusted models treated PSQI, ESS, and AIS as continuous scores.
# This sensitivity analysis converts them into clinical sleep-disturbance groups:
# PSQI > 4, ESS > 10, and AIS > 5.
#
# We keep the same adjustment variables selected in Section 10.
# This lets us examine the effect of changing how sleep disturbance is measured,
# without allowing a second variable-selection procedure to change the model.

# Store the clinically accepted cutoff for each sleep questionnaire.
clinical_cutoffs <- c(
  PSQI = 4,
  ESS  = 10,
  AIS  = 5
)

# Only PSQI, ESS, and AIS need to be converted.
# BSS is not included here because it is already binary.
sensitivity_model_names <- c(
  "SF36_PCS_PSQI", "SF36_MCS_PSQI",
  "SF36_PCS_ESS",  "SF36_MCS_ESS",
  "SF36_PCS_AIS",  "SF36_MCS_AIS"
)

# Create an empty list to store the six fitted sensitivity models.
binary_sensitivity_models <- list()

# Create an empty results table.
binary_sensitivity_results <- data.frame()

for (model_name in sensitivity_model_names) {
  
  # Retrieve the selected continuous-score model from Section 10.
  continuous_model <- selected_qol_models[[model_name]]$Selected_Model
  
  # Retrieve the exact complete-case dataset used for that model.
  # This keeps the continuous and binary versions based on the same patients.
  model_data <- selected_qol_models[[model_name]]$Model_Data
  
  # Identify the sleep questionnaire from the model name.
  if (grepl("PSQI$", model_name)) {
    sleep_variable <- "PSQI"
  } else if (grepl("ESS$", model_name)) {
    sleep_variable <- "ESS"
  } else {
    sleep_variable <- "AIS"
  }
  
  # Name the new binary exposure variable.
  binary_variable <- paste0(sleep_variable, "_binary")
  
  # Create the clinical disturbance indicator:
  # 0 = not disturbed
  # 1 = disturbed
  #
  # The assignment specifies values ABOVE the cutoff, so the code uses ">".
  model_data[[binary_variable]] <- as.integer(
    model_data[[sleep_variable]] > clinical_cutoffs[sleep_variable]
  )
  
  # Extract the outcome and predictors from the selected continuous model.
  outcome_variable <- all.vars(formula(continuous_model))[1]
  selected_predictors <- attr(
    terms(continuous_model),
    "term.labels"
  )
  
  # Replace the continuous sleep score with its binary clinical version.
  sensitivity_predictors <- selected_predictors
  sensitivity_predictors[
    sensitivity_predictors == sleep_variable
  ] <- binary_variable
  
  # Build the sensitivity-model formula.
  sensitivity_formula <- reformulate(
    termlabels = sensitivity_predictors,
    response = outcome_variable
  )
  
  # Fit an adjusted linear regression using the clinical sleep group.
  sensitivity_model <- lm(
    formula = sensitivity_formula,
    data = model_data
  )
  
  # Save the fitted model in case we need its full output later.
  binary_sensitivity_models[[model_name]] <- sensitivity_model
  
  # Extract the disturbed-versus-not-disturbed coefficient.
  coefficient_table <- summary(sensitivity_model)$coefficients
  binary_row <- coefficient_table[binary_variable, ]
  
  # Calculate its 95% confidence interval.
  binary_ci <- confint(
    sensitivity_model,
    parm = binary_variable,
    level = 0.95
  )
  
  # Count patients in each clinical sleep group.
  n_not_disturbed <- sum(model_data[[binary_variable]] == 0)
  n_disturbed <- sum(model_data[[binary_variable]] == 1)
  
  # Add the results from this model to the combined table.
  binary_sensitivity_results <- rbind(
    binary_sensitivity_results,
    data.frame(
      Model = model_name,
      Outcome = outcome_variable,
      Sleep_Measure = sleep_variable,
      N = nobs(sensitivity_model),
      N_Not_Disturbed = n_not_disturbed,
      N_Disturbed = n_disturbed,
      Adjusted_Mean_Difference = unname(binary_row["Estimate"]),
      Standard_Error = unname(binary_row["Std. Error"]),
      CI_Lower = unname(binary_ci[1]),
      CI_Upper = unname(binary_ci[2]),
      P_Value = unname(binary_row["Pr(>|t|)"]),
      Adjusted_R_Squared = summary(sensitivity_model)$adj.r.squared,
      stringsAsFactors = FALSE
    )
  )
}

# Adjust the six p-values using Holm's method.
# This accounts for testing three sleep measures against two QoL outcomes.
binary_sensitivity_results$Holm_P <- p.adjust(
  binary_sensitivity_results$P_Value,
  method = "holm"
)

# Make a copy for clean display without changing the original values.
binary_sensitivity_table <- binary_sensitivity_results

# Round the numerical results used in the report.
columns_to_round <- c(
  "Adjusted_Mean_Difference",
  "Standard_Error",
  "CI_Lower",
  "CI_Upper",
  "Adjusted_R_Squared"
)

binary_sensitivity_table[columns_to_round] <- round(
  binary_sensitivity_table[columns_to_round],
  3
)

# Display very small p-values as <0.0001 instead of incorrectly showing 0.
format_p_value <- function(p) {
  ifelse(
    p < 0.0001,
    "<0.0001",
    formatC(p, format = "f", digits = 4)
  )
}

binary_sensitivity_table$P_Value_Display <- format_p_value(
  binary_sensitivity_table$P_Value
)

binary_sensitivity_table$Holm_P_Display <- format_p_value(
  binary_sensitivity_table$Holm_P
)

# Keep only the report-relevant columns.
binary_sensitivity_table <- binary_sensitivity_table[
  c(
    "Model",
    "N",
    "N_Not_Disturbed",
    "N_Disturbed",
    "Adjusted_Mean_Difference",
    "Standard_Error",
    "CI_Lower",
    "CI_Upper",
    "Adjusted_R_Squared",
    "P_Value_Display",
    "Holm_P_Display"
  )
]

# Remove automatic row names so the printed table is clean.
rownames(binary_sensitivity_table) <- NULL

print(binary_sensitivity_table)