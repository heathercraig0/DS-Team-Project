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
  Median   = round(median_vals, 2),
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
# Denominator = No + Yes 
# (i.e. Not Total since Missing is excluded from the denominator)
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
# A subject classified as "disturbed" if >=50% of their COMPLETED sleep 
# instruments flagged disturbance. 
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

# Restricting the Number of Predictors;  LiverDiagnosis has 5 levels = 4 DoF, 
# all other categorical and continuous predictors contribute 1 DoF
# Total candidate predictor DoF = 14.

# PSQI Model  - Linear Regression w Stepwise Backward Model Selection
library(MASS)
# Max DoF is 12 (183/15 = 12.2), so LiverDiagnosis was removed to reduce the 
# candidate model from 14 to 10 DoF.# Removing missing data 
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


# ----------------------------------------------------------------------------
# Q2 Relationship Between Sleep Disturbance and QOL Scores
# ----------------------------------------------------------------------------

qol_vars <- c("SF36_PCS", "SF36_MCS")
continuous_sleep_vars <- c("PSQI", "ESS", "AIS")
primary_sleep_vars <- c("PSQI", "ESS", "AIS", "BSS")
binary_sleep_vars <- c("PSQI_binary","ESS_binary","AIS_binary","BSS")

# ------------------------------------------------------------------------------
# Q2 Available Sample Sizes
# ------------------------------------------------------------------------------

# Not every patient completed every sleep assessment. Therefore, each sleep-QoL analysis will have its own sample size rather than using all 268 patients.

pair_sample_sizes <- data.frame()

# Repeat the calculation for each of the four sleep measures and both quality-of-life outcomes.
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
# Q2 QoL Distributions
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
# Q2 Visual of Continuous Sleep Scores and QoL
# ------------------------------------------------------------------------------
# This function creates one scatterplot for a sleep score and QoL outcome.
# Keep patients who have both the sleep score and QoL outcome.
plot_sleep_qol <- function(sleep_var, qol_var, sleep_label, qol_label) {
  plot_data <- key_variables[
    complete.cases(key_variables[, c(sleep_var, qol_var)]),
    c(sleep_var, qol_var)
  ]
# Plot the sleep score against the QoL score.
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
  
  abline(
    visual_model,
    col = "red",
    lwd = 2
  )
}
# Display the six plots together.
par(mfrow = c(2, 3))
# Physical quality-of-life relationships.
plot_sleep_qol("PSQI", "SF36_PCS", "PSQI Score", "SF-36 PCS")
plot_sleep_qol("ESS", "SF36_PCS","ESS Score", "SF-36 PCS")
plot_sleep_qol("AIS", "SF36_PCS", "AIS Score", "SF-36 PCS")
# Mental quality-of-life relationships.
plot_sleep_qol("PSQI", "SF36_MCS", "PSQI Score", "SF-36 MCS")
plot_sleep_qol("ESS", "SF36_MCS", "ESS Score", "SF-36 MCS")
plot_sleep_qol("AIS", "SF36_MCS", "AIS Score", "SF-36 MCS")

par(mfrow = c(1, 1))

# ------------------------------------------------------------------------------
# Q2 Unadjusted Correlations Between Sleep and QoL
# ------------------------------------------------------------------------------
# three continuous sleep measures examined against two QoL outcomes
correlation_results <- data.frame()
for (sleep_var in continuous_sleep_vars) {
  for (qol_var in qol_vars) {
    analysis_data <- key_variables[
      complete.cases(key_variables[, c(sleep_var, qol_var)]),
      c(sleep_var, qol_var)
    ]
    # Use Pearson correlation because both variables are numeric and the scatterplots showed approximately linear relationships.
    correlation_test <- cor.test(
      analysis_data[[sleep_var]],
      analysis_data[[qol_var]],
      method = "pearson"
    )
    # Store the sample size, correlation estimate, 95% confidence interval and original unadjusted p-value for each relationship.
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
correlation_results$Holm_P <- p.adjust(correlation_results$P_Value,method = "holm")

# Round the results for presentation.
correlation_results$Correlation <- round(
  correlation_results$Correlation, 3
)

correlation_results$CI_Lower <- round(
  correlation_results$CI_Lower, 3
)

correlation_results$CI_Upper <- round(
  correlation_results$CI_Upper, 3
)

# Preserve very small p-values using scientific notation.
correlation_results$P_Value <- signif(
  correlation_results$P_Value, 3
)

correlation_results$Holm_P <- signif(
  correlation_results$Holm_P, 3
)

print(correlation_results)

# ------------------------------------------------------------------------------
# Q2 Visual Binary Sleep Disturbance vs QoL
# ------------------------------------------------------------------------------

plot_binary_qol <- function(group_var, qol_var, sleep_label, qol_label) {
  plot_data <- key_variables[
    complete.cases(key_variables[, c(group_var, qol_var)]),
    c(group_var, qol_var)
  ]
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
plot_binary_qol( "PSQI_binary", "SF36_PCS","PSQI Status","SF-36 PCS")
plot_binary_qol("ESS_binary", "SF36_PCS", "ESS Status", "SF-36 PCS")
plot_binary_qol("AIS_binary", "SF36_PCS", "AIS Status", "SF-36 PCS")
plot_binary_qol("BSS", "SF36_PCS", "BSS Risk", "SF-36 PCS")

# Mental quality-of-life comparisons.
plot_binary_qol("PSQI_binary", "SF36_MCS", "PSQI Status", "SF-36 MCS")
plot_binary_qol("ESS_binary", "SF36_MCS", "ESS Status", "SF-36 MCS")
plot_binary_qol( "AIS_binary", "SF36_MCS", "AIS Status", "SF-36 MCS")
plot_binary_qol("BSS", "SF36_MCS", "BSS Risk", "SF-36 MCS")

# Return to the normal single-plot layout.
par(mfrow = c(1, 1))

# ------------------------------------------------------------------------------
# Q2 Unadjusted Mean QoL Between Sleep Groups 
# ------------------------------------------------------------------------------

compare_qol_groups <- function(qol_var, group_var) {
  analysis_data <- key_variables[
    complete.cases(key_variables[, c(qol_var, group_var)]),
    c(qol_var, group_var)
  ]
  
  # Separate QoL values according to sleep-disturbance status.
  not_disturbed <- analysis_data[analysis_data[[group_var]] == 0, qol_var]
  disturbed <- analysis_data[analysis_data[[group_var]] == 1, qol_var]
  
  # Use Welch's two-sample t-test (not assuming equal variance)
  test_result <- t.test(disturbed, not_disturbed,  var.equal = FALSE)
  
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
    Mean_Difference = mean(disturbed) - mean(not_disturbed),
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

# Adjust the eight original p-values using the Holm method. This accounts for conducting several related comparisons.
group_comparison_results$Holm_P <- p.adjust(
  group_comparison_results$P_Value,
  method = "holm"
)

# Round the means, standard deviations, mean differences and confidence
# intervals to two decimal places for easier interpretation.
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
group_comparison_results$P_Value <- signif(
  group_comparison_results$P_Value,
  3
)
group_comparison_results$Holm_P <- signif(
  group_comparison_results$Holm_P,
  3
)
print(group_comparison_results)


# ------------------------------------------------------------------------------
# Q2 SECTION 10: SELECT THE ADJUSTED LINEAR REGRESSION MODELS
# ------------------------------------------------------------------------------

# Demographic and clinical variables considered for adjustment.
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

# Store the final models for later analysis.
selected_qol_models <- list()

# Store a short summary of the model-selection results.
selection_summary <- data.frame()

# Fit separate models for each sleep measure and QoL outcome.
for (sleep_var in primary_sleep_vars) {
  
  for (qol_var in qol_vars) {
    
    # Identify all variables needed for this model.
    required_vars <- c(
      qol_var,
      sleep_var,
      covariate_names
    )
    
    # Keep patients with complete data for the outcome, sleep measure and
    # all possible adjustment variables.
    #
    # Using one fixed sample ensures that all AIC values are calculated
    # using the same patients.
    model_data <- key_variables[
      complete.cases(key_variables[, required_vars]),
      required_vars
    ]
    
    # The full model contains the sleep measure and all possible
    # demographic and clinical adjustment variables.
    full_formula <- reformulate(
      termlabels = c(sleep_var, covariate_names),
      response = qol_var
    )
    
    # The minimum model contains only the sleep measure.
    # This forces the sleep measure to remain during backward selection.
    minimum_formula <- reformulate(
      termlabels = sleep_var,
      response = qol_var
    )
    
    # Fit the full multiple linear regression model.
    full_model <- lm(
      full_formula,
      data = model_data
    )
    
    # Use backward AIC to remove adjustment variables that do not improve
    # the balance between model fit and model complexity.
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
    
    # Save the selected model for later results and diagnostic checks.
    selected_qol_models[[model_name]] <- list(
      Selected_Model = selected_model
    )
    
    # Save the information needed to check and describe each selected model.
    selection_summary <- rbind(
      selection_summary,
      data.frame(
        Model = model_name,
        Complete_Case_N = nrow(model_data),
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

# Create an empty table for the sleep coefficient from each selected model.
adjusted_sleep_results <- data.frame()

# Extract the sleep result from all eight models.
for (sleep_var in primary_sleep_vars) {
  
  for (qol_var in qol_vars) {
    
    # Recreate the name used to store the model.
    model_name <- paste(
      qol_var,
      sleep_var,
      sep = "_"
    )
    
    # Retrieve the selected model.
    selected_model <-
      selected_qol_models[[model_name]]$Selected_Model
    
    # Extract the model's coefficient table.
    coefficient_table <- summary(selected_model)$coefficients
    
    # Calculate the 95% confidence interval for the sleep coefficient only.
    sleep_ci <- confint(
      selected_model,
      parm = sleep_var
    )
    
    # Save the adjusted sleep result.
    adjusted_sleep_results <- rbind(
      adjusted_sleep_results,
      data.frame(
        Model = model_name,
        QoL_Outcome = qol_var,
        Sleep_Variable = sleep_var,
        
        # Number of complete patients included in the model.
        N = nobs(selected_model),
        
        # Adjusted change in QoL associated with the sleep measure.
        # For PSQI, ESS and AIS, this represents a one-point score increase.
        # For BSS, this compares disturbed with not disturbed.
        Adjusted_Estimate =
          coefficient_table[sleep_var, "Estimate"],
        
        CI_Lower = sleep_ci[1],
        CI_Upper = sleep_ci[2],
        
        # Test whether the adjusted sleep coefficient differs from zero.
        P_Value =
          coefficient_table[sleep_var, "Pr(>|t|)"],
        
        stringsAsFactors = FALSE
      )
    )
  }
}

# Adjust the eight related sleep-effect p-values using the Holm method.
adjusted_sleep_results$Holm_P <- p.adjust(
  adjusted_sleep_results$P_Value,
  method = "holm"
)

# Round estimates and confidence intervals after completing the calculations.
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

# Keep small p-values in scientific notation rather than rounding them to zero.
# In the report, values below 0.0001 can be written as p < 0.0001.
adjusted_sleep_results$P_Value <- signif(
  adjusted_sleep_results$P_Value,
  3
)

adjusted_sleep_results$Holm_P <- signif(
  adjusted_sleep_results$Holm_P,
  3
)

print(adjusted_sleep_results)
# ------------------------------------------------------------------------------
# Q2 SECTION 12: CHECK THE LINEAR REGRESSION ASSUMPTIONS
# ------------------------------------------------------------------------------

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
if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}

# Check every selected QoL model.
for (model_name in names(selected_qol_models)) {
  
  # Retrieve the selected model.
  selected_model <-
    selected_qol_models[[model_name]]$Selected_Model
  
  print(model_name)
  print(round(car::vif(selected_model), 3))
}

# Ordinary VIF values below 5 suggest no major multicollinearity concern.
# For multi-level categorical predictors, review the adjusted
# GVIF^(1/(2*Df)) column instead.

# ------------------------------------------------------------------------------
# Q2 SECTION 14: ADJUSTED MODELS USING CLINICAL THRESHOLDS
# ------------------------------------------------------------------------------

# Clinical thresholds used to classify sleep disturbance.
sleep_cutoffs <- c(
  PSQI = 4,
  ESS = 10,
  AIS = 5
)

# Store the adjusted binary sleep results.
sensitivity_results <- data.frame()

# Repeat the adjusted analyses using binary sleep classifications.
# BSS is excluded because it was already binary in the main analysis.
for (sleep_var in continuous_sleep_vars) {
  
  for (qol_var in qol_vars) {
    
    # Retrieve the selected continuous-score model.
    model_name <- paste(qol_var, sleep_var, sep = "_")
    
    main_model <-
      selected_qol_models[[model_name]]$Selected_Model
    
    # Use the same patients and retained covariates as the main model.
    model_data <- model.frame(main_model)
    
    # Create the binary sleep variable using its clinical threshold.
    binary_var <- paste0(sleep_var, "_binary")
    
    model_data[[binary_var]] <- ifelse(
      model_data[[sleep_var]] > sleep_cutoffs[sleep_var],
      1,
      0
    )
    
    # Identify the variables retained in the main model.
    selected_vars <- attr(
      terms(main_model),
      "term.labels"
    )
    
    # Replace the continuous sleep score with its binary version.
    selected_vars[selected_vars == sleep_var] <- binary_var
    
    # Fit the sensitivity model with the same retained covariates.
    sensitivity_model <- lm(
      reformulate(
        selected_vars,
        response = qol_var
      ),
      data = model_data
    )
    
    # Extract the adjusted binary sleep result.
    coefficient_table <- summary(sensitivity_model)$coefficients
    
    sleep_ci <- confint(
      sensitivity_model,
      parm = binary_var
    )
    
    sensitivity_results <- rbind(
      sensitivity_results,
      data.frame(
        Model = model_name,
        N = nobs(sensitivity_model),
        Adjusted_Mean_Difference =
          coefficient_table[binary_var, "Estimate"],
        CI_Lower = sleep_ci[1],
        CI_Upper = sleep_ci[2],
        P_Value =
          coefficient_table[binary_var, "Pr(>|t|)"],
        stringsAsFactors = FALSE
      )
    )
  }
}

# Correct the six p-values for multiple testing.
sensitivity_results$Holm_P <- p.adjust(
  sensitivity_results$P_Value,
  method = "holm"
)

# Round estimates and confidence intervals for presentation.
columns_to_round <- c(
  "Adjusted_Mean_Difference",
  "CI_Lower",
  "CI_Upper"
)

sensitivity_results[columns_to_round] <- round(
  sensitivity_results[columns_to_round],
  3
)

# Preserve very small p-values using scientific notation.
sensitivity_results$P_Value <- signif(
  sensitivity_results$P_Value,
  3
)

sensitivity_results$Holm_P <- signif(
  sensitivity_results$Holm_P,
  3
)

print(sensitivity_results)