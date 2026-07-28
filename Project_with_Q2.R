#Data Science 1859 Team Project
#Team 6 - Heather Craig, Amanda Illeperuma, Edward Li & Jayati Mishra

# Reading in data and basic info
given_data <- read.csv("project_data.csv")
head(given_data)
summary(given_data)
dim(given_data)
colnames(given_data)

# Making new DF with only variables of interest
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


# Derived Variables
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
# Description of relevant data
# ------------------------------------------------------------------------------
continuous_vars <- c("Age", "BMI", "TransplantTime", "SF36_PCS", "SF36_MCS", 
                     "PSQI", "ESS", "AIS")

# empty vectors to collect results from each loop iteration
mean_vals   <- c()
sd_vals     <- c()
median_vals <- c()
iqr_vals    <- c()
na_vals     <- c()

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
# Individual prevalence for each of the 4 sleep instruments
# ----------------------------------------------------------------------------
# Denominator = No + Yes (i.e. Not Total sinceMissing is excluded from the 
# denominator)
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

# --- Collect into one summary table ---
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

# Individual prevalence by instrument (PSQI, AIS, ESS, BSS):
# PSQI 63.9% (n=183, 32% missing) | AIS 55.3% (n=262) |
# ESS 26.7% (n=251) | BSS 38.9% (n=262)
# Prevalence ranges widely (26.7%-63.9%). Consistent with each
# instrument capturing a different facet of sleep disturbance

# ----------------------------------------------------------------------------
# Overall (composite) prevalence: subject classified as "disturbed" if
# >=50% of their COMPLETED sleep instruments flagged disturbance.
# Denominator per subject = number of instruments they actually completed
# (missing instruments are excluded, not counted as "not disturbed").
# ----------------------------------------------------------------------------

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

# Check how many subjects have each number of completed tests, and how many
# ended up disturbed/not disturbed/NA on the composite
table(key_variables$Total_tests_done, useNA = "ifany")
table(key_variables$Sleep_disturbed_composite, useNA = "ifany")

# ----------------------------------------------------------------------------
# Overall prevalence: restrict to subjects with a valid composite value
# (excludes the small number with 0 completed instruments), then binom.test()
# ----------------------------------------------------------------------------
composite_valid <- key_variables[!is.na(key_variables$Sleep_disturbed_composite), ]
composite_n <- nrow(composite_valid)
composite_x <- sum(composite_valid$Sleep_disturbed_composite)

composite_test <- binom.test(composite_x, composite_n)
composite_test


# Composite sleep disturbance measure - derivation steps:
#   1. Binarize continuous instruments (PSQI>4, ESS>10, AIS>5); BSS already binary
#   2. Total_tests_done = count of non-missing instruments per subject (0-4)
#   3. Pct_disturbed = Tests_disturbed / Total_tests_done (NA if 0 tests done,
#      not 0 - percentage is undefined, not zero)
#   4. Sleep_disturbed_composite = 1 if Pct_disturbed >= 0.5 (ties go to
#      "disturbed"), else 0; NA subjects excluded from prevalence calc
#
# Result: overall prevalence = 53.4% (95% CI: 47.2%-59.5%, n=264; 4 subjects
# with zero completed instruments excluded). Falls within the range of
# individual instruments (26.7%-63.9%), closer to AIS/PSQI than ESS.


# ----------------------------------------------------------------------------
# Convert categorical predictors to factors 
# ----------------------------------------------------------------------------
key_variables$Gender <- factor(key_variables$Gender, 
                               labels = c("Male", "Female"))
key_variables$LiverDiagnosis <- factor(key_variables$LiverDiagnosis)
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


# ----------------------------------------------------------------------------
# Model 1: PSQI (continuous outcome) - linear regression
# ----------------------------------------------------------------------------
model_PSQI <- lm(PSQI ~ Age + Gender + BMI + TransplantTime + LiverDiagnosis +
                   DiseaseRecurrence + Rejection + Fibrosis + RenalFailure +
                   Depression + Corticosteroid,
                 data = key_variables)
summary(model_PSQI)

# ----------------------------------------------------------------------------
# Model 2: ESS (continuous outcome) - linear regression
# ----------------------------------------------------------------------------
model_ESS <- lm(ESS ~ Age + Gender + BMI + TransplantTime + LiverDiagnosis +
                  DiseaseRecurrence + Rejection + Fibrosis + RenalFailure +
                  Depression + Corticosteroid,
                data = key_variables)
summary(model_ESS)

# ----------------------------------------------------------------------------
# Model 3: AIS (continuous outcome) - linear regression
# ----------------------------------------------------------------------------
model_AIS <- lm(AIS ~ Age + Gender + BMI + TransplantTime + LiverDiagnosis +
                  DiseaseRecurrence + Rejection + Fibrosis + RenalFailure +
                  Depression + Corticosteroid,
                data = key_variables)
summary(model_AIS)

# ----------------------------------------------------------------------------
# Model 4: BSS (binary outcome, already 0/1) - logistic regression
# ----------------------------------------------------------------------------
model_BSS <- glm(BSS ~ Age + Gender + BMI + TransplantTime + LiverDiagnosis +
                   DiseaseRecurrence + Rejection + Fibrosis + RenalFailure +
                   Depression + Corticosteroid,
                 data = key_variables, family = binomial)
summary(model_BSS)

# ----------------------------------------------------------------------------
# Check actual sample size used per model (varies due to NAs in
# outcome and predictors - each model drops its own incomplete cases)
# ----------------------------------------------------------------------------
nobs(model_PSQI)
nobs(model_ESS)
nobs(model_AIS)
nobs(model_BSS)

# ----------------------------------------------------------------------------
# Summary table: predictor significance/direction across the 4 sleep models
# Rows = predictors, columns = estimate + p-value per instrument
# ----------------------------------------------------------------------------

# Pull the coefficient tables out of each model's summary()
psqi_coef <- summary(model_PSQI)$coefficients
ess_coef  <- summary(model_ESS)$coefficients
ais_coef  <- summary(model_AIS)$coefficients
bss_coef  <- summary(model_BSS)$coefficients

# All four models use the same formula/predictors, so row names line up -
# use PSQI's rows as the reference list, minus the intercept
predictor_names <- rownames(psqi_coef)
predictor_names <- predictor_names[predictor_names != "(Intercept)"]

# Build the combined table, matching each predictor by name (not position)
# in case any model handles a factor level slightly differently
results_table <- data.frame(
  Predictor      = predictor_names,
  PSQI_Estimate  = round(psqi_coef[predictor_names, "Estimate"], 3),
  PSQI_p         = round(psqi_coef[predictor_names, "Pr(>|t|)"], 3),
  ESS_Estimate   = round(ess_coef[predictor_names, "Estimate"], 3),
  ESS_p          = round(ess_coef[predictor_names, "Pr(>|t|)"], 3),
  AIS_Estimate   = round(ais_coef[predictor_names, "Estimate"], 3),
  AIS_p          = round(ais_coef[predictor_names, "Pr(>|t|)"], 3),
  BSS_OR         = round(exp(bss_coef[predictor_names, "Estimate"]), 3),  
  BSS_p          = round(bss_coef[predictor_names, "Pr(>|z|)"], 3)
)

# Add a significance flag per instrument (cutoff = 0.05)
results_table$PSQI_sig <- ifelse(results_table$PSQI_p < 0.05, "Yes", "No")
results_table$ESS_sig  <- ifelse(results_table$ESS_p  < 0.05, "Yes", "No")
results_table$AIS_sig  <- ifelse(results_table$AIS_p  < 0.05, "Yes", "No")
results_table$BSS_sig  <- ifelse(results_table$BSS_p  < 0.05, "Yes", "No")

print(results_table)


write.csv(key_variables, "key_variables.csv", row.names = FALSE)

library(car)   # for vif() 

qol_vars    <- c("SF36_PCS", "SF36_MCS")
sleep_flags <- c("PSQI_binary", "ESS_binary", "AIS_binary", "BSS",
                 "Sleep_disturbed_composite")

# ----------------------------------------------------------------------------
# 1. Visual comparison: boxplots of QoL by sleep-disturbance status
#    
# ----------------------------------------------------------------------------
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
for (qv in qol_vars) {
  for (sv in sleep_flags) {
    boxplot(key_variables[[qv]] ~ key_variables[[sv]],
            main = paste(qv, "by", sv), xlab = sv, ylab = qv)
  }
}
par(mfrow = c(1, 1))

# ----------------------------------------------------------------------------
# 2. Group comparisons: disturbed vs. not disturbed, per instrument
# ----------------------------------------------------------------------------
compare_group <- function(qol_var, group_var) {
  d  <- key_variables[!is.na(key_variables[[group_var]]) &
                        !is.na(key_variables[[qol_var]]), ]
  g0 <- d[[qol_var]][d[[group_var]] == 0]
  g1 <- d[[qol_var]][d[[group_var]] == 1]
  
  t_res <- t.test(g1, g0)
  w_res <- wilcox.test(g1, g0, conf.int = TRUE)
  
  data.frame(
    QoL_Variable   = qol_var,
    Sleep_Variable = group_var,
    N_disturbed    = length(g1),
    N_not          = length(g0),
    Mean_disturbed = round(mean(g1), 2),
    Mean_not       = round(mean(g0), 2),
    t_p            = round(t_res$p.value, 4),
    wilcox_p       = round(w_res$p.value, 4)
  )
}

qol_comparison_results <- do.call(rbind, lapply(qol_vars, function(qv) {
  do.call(rbind, lapply(sleep_flags, function(sv) compare_group(qv, sv)))
}))

# Holm-adjusted p-values across the 10 comparisons (each test family adjusted
# separately) 
qol_comparison_results$t_p_holm      <- round(p.adjust(qol_comparison_results$t_p, method = "holm"), 4)
qol_comparison_results$wilcox_p_holm <- round(p.adjust(qol_comparison_results$wilcox_p, method = "holm"), 4)
print(qol_comparison_results)

# ----------------------------------------------------------------------------
# 3. Correlation between continuous sleep scores and QoL
#    Pearson AND Spearman side by side
#    (cor.test(..., use = "complete.obs") and method = "spearman")
# ----------------------------------------------------------------------------
continuous_sleep_vars <- c("PSQI", "ESS", "AIS")

correlation_results <- data.frame()
for (sv in continuous_sleep_vars) {
  for (qv in qol_vars) {
    pear  <- cor.test(key_variables[[sv]], key_variables[[qv]], use = "complete.obs")
    spear <- cor.test(key_variables[[sv]], key_variables[[qv]], method = "spearman")
    correlation_results <- rbind(correlation_results, data.frame(
      Sleep_Variable = sv,
      QoL_Variable   = qv,
      Pearson_r      = round(unname(pear$estimate), 3),
      Pearson_p      = round(pear$p.value, 4),
      Spearman_rho   = round(unname(spear$estimate), 3),
      Spearman_p     = round(spear$p.value, 4)
    ))
  }
}
print(correlation_results)

# quick scatterplot matrix
pairs(key_variables[, c(continuous_sleep_vars, qol_vars)])

# ----------------------------------------------------------------------------
# 4. Multivariable regression + formal nested-model test 
#    Reduced model: covariates only. Full model: covariates + sleep
#    disturbance (composite). anova(reduced, full) gives an F-test for
#    whether adding sleep disturbance significantly improves the fit - this
#    IS the formal test of RQ2's hypothesis (H0: no association between
#    sleep disturbance and QoL, adjusting for covariates).
# ----------------------------------------------------------------------------
covariates <- "Age + Gender + BMI + TransplantTime + LiverDiagnosis + DiseaseRecurrence + Rejection + Fibrosis + RenalFailure + Depression + Corticosteroid"

# --- SF36_PCS ---
reduced_PCS <- lm(as.formula(paste("SF36_PCS ~", covariates)), data = key_variables)
full_PCS    <- lm(as.formula(paste("SF36_PCS ~ Sleep_disturbed_composite +", covariates)),
                  data = key_variables)

anova(reduced_PCS, full_PCS)                 # F-test: does adding sleep disturbance help?
AIC(reduced_PCS, full_PCS)                   # AIC comparison, corroborating evidence
summary(full_PCS)
confint(full_PCS)["Sleep_disturbed_composite", ]   # 95% CI for the sleep effect
vif(full_PCS)                                # check multicollinearity among predictors

# residual diagnostics
par(mfrow = c(1, 2))
hist(resid(full_PCS), main = "Residuals: PCS model", xlab = "")
qqnorm(resid(full_PCS)); qqline(resid(full_PCS), col = 2)
par(mfrow = c(1, 1))
plot(fitted(full_PCS), resid(full_PCS), xlab = "Fitted", ylab = "Residuals",
     main = "Residuals vs Fitted: PCS model")
abline(h = 0, lty = 2)

# --- SF36_MCS --- (identical structure)
reduced_MCS <- lm(as.formula(paste("SF36_MCS ~", covariates)), data = key_variables)
full_MCS    <- lm(as.formula(paste("SF36_MCS ~ Sleep_disturbed_composite +", covariates)),
                  data = key_variables)

anova(reduced_MCS, full_MCS)
AIC(reduced_MCS, full_MCS)
summary(full_MCS)
confint(full_MCS)["Sleep_disturbed_composite", ]
vif(full_MCS)

par(mfrow = c(1, 2))
hist(resid(full_MCS), main = "Residuals: MCS model", xlab = "")
qqnorm(resid(full_MCS)); qqline(resid(full_MCS), col = 2)
par(mfrow = c(1, 1))
plot(fitted(full_MCS), resid(full_MCS), xlab = "Fitted", ylab = "Residuals",
     main = "Residuals vs Fitted: MCS model")
abline(h = 0, lty = 2)

# ----------------------------------------------------------------------------
# 5. Sensitivity: repeat the nested-model test using each individual sleep
#    instrument instead of the composite, to see whether the QoL association
#    is consistent across instruments or specific to one facet of sleep
#    (same "does each instrument tell the same story?" logic used in RQ1).
#    NOTE: the reduced model is refit on the same non-missing subset as the
#    full model in each case - anova() will error/mismatch otherwise if the
#    two models are fit to different numbers of complete cases (this is the
#    exact issue Tutorial 8/9 flagged when comparing nested models).
# ----------------------------------------------------------------------------
exposures <- c("PSQI_binary", "ESS_binary", "AIS_binary", "BSS")

sensitivity_results <- data.frame()
for (ex in exposures) {
  for (qv in qol_vars) {
    full_form <- as.formula(paste(qv, "~", ex, "+", covariates))
    m_full    <- lm(full_form, data = key_variables)
    
    d_reduced  <- key_variables[!is.na(key_variables[[ex]]), ]
    m_reduced  <- lm(as.formula(paste(qv, "~", covariates)), data = d_reduced)
    
    a <- anova(m_reduced, m_full)
    sensitivity_results <- rbind(sensitivity_results, data.frame(
      QoL_Variable   = qv,
      Sleep_Exposure = ex,
      F_p_value      = round(a$`Pr(>F)`[2], 4),
      N              = nobs(m_full)
    ))
  }
}
sensitivity_results$Significant <- ifelse(sensitivity_results$F_p_value < 0.05, "Yes", "No")
print(sensitivity_results)

