# Predicting the occurrence of diabetes

# Install and load packages
if (!require(pacman)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  arsenal,
  kableExtra,
  pscl,
  ROCR,
  performance,
  see,
  tidymodels
)

# Dataset
url_base <- "https://raw.githubusercontent.com/plotly/"
data_path <- "datasets/master/diabetes.csv"

# Load data
diabetes_data <- read_csv(
  paste0(url_base, data_path)
)

# Standardizing column names
colnames(diabetes_data) <- tolower(colnames(diabetes_data))

# Inspect data
head(diabetes_data)
colnames(diabetes_data)
str(diabetes_data)

# Convert outcome to factor for classification evaluation
diabetes_data$outcome <- factor(
  diabetes_data$outcome,
  levels = c(0, 1)
)

# Plot: Outcome vs BMI
ggplot(diabetes_data, aes(x = bmi, y = as.numeric(as.character(outcome)))) +
  geom_point() +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE
  ) +
  labs(
    title = "Model outcome vs. BMI by linear regression",
    x = "BMI",
    y = "Outcome"
  )

ggsave(
  "LM.png",
  dpi = 300
)

# Univariable logistic regression model
# Outcome is predicted using BMI only

logistic_uni <- glm(
  outcome ~ bmi,
  data = diabetes_data,
  family = binomial(link = "logit")
)

summary(logistic_uni)

# Create a new dataframe for prediction
# BMI values come from the original dataset

new_data <- data.frame(
  bmi = diabetes_data$bmi
)

# Generate predicted probabilities
# type = "response" gives probabilities between 0 and 1

predicted_probs <- predict(
  logistic_uni,
  newdata = new_data,
  type = "response"
)

head(predicted_probs)

# Add predictions to the new dataframe

new_data$predicted_prob <- predicted_probs

# Add the actual outcome for plotting

new_data$outcome <- as.numeric(
  as.character(diabetes_data$outcome)
)

# Sort by BMI so that the logistic curve is drawn correctly

new_data <- new_data %>%
  arrange(bmi)

# Generate BMI vs probability plot

ggplot(
  data = new_data,
  aes(x = bmi, y = outcome)
) +

  # Raw outcome observations
  geom_jitter(
    height = 0.05,
    width = 0,
    alpha = 0.2
  ) +

  # Logistic regression probability curve
  geom_line(
    aes(
      x = bmi,
      y = predicted_prob
    ),
    linewidth = 1.2
  ) +

  labs(
    x = "BMI",
    y = "Probability of outcome",
    title = "BMI vs Probability of Outcome"
  ) +

  theme_bw(base_size = 12)

ggsave(
  "BMI_vs_Probability_of_Outcome.png",
  dpi = 300
)

# Multivariable logistic regression model
# Predictors: BMI, glucose and blood pressure

logistic_multi <- glm(
  outcome ~ bmi + glucose + bloodpressure,
  data = diabetes_data,
  family = binomial(link = "logit")
)

summary(logistic_multi)

# Compare models using log-likelihood

ll_model_1 <- logLik(logistic_uni)
ll_model_2 <- logLik(logistic_multi)

model_comparison <- data.frame(
  Model = c(
    "1: Univariable Model",
    "2: Multivariable Model"
  ),
  LogLikelihood = c(
    as.numeric(ll_model_1),
    as.numeric(ll_model_2)
  )
)

# Sort from highest to lowest log-likelihood

print(
  model_comparison[
    order(
      model_comparison$LogLikelihood,
      decreasing = TRUE
    ),
  ]
)

# Create dataframe for plotting both models

plot_df <- data.frame(
  outcome = as.numeric(
    as.character(diabetes_data$outcome)
  ),
  bmi = diabetes_data$bmi,
  glucose = diabetes_data$glucose,
  bloodpressure = diabetes_data$bloodpressure
)

# Predict outcome probability using BMI only

plot_df$predicted_probs_uni <- predict(
  logistic_uni,
  newdata = data.frame(
    bmi = diabetes_data$bmi
  ),
  type = "response"
)

# Predict outcome probability using the multivariable model
# Glucose and blood pressure are held at their means

plot_df$predicted_probs_multi <- predict(
  logistic_multi,
  newdata = data.frame(
    bmi = diabetes_data$bmi,
    glucose = mean(diabetes_data$glucose),
    bloodpressure = mean(diabetes_data$bloodpressure)
  ),
  type = "response"
)

# Sort by BMI so the curves are plotted correctly

plot_df <- plot_df %>%
  arrange(bmi)

# Plot the two logistic regression curves

ggplot(
  plot_df,
  aes(x = bmi, y = outcome)
) +

  # Raw observations
  geom_jitter(
    height = 0.05,
    width = 0,
    alpha = 0.2
  ) +

  # Univariable model
  geom_line(
    aes(
      x = bmi,
      y = predicted_probs_uni,
      color = "Model 1: Univariable"
    ),
    linewidth = 1
  ) +

  # Multivariable model
  geom_line(
    aes(
      x = bmi,
      y = predicted_probs_multi,
      color = "Model 2: Multivariable"
    ),
    linewidth = 1
  ) +

  labs(
    x = "BMI",
    y = "Probability of outcome",
    color = "Models"
  ) +

  scale_color_manual(
    values = c(
      "Model 1: Univariable" = "red",
      "Model 2: Multivariable" = "blue"
    )
  ) +

  theme_minimal() +

  theme(
    legend.position = "bottom",
    text = element_text(size = 12)
  )

ggsave(
  "logistic_curves.png",
  dpi = 300
)

# Compare models using the performance package

comparison <- performance::compare_performance(
  logistic_uni,
  logistic_multi,
  rank = TRUE
)

# Visualize model comparison

plot(comparison)

ggsave(
  "Model_Comparison.png",
  dpi = 300
)

# Fit the null model
# This model contains only the intercept

null_model <- glm(
  outcome ~ 1,
  data = diabetes_data,
  family = binomial(link = "logit")
)

# Extract log-likelihoods

logLik_full <- logLik(logistic_multi)
logLik_null <- logLik(null_model)

# Calculate McFadden's pseudo-R-squared

mcfadden_r2 <- 1 -
  (as.numeric(logLik_full) /
    as.numeric(logLik_null))

cat(
  "McFadden's R-squared:",
  mcfadden_r2,
  "\n"
)

# Summary of all variables using the arsenal package

my_controls <- tableby.control(
  test = TRUE,
  total = TRUE,
  numeric.stats = c(
    "meansd",
    "medianq1q3",
    "range",
    "Nmiss2"
  ),
  cat.stats = c(
    "countpct",
    "Nmiss2"
  ),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)",
    range = "Min - Max",
    Nmiss2 = "Missing",
    meanCI = "Mean (95%CI)"
  )
)

prevar <- colnames(diabetes_data)[
  !colnames(diabetes_data) %in% "outcome"
]

mylabels <- as.list(prevar)
names(mylabels) <- prevar

tab <- tableby(
  as.formula(
    paste(
      "outcome",
      paste(prevar, collapse = "+"),
      sep = "~"
    )
  ),
  data = diabetes_data,
  control = my_controls
)

kable(
  summary(
    tab,
    labelTranslations = mylabels,
    text = TRUE
  ),
  caption = "Data summary"
) %>%
  kable_styling(
    latex_options = c(
      "scale_down",
      "hold_position"
    )
  )

# Model Evaluation using ROCR

# Generate predicted probabilities from the multivariable model

predicted <- predict(
  logistic_multi,
  type = "response"
)

# Create ROCR prediction object

pred <- ROCR::prediction(
  predicted,
  diabetes_data$outcome
)

# ROC curve
# TPR = True Positive Rate = Sensitivity
# FPR = False Positive Rate = 1 - Specificity

perf_roc <- ROCR::performance(
  pred,
  "tpr",
  "fpr"
)

plot(
  perf_roc,
  main = "ROC Curve"
)

abline(
  a = 0,
  b = 1,
  lty = 2,
  col = "blue"
)

# Sensitivity by cutoff

perf_sens <- ROCR::performance(
  pred,
  "sens",
  "cutoff"
)

plot(
  perf_sens,
  main = "Sensitivity by Cutoff"
)

# Specificity by cutoff

perf_spec <- ROCR::performance(
  pred,
  "spec",
  "cutoff"
)

plot(
  perf_spec,
  main = "Specificity by Cutoff"
)

# Sensitivity versus specificity

perf_sens_spec <- ROCR::performance(
  pred,
  "sens",
  "spec"
)

plot(
  perf_sens_spec,
  main = "Sensitivity vs Specificity",
  xlab = "Sensitivity",
  ylab = "Specificity"
)

# Extract sensitivity, specificity and cutoffs

sensitivity <- perf_sens_spec@x.values[[1]]
specificity <- perf_sens_spec@y.values[[1]]
cutoff <- perf_sens_spec@alpha.values[[1]]

# Calculate sensitivity + specificity

youden_sum <- sensitivity + specificity

# Find cutoff that maximizes sensitivity + specificity

best_index <- which.max(
  youden_sum
)

best_cutoff <- cutoff[best_index]

best_sensitivity <- sensitivity[best_index]

best_specificity <- specificity[best_index]

# Display optimal cutoff results

youden_results <- data.frame(
  cutoff = best_cutoff,
  sensitivity = best_sensitivity,
  specificity = best_specificity,
  sensitivity_plus_specificity = youden_sum[best_index],
  youden_j = youden_sum[best_index] - 1
)

youden_results

# Accuracy by cutoff

perf_acc <- ROCR::performance(
  pred,
  "acc",
  "cutoff"
)

plot(
  perf_acc,
  main = "Accuracy by Cutoff",
  xlab = "Cutoff",
  ylab = "Accuracy"
)

# Extract accuracy and cutoffs

accuracy <- perf_acc@y.values[[1]]
accuracy_cutoffs <- perf_acc@x.values[[1]]

# Find cutoff with highest accuracy

accuracy_index <- which.max(
  accuracy
)

best_accuracy_cutoff <- accuracy_cutoffs[
  accuracy_index
]

best_accuracy <- accuracy[
  accuracy_index
]

accuracy_results <- data.frame(
  cutoff = best_accuracy_cutoff,
  accuracy = best_accuracy
)

accuracy_results

# AUC using ROCR

perf_auc <- ROCR::performance(
  pred,
  "auc"
)

auc_rocr <- perf_auc@y.values[[1]]

cat(
  "ROCR AUC:",
  auc_rocr,
  "\n"
)

# Model Evaluation using tidymodels / yardstick

# Generate predicted classes using a 0.5 cutoff

probs <- predict(
  logistic_multi,
  type = "response"
)

predicted_classes <- ifelse(
  probs >= 0.5,
  "1",
  "0"
)

# Create dataframe containing actual and predicted outcomes

results <- data.frame(
  truth = factor(
    diabetes_data$outcome,
    levels = c("0", "1")
  ),
  estimate = factor(
    predicted_classes,
    levels = c("0", "1")
  )
)

# Confusion matrix

cm <- yardstick::conf_mat(
  results,
  truth = truth,
  estimate = estimate
)

cm

# Sensitivity

sensitivity_result <- yardstick::sens(
  results,
  truth = truth,
  estimate = estimate,
  event_level = "second"
)

sensitivity_result

# Specificity

specificity_result <- yardstick::spec(
  results,
  truth = truth,
  estimate = estimate,
  event_level = "second"
)

specificity_result

# Extract numeric sensitivity and specificity

sensitivity_value <- sensitivity_result$.estimate

specificity_value <- specificity_result$.estimate

# Balanced accuracy

balanced_accuracy <- (sensitivity_value +
  specificity_value) /
  2

cat(
  "Balanced Accuracy:",
  balanced_accuracy,
  "\n"
)

# Youden's J statistic at the 0.5 cutoff

youden_j <- (sensitivity_value +
  specificity_value) -
  1

cat(
  "Youden's J:",
  youden_j,
  "\n"
)

# Calculate the main classification metrics together

classification_metrics <- yardstick::metric_set(
  yardstick::sens,
  yardstick::spec,
  yardstick::bal_accuracy,
  yardstick::accuracy
)

classification_metrics(
  results,
  truth = truth,
  estimate = estimate,
  event_level = "second"
)

# ROC curve using yardstick

roc_input <- data.frame(
  truth = factor(
    diabetes_data$outcome,
    levels = c("0", "1")
  ),
  .pred_1 = probs
)

roc_data <- yardstick::roc_curve(
  roc_input,
  truth = truth,
  .pred_1,
  event_level = "second"
)

# Plot ROC curve

autoplot(
  roc_data
) +
  labs(
    title = "ROC Curve - Multivariable Logistic Regression",
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  theme_minimal()

# Calculate ROC-AUC using yardstick

auc_result <- yardstick::roc_auc(
  roc_input,
  truth = truth,
  .pred_1,
  event_level = "second"
)

auc_result
