#Prediciting the occurance of diabetes

if (!require(pacman)) {
  install.packages("pacman")
}

p_load(tidyverse, arsenal, kableExtra, pscl, ROCR)


#dataset
url_base <- "https://raw.githubusercontent.com/plotly/"
data_path <- "datasets/master/diabetes.csv"

#Load Data
diabetes_data <- read_csv(paste0(url_base, data_path))

#standardizing column names
colnames(diabetes_data) <- tolower((colnames(diabetes_data)))
head(diabetes_data)
colnames(diabetes_data)

str(diabetes_data)

#plot
ggplot(diabetes_data, aes(x = bmi, y = outcome)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +
  labs(title = "Model outcome vs. BMI by linear regression")


#Univariable LR model(log)
logistic_uni <- glm(
  outcome ~ bmi,
  data = diabetes_data,
  family = binomial(link = "logit")
)


#Create a new dataframe for prediction
""
"We will vary 'glucose' across its range and hold other predictors at their mean "
""
new_data <- data.frame(
  bmi = diabetes_data$bmi,
  outcome = diabetes_data$outcome
)


""
"We use the model to predict probabilities for this new data, The 'type = response' argument ensures we get a probability (0-1)"
""

predicted_probs <- predict(logistic_uni, new_data = new_data, type = "response")
head(predicted_probs)

# Add these predictions to our new data frame
new_data$predicted_prob <- predicted_probs


#Generate the plot
ggplot(data = new_data, aes(x = bmi, y = outcome)) +
  # add the raw data points (0s and 1s). We use jitter to avoid overplotting
  geom_jitter(height = 0.05, width = 0, alpha = 0.2, color = "gray50") +
  # Add the logistic curve from our model's predictions
  geom_line(
    data = new_data,
    aes(x = bmi, y = predicted_prob),
    color = "blue",
    linewidth = 1.2
  ) +
  # Add a label and a title
  labs(
    x = 'BMI',
    y = "Probability of outcome",
    title = "BMI vs Probability of Outcome"
  ) +
  # Clean theme
  theme_bw(base_size = 12)

#Multivariate model
logistic_multi <- glm(
  outcome ~ bmi + glucose + bloodpressure,
  data = diabetes_data,
  family = binomial(link = "logit")
)

summary(logistic_multi)

""
"Comparing the above uni and mulitvariate models 
regarding their maximum likelihood to evaluate which model better fits "
""

ll_model_1 <- logLik(logistic_uni)
ll_model_2 <- logLik(logistic_multi)

#Create a summary data frame for easy comparison
model_comparison <- data.frame(
  Model = c("1: Univariable Model", "2: Multivariable Model"),
  LogLikelihod = c(ll_model_1, ll_model_2)
)

#Print the comparison table, sorted by the best fit
print(model_comparison[
  order(model_comparison$LogLikelihod, decreasing = TRUE),
])
