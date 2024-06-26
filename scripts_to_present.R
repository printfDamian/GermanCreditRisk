library(dplyr)
library(ggplot2)

credit_data = read.csv("german_credit_data.csv")

# Calculate absolute frequency for 'Checking.account' variable
freq_checking <- table(credit_data$Checking.account)

# Create data frame for the frequency table
freq_checking_df <- data.frame(
  Checking_Account = as.character(names(freq_checking)),
  Absolute_Frequency = as.integer(freq_checking),
  Relative_Frequency = prop.table(freq_checking)
)

# Print the frequency table
print("Frequency Table for Checking Account:")
print(freq_checking_df)

# Create bar chart for Checking Account categories
ggplot(credit_data, aes(x = Checking.account, fill = Checking.account)) +
  geom_bar() +
  labs(x = "Checking Account", y = "Frequency") +
  ggtitle("Frequency of Checking Account Categories") +
  theme_minimal()

ggplot()

# Calculate absolute frequency for 'Housing' variable
freq_housing <- table(credit_data$Housing)

# Create data frame for the frequency table
freq_housing_df <- data.frame(
  Housing = as.character(names(freq_housing)),
  Absolute_Frequency = as.integer(freq_housing),
  Relative_Frequency = prop.table(freq_housing)
)

# Print the frequency table
print("Frequency Table for Housing:")
print(freq_housing_df)



# Create pie chart for Housing categories
ggplot(freq_housing_df, aes(x = "", y = Relative_Frequency, fill = Housing)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart of Housing Categories") +
  theme_void() +
  theme(legend.position = "right") +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(round(Relative_Frequency * 100, 1), "%")),
            position = position_stack(vjust = 0.5))


# Calculate absolute frequency for 'Purpose' variable
freq_purpose <- table(credit_data$Purpose)

# Create data frame for the frequency table
freq_purpose_df <- data.frame(
  Purpose = as.character(names(freq_purpose)),
  Absolute_Frequency = as.integer(freq_purpose),
  Relative_Frequency = prop.table(freq_purpose)
)

# Print the frequency table
print("Frequency Table for Purpose:")
print(freq_purpose_df)


# Create bar chart for Purpose categories
ggplot(credit_data, aes(x = Purpose, fill = Purpose)) +
  geom_bar() +
  labs(x = "Purpose", y = "Frequency") +
  ggtitle("Frequency of Purpose Categories") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Summary statistics for Duration
summary_duration <- summary(credit_data$Duration)
print("Summary Statistics for Duration:")
print(summary_duration)

# Create boxplot for Duration
ggplot(credit_data, aes(x = "", y = Duration, fill = factor(Duration))) +
  geom_boxplot() +
  labs(title = "Boxplot of Duration")


# Summary statistics for Credit.amount
summary_credit <- summary(credit_data$Credit.amount)
print("Summary Statistics for Credit Amount:")
print(summary_credit)


#Regressão Linear

# Scatter plot with linear regression line for Age vs. Credit.amount
ggplot(credit_data, aes(x = Age, y = Credit.amount)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Age", y = "Credit Amount") +
  ggtitle("Scatter Plot with Linear Regression Line")



# Hypothetical weights for variables affecting credit risk
weights <- c(
  Credit.amount = 0.4,
  Duration = 0.3,
  Purpose = 0.2,
  Checking.account = 0.1
)

# Calculate credit risk score for each individual


credit_data$Credit_Risk_Score <- with(credit_data, {
  score <- (Credit.amount * weights["Credit.amount"] +
              Duration * weights["Duration"] +
              as.numeric(factor(Purpose)) * weights["Purpose"] +
              as.numeric(factor(Checking.account)) * weights["Checking.account"])
  round(score, 2)  # Round score to two decimal places
})
