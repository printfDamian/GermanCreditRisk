# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read the data from CSV file
credit_data <- read.csv("german_credit_data.csv")

# Plot Credit.amount against Age
credit_data %>%
  ggplot(aes(x = Age, y = Credit.amount)) + 
  geom_line() +
  labs(x = "Age", y = "Credit Amount") +  # Add axis labels
  ggtitle("Credit Amount by Age") +  # Add plot title
  theme_minimal()  # Use a minimalistic theme for better readability

# Histogram of Age
hist(credit_data$Age)

# Histogram of Job
hist(credit_data$Job)

# Histogram of Purpose
hist(credit_data$Purpose)

# Summary statistics by Purpose
summary_stats <- credit_data %>%
  group_by(Purpose) %>%
  summarize(
    mean_credit = mean(Credit.amount, na.rm = TRUE),
    total_count = n()
  )

# Stacked bar chart of Purpose by Sex
stacked_bar_chart <- ggplot(credit_data, aes(x = Sex, fill = Purpose)) +
  geom_bar() +
  labs(x = "Sex", y = "Count") +
  ggtitle("Distribution of Purpose by Sex")

# Frequency table for Purpose
freq_table <- credit_data %>%
  count(Purpose) %>%
  mutate(
    freq_relative = n / sum(n),
    freq_absolute_cumulative = cumsum(freq_relative),
    freq_relative_cumulative = cumsum(freq_relative)
  ) %>%
  arrange(desc(n))  # Arrange by descending absolute frequency

# Create data frame for the final table
tabela_final <- data.frame(
  Purpose = summary_stats$Purpose,
  Mean_Credit_Amount = summary_stats$mean_credit,
  Total_Count = summary_stats$total_count,
  Stacked_Bar_Chart = list(stacked_bar_chart),
  Absolute_Frequency = freq_table$n,
  Relative_Frequency = freq_table$freq_relative,
  Cumulative_Absolute_Frequency = freq_table$freq_absolute_cumulative,
  Cumulative_Relative_Frequency = freq_table$freq_relative_cumulative
)

# Display the table using kable
knitr::kable(tabela_final, format = "markdown")

# Calculate absolute frequency for 'Purpose' variable
frequencia_absoluta <- table(credit_data$Purpose)

# Calculate relative frequency
frequencia_relativa <- prop.table(frequencia_absoluta)

# Create data frame for the pie chart
tabela_frequencias_C <- data.frame(
  Intervalo = as.character(names(frequencia_absoluta)),
  Frequencia_Relativa = as.vector(frequencia_relativa)
)

# Create pie chart using base R
pie(tabela_frequencias_C$Frequencia_Relativa,
    labels = tabela_frequencias_C$Intervalo,
    main = 'Pie Chart of Relative Frequencies')

# Create pie chart using ggplot2
ggplot(tabela_frequencias_C, aes(x = "", y = Frequencia_Relativa, fill = Intervalo)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart of Relative Frequencies") +
  theme_void() +
  theme(legend.position = "right") +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(round(Frequencia_Relativa * 100, 1), "%")),
            position = position_stack(vjust = 0.5))

# Create bar chart using ggplot2
ggplot(tabela_frequencias_C, aes(x = Intervalo, y = Frequencia_Relativa, fill = Intervalo)) +
  geom_bar(stat = "identity") +
  labs(title = "Bar Chart of Relative Frequencies", x = "Category", y = "Relative Frequency") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = paste0(round(Frequencia_Relativa * 100, 1), "%")),
            vjust = -0.5)

