# Install and load the necessary packages
install.packages("readxl")
install.packages("psych")
install.packages("ggplot2")

library(readxl)
library(psych)
library(ggplot2)

# Load the dataset
file_path <- "Downloads/german_credit_data(1).xls"
df <- read_excel(file_path)

# View the first few rows of the dataset
head(df)
