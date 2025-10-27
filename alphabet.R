# Load necessary libraries
library(ggplot2)

# Assuming your data is already loaded into a data frame called 'df'

# Filter data for Alphabet (ticker: GOOGL)
alphabet_data <- subset(DRIM_VF, tic == "GOOGL")

# Convert the 'data_date' column to Date format if it's not already
alphabet_data$data_date <- as.Date(alphabet_data$data_date)

# Plotting the graph for KDP_1MO
ggplot(alphabet_data, aes(x = data_date, y = KDP_1MO)) +
  geom_line(color = "blue") + 
  labs(title = "KDP 1MO for Alphabet", x = "Date", y = "KDP 1MO") +
  theme_minimal()