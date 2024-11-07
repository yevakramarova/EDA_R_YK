# Individual Project 3 - EDA Using R #
# Yeva Kramarova #

#I.	INTRODUCTION #
# US Cereal: Nutritional and Marketing Information of US cereal. The data set is build-in into R and it can be accessed through a ‘MASS’ package in R. I chose it because as being international, it was a cultural shock for me to learn how Americans eat so much cereal. #

# II.	DATA SET DESCRIPTION #
# The US Cereal data frame has 65 rows and 11 columns. The data come from the 1993 ASA Statistical Graphics Exposition, and are taken from the mandatory F&DA food label. The data have been normalized here to a portion of one American cup. #
# The data set is located in MASS library in R. The following lines of code access the data set and structure (data types) of the data set. #
library(tidyverse)
library(MASS)
data<-UScereal
head(data)
str(data) # data types

#III.	Data Set Summary Statistics #
summary(data) # statistical summary of the data 
# The code below checks for the missing values. #
sapply(data,function(x) sum(is.na(x)))
# No missing values in this data set. #

# Next, we evaluate frequency and proportions of each of the categorical variables: mfr, vitamins. #
# Frequency and proportion for 'mfr'
mfr_freq <- table(data$mfr)
mfr_prop <- prop.table(mfr_freq)
mfr_result <- data.frame(Frequency = mfr_freq, Proportion = mfr_prop)
print(mfr_result)

# Frequency and proportion for 'vitamins'
vitamins_freq <- table(data$vitamins)
vitamins_prop <- prop.table(vitamins_freq)
vitamins_result <- data.frame(Frequency = vitamins_freq, Proportion = vitamins_prop)
print(vitamins_result)

# Next, creting a correlation table and correlation heatmap to depict the correlations between the numerical variables of the data set. #
# Select the relevant columns
numeric_data <- data[, c("calories", "protein", "fat", "sodium", "fibre", "carbo", "sugars", "shelf", "potassium")]

# Create the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Convert the correlation matrix to a format suitable for ggplot2
cor_data <- as.data.frame(as.table(cor_matrix))

# Plot the heatmap
library(ggplot2)
ggplot(data = cor_data, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), 
                       name = "Correlation") +
  geom_text(aes(label = round(Freq, 2)), color = "black", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = NULL, y = NULL, title = "Correlation Matrix Heatmap")


# IV.	DATA SET GRAPHICAL EXPLORATION #
# The next set is made for the graphical exploration of the data. In this section, different kinds of graphs will help us to become more familiar with the data set and to discuss interesting distributions, anomalies, or imbalances of the data set. 

# Bar Plor for Manufacturer Count #
ggplot(data, aes(x = mfr)) + 
  geom_bar(fill = "skyblue") + 
  labs(title = "Count of Cereals by Manufacturer", x = "Manufacturer", y = "Count")

# Histograms for Nutritional Variables #
# Distribution of calories #
ggplot(data, aes(x = calories)) + 
  geom_histogram(binwidth = 20, fill = "orange", color = "black") + 
  labs(title = "Distribution of Calories", x = "Calories")
# The histogram groups the calorie values into bins of 20-calorie increments (e.g., 0–20, 20–40, etc.). The height of each bar represents the number of cereals that fall within that specific calorie range. For instance, if a bar over the range 100–120 has a count of 5, it means there are 5 cereals with calorie values between 100 and 120. #


# Graph of the distribution of fibre #
ggplot(data, aes(x = fibre)) + 
  geom_histogram(binwidth = 1, fill = "purple", color = "black") + 
  labs(title = "Distribution of Fibre", x = "Fibre")

# Based on the graphs above, it could be concluded that fiber is not widely used in cereal production. However, fiber helps regulate the body's use pf sugars, helping to keep hunger and blood sugar in check. Therefore, fiber is an important nutrition in our daily food consumption. So, I decided to create a graph that would show which manufacturers use the most fiber in their cereal production. #
#This code groups the data set by manufacturer, calculates the average fiber content for each, and then displays it in a bar chart for a clear comparison.#
# Calculate the average fiber per manufacturer
avg_fiber_per_mfr <- aggregate(fibre ~ mfr, data = UScereal, FUN = mean)

# Plot average fiber content by manufacturer
ggplot(avg_fiber_per_mfr, aes(x = mfr, y = fibre, fill = mfr)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Average Fiber Content by Manufacturer", x = "Manufacturer", y = "Average Fiber Content") + 
  theme_minimal() + 
  scale_fill_brewer(palette = "Pastel1") 
# Here, it is easy to see that Nabisco (N) manufacturer has a much higher average fiber content than any other manufacturer. #


# Also, recall the correlation matrix that showed us that potassium has several strong correlations with other variables such as fiber and protein. As was mentioned earlier, fibre is an extremely important nutrition, and protein is too. Potassium is also one of the highly important nutrition in our lives since it decreases the risk of the cardiovascular diseases. So, the graph below also shows what manufacturer uses potassium in their cereal production the most. #

library(dplyr)
# Calculate average potassium by manufacturer
avg_potassium <- data %>%
  group_by(mfr) %>%
  summarize(avg_potassium = mean(potassium, na.rm = TRUE))

# Create the bar plot
ggplot(avg_potassium, aes(x = mfr, y = avg_potassium, fill = mfr)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Potassium Content by Manufacturer", 
       x = "Manufacturer", 
       y = "Average Potassium") +
  theme_minimal() +
  theme(legend.position = "none")
# Here, it is easy to see that Nabisco (N) manufacturer has a much higher average potassium content than any other manufacturer. #


# Scatter Plots with Trend Lines #
# Next, the graph below shows relationship between calories and sugars in a dataset.It is easy to see that there is a positive relationship between the calories and sugars.#
ggplot(data, aes(x = calories, y = sugars)) + 
  geom_point(color = "purple") + 
  labs(title = "Calories vs. Sugars", x = "Calories", y = "Sugars")

# Another scatter plot showing a positive relationship between protein and calories. #
ggplot(data, aes(x = protein, y = calories)) + 
  geom_point(color = "blue") + 
  labs(title = "Protein vs. Calories", x = "Protein", y = "Calories")


# Below is the pie chart that visualizes the distribution of categorical variables in the data set. This pie chart visualizes the proportion of cereals produced by each manufacturer. #
mfr_counts <- data %>%
  group_by(mfr) %>%
  summarize(count = n()) %>%
  mutate(percentage = round((count / sum(count)) * 100, 1))  # Calculate percentage for labels

# Create the pie chart with labels
ggplot(mfr_counts, aes(x = "", y = count, fill = mfr)) + 
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste(mfr, percentage, "%")), 
            position = position_stack(vjust = 0.5)) +  # Places label in the middle of each slice
  labs(title = "Proportion of Cereals by Manufacturer") +
  theme_void() +  # Removes axis and background for a cleaner pie chart
  theme(legend.position = "none")  # Removes legend to avoid redundancy




# Box Plots for Nutritional Content by Manufacturer #
#A box plot below shows the shows the distribution of protein content across cereals for each manufacturer. The protein content is in grams for each cereal. This boxplot helps compare protein content across manufacturers, showing differences in protein ranges, medians, and outliers for each brand, which can reveal if some manufacturers tend to produce higher-protein cereals than others. #
ggplot(data, aes(x = mfr, y = protein)) + 
  geom_boxplot(fill = "lightpink") + 
  labs(title = "Protein by Manufacturer", x = "Manufacturer", y = "Protein")

# Lastly, a stacked bar plot that shows the proportion of cereals with each vitamin enrichment level by manufacturer. This way, we can compare how manufacturers differ in their use of vitamin enrichment. #

ggplot(data, aes(x = mfr, fill = vitamins)) + 
  geom_bar(position = "fill") +
  labs(title = "Proportion of Vitamin Enrichment by Manufacturer", 
       x = "Manufacturer", 
       y = "Proportion of Cereals") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "orange", "purple"), 
                    name = "Vitamins", 
                    labels = c("100%", "Enriched", "None"))


#V.	SUMMARY OF FINDINGS
#The US cereal data set showed us the information about the US cereal manufacturers, together with the nutritional contents of them such as carbohydrates, sugars, calories, protein, fiber, potassium, vitamins, fat and sodium. The data set is from the 1993 data. The graphs did a great job indicating what manufacturers are doing the best job with enriching their cereal with the highest amount of the most vital nutrition. Nabisco showed the best results when the protein, fiber and potassium was being compared among the manufacturers, whereas being the manufacturer with the lowest count of the cereal as shown in the Graph 1. General Mills is the producer of the highest count of cereal, however, contains some of lowest amounts of potassium, protein and fiber. But, General Mills together with Kelloggs and Ralston Purina have high amounts of vitamins in their cereal. #



