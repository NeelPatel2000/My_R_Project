HEMOGLOB_data <- Sheet1_Table_1
head(HEMOGLOB_data)

library(dplyr)


# Set seed for reproducibility
set.seed(123)

# Extract Population A (Children with Anemia)
tA <- HEMOGLOB_data %>% select(A, Hb_A) %>% na.omit()

# Extract Population B (Healthy Children)
tB <- HEMOGLOB_data %>% select(B, Hb_B) %>% na.omit()

# Sample 80 observations from each group
sample_A <- tA %>% slice_sample(n = 80, replace = FALSE)
sample_B <- tB %>% slice_sample(n = 80, replace = FALSE)

# Check sample size
nrow(sample_A)  # Should be 80
nrow(sample_B)  # Should be 80

#View the sample of 80 from both sample with healthy children and Anemic Children
View(sample_A)
View(sample_B)

# Function to detect and remove outliers using IQR
remove_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25)
  Q3 <- quantile(data[[column]], 0.75)
  IQR_value <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  # Filter out outliers
  cleaned_data <- data %>% filter(data[[column]] >= lower_bound & data[[column]] <= upper_bound)
  return(cleaned_data)
}

# Apply to both groups
sample_A_clean <- remove_outliers(sample_A, "Hb_A")
sample_B_clean <- remove_outliers(sample_B, "Hb_B")

# Check number of rows after removing outliers
nrow(sample_A_clean) # 78 remove 2 put that in the report
nrow(sample_B_clean) # contains all the rows

#2 Check Normality Assumption
# Histogram
# Set up the plotting area to have 1 row and 2 columns
par(mfrow = c(1, 2))

# Plot Histogram for Hb_A (Children with Anemia)
hist(sample_A_clean$Hb_A, main = "Histogram of Hb_A", xlab = "Hb_A", col = "skyblue", breaks = 15, probability = TRUE)

# Add the normal distribution curve for Hb_A
curve(dnorm(x, mean = mean(sample_A_clean$Hb_A), sd = sd(sample_A_clean$Hb_A)), 
      add = TRUE, col = "blue", lwd = 2)

# Plot Histogram for Hb_B (Healthy Children)
hist(sample_B_clean$Hb_B, main = "Histogram of Hb_B", xlab = "Hb_B", col = "salmon", breaks = 15, probability = TRUE)

# Add the normal distribution curve for Hb_B
curve(dnorm(x, mean = mean(sample_B_clean$Hb_B), sd = sd(sample_B_clean$Hb_B)), 
      add = TRUE, col = "red", lwd = 2)

# Reset the plotting area to default (single plot)
par(mfrow = c(1, 1))

# Q-Q Plot
# Set up the plotting area to have 1 row and 2 columns
par(mfrow = c(1, 2))

# Q-Q plot for Hb_A (Children with Anemia)
qqnorm(sample_A_clean$Hb_A, main = "Q-Q Plot of Hb_A")
qqline(sample_A_clean$Hb_A, col = "red")

# Q-Q plot for Hb_B (Healthy Children)
qqnorm(sample_B_clean$Hb_B, main = "Q-Q Plot of Hb_B")
qqline(sample_B_clean$Hb_B, col = "blue")

# Reset the plotting area to default (single plot)
par(mfrow = c(1, 1))


#3 Shapiro-Wilk Test 
shapiro.test(sample_A_clean$Hb_A) 
#Since p-value = 0.221 is greater than 0.05, you fail to reject the null hypothesis,
#meaning there is no significant evidence to suggest that Sample A (Hb_A) 
#is not normally distributed. The data appears to be normally distributed.

shapiro.test(sample_B_clean$Hb_B)
#Since p-value = 0.1411 is greater than 0.05, you fail to reject the null hypothesis. 
#This means there is no significant evidence to suggest that Sample B (Hb_B) 
#is not normally distributed. The data appears to be normally distributed.

#4 Two Sample t_test 

t_test_results <- t.test(sample_A_clean$Hb_A, sample_B_clean$Hb_B, 
                         alternative = "two.sided", 
                         var.equal = FALSE, # Use Welch’s t-test
                         conf.level = 0.95)

print(t_test_results)

