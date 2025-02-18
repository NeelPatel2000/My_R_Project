head(RBCDATA)

# HERE WE EXTRACTED THREE SEPERATE POPULATION

tGOOD <- data.frame(RBCDATA$OBSERV, RBCDATA$GOOD)
names(tGOOD) <- c("ID_GOOD_S", "GOOD_S")
View(tGOOD)

tFAIR <- data.frame(RBCDATA$OBSERV, RBCDATA$FAIR)
names(tFAIR) <- c("ID_FAIR_S", "FAIR_S")
View(tFAIR)

tPOOR <- data.frame(RBCDATA$OBSERV, RBCDATA$POOR)
names(tPOOR) <- c("ID_POOR_S", "POOR_S")
View(tPOOR)

# GENERATING SRS 80 FOR EACH OF THE POPULATION
library(dplyr) 
set.seed(123)
tSGOOD <- tGOOD %>% slice_sample(n = 80, replace = FALSE)
set.seed(456)
tSFAIR <- tFAIR %>% slice_sample(n = 80, replace = FALSE)
set.seed(789)
tSPOOR <- tPOOR %>% slice_sample(n = 80, replace = FALSE)
View(tSGOOD)
View(tSFAIR)
View(tSPOOR)

# NOW WE COMBINE THE THREE POPULATION
RBCSR80 <- data.frame(tSGOOD, tSFAIR, tSPOOR)
View(RBCSR80)
install.packages("writexl")
library(writexl)
write_xlsx(RBCSR80, 'RBCSR80.xlsx')


getwd()
# summary of the distribution
library(psych)
describe(RBCSR80$GOOD_S, skew = FALSE)
describe(RBCSR80$FAIR_S, skew = FALSE)
describe(RBCSR80$POOR_S, skew = FALSE)

# DID GRUBBS TEST TO LOOK FOR OUTLIERS: 

install.packages("outliers")
library(outliers) 
grubbs.test(RBCSR80$GOOD_S)
grubbs.test(RBCSR80$FAIR_S)
grubbs.test(RBCSR80$POOR_S) # NO SIGNIFICANT OUTLIERS FOUND, HENCE CAN KEEP ALL THE DATA POINT


# Load necessary library for the shapiro test
if (!require(tidyr)) install.packages("tidyr", dependencies=TRUE)
library(tidyr)

# Convert wide format to long format (keep only the relevant columns)
RBCSR80_long <- RBCSR80 %>%
  pivot_longer(cols = c(GOOD_S, FAIR_S, POOR_S), 
               names_to = "Group", 
               values_to = "Score")

# View first few rows
head(RBCSR80_long)

if (!require(car)) install.packages("car", dependencies=TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies=TRUE)
if (!require(agricolae)) install.packages("agricolae", dependencies=TRUE)

library(car)
library(ggplot2)
library(agricolae)  # For Turkey test

# Assumption Check 1: Normality Test
shapiro.test(RBCSR80$GOOD_S)
shapiro.test(RBCSR80$FAIR_S)
shapiro.test(RBCSR80$POOR_S)


# Assumption Check 2: Homogeneity of Variances (Levene's Test)
RBCSR80_long$Group <- as.factor(RBCSR80_long$Group)

# Run Levene's Test Again
leveneTest(Score ~ Group, data = RBCSR80_long)

# Perform One-Way ANOVA
anova_model <- aov(Score ~ Group, data = RBCSR80_long)
summary(anova_model) # significant difference between mean, reject the null hypothesis


# Perform Tukey’s HSD Post-hoc Test (if ANOVA is significant)
tukey_result <- HSD.test(anova_model, "Group", group=TRUE)
print(tukey_result)

# Boxplot Visualization
ggplot(RBCSR80_long, aes(x = Group, y = Score, fill = Group)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "One-Way ANOVA: RBC Score Across Groups",
       x = "Group",
       y = "Score")






