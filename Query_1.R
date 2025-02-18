install.packages("dplyr")
library(dplyr)
install.packages("readxl")
library(readxl)
NCBIRTH800_1_ <- read_excel("~/Downloads/NCBIRTH800(1).xlsx")

# Getting a simple random sample of size 80
set.seed(123)
srs_sample <- NCBIRTH800_1_ %>% sample_n(80)
write.csv(srs_sample,"SRS_Sample_80.csv", row.names = FALSE)
head(srs_sample)
View(srs_sample)

# Calculating mean, median, mode, standard deviation, IQR and range
#MAGE
mean(srs_sample$mage)
median(srs_sample$mage)
sd(srs_sample$mage)
IQR(srs_sample$mage)
range(srs_sample$mage)

#WEEKS
mean(srs_sample$weeks)
median(srs_sample$weeks)
sd(srs_sample$weeks)
IQR(srs_sample$weeks)
range(srs_sample$weeks)

#GAINED 
mean(srs_sample$gained, na.rm = TRUE)
median(srs_sample$gained, na.rm = TRUE)
sd(srs_sample$gained, na.rm = TRUE)
IQR(srs_sample$gained, na.rm = TRUE)
range(srs_sample$gained, na.rm = TRUE)

#TOUNCES
mean(srs_sample$tounces, na.rm = TRUE)
median(srs_sample$tounces, na.rm = TRUE)
sd(srs_sample$tounces, na.rm = TRUE)
IQR(srs_sample$tounces, na.rm = TRUE)
range(srs_sample$tounces, na.rm = TRUE)

#TGRAMS
mean(srs_sample$tgrams, na.rm = TRUE)
median(srs_sample$tgrams, na.rm = TRUE)
sd(srs_sample$tgrams, na.rm = TRUE)
IQR(srs_sample$tgrams, na.rm = TRUE)
range(srs_sample$tgrams, na.rm = TRUE)

install.packages("e1071")
library(e1071)
#SKEWNESS
skewness(srs_sample$mage)
skewness(srs_sample$weeks)
skewness(srs_sample$gained, na.rm = TRUE)
skewness(srs_sample$tounces, na.rm = TRUE)
skewness(srs_sample$tgrams, na.rm = TRUE)

#KURTOSIS
kurtosis(srs_sample$mage)
kurtosis(srs_sample$weeks)
kurtosis(srs_sample$gained, na.rm = TRUE)
kurtosis(srs_sample$tounces, na.rm = TRUE)
kurtosis(srs_sample$tgrams, na.rm = TRUE)

#HISTOGRAM
hist(srs_sample$mage, breaks = 10)
hist(srs_sample$weeks, breaks = 10)
hist(srs_sample$gained, breaks = 15)
hist(srs_sample$tounces, breaks = 10)
hist(srs_sample$tgrams, breaks = 10)

#BOX AND WHISKER PLOT
boxplot(srs_sample$mage)
boxplot(srs_sample$weeks)
boxplot(srs_sample$gained)
boxplot(srs_sample$tounces)
boxplot(srs_sample$tgrams)

# BOXPLOT COMPARING TGRAMS FOR SMOKING VS. NONSMOKING WOMEN

boxplot(srs_sample$tgrams ~ srs_sample$smoke, col = "yellow", 
        main = "Effect of Smoking on Baby's Weight",
        xlab = "Smoking Habit",
        ylab = "Baby's Weight (grams)" )


