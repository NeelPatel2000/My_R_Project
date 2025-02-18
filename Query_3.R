# GETTING A SAMPLE SIZE OF 50
library(dplyr)
set.seed(108)
srs_sam <- NCBIRTH800_1_ %>% sample_n (50)

# THE SAMPLE DATA
View(srs_sam)

# (A) COUNT THE NUMBER OF MALES IN THE SAMPLE
num_male <- sum(srs_sam$sex == "1")
print(num_male)

result <- binom.test(num_male, 50, conf.level = 0.95)
print(result)

# (B) CALCULATE THE MEAN AGE OF THE MOTHER GIVING BIRTH
mother_age <- mean(srs_sam$mage)
print(mother_age)

sd_age <- sd(srs_sam$mage)
print(sd_age)
n <- length(srs_sam$mage)
SE <- sd_age / sqrt(n)
CI_lower <- mother_age - 1.96 * SE
CI_upper <- mother_age + 1.96 * SE
c(CI_lower, CI_upper)

# (C) THE MEAN WEIGHT GAINED DURING PREGNANCY 

mean_gained <- mean(srs_sam$gained, na.rm = TRUE)
print(mean_gained)

sd_gained <- sd(srs_sam$gained, na.rm = TRUE)
print(sd_gained)

r <- sum(!is.na(srs_sam$gained))  
print(r)  

SE_gained <- sd_gained / sqrt(r)

CI_lower_gained <- mean_gained - 1.96 * SE_gained
CI_upper_gained <- mean_gained + 1.96 * SE_gained

c(CI_lower_gained, CI_upper_gained)


# (D) The proportion (percentage) of mothers admitting to smoking during pregnancy

num_smoking <- sum(srs_sam$smoke == 1, na.rm = TRUE)  
n <- sum(!is.na(srs_sam$smoke))  

proportion_smoking <- num_smoking / n
print(proportion_smoking)

SE_proportion <- sqrt(proportion_smoking * (1 - proportion_smoking) / n)
print(SE_proportion)

CI_lower_smoking <- proportion_smoking - 1.96 * SE_proportion
CI_upper_smoking <- proportion_smoking + 1.96 * SE_proportion

c(CI_lower_smoking, CI_upper_smoking)


# (E) The difference in the average weight gained between smoking and nonsmoking mothers

smoking_group <- srs_sam$gained[srs_sam$smoke == 1]
nonsmoking_group <- srs_sam$gained[srs_sam$smoke == 0]

mean_smoking <- mean(smoking_group, na.rm = TRUE)
mean_nonsmoking <- mean(nonsmoking_group, na.rm = TRUE)

sd_smoking <- sd(smoking_group, na.rm = TRUE)
sd_nonsmoking <- sd(nonsmoking_group, na.rm = TRUE)

result_diff <- t.test(smoking_group, nonsmoking_group, conf.level = 0.95)

result_diff$conf.int

# (F) The difference in the average birth weight in grams between married and nonmarried mothers

married_group <- srs_sam$tgrams[srs_sam$marital == 1]
unmarried_group <- srs_sam$tgrams[srs_sam$marital == 2]

result_diff <- t.test(married_group, unmarried_group, conf.level = 0.95)

result_diff$conf.int

# (G) the difference in the proportion (percentage) of low birth weight babies between married and
#nonmarried mothers

low_birth_weight <- srs_sam$tgrams < 2500

married_low_bw <- low_birth_weight[srs_sam$marital == 1]
unmarried_low_bw <- low_birth_weight[srs_sam$marital == 2]

result_diff <- t.test(married_low_bw, unmarried_low_bw, conf.level = 0.95)

result_diff$conf.int


low_bw_married <- sum(srs_sam$tgrams < 2500 & srs_sam$marital == 1, na.rm = TRUE)
low_bw_unmarried <- sum(srs_sam$tgrams < 2500 & srs_sam$marital == 2, na.rm = TRUE)


total_married <- sum(srs_sam$marital == 1, na.rm = TRUE)
total_unmarried <- sum(srs_sam$marital == 2, na.rm = TRUE)


t <- matrix(c(low_bw_married, total_married - low_bw_married,
              low_bw_unmarried, total_unmarried - low_bw_unmarried),
            nrow = 2, byrow = TRUE)

print(t)

fisher.test(t, conf.level = 0.95)


