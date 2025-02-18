#QUESTION 1 

library(gmodels)
CrossTable(NCBIRTH800_1_$premie, NCBIRTH800_1_$smoke, chisq = FALSE, prop.chisq = FALSE)

# A) Find the probability that a mother in this sample admitted to smoking.

total_mother <- nrow(NCBIRTH800_1_)
smoking_mother <- sum(NCBIRTH800_1_$smoke == 1, na.rm = TRUE)
pro_mother_smoking <- smoking_mother / total_mother * 100
print(pro_mother_smoking) #14.25% chance

# B) Find the probability that a mother in this sample had a premature baby.
total_mother <- nrow(NCBIRTH800_1_)
premature_birth_mother <- sum(NCBIRTH800_1_$premie == 1, na.rm = TRUE)
pro_premature_birth_mother <- premature_birth_mother/total_mother * 100
print(pro_premature_birth_mother) #11.26


# C) Find the probability that a mother in the sample had a premature baby given that the
# mother admitted to smoking

smoking_mother <- sum(NCBIRTH800_1_$smoke == 1, na.rm = TRUE)
premature_birth_mother <- sum(NCBIRTH800_1_$premie == 1 & NCBIRTH800_1_$smoke == 1, na.rm = TRUE)
pro_pre_birth_smoking <- premature_birth_mother / smoking_mother * 100
print(pro_pre_birth_smoking) #13.16

# D) Find the probability that a mother in the sample had a premature baby given that the
# mother did not admit to smoking.

nonsmoking_mother <- sum(NCBIRTH800_1_$smoke == 0, na.rm = TRUE)
premature_birth_mother <- sum(NCBIRTH800_1_$premie == 1 & NCBIRTH800_1_$smoke == 0, na.rm = TRUE)
pro_pre_birth_nonsmoking <- premature_birth_mother / nonsmoking_mother * 100
print(pro_pre_birth_nonsmoking) #11.40

# E) Find the probability that a mother in the sample had a premature baby or that the mother
# did not admit to smoking.

total_mothers <- nrow(NCBIRTH800_1_)
pre_birth_mother <- sum(NCBIRTH800_1_$premie == 1, na.rm = TRUE) / total_mothers
non_smoking_mother <- sum(NCBIRTH800_1_$smoke == 0, na.rm = TRUE) / total_mothers
pre_nonsmok_mother <- sum(NCBIRTH800_1_$premie == 1 & NCBIRTH800_1_ == 0, na.rm = TRUE) / total_mothers
prob_prebirth_nonsmoke <- pre_birth_mother + non_smoking_mother - pre_nonsmok_mother
prob_prebirth_nonsmoke_percent <- prob_prebirth_nonsmoke * 100
print(prob_prebirth_nonsmoke_percent) #69.25





library(tigerstats)

pnormGC(c(20), mean = 26.91, region = "below", sd = 6.11, graph = TRUE)
#QUESTION 2 PART A

mu <- 26.91
sigma <- 6.11

prob_less_than_20 <- pnormGC(c(20), mean = 26.91, region = "below", sd = 6.11, graph = TRUE)
print(paste("Probability that the age is less than 20 is", prob_less_than_20))

probablity_more_than_36 <- 1 - pnormGC(c(36), mean = 26.91, region = "above", sd = 6.11, graph = TRUE)
print(paste("Probability that the age is more than 36 is", probablity_more_than_36))

prob_bet_20_and_33 <- pnormGC(c(20, 36), mean = 26.91, region = "between", sd = 6.11, graph = TRUE)
print(paste("Probability that the age is between 33 and 20 is", prob_bet_20_and_33))

#QUESTION 2 PART B 

#HISTOGRAM
mother_age <- rnorm(NCBIRTH800_1_, mean = mu, sd = sigma)
hist(mother_age,
     main = "Histogram for mother's age",
     xlab = "Age")

#BOXPLOT
mother_age <- rnorm(NCBIRTH800_1_, mean = mu, sd = sigma)
boxplot(mother_age,
        main = "Boxplot for mother's age",
        xlab = "Age")

#DISTRIBUTION PLOT
mother_age <- rnorm(NCBIRTH800_1_, mean = mu, sd = sigma)
plot(density(mother_age),
     main = "Distribution Plot for mother's age")

#CALCULATE THE 68.2% RANGE (1 sd away from mean)
lower_bound <- mu - 1 * sigma
upper_bound<- mu + 1 * sigma
print(paste("Lower Bound =", lower_bound, "Upper Bound =", upper_bound))

# CALCULATE THE 95% RANGE (2 sd away from mean)
lower_bound_95 <- mu - 2 *sigma
upper_bound_95 <- mu + 2 * sigma
print(paste("Lower Bound =", lower_bound_95, "Upper Bound =", upper_bound_95))


#DISTRIBUTION PLOT
mother_age <- rnorm(800, mean = mu, sd = sigma)
plot(density(mother_age),
     main = "Histogram for mother's age") 


