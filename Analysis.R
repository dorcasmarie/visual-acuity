data <- read.csv(file = '12780KellyImplicatio_DATA_2021-01-27_1452.csv', header=TRUE)

######################################################################################
# English as second language summary stats 
######################################################################################

library('tidyr')

data_lang_count <- data %>% count(preferred_language)

data$English <- factor(ifelse(data$preferred_language == 'English', 'Y', 'N'))

data_lang_binary <- data %>% group_by(english) %>% count(english)

sum(is.na(data$postop_bcva_denominator)) #280

data$postop_bcva_denominator <- ifelse(data$postop_bcva_denominator == '20(CC)', "20", data$postop_bcva_denominator)
data$postop_bcva_denominator <- as.numeric(ifelse(data$postop_bcva_denominator == "", NA, data$postop_bcva_denominator))
#there are no denominator values that are 20 are larger, interesting
data$bcva_20_or_better <- as.factor(ifelse(data$postop_bcva_denominator <= 20, 'Y', 'N'))

sum(ifelse(data$postop_bcva_denominator <= 20, 1, 0))

#at least 35 have a denim. greater than 
# worse if denim. is greater 
# 20/20 greater 20/30 is worse 
# 20/15 

#dat_check <- cbind(data$postop_bcva_denominator, 
# as.numeric(ifelse(data$postop_bcva_denominator == '20(CC)', "20", data$postop_bcva_denominator)),
# as.numeric(ifelse(data$postop_bcva_denominator == "", NA, data$postop_bcva_denominator)), 
# as.numeric(ifelse(data$postop_bcva_denominator == "", NA, data$postop_bcva_denominator)))

Q_1 <- xtabs(~English + bcva_20_or_better, data)
Q_1

Q_1_dat <- table(Better_BCVA=data$bcva_20_or_better, English=data$english)

#do chi-square test because obviously not normal and non-parametric

# can't do chi-sq test because English - No and Better BCVA - No has 0 in the category

A_1 <- chisq.test(Q_1)

# Nope p-value = 0.3153

# we want to know if English - yes has more Better BCVA - Yes, than English - No & Better BCVA - No
# 


#######################################################################################################
# Q2
########################################################################################################

data$bcva_25_or_better <- as.factor(ifelse(data$postop_bcva_denominator <= 25, 'Y', 'N'))

Q_2 <- xtabs(~ English + bcva_25_or_better, data)
Q_2

# this might do better because there's a significant amount in each category

# will use fisher's test becuase one category has 5 or less

A_2 <- fisher.test(Q_2)
A_2

# result: nope, there was not a difference in how many people reached a best 
# corrected visual acuity of 20/25 or greater after cataract surgery based on language preference

#######################################################################################################
# Q3
########################################################################################################

data$bcva_30_or_better <- as.factor(ifelse(data$postop_bcva_denominator <= 30, 'Y', 'N'))
Q_3 <- xtabs(~English + bcva_30_or_better, data)
Q_3

#use fisher test because one category has 5 or less 

A_3 <- fisher.test(Q_3, alternative="two.sided")
A_3

# result : nope, there was not a difference in how many people reached a best corrected
# visual acuity of 20/30 or greater after cataract surgery based on language preference

#######################################################################################################
# Q4
########################################################################################################

# suboptimal 20/40 

data$bcva_40_or_better <- as.factor(ifelse(data$postop_bcva_denominator <= 40, 'Y', 'N'))
Q_4 <- xtabs(~English + bcva_40_or_better, data)
Q_4

A_4 <- fisher.test(Q_3, alternative="two.sided")
A_4

#######################################################################################################
# Q5
########################################################################################################
library(tidyverse)
group_means_by_lang <- data[!is.na(data["postop_bcva_logmar"]),] %>% group_by(English) %>%
  summarise(mean_postop_bcva_logmar = mean(postop_bcva_logmar, na.rm=TRUE), 
            var_postop_bcva_logmar = var(postop_bcva_logmar, na.rm=TRUE), 
            count_postop_bcva_logmar = n(), equal_to_zero = sum(postop_bcva_logmar==0, na.rm=TRUE))       
              

Zero_Check <- data[!is.na(data["postop_bcva_logmar"]),] %>% group_by(English) %>%
  summarise(equal_to_zero = sum(postop_bcva_logmar==0, na.rm=TRUE), 
            not_equal_to_zero = sum(postop_bcva_logmar != 0, na.rm=TRUE), 
            negative_values = sum(postop_bcva_logmar < 0, na.rm=TRUE),
            total = n())


#check normality of values >= 0
data_Q_5 <- data[!is.na(data["postop_bcva_logmar"]) & data["postop_bcva_logmar"] >= 0, ]
qqnorm(data_Q_5$postop_bcva_logmar)
shapiro.test(data_Q_5$postop_bcva_logmar)


data_Q_5_B <- data
data_Q_5_B$postop_bcva_logmar <- ( 0.125 + data_Q_5_B$postop_bcva_logmar )

qqnorm(data_Q_5_B$postop_bcva_logmar)
min(data_Q_5_B$postop_bcva_logmar, na.rm=TRUE)
#this is siginficantly different from normal dist 

#check equal variance 
model1 <- lm(postop_bcva_logmar ~ English, data_Q_5_B)
var.test(postop_bcva_logmar ~ English, data_Q_5_B)
    ## good news they are equal 0.85

# 279 missing postop_bcva_logmar


# also problem lots of 0 in the postop_bcva_logmar? Why?
data$zero_postop_bcva_logmar<-(data$postop_bcva_logmar == 0)

sum(data$zero_postop_bcva_logmar, na.rm = TRUE)

#240 == 0 how?!?


### because there are so many zeros we will either have to do possion or negative binomial regression

#checking for overdispersion.

library(performance) 
library(MASS)
data_model_1 <- data[ , c("postop_bcva_logmar", "English")]
min_value <- abs(min(data_model_1$postop_bcva_logmar, na.rm=TRUE))
data_model_1$postop_bcva_logmar <- data_model_1$postop_bcva_logmar + min_value
summary(data_model_1$postop_bcva_logmar)


model1 <- glm.nb((postop_bcva_logmar) ~ English, data=data_model_1)
model2 <- glm(postop_bcva_logmar ~ English, family = "poisson", data=data_model_1)
summary(model1)
library(see)
check_model(model1)# 
check_zeroinflation(model1) #model is overfitting zeros


kruskal.test(postop_bcva_logmar ~ English, data)

# p -value 0.1881 

wilcox.test(postop_bcva_logmar ~ English, data)
 # p-value 0.1886
#######################################################################################################
# Q6
########################################################################################################
# Was there a difference in the mean change (from pre to post operative) in 
# best corrected visual acuity (using logMAR, which is linear) between the two groups?


data_7 <- data %>% group_by(English)
wilcox.test(Pair(preop_bcva_logmar, postop_bcva_logmar) ~ English, data=data_7)

# depends, if you use 0.05 as your cut off then no, 
# 0.1 then yes...



#######################################################################################################
# Q7
########################################################################################################

# Was there a difference in post-op UCVA between patients 
# who had BCVA measured and those who did not?



#######################################################################################################
# Q8
########################################################################################################
# Were non-English speaking patients less likely to have their
# BCVA measured than English speaking patients?

data$postop_bcva_present = ifelse(is.na(data$postop_bcva_denominator), 'No', 'Yes')

q8 <- xtabs(~English + postop_bcva_present,  data)

q8

a8 <- chisq.test(q8)

a8 # statistically significant 

# English as second language does affect if postop_bcva was measured.
#######################################################################################################
# Q9
########################################################################################################
# Was there a difference in insurance type between the groups that
# did and did not have BCVA measured?
data$insurance <- as.factor(data$insurance)
unique(levels(data$insurance))

data %>% group_by(insurance) %>% count


#######################################################################################################
# Q10
########################################################################################################
#  	Was there a difference in median household income between 
#   the groups that did and did not have BCVA?
data$median_household_income <- ifelse(data$median_household_income == "#N/A" | 
                                         data$median_household_income == "Unknown"|
                                         data$median_household_income == "", NA, data$median_household_income)

income <- data %>% group_by(median_household_income) %>% count

xtabs(~median_household_income + postop_bcva_present, data)


