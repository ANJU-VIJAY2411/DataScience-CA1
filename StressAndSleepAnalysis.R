#PART 1 - READING THE DATASET

#Load the data in sleep.csv
sleepRecords <- read.csv("sleep.csv", na = "")
sleepRecords


print(is.data.frame(sleepRecords))


print(ncol(sleepRecords))
#print the number of columns in the dataset : 9

print(nrow(sleepRecords))
#*print the number of records in the dataset : 9


str(sleepRecords)

sum(is.na(sleepRecords))
#validating the null values in the dataset : 0

names(sleepRecords)
#we rename all columns to make the data more understandable
names(sleepRecords)[names(sleepRecords)=="sr"] <- "snoring_rate"
names(sleepRecords)[names(sleepRecords)=="rr"] <- "respiration_rate"
names(sleepRecords)[names(sleepRecords)=="t"] <- "body_temperature"
names(sleepRecords)[names(sleepRecords)=="lm"] <- "limb_movement"
names(sleepRecords)[names(sleepRecords)=="bo"] <- "blood_oxygen"
names(sleepRecords)[names(sleepRecords)=="rem"] <- "rapid_eye_movement"
names(sleepRecords)[names(sleepRecords)=="sr.1"] <- "sleeping_hours"
names(sleepRecords)[names(sleepRecords)=="hr"] <- "heart_rate"
names(sleepRecords)[names(sleepRecords)=="sl"] <- "stress_level"

names(sleepRecords)

str(sleepRecords)


unique(sleepRecords$stress_level)
#checking the unique values for the column stress_level so as to finalise the same as 
#a categorical variable


stressLevel0 <- subset( sleepRecords, stress_level == "0")
print(stressLevel0)
stressLevel1 <- subset( sleepRecords, stress_level == "1")
print(stressLevel1)
stressLevel2 <- subset( sleepRecords, stress_level == "2")
print(stressLevel2)
stressLevel3 <- subset( sleepRecords, stress_level == "3")
print(stressLevel3)
stressLevel4 <- subset( sleepRecords, stress_level == "4")
print(stressLevel4)
#finding the count of different types of chest pain within the data
table(sleepRecords$stress_level)
#using bar plot to understand the chest pain (cp) variable

#--------------------------------------------------------------------------------------------------------------------------------

# HYPOTHESIS TESTING 

#--------------------------------------------------------------------------------------------------------------------------------

# Q1. Is there any relation between respiratory rate and blood oxygen level ?

#H0 : Respiratory rate has no impact in blood oxygen level
#H1 : Respiratory rate has no impact in blood oxygen level

#ANALYSIS OF Q1
#The variables we consider for the research question is respiration_rate and blood_oxygen variable 
#The respiration_rate variable is a continuous variable. 
#The blood_oxygen variable is a continuous variable. 
#Here our Dependent Variable is blood_oxygen 
#and our Independent Variable is respiration_rate


opar <- par(no.readonly = TRUE)
# display 2 charts in 1 row
par(mfrow = c(1,2))
#histogram for normaly distributed data
hist(sleepRecords$respiration_rate, 
     col = "steelblue", 
     main = "frequency chart for respiration rate")

hist(sleepRecords$blood_oxygen, 
     col = "steelblue", 
     main = "frequency chart for blood oxygen level")


#qq plot
qqnorm(sleepRecords$respiration_rate, main="QQ plot for  Respiration Rate", pch=19, ylab = "Respiration Rate")
qqline(sleepRecords$respiration_rate)

qqnorm(sleepRecords$blood_oxygen, main="QQ plot for blood oxygen level", pch=19, ylab = "Blood Oxygen Level")
qqline(sleepRecords$blood_oxygen)

#shapiro-wilk test for normality
# if the p-value > 0.05 then data is normally distributed
# if p-value < 0.05 then data is not normally distributed

#Note : for shapiro-test , data set should be between 3 - 5000 rows , here the row count is 630 and hence we can perform the same

shapiro.test(sleepRecords$blood_oxygen)
#Gives a p-value of 1.324e-11 (ie., < 0.05) which indicates that blood_oxygen variable is not normally distributed
shapiro.test(sleepRecords$respiration_rate)
#Gives a p-value of 1.571e-15 (ie., < 0.05) which indicates that respiration_rate variable is not normally distributed

attach(sleepRecords)
correlation <- cor.test(respiration_rate,blood_oxygen , method = "spearman")
correlation



#If the correlation coefficient between blood oxygen and respiration rate is -0.9199567 using Spearman's method, 
#it indicates a strong negative correlation between the two variables. 
#This suggests that as one variable increases, the other variable tends to decrease, and vice versa.



#--------------------------------------------------------------------------------------------------------------------------------

# Q2. Is there any relation between Limb movement and blood oxygen level ?

#H0 : Limb movement has no impact in blood oxygen level
#H1 : Limb movement has no impact in blood oxygen level

#ANALYSIS OF Q1
#The variables we consider for the research question is respiration_rate and blood_oxygen variable 
#The limb_movement variable is a continuous variable. 
#The blood_oxygen variable is a continuous variable. 
#Here our Dependent Variable is blood_oxygen 
#and our Independent Variable is limb_movement


#histogram for normaly distributed data
hist(sleepRecords$limb_movement, 
     col = "steelblue", 
     main = "frequency chart for Limb movement")

hist(sleepRecords$blood_oxygen, 
     col = "steelblue", 
     main = "frequency chart for blood oxygen level")


#qq plot
qqnorm(sleepRecords$limb_movement, main="QQ plot for Limb movement", pch=19, ylab = "Limb Movement")
qqline(sleepRecords$limb_movement)

qqnorm(sleepRecords$blood_oxygen, main="QQ plot for blood oxygen level", pch=19, ylab = "Blood Oxygen Level")
qqline(sleepRecords$blood_oxygen)

#shapiro-wilk test for normality
# if the p-value > 0.05 then data is normally distributed
# if p-value < 0.05 then data is not normally distributed

#Note : for shapiro-test , data set should be between 3 - 5000 rows , here the row count is 630 and hence we can perform the same

shapiro.test(sleepRecords$blood_oxygen)
#Gives a p-value of 1.324e-11 (ie., < 0.05) which indicates that blood_oxygen variable is not normally distributed
shapiro.test(sleepRecords$limb_movement)
#Gives a p-value of 3.211e-14 (ie., < 0.05) which indicates that limb_movement variable is not normally distributed


attach(sleepRecords)
correlation <- cor.test(limb_movement, blood_oxygen, method = "spearman")
correlation

#The coefficient value of -0.9199567 indicates that limb_movement and blood_oxygen have a strong 
#negative correlation. This indicates that as limb movement increases, blood oxygen levels tend to 
#decrease and vice versa. It is important to note, however, that correlation does not imply causation; 
#therefore, additional analysis is required to establish the underlying cause-and-effect relationship between these two variables.

#--------------------------------------------------------------------------------------------------------------------------------

# Q3. Is there any relation between respiratory rate and heart rate ?

#H0 : Heart rate has no impact in Respiration Rate
#H1 : Heart rate has an impact in Respiration Rate

#ANALYSIS OF Q3
#The variables we consider for the research question is respiration_rate and heart_rate variable 
#The heart_rate variable is a continuous variable. 
#The respiration_rate variable is a continuous variable. 
#Here we are considering our Dependent Variable is heart_rate 
#and our Independent Variable is respiration_rate


#histogram for normall y distribution check
hist(sleepRecords$respiration_rate, 
     col = "steelblue", 
     main = "frequency chart for Respiration rate")

hist(sleepRecords$heart_rate, 
     col = "steelblue", 
     main = "frequency chart for heart rate")


#qq plot
qqnorm(sleepRecords$respiration_rate, main="QQ plot for respiration rate", pch=19, ylab = "Respiratory rate")
qqline(sleepRecords$respiration_rate)

qqnorm(sleepRecords$heart_rate, main="QQ plot for heart rate", pch=19, ylab = "Heart rate")
qqline(sleepRecords$heart_rate)

#shapiro-wilk test for normality
# if the p-value > 0.05 then data is normally distributed
# if p-value < 0.05 then data is not normally distributed

#Note : for shapiro-test , data set should be between 3 - 5000 rows , here the row count is 630 and hence we can perform the same

shapiro.test(sleepRecords$respiration_rate)
#Gives a p-value of 1.571e-15 (ie., < 0.05) which indicates that blood_oxygen variable is not normally distributed
shapiro.test(sleepRecords$heart_rate)
#Gives a p-value of 1.571e-15 (ie., < 0.05) which indicates that limb_movement variable is not normally distributed

attach(sleepRecords)
correlation <- cor.test(heart_rate, respiration_rate, method = "spearman")
correlation

#A perfect correlation coefficient of 1 indicates that the two variables have a deterministic 
#relationship, and the values of one variable can be perfectly predicted from the values of the 
#other variable. In this case, if the correlation coefficient is truly 1, it suggests that heart rate 
#and respiration rate are perfectly linearly related, with no variability around the relationship.


#--------------------------------------------------------------------------------------------------------------------------------

# Q4. Is there any relation between stress level and heart rate ?

#H0 : Heart rate has no impact in Stress level
#H1 : Heart rate has an impact in Stress level

#ANALYSIS OF Q1
#The variables we consider for the research question is stress_level and heart_rate variable 
#The heart_rate variable is a continuous variable. 
#The stress_level variable is a categorical variable. 
#Here our Dependent Variable is stress_level 
#and our independent Variable is heart_rate


#histogram for normaly distributed data
hist(sleepRecords$stress_level, 
     col = "steelblue", 
     main = "frequency chart for Stress level")

hist(sleepRecords$heart_rate, 
     col = "steelblue", 
     main = "frequency chart for heart rate")


qqnorm(sleepRecords$heart_rate, main="QQ plot for heart rate", pch=19, ylab = "Heart rate")
qqline(sleepRecords$heart_rate)

qqnorm(sleepRecords$stress_level, main="QQ plot for stress level", pch=19, ylab = "Stress level")
qqline(sleepRecords$stress_level)

#shapiro-wilk test for normality
# if the p-value > 0.05 then data is normally distributed
# if p-value < 0.05 then data is not normally distributed

#Note : for shapiro-test , data set should be between 3 - 5000 rows

shapiro.test(sleepRecords$heart_rate)
#Gives a p-value of 1.571e-15 (ie., < 0.05) which indicates that heart_rate variable is not normally distributed
shapiro.test(sleepRecords$stress_level)
#Gives a p-value of 2.2e-16 (ie., < 0.05) which indicates that stress_level variable is not normally distributed


attach(sleepRecords)
kruskal.test(heart_rate, stress_level)

#A p-value of < 2.2e-16 means that the probability of observing such extreme test statistics (or more extreme) when the null hypothesis is true is very low (less than 0.00000000000000022). In other words, the p-value is extremely small, indicating very strong evidence against the null hypothesis.
#Therefore, we can conclude that there is strong evidence to suggest that there is a difference in median heart rate values between the different stress levels.


#--------------------------------------------------------------------------------------------------------------------------------

# Q5. Is there any relation between sleeping hours and stress level ?

#H0 : Sleeping hours has no impact in Stress level
#H1 : Sleeping hours has an impact in Stress level

#ANALYSIS OF Q5
#The variables we consider for the research question is sleeping_hours and stress_level variable 
#The sleeping_hours variable is a continuous variable. 
#The stress_level variable is a categorical variable. 
#Here our Independent Variable is sleeping_hours 
#and our Dependent Variable is stress_level


#histogram for normaly distributed data
hist(sleepRecords$stress_level, 
     col = "steelblue", 
     main = "frequency chart for Limb movement")

hist(sleepRecords$sleeping_hours, 
     col = "steelblue", 
     main = "frequency chart for blood oxygen level")


qqnorm(sleepRecords$stress_level, main="QQ plot for stress level", pch=19, ylab = "Stress level")
qqline(sleepRecords$stress_level)

qqnorm(sleepRecords$sleeping_hours, main="QQ plot for sleeping hours", pch=19, ylab = "Sleeping hours")
qqline(sleepRecords$sleeping_hours)

#shapiro-wilk test for normality
# if the p-value > 0.05 then data is normally distributed
# if p-value < 0.05 then data is not normally distributed

#Note : for shapiro-test , data set should be between 3 - 5000 rows

shapiro.test(sleepRecords$sleeping_hours)
#Gives a p-value of 2.2e-16 (ie., < 0.05) which indicates that sleeping_hours variable is not normally distributed
shapiro.test(sleepRecords$stress_level)
#Gives a p-value of 2.2e-16 (ie., < 0.05) which indicates that stress_level variable is not normally distributed


attach(sleepRecords)
kruskal.test(sleeping_hours, stress_level)
