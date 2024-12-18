#import packages
library(dplyr)
library(stats) 
library(ggplot2)
library(tidyverse)
# Read csv file 
student <- read.csv("StudentsPerformance.csv")
summary(student)
# Observe data
head(student)
View(student)
# Check dataset structure
str(student)
#missing values
mv_df<-sum(is.na(student))
mv_df
#Exploratory data analysis
# Gender
gender_counts <- table(student$gender)
gender_counts
gender_percentages <- round(100 * gender_counts / sum(gender_counts), 2)
pie(gender_percentages, labels = paste(names(gender_percentages), "(", gender_percentages, "%)"),
    main = "Gender")
# Race
race_counts <- table(student$race.ethnicity)
race_counts
race_percentages <- round(100 * race_counts / sum(race_counts), 2)
pie(race_percentages, labels = paste(names(race_percentages), "(", race_percentages, "%)"),
    main = "Race")
# Parent Level of Education
education_counts <- table(student$parental.level.of.education)
education_counts
edu_percentages <- round(100 * education_counts/ sum(education_counts), 2)
pie(edu_percentages, labels = paste(names(edu_percentages), "(", edu_percentages, "%)"),
    main = "Parent Level of Education")
# Lunch
lunch_counts <- table(student$lunch)
lunch_counts
lunch_percentages <- round(100 * lunch_counts / sum(lunch_counts), 2)
pie(lunch_percentages, labels = paste(names(lunch_percentages), "(", lunch_percentages, "%)"),
    main = "Lunch")
# Test Preparation Course
prep_counts <- table(student$test.preparation.course)
prep_counts
prep_percentages <- round(100 * prep_counts / sum(prep_counts), 2)
pie(prep_percentages, labels = paste(names(prep_percentages), "(", prep_percentages, "%)"),
    main = "Test Preparation Course")
# Math Score
summary(student$math.score)
hist(student$math.score, xlab = "Math Score", ylab = "Frequency",
     main = "Distribution of Math Scores")
# Reading Score
summary(student$reading.score)
hist(student$reading.score, xlab = "Reading Score", ylab = "Frequency",
     main = "Distribution of Reading Scores")
# Writing Score
summary(student$writing.score)
hist(student$writing.score, xlab = "Writing Score", ylab = "Frequency",
     main = "Distribution of Writing Scores")
# Hypothesis Testing 1
Male_student <- student$math_score[student$gender=="male"]
sd(Male_student)
library(BSDA)
z.test(Male_student,mu=65, sigma.x=14.36, alt="greater", conf.level=0.95)
# Hypothesis Testing 2
sd(student$writing_score)
library(BSDA)
z.test(student$writing_score,mu=62, sigma.x=15.20, alt="less", conf.level=0.95)
# Hypothesis Testing 3
student_no_prepare_course <- student$reading_score[student$test_preparation_course=="none"]
sd(student_no_prepare_course)
library(BSDA)
z.test(student_no_prepare_course,mu=75, sigma.x=14.46, alt="two.sided", conf.level=0.95)
#Goodness of fit
# Define expected probabilities
expected_probs <- rep(0.20, 5)
# Calculate observed frequencies
observed_freqs <- table(student$race.ethnicity)
# Perform chi-square test
chi_sq_test <- chisq.test(observed_freqs, p = expected_probs)
# Print test results
print(chi_sq_test)
#Chi-Square test for independence
# Create contingency tables for chi-square test for gender vs score
contingency_table_gender_math <- table(student$gender, student$math_score)
contingency_table_gender_reading <- table(student$gender, student$reading_score)
contingency_table_gender_writing <- table(student$gender, student$writing_score)
# Perform chi-square test
result_gender_math <- chisq.test(contingency_table_gender_math)
result_gender_reading <- chisq.test(contingency_table_gender_reading)
result_gender_writing <- chisq.test(contingency_table_gender_writing)
# Interpret the results
result_gender_math
result_gender_reading
result_gender_writing
# Create a new column for average score
student $average_score <- rowMeans(student[, c("math_score", "reading_score", "writing_score")])
# Create contingency table
contingency_table_education_avg <- table(student$parental_level_of_education, student $average_score)
contingency_table_race_avg <- table(student$race, student $average_score)
# Perform chi-square test
result_education_avg <- chisq.test(contingency_table_education_avg )
result_race_avg <- chisq.test(contingency_table_race_avg)
# Interpret the results
result_education_avg
result_race_avg
#Correlation
# Pearson correlation between 2 variables
cor(student$math_score, student$reading_score)
# Spearman correlation between 2 variables
cor(student$math_score, student$reading_score,method = "spearman")
#multiple scatter plots
pairs(student[, c("math_score", "reading_score", "writing_score")])
# Pearson correlation test
test <- cor.test(student$math_score, student$reading_score)
#Regression
# combine the regression with the scatter plot
# Plotting the scatter plot 
plot(math_score ~ reading_score, data = student, pch=16)
# plotting the fitted line:
abline(mod)
# linear model and saving it as 'mod'
mod <- lm(math_score ~ writing_score, data = student)
# the output:
mod
#more results
summary(mod)
#extract table of regression
library('broom')
# extract the table
my_results <- tidy(mod)
my_results
#ANOVA
student <- read.csv("StudentsPerformance.csv", header = TRUE,
                    colClasses = c("factor", "factor", "factor", "factor", "factor", "character", "character", "character"),
                    quote = "\"")
student[, 6:8] <- lapply(student[, 6:8], function(x) as.numeric(as.character(x)))
summary(student)
# 1.
one.way <- aov(math_score ~ gender, data = student)
summary(one.way)
# 2.
one.way <- aov(reading_score ~ gender, data = student)
summary(one.way)
# 3.
one.way <- aov(writing_score ~ gender, data = student)
summary(one.way)
# 4.
one.way <- aov(math_score ~ race, data = student)
summary(one.way)
# 5.
one.way <- aov(reading_score ~ race, data = student)
summary(one.way)
# 6.
one.way <- aov(writing_score ~ race, data = student)
summary(one.way)
# 7.
one.way <- aov(math_score ~ parental_level_of_education, data = student)
summary(one.way)
# 8.
one.way <- aov(reading_score ~ parental_level_of_education, data = student)
summary(one.way)
# 9.
one.way <- aov(writing_score ~ parental_level_of_education, data = student)
summary(one.way)
# 10.
one.way <- aov(math_score ~ lunch, data = student)
summary(one.way)
# 11.
one.way <- aov(writing_score ~ lunch, data = student)
summary(one.way)
# 12.
one.way <- aov(reading_score ~ lunch, data = student)
summary(one.way)
# 13.
one.way <- aov(math_score ~ test_preparation_course, data = student)
summary(one.way)
# 14.
one.way <- aov(writing_score ~ test_preparation_course, data = student)
summary(one.way)
# 15.
one.way <- aov(reading_score ~ test_preparation_course, data = student)
summary(one.way)