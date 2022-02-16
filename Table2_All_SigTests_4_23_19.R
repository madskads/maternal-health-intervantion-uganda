# Name: Maddy Kluesner
# Project: Professional Paper
# Purpose: Uganda Demographics Query for Table 2
# Description: Run Statistical tests on sample to population sample
# Last Date: April 22, 2019

# Load libraries
library(tidyverse)
library(foreign)
library(readr)

# Set working directory
setwd("/Users/maddykluesner/Desktop/DHS_ProfPaper/UG_Women_7HDT/")

# Load the DHS dataset
# Data prefiltered on women of reproductive age in Uganda (15-49)
raw_population <- read.dta("UGIR7HFL.dta", convert.factors = FALSE)

# Renaming population dataset
population = raw_population %>%
  # Renaming Age, Education, Employment, Residence, Literacy, and Mobile Phone Ownership variables
  rename(age = v012, educ = v106, empl = v714, res = v025, lit = v155, mobile = v169a) %>%
  # Selecting Variables of interest
  dplyr::select(age, educ, empl, res, lit, mobile, sdistrict) %>%
  # Filter out those who are not educated
  filter(educ > 0) %>%
  # Filter on districts of interest
  filter(sdistrict == 113 | sdistrict == 102 | sdistrict == 108) %>%
  # Recode literacy to only contain women who can read a full sentence
  mutate(lit = {ifelse(lit == 2, 1, 0)})

# Load SMS Maama data
sample = read_csv("pilotstudy.csv") %>%
  # Recode three binary education level variables to single educ variable
  mutate(educ = {ifelse(educ_1 == 1, 1, 
                        ifelse(educ_2 == 1, 2, 
                               ifelse(educ_3 == 1, 3, NA)))}) %>%
  # Formatting issue: convert education variable from character to numeric
  mutate(empl = as.numeric(empl)) %>%
  # Filter out those who did not report an education value
  filter(!is.na(educ))


### 1. AGE
# INSPECT DATA
# Compare average age of sample to population.
# DHS range is 15-49, and sample range is 20-40.
# Check for assumptions homeoskedasticity (i.e. equal variance)
var.test(sample$age, population$age)
# The variance is not equal between samples, will set "var.equal = FALSE"

# TESTING
# Welch's t-test
t.test(x = sample$age, y = population$age, var.equal = FALSE)
# Results: P = 0.4782, DNR the null hypothesis
# Conclusion: Age in sample representative of population. 
# Get the N, SD for both sample and population
stats::sd(sample$age)
stats::sd(population$age)

### 2. EDUCATION
# NOTE: Using a chi-squared test to check if the proportions of educational levels are stat. sig. different.

# TIDY DATA
# Create education table for population
population_education = population$educ %>% table() %>% as.vector()
population_education_probs = population_education/sum(population_education) %>% unlist()
# Create education table for sample
sample_education = sample$educ %>% table() %>% as.vector()
sample_education_probs = sample_education/sum(sample_education)
# Make a matrix of the two
education_matrix = matrix(c(population_education, sample_education), nrow = 3) %>% t() %>% as.vector()

# TESTING
# Chi-Square Test
chisq.test(education_matrix)
# Results: X-squared = 2373.8, df = 5, p-value < 2.2e-16
# Conclusion:We can reject the null hypotheis that the sample has the same proportions of education
# Printing Results for table of the proportions: sample on left, population on right.
matrix(c(sample_education_probs, population_education_probs), nrow = 3)


### 3. EMPLOYMENT
# TIDY DATA
# Create employment table for population
population_employment = population$empl %>% table() %>% as.vector()
population_probs = population_employment/sum(population_employment)
# Create employment table for sample
sample_employment = sample$empl %>% table() %>% as.vector()
sample_probs = sample_employment/sum(sample_employment)

# TESTING
# Proportions Test
# Test for comparing the proportions in your sample to the proportions in your population, like a z-test for proportions
# Note on grammar: x is the number of successes of "being employed", n is the number of sample draws, p is probability of "being employed" in population
prop.test(x = sample_employment[2], n = sum(sample_employment), p = population_probs[2])
# Results : X-squared = 3.0874, df = 1, p-value = 0.0789
# Conclusion: We cannot reject the null hypothesis that the proportion of employed individuals in our sample is different from the population

### 4. LITERACY
# NOTE: All the indivdiuals in the sample are literate based on the inclusion criteria
# This filtered the sample
# Will use a proportions test comparing the proportion of literate individuals in the sample (i.e 100%) to the population

# TIDY DATA
# Create population table
population_literacy = population$lit %>% table() %>% as.vector()
population_prob = population_literacy/sum(population_literacy)
# No literate variable for sample because inclusion criteria was having to be literate

# TESTING
# Using Nrow() to estimate the successes and trials of literacy
prop.test(x = NROW(sample), n = NROW(sample), p = population_prob[2])
# Results: # X-squared = 21.061, df = 1, p-value = 4.45e-06
# Conclusion: we can reject the null hypothesis that they are the same proprotions. Sample is more literate.

### 5. MOBILE PHONE OWNERSHIP
# TIDY DATA
# Create population table
population_mobile = population$mobile %>% table() %>% as.vector()
population_prob = population_mobile/sum(population_mobile)
# No mobile phone variable for sample because inclusion criteria was having to be literate

# TESTING
# Prop test
prop.test(x = NROW(sample), n = NROW(sample), p = population_prob[2])
# X-squared = 24.437, df = 1, p-value = 7.678e-07, reject the null that the sample and population are the same.

### 6. URBAN RESIDENCE
# TIDY DATA
population_res = population$res %>% table() %>% as.vector() %>% rev()
population_prob = population_res/sum(population_res)
# Sample has non-responders removed
sample_res = sample %>% filter(!is.na(urban)) %>% .$urban %>% table() %>% as.vector()
sample_res_n = sample %>% filter(!is.na(urban)) %>% NROW()

# TESTING
# Prop test
prop.test(x = sample_res[2], n = sample_res_n, p = population_prob[2])
# Results: X-squared = 15.779, df = 1, p-value = 7.118e-05
# Reject the null hypothesis that the same proportion of urban-rural individuals are in the sample relative to the population

### SUMMARY
# Age is representative in sample
# Sample is more educated than pop
# Employment is representative in sample
# Sample is more literate than pop
# Sample owns more phones than pop
# Sample is more urban than pop

#NOTE: All prop tests are two sided.

