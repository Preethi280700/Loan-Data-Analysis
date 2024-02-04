# Load the package

library(openintro)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gtsummary)

# Load the data

accepted_loans <- read.csv("accepted_2007_to_2018Q4 2.csv")

# Filtering accepted loans to closed (Full Paid or Charged Off) and 
# credit card/debt consolidation loans only. 

loans_data <- accepted_loans %>% 
  filter(loan_status == "Fully Paid" | loan_status == "Charged Off") %>%  
  filter(purpose == "credit_card" | purpose == "debt_consolidation") %>%  
  select(loan_status, grade, purpose, dti,int_rate,
         fico_range_low, fico_range_high)  

loans_data$loan_status <- ifelse(loans_data$loan_status == "Charged Off",
                                 "Default", "Fully Paid")  
loans_data$fico_score <- rowMeans(loans_data[,c("fico_range_high",
                                                "fico_range_low")])  

#Cleaned data summary of variables of interest
loans_data %>%
  tbl_summary(
    include = c(loan_status, grade, purpose, dti,  int_rate, fico_range_low, 
                fico_range_high),  
    type = list(all_dichotomous() ~ "categorical"),
    statistic = list(all_categorical() ~"{n} ({p}%)",
                     all_continuous() ~ "{mean} ({sd})"),
    label = list(loan_status = "Loan Status", int_rate = "Interest Rate", 
                 grade = "Grade", purpose = "Purpose", dti = "Debt-to-Income", 
                 fico_range_low = "FICO Range Low", 
                 fico_range_high = "FICO Range High") 
  )

#Summary table of variables of interest
loans_data %>% 
  tbl_summary(
    include = c(loan_status, grade, purpose, dti, 
                int_rate, fico_range_low, fico_range_high),  
    type = list(all_dichotomous() ~ "categorical"),          
    statistic = list(all_categorical() ~"{n} ({p}%)",
                     all_continuous() ~ "{mean} ({sd})"),
    label = list(loan_status = "Loan Status", int_rate = "Interest Rate", 
                 grade = "Grade", purpose = "Purpose", dti = "Debt-to-Income", 
                 fico_range_low = "FICO Range Low", 
                 fico_range_high = "FICO Range High")
  )

## Exploratory Analysis Plots
#Generating plots for the proposal

loans_data %>% 
  ggplot(aes(x=int_rate, fill = loan_status)) +
  geom_histogram(binwidth = 0.5,
                 col = "white") +
  labs(x = "Interest Rate",
       y = "Frequency",
       title = "Distribution of Interest Rates",
       fill = "Loan Status") +
  theme_classic()

loans_data %>% 
  ggplot(aes(x=fico_score, y=after_stat(density), fill = loan_status)) +
  geom_histogram(binwidth = 5,
                 col = "white") +
  labs(x = "FICO Score",
       y = "Density",
       title = "Distribution of FICO Scores",
       fill = "Loan Status") +
  theme_classic()

loans_data %>% 
  ggplot(aes(x=grade, fill = loan_status)) +
  geom_bar(col = "white") +
  labs(x = "Loan Grade",
       y = "Frequency",
       title = "Loan Grade Counts",
       fill = "Loan Status") +
  theme_classic()

## Testing the data
# t-test

fully_paid = loans_data %>% 
  filter(loan_status == "Fully Paid") %>% 
  select(int_rate) %>% 
  pull()

default = loans_data %>% 
  filter(loan_status == "Default") %>% 
  select(int_rate) %>% 
  pull()

t.test(x = fully_paid, y = default)

# Chi square test
# Plotting a 100% bar chart to visually show proportions
ggplot(loans_data, aes(x=grade, fill=loan_status)) +
  geom_bar(position="fill") +
  labs(x = "Loan Purpose", y= "Proportion of Loans", 
       title = "Loan Status Proportion by Loan Grade", fill ="Loan Status")

test <- chisq.test(x=loans_data$loan_status, y=loans_data$grade)
# Checking expected Values
test$expected

#Removing outliers from the set
loans_data_no_zeroes <- filter(loans_data, dti != 0 & dti < 100)

ggplot(loans_data_no_zeroes, aes(x=dti, y=int_rate) +
         geom_point() +
         geom_smooth(method='lm', se= FALSE) +
         labs(title = "Interest Rate vs  DTI",
              x= " DTI",
              y= "Interest Rate")

## Machine learning model / linear regression model      
linear_model <- lm(log(int_rate) ~ dti , data = loans_data_no_zeroes)
summary(linear_model)
       
qqnorm(resid(linear_model))
qqline(resid(linear_model), col = "red")