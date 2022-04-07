---
title: "Financial Literacy and Cryptocurrency"
font-family: 'Corbel'
author: Kieran Yuen
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r Loading in NFCS data, include=FALSE}
library(readxl)
NFCS_data <- read_excel("NFCS 2018 State Tracking Data 190623.xlsx", 
    sheet = "Final Data")
View(NFCS_data)
```

```{r Changing Data Types for variables, include=FALSE}
NFCS_data$Year <- as.factor(NFCS_data$Year)
levels(NFCS_data$Year)

NFCS_data$NFCS_ID <- as.character(NFCS_data$NFCS_ID)

NFCS_data$Score <- as.numeric(NFCS_data$Score)

NFCS_data$Age <- as.factor(NFCS_data$Age)
levels(NFCS_data$Age)

NFCS_data$Level_of_Education <- as.factor(NFCS_data$Level_of_Education)
levels(NFCS_data$Level_of_Education) <-
  c("Did not complete high school",
    "GED",
    "High school diploma",
    "Some college",
    "Associate's",
    "Bachelor's",
    "Post graduate")
levels(NFCS_data$Level_of_Education)

NFCS_data$Level_of_Income <- as.factor(NFCS_data$Level_of_Income)
levels(NFCS_data$Level_of_Income) <-
  c("<$15,000",
    "$15,000-$24,999",
    "$25,000-$34,999",
    "$35,000-$49,999",
    "$50,000-$74,999",
    "$75,000-$99,999",
    "$100,000-$149,999",
    ">$150,000")
levels(NFCS_data$Level_of_Income)

NFCS_data$Marital_Status <- as.factor(NFCS_data$Marital_Status)
levels(NFCS_data$Marital_Status) <-
  c("Single",
    "Married",
    "Divorced",
    "Separated",
    "Widowed/widower")
levels(NFCS_data$Marital_Status)

NFCS_data$Number_of_Children <- as.integer(NFCS_data$Number_of_Children)

NFCS_data$Financial_education <- as.factor(NFCS_data$Financial_education)
levels(NFCS_data$Financial_education) <-
  c("Very Low",
    "Low",
    "Low-Moderate",
    "Moderate",
    "Moderate-High",
    "High",
    "Very High")
levels(NFCS_data$Financial_education)


NFCS_data$State <- as.character(NFCS_data$State)

NFCS_data$State_Crypto_Year_Dummy_Variable <- as.factor(NFCS_data$State_Crypto_Year_Dummy_Variable)
levels(NFCS_data$State_Crypto_Year_Dummy_Variable)
```

# Regression Models
## Model with Demographic information only
```{r Model with Demographic information only}
Demographic_Model <- lm(Score ~ Age + Level_of_Income + Level_of_Education + Marital_Status + Number_of_Children, data = NFCS_data)
summary(Demographic_Model)

#plot(Demographic_Model)
```

## Model with Demographic information & Crypto State-Year Variable
```{r Model with Demographic information & Crypto State-Year Variable}
Demo_Crypto_Model <- lm(Score ~ Age + Level_of_Income + Level_of_Education + Marital_Status + Number_of_Children + State_Crypto_Year_Dummy_Variable, data = NFCS_data)
summary(Demo_Crypto_Model)

#plot(Demo_Crypto_Model)
```


## Model with Demographic information & Crypto State-Year Variable & interaction of Age and Crypto State-Year variable
```{r Model with Demographic information & Crypto State-Year Variable & interaction of Age and Crypto State-Year variable}
Demo_Crypto_Interaction_Model <- lm(Score ~ Age + Level_of_Income + Level_of_Education + Marital_Status + Number_of_Children + State_Crypto_Year_Dummy_Variable + Age*State_Crypto_Year_Dummy_Variable, data = NFCS_data)
summary(Demo_Crypto_Interaction_Model)

#plot(Demo_Crypto_Interaction_Model)
```

