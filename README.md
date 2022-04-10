---
title: "Financial Literacy and Cryptocurrency"
font-family: 'Corbel'
author: Kieran Yuen
output: rmarkdown::github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r Loading in NFCS data, include=FALSE}
library(readxl)
X04092022_NFCS_2018_State_Tracking_Data_190623 <- read_excel("04092022_NFCS 2018 State Tracking Data 190623.xlsx")
View(X04092022_NFCS_2018_State_Tracking_Data_190623)
```


```{r Cleaning Data, include=FALSE}
library(tidyverse)
NFCS_data <- X04092022_NFCS_2018_State_Tracking_Data_190623 %>% 
  select(TRACK,NFCSID,STATEQ,A3Ar_w,A5_2015,A8,A6,A11,M4,M6,M7,M8,M9,M10) %>%
  rename(Year = TRACK,
         NFCS_ID = NFCSID,
         State = STATEQ,
         Age = A3Ar_w,
         Education = A5_2015,
         Income = A8,
         Marital_Status = A6,
         Children = A11,
         Financial_Education = M4,
         Question_1 = M6,
         Question_2 = M7,
         Question_3 = M8,
         Question_4 = M9,
         Question_5 = M10) %>%
  na.omit() %>% 
  filter(Year =="2018"|
           Year == "2015") %>%
  mutate(Age = recode(Age,
                      '1' = "18-24",
                      '2' = "25-34",
                      '3' = "35-44",
                      '4' = "45-54",
                      '5' = "55-64",
                      '6' = "65+")) %>%
  mutate(Education = recode(Education,
                      '1' = "No High School",
                      '2' = "High School",
                      '3' = "High School",
                      '4' = "Some College",
                      '5' = "Associate's",
                      '6' = "Bachelor's",
                      '7' = "Post Graduate",
                      '99' = 'NA')) %>%
  mutate(Income = recode(Income,
                      '1' = "<$15,000",
                      '2' = "$15,000-$24,999",
                      '3' = "$25,000-$34,999",
                      '4' = "$35,000-$49,999",
                      '5' = "$50,000-$74,999",
                      '6' = "$75,000-$99,999",
                      '7' = "$100,000-$149,999",
                      '8' = ">$150,000",
                      '98' = 'NA',
                      '99' = 'NA')) %>%
  mutate(Marital_Status = recode(Marital_Status,
                      '1' = "Married",
                      '2' = "Single",
                      '3' = "Separated",
                      '4' = "Divorced",
                      '5' = "Widowed/widower",
                      '99' = 'NA')) %>%
  mutate(Children = recode(Children,
                      '1' = 1,
                      '2' = 2,
                      '3' = 3,
                      '4' = 4,
                      '5' = 0,
                      '6' = 0,
                      '99' = NA_real_)) %>%
    mutate(Financial_Education = recode(Financial_Education,
                      '1' = 1,
                      '2' = 2,
                      '3' = 3,
                      '4' = 4,
                      '5' = 5,
                      '6' = 6,
                      '7' = 7,
                      '98' = NA_real_,
                      '99' = NA_real_)) %>%
  mutate(Question_1 = recode(Question_1,
                      '1' = 1,
                      '2' = 0,
                      '3' = 0,
                      '98' = NA_real_,
                      '99' = NA_real_)) %>%
  mutate(Question_2 = recode(Question_2,
                      '1' = 0,
                      '2' = 0,
                      '3' = 1,
                      '98' = NA_real_,
                      '99' = NA_real_)) %>%
  mutate(Question_3 = recode(Question_3,
                      '1' = 0,
                      '2' = 1,
                      '3' = 0,
                      '4' = 0,
                      '98' = NA_real_,
                      '99' = NA_real_)) %>%
  mutate(Question_4 = recode(Question_4,
                      '1' = 1,
                      '2' = 0,
                      '98' = NA_real_,
                      '99' = NA_real_)) %>%
  mutate(Question_5 = recode(Question_5,
                      '1' = 0,
                      '2' = 1,
                      '98' = NA_real_,
                      '99' = NA_real_)) %>%
  mutate(Score = select(., Question_1:Question_5) %>% 
           rowSums(na.rm = TRUE)) %>% 
  unite("State_Year", State, Year, remove = FALSE) %>% 
  mutate(Crypto_State_Year = ifelse(State_Year == '1_2018'
                                    |State_Year =='10_2018'
                                    |State_Year =='11_2018'
                                    |State_Year =='14_2018'
                                    |State_Year =='30_2015'
                                    |State_Year =='30_2018'
                                    |State_Year =='34_2018'
                                    |State_Year =='39_2018'
                                    |State_Year =='43_2015'
                                    |State_Year =='43_2018'
                                    |State_Year =='45_2018'
                                    |State_Year =='46_2015'
                                    |State_Year =='46_2018'
                                    |State_Year =='48_2018'
                                    |State_Year =='7_2015'
                                    |State_Year =='7_2018'
                                    ,1,0))
```

```{r Changing Data Types for variables, include=FALSE}
#Score
NFCS_data$Score <- as.numeric(NFCS_data$Score)

#Age
NFCS_data$Age <- as.factor(NFCS_data$Age)
levels(NFCS_data$Age)

#Education
NFCS_data$Education <- as.factor(NFCS_data$Education)
NFCS_data$Education <- factor(NFCS_data$Education,
                              levels = c("No High School",
                                         "High School",
                                         "Some College",
                                         "Associate's",
                                         "Bachelor's",
                                         "Post Graduate"))
levels(NFCS_data$Education)

#Income
NFCS_data$Income <- as.factor(NFCS_data$Income)
NFCS_data$Income <- factor(NFCS_data$Income,
                              levels = c("<$15,000",
                                         "$15,000-$24,999",
                                         "$25,000-$34,999",
                                         "$35,000-$49,999",
                                         "$50,000-$74,999",
                                         "$75,000-$99,999",
                                         "$100,000-$149,999",
                                         ">$150,000"))
levels(NFCS_data$Income)

#Marital Status
NFCS_data$Marital_Status <- as.factor(NFCS_data$Marital_Status)
NFCS_data$Marital_Status <- factor(NFCS_data$Marital_Status,
                              levels = c("Single",
                                         "Married",
                                         "Divorced",
                                         "Separated",
                                         "Widowed/widower"))
levels(NFCS_data$Marital_Status)

#Children
NFCS_data$Children <- as.integer(NFCS_data$Children)

#Financial Education
NFCS_data$Financial_Education <- as.numeric(NFCS_data$Financial_Education)

#Crypto_State_Year
NFCS_data$Crypto_State_Year <- as.factor(NFCS_data$Crypto_State_Year)
levels(NFCS_data$Crypto_State_Year)
```

# Regression Models
## Model with Demographic information only
```{r Model with Demographic information only}
Demographic_Model <- lm(Score ~ Age 
                        + Income 
                        + Education 
                        + Marital_Status 
                        + Children
                        , data = NFCS_data)

summary(Demographic_Model)

plot(Demographic_Model)
```

## Model with Demographic information & Crypto State-Year Variable
```{r Model with Demographic information & Crypto State-Year Variable}
Demo_Crypto_Model <- lm(Score ~ Age 
                        + Income 
                        + Education 
                        + Marital_Status 
                        + Children 
                        + Crypto_State_Year
                        , data = NFCS_data)
summary(Demo_Crypto_Model)

plot(Demo_Crypto_Model)
```


## Model with Demographic information & Crypto State-Year Variable & interaction of Age and Crypto State-Year variable
```{r Model with Demographic information & Crypto State-Year Variable & interaction of Age and Crypto State-Year variable}
Demo_Crypto_Interaction_Model <- lm(Score ~ Age
                                    + Income 
                                    + Education 
                                    + Marital_Status 
                                    + Children 
                                    + Crypto_State_Year 
                                    + Age*Crypto_State_Year
                                    , data = NFCS_data)
summary(Demo_Crypto_Interaction_Model)

#plot(Demo_Crypto_Interaction_Model)
```


