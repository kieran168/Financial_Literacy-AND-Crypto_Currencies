Financial Literacy and Cryptocurrency
================
Kieran Yuen

  - [R setup](#r-setup)
  - [Loading in NFCS data](#loading-in-nfcs-data)
  - [Cleaning Data](#cleaning-data)
  - [Changing Data Types for
    variables](#changing-data-types-for-variables)
  - [Regression Models](#regression-models)
      - [Model \#1: Demographic information
        only](#model-1-demographic-information-only)
      - [Model \#2: Demographic information & Crypto State-Year
        Variable](#model-2-demographic-information--crypto-state-year-variable)

# R setup

``` r
knitr::opts_chunk$set(echo = TRUE)
```

# Loading in NFCS data

``` r
library(readxl)
X04092022_NFCS_2018_State_Tracking_Data_190623 <- read_excel("04092022_NFCS 2018 State Tracking Data 190623.xlsx")
```

# Cleaning Data

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.6     ✓ dplyr   1.0.6
    ## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
    ## ✓ readr   2.1.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
NFCS_data <- X04092022_NFCS_2018_State_Tracking_Data_190623 %>% 
  select(TRACK,
         NFCSID,
         STATEQ,
         A3,
         A3Ar_w,
         A4A_new_w, 
         A5_2015,
         A8,
         A6,
         A11,
         M6,
         M7,
         M8,
         M9,
         M10) %>%
  
  rename(Year = TRACK,
         NFCS_ID = NFCSID,
         State = STATEQ,
         Gender = A3,
         Age = A3Ar_w,
         Ethnicity = A4A_new_w,
         Education = A5_2015,
         Income = A8,
         Marital_Status = A6,
         Children = A11,
         Question_1 = M6,
         Question_2 = M7,
         Question_3 = M8,
         Question_4 = M9,
         Question_5 = M10) %>%
  
  na.omit() %>%
  
  filter(Year =="2018"|
           Year == "2015") %>%
  
  mutate(Gender = recode(Gender,
                      '1' = 1,
                      '2' = 0)) %>%
  
  mutate(Age_24_or_younger = Age == 1,
         Age_24_or_younger = ifelse(Age_24_or_younger == TRUE, 1, 0)) %>%
  
  mutate(Age_34_or_younger = (Age == 1 | Age == 2),
         Age_34_or_younger = ifelse(Age_34_or_younger == TRUE, 1, 0)) %>%
  
  mutate(Age_44_or_younger = (Age == 1 | Age == 2 | Age == 3),
         Age_44_or_younger = ifelse(Age_44_or_younger == TRUE, 1, 0)) %>%
  
  mutate(Ethnicity = recode(Ethnicity,
                      '1' = 1,
                      '2' = 0)) %>%
  
  mutate(Education = recode(Education,
                      '1' = 1,
                      '2' = 2,
                      '3' = 2,
                      '4' = 3,
                      '5' = 4,
                      '6' = 5,
                      '7' = 6,
                      '99' = NA_real_)) %>%
  
  mutate(Income = recode(Income,
                      '1' = 1,
                      '2' = 2,
                      '3' = 3,
                      '4' = 4,
                      '5' = 5,
                      '6' = 6,
                      '7' = 7,
                      '8' = 8,
                      '98' = NA_real_,
                      '99' = NA_real_)) %>%
  
  mutate(Marital_Status = recode(Marital_Status,
                      '1' = "Married",
                      '2' = "Single",
                      '3' = "Divorced",
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

# Changing Data Types for variables

# Regression Models

## Model \#1: Demographic information only

``` r
Model_1 <- lm(Score ~ Gender
              + Age
              + Ethnicity
              + Income 
              + Education 
              + Marital_Status 
              + Children
              , data = NFCS_data)
summary(Model_1)
```

    ## 
    ## Call:
    ## lm(formula = Score ~ Gender + Age + Ethnicity + Income + Education + 
    ##     Marital_Status + Children, data = NFCS_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.7243 -0.8415  0.1259  0.9296  3.9212 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    0.282726   0.021653  13.057  < 2e-16 ***
    ## Gender1                        0.414365   0.011235  36.880  < 2e-16 ***
    ## Age                            0.199718   0.004128  48.379  < 2e-16 ***
    ## Ethnicity1                     0.318100   0.012786  24.878  < 2e-16 ***
    ## Income                         0.132703   0.003273  40.541  < 2e-16 ***
    ## Education                      0.231843   0.004153  55.820  < 2e-16 ***
    ## Marital_StatusMarried          0.058142   0.015569   3.734 0.000188 ***
    ## Marital_StatusDivorced         0.080129   0.020459   3.916    9e-05 ***
    ## Marital_StatusWidowed/widower -0.072109   0.030712  -2.348 0.018882 *  
    ## Children                      -0.063848   0.005761 -11.082  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.279 on 54645 degrees of freedom
    ## Multiple R-squared:  0.2566, Adjusted R-squared:  0.2565 
    ## F-statistic:  2096 on 9 and 54645 DF,  p-value: < 2.2e-16

``` r
nobs(Model_1) #Number of Observations
```

    ## [1] 54655

## Model \#2: Demographic information & Crypto State-Year Variable

``` r
Model_2 <- lm(Score ~ Gender
              + Age
              + Ethnicity
              + Income 
              + Education 
              + Marital_Status 
              + Children 
              + Crypto_State_Year
              , data = NFCS_data)
summary(Model_2)
```

    ## 
    ## Call:
    ## lm(formula = Score ~ Gender + Age + Ethnicity + Income + Education + 
    ##     Marital_Status + Children + Crypto_State_Year, data = NFCS_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.7373 -0.8427  0.1255  0.9313  3.9872 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    0.293973   0.021758  13.511  < 2e-16 ***
    ## Gender1                        0.414884   0.011233  36.933  < 2e-16 ***
    ## Age                            0.199727   0.004127  48.392  < 2e-16 ***
    ## Ethnicity1                     0.320310   0.012790  25.043  < 2e-16 ***
    ## Income                         0.132831   0.003273  40.589  < 2e-16 ***
    ## Education                      0.231549   0.004153  55.757  < 2e-16 ***
    ## Marital_StatusMarried          0.057798   0.015566   3.713 0.000205 ***
    ## Marital_StatusDivorced         0.079994   0.020455   3.911 9.21e-05 ***
    ## Marital_StatusWidowed/widower -0.071962   0.030705  -2.344 0.019099 *  
    ## Children                      -0.063961   0.005760 -11.104  < 2e-16 ***
    ## Crypto_State_Year1            -0.076814   0.014899  -5.156 2.53e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.278 on 54644 degrees of freedom
    ## Multiple R-squared:  0.257,  Adjusted R-squared:  0.2568 
    ## F-statistic:  1890 on 10 and 54644 DF,  p-value: < 2.2e-16

``` r
nobs(Model_2) #Number of Observations
```

    ## [1] 54655
