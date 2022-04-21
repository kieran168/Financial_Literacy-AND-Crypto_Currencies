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
      - [Model \#3a: Demographic information & Crypto State-Year
        Variable & interaction of Age\_24\_or\_younger and Crypto
        State-Year
        variable](#model-3a-demographic-information--crypto-state-year-variable--interaction-of-age_24_or_younger-and-crypto-state-year-variable)
      - [Model \#3b: Demographic information & Crypto State-Year
        Variable & interaction of Age\_34\_or\_younger and Crypto
        State-Year
        variable](#model-3b-demographic-information--crypto-state-year-variable--interaction-of-age_34_or_younger-and-crypto-state-year-variable)
      - [Model \#3c: Demographic information & Crypto State-Year
        Variable & interaction of Age\_44\_or\_younger and Crypto
        State-Year
        variable](#model-3c-demographic-information--crypto-state-year-variable--interaction-of-age_44_or_younger-and-crypto-state-year-variable)
      - [Model \#4: Demographic information & Crypto State-Year Variable
        & Age-squared
        variable](#model-4-demographic-information--crypto-state-year-variable--age-squared-variable)
      - [Model \#5a: Demographic information & Crypto State-Year
        Variable & interaction of Age\_24\_or\_younger and Crypto
        State-Year variable excluding MAIN EFFECT of
        Age\_24\_or\_younger](#model-5a-demographic-information--crypto-state-year-variable--interaction-of-age_24_or_younger-and-crypto-state-year-variable-excluding-main-effect-of-age_24_or_younger)
      - [Model \#5b: Demographic information & Crypto State-Year
        Variable & interaction of Age\_34\_or\_younger and Crypto
        State-Year variable excluding MAIN EFFECT of
        Age\_34\_or\_younger](#model-5b-demographic-information--crypto-state-year-variable--interaction-of-age_34_or_younger-and-crypto-state-year-variable-excluding-main-effect-of-age_34_or_younger)
      - [Model \#5c: Demographic information & Crypto State-Year
        Variable & interaction of Age\_44\_or\_younger and Crypto
        State-Year variable excluding MAIN EFFECT of
        Age\_44\_or\_younger](#model-5c-demographic-information--crypto-state-year-variable--interaction-of-age_44_or_younger-and-crypto-state-year-variable-excluding-main-effect-of-age_44_or_younger)
  - [Multicollinearity Checks](#multicollinearity-checks)

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

## Model \#3a: Demographic information & Crypto State-Year Variable & interaction of Age\_24\_or\_younger and Crypto State-Year variable

``` r
Model_3a <- lm(Score ~ Gender
              + Age
              + Ethnicity
              + Income 
              + Education 
              + Marital_Status 
              + Children 
              + Crypto_State_Year 
              + Age_24_or_younger*Crypto_State_Year
              , data = NFCS_data)
summary(Model_3a)
```

    ## 
    ## Call:
    ## lm(formula = Score ~ Gender + Age + Ethnicity + Income + Education + 
    ##     Marital_Status + Children + Crypto_State_Year + Age_24_or_younger * 
    ##     Crypto_State_Year, data = NFCS_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8011 -0.8358  0.1226  0.9316  3.9255 
    ## 
    ## Coefficients:
    ##                                         Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                            0.0589569  0.0269786   2.185   0.0289
    ## Gender1                                0.4179732  0.0112133  37.275  < 2e-16
    ## Age                                    0.2367295  0.0048270  49.043  < 2e-16
    ## Ethnicity1                             0.3190766  0.0127660  24.994  < 2e-16
    ## Income                                 0.1352889  0.0032705  41.366  < 2e-16
    ## Education                              0.2377152  0.0041659  57.063  < 2e-16
    ## Marital_StatusMarried                  0.0761078  0.0155856   4.883 1.05e-06
    ## Marital_StatusDivorced                 0.1025932  0.0204725   5.011 5.42e-07
    ## Marital_StatusWidowed/widower         -0.0732449  0.0306449  -2.390   0.0168
    ## Children                              -0.0423426  0.0059341  -7.135 9.76e-13
    ## Crypto_State_Year1                    -0.0769446  0.0157215  -4.894 9.90e-07
    ## Age_24_or_younger1                     0.3382444  0.0242461  13.950  < 2e-16
    ## Crypto_State_Year1:Age_24_or_younger1 -0.0009182  0.0483294  -0.019   0.9848
    ##                                          
    ## (Intercept)                           *  
    ## Gender1                               ***
    ## Age                                   ***
    ## Ethnicity1                            ***
    ## Income                                ***
    ## Education                             ***
    ## Marital_StatusMarried                 ***
    ## Marital_StatusDivorced                ***
    ## Marital_StatusWidowed/widower         *  
    ## Children                              ***
    ## Crypto_State_Year1                    ***
    ## Age_24_or_younger1                    ***
    ## Crypto_State_Year1:Age_24_or_younger1    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.276 on 54642 degrees of freedom
    ## Multiple R-squared:  0.2599, Adjusted R-squared:  0.2597 
    ## F-statistic:  1599 on 12 and 54642 DF,  p-value: < 2.2e-16

## Model \#3b: Demographic information & Crypto State-Year Variable & interaction of Age\_34\_or\_younger and Crypto State-Year variable

``` r
Model_3b <- lm(Score ~ Gender
              + Age
              + Ethnicity
              + Income 
              + Education 
              + Marital_Status 
              + Children 
              + Crypto_State_Year 
              + Age_34_or_younger*Crypto_State_Year
              , data = NFCS_data)
summary(Model_3b)
```

    ## 
    ## Call:
    ## lm(formula = Score ~ Gender + Age + Ethnicity + Income + Education + 
    ##     Marital_Status + Children + Crypto_State_Year + Age_34_or_younger * 
    ##     Crypto_State_Year, data = NFCS_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.7238 -0.8412  0.1253  0.9302  4.0151 
    ## 
    ## Coefficients:
    ##                                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                            0.354995   0.033314  10.656  < 2e-16 ***
    ## Gender1                                0.414500   0.011234  36.896  < 2e-16 ***
    ## Age                                    0.187845   0.006307  29.783  < 2e-16 ***
    ## Ethnicity1                             0.320162   0.012790  25.033  < 2e-16 ***
    ## Income                                 0.132261   0.003281  40.309  < 2e-16 ***
    ## Education                              0.231876   0.004156  55.797  < 2e-16 ***
    ## Marital_StatusMarried                  0.057708   0.015566   3.707 0.000210 ***
    ## Marital_StatusDivorced                 0.076805   0.020492   3.748 0.000178 ***
    ## Marital_StatusWidowed/widower         -0.067679   0.030753  -2.201 0.027757 *  
    ## Children                              -0.066765   0.005875 -11.364  < 2e-16 ***
    ## Crypto_State_Year1                    -0.065209   0.017498  -3.727 0.000194 ***
    ## Age_34_or_younger1                    -0.045755   0.021671  -2.111 0.034742 *  
    ## Crypto_State_Year1:Age_34_or_younger1 -0.042994   0.033331  -1.290 0.197087    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.278 on 54642 degrees of freedom
    ## Multiple R-squared:  0.2571, Adjusted R-squared:  0.2569 
    ## F-statistic:  1576 on 12 and 54642 DF,  p-value: < 2.2e-16

## Model \#3c: Demographic information & Crypto State-Year Variable & interaction of Age\_44\_or\_younger and Crypto State-Year variable

``` r
Model_3c <- lm(Score ~ Gender
              + Age
              + Ethnicity
              + Income 
              + Education 
              + Marital_Status 
              + Children 
              + Crypto_State_Year 
              + Age_44_or_younger*Crypto_State_Year
              , data = NFCS_data)
summary(Model_3c)
```

    ## 
    ## Call:
    ## lm(formula = Score ~ Gender + Age + Ethnicity + Income + Education + 
    ##     Marital_Status + Children + Crypto_State_Year + Age_44_or_younger * 
    ##     Crypto_State_Year, data = NFCS_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.7089 -0.8384  0.1247  0.9271  3.9796 
    ## 
    ## Coefficients:
    ##                                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                            0.556627   0.038091  14.613  < 2e-16 ***
    ## Gender1                                0.415208   0.011226  36.986  < 2e-16 ***
    ## Age                                    0.148627   0.007299  20.364  < 2e-16 ***
    ## Ethnicity1                             0.319701   0.012782  25.011  < 2e-16 ***
    ## Income                                 0.132180   0.003271  40.405  < 2e-16 ***
    ## Education                              0.234529   0.004166  56.299  < 2e-16 ***
    ## Marital_StatusMarried                  0.060969   0.015561   3.918 8.94e-05 ***
    ## Marital_StatusDivorced                 0.075416   0.020448   3.688 0.000226 ***
    ## Marital_StatusWidowed/widower         -0.059691   0.030719  -1.943 0.052007 .  
    ## Children                              -0.058348   0.005794 -10.071  < 2e-16 ***
    ## Crypto_State_Year1                    -0.061269   0.019946  -3.072 0.002130 ** 
    ## Age_44_or_younger1                    -0.189804   0.023516  -8.071 7.09e-16 ***
    ## Crypto_State_Year1:Age_44_or_younger1 -0.034998   0.029957  -1.168 0.242705    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.277 on 54642 degrees of freedom
    ## Multiple R-squared:  0.258,  Adjusted R-squared:  0.2578 
    ## F-statistic:  1583 on 12 and 54642 DF,  p-value: < 2.2e-16

## Model \#4: Demographic information & Crypto State-Year Variable & Age-squared variable

``` r
NFCS_data$Age_squared <- NFCS_data$Age^2

Model_4 <- lm(Score ~ Gender
              + Age
              + Ethnicity
              + Income 
              + Education 
              + Marital_Status 
              + Children 
              + Crypto_State_Year
              + Age_squared
              , data = NFCS_data)
summary(Model_4)
```

    ## 
    ## Call:
    ## lm(formula = Score ~ Gender + Age + Ethnicity + Income + Education + 
    ##     Marital_Status + Children + Crypto_State_Year + Age_squared, 
    ##     data = NFCS_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.7863 -0.8384  0.1256  0.9328  3.9412 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    0.433108   0.030912  14.011  < 2e-16 ***
    ## Gender1                        0.416571   0.011232  37.086  < 2e-16 ***
    ## Age                            0.087949   0.018122   4.853 1.22e-06 ***
    ## Ethnicity1                     0.320046   0.012786  25.031  < 2e-16 ***
    ## Income                         0.134634   0.003284  41.000  < 2e-16 ***
    ## Education                      0.232122   0.004152  55.902  < 2e-16 ***
    ## Marital_StatusMarried          0.062852   0.015581   4.034 5.49e-05 ***
    ## Marital_StatusDivorced         0.093242   0.020554   4.536 5.73e-06 ***
    ## Marital_StatusWidowed/widower -0.083211   0.030745  -2.706   0.0068 ** 
    ## Children                      -0.054570   0.005946  -9.178  < 2e-16 ***
    ## Crypto_State_Year1            -0.076562   0.014893  -5.141 2.75e-07 ***
    ## Age_squared                    0.015452   0.002439   6.334 2.40e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.278 on 54643 degrees of freedom
    ## Multiple R-squared:  0.2575, Adjusted R-squared:  0.2574 
    ## F-statistic:  1723 on 11 and 54643 DF,  p-value: < 2.2e-16

``` r
library(jtools)
summ(Model_4)
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">

<tbody>

<tr>

<td style="text-align:left;font-weight: bold;">

Observations

</td>

<td style="text-align:right;">

54655

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Dependent variable

</td>

<td style="text-align:right;">

Score

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Type

</td>

<td style="text-align:right;">

OLS linear regression

</td>

</tr>

</tbody>

</table>

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">

<tbody>

<tr>

<td style="text-align:left;font-weight: bold;">

F(11,54643)

</td>

<td style="text-align:right;">

1722.96

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

R²

</td>

<td style="text-align:right;">

0.26

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Adj. R²

</td>

<td style="text-align:right;">

0.26

</td>

</tr>

</tbody>

</table>

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

Est.

</th>

<th style="text-align:right;">

S.E.

</th>

<th style="text-align:right;">

t val.

</th>

<th style="text-align:right;">

p

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;font-weight: bold;">

(Intercept)

</td>

<td style="text-align:right;">

0.43

</td>

<td style="text-align:right;">

0.03

</td>

<td style="text-align:right;">

14.01

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Gender1

</td>

<td style="text-align:right;">

0.42

</td>

<td style="text-align:right;">

0.01

</td>

<td style="text-align:right;">

37.09

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Age

</td>

<td style="text-align:right;">

0.09

</td>

<td style="text-align:right;">

0.02

</td>

<td style="text-align:right;">

4.85

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Ethnicity1

</td>

<td style="text-align:right;">

0.32

</td>

<td style="text-align:right;">

0.01

</td>

<td style="text-align:right;">

25.03

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Income

</td>

<td style="text-align:right;">

0.13

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

41.00

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Education

</td>

<td style="text-align:right;">

0.23

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

55.90

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Marital\_StatusMarried

</td>

<td style="text-align:right;">

0.06

</td>

<td style="text-align:right;">

0.02

</td>

<td style="text-align:right;">

4.03

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Marital\_StatusDivorced

</td>

<td style="text-align:right;">

0.09

</td>

<td style="text-align:right;">

0.02

</td>

<td style="text-align:right;">

4.54

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Marital\_StatusWidowed/widower

</td>

<td style="text-align:right;">

\-0.08

</td>

<td style="text-align:right;">

0.03

</td>

<td style="text-align:right;">

\-2.71

</td>

<td style="text-align:right;">

0.01

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Children

</td>

<td style="text-align:right;">

\-0.05

</td>

<td style="text-align:right;">

0.01

</td>

<td style="text-align:right;">

\-9.18

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Crypto\_State\_Year1

</td>

<td style="text-align:right;">

\-0.08

</td>

<td style="text-align:right;">

0.01

</td>

<td style="text-align:right;">

\-5.14

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Age\_squared

</td>

<td style="text-align:right;">

0.02

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

6.33

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

</tbody>

<tfoot>

<tr>

<td style="padding: 0; " colspan="100%">

<sup></sup> Standard errors: OLS

</td>

</tr>

</tfoot>

</table>

``` r
# effect_plot(Model_4, pred = Age_squared, interval = TRUE)
# effect_plot(Model_4, pred = Age, interval = TRUE)
```

## Model \#5a: Demographic information & Crypto State-Year Variable & interaction of Age\_24\_or\_younger and Crypto State-Year variable excluding MAIN EFFECT of Age\_24\_or\_younger

``` r
Model_5a <- lm(Score ~ Gender
              + Age
              + Ethnicity
              + Income 
              + Education 
              + Marital_Status 
              + Children 
              + Crypto_State_Year
              + Age_24_or_younger:Crypto_State_Year
              , data = NFCS_data)
summary(Model_5a)
```

    ## 
    ## Call:
    ## lm(formula = Score ~ Gender + Age + Ethnicity + Income + Education + 
    ##     Marital_Status + Children + Crypto_State_Year + Age_24_or_younger:Crypto_State_Year, 
    ##     data = NFCS_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8011 -0.8358  0.1226  0.9316  3.9255 
    ## 
    ## Coefficients:
    ##                                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                            0.058957   0.026979   2.185   0.0289 *  
    ## Gender1                                0.417973   0.011213  37.275  < 2e-16 ***
    ## Age                                    0.236730   0.004827  49.043  < 2e-16 ***
    ## Ethnicity1                             0.319077   0.012766  24.994  < 2e-16 ***
    ## Income                                 0.135289   0.003271  41.366  < 2e-16 ***
    ## Education                              0.237715   0.004166  57.063  < 2e-16 ***
    ## Marital_StatusMarried                  0.076108   0.015586   4.883 1.05e-06 ***
    ## Marital_StatusDivorced                 0.102593   0.020472   5.011 5.42e-07 ***
    ## Marital_StatusWidowed/widower         -0.073245   0.030645  -2.390   0.0168 *  
    ## Children                              -0.042343   0.005934  -7.135 9.76e-13 ***
    ## Crypto_State_Year1                    -0.076945   0.015721  -4.894 9.90e-07 ***
    ## Crypto_State_Year0:Age_24_or_younger1  0.338244   0.024246  13.950  < 2e-16 ***
    ## Crypto_State_Year1:Age_24_or_younger1  0.337326   0.046689   7.225 5.08e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.276 on 54642 degrees of freedom
    ## Multiple R-squared:  0.2599, Adjusted R-squared:  0.2597 
    ## F-statistic:  1599 on 12 and 54642 DF,  p-value: < 2.2e-16

``` r
# stargazer(Model_5a,type="text",omit=":")

library(jtools)
summ(Model_5a)
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">

<tbody>

<tr>

<td style="text-align:left;font-weight: bold;">

Observations

</td>

<td style="text-align:right;">

54655

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Dependent variable

</td>

<td style="text-align:right;">

Score

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Type

</td>

<td style="text-align:right;">

OLS linear regression

</td>

</tr>

</tbody>

</table>

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">

<tbody>

<tr>

<td style="text-align:left;font-weight: bold;">

F(12,54642)

</td>

<td style="text-align:right;">

1599.10

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

R²

</td>

<td style="text-align:right;">

0.26

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Adj. R²

</td>

<td style="text-align:right;">

0.26

</td>

</tr>

</tbody>

</table>

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

Est.

</th>

<th style="text-align:right;">

S.E.

</th>

<th style="text-align:right;">

t val.

</th>

<th style="text-align:right;">

p

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;font-weight: bold;">

(Intercept)

</td>

<td style="text-align:right;">

0.06

</td>

<td style="text-align:right;">

0.03

</td>

<td style="text-align:right;">

2.19

</td>

<td style="text-align:right;">

0.03

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Gender1

</td>

<td style="text-align:right;">

0.42

</td>

<td style="text-align:right;">

0.01

</td>

<td style="text-align:right;">

37.27

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Age

</td>

<td style="text-align:right;">

0.24

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

49.04

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Ethnicity1

</td>

<td style="text-align:right;">

0.32

</td>

<td style="text-align:right;">

0.01

</td>

<td style="text-align:right;">

24.99

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Income

</td>

<td style="text-align:right;">

0.14

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

41.37

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Education

</td>

<td style="text-align:right;">

0.24

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

57.06

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Marital\_StatusMarried

</td>

<td style="text-align:right;">

0.08

</td>

<td style="text-align:right;">

0.02

</td>

<td style="text-align:right;">

4.88

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Marital\_StatusDivorced

</td>

<td style="text-align:right;">

0.10

</td>

<td style="text-align:right;">

0.02

</td>

<td style="text-align:right;">

5.01

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Marital\_StatusWidowed/widower

</td>

<td style="text-align:right;">

\-0.07

</td>

<td style="text-align:right;">

0.03

</td>

<td style="text-align:right;">

\-2.39

</td>

<td style="text-align:right;">

0.02

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Children

</td>

<td style="text-align:right;">

\-0.04

</td>

<td style="text-align:right;">

0.01

</td>

<td style="text-align:right;">

\-7.14

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Crypto\_State\_Year1

</td>

<td style="text-align:right;">

\-0.08

</td>

<td style="text-align:right;">

0.02

</td>

<td style="text-align:right;">

\-4.89

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Crypto\_State\_Year0:Age\_24\_or\_younger1

</td>

<td style="text-align:right;">

0.34

</td>

<td style="text-align:right;">

0.02

</td>

<td style="text-align:right;">

13.95

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Crypto\_State\_Year1:Age\_24\_or\_younger1

</td>

<td style="text-align:right;">

0.34

</td>

<td style="text-align:right;">

0.05

</td>

<td style="text-align:right;">

7.22

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

</tbody>

<tfoot>

<tr>

<td style="padding: 0; " colspan="100%">

<sup></sup> Standard errors: OLS

</td>

</tr>

</tfoot>

</table>

## Model \#5b: Demographic information & Crypto State-Year Variable & interaction of Age\_34\_or\_younger and Crypto State-Year variable excluding MAIN EFFECT of Age\_34\_or\_younger

``` r
Model_5b <- lm(Score ~ Gender
              + Age
              + Ethnicity
              + Income 
              + Education 
              + Marital_Status 
              + Children 
              + Crypto_State_Year
              + Age_34_or_younger:Crypto_State_Year
              , data = NFCS_data)
summary(Model_5b)
```

    ## 
    ## Call:
    ## lm(formula = Score ~ Gender + Age + Ethnicity + Income + Education + 
    ##     Marital_Status + Children + Crypto_State_Year + Age_34_or_younger:Crypto_State_Year, 
    ##     data = NFCS_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.7238 -0.8412  0.1253  0.9302  4.0151 
    ## 
    ## Coefficients:
    ##                                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                            0.354995   0.033314  10.656  < 2e-16 ***
    ## Gender1                                0.414500   0.011234  36.896  < 2e-16 ***
    ## Age                                    0.187845   0.006307  29.783  < 2e-16 ***
    ## Ethnicity1                             0.320162   0.012790  25.033  < 2e-16 ***
    ## Income                                 0.132261   0.003281  40.309  < 2e-16 ***
    ## Education                              0.231876   0.004156  55.797  < 2e-16 ***
    ## Marital_StatusMarried                  0.057708   0.015566   3.707 0.000210 ***
    ## Marital_StatusDivorced                 0.076805   0.020492   3.748 0.000178 ***
    ## Marital_StatusWidowed/widower         -0.067679   0.030753  -2.201 0.027757 *  
    ## Children                              -0.066765   0.005875 -11.364  < 2e-16 ***
    ## Crypto_State_Year1                    -0.065209   0.017498  -3.727 0.000194 ***
    ## Crypto_State_Year0:Age_34_or_younger1 -0.045755   0.021671  -2.111 0.034742 *  
    ## Crypto_State_Year1:Age_34_or_younger1 -0.088749   0.035106  -2.528 0.011472 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.278 on 54642 degrees of freedom
    ## Multiple R-squared:  0.2571, Adjusted R-squared:  0.2569 
    ## F-statistic:  1576 on 12 and 54642 DF,  p-value: < 2.2e-16

``` r
library(jtools)
summ(Model_5b)
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">

<tbody>

<tr>

<td style="text-align:left;font-weight: bold;">

Observations

</td>

<td style="text-align:right;">

54655

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Dependent variable

</td>

<td style="text-align:right;">

Score

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Type

</td>

<td style="text-align:right;">

OLS linear regression

</td>

</tr>

</tbody>

</table>

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">

<tbody>

<tr>

<td style="text-align:left;font-weight: bold;">

F(12,54642)

</td>

<td style="text-align:right;">

1575.73

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

R²

</td>

<td style="text-align:right;">

0.26

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Adj. R²

</td>

<td style="text-align:right;">

0.26

</td>

</tr>

</tbody>

</table>

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

Est.

</th>

<th style="text-align:right;">

S.E.

</th>

<th style="text-align:right;">

t val.

</th>

<th style="text-align:right;">

p

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;font-weight: bold;">

(Intercept)

</td>

<td style="text-align:right;">

0.35

</td>

<td style="text-align:right;">

0.03

</td>

<td style="text-align:right;">

10.66

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Gender1

</td>

<td style="text-align:right;">

0.41

</td>

<td style="text-align:right;">

0.01

</td>

<td style="text-align:right;">

36.90

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Age

</td>

<td style="text-align:right;">

0.19

</td>

<td style="text-align:right;">

0.01

</td>

<td style="text-align:right;">

29.78

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Ethnicity1

</td>

<td style="text-align:right;">

0.32

</td>

<td style="text-align:right;">

0.01

</td>

<td style="text-align:right;">

25.03

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Income

</td>

<td style="text-align:right;">

0.13

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

40.31

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Education

</td>

<td style="text-align:right;">

0.23

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

55.80

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Marital\_StatusMarried

</td>

<td style="text-align:right;">

0.06

</td>

<td style="text-align:right;">

0.02

</td>

<td style="text-align:right;">

3.71

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Marital\_StatusDivorced

</td>

<td style="text-align:right;">

0.08

</td>

<td style="text-align:right;">

0.02

</td>

<td style="text-align:right;">

3.75

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Marital\_StatusWidowed/widower

</td>

<td style="text-align:right;">

\-0.07

</td>

<td style="text-align:right;">

0.03

</td>

<td style="text-align:right;">

\-2.20

</td>

<td style="text-align:right;">

0.03

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Children

</td>

<td style="text-align:right;">

\-0.07

</td>

<td style="text-align:right;">

0.01

</td>

<td style="text-align:right;">

\-11.36

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Crypto\_State\_Year1

</td>

<td style="text-align:right;">

\-0.07

</td>

<td style="text-align:right;">

0.02

</td>

<td style="text-align:right;">

\-3.73

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Crypto\_State\_Year0:Age\_34\_or\_younger1

</td>

<td style="text-align:right;">

\-0.05

</td>

<td style="text-align:right;">

0.02

</td>

<td style="text-align:right;">

\-2.11

</td>

<td style="text-align:right;">

0.03

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Crypto\_State\_Year1:Age\_34\_or\_younger1

</td>

<td style="text-align:right;">

\-0.09

</td>

<td style="text-align:right;">

0.04

</td>

<td style="text-align:right;">

\-2.53

</td>

<td style="text-align:right;">

0.01

</td>

</tr>

</tbody>

<tfoot>

<tr>

<td style="padding: 0; " colspan="100%">

<sup></sup> Standard errors: OLS

</td>

</tr>

</tfoot>

</table>

## Model \#5c: Demographic information & Crypto State-Year Variable & interaction of Age\_44\_or\_younger and Crypto State-Year variable excluding MAIN EFFECT of Age\_44\_or\_younger

``` r
Model_5c <- lm(Score ~ Gender
              + Age
              + Ethnicity
              + Income 
              + Education 
              + Marital_Status 
              + Children 
              + Crypto_State_Year
              + Age_44_or_younger:Crypto_State_Year
              , data = NFCS_data)
summary(Model_5c)
```

    ## 
    ## Call:
    ## lm(formula = Score ~ Gender + Age + Ethnicity + Income + Education + 
    ##     Marital_Status + Children + Crypto_State_Year + Age_44_or_younger:Crypto_State_Year, 
    ##     data = NFCS_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.7089 -0.8384  0.1247  0.9271  3.9796 
    ## 
    ## Coefficients:
    ##                                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                            0.556627   0.038091  14.613  < 2e-16 ***
    ## Gender1                                0.415208   0.011226  36.986  < 2e-16 ***
    ## Age                                    0.148627   0.007299  20.364  < 2e-16 ***
    ## Ethnicity1                             0.319701   0.012782  25.011  < 2e-16 ***
    ## Income                                 0.132180   0.003271  40.405  < 2e-16 ***
    ## Education                              0.234529   0.004166  56.299  < 2e-16 ***
    ## Marital_StatusMarried                  0.060969   0.015561   3.918 8.94e-05 ***
    ## Marital_StatusDivorced                 0.075416   0.020448   3.688 0.000226 ***
    ## Marital_StatusWidowed/widower         -0.059691   0.030719  -1.943 0.052007 .  
    ## Children                              -0.058348   0.005794 -10.071  < 2e-16 ***
    ## Crypto_State_Year1                    -0.061269   0.019946  -3.072 0.002130 ** 
    ## Crypto_State_Year0:Age_44_or_younger1 -0.189804   0.023516  -8.071 7.09e-16 ***
    ## Crypto_State_Year1:Age_44_or_younger1 -0.224801   0.034054  -6.601 4.11e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.277 on 54642 degrees of freedom
    ## Multiple R-squared:  0.258,  Adjusted R-squared:  0.2578 
    ## F-statistic:  1583 on 12 and 54642 DF,  p-value: < 2.2e-16

``` r
library(jtools)
summ(Model_5c)
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">

<tbody>

<tr>

<td style="text-align:left;font-weight: bold;">

Observations

</td>

<td style="text-align:right;">

54655

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Dependent variable

</td>

<td style="text-align:right;">

Score

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Type

</td>

<td style="text-align:right;">

OLS linear regression

</td>

</tr>

</tbody>

</table>

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">

<tbody>

<tr>

<td style="text-align:left;font-weight: bold;">

F(12,54642)

</td>

<td style="text-align:right;">

1583.10

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

R²

</td>

<td style="text-align:right;">

0.26

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Adj. R²

</td>

<td style="text-align:right;">

0.26

</td>

</tr>

</tbody>

</table>

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

Est.

</th>

<th style="text-align:right;">

S.E.

</th>

<th style="text-align:right;">

t val.

</th>

<th style="text-align:right;">

p

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;font-weight: bold;">

(Intercept)

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

0.04

</td>

<td style="text-align:right;">

14.61

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Gender1

</td>

<td style="text-align:right;">

0.42

</td>

<td style="text-align:right;">

0.01

</td>

<td style="text-align:right;">

36.99

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Age

</td>

<td style="text-align:right;">

0.15

</td>

<td style="text-align:right;">

0.01

</td>

<td style="text-align:right;">

20.36

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Ethnicity1

</td>

<td style="text-align:right;">

0.32

</td>

<td style="text-align:right;">

0.01

</td>

<td style="text-align:right;">

25.01

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Income

</td>

<td style="text-align:right;">

0.13

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

40.41

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Education

</td>

<td style="text-align:right;">

0.23

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

56.30

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Marital\_StatusMarried

</td>

<td style="text-align:right;">

0.06

</td>

<td style="text-align:right;">

0.02

</td>

<td style="text-align:right;">

3.92

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Marital\_StatusDivorced

</td>

<td style="text-align:right;">

0.08

</td>

<td style="text-align:right;">

0.02

</td>

<td style="text-align:right;">

3.69

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Marital\_StatusWidowed/widower

</td>

<td style="text-align:right;">

\-0.06

</td>

<td style="text-align:right;">

0.03

</td>

<td style="text-align:right;">

\-1.94

</td>

<td style="text-align:right;">

0.05

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Children

</td>

<td style="text-align:right;">

\-0.06

</td>

<td style="text-align:right;">

0.01

</td>

<td style="text-align:right;">

\-10.07

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Crypto\_State\_Year1

</td>

<td style="text-align:right;">

\-0.06

</td>

<td style="text-align:right;">

0.02

</td>

<td style="text-align:right;">

\-3.07

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Crypto\_State\_Year0:Age\_44\_or\_younger1

</td>

<td style="text-align:right;">

\-0.19

</td>

<td style="text-align:right;">

0.02

</td>

<td style="text-align:right;">

\-8.07

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Crypto\_State\_Year1:Age\_44\_or\_younger1

</td>

<td style="text-align:right;">

\-0.22

</td>

<td style="text-align:right;">

0.03

</td>

<td style="text-align:right;">

\-6.60

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

</tbody>

<tfoot>

<tr>

<td style="padding: 0; " colspan="100%">

<sup></sup> Standard errors: OLS

</td>

</tr>

</tfoot>

</table>

# Multicollinearity Checks

``` r
car::vif(Model_1)
```

    ##                    GVIF Df GVIF^(1/(2*Df))
    ## Gender         1.041828  1        1.020700
    ## Age            1.547397  1        1.243944
    ## Ethnicity      1.075725  1        1.037172
    ## Income         1.520756  1        1.233189
    ## Education      1.202145  1        1.096424
    ## Marital_Status 1.859677  3        1.108935
    ## Children       1.235222  1        1.111406

``` r
car::vif(Model_2)
```

    ##                       GVIF Df GVIF^(1/(2*Df))
    ## Gender            1.041911  1        1.020740
    ## Age               1.547398  1        1.243944
    ## Ethnicity         1.076935  1        1.037755
    ## Income            1.520843  1        1.233225
    ## Education         1.202373  1        1.096528
    ## Marital_Status    1.859725  3        1.108940
    ## Children          1.235240  1        1.111413
    ## Crypto_State_Year 1.001582  1        1.000791

``` r
car::vif(Model_3a)
```

    ##                                         GVIF Df GVIF^(1/(2*Df))
    ## Gender                              1.042278  1        1.020920
    ## Age                                 2.124870  1        1.457693
    ## Ethnicity                           1.077040  1        1.037805
    ## Income                              1.524885  1        1.234862
    ## Education                           1.214691  1        1.102130
    ## Marital_Status                      1.878182  3        1.110767
    ## Children                            1.316160  1        1.147240
    ## Crypto_State_Year                   1.119644  1        1.058132
    ## Age_24_or_younger                   1.885559  1        1.373157
    ## Crypto_State_Year:Age_24_or_younger 1.306486  1        1.143016

``` r
car::vif(Model_3b)
```

    ##                                         GVIF Df GVIF^(1/(2*Df))
    ## Gender                              1.042195  1        1.020879
    ## Age                                 3.613934  1        1.901035
    ## Ethnicity                           1.076954  1        1.037764
    ## Income                              1.528990  1        1.236523
    ## Education                           1.204168  1        1.097346
    ## Marital_Status                      1.879884  3        1.110935
    ## Children                            1.285261  1        1.133694
    ## Crypto_State_Year                   1.381757  1        1.175482
    ## Age_34_or_younger                   3.197511  1        1.788159
    ## Crypto_State_Year:Age_34_or_younger 1.561064  1        1.249425

``` r
car::vif(Model_3c)
```

    ##                                         GVIF Df GVIF^(1/(2*Df))
    ## Gender                              1.041936  1        1.020753
    ## Age                                 4.845386  1        2.201224
    ## Ethnicity                           1.077011  1        1.037791
    ## Income                              1.521690  1        1.233568
    ## Education                           1.211489  1        1.100676
    ## Marital_Status                      1.869479  3        1.109907
    ## Children                            1.251286  1        1.118609
    ## Crypto_State_Year                   1.797574  1        1.340736
    ## Age_44_or_younger                   4.586361  1        2.141579
    ## Crypto_State_Year:Age_44_or_younger 1.980103  1        1.407161

``` r
#variance inflation factor: measures how much the variance of a regression coefficient is inflated due to multicollinearity in the model
#The smallest possible value of VIF is one (absence of multicollinearity). As a rule of thumb, a VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity (James et al. 2014).
```
