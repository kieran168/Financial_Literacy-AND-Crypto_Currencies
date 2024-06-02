### Analysis of the Influence of Cryptocurrency Regulation on Levels of Financial Literacy

Welcome to my master's thesis project repository! This project is a comprehensive analysis of how cryptocurrency regulations impact financial literacy across different states in the U.S. Using data from the National Financial Capability Study (NFCS) and various statistical methods, including OLS regression, this research provides valuable insights into the intersection of financial policy and public financial education.

#### Key Skills Demonstrated:
- **Data Analysis and Statistical Modeling**: Proficiency in using statistical software for data analysis, including creating and interpreting regression models.
- **Economics Research**: Deep understanding of economic principles and their application to real-world issues.
- **Programming and Data Handling**: Extensive use of R programming for data manipulation, cleaning, and analysis.
- **Critical Thinking and Problem Solving**: Ability to hypothesize, test, and draw conclusions from complex data sets.
- **Communication**: Clear presentation of findings through well-documented code and thorough explanations.

#### Project Highlights:
- **Objective**: Investigate the effect of state-level cryptocurrency regulation on financial literacy rates.
- **Methodology**: Used a mix of demographic and policy variables to assess their impact on financial literacy scores derived from NFCS data.
- **Findings**: Discovered a small but significant negative effect of cryptocurrency regulation on financial literacy, suggesting regulatory actions influence public financial knowledge.

Feel free to explore the code, review the analyses, and contact me with any questions or feedback. I'm always open to discussions and collaborations on similar topics.

================

  - [Loading in NFCS data](#loading-in-nfcs-data)
  - [Cleaning Data](#cleaning-data)
  - [Changing Data Types for
    variables](#changing-data-types-for-variables)
  - [Summary Statistics](#summary-statistics)
      - [Summary Statistics: Overall](#summary-statistics-overall)
      - [Summary Statistics:
        Crypto\_State\_Year](#summary-statistics-crypto_state_year)
  - [Regression Models](#regression-models)
      - [Model \#1: Demographic information
        only](#model-1-demographic-information-only)
      - [Model \#2: Demographic information & Crypto State-Year
        Variable](#model-2-demographic-information--crypto-state-year-variable)
      - [Regression Results Table](#regression-results-table)

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
                      '1' = 1,
                      '2' = 0,
                      '3' = 0,
                      '4' = 0,
                      '5' = 0,
                      '99' = NA_real_)) %>%
  
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

# Summary Statistics

## Summary Statistics: Overall

``` r
library(psych)
```

    ## Warning: package 'psych' was built under R version 4.0.5

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
#Overall
mean(NFCS_data$Score)
```

    ## [1] 2.878474

``` r
sd(NFCS_data$Score)
```

    ## [1] 1.482734

``` r
mean(NFCS_data$Age)
```

    ## [1] 3.723045

``` r
sd(NFCS_data$Age)
```

    ## [1] 1.64791

``` r
mean(NFCS_data$Education)
```

    ## [1] 3.670826

``` r
sd(NFCS_data$Education)
```

    ## [1] 1.443682

``` r
mean(NFCS_data$Income)
```

    ## [1] 4.4434

``` r
sd(NFCS_data$Income)
```

    ## [1] 2.060351

``` r
mean(NFCS_data$Children)
```

    ## [1] 0.6711554

``` r
sd(NFCS_data$Children)
```

    ## [1] 1.054978

``` r
describeBy(NFCS_data)
```

    ## Warning in describeBy(NFCS_data): no grouping variable requested

    ##                    vars     n         mean         sd     median      trimmed
    ## State_Year*           1 54655 5.236000e+01      29.38         53 5.258000e+01
    ## Year                  2 54655 2.016490e+03       1.50       2015 2.016480e+03
    ## NFCS_ID               3 54655 2.016511e+09 1499860.05 2015037328 2.016507e+09
    ## State                 4 54655 2.640000e+01      14.83         27 2.648000e+01
    ## Gender*               5 54655 1.440000e+00       0.50          1 1.430000e+00
    ## Age                   6 54655 3.720000e+00       1.65          4 3.780000e+00
    ## Ethnicity*            7 54655 1.730000e+00       0.44          2 1.790000e+00
    ## Education             8 54655 3.670000e+00       1.44          3 3.620000e+00
    ## Income                9 54655 4.440000e+00       2.06          5 4.480000e+00
    ## Marital_Status*      10 54655 1.540000e+00       0.50          2 1.550000e+00
    ## Children             11 54655 6.700000e-01       1.05          0 4.600000e-01
    ## Question_1           12 48035 8.600000e-01       0.35          1 9.500000e-01
    ## Question_2           13 43354 7.500000e-01       0.43          1 8.200000e-01
    ## Question_3           14 33957 4.600000e-01       0.50          0 4.500000e-01
    ## Question_4           15 45979 9.100000e-01       0.28          1 1.000000e+00
    ## Question_5           16 30933 8.300000e-01       0.37          1 9.200000e-01
    ## Score                17 54655 2.880000e+00       1.48          3 2.940000e+00
    ## Crypto_State_Year*   18 54655 1.160000e+00       0.37          1 1.080000e+00
    ##                         mad        min        max   range  skew kurtosis
    ## State_Year*           38.55          1        102     101 -0.06    -1.21
    ## Year                   0.00       2015       2018       3  0.02    -2.00
    ## NFCS_ID            40165.12 2015010001 2018037091 3027090  0.02    -2.00
    ## State                 19.27          1         51      50 -0.04    -1.23
    ## Gender*                0.00          1          2       1  0.23    -1.95
    ## Age                    1.48          1          6       5 -0.10    -1.21
    ## Ethnicity*             0.00          1          2       1 -1.04    -0.92
    ## Education              1.48          1          6       5  0.19    -1.22
    ## Income                 2.97          1          8       7 -0.12    -0.98
    ## Marital_Status*        0.00          1          2       1 -0.16    -1.98
    ## Children               0.00          0          4       4  1.52     1.42
    ## Question_1             0.00          0          1       1 -2.09     2.37
    ## Question_2             0.00          0          1       1 -1.18    -0.60
    ## Question_3             0.00          0          1       1  0.18    -1.97
    ## Question_4             0.00          0          1       1 -2.91     6.49
    ## Question_5             0.00          0          1       1 -1.80     1.23
    ## Score                  1.48          0          5       5 -0.30    -0.86
    ## Crypto_State_Year*     0.00          1          2       1  1.85     1.41
    ##                         se
    ## State_Year*           0.13
    ## Year                  0.01
    ## NFCS_ID            6415.58
    ## State                 0.06
    ## Gender*               0.00
    ## Age                   0.01
    ## Ethnicity*            0.00
    ## Education             0.01
    ## Income                0.01
    ## Marital_Status*       0.00
    ## Children              0.00
    ## Question_1            0.00
    ## Question_2            0.00
    ## Question_3            0.00
    ## Question_4            0.00
    ## Question_5            0.00
    ## Score                 0.01
    ## Crypto_State_Year*    0.00

``` r
#Crypto_State_Year=0
describeBy(NFCS_data, group = NFCS_data$Crypto_State_Year)
```

    ## 
    ##  Descriptive statistics by group 
    ## group: 0
    ##                    vars     n         mean         sd     median      trimmed
    ## State_Year*           1 45871 4.397000e+01      24.76         45 4.411000e+01
    ## Year                  2 45871 2.016330e+03       1.49       2015 2.016290e+03
    ## NFCS_ID               3 45871 2.016352e+09 1490320.64 2015034659 2.016309e+09
    ## State                 4 45871 2.570000e+01      14.34         25 2.559000e+01
    ## Gender*               5 45871 1.440000e+00       0.50          1 1.430000e+00
    ## Age                   6 45871 3.720000e+00       1.65          4 3.770000e+00
    ## Ethnicity*            7 45871 1.720000e+00       0.45          2 1.780000e+00
    ## Education             8 45871 3.680000e+00       1.44          3 3.630000e+00
    ## Income                9 45871 4.440000e+00       2.06          5 4.470000e+00
    ## Marital_Status*      10 45871 1.540000e+00       0.50          2 1.550000e+00
    ## Children             11 45871 6.700000e-01       1.06          0 4.600000e-01
    ## Question_1           12 40371 8.600000e-01       0.35          1 9.500000e-01
    ## Question_2           13 36500 7.600000e-01       0.43          1 8.200000e-01
    ## Question_3           14 28471 4.600000e-01       0.50          0 4.500000e-01
    ## Question_4           15 38594 9.100000e-01       0.28          1 1.000000e+00
    ## Question_5           16 26064 8.400000e-01       0.37          1 9.200000e-01
    ## Score                17 45871 2.890000e+00       1.48          3 2.950000e+00
    ## Crypto_State_Year*   18 45871 1.000000e+00       0.00          1 1.000000e+00
    ##                         mad        min        max   range  skew kurtosis
    ## State_Year*           32.62          1         86      85 -0.05    -1.20
    ## Year                   0.00       2015       2018       3  0.23    -1.95
    ## NFCS_ID            32421.50 2015010001 2018037091 3027090  0.23    -1.95
    ## State                 17.79          1         51      50  0.03    -1.11
    ## Gender*                0.00          1          2       1  0.24    -1.94
    ## Age                    1.48          1          6       5 -0.10    -1.22
    ## Ethnicity*             0.00          1          2       1 -1.00    -1.00
    ## Education              1.48          1          6       5  0.19    -1.22
    ## Income                 2.97          1          8       7 -0.11    -0.97
    ## Marital_Status*        0.00          1          2       1 -0.16    -1.98
    ## Children               0.00          0          4       4  1.51     1.38
    ## Question_1             0.00          0          1       1 -2.09     2.38
    ## Question_2             0.00          0          1       1 -1.20    -0.56
    ## Question_3             0.00          0          1       1  0.17    -1.97
    ## Question_4             0.00          0          1       1 -2.92     6.55
    ## Question_5             0.00          0          1       1 -1.81     1.27
    ## Score                  1.48          0          5       5 -0.30    -0.85
    ## Crypto_State_Year*     0.00          1          1       0   NaN      NaN
    ##                         se
    ## State_Year*           0.12
    ## Year                  0.01
    ## NFCS_ID            6958.42
    ## State                 0.07
    ## Gender*               0.00
    ## Age                   0.01
    ## Ethnicity*            0.00
    ## Education             0.01
    ## Income                0.01
    ## Marital_Status*       0.00
    ## Children              0.00
    ## Question_1            0.00
    ## Question_2            0.00
    ## Question_3            0.00
    ## Question_4            0.00
    ## Question_5            0.00
    ## Score                 0.01
    ## Crypto_State_Year*    0.00
    ## ------------------------------------------------------------ 
    ## group: 1
    ##                    vars    n         mean         sd     median      trimmed
    ## State_Year*           1 8784 8.960000e+00       4.67          9 9.080000e+00
    ## Year                  2 8784 2.017320e+03       1.26       2018 2.017520e+03
    ## NFCS_ID               3 8784 2.017338e+09 1258174.29 2018018283 2.017541e+09
    ## State                 4 8784 3.005000e+01      16.72         34 3.112000e+01
    ## Gender*               5 8784 1.450000e+00       0.50          1 1.440000e+00
    ## Age                   6 8784 3.760000e+00       1.64          4 3.820000e+00
    ## Ethnicity*            7 8784 1.770000e+00       0.42          2 1.830000e+00
    ## Education             8 8784 3.630000e+00       1.46          3 3.580000e+00
    ## Income                9 8784 4.470000e+00       2.06          5 4.510000e+00
    ## Marital_Status*      10 8784 1.540000e+00       0.50          2 1.550000e+00
    ## Children             11 8784 6.500000e-01       1.05          0 4.300000e-01
    ## Question_1           12 7664 8.600000e-01       0.35          1 9.500000e-01
    ## Question_2           13 6854 7.400000e-01       0.44          1 8.000000e-01
    ## Question_3           14 5486 4.500000e-01       0.50          0 4.300000e-01
    ## Question_4           15 7385 9.100000e-01       0.29          1 1.000000e+00
    ## Question_5           16 4869 8.300000e-01       0.38          1 9.100000e-01
    ## Score                17 8784 2.830000e+00       1.49          3 2.890000e+00
    ## Crypto_State_Year*   18 8784 2.000000e+00       0.00          2 2.000000e+00
    ##                         mad        min        max   range  skew kurtosis
    ## State_Year*            5.93          1         16      15 -0.16    -1.27
    ## Year                   0.00       2015       2018       3 -1.29    -0.32
    ## NFCS_ID            12166.96 2015010013 2018036552 3026539 -1.29    -0.32
    ## State                 17.79          1         48      47 -0.45    -1.44
    ## Gender*                0.00          1          2       1  0.18    -1.97
    ## Age                    1.48          1          6       5 -0.14    -1.19
    ## Ethnicity*             0.00          1          2       1 -1.26    -0.41
    ## Education              1.48          1          6       5  0.20    -1.23
    ## Income                 2.97          1          8       7 -0.14    -0.98
    ## Marital_Status*        0.00          1          2       1 -0.16    -1.97
    ## Children               0.00          0          4       4  1.58     1.61
    ## Question_1             0.00          0          1       1 -2.08     2.34
    ## Question_2             0.00          0          1       1 -1.10    -0.79
    ## Question_3             0.00          0          1       1  0.21    -1.96
    ## Question_4             0.00          0          1       1 -2.86     6.19
    ## Question_5             0.00          0          1       1 -1.75     1.05
    ## Score                  1.48          0          5       5 -0.26    -0.87
    ## Crypto_State_Year*     0.00          2          2       0   NaN      NaN
    ##                          se
    ## State_Year*            0.05
    ## Year                   0.01
    ## NFCS_ID            13424.39
    ## State                  0.18
    ## Gender*                0.01
    ## Age                    0.02
    ## Ethnicity*             0.00
    ## Education              0.02
    ## Income                 0.02
    ## Marital_Status*        0.01
    ## Children               0.01
    ## Question_1             0.00
    ## Question_2             0.01
    ## Question_3             0.01
    ## Question_4             0.00
    ## Question_5             0.01
    ## Score                  0.02
    ## Crypto_State_Year*     0.00

## Summary Statistics: Crypto\_State\_Year

``` r
library(gmodels)

CrossTable(NFCS_data$Score, NFCS_data$Crypto_State_Year, prop.chisq=FALSE, prop.t=FALSE, prop.r=TRUE, prop.c=TRUE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  54655 
    ## 
    ##  
    ##                 | NFCS_data$Crypto_State_Year 
    ## NFCS_data$Score |         0 |         1 | Row Total | 
    ## ----------------|-----------|-----------|-----------|
    ##               0 |      3455 |       728 |      4183 | 
    ##                 |     0.826 |     0.174 |     0.077 | 
    ##                 |     0.075 |     0.083 |           | 
    ## ----------------|-----------|-----------|-----------|
    ##               1 |      5584 |      1068 |      6652 | 
    ##                 |     0.839 |     0.161 |     0.122 | 
    ##                 |     0.122 |     0.122 |           | 
    ## ----------------|-----------|-----------|-----------|
    ##               2 |      8617 |      1732 |     10349 | 
    ##                 |     0.833 |     0.167 |     0.189 | 
    ##                 |     0.188 |     0.197 |           | 
    ## ----------------|-----------|-----------|-----------|
    ##               3 |     10334 |      1977 |     12311 | 
    ##                 |     0.839 |     0.161 |     0.225 | 
    ##                 |     0.225 |     0.225 |           | 
    ## ----------------|-----------|-----------|-----------|
    ##               4 |     10786 |      1974 |     12760 | 
    ##                 |     0.845 |     0.155 |     0.233 | 
    ##                 |     0.235 |     0.225 |           | 
    ## ----------------|-----------|-----------|-----------|
    ##               5 |      7095 |      1305 |      8400 | 
    ##                 |     0.845 |     0.155 |     0.154 | 
    ##                 |     0.155 |     0.149 |           | 
    ## ----------------|-----------|-----------|-----------|
    ##    Column Total |     45871 |      8784 |     54655 | 
    ##                 |     0.839 |     0.161 |           | 
    ## ----------------|-----------|-----------|-----------|
    ## 
    ## 

``` r
CrossTable(NFCS_data$Gender, NFCS_data$Crypto_State_Year, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=TRUE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  54655 
    ## 
    ##  
    ##                  | NFCS_data$Crypto_State_Year 
    ## NFCS_data$Gender |         0 |         1 | Row Total | 
    ## -----------------|-----------|-----------|-----------|
    ##                0 |     25617 |      4789 |     30406 | 
    ##                  |     0.842 |     0.158 |     0.556 | 
    ##                  |     0.558 |     0.545 |           | 
    ## -----------------|-----------|-----------|-----------|
    ##                1 |     20254 |      3995 |     24249 | 
    ##                  |     0.835 |     0.165 |     0.444 | 
    ##                  |     0.442 |     0.455 |           | 
    ## -----------------|-----------|-----------|-----------|
    ##     Column Total |     45871 |      8784 |     54655 | 
    ##                  |     0.839 |     0.161 |           | 
    ## -----------------|-----------|-----------|-----------|
    ## 
    ## 

``` r
CrossTable(NFCS_data$Age,NFCS_data$Crypto_State_Year, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=TRUE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  54655 
    ## 
    ##  
    ##               | NFCS_data$Crypto_State_Year 
    ## NFCS_data$Age |         0 |         1 | Row Total | 
    ## --------------|-----------|-----------|-----------|
    ##             1 |      4919 |       926 |      5845 | 
    ##               |     0.842 |     0.158 |     0.107 | 
    ##               |     0.107 |     0.105 |           | 
    ## --------------|-----------|-----------|-----------|
    ##             2 |      8224 |      1475 |      9699 | 
    ##               |     0.848 |     0.152 |     0.177 | 
    ##               |     0.179 |     0.168 |           | 
    ## --------------|-----------|-----------|-----------|
    ##             3 |      7635 |      1474 |      9109 | 
    ##               |     0.838 |     0.162 |     0.167 | 
    ##               |     0.166 |     0.168 |           | 
    ## --------------|-----------|-----------|-----------|
    ##             4 |      8112 |      1583 |      9695 | 
    ##               |     0.837 |     0.163 |     0.177 | 
    ##               |     0.177 |     0.180 |           | 
    ## --------------|-----------|-----------|-----------|
    ##             5 |      8126 |      1583 |      9709 | 
    ##               |     0.837 |     0.163 |     0.178 | 
    ##               |     0.177 |     0.180 |           | 
    ## --------------|-----------|-----------|-----------|
    ##             6 |      8855 |      1743 |     10598 | 
    ##               |     0.836 |     0.164 |     0.194 | 
    ##               |     0.193 |     0.198 |           | 
    ## --------------|-----------|-----------|-----------|
    ##  Column Total |     45871 |      8784 |     54655 | 
    ##               |     0.839 |     0.161 |           | 
    ## --------------|-----------|-----------|-----------|
    ## 
    ## 

``` r
CrossTable(NFCS_data$Ethnicity,NFCS_data$Crypto_State_Year, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=TRUE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  54655 
    ## 
    ##  
    ##                     | NFCS_data$Crypto_State_Year 
    ## NFCS_data$Ethnicity |         0 |         1 | Row Total | 
    ## --------------------|-----------|-----------|-----------|
    ##                   0 |     12671 |      2049 |     14720 | 
    ##                     |     0.861 |     0.139 |     0.269 | 
    ##                     |     0.276 |     0.233 |           | 
    ## --------------------|-----------|-----------|-----------|
    ##                   1 |     33200 |      6735 |     39935 | 
    ##                     |     0.831 |     0.169 |     0.731 | 
    ##                     |     0.724 |     0.767 |           | 
    ## --------------------|-----------|-----------|-----------|
    ##        Column Total |     45871 |      8784 |     54655 | 
    ##                     |     0.839 |     0.161 |           | 
    ## --------------------|-----------|-----------|-----------|
    ## 
    ## 

``` r
CrossTable(NFCS_data$Education,NFCS_data$Crypto_State_Year, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=TRUE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  54655 
    ## 
    ##  
    ##                     | NFCS_data$Crypto_State_Year 
    ## NFCS_data$Education |         0 |         1 | Row Total | 
    ## --------------------|-----------|-----------|-----------|
    ##                   1 |      1037 |       251 |      1288 | 
    ##                     |     0.805 |     0.195 |     0.024 | 
    ##                     |     0.023 |     0.029 |           | 
    ## --------------------|-----------|-----------|-----------|
    ##                   2 |     10744 |      2220 |     12964 | 
    ##                     |     0.829 |     0.171 |     0.237 | 
    ##                     |     0.234 |     0.253 |           | 
    ## --------------------|-----------|-----------|-----------|
    ##                   3 |     12708 |      2294 |     15002 | 
    ##                     |     0.847 |     0.153 |     0.274 | 
    ##                     |     0.277 |     0.261 |           | 
    ## --------------------|-----------|-----------|-----------|
    ##                   4 |      4981 |       907 |      5888 | 
    ##                     |     0.846 |     0.154 |     0.108 | 
    ##                     |     0.109 |     0.103 |           | 
    ## --------------------|-----------|-----------|-----------|
    ##                   5 |     10267 |      1956 |     12223 | 
    ##                     |     0.840 |     0.160 |     0.224 | 
    ##                     |     0.224 |     0.223 |           | 
    ## --------------------|-----------|-----------|-----------|
    ##                   6 |      6134 |      1156 |      7290 | 
    ##                     |     0.841 |     0.159 |     0.133 | 
    ##                     |     0.134 |     0.132 |           | 
    ## --------------------|-----------|-----------|-----------|
    ##        Column Total |     45871 |      8784 |     54655 | 
    ##                     |     0.839 |     0.161 |           | 
    ## --------------------|-----------|-----------|-----------|
    ## 
    ## 

``` r
CrossTable(NFCS_data$Income,NFCS_data$Crypto_State_Year, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=TRUE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  54655 
    ## 
    ##  
    ##                  | NFCS_data$Crypto_State_Year 
    ## NFCS_data$Income |         0 |         1 | Row Total | 
    ## -----------------|-----------|-----------|-----------|
    ##                1 |      5207 |       996 |      6203 | 
    ##                  |     0.839 |     0.161 |     0.113 | 
    ##                  |     0.114 |     0.113 |           | 
    ## -----------------|-----------|-----------|-----------|
    ##                2 |      4885 |       906 |      5791 | 
    ##                  |     0.844 |     0.156 |     0.106 | 
    ##                  |     0.106 |     0.103 |           | 
    ## -----------------|-----------|-----------|-----------|
    ##                3 |      4980 |       943 |      5923 | 
    ##                  |     0.841 |     0.159 |     0.108 | 
    ##                  |     0.109 |     0.107 |           | 
    ## -----------------|-----------|-----------|-----------|
    ##                4 |      6697 |      1270 |      7967 | 
    ##                  |     0.841 |     0.159 |     0.146 | 
    ##                  |     0.146 |     0.145 |           | 
    ## -----------------|-----------|-----------|-----------|
    ##                5 |      9175 |      1734 |     10909 | 
    ##                  |     0.841 |     0.159 |     0.200 | 
    ##                  |     0.200 |     0.197 |           | 
    ## -----------------|-----------|-----------|-----------|
    ##                6 |      6374 |      1227 |      7601 | 
    ##                  |     0.839 |     0.161 |     0.139 | 
    ##                  |     0.139 |     0.140 |           | 
    ## -----------------|-----------|-----------|-----------|
    ##                7 |      5645 |      1162 |      6807 | 
    ##                  |     0.829 |     0.171 |     0.125 | 
    ##                  |     0.123 |     0.132 |           | 
    ## -----------------|-----------|-----------|-----------|
    ##                8 |      2908 |       546 |      3454 | 
    ##                  |     0.842 |     0.158 |     0.063 | 
    ##                  |     0.063 |     0.062 |           | 
    ## -----------------|-----------|-----------|-----------|
    ##     Column Total |     45871 |      8784 |     54655 | 
    ##                  |     0.839 |     0.161 |           | 
    ## -----------------|-----------|-----------|-----------|
    ## 
    ## 

``` r
CrossTable(NFCS_data$Marital_Status,NFCS_data$Crypto_State_Year, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=TRUE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  54655 
    ## 
    ##  
    ##                          | NFCS_data$Crypto_State_Year 
    ## NFCS_data$Marital_Status |         0 |         1 | Row Total | 
    ## -------------------------|-----------|-----------|-----------|
    ##                        0 |     21155 |      4036 |     25191 | 
    ##                          |     0.840 |     0.160 |     0.461 | 
    ##                          |     0.461 |     0.459 |           | 
    ## -------------------------|-----------|-----------|-----------|
    ##                        1 |     24716 |      4748 |     29464 | 
    ##                          |     0.839 |     0.161 |     0.539 | 
    ##                          |     0.539 |     0.541 |           | 
    ## -------------------------|-----------|-----------|-----------|
    ##             Column Total |     45871 |      8784 |     54655 | 
    ##                          |     0.839 |     0.161 |           | 
    ## -------------------------|-----------|-----------|-----------|
    ## 
    ## 

``` r
CrossTable(NFCS_data$Children,NFCS_data$Crypto_State_Year, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=TRUE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  54655 
    ## 
    ##  
    ##                    | NFCS_data$Crypto_State_Year 
    ## NFCS_data$Children |         0 |         1 | Row Total | 
    ## -------------------|-----------|-----------|-----------|
    ##                  0 |     29274 |      5701 |     34975 | 
    ##                    |     0.837 |     0.163 |     0.640 | 
    ##                    |     0.638 |     0.649 |           | 
    ## -------------------|-----------|-----------|-----------|
    ##                  1 |      7150 |      1368 |      8518 | 
    ##                    |     0.839 |     0.161 |     0.156 | 
    ##                    |     0.156 |     0.156 |           | 
    ## -------------------|-----------|-----------|-----------|
    ##                  2 |      5833 |      1030 |      6863 | 
    ##                    |     0.850 |     0.150 |     0.126 | 
    ##                    |     0.127 |     0.117 |           | 
    ## -------------------|-----------|-----------|-----------|
    ##                  3 |      2320 |       438 |      2758 | 
    ##                    |     0.841 |     0.159 |     0.050 | 
    ##                    |     0.051 |     0.050 |           | 
    ## -------------------|-----------|-----------|-----------|
    ##                  4 |      1294 |       247 |      1541 | 
    ##                    |     0.840 |     0.160 |     0.028 | 
    ##                    |     0.028 |     0.028 |           | 
    ## -------------------|-----------|-----------|-----------|
    ##       Column Total |     45871 |      8784 |     54655 | 
    ##                    |     0.839 |     0.161 |           | 
    ## -------------------|-----------|-----------|-----------|
    ## 
    ## 

``` r
CrossTable(NFCS_data$Crypto_State_Year,NFCS_data$Crypto_State_Year, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=TRUE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  54655 
    ## 
    ##  
    ##                             | NFCS_data$Crypto_State_Year 
    ## NFCS_data$Crypto_State_Year |         0 |         1 | Row Total | 
    ## ----------------------------|-----------|-----------|-----------|
    ##                           0 |     45871 |         0 |     45871 | 
    ##                             |     1.000 |     0.000 |     0.839 | 
    ##                             |     1.000 |     0.000 |           | 
    ## ----------------------------|-----------|-----------|-----------|
    ##                           1 |         0 |      8784 |      8784 | 
    ##                             |     0.000 |     1.000 |     0.161 | 
    ##                             |     0.000 |     1.000 |           | 
    ## ----------------------------|-----------|-----------|-----------|
    ##                Column Total |     45871 |      8784 |     54655 | 
    ##                             |     0.839 |     0.161 |           | 
    ## ----------------------------|-----------|-----------|-----------|
    ## 
    ## 

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
    ## -4.7267 -0.8431  0.1257  0.9330  3.9144 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      0.286782   0.021646  13.249  < 2e-16 ***
    ## Gender1          0.414703   0.011150  37.193  < 2e-16 ***
    ## Age              0.202129   0.003638  55.561  < 2e-16 ***
    ## Ethnicity1       0.319466   0.012768  25.022  < 2e-16 ***
    ## Income           0.132455   0.003273  40.470  < 2e-16 ***
    ## Education        0.232126   0.004154  55.882  < 2e-16 ***
    ## Marital_Status1  0.040561   0.012955   3.131  0.00174 ** 
    ## Children        -0.060748   0.005649 -10.753  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.279 on 54647 degrees of freedom
    ## Multiple R-squared:  0.2562, Adjusted R-squared:  0.2561 
    ## F-statistic:  2689 on 7 and 54647 DF,  p-value: < 2.2e-16

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
    ## -4.7397 -0.8404  0.1263  0.9326  3.9805 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         0.298045   0.021750  13.703  < 2e-16 ***
    ## Gender1             0.415221   0.011148  37.247  < 2e-16 ***
    ## Age                 0.202135   0.003637  55.576  < 2e-16 ***
    ## Ethnicity1          0.321678   0.012772  25.187  < 2e-16 ***
    ## Income              0.132584   0.003272  40.518  < 2e-16 ***
    ## Education           0.231830   0.004153  55.819  < 2e-16 ***
    ## Marital_Status1     0.040243   0.012952   3.107  0.00189 ** 
    ## Children           -0.060865   0.005648 -10.777  < 2e-16 ***
    ## Crypto_State_Year1 -0.076969   0.014902  -5.165 2.41e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.279 on 54646 degrees of freedom
    ## Multiple R-squared:  0.2566, Adjusted R-squared:  0.2565 
    ## F-statistic:  2357 on 8 and 54646 DF,  p-value: < 2.2e-16

``` r
nobs(Model_2) #Number of Observations
```

    ## [1] 54655

## Regression Results Table

``` r
library(stargazer)
```

    ## 
    ## Please cite as:

    ##  Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables.

    ##  R package version 5.2.2. https://CRAN.R-project.org/package=stargazer

``` r
stargazer(Model_1, Model_2, type = "text")
```

    ## 
    ## =============================================================================
    ##                                        Dependent variable:                   
    ##                     ---------------------------------------------------------
    ##                                               Score                          
    ##                                 (1)                          (2)             
    ## -----------------------------------------------------------------------------
    ## Gender1                       0.415***                     0.415***          
    ##                               (0.011)                      (0.011)           
    ##                                                                              
    ## Age                           0.202***                     0.202***          
    ##                               (0.004)                      (0.004)           
    ##                                                                              
    ## Ethnicity1                    0.319***                     0.322***          
    ##                               (0.013)                      (0.013)           
    ##                                                                              
    ## Income                        0.132***                     0.133***          
    ##                               (0.003)                      (0.003)           
    ##                                                                              
    ## Education                     0.232***                     0.232***          
    ##                               (0.004)                      (0.004)           
    ##                                                                              
    ## Marital_Status1               0.041***                     0.040***          
    ##                               (0.013)                      (0.013)           
    ##                                                                              
    ## Children                     -0.061***                    -0.061***          
    ##                               (0.006)                      (0.006)           
    ##                                                                              
    ## Crypto_State_Year1                                        -0.077***          
    ##                                                            (0.015)           
    ##                                                                              
    ## Constant                      0.287***                     0.298***          
    ##                               (0.022)                      (0.022)           
    ##                                                                              
    ## -----------------------------------------------------------------------------
    ## Observations                   54,655                       54,655           
    ## R2                             0.256                        0.257            
    ## Adjusted R2                    0.256                        0.256            
    ## Residual Std. Error      1.279 (df = 54647)           1.279 (df = 54646)     
    ## F Statistic         2,689.140*** (df = 7; 54647) 2,357.437*** (df = 8; 54646)
    ## =============================================================================
    ## Note:                                             *p<0.1; **p<0.05; ***p<0.01
