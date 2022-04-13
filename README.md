Financial Literacy and Cryptocurrency
================
Kieran Yuen

# Regression Models

## Model \#1: Demographic information only

``` r
Model_1 <- lm(Score ~ Age 
              + Income 
              + Education 
              + Marital_Status 
              + Children
              , data = NFCS_data)
summary(Model_1)
```

    ## 
    ## Call:
    ## lm(formula = Score ~ Age + Income + Education + Marital_Status + 
    ##     Children, data = NFCS_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4257 -0.8864  0.1101  0.9671  3.9380 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    1.062020   0.039899  26.618  < 2e-16 ***
    ## Age25-34                      -0.102784   0.022544  -4.559 5.14e-06 ***
    ## Age35-44                       0.202439   0.023563   8.591  < 2e-16 ***
    ## Age45-54                       0.513726   0.023399  21.955  < 2e-16 ***
    ## Age55-64                       0.730388   0.023925  30.528  < 2e-16 ***
    ## Age65+                         0.918158   0.024650  37.248  < 2e-16 ***
    ## Income$15,000-$24,999          0.190377   0.023910   7.962 1.72e-15 ***
    ## Income$25,000-$34,999          0.286714   0.024005  11.944  < 2e-16 ***
    ## Income$35,000-$49,999          0.491674   0.022869  21.499  < 2e-16 ***
    ## Income$50,000-$74,999          0.637115   0.022255  28.628  < 2e-16 ***
    ## Income$75,000-$99,999          0.709090   0.024428  29.028  < 2e-16 ***
    ## Income$100,000-$149,999        0.958711   0.025589  37.466  < 2e-16 ***
    ## Income>$150,000                1.029874   0.030611  33.644  < 2e-16 ***
    ## EducationHigh School           0.395850   0.038120  10.384  < 2e-16 ***
    ## EducationSome College          0.827916   0.038050  21.758  < 2e-16 ***
    ## EducationAssociate's           0.847691   0.040396  20.984  < 2e-16 ***
    ## EducationBachelor's            1.234166   0.038923  31.708  < 2e-16 ***
    ## EducationPost Graduate         1.332754   0.040649  32.787  < 2e-16 ***
    ## Marital_StatusMarried          0.082900   0.015731   5.270 1.37e-07 ***
    ## Marital_StatusDivorced         0.062776   0.021571   2.910  0.00361 ** 
    ## Marital_StatusSeparated       -0.031307   0.048870  -0.641  0.52177    
    ## Marital_StatusWidowed/widower -0.149081   0.031034  -4.804 1.56e-06 ***
    ## Children                      -0.062664   0.006112 -10.253  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.295 on 54632 degrees of freedom
    ## Multiple R-squared:  0.2376, Adjusted R-squared:  0.2372 
    ## F-statistic: 773.7 on 22 and 54632 DF,  p-value: < 2.2e-16

``` r
plot(Model_1)
```

![](Regression_files/figure-gfm/Model%20#1:%20Demographic%20information%20only-1.png)<!-- -->![](Regression_files/figure-gfm/Model%20#1:%20Demographic%20information%20only-2.png)<!-- -->![](Regression_files/figure-gfm/Model%20#1:%20Demographic%20information%20only-3.png)<!-- -->![](Regression_files/figure-gfm/Model%20#1:%20Demographic%20information%20only-4.png)<!-- -->

### Model \#1: Multicollinearity Check

``` r
car::vif(Model_1)
```

    ##                    GVIF Df GVIF^(1/(2*Df))
    ## Age            1.897111  5        1.066128
    ## Income         1.611006  7        1.034648
    ## Education      1.275290  5        1.024615
    ## Marital_Status 1.889156  4        1.082763
    ## Children       1.355032  1        1.164058

``` r
#variance inflation factor: measures how much the variance of a regression coefficient is inflated due to multicollinearity in the model
#The smallest possible value of VIF is one (absence of multicollinearity). As a rule of thumb, a VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity (James et al. 2014).
```

## Model \#2: Demographic information & Crypto State-Year Variable

``` r
Model_2 <- lm(Score ~ Age 
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
    ## lm(formula = Score ~ Age + Income + Education + Marital_Status + 
    ##     Children + Crypto_State_Year, data = NFCS_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4346 -0.8794  0.1037  0.9677  3.9271 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    1.072863   0.040001  26.821  < 2e-16 ***
    ## Age25-34                      -0.102974   0.022541  -4.568 4.93e-06 ***
    ## Age35-44                       0.202782   0.023561   8.607  < 2e-16 ***
    ## Age45-54                       0.514036   0.023397  21.970  < 2e-16 ***
    ## Age55-64                       0.730640   0.023922  30.542  < 2e-16 ***
    ## Age65+                         0.918472   0.024647  37.265  < 2e-16 ***
    ## Income$15,000-$24,999          0.190234   0.023907   7.957 1.79e-15 ***
    ## Income$25,000-$34,999          0.286828   0.024002  11.950  < 2e-16 ***
    ## Income$35,000-$49,999          0.491886   0.022867  21.511  < 2e-16 ***
    ## Income$50,000-$74,999          0.637370   0.022253  28.643  < 2e-16 ***
    ## Income$75,000-$99,999          0.709565   0.024425  29.051  < 2e-16 ***
    ## Income$100,000-$149,999        0.959725   0.025587  37.508  < 2e-16 ***
    ## Income>$150,000                1.030189   0.030608  33.658  < 2e-16 ***
    ## EducationHigh School           0.394377   0.038117  10.346  < 2e-16 ***
    ## EducationSome College          0.825358   0.038052  21.690  < 2e-16 ***
    ## EducationAssociate's           0.845177   0.040397  20.922  < 2e-16 ***
    ## EducationBachelor's            1.231903   0.038923  31.649  < 2e-16 ***
    ## EducationPost Graduate         1.330280   0.040649  32.726  < 2e-16 ***
    ## Marital_StatusMarried          0.082816   0.015729   5.265 1.41e-07 ***
    ## Marital_StatusDivorced         0.062690   0.021569   2.907 0.003656 ** 
    ## Marital_StatusSeparated       -0.030654   0.048865  -0.627 0.530452    
    ## Marital_StatusWidowed/widower -0.148915   0.031030  -4.799 1.60e-06 ***
    ## Children                      -0.062825   0.006111 -10.280  < 2e-16 ***
    ## Crypto_State_Year1            -0.056081   0.015087  -3.717 0.000202 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.295 on 54631 degrees of freedom
    ## Multiple R-squared:  0.2377, Adjusted R-squared:  0.2374 
    ## F-statistic: 740.8 on 23 and 54631 DF,  p-value: < 2.2e-16

``` r
plot(Model_2)
```

![](Regression_files/figure-gfm/Model%20#2:%20Demographic%20information%20&%20Crypto%20State-Year%20Variable-1.png)<!-- -->![](Regression_files/figure-gfm/Model%20#2:%20Demographic%20information%20&%20Crypto%20State-Year%20Variable-2.png)<!-- -->![](Regression_files/figure-gfm/Model%20#2:%20Demographic%20information%20&%20Crypto%20State-Year%20Variable-3.png)<!-- -->![](Regression_files/figure-gfm/Model%20#2:%20Demographic%20information%20&%20Crypto%20State-Year%20Variable-4.png)<!-- -->

### Model \#2: Multicollinearity Check

``` r
car::vif(Model_2)
```

    ##                       GVIF Df GVIF^(1/(2*Df))
    ## Age               1.897261  5        1.066136
    ## Income            1.611313  7        1.034662
    ## Education         1.276116  5        1.024682
    ## Marital_Status    1.889199  4        1.082766
    ## Children          1.355099  1        1.164087
    ## Crypto_State_Year 1.000965  1        1.000482

``` r
#variance inflation factor: measures how much the variance of a regression coefficient is inflated due to multicollinearity in the model
#The smallest possible value of VIF is one (absence of multicollinearity). As a rule of thumb, a VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity (James et al. 2014).
```

## Model \#3: Demographic information & Crypto State-Year Variable & interaction of Age and Crypto State-Year variable

``` r
Model_3 <- lm(Score ~ Age
              + Income 
              + Education 
              + Marital_Status 
              + Children 
              + Crypto_State_Year 
              + Age*Crypto_State_Year
              , data = NFCS_data)
summary(Model_3)
```

    ## 
    ## Call:
    ## lm(formula = Score ~ Age + Income + Education + Marital_Status + 
    ##     Children + Crypto_State_Year + Age * Crypto_State_Year, data = NFCS_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4316 -0.8791  0.1038  0.9624  3.9284 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    1.071648   0.040601  26.395  < 2e-16 ***
    ## Age25-34                      -0.093081   0.024373  -3.819 0.000134 ***
    ## Age35-44                       0.204490   0.025420   8.044 8.84e-16 ***
    ## Age45-54                       0.520949   0.025212  20.663  < 2e-16 ***
    ## Age55-64                       0.720882   0.025682  28.069  < 2e-16 ***
    ## Age65+                         0.917424   0.026271  34.921  < 2e-16 ***
    ## Income$15,000-$24,999          0.190132   0.023907   7.953 1.86e-15 ***
    ## Income$25,000-$34,999          0.286792   0.024003  11.948  < 2e-16 ***
    ## Income$35,000-$49,999          0.491797   0.022867  21.507  < 2e-16 ***
    ## Income$50,000-$74,999          0.637077   0.022253  28.629  < 2e-16 ***
    ## Income$75,000-$99,999          0.709463   0.024426  29.046  < 2e-16 ***
    ## Income$100,000-$149,999        0.959241   0.025588  37.487  < 2e-16 ***
    ## Income>$150,000                1.029814   0.030609  33.644  < 2e-16 ***
    ## EducationHigh School           0.394486   0.038124  10.348  < 2e-16 ***
    ## EducationSome College          0.825553   0.038058  21.692  < 2e-16 ***
    ## EducationAssociate's           0.845711   0.040403  20.932  < 2e-16 ***
    ## EducationBachelor's            1.231921   0.038931  31.644  < 2e-16 ***
    ## EducationPost Graduate         1.330043   0.040657  32.713  < 2e-16 ***
    ## Marital_StatusMarried          0.082702   0.015731   5.257 1.47e-07 ***
    ## Marital_StatusDivorced         0.062538   0.021569   2.899 0.003740 ** 
    ## Marital_StatusSeparated       -0.031082   0.048866  -0.636 0.524743    
    ## Marital_StatusWidowed/widower -0.148392   0.031032  -4.782 1.74e-06 ***
    ## Children                      -0.062824   0.006112 -10.279  < 2e-16 ***
    ## Crypto_State_Year1            -0.048533   0.046394  -1.046 0.295521    
    ## Age25-34:Crypto_State_Year1   -0.063734   0.059102  -1.078 0.280872    
    ## Age35-44:Crypto_State_Year1   -0.009448   0.059248  -0.159 0.873297    
    ## Age45-54:Crypto_State_Year1   -0.041368   0.058474  -0.707 0.479283    
    ## Age55-64:Crypto_State_Year1    0.060661   0.058463   1.038 0.299461    
    ## Age65+:Crypto_State_Year1      0.007008   0.057490   0.122 0.902972    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.295 on 54626 degrees of freedom
    ## Multiple R-squared:  0.2378, Adjusted R-squared:  0.2375 
    ## F-statistic: 608.8 on 28 and 54626 DF,  p-value: < 2.2e-16

``` r
#plot(Model_3)
```

### Model \#3: Multicollinearity Check

``` r
car::vif(Model_3)
```

    ##                            GVIF Df GVIF^(1/(2*Df))
    ## Age                    4.160504  5        1.153226
    ## Income                 1.612946  7        1.034737
    ## Education              1.277093  5        1.024760
    ## Marital_Status         1.890551  4        1.082863
    ## Children               1.355384  1        1.164209
    ## Crypto_State_Year      9.465291  1        3.076571
    ## Age:Crypto_State_Year 19.530603  5        1.346082

``` r
#variance inflation factor: measures how much the variance of a regression coefficient is inflated due to multicollinearity in the model
#The smallest possible value of VIF is one (absence of multicollinearity). As a rule of thumb, a VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity (James et al. 2014).
```
