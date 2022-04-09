Financial Literacy and Cryptocurrency
================
Kieran Yuen

# Regression Models

## Model with Demographic information only

``` r
Demographic_Model <- lm(Score ~ Age 
                        + Income 
                        + Education 
                        + Marital_Status 
                        + Children
                        , data = NFCS_data)

summary(Demographic_Model)
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
    ## (Intercept)                    1.972487   0.034074  57.888  < 2e-16 ***
    ## Age25-34                      -0.102784   0.022544  -4.559 5.14e-06 ***
    ## Age35-44                       0.202439   0.023563   8.591  < 2e-16 ***
    ## Age45-54                       0.513726   0.023399  21.955  < 2e-16 ***
    ## Age55-64                       0.730388   0.023925  30.528  < 2e-16 ***
    ## Age65+                         0.918158   0.024650  37.248  < 2e-16 ***
    ## Income$15,000-$24,999          1.029874   0.030611  33.644  < 2e-16 ***
    ## Income$25,000-$34,999          0.958711   0.025589  37.466  < 2e-16 ***
    ## Income$35,000-$49,999          0.190377   0.023910   7.962 1.72e-15 ***
    ## Income$50,000-$74,999          0.286714   0.024005  11.944  < 2e-16 ***
    ## Income$75,000-$99,999          0.491674   0.022869  21.499  < 2e-16 ***
    ## Income$100,000-$149,999        0.637115   0.022255  28.628  < 2e-16 ***
    ## Income>$150,000                0.709090   0.024428  29.028  < 2e-16 ***
    ## EducationHigh School           0.386476   0.020749  18.626  < 2e-16 ***
    ## EducationSome College         -0.451841   0.020522 -22.018  < 2e-16 ***
    ## EducationAssociate's          -0.847691   0.040396 -20.984  < 2e-16 ***
    ## EducationBachelor's            0.485063   0.023463  20.673  < 2e-16 ***
    ## EducationPost Graduate        -0.019775   0.019971  -0.990  0.32209    
    ## Marital_StatusMarried          0.020123   0.019557   1.029  0.30349    
    ## Marital_StatusDivorced        -0.094084   0.050162  -1.876  0.06071 .  
    ## Marital_StatusSeparated       -0.062776   0.021571  -2.910  0.00361 ** 
    ## Marital_StatusWidowed/widower -0.211858   0.032059  -6.608 3.92e-11 ***
    ## Children                      -0.062664   0.006112 -10.253  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.295 on 54632 degrees of freedom
    ## Multiple R-squared:  0.2376, Adjusted R-squared:  0.2372 
    ## F-statistic: 773.7 on 22 and 54632 DF,  p-value: < 2.2e-16

``` r
#plot(Demographic_Model)
```

## Model with Demographic information & Crypto State-Year Variable

``` r
Demo_Crypto_Model <- lm(Score ~ Age 
                        + Income 
                        + Education 
                        + Marital_Status 
                        + Children 
                        + Crypto_State_Year
                        , data = NFCS_data)
summary(Demo_Crypto_Model)
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
    ## (Intercept)                    1.980730   0.034142  58.014  < 2e-16 ***
    ## Age25-34                      -0.102974   0.022541  -4.568 4.93e-06 ***
    ## Age35-44                       0.202782   0.023561   8.607  < 2e-16 ***
    ## Age45-54                       0.514036   0.023397  21.970  < 2e-16 ***
    ## Age55-64                       0.730640   0.023922  30.542  < 2e-16 ***
    ## Age65+                         0.918472   0.024647  37.265  < 2e-16 ***
    ## Income$15,000-$24,999          1.030189   0.030608  33.658  < 2e-16 ***
    ## Income$25,000-$34,999          0.959725   0.025587  37.508  < 2e-16 ***
    ## Income$35,000-$49,999          0.190234   0.023907   7.957 1.79e-15 ***
    ## Income$50,000-$74,999          0.286828   0.024002  11.950  < 2e-16 ***
    ## Income$75,000-$99,999          0.491886   0.022867  21.511  < 2e-16 ***
    ## Income$100,000-$149,999        0.637370   0.022253  28.643  < 2e-16 ***
    ## Income>$150,000                0.709565   0.024425  29.051  < 2e-16 ***
    ## EducationHigh School           0.386726   0.020747  18.640  < 2e-16 ***
    ## EducationSome College         -0.450800   0.020521 -21.967  < 2e-16 ***
    ## EducationAssociate's          -0.845177   0.040397 -20.922  < 2e-16 ***
    ## EducationBachelor's            0.485103   0.023461  20.677  < 2e-16 ***
    ## EducationPost Graduate        -0.019820   0.019968  -0.993 0.320937    
    ## Marital_StatusMarried          0.020126   0.019554   1.029 0.303374    
    ## Marital_StatusDivorced        -0.093344   0.050156  -1.861 0.062740 .  
    ## Marital_StatusSeparated       -0.062690   0.021569  -2.907 0.003656 ** 
    ## Marital_StatusWidowed/widower -0.211605   0.032055  -6.601 4.11e-11 ***
    ## Children                      -0.062825   0.006111 -10.280  < 2e-16 ***
    ## Crypto_State_Year1            -0.056081   0.015087  -3.717 0.000202 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.295 on 54631 degrees of freedom
    ## Multiple R-squared:  0.2377, Adjusted R-squared:  0.2374 
    ## F-statistic: 740.8 on 23 and 54631 DF,  p-value: < 2.2e-16

``` r
#plot(Demo_Crypto_Model)
```

## Model with Demographic information & Crypto State-Year Variable & interaction of Age and Crypto State-Year variable

``` r
Demo_Crypto_Interaction_Model <- lm(Score ~ Age
                                    + Income 
                                    + Education 
                                    + Marital_Status 
                                    + Children 
                                    + Crypto_State_Year 
                                    + Age*Crypto_State_Year
                                    , data = NFCS_data)
summary(Demo_Crypto_Interaction_Model)
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
    ## (Intercept)                    1.979896   0.034813  56.872  < 2e-16 ***
    ## Age25-34                      -0.093081   0.024373  -3.819 0.000134 ***
    ## Age35-44                       0.204490   0.025420   8.044 8.84e-16 ***
    ## Age45-54                       0.520949   0.025212  20.663  < 2e-16 ***
    ## Age55-64                       0.720882   0.025682  28.069  < 2e-16 ***
    ## Age65+                         0.917424   0.026271  34.921  < 2e-16 ***
    ## Income$15,000-$24,999          1.029814   0.030609  33.644  < 2e-16 ***
    ## Income$25,000-$34,999          0.959241   0.025588  37.487  < 2e-16 ***
    ## Income$35,000-$49,999          0.190132   0.023907   7.953 1.86e-15 ***
    ## Income$50,000-$74,999          0.286792   0.024003  11.948  < 2e-16 ***
    ## Income$75,000-$99,999          0.491797   0.022867  21.507  < 2e-16 ***
    ## Income$100,000-$149,999        0.637077   0.022253  28.629  < 2e-16 ***
    ## Income>$150,000                0.709463   0.024426  29.046  < 2e-16 ***
    ## EducationHigh School           0.386210   0.020748  18.614  < 2e-16 ***
    ## EducationSome College         -0.451225   0.020522 -21.987  < 2e-16 ***
    ## EducationAssociate's          -0.845711   0.040403 -20.932  < 2e-16 ***
    ## EducationBachelor's            0.484332   0.023462  20.643  < 2e-16 ***
    ## EducationPost Graduate        -0.020158   0.019969  -1.009 0.312761    
    ## Marital_StatusMarried          0.020165   0.019557   1.031 0.302501    
    ## Marital_StatusDivorced        -0.093619   0.050158  -1.866 0.061980 .  
    ## Marital_StatusSeparated       -0.062538   0.021569  -2.899 0.003740 ** 
    ## Marital_StatusWidowed/widower -0.210929   0.032059  -6.579 4.77e-11 ***
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
#plot(Demo_Crypto_Interaction_Model)
```
