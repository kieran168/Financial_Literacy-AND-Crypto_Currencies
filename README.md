Financial Literacy and Cryptocurrency
================
Kieran Yuen

# Regression Models

## Model with Demographic information only

``` r
Demographic_Model <- lm(Score ~ Age 
                        + Level_of_Income 
                        + Level_of_Education 
                        + Marital_Status 
                        + Number_of_Children
                        , data = NFCS_data)
summary(Demographic_Model)
```

    ## 
    ## Call:
    ## lm(formula = Score ~ Age + Level_of_Income + Level_of_Education + 
    ##     Marital_Status + Number_of_Children, data = NFCS_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4246 -0.8788  0.1102  0.9676  3.9389 
    ## 
    ## Coefficients:
    ##                                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                            1.972426   0.034071  57.892  < 2e-16 ***
    ## Age25-34                              -0.100433   0.022551  -4.454 8.46e-06 ***
    ## Age35-44                               0.204744   0.023570   8.687  < 2e-16 ***
    ## Age45-54                               0.515837   0.023404  22.040  < 2e-16 ***
    ## Age55-64                               0.731979   0.023927  30.592  < 2e-16 ***
    ## Age65+                                 0.919870   0.024652  37.314  < 2e-16 ***
    ## Level_of_Income$15,000-$24,999         1.026852   0.030620  33.535  < 2e-16 ***
    ## Level_of_Income$25,000-$34,999         0.955790   0.025599  37.337  < 2e-16 ***
    ## Level_of_Income$35,000-$49,999         0.189432   0.023909   7.923 2.36e-15 ***
    ## Level_of_Income$50,000-$74,999         0.284681   0.024010  11.857  < 2e-16 ***
    ## Level_of_Income$75,000-$99,999         0.489811   0.022873  21.414  < 2e-16 ***
    ## Level_of_Income$100,000-$149,999       0.634678   0.022263  28.508  < 2e-16 ***
    ## Level_of_Income>$150,000               0.706514   0.024436  28.913  < 2e-16 ***
    ## Level_of_EducationGED                  0.386794   0.020747  18.643  < 2e-16 ***
    ## Level_of_EducationHigh school diploma -0.848433   0.040392 -21.005  < 2e-16 ***
    ## Level_of_EducationSome college        -0.516812   0.027526 -18.775  < 2e-16 ***
    ## Level_of_EducationAssociate's         -0.426867   0.021698 -19.673  < 2e-16 ***
    ## Level_of_EducationBachelor's           0.485579   0.023461  20.697  < 2e-16 ***
    ## Level_of_EducationPost graduate       -0.019741   0.019969  -0.989  0.32286    
    ## Marital_StatusMarried                  0.019881   0.019555   1.017  0.30930    
    ## Marital_StatusDivorced                -0.092322   0.050159  -1.841  0.06569 .  
    ## Marital_StatusSeparated               -0.062875   0.021569  -2.915  0.00356 ** 
    ## Marital_StatusWidowed/widower         -0.211862   0.032056  -6.609 3.90e-11 ***
    ## Number_of_Children                    -0.062225   0.006113 -10.180  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.295 on 54631 degrees of freedom
    ## Multiple R-squared:  0.2377, Adjusted R-squared:  0.2374 
    ## F-statistic: 740.8 on 23 and 54631 DF,  p-value: < 2.2e-16

``` r
#plot(Demographic_Model)
```

## Model with Demographic information & Crypto State-Year Variable

``` r
Demo_Crypto_Model <- lm(Score ~ Age 
                        + Level_of_Income 
                        + Level_of_Education 
                        + Marital_Status 
                        + Number_of_Children 
                        + Crypto_Year
                        , data = NFCS_data)
summary(Demo_Crypto_Model)
```

    ## 
    ## Call:
    ## lm(formula = Score ~ Age + Level_of_Income + Level_of_Education + 
    ##     Marital_Status + Number_of_Children + Crypto_Year, data = NFCS_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4311 -0.8788  0.1059  0.9664  3.9316 
    ## 
    ## Coefficients:
    ##                                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                            1.978398   0.034136  57.956  < 2e-16 ***
    ## Age25-34                              -0.100546   0.022550  -4.459 8.26e-06 ***
    ## Age35-44                               0.205013   0.023568   8.699  < 2e-16 ***
    ## Age45-54                               0.516097   0.023403  22.052  < 2e-16 ***
    ## Age55-64                               0.732256   0.023925  30.606  < 2e-16 ***
    ## Age65+                                 0.920203   0.024651  37.330  < 2e-16 ***
    ## Level_of_Income$15,000-$24,999         1.027258   0.030618  33.550  < 2e-16 ***
    ## Level_of_Income$25,000-$34,999         0.956721   0.025600  37.372  < 2e-16 ***
    ## Level_of_Income$35,000-$49,999         0.189367   0.023907   7.921 2.40e-15 ***
    ## Level_of_Income$50,000-$74,999         0.284844   0.024008  11.865  < 2e-16 ***
    ## Level_of_Income$75,000-$99,999         0.490056   0.022872  21.426  < 2e-16 ***
    ## Level_of_Income$100,000-$149,999       0.635004   0.022262  28.524  < 2e-16 ***
    ## Level_of_Income>$150,000               0.707013   0.024435  28.934  < 2e-16 ***
    ## Level_of_EducationGED                  0.386899   0.020746  18.650  < 2e-16 ***
    ## Level_of_EducationHigh school diploma -0.847114   0.040393 -20.972  < 2e-16 ***
    ## Level_of_EducationSome college        -0.516035   0.027526 -18.747  < 2e-16 ***
    ## Level_of_EducationAssociate's         -0.426343   0.021697 -19.650  < 2e-16 ***
    ## Level_of_EducationBachelor's           0.485488   0.023460  20.694  < 2e-16 ***
    ## Level_of_EducationPost graduate       -0.019846   0.019968  -0.994  0.32026    
    ## Marital_StatusMarried                  0.019793   0.019554   1.012  0.31142    
    ## Marital_StatusDivorced                -0.091601   0.050156  -1.826  0.06781 .  
    ## Marital_StatusSeparated               -0.062840   0.021568  -2.914  0.00357 ** 
    ## Marital_StatusWidowed/widower         -0.211722   0.032054  -6.605 4.01e-11 ***
    ## Number_of_Children                    -0.062357   0.006112 -10.202  < 2e-16 ***
    ## Crypto_Year1                          -0.043047   0.015451  -2.786  0.00534 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.295 on 54630 degrees of freedom
    ## Multiple R-squared:  0.2378, Adjusted R-squared:  0.2375 
    ## F-statistic: 710.3 on 24 and 54630 DF,  p-value: < 2.2e-16

``` r
#plot(Demo_Crypto_Model)
```

## Model with Demographic information & Crypto State-Year Variable & interaction of Age and Crypto State-Year variable

``` r
Demo_Crypto_Interaction_Model <- lm(Score ~ Age 
                                    + Level_of_Income 
                                    + Level_of_Education 
                                    + Marital_Status 
                                    + Number_of_Children 
                                    + Crypto_Year 
                                    + Age*Crypto_Year
                                    , data = NFCS_data)
summary(Demo_Crypto_Interaction_Model)
```

    ## 
    ## Call:
    ## lm(formula = Score ~ Age + Level_of_Income + Level_of_Education + 
    ##     Marital_Status + Number_of_Children + Crypto_Year + Age * 
    ##     Crypto_Year, data = NFCS_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4302 -0.8782  0.1064  0.9656  3.9316 
    ## 
    ## Coefficients:
    ##                                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                            1.978832   0.034748  56.948  < 2e-16 ***
    ## Age25-34                              -0.092903   0.024239  -3.833 0.000127 ***
    ## Age35-44                               0.205481   0.025281   8.128 4.46e-16 ***
    ## Age45-54                               0.520143   0.025080  20.739  < 2e-16 ***
    ## Age55-64                               0.720117   0.025555  28.179  < 2e-16 ***
    ## Age65+                                 0.919592   0.026156  35.158  < 2e-16 ***
    ## Level_of_Income$15,000-$24,999         1.027024   0.030620  33.541  < 2e-16 ***
    ## Level_of_Income$25,000-$34,999         0.956213   0.025601  37.350  < 2e-16 ***
    ## Level_of_Income$35,000-$49,999         0.189250   0.023908   7.916 2.50e-15 ***
    ## Level_of_Income$50,000-$74,999         0.284890   0.024008  11.866  < 2e-16 ***
    ## Level_of_Income$75,000-$99,999         0.490017   0.022872  21.425  < 2e-16 ***
    ## Level_of_Income$100,000-$149,999       0.634852   0.022262  28.517  < 2e-16 ***
    ## Level_of_Income>$150,000               0.706928   0.024435  28.931  < 2e-16 ***
    ## Level_of_EducationGED                  0.386410   0.020747  18.625  < 2e-16 ***
    ## Level_of_EducationHigh school diploma -0.847861   0.040397 -20.988  < 2e-16 ***
    ## Level_of_EducationSome college        -0.516694   0.027528 -18.770  < 2e-16 ***
    ## Level_of_EducationAssociate's         -0.426630   0.021698 -19.662  < 2e-16 ***
    ## Level_of_EducationBachelor's           0.484807   0.023461  20.664  < 2e-16 ***
    ## Level_of_EducationPost graduate       -0.020205   0.019968  -1.012 0.311602    
    ## Marital_StatusMarried                  0.019977   0.019555   1.022 0.306991    
    ## Marital_StatusDivorced                -0.091519   0.050157  -1.825 0.068062 .  
    ## Marital_StatusSeparated               -0.062551   0.021568  -2.900 0.003730 ** 
    ## Marital_StatusWidowed/widower         -0.210883   0.032056  -6.579 4.79e-11 ***
    ## Number_of_Children                    -0.062377   0.006113 -10.204  < 2e-16 ***
    ## Crypto_Year1                          -0.044702   0.047794  -0.935 0.349636    
    ## Age25-34:Crypto_Year1                 -0.052638   0.060808  -0.866 0.386685    
    ## Age35-44:Crypto_Year1                 -0.001685   0.060942  -0.028 0.977948    
    ## Age45-54:Crypto_Year1                 -0.024978   0.060115  -0.416 0.677771    
    ## Age55-64:Crypto_Year1                  0.079385   0.060022   1.323 0.185973    
    ## Age65+:Crypto_Year1                    0.004766   0.059023   0.081 0.935646    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.295 on 54625 degrees of freedom
    ## Multiple R-squared:  0.2379, Adjusted R-squared:  0.2375 
    ## F-statistic: 588.1 on 29 and 54625 DF,  p-value: < 2.2e-16

``` r
#plot(Demo_Crypto_Interaction_Model)
```
