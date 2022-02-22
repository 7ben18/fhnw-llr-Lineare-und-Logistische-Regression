#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Part II: Multiple Regression
#   "Solution 7.5.1 Venice Sea Level.R"
#   Author: Marcel Steiner-Curtis
#   Date: 21.08.2018    sml
#--------------------------------------------------------------------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Solution 7.5.1 Venice Sea Level Data
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   read data
file <- "U:/Eigene Dateien/Unterricht/Module/FTAL-MSE/AppStat (e)/04 Datasets/venice.dat"
data <- read.table(file, header=TRUE)
str(data)
##    'data.frame':   51 obs. of  2 variables:
##     $ Year    : int  1931 1932 1933 1934 1935 1936 1937 1938 1939 1940 ...
##     $ SeaLevel: int  103 78 121 116 115 147 119 114 89 102 ...


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (a) Scatter Diagram: Sea Level versus Year
#--------------------------------------------------------------------------------------------------------------------------------------------------
plot(SeaLevel ~ Year, data, pch=20, main="Sea Level versus Year")
grid()

#   REMARK: The diagram indicates that there might be a weak relationship between the sea level and the year.
#           It seems that we can see in the data that Venice sinks.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (b) Parameter Estimation
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Estimation of the parameters (explicit formulae)
Year.bar <- mean(data$Year)
SeaLevel.bar <- mean(data$SeaLevel)
Sxx <- sum((data$Year-Year.bar)^2)
Sxy <- sum((data$Year-Year.bar)*(data$SeaLevel-SeaLevel.bar))
#   slope
beta1 <- Sxy/Sxx;   beta1
##  0.5669683
#   intercept
beta0 <- SeaLevel.bar - beta1*Year.bar;   beta0
##  -989.3822

#   REMARK: The intercept is the sea level in the year 0, that is 2000 years ago. This shows clearly that the intercept in this
#           example has no physical meaning, since it is much too far away from the point cloud.

#   fitted values
SeaLevel.hat <- beta0 + beta1*data$Year
#   residuals
res <- data$SeaLevel - SeaLevel.hat
mean(res)
##  -7.801999e-14
#   error sum of squares
sigma2.hat <- sum(res^2)/(nrow(data)-2)
#   residual standard error
sqrt(sigma2.hat)
##  18.61977

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Estimation of the parameters (lm)
mod <- lm(SeaLevel ~ Year, data);   mod
##    Call:
##    lm(formula = SeaLevel ~ Year, data = data)
##
##    Coefficients:
##    (Intercept)         Year
##       -989.382        0.567

#   mod is a big list
str(mod)
##    lIST OF 12
##     $ COEFFICIENTS : nAMED NUM [1:2] -989.382 0.567
##      ..- ATTR(*, "NAMES")= CHR [1:2] "(iNTERCEPT)" "yEAR"
##     $ RESIDUALS    : nAMED NUM [1:51] -2.43 -28 14.43 8.87 7.3 ...
##      ..- ATTR(*, "NAMES")= CHR [1:51] "1" "2" "3" "4" ...
##     $ EFFECTS      : nAMED NUM [1:51] -854.2 59.6 19.1 13.3 11.5 ...
##      ..- ATTR(*, "NAMES")= CHR [1:51] "(iNTERCEPT)" "yEAR" "" "" ...
##     $ RANK         : INT 2
##     $ FITTED.VALUES: nAMED NUM [1:51] 105 106 107 107 108 ...
##      ..- ATTR(*, "NAMES")= CHR [1:51] "1" "2" "3" "4" ...
##     $ ASSIGN       : INT [1:2] 0 1
##     $ QR           :lIST OF 5
##      ..$ QR   : NUM [1:51, 1:2] -7.14 0.14 0.14 0.14 0.14 ...
##      .. ..- ATTR(*, "DIMNAMES")=lIST OF 2
##      .. .. ..$ : CHR [1:51] "1" "2" "3" "4" ...
##      .. .. ..$ : CHR [1:2] "(iNTERCEPT)" "yEAR"
##      .. ..- ATTR(*, "ASSIGN")= INT [1:2] 0 1
##      ..$ QRAUX: NUM [1:2] 1.14 1.2
##      ..$ PIVOT: INT [1:2] 1 2
##      ..$ TOL  : NUM 1E-07
##      ..$ RANK : INT 2
##      ..- ATTR(*, "CLASS")= CHR "QR"
##     $ DF.RESIDUAL  : INT 49
##     $ XLEVELS      : nAMED LIST()
##     $ CALL         : LANGUAGE LM(FORMULA = sEAlEVEL ~ yEAR, DATA = DATA)
##     $ TERMS        :cLASSES 'TERMS', 'FORMULA'  LANGUAGE sEAlEVEL ~ yEAR
##      .. ..- ATTR(*, "VARIABLES")= LANGUAGE LIST(sEAlEVEL, yEAR)
##      .. ..- ATTR(*, "FACTORS")= INT [1:2, 1] 0 1
##      .. .. ..- ATTR(*, "DIMNAMES")=lIST OF 2
##      .. .. .. ..$ : CHR [1:2] "sEAlEVEL" "yEAR"
##      .. .. .. ..$ : CHR "yEAR"
##      .. ..- ATTR(*, "TERM.LABELS")= CHR "yEAR"
##      .. ..- ATTR(*, "ORDER")= INT 1
##      .. ..- ATTR(*, "INTERCEPT")= INT 1
##      .. ..- ATTR(*, "RESPONSE")= INT 1
##      .. ..- ATTR(*, ".eNVIRONMENT")=<ENVIRONMENT: r_gLOBALeNV>
##      .. ..- ATTR(*, "PREDVARS")= LANGUAGE LIST(sEAlEVEL, yEAR)
##      .. ..- ATTR(*, "DATAcLASSES")= nAMED CHR [1:2] "NUMERIC" "NUMERIC"
##      .. .. ..- ATTR(*, "NAMES")= CHR [1:2] "sEAlEVEL" "yEAR"
##     $ MODEL        :'DATA.FRAME':  51 OBS. OF  2 VARIABLES:
##      ..$ sEAlEVEL: INT [1:51] 103 78 121 116 115 147 119 114 89 102 ...
##      ..$ yEAR    : INT [1:51] 1931 1932 1933 1934 1935 1936 1937 1938 1939 1940 ...
##      ..- ATTR(*, "TERMS")=cLASSES 'TERMS', 'FORMULA'  LANGUAGE sEAlEVEL ~ yEAR
##      .. .. ..- ATTR(*, "VARIABLES")= LANGUAGE LIST(sEAlEVEL, yEAR)
##      .. .. ..- ATTR(*, "FACTORS")= INT [1:2, 1] 0 1
##      .. .. .. ..- ATTR(*, "DIMNAMES")=lIST OF 2
##      .. .. .. .. ..$ : CHR [1:2] "sEAlEVEL" "yEAR"
##      .. .. .. .. ..$ : CHR "yEAR"
##      .. .. ..- ATTR(*, "TERM.LABELS")= CHR "yEAR"
##      .. .. ..- ATTR(*, "ORDER")= INT 1
##      .. .. ..- ATTR(*, "INTERCEPT")= INT 1
##      .. .. ..- ATTR(*, "RESPONSE")= INT 1
##      .. .. ..- ATTR(*, ".eNVIRONMENT")=<ENVIRONMENT: r_gLOBALeNV>
##      .. .. ..- ATTR(*, "PREDVARS")= LANGUAGE LIST(sEAlEVEL, yEAR)
##      .. .. ..- ATTR(*, "DATAcLASSES")= nAMED CHR [1:2] "NUMERIC" "NUMERIC"
##      .. .. .. ..- ATTR(*, "NAMES")= CHR [1:2] "sEAlEVEL" "yEAR"
##     - ATTR(*, "CLASS")= CHR "LM"


#   coefficients
mod$coefficients
##     (Intercept)         Year
##    -989.3822021    0.5669683

#   residuals
mod$residuals
##              1           2           3           4           5           6           7           8           9          10
##     -2.4336350 -28.0006033  14.4324284   8.8654600   7.2984917  38.7315234  10.1645551   4.5975867 -20.9693816  -8.5363499
##             11          12          13          14          15          16          17          18          19          20
##    -12.1033183 -20.6702866 -15.2372549  -6.8042232  -8.3711916  22.0618401  11.4948718  16.9279035 -11.6390649   0.7939668
##             21          22          23          24          25          26          27          28          29          30
##     34.2269985  -1.3399698 -10.9069382  -6.4739065 -22.0408748 -24.6078431  -1.1748115   3.2582202  -3.3087481  23.1242836
##             31          32          33          34          35          36          37          38          39          40
##     -0.4426848  -9.0096531  -5.5766214 -17.1435897 -14.7105581  68.7224736  12.1555053  17.5885370  11.0215686  -4.5453997
##             41          42          43          44          45          46          47          48          49          50
##     -6.1123680  -8.6793363 -15.2463047 -33.8132730  -5.3802413  -6.9472097 -11.5141780  -0.0811463  33.3518854   0.7849170
##             51
##      4.2179487

#   fitted values
mod$fitted.values
##           1        2        3        4        5        6        7        8        9       10       11       12       13       14
##    105.4336 106.0006 106.5676 107.1345 107.7015 108.2685 108.8354 109.4024 109.9694 110.5363 111.1033 111.6703 112.2373 112.8042
##          15       16       17       18       19       20       21       22       23       24       25       26       27       28
##    113.3712 113.9382 114.5051 115.0721 115.6391 116.2060 116.7730 117.3400 117.9069 118.4739 119.0409 119.6078 120.1748 120.7418
##          29       30       31       32       33       34       35       36       37       38       39       40       41       42
##    121.3087 121.8757 122.4427 123.0097 123.5766 124.1436 124.7106 125.2775 125.8445 126.4115 126.9784 127.5454 128.1124 128.6793
##          43       44       45       46       47       48       49       50       51
##    129.2463 129.8133 130.3802 130.9472 131.5142 132.0811 132.6481 133.2151 133.7821

#   deviance
dev <- deviance(mod);   dev
##  16988.1
#   degrees of freedom
dfree <- df.residual(mod);   dfree
##  49

#   residual standard error
sigma.hat <- sqrt(dev/dfree);   sigma.hat
##  18.61977


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (c) Scatter Diagram: Time versus Volume with the best Model
#--------------------------------------------------------------------------------------------------------------------------------------------------
plot(SeaLevel ~ Year, data, pch=20, main="Sea Level versus Year with best model")
grid()
#   add best model
abline(mod)

#   REMARK: The best model indeed has a positive slope indicating that the data could support the hypothesis the Venice sinks.
#           On the other hand the points scatter a lot around the straight line.
#           We will have to test the hypothesis H0: slope=0

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (d) Testing Hypothesis
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   null hypothesis        H0: slope=0
#   alternative hypothesis H1: slope <> 0
beta10 <- 0
#   estimator of slope
beta1.hat <- as.numeric(mod$coefficients["Year"]);   beta1.hat
##  0.5669683
#   standard error (Sxx from Ex. 7.2.1)
se.beta1 <- sqrt(sigma.hat^2/Sxx);   se.beta1
##  0.1771305
#   test statistic
Test.beta1 <- (beta1.hat-beta10)/se.beta1;   Test.beta1
##  3.200852
#   critical value (two-sided)
alpha <- 0.05
#   the degrees of freedom were defined in Ex. 7.2.1
t.crit <- qt(p=1-alpha/2, df=dfree);   t.crit
##  2.009575

#   REMARK: Since abs(Test.beta1)=3.200852 > t.crit=2.009575 we reject the null hypothesis and conclude that the Sea level
#           depends significantly on the year.
#           In other words the data set supports the the hypothesis that Venice sinks.
#           The annual sinking rate is 0.5669683 cm/year.

#   total amount of sinking
diff(range(data$Year)) * beta1.hat
##  28.34842

#   REMARK: In the years 1931-1981 Venice sank 28.34842 cm.


#--------------------------------------------------------------------------------------------------------------------------------------------------
summary(mod)
##    Call:
##    lm(formula = SeaLevel ~ Year, data = data)
##
##    Residuals:
##        Min      1Q  Median      3Q     Max
##    -33.813 -11.211  -3.309   9.515  68.722
##
##    Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)
##    (Intercept) -989.3822   346.4770  -2.856  0.00628 **
##    Year           0.5670     0.1771   3.201  0.00241 **
##    ---
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##    Residual standard error: 18.62 on 49 degrees of freedom
##    Multiple R-squared:  0.1729,    Adjusted R-squared:  0.1561
##    F-statistic: 10.25 on 1 and 49 DF,  p-value: 0.002406


#--------------------------------------------------------------------------------------------------------------------------------------------------
