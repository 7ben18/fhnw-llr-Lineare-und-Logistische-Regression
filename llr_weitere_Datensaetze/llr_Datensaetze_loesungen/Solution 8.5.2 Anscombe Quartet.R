#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Part II: Multiple Regression
#   "Solution 8.5.2 Anscombe Quartet.R"
#   Author: Marcel Steiner-Curtis
#   Date: 24.08.2018    sml
#--------------------------------------------------------------------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Solution 8.5.2 Anscombe Quartet
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   read data
file <- "U:/Eigene Dateien/Unterricht/Module/FTAL-MSE/AppStat (e)/04 Datasets/anscombe.dat"
data <- read.table(file, header=TRUE)
str(data)
##    'data.frame':   11 obs. of  8 variables:
##     $ Y1: num  8.04 6.95 7.58 8.81 8.33 ...
##     $ X1: int  10 8 13 9 11 14 6 4 12 7 ...
##     $ Y2: num  9.14 8.14 8.74 8.77 9.26 8.1 6.13 3.1 9.13 7.26 ...
##     $ X2: int  10 8 13 9 11 14 6 4 12 7 ...
##     $ Y3: num  7.46 6.77 12.74 7.11 7.81 ...
##     $ X3: int  10 8 13 9 11 14 6 4 12 7 ...
##     $ Y4: num  6.58 5.76 7.71 8.84 8.47 7.04 5.25 5.56 7.91 6.89 ...
##     $ X4: int  8 8 8 8 8 8 8 8 8 8 ...


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (a) Fit Linear Models
#--------------------------------------------------------------------------------------------------------------------------------------------------
mod1 <- lm(Y1 ~ X1, data)
summary(mod1)
##    Call:
##    lm(formula = Y1 ~ X1, data = data)
##
##    Residuals:
##         Min       1Q   Median       3Q      Max
##    -1.92127 -0.45577 -0.04136  0.70941  1.83882
##
##    Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
##    (Intercept)   3.0001     1.1247   2.667  0.02573 *
##    X1            0.5001     0.1179   4.241  0.00217 **
##    ---
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##    Residual standard error: 1.237 on 9 degrees of freedom
##    Multiple R-squared:  0.6665,    Adjusted R-squared:  0.6295
##    F-statistic: 17.99 on 1 and 9 DF,  p-value: 0.00217

mod2 <- lm(Y2 ~ X2, data)
summary(mod2)
##    Call:
##    lm(formula = Y2 ~ X2, data = data)
##
##    Residuals:
##        Min      1Q  Median      3Q     Max
##    -1.9009 -0.7609  0.1291  0.9491  1.2691
##
##    Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
##    (Intercept)    3.001      1.125   2.667  0.02576 *
##    X2             0.500      0.118   4.239  0.00218 **
##    ---
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##    Residual standard error: 1.237 on 9 degrees of freedom
##    Multiple R-squared:  0.6662,    Adjusted R-squared:  0.6292
##    F-statistic: 17.97 on 1 and 9 DF,  p-value: 0.002179

mod3 <- lm(Y3 ~ X3, data)
summary(mod3)
##    Call:
##    lm(formula = Y3 ~ X3, data = data)
##
##    Residuals:
##        Min      1Q  Median      3Q     Max
##    -1.1586 -0.6146 -0.2303  0.1540  3.2411
##
##    Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
##    (Intercept)   3.0025     1.1245   2.670  0.02562 *
##    X3            0.4997     0.1179   4.239  0.00218 **
##    ---
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##    Residual standard error: 1.236 on 9 degrees of freedom
##    Multiple R-squared:  0.6663,    Adjusted R-squared:  0.6292
##    F-statistic: 17.97 on 1 and 9 DF,  p-value: 0.002176

mod4 <- lm(Y4 ~ X4, data)
summary(mod4)
##    Call:
##    lm(formula = Y4 ~ X4, data = data)
##
##    Residuals:
##       Min     1Q Median     3Q    Max
##    -1.751 -0.831  0.000  0.809  1.839
##
##    Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
##    (Intercept)   3.0017     1.1239   2.671  0.02559 *
##    X4            0.4999     0.1178   4.243  0.00216 **
##    ---
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##    Residual standard error: 1.236 on 9 degrees of freedom
##    Multiple R-squared:  0.6667,    Adjusted R-squared:  0.6297
##    F-statistic:    18 on 1 and 9 DF,  p-value: 0.002165

#   Summary table
Tab1 <- rbind(summary(mod1)$coefficients,
              summary(mod2)$coefficients,
              summary(mod3)$coefficients,
              summary(mod4)$coefficients)
rownames(Tab1) <- paste0(rep(1:4, each=2), ". Anscombe Quartet: ", rep(c("Intercept", "Slope"), 4))
Tab1
##                                    Estimate Std. Error  t value    Pr(>|t|)
##    1. Anscombe Quartet: Intercept 3.0000909  1.1247468 2.667348 0.025734051
##    1. Anscombe Quartet: Slope     0.5000909  0.1179055 4.241455 0.002169629
##    2. Anscombe Quartet: Intercept 3.0009091  1.1253024 2.666758 0.025758941
##    2. Anscombe Quartet: Slope     0.5000000  0.1179637 4.238590 0.002178816
##    3. Anscombe Quartet: Intercept 3.0024545  1.1244812 2.670080 0.025619109
##    3. Anscombe Quartet: Slope     0.4997273  0.1178777 4.239372 0.002176305
##    4. Anscombe Quartet: Intercept 3.0017273  1.1239211 2.670763 0.025590425
##    4. Anscombe Quartet: Slope     0.4999091  0.1178189 4.243028 0.002164602

Tab2 <- rbind(c(sigma=summary(mod1)$sigma, R2=summary(mod1)$r.squared),
              c(sigma=summary(mod2)$sigma, R2=summary(mod2)$r.squared),
              c(sigma=summary(mod3)$sigma, R2=summary(mod3)$r.squared),
              c(sigma=summary(mod4)$sigma, R2=summary(mod4)$r.squared))
rownames(Tab2) <- paste0(1:4, ". Anscombe Quartet: ")
Tab2
##                             sigma        R2
##    1. Anscombe Quartet:  1.236603 0.6665425
##    2. Anscombe Quartet:  1.237214 0.6662420
##    3. Anscombe Quartet:  1.236311 0.6663240
##    4. Anscombe Quartet:  1.235695 0.6667073


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (b) Graphic
#--------------------------------------------------------------------------------------------------------------------------------------------------
op <- par(mfrow=c(2,2))
    plot(data$X1, data$Y1, pch=20, xlab="x", ylab="y", xlim=c(0,20), ylim=c(0,15), main="1. Anscombe Quartet")
    grid()
    abline(mod1)
    plot(data$X2, data$Y2, pch=20, xlab="x", ylab="y", xlim=c(0,20), ylim=c(0,15), main="2. Anscombe Quartet")
    grid()
    abline(mod2)
    plot(data$X3, data$Y3, pch=20, xlab="x", ylab="y", xlim=c(0,20), ylim=c(0,15), main="3. Anscombe Quartet")
    grid()
    abline(mod3)
    plot(data$X4, data$Y4, pch=20, xlab="x", ylab="y", xlim=c(0,20), ylim=c(0,15), main="4. Anscombe Quartet")
    grid()
    abline(mod4)
par(op)


#--------------------------------------------------------------------------------------------------------------------------------------------------
