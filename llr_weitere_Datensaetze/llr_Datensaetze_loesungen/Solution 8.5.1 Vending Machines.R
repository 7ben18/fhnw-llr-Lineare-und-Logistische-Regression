#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Part II: Multiple Regression
#   "Solution 8.5.1 Vending Machines.R"
#   Author: Marcel Steiner-Curtis
#   Date: 24.08.2018    sml
#--------------------------------------------------------------------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#   load R-function to do residual analysis (written by A. Ruckstuhl)
source("U:/Eigene Dateien/Unterricht/Module/FTAL-MSE/AppStat (e)/08 R/RFn_Plot-lmSim.R")

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Solution 8.5.1  Vending Machines
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   read data
file <- "U:/Eigene Dateien/Unterricht/Module/FTAL-MSE/AppStat (e)/04 Datasets/vending-machines.dat"
data <- read.table(file, header=TRUE)
str(data)
##    'data.frame':   25 obs. of  4 variables:
##     $ Time    : num  16.7 11.5 12 14.9 13.8 ...
##     $ Volume  : int  7 3 3 4 6 7 2 7 30 5 ...
##     $ Distance: int  560 220 340 80 150 330 110 210 1460 605 ...
##     $ Town    : Factor w/ 4 levels "Austin","Boston",..: 4 4 4 4 4 4 4 2 2 2 ...


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (a) Fit Model with Original Variables
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Estimation of the parameters
mod <- lm(Time ~ Volume, data);   mod
summary(mod)
##    Call:
##    lm(formula = Time ~ Volume, data = data)
##
##    Residuals:
##        Min      1Q  Median      3Q     Max
##    -7.5811 -1.8739 -0.3493  2.1807 10.6342
##
##    Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
##    (Intercept)    3.321      1.371   2.422   0.0237 *
##    Volume         2.176      0.124  17.546 8.22e-15 ***
##    ---
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##    Residual standard error: 4.181 on 23 degrees of freedom
##    Multiple R-squared:  0.9305,    Adjusted R-squared:  0.9275
##    F-statistic: 307.8 on 1 and 23 DF,  p-value: 8.22e-15

#   Scatter diagram: Time versus Volume
plot(Time ~ Volume, data, pch=20, xlim=c(0,30), ylim=c(0,80), main="Delivery Time versus Delivery Volume")
grid()
abline(mod)

#   residual analysis
op <- par(mfcol=c(2,3))
    #   Tukey-Anscombe plot
    plot(mod, which=1, pch=20)
    grid()
    abline(h=0, lty=3)
    plot.lmSim(mod, which=1, SEED=1)
    grid()
    abline(h=0, lty=3)

    #   scale-location plot
    plot(mod, which=3, pch=20)
    grid()
    abline(h=0, lty=3)
    plot.lmSim(mod, which=3, SEED=1)
    grid()
    abline(h=0, lty=3)

    #   q-q plot
    plot(mod, which=2, pch=20)
    grid()
    plot.lmSim(mod, which=2, SEED=1)
    grid()
par(op)

#   short form without the grids and different order of the plots
op <- par(mfrow=c(2,3))
    plot(mod, which=1:3, pch=20)
    plot.lmSim(mod, which=1:3, SEED=1)
par(op)

#   REMARKS:
#   1.  Tukey-Anscombe plot shows outlier with index i=9 which affects smooth curve.
#       In the simulation it is visible that the original curve is extreme.
#       => The expected value of the residuals cannot be constant.
#   2.  Scale-location plot shows a clear upwards trend.
#       In the simulation it is visible that the original curve is extreme.
#       => The scattering of the residuals is not constant.
#   3.  q-q plot shows a slightly heavy tail and the outlier with index i=9 is again obvious.
#       => Residuals are not normally distributed.

#   CONCLUSION: The fit is not satisfactory. Try transformations of response and explanatory variable.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (b) Fit Model with Transformes Variables
#--------------------------------------------------------------------------------------------------------------------------------------------------
data$Time.log   <- log10(data$Time)
data$Volume.log <- log10(data$Volume)
data$Volume.sq  <- sqrt(data$Volume)

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   1. Variant
mod1 <- lm(Time.log ~ Volume.log, data);   mod1
summary(mod1)
##    Call:
##    lm(formula = Time.log ~ Volume.log, data = data)
##
##    Residuals:
##          Min        1Q    Median        3Q       Max
##    -0.146768 -0.048069 -0.005349  0.053786  0.157310
##
##    Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
##    (Intercept)  0.65387    0.04732   13.82 1.26e-12 ***
##    Volume.log   0.74575    0.05317   14.03 9.25e-13 ***
##    ---
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##    Residual standard error: 0.07547 on 23 degrees of freedom
##    Multiple R-squared:  0.8953,    Adjusted R-squared:  0.8908
##    F-statistic: 196.7 on 1 and 23 DF,  p-value: 9.252e-13

#   Scatter diagram: Time versus Volume
plot(Time.log ~ Volume.log, data, pch=20, xlim=log10(c(1,30)), ylim=log10(c(1,80)), main="log(Delivery Time) versus log(Delivery Volume)")
grid()
abline(mod1)

#   residual analysis
op <- par(mfcol=c(2,3))
    #   Tukey-Anscombe plot
    plot(mod1, which=1, pch=20)
    grid()
    abline(h=0, lty=3)
    plot.lmSim(mod1, which=1, SEED=1)
    grid()
    abline(h=0, lty=3)

    #   scale-location plot
    plot(mod1, which=3, pch=20)
    grid()
    abline(h=0, lty=3)
    plot.lmSim(mod1, which=3, SEED=1)
    grid()
    abline(h=0, lty=3)

    #   q-q plot
    plot(mod1, which=2, pch=20)
    grid()
    plot.lmSim(mod1, which=2, SEED=1)
    grid()
par(op)

#   REMARKS:
#   1.  Tukey-Anscombe plot shows strange behaviour on the right side.
#       In the simulation it is visible that the original curve is extreme.
#       => The expected value of the residuals cannot be constant.
#   2.  Scale-location plot is okay.
#       => There is no hint that the scattering of the residuals is not constant.
#   3.  q-q plot is okay.
#       => There is no hint that the residuals are not normally distributed.

#   CONCLUSION: The fit is satisfactory. But let us try als the square-root transformation.

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   2. Variant
mod2 <- lm(Time.log ~ Volume.sq, data);   mod2
summary(mod2)
##    Call:
##    lm(formula = Time.log ~ Volume.sq, data = data)
##
##    Residuals:
##          Min        1Q    Median        3Q       Max
##    -0.110836 -0.055356  0.006322  0.031229  0.170410
##
##    Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
##    (Intercept)  0.67518    0.04093   16.50 3.08e-14 ***
##    Volume.sq    0.21773    0.01383   15.74 8.27e-14 ***
##    ---
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##    Residual standard error: 0.06798 on 23 degrees of freedom
##    Multiple R-squared:  0.9151,    Adjusted R-squared:  0.9114
##    F-statistic: 247.8 on 1 and 23 DF,  p-value: 8.271e-14

#   Scatter diagram: Time versus Volume
plot(Time.log ~ Volume.sq, data, pch=20, xlim=sqrt(c(0,30)), ylim=log10(c(1,80)), main="log(Delivery Time) versus sqrt(Delivery Volume)")
grid()
abline(mod2)

#   REMARK: Nice fit!

#   residual analysis
op <- par(mfcol=c(2,3))
    #   Tukey-Anscombe plot
    plot(mod2, which=1, pch=20)
    grid()
    abline(h=0, lty=3)
    plot.lmSim(mod2, which=1, SEED=1)
    grid()
    abline(h=0, lty=3)

    #   scale-location plot
    plot(mod2, which=3, pch=20)
    grid()
    abline(h=0, lty=3)
    plot.lmSim(mod2, which=3, SEED=1)
    grid()
    abline(h=0, lty=3)

    #   q-q plot
    plot(mod2, which=2, pch=20)
    grid()
    plot.lmSim(mod2, which=2, SEED=1)
    grid()
par(op)

#   REMARKS:
#   1.  Tukey-Anscombe plot is okay.
#       => The expected value of the residuals is constant.
#   2.  Scale-location plot is okay.
#       => The scattering of the residuals is constant.
#   3.  q-q plot is okay.
#       => Residuals are normally distributed.

#   CONCLUSION: The fit is perfect.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Correlation of the Residuals
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   WARNING: We absolutely have NO idea if the chronological order in the data set "vending-machines.dat" has never been changed.
#            At least there is no obvoius order visible in the data set, exept the ordering with respect to the Town.
#            => We will look at the correlation of the residuals assuming that in the data set "vending-machines.dat" we still have the original order.
op <- par(mfcol=c(2,3))
    ts0 <- residuals(mod)
    max0 <- max(abs(range(ts0)))
    plot(ts0, ylim=max0*c(-1,1), type="h", xlab="Index i", ylab="Residuals", main="Delivery Time versus Delivery Volume")
    grid()
    lines(ts0, type="h")
    abline(h=0)
    #   correlation:  R[t+1] ~ R[t]
    plot(ts0[-length(ts0)], ts0[-1], pch=20, xlim=max0*c(-1,1), ylim=max0*c(-1,1), xlab="e[i]", ylab="e[i+1]")
    grid()

    ts1 <- residuals(mod1)
    max1 <- max(abs(range(ts1)))
    plot(ts1, ylim=max1*c(-1,1), type="h", xlab="Index i", ylab="Residuals", main="log(Delivery Time) versus log(Delivery Volume)")
    grid()
    lines(ts1, type="h")
    abline(h=0)
    #   correlation:  R[t+1] ~ R[t]
    plot(ts1[-length(ts1)], ts1[-1], pch=20, xlim=max1*c(-1,1), ylim=max1*c(-1,1), xlab="e[i]", ylab="e[i+1]")
    grid()

    ts2 <- residuals(mod2)
    max2 <- max(abs(range(ts2)))
    plot(ts2, ylim=max2*c(-1,1), type="h", xlab="Index i", ylab="Residuals", main="log(Delivery Time) versus sqrt(Delivery Volume)")
    grid()
    lines(ts2, type="h")
    abline(h=0)
    #   correlation:  R[t+1] ~ R[t]
    plot(ts2[-length(ts2)], ts2[-1], pch=20, xlim=max2*c(-1,1), ylim=max2*c(-1,1), xlab="e[i]", ylab="e[i+1]")
    grid()
par(op)

#   REMARKS: Model 0 and 2 have slightly positiv correlated residuals.
#            Model 1 on the other hand has uncorrelated residuals.
#            Since the order in the data set might not correspond anymore to the original order we safely ignore these findings.


#--------------------------------------------------------------------------------------------------------------------------------------------------
