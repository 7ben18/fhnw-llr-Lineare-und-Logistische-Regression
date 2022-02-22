#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Part II: Multiple Regression
#   "Solution 8.5.4 Windmill.R"
#   Author: Marcel Steiner-Curtis
#   Date: 25.08.2018    sml
#--------------------------------------------------------------------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#   load R-function to do residual analysis (written by A. Ruckstuhl)
source("U:/Eigene Dateien/Unterricht/Module/FTAL-MSE/AppStat (e)/08 R/RFn_Plot-lmSim.R")

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Solution 8.5.4 Windmill
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   read data
file <- "U:/Eigene Dateien/Unterricht/Module/FTAL-MSE/AppStat (e)/04 Datasets/windmill.dat"
data <- read.table(file, header=TRUE)
str(data)
##    'data.frame':   25 obs. of  2 variables:
##     $ WindVelocity: num  11.19 13.42 7.61 6.04 22.37 ...
##     $ DCOutput    : num  1.58 1.82 1.06 0.5 2.24 ...

#   define new variables
data$WindVelocity.log <- log10(data$WindVelocity)
data$WindVelocity.inv <- 1/data$WindVelocity
data$DCOutput.log     <- log10(data$DCOutput)


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (1) Naiv Model
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Estimation of the parameters
mod <- lm(DCOutput ~ WindVelocity, data)
summary(mod)
##    Call:
##    lm(formula = DCOutput ~ WindVelocity, data = data)
##
##    Residuals:
##         Min       1Q   Median       3Q      Max
##    -0.59869 -0.14099  0.06059  0.17262  0.32184
##
##    Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)
##    (Intercept)  0.130875   0.125989   1.039     0.31
##    WindVelocity 0.107780   0.008514  12.659 7.55e-12 ***
##    ---
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##    Residual standard error: 0.2361 on 23 degrees of freedom
##    Multiple R-squared:  0.8745,    Adjusted R-squared:  0.869
##    F-statistic: 160.3 on 1 and 23 DF,  p-value: 7.546e-12

#   Scatter diagram: DCOutput ~ WindVelocity
plot(DCOutput ~ WindVelocity, data, pch=20, xlim=c(5,25), ylim=c(0,3), main="DCOutput ~ WindVelocity")
grid()
abline(mod)

#   REMARK: It is obvious that the straight line does not appropriately fit the curved data.

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

#   REMARKS:
#   1.  Tukey-Anscombe plot shows very curved structure.
#       In the simulation it is visible that the original curve is extreme.
#       => The expected value of the residuals cannot be constant.
#   2.  Scale-location plot shows a weak downward trend.
#       In the simulation it is visible that the original curve is within the 19 simulated curves.
#       => There is no hint that the scattering of the residuals is not constant.
#   3.  q-q plot shows no obvious discrepancy.
#       => There is no hint that the residuals are not normally distributed.

#   CONCLUSION: The fit is not satisfactory at all. Try log-transformations of response and explanatory variable.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (2) Model with First Aid Transformations of Response and Explanatory Variable
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Estimation of the parameters
mod.loglog <- lm(DCOutput.log ~ WindVelocity.log, data)
summary(mod.loglog)
##    Call:
##    lm(formula = DCOutput.log ~ WindVelocity.log, data = data)
##
##    Residuals:
##         Min       1Q   Median       3Q      Max
##    -0.59144 -0.05717  0.02044  0.07850  0.16017
##
##    Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)
##    (Intercept)       -1.2698     0.1786  -7.110 3.05e-07 ***
##    WindVelocity.log   1.2872     0.1603   8.031 4.01e-08 ***
##    ---
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##    Residual standard error: 0.1536 on 23 degrees of freedom
##    Multiple R-squared:  0.7371,    Adjusted R-squared:  0.7257
##    F-statistic:  64.5 on 1 and 23 DF,  p-value: 4.014e-08

#   Scatter diagram: DCOutput ~ WindVelocity
plot(DCOutput.log ~ WindVelocity.log, data, pch=20, xlim=log10(c(5,25)), ylim=log10(c(0.1,3)), main="log(DCOutput) ~ log(WindVelocity)")
grid()
abline(mod.loglog)
#identify(data$WindVelocity.log, data$DCOutput.log)
##  25

#   REMARK: It is obvious that the straight line does not appropriately fit the curved data.
#           Especially the observation with index i=25 is extremely far away from the straight line and will be marked as
#           an outlier in the following residual analysis.

#   residual analysis
op <- par(mfcol=c(2,3))
    #   Tukey-Anscombe plot
    plot(mod.loglog, which=1, pch=20)
    grid()
    abline(h=0, lty=3)
    plot.lmSim(mod.loglog, which=1, SEED=1)
    grid()
    abline(h=0, lty=3)

    #   scale-location plot
    plot(mod.loglog, which=3, pch=20)
    grid()
    abline(h=0, lty=3)
    plot.lmSim(mod.loglog, which=3, SEED=1)
    grid()
    abline(h=0, lty=3)

    #   q-q plot
    plot(mod.loglog, which=2, pch=20)
    grid()
    plot.lmSim(mod.loglog, which=2, SEED=1)
    grid()
par(op)

#   REMARKS:
#   1.  Tukey-Anscombe plot shows very curved structure.
#       In the simulation it is visible that the original curve is extreme.
#       => The expected value of the residuals cannot be constant.
#   2.  Scale-location plot shows an oscillating behavior.
#       In the simulation it is visible that the original curve is somehow different from the 19 simulated curves.
#       => The scattering of the residuals is not constant.
#   3.  q-q plot shows no obvious discrepancy. Observation with index i=25 is marked as an outlier.
#       => The residuals are not normally distributed.

#   CONCLUSION: The fit is not satisfactory at all, all three diagnostic tools show abnormalities.
#               => The first aid transformations were not helpful.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (3) Mechanistic Model based on First Principles
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Estimation of the parameters
mod.inv <- lm(DCOutput ~ WindVelocity.inv, data)
summary(mod.inv)
##    Call:
##    lm(formula = DCOutput ~ WindVelocity.inv, data = data)
##
##    Residuals:
##         Min       1Q   Median       3Q      Max
##    -0.20547 -0.04940  0.01100  0.08352  0.12204
##
##    Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)
##    (Intercept)        2.9789     0.0449   66.34   <2e-16 ***
##    WindVelocity.inv -15.5155     0.4619  -33.59   <2e-16 ***
##    ---
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##    Residual standard error: 0.09417 on 23 degrees of freedom
##    Multiple R-squared:   0.98,     Adjusted R-squared:  0.9792
##    F-statistic:  1128 on 1 and 23 DF,  p-value: < 2.2e-16

#   Scatter diagram: DCOutput ~ WindVelocity
plot(DCOutput ~ WindVelocity.inv, data, pch=20, xlim=1/(c(5,25)), ylim=c(0,3), main="DCOutput ~ 1/WindVelocity")
grid()
abline(mod.inv)

#   REMARK: Nice fit!

#   residual analysis
op <- par(mfcol=c(2,3))
    #   Tukey-Anscombe plot
    plot(mod.inv, which=1, pch=20)
    grid()
    abline(h=0, lty=3)
    plot.lmSim(mod.inv, which=1, SEED=1)
    grid()
    abline(h=0, lty=3)

    #   scale-location plot
    plot(mod.inv, which=3, pch=20)
    grid()
    abline(h=0, lty=3)
    plot.lmSim(mod.inv, which=3, SEED=1)
    grid()
    abline(h=0, lty=3)

    #   q-q plot
    plot(mod.inv, which=2, pch=20)
    grid()
    plot.lmSim(mod.inv, which=2, SEED=1)
    grid()
par(op)

#   REMARKS:
#   1.  Tukey-Anscombe plot shows weak curved structure.
#       In the simulation it is visible that the original curve is not extreme.
#       => There is no hint that the expected value of the residuals is not constant.
#   2.  Scale-location plot shows a hick.
#       In the simulation it is visible that the original curve is within 19 simulated curves.
#       => There is no hint that the scattering of the residuals is not constant.
#   3.  q-q plot shows no obvious discrepancy exept maybe an indication of a short-tailed distribution at the right end.
#       (Short-tailed distribution of the errors is in general not critical for least-squares estimation.)
#       => There is no hint that the residuals are not normally distributed.

#   CONCLUSION: The fit is satisfactory, all three diagnostic tools show no abnormalities.
#               => The mechanistic model is the best of all three.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Correlation of the Residuals
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   WARNING: We absolutely have NO idea if the chronological order in the data set "windmill.dat" has never been changed.
#            At least there is no obvoius order visible in the data set.
#            => We will look at the correlation of the residuals assuming that in the data set "windmill.dat" we still have the original order.
op <- par(mfcol=c(2,3))
    ts0 <- residuals(mod)
    max0 <- max(abs(range(ts0)))
    plot(ts0, ylim=max0*c(-1,1), type="h", xlab="Index i", ylab="Residuals", main="DCOutput ~ WindVelocity")
    grid()
    lines(ts0, type="h")
    abline(h=0)
    #   correlation:  R[t+1] ~ R[t]
    plot(ts0[-length(ts0)], ts0[-1], pch=20, xlim=max0*c(-1,1), ylim=max0*c(-1,1), xlab="e[i]", ylab="e[i+1]")
    grid()

    ts1 <- residuals(mod.loglog)
    max1 <- max(abs(range(ts1)))
    plot(ts1, ylim=max1*c(-1,1), type="h", xlab="Index i", ylab="Residuals", main="log(DCOutput) ~ log(WindVelocity)")
    grid()
    lines(ts1, type="h")
    abline(h=0)
    #   correlation:  R[t+1] ~ R[t]
    plot(ts1[-length(ts1)], ts1[-1], pch=20, xlim=max1*c(-1,1), ylim=max1*c(-1,1), xlab="e[i]", ylab="e[i+1]")
    grid()

    ts2 <- residuals(mod.inv)
    max2 <- max(abs(range(ts2)))
    plot(ts2, ylim=max2*c(-1,1), type="h", xlab="Index i", ylab="Residuals", main="DCOutput ~ 1/WindVelocity")
    grid()
    lines(ts2, type="h")
    abline(h=0)
    #   correlation:  R[t+1] ~ R[t]
    plot(ts2[-length(ts2)], ts2[-1], pch=20, xlim=max2*c(-1,1), ylim=max2*c(-1,1), xlab="e[i]", ylab="e[i+1]")
    grid()
par(op)

#   REMARKS: All models show uncorrelated residuals.


#--------------------------------------------------------------------------------------------------------------------------------------------------
