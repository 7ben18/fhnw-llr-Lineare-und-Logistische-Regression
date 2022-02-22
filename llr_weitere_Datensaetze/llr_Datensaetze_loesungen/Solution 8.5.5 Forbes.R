#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Part II: Multiple Regression
#   "Solution 8.5.5 Forbes.R"
#   Author: Marcel Steiner-Curtis
#   Date: 25.08.2018    sml
#         20.06.2019    Remarks 1 and 2 of the second model corrected, sml
#--------------------------------------------------------------------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#   load R-function to do residual analysis (written by A. Ruckstuhl)
source("U:/Eigene Dateien/Unterricht/Module/FTAL-MSE/AppStat (e)/08 R/RFn_Plot-lmSim.R")

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Solution 8.5.5 Forbes
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   read data
file <- "U:/Eigene Dateien/Unterricht/Module/FTAL-MSE/AppStat (e)/04 Datasets/forbes.dat"
data <- read.table(file, header=TRUE)
str(data)
##    'data.frame':   17 obs. of  2 variables:
##     $ Boiling : num  194 194 198 198 199 ...
##     $ Pressure: num  20.8 20.8 22.4 22.7 23.1 ...

#   define new variable
data$Pressure.log <- log10(data$Pressure)


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (1) Model with log-transformed Pressure
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Estimation of the parameters
mod.log <- lm(Boiling ~ Pressure.log, data)
summary(mod.log)
##    Call:
##    lm(formula = Boiling ~ Pressure.log, data = data)
##
##    Residuals:
##         Min       1Q   Median       3Q      Max
##    -1.50249 -0.04380  0.03427  0.16540  0.38366
##
##    Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)
##    (Intercept)    47.864      2.852   16.78 3.93e-11 ***
##    Pressure.log  111.092      2.041   54.42  < 2e-16 ***
##    ---
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##    Residual standard error: 0.4223 on 15 degrees of freedom
##    Multiple R-squared:  0.995,     Adjusted R-squared:  0.9946
##    F-statistic:  2962 on 1 and 15 DF,  p-value: < 2.2e-16

#   Scatter diagram: Boiling ~ Pressure.log
plot(Boiling ~ Pressure.log, data, pch=20, main="Boiling ~ log(Pressure)")
grid()
abline(mod.log)
#identify(data$Pressure.log, data$Boiling)
##  12

#   REMARK: It is obvious that the straight line fits pretty well.
#           The observation with index i=12 is far away from the straight line and will be marked as
#           an outlier in the following residual analysis.

#   residual analysis
op <- par(mfcol=c(2,3))
    #   Tukey-Anscombe plot
    plot(mod.log, which=1, pch=20)
    grid()
    abline(h=0, lty=3)
    plot.lmSim(mod.log, which=1, SEED=1)
    grid()
    abline(h=0, lty=3)

    #   scale-location plot
    plot(mod.log, which=3, pch=20)
    grid()
    abline(h=0, lty=3)
    plot.lmSim(mod.log, which=3, SEED=1)
    grid()
    abline(h=0, lty=3)

    #   q-q plot
    plot(mod.log, which=2, pch=20)
    grid()
    plot.lmSim(mod.log, which=2, SEED=1)
    grid()
par(op)

#   REMARKS: The outlier with index i=12 destroys most of the plots in the residual analysis.
#            => Remove outlier and try again
#


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (2) Model with log-transformed Pressure without Outlier
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Estimation of the parameters
mod.log.red <- lm(Boiling ~ Pressure.log, data, subset=-12)
summary(mod.log.red)
##    Call:
##    lm(formula = Boiling ~ Pressure.log, data = data, subset = -12)
##
##    Residuals:
##         Min       1Q   Median       3Q      Max
##    -0.15284 -0.10099 -0.02385  0.07118  0.23671
##
##    Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)
##    (Intercept)   46.4530     0.8683    53.5   <2e-16 ***
##    Pressure.log 112.1713     0.6224   180.2   <2e-16 ***
##    ---
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##    Residual standard error: 0.1274 on 14 degrees of freedom
##    Multiple R-squared:  0.9996,    Adjusted R-squared:  0.9995
##    F-statistic: 3.249e+04 on 1 and 14 DF,  p-value: < 2.2e-16

#   Scatter diagram: Boiling ~ Pressure.log
plot(Boiling ~ Pressure.log, data, subset=-12, pch=20, main="Boiling ~ log(Pressure) without Outlier")
grid()
abline(mod.log.red)

#   REMARK: The straight line fits pretty well.

#   residual analysis
op <- par(mfcol=c(2,3))
    #   Tukey-Anscombe plot
    plot(mod.log.red, which=1, pch=20)
    grid()
    abline(h=0, lty=3)
    plot.lmSim(mod.log.red, which=1, SEED=1)
    grid()
    abline(h=0, lty=3)

    #   scale-location plot
    plot(mod.log.red, which=3, pch=20)
    grid()
    abline(h=0, lty=3)
    plot.lmSim(mod.log.red, which=3, SEED=1)
    grid()
    abline(h=0, lty=3)

    #   q-q plot
    plot(mod.log.red, which=2, pch=20)
    grid()
    plot.lmSim(mod.log.red, which=2, SEED=1)
    grid()
par(op)

#   REMARKS:
#   1.  Tukey-Anscombe plot shows very curved structure.
#       In the simulation it is visible that the original curve is not extreme.
#       => There is no hint that the expected value of the residuals is not constant.
#   2.  Scale-location plot shows a non-constant behaviour.
#       In the simulation it is visible that the original curve is not extreme.
#       => There is no hint that the scattering of the residuals is not constant.
#   3.  q-q plot shows no obvious discrepancy.
#       => There is no hint that the residuals are not normally distributed.

#   CONCLUSION: The fit without the 12th observation is satisfactory.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Correlation of the Residuals
#--------------------------------------------------------------------------------------------------------------------------------------------------
data
barplot(data$Pressure)
##       Boiling Pressure Pressure.log
##    1    194.5    20.79     1.317854
##    2    194.3    20.79     1.317854
##    3    197.9    22.40     1.350248
##    4    198.4    22.67     1.355452
##    5    199.4    23.15     1.364551
##    6    199.9    23.35     1.368287
##    7    200.9    23.89     1.378216
##    8    201.1    23.99     1.380030
##    9    201.4    24.02     1.380573
##    10   201.3    24.01     1.380392
##    11   203.6    25.14     1.400365
##    12   204.6    26.57     1.424392
##    13   209.5    28.49     1.454692
##    14   208.6    27.76     1.443419
##    15   210.7    29.04     1.462997
##    16   211.9    29.88     1.475381
##    17   212.2    30.06     1.477989

#   REMARK: We observe that the variable Pressure is ordered (exept observation i=14). Therefore, the original order is very likely lost.
#           => An investigation of the correlation of the residuals is meaningless.


#--------------------------------------------------------------------------------------------------------------------------------------------------
