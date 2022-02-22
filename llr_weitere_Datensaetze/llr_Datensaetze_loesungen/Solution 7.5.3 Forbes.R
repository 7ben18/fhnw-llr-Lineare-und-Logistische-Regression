#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Part II: Multiple Regression
#   "Solution 7.5.3 Forbes.R"
#   Author: Marcel Steiner-Curtis
#   Date: 21.08.2018    sml
#--------------------------------------------------------------------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Solution 7.5.3 Forbes
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   read data
file <- "U:/Eigene Dateien/Unterricht/Module/FTAL-MSE/AppStat (e)/04 Datasets/forbes.dat"
data <- read.table(file, header=TRUE)
str(data)
##    'data.frame':   17 obs. of  2 variables:
##     $ Boiling : num  194 194 198 198 199 ...
##     $ Pressure: num  20.8 20.8 22.4 22.7 23.1 ...


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (a) Scatter Diagram
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   define new variable
data$Pressure.log <- log10(data$Pressure)
#   scatter diagram
op <- par(mfrow=c(1,2))
    plot(Boiling ~ Pressure, data, pch=20, main="Boiling versus Pressure")
    grid()
    plot(Boiling ~ Pressure.log, data, pch=20, main="Boiling versus log(Pressure)")
    grid()
par(op)

#   REMARKS: In the first diagram the points are slightly curved.
#            In the second diagram the points lie almost perfectly on a straight line.
#            There might be an outlier.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (b) Parameter Estimation
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

#   graphic
plot(Boiling ~ Pressure.log, data, pch=20, main="Boiling versus log(Pressure)")
grid()
abline(mod.log)
#identify(data$Pressure.log, data$Boiling)
##  12

#   REMARKS: The straight line model almost perfectly describes the transformed data.
#            The point with index 12 seems to be too far away from the line. It might be an outlier.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (c) Parameter Estimation without Outlier
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

#   graphic
plot(Boiling ~ Pressure.log, data, pch=20, main="Boiling versus log(Pressure)")
grid()
abline(mod.log)
abline(mod.log.red, col="red")

#   REMARKS: The estimates of the parameters did not change much and the new fitted straight line remained almost the same.
#            On the other hand we observe that the standard errors of the intercept and the slope and the residual standard error
#            were reduced by a factor of 3. We observe that outliers can have a huge impact on the least-squares estimation.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (d) Test the slope for significance of regression on the 5% level
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   null hypothesis H0: beta1 = 0

coefficients(summary(mod.log.red))
##                  Estimate Std. Error   t value     Pr(>|t|)
##    (Intercept)   46.45304  0.8683141  53.49796 1.359548e-17
##    Pressure.log 112.17131  0.6223544 180.23703 5.767389e-25

#   REMARKS: Since the P-value Pr(>|t|)=5.767389e-25 for the slope is much snaller than 5% we reject the null hypothesis
#            and conclude that there is a significant linear relationship between the logarithm of the atmospheric pressure
#            and the boiling point of water.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (e) 95% Confidence Interval
#--------------------------------------------------------------------------------------------------------------------------------------------------
confint(mod.log.red, parm=2, level=0.95)
##                    2.5 %   97.5 %
##    Pressure.log 110.8365 113.5061

#   REMARK: The 95% confidence interval for the slope is [110.8365, 113.5061]


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (f) Confidence Intervals on the Response
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   define a new data.frame of equally spaced x-values covering at least the range log(Pressure)
range(data$Pressure)
#   20.79 30.06
data.new <- data.frame(Pressure.log=log10(seq(15, 35, by=0.1)))
#   predict using interval="confidence"
Boiling.Conf <- predict(mod.log.red, newdata=data.new, interval="confidence", level=0.95)
Boiling.Conf[data.new$Pressure.log==log10(26),]
##         fit      lwr      upr
##    205.1725 205.0987 205.2462

#   REMARK: The estimated value is 205.1725 and the 95% confidence interval on the response is [205.0987, 205.2462].

#   scatter diagram: Time versus Volume with confidence intervals on the response
plot(Boiling ~ Pressure.log, data, subset=-12, pch=20, main="Boiling versus log(Pressure), subset=-12")
grid()
abline(v=log10(26), col="red")
#   add best model
lines(data.new$Pressure.log, Boiling.Conf[,"fit"], lty=1)
#   add confidence intervals on the response
lines(data.new$Pressure.log, Boiling.Conf[,"lwr"], lty=2)
lines(data.new$Pressure.log, Boiling.Conf[,"upr"], lty=2)


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (g) Prediction Intervals
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   predict using interval="prediction"
Boiling.Pred <- predict(mod.log.red, newdata=data.new, interval="prediction", level=0.95)
Boiling.Pred[data.new$Pressure.log==log10(26),]
##         fit      lwr      upr
##    205.1725 204.8893 205.4556

#   REMARK: The estimated value is 205.1725 and the 95% prediction intervalc is [204.8893, 205.4556] which is pretty small.

#   add prediction intervals
lines(data.new$Pressure.log, Boiling.Pred[,"lwr"], lty=3)
lines(data.new$Pressure.log, Boiling.Pred[,"upr"], lty=3)
#   add legend
legend("topleft", legend=c("best model", "confidence intervals on the response", "prediction intervals"), lty=c(1,2,3), bty="n")


#--------------------------------------------------------------------------------------------------------------------------------------------------
