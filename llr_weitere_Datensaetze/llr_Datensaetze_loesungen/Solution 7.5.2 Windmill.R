#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Part II: Multiple Regression
#   "Solution 7.5.2 Windmill.R"
#   Author: Marcel Steiner-Curtis
#   Date: 21.08.2018    sml
#--------------------------------------------------------------------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Solution 7.5.2 Windmill
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   read data
file <- "U:/Eigene Dateien/Unterricht/Module/FTAL-MSE/AppStat (e)/04 Datasets/windmill.dat"
data <- read.table(file, header=TRUE)
str(data)
##    'data.frame':   25 obs. of  2 variables:
##     $ WindVelocity: num  11.19 13.42 7.61 6.04 22.37 ...
##     $ DCOutput    : num  1.58 1.82 1.06 0.5 2.24 ...

#   define new variable
data$WindVelocity.inv <- 1/data$WindVelocity

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (a) Scatter Diagram
#--------------------------------------------------------------------------------------------------------------------------------------------------
op <- par(mfrow=c(1,2))
    plot(DCOutput ~ WindVelocity, data, pch=20, main="DCOutput versus WindVelocity")
    grid()
    plot(DCOutput ~ WindVelocity.inv, data, pch=20, main="DCOutput versus 1/WindVelocity")
    grid()
par(op)

#   REMARK: A straight line model fits much better to the second diagram with DCOutput versus 1/WindVelocity
#           than to the first with DCOutput versus WindVelocity.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (b) Parameter Estimation
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
##                      Estimate Std. Error t value Pr(>|t|)
##    (Intercept)         2.9789     0.0449   66.34   <2e-16 ***
##    I(1/WindVelocity) -15.5155     0.4619  -33.59   <2e-16 ***
##    ---
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##    Residual standard error: 0.09417 on 23 degrees of freedom
##    Multiple R-squared:   0.98,     Adjusted R-squared:  0.9792
##    F-statistic:  1128 on 1 and 23 DF,  p-value: < 2.2e-16

#   coefficients
mod.inv$coefficients
##    (Intercept) WindVelocity.inv
##       2.97886         -15.51546

#   standard errors
summary(mod.inv)$coefficients[,2]
##    (Intercept) WindVelocity.inv
##    0.04490228        0.46187746


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (c) 99% Confidence Interval
#--------------------------------------------------------------------------------------------------------------------------------------------------
confint(mod.inv, parm=2, level=0.99)
##                        0.5 %    99.5 %
##    WindVelocity.inv -16.8121 -14.21881


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (d) Scatter Diagram: DCOutput versus 1/WindVelocity with the Best Model
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   transformed variable
plot(DCOutput ~ WindVelocity.inv, data, pch=20, ylim=c(0,3), main="DCOutput versus 1/WindVelocity with best model")
grid()
#   add best model
abline(mod.inv)

#   original variable
WV.new <- data.frame(WindVelocity.inv=seq(0.2*min(1/data$WindVelocity), 2*max(1/data$WindVelocity), length=101))
DCOutput.pred <- predict(mod.inv, newdata=WV.new)
plot(DCOutput ~ WindVelocity, data, pch=20, xlim=c(0,50), ylim=c(0,3), main="DCOutput versus WindVelocity with best model")
grid()
abline(h=0)
#   add best model
lines(1/WV.new$WindVelocity.inv, DCOutput.pred)
#   add maximum possible DC output
abline(h=mod.inv$coefficients["(Intercept)"], col="blue")

#   minimal wind velocity to produce DC output, i.e. solve DCOutput(WindVelocity)=0 for WindVelocity
WV.min <- -as.numeric(mod.inv$coefficients["WindVelocity.inv"]/mod.inv$coefficients["(Intercept)"]);   WV.min
abline(v=WV.min, col="red")
##  5.208521

#   REMARKS: The best model fits nicely.
#            If the wind velocity were to tend to infinity then the parameter beta0 would be the maximum possible DC output, that is 2.97886 A.
#            Observing the diagram we can see that a certain minimal wind velocity is necessary to produce a DC output, that is 5.208521 m/s.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (e) Estimated DC Output and Confidence Interval of the Response and the Prediction Interval
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   wind velocities of interest
WV.new2 <- data.frame(WindVelocity.inv=c(1/1,1/10))
#   estimated DC output and confidence interval of the response
WV.conf <- predict(mod.inv, newdata=WV.new2, interval="confidence", level=0.95);   WV.conf
##             fit        lwr        upr
##    1 -12.536597 -13.408613 -11.664581
##    2   1.427314   1.386768   1.467861
#   prediction interval
WV.pred <- predict(mod.inv, newdata=WV.new2, interval="prediction", level=0.95);   WV.pred
##             fit        lwr        upr
##    1 -12.536597 -13.430108 -11.643086
##    2   1.427314   1.228331   1.626298

#   REMARKS: With a wind velocity of 10 m/s we obtain an estimated DC output of 1.427314 A.
#            The confidence interval of the response is [1.386768, 1.467861] and the slightly larger prediction interval is [1.228331, 1.626298].
#            On the other hand with a wind velocity of 1 m/s we obtain a negative estimated DC output which is useless.
#            The reason for this is that we are extrapolating outside the interval of the measured wind velocities.
#            In Prob. 7.5.2.d we calculated the minimal wind velocity necessary to produce a DC output which was considerably bigger than 1 m/s.


#--------------------------------------------------------------------------------------------------------------------------------------------------
