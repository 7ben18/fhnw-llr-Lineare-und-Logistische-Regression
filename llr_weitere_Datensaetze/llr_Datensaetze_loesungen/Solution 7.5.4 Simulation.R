#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Part II: Multiple Regression
#   "Solution 7.5.4 Simulation.R"
#   Author: Marcel Steiner-Curtis
#   Date: 21.08.2018    sml
#--------------------------------------------------------------------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Solution 7.5.4 Simulation
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   create data without measurement errors but with random x-values
set.seed(1)
n <- 20
x <- runif(n, min=0, max=50)
beta0 <- 5
beta1 <- 3
y <- beta0+beta1*x


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (a) Simulation of the Different Straight Lines with Graphic
#--------------------------------------------------------------------------------------------------------------------------------------------------
sigma <- 5
set.seed(1)
for(i in 1:100){
    E <- rnorm(n, mean=0, sd=sigma)
    Y <- y + E
    #   estimation
    mod <- lm(Y ~ x)
    #   graphic
    plot(x, Y, pch=20, xlim=c(0,50), ylim=c(0,160), main="Data Set with Simulated Errors")
    abline(v=x, lty=3, col="gray")
    abline(a=5, b=3)
    abline(mod, col="red")
    dev.flush()
    Sys.sleep(0.2)
}


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (b) Simulation with 100 Replications
#--------------------------------------------------------------------------------------------------------------------------------------------------
beta0.sim <- NULL
beta1.sim <- NULL
set.seed(1)
for(i in 1:100){
    E <- rnorm(n, mean=0, sd=sigma)
    Y <- y + E
    #   estimation
    mod <- lm(Y ~ x)
    beta0.sim[i] <- coefficients(mod)[1]
    beta1.sim[i] <- coefficients(mod)[2]
}

#   distributions of beta0 and beta1 with theoretical distributions
op <- par(mfrow=c(1,2))
    #   intercept
    sd0.theory <- sigma*sqrt(1/n+mean(x)^2/sum((x-mean(x))^2))
    hist(beta0.sim, xlab="beta0", freq=F, ylim=c(0,0.2), breaks=seq(-10,15,by=1), xlim=range(pretty(beta0.sim)), col="gray")
    abline(v=mean(beta0.sim)+sd(beta0.sim)*c(-1,0,1), col="blue", lwd=2)
    abline(v=beta0+sd0.theory*c(-1,0,1), col="red")
    curve(dnorm(xi, mean=beta0, sd=sd0.theory),
          xname="xi", from=range(pretty(beta0.sim))[1], to=range(pretty(beta0.sim))[2], n=1001, col="red", add=T)

    #   slope
    sd1.theory <- sqrt(sigma^2/sum((x-mean(x))^2))
    hist(beta1.sim, xlab="beta1", freq=F, ylim=c(0,6), breaks=seq(2,4,by=0.05), xlim=range(pretty(beta1.sim)), col="gray")
    abline(v=mean(beta1.sim)+sd(beta1.sim)*c(-1,0,1), col="blue", lwd=2)
    abline(v=beta1+sd1.theory*c(-1,0,1), col="red")
    curve(dnorm(xi, mean=beta1, sd=sd1.theory),
          xname="xi", from=range(pretty(beta1.sim))[1], to=range(pretty(beta1.sim))[2], n=1001, col="red", add=T)
par(op)


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (c) Simulation with 10000 Replications
#--------------------------------------------------------------------------------------------------------------------------------------------------
beta0.sim <- NULL
beta1.sim <- NULL
set.seed(1)
for(i in 1:10000){
    E <- rnorm(n, mean=0, sd=sigma)
    Y <- y + E
    #   estimation
    mod <- lm(Y ~ x)
    beta0.sim[i] <- coefficients(mod)[1]
    beta1.sim[i] <- coefficients(mod)[2]
}

#   distributions of beta0 and beta1 with theoretical distributions
op <- par(mfrow=c(1,2))
    #   intercept
    sd0.theory <- sigma*sqrt(1/n+mean(x)^2/sum((x-mean(x))^2))
    hist(beta0.sim, xlab="beta0", freq=F, ylim=c(0,0.2), breaks=seq(-10,15,by=1), xlim=range(pretty(beta0.sim)), col="gray")
    abline(v=mean(beta0.sim)+sd(beta0.sim)*c(-1,0,1), col="blue", lwd=2)
    abline(v=beta0+sd0.theory*c(-1,0,1), col="red")
    curve(dnorm(xi, mean=beta0, sd=sd0.theory),
          xname="xi", from=range(pretty(beta0.sim))[1], to=range(pretty(beta0.sim))[2], n=1001, col="red", add=T)

    #   slope
    sd1.theory <- sqrt(sigma^2/sum((x-mean(x))^2))
    hist(beta1.sim, xlab="beta1", freq=F, ylim=c(0,6), breaks=seq(2,4,by=0.05), xlim=range(pretty(beta1.sim)), col="gray")
    abline(v=mean(beta1.sim)+sd(beta1.sim)*c(-1,0,1), col="blue", lwd=2)
    abline(v=beta1+sd1.theory*c(-1,0,1), col="red")
    curve(dnorm(xi, mean=beta1, sd=sd1.theory),
          xname="xi", from=range(pretty(beta1.sim))[1], to=range(pretty(beta1.sim))[2], n=1001, col="red", add=T)
par(op)

#   REMARKS: The more simulations are carried out, the closer are the expected values calculated from the simulation results
#            to the theoretical values.
#            This has to be this way: if we repeat the simulation infinitely often, then the theoretical value would come out.


#--------------------------------------------------------------------------------------------------------------------------------------------------
