
wait <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  for(j in 1:100) cat("")
  proc.time() - p1 # The cpu usage should be negligible
}





## 18 ones and 12 zeroes
k = 18
n = 30
## x-axis for plotting
numSteps = 200
x = seq(0, 1, 1 / numSteps)

## Likelihood function
L = x^k * (1 - x)^(n - k)

## Just normalize likelihood to integrate to one (for purposes of plotting)
L = L / sum(L) * numSteps

#vary the prior
for(a in seq(0.5,10,by=0.5)){
  for(b in seq(10,0.5,by=-0.5)){
## Plot likelihood

plot(x, L, type = 'l', lwd = 3, ylim = c(0,6),
     main = paste0("Bernoulli Likelihood with Beta(",a,", ",b,") Prior"),
     xlab = expression(theta), ylab = "pdf")

    
## Plot Beta(1,1) prior
lines(x, dbeta(x, a, b), lwd = 3, col = "blue")

## Plot posterior
lines(x, dbeta(x, k + a, n - k + b), lwd = 3, col = "red")

legend("topright", c("Likelihood", "Prior", "Posterior"),
    lwd = 3, col = c("black", "blue", "red"))
wait(.5)
  }
}



## 18 ones and 12 zeroes
k = 18
n = 30

## x-axis for plotting
numSteps = 200
x = seq(0, 1, 1 / numSteps)

## Likelihood function
L = x^k * (1 - x)^(n - k)

## Just normalize likelihood to integrate to one (for purposes of plotting)
L = L / sum(L) * numSteps


### Uniform Prior
## Plot likelihood

plot(x, L, type = 'l', lwd = 3, ylim = c(0,6),
     main = "Bernoulli Likelihood with Beta(1,1) Prior",
     xlab = expression(theta), ylab = "pdf")

## Plot Beta(1,1) prior
lines(x, dbeta(x, 1, 1), lwd = 3, col = "blue")

## Plot posterior
lines(x, dbeta(x, k + 1, n - k + 1),  lwd = 3, col = "red")

legend("topright", c("Likelihood", "Prior", "Posterior"),
       lwd = 3, col = c("black", "blue", "red"))



### Beta(5, 5) Prior
plot(x, L, type = 'l', lwd = 3, ylim = c(0,6),
     main = "Bernoulli Likelihood with Beta(2,2) Prior",
     xlab = expression(theta), ylab = "pdf")

## Plot Beta(1,4) prior
lines(x, dbeta(x, 5, 5),  lwd = 3, col = "blue")

## Plot posterior
lines(x, dbeta(x, k + 5, n - k + 5), lwd = 3, col = "red")

legend("topright", c("Likelihood", "Prior", "Posterior"),
       lwd = 3, col = c("black", "blue", "red"))


### Beta(10, 10) Prior, but increase n by 10
## 180 ones and 120 zeroes
k = 180
n = 300

## x-axis for plotting
numSteps = 200
x = seq(0, 1, 1 / numSteps)

## Likelihood function
L = x^k * (1 - x)^(n - k)

## Just normalize likelihood to integrate to one (for purposes of plotting)
L = L / sum(L) * numSteps
plot(x, L, type = 'l', lwd = 3, ylim = c(0,15),
     main = "Bernoulli Likelihood with Beta(2,2) Prior",
     xlab = expression(theta), ylab = "pdf")

## Plot Beta(1,4) prior
lines(x, dbeta(x, 5, 5),  lwd = 3, col = "blue")

## Plot posterior
lines(x, dbeta(x, k + 5, n - k + 5), lwd = 3, col = "red")

legend("topright", c("Likelihood", "Prior", "Posterior"),
       lwd = 3, col = c("black", "blue", "red"))


