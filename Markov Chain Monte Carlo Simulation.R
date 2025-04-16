s0=matrix(c(0.21,0.68,0.11),nrow=1)
s0
P=matrix(c(0.65,0.28,0.07,0.15,0.67,0.18,0.12,0.36,0.52),
         ncol = 3, byrow = T)
P
##checking whether p sums to 1
apply(P,1,sum)

s0=matrix(c(0.21,0.68,0.11),nrow=1)
out<-NULL
for(iter in 1:15){
  s0<-s0%*%P
  out<-rbind(out,c(iter, s0))
}
out
colnames(out)<-c("iter", "lower","middle","upper")
out

## Using MonterCarlo Integration 
M<-10000
beta.sims<-rbeta(M,3,3)
sum(beta.sims)/M


## Gibs Sampler 
y <- c(5, 1, 5, 14, 3, 19, 1, 1, 4, 22)
t <- c(94, 16, 63, 126, 5, 31, 1, 1, 2, 10)
rbind(y, t)
beta.cur <- 1
lambda.update <- function(alpha, beta, y, t) {
  + rgamma(length(y), y + alpha, t + beta)
  + }
beta.update <- function(alpha, gamma, delta, lambda, y) {
  + rgamma(1, length(y) * alpha + gamma, delta + sum(lambda))
  + }
beta.draws<-c()
lambda.draws<-matrix(NA,nrow=nsims,ncol=length(y))
beta.cur<-beta.start
lambda.update<-function(alpha,beta,y,t){
  rgamma(length(y),y+alpha,t+beta)
}

