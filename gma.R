x=c(0.35,0.59,0.96,0.99,1.69,1.97,2.07,2.58,2.71,2.90,3.67,3.99,5.35,13.77)
n=length(x)

p_cap=1/(2*(log(mean(x))-1/n*sum(log(x))))
p_cap
sigma_cap=mean(x)/p_cap;sigma_cap

diff=1
p0=0
p1=p_cap
i=0
f1=function(p){-n*log(mean(x))+n*log(p)-n*digamma(p)+sum(log(x))}
f2=function(p){n/p-n*trigamma(p)}
while(diff>0.0001)
{ p0=p1
p1= p0-f1(p0)/f2(p0)
diff=abs(p0-p1)
i=i+1
}
i
p1
sigma=mean(x)/p1
R=function(t){1-pgamma(t,p1,sigma)}
t<-seq(0,5,0.001)
plot(t,R(t),type="l")

#OR for gamma
x <- c(0.35,0.59,0.96,0.99,1.69,1.97,2.07,
       2.58,2.71,2.90,3.67,3.99,5.35,13.77)

n <- length(x)
x_mean <- mean(x)
A <- log(x_mean) - mean(log(x))

# Initial value of alpha
alpha <- 0.5/A

# Newton-Raphson Method
for(i in 1:10)
{
  f  <- log(alpha) - digamma(alpha) - A
  fp <- 1/alpha - trigamma(alpha)
  
  alpha <- alpha - f/fp
  alpha
}

# Estimate of beta
beta <- x_mean/alpha
beta

t <- seq(0,20,0.1)

R <- 1 - pgamma(t,
                shape=alpha,
                scale=beta)

plot(t,R,
     type="l",
     col="blue",
     lwd=2,
     xlab="Time",
     ylab="Reliability",
     main="Reliability Curve")
