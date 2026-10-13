x=c(0.35, 0.59, 0.96, 0.99, 1.69, 1.97, 2.07, 2.58,
    2.71, 2.90, 3.67, 3.99, 5.35, 13.77)
x=sort(x)
n=length(x)
i=1:n
e_cdf=(i-0.5)/n
d=data.frame(x,i,e_cdf)
y=log(-log(1-e_cdf))
x1=log(x)
slr=lm(y~x1)
summary(slr)
plot(x1,y)
abline(slr)

# MLE
# Fixed point iteration
p=2
f_th=function(p){
  sum(x^p)/n
}
f_p=function(p){
  n/(sum((x^p)*log(x))/f_th(p)-sum(log(x)))
}
pp=rep(0)
for(i in 1:200){
  pp[i]=f_p(p)
  p=pp[i]
}
plot(pp,type="l")
p=pp[200]; p
theta=f_th(p); theta

# Asymptotic confidence interval
a11=n/theta^2-(2/(theta^3))*sum(x^p); a11
a12=a21=sum((x^p)*log(x))/(theta^2); a12
a22=-n/(p)^2-sum((x^p)*(log(x))^2)/theta; a22

inf=matrix(c(-a11,-a21,-a12,-a22),byrow=T,nrow=2)
var_cov=solve(inf)

conf_th=theta+c(-1,1)*qnorm(.975)*sqrt(var_cov[1,1]); conf_th
conf_p=p+c(-1,1)*qnorm(.975)*sqrt(var_cov[2,2]); conf_p

# Plot
t=seq(0,20,0.5)
R_t=exp(-(t^p)/theta)
plot(t,R_t,type="l",lty=2,col="red",
     ylab="R(t)",main="Reliability curve")

h_t=(p/theta)*(t^(p-1))
plot(t,h_t,type="l",ylab="h(t)",
     main="Hazard rate curve")