x<-c(0.35,0.59,0.96,0.99,1.69,1.97,2.07,2.58,2.71,2.90,3.67,3.99,5.35,13.77)
x=sort(x)
n=length(x)
i=seq(1,n)
e_cdf=(i-0.5)/n
d=data.frame(x,i,e_cdf)
y=log(-log(1-e_cdf))
x1=log(x)
slr=lm(y~x1)
summary(slr)
plot(x1,y)
abline(slr)

pp=0
diff=1
i=0
p=2
while(diff>0.00001)
{ 
  p0=p
  sigma=sum(x^p0)/n
  
  p=n/(sum((x^p0)*log(x))/sigma-sum(log(x)))
  diff=abs(p0-p)
  i=i+1
  pp[i]=p 
}
p
sigma=sum(x^p)/n;sigma
i


plot(seq(1,i),pp,xlab="number of iteration",ylab="value of p ")

A=-n/(p^2)-sum((x^p)*log(x)^2)/sigma;A
B=C=sum((x^p)*log(x))/(sigma^2);B
D=n/sigma^2-2*sum(x^p)/(sigma^3);D

I=-matrix(c(A,B,C,D),nrow=2)
I

inv_I=solve(I);inv_I
conf_p=p+c(-1,1)*qnorm(0.975)*sqrt(inv_I[1,1]);conf_p
conf_sigma=sigma+c(-1,1)*qnorm(0.975)*sqrt(inv_I[2,2]);conf_sigma

t<-seq(0,30,0.1)
plot(t,exp(-t^p/sigma),xlab="time",ylab="Reliability",type="l")
plot(t,p*t^(p-1)/sigma,type="l")