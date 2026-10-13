#(a)
temp_matrix=matrix(c(
  -0.28,-0.33,-0.19,-0.20,-0.08, 0.06, 0.08,-0.10, 0.09, 0.39,
  -0.21,-0.33,-0.39,-0.19,-0.19,-0.05, 0.06, 0.00, 0.05, 0.56,
  -0.26,-0.25,-0.33,-0.14,-0.07, 0.00,-0.01, 0.14, 0.12, 0.32,
  -0.27,-0.14,-0.35,-0.26,-0.12,-0.04, 0.07,-0.08, 0.26, 0.33,
  -0.32,-0.11,-0.33,-0.22,-0.05,-0.07, 0.04,-0.05, 0.31, 0.48,
  -0.32,-0.25,-0.34,-0.22, 0.07,-0.16, 0.08,-0.16, 0.19, 0.56,
  -0.29,-0.15,-0.32,-0.17, 0.10,-0.04,-0.21, 0.12, 0.37, 0.55,
  -0.36,-0.07,-0.30,-0.02, 0.01, 0.03,-0.11, 0.01, 0.35, 0.48,
  -0.27,-0.14,-0.15,-0.15, 0.04, 0.11,-0.03, 0.08, 0.12, 0.62,
  -0.17,-0.24,-0.10,-0.12, 0.10,-0.10,-0.01, 0.18, 0.13, 0.54,
  -0.39,-0.30,-0.30,-0.26, 0.03,-0.10,-0.04, 0.26, 0.23, 0.57,
  -0.27,-0.34,-0.39,-0.08, 0.09,-0.17, 0.08, 0.04, 0.37, 0.43,
  -0.32,-0.24,-0.33,-0.02, 0.19, 0.08, 0.03, 0.26, 0.29, 0.57
), nrow=13, ncol=10, byrow= TRUE)
x= as.vector(temp_matrix)
n= length(x)
t=1:n
plot(t,x,type="o",
     main="Global Temperature Data",xlab="time",ylab="Temperature")
#(b)
ma_5=stats::filter(x,rep(1/5, 5), sides=2)
plot(t,x,type="l",col="grey60",
     main="Moving Average(order 5)",xlab="time",ylab="temperature")
lines(t,ma_5,col="red",lwd=2)
legend("topleft",legend=c("original","MA(5)"),
       col=c("grey60","red"),lty=1,lwd=c(1,2))
#(c)
fit=lm(x~t)
a=coef(fit)[1]
b=coef(fit)[2]
cat("a =",a,"\n")
cat("b =",b,"\n")
m_hat=a+b*t
plot(t,x,type="l",main="Data with fitted straight line",
     xlab="time",ylab="Temperature")
lines(t,m_hat,col="blue",lwd=2)
#(d)
res=x-m_hat
plot(t,res,typ="o",pch=16,cex=0.6,
     main="Residual Plot",xlab="time",ylab="Residual")
abline(h=0,col="red",lty=2)
acf(res,main="ACF of residual")
Box.test(res,lag=10,type="Ljung-Box")
#(e)
t_new=(n+1):(n+10)
forecast=a+b*t_new
print(data.frame(t=t_new,forecast=forecast))
plot(c(t,t_new),c(x,rep(NA,10)),type="l",
     xlim=c(1,n+10),ylim=range(c(x,forecast)),
     main="10-step forecast",xlab="time",ylab="Temperature")
lines(t_new,forecast,col="red",lwd=2)