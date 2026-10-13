x<-c(0.35,0.59,0.96,0.99,1.69,1.97,2.07,2.58,2.71,2.90,3.67,3.99,5.35,13.77)
n<-length(x)
sum_x<-sum(x)
mean_x<-sum_x/n
sumlog_x<-sum(log(x))
p<-1/(2*(log(mean_x)-(sumlog_x/n)))
p
theta<-mean_x/p
theta
f_th<-function(p){
  -n*log(mean_x)+n*log(p)-n*digamma(p)+sumlog_x
}
f_p<-function(p){
  -(n/p)-n*trigamma(p)
}

shape<-rep()
for(i in 1:100){
  p_1=p-(f_th(p)/f_p(p))
  shape[i]=p_1
  p=p_1
}
plot(shape, type = "l",
     col = "royalblue",
     lwd = 2,
     xlab = "Iteration",
     ylab = "Shape estimate",
     main = "Newton-Raphson Convergence")
p=p_1
p
theta=mean_x/p
theta
t = seq(0,20,.1)
R_t = pgamma(t,shape=p,scale=theta,lower.tail = FALSE)
print(head(R_t))
plot(t,R_t,type="l", lwd=2,lty=14,
     col="royalblue",
     xlab="Time(in minutes)",
     ylab="Reliability",
     main="MLE of Reliability"
)