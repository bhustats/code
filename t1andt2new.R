#1 Type 1 For Expo Distn
n<-150
to<-800
m<-10
x<-c(50,60,80,150,180,190,250,320,400,480)
sum_x<-sum(x)
sum_x
total<-sum_x + (n-m)*to
total
theta<-total/m
theta
t<-seq(0,50000)
Rt<-exp(-t/theta)
plot(t,Rt,lty=1,type="l")


#Type 2 Expo Distn
#(a) Failure times
x <- c(90,156,285,381,497,525,628,720,825,919)
n <- 60      # total items
r <- 10      # failed items observed
total<-sum(x)+(n-r)*max(x)
total
theta <- total/r # MLE of theta
theta
t <- seq(0, 30000) # Time points
Rt <- exp(-t/theta) # MLE Reliability # UMVUE Reliability, # Formula for exponential Type-II censoring
umvue <- (1 - t/total)^(r-1)
plot(t, Rt, type="l", lty=1, ylim=c(0,1), xlab="Time", ylab="Rt")
lines(t,umvue,lty=2,type="l")

#(c) Diff b/n MLE and UMVUE
diff <- abs(Rt - umvue) # Difference
max_diff <- max(diff) # Maximum difference
max_diff
t_max <- t[which.max(diff)] # Time at maximum difference
t_max #Occurs at t