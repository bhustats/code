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
# MLE of theta
theta_mle <- (sum(x) + (n-r)*max(x)) / r
theta_mle

#(b) MLE of theta
theta_mle <- (sum(x) + (60-10)*919)/10
t <- seq(0, 30000) # Time points
R_mle <- exp(-t/theta_mle) # MLE Reliability
S <- sum(x) + (60-10)*919 # UMVUE Reliability, # Formula for exponential Type-II censoring

R_umvue <- (1 - t/S)^(10-1)
R_umvue[t >= S] <- 0
plot(t, R_mle, type="l", lwd=2, ylim=c(0,1), xlab="Time", ylab="Rt")

#(c) Diff b/n MLE and UMVUE
diff <- abs(R_mle - R_umvue) # Difference
max_diff <- max(diff) # Maximum difference
max_diff
t_max <- t[which.max(diff)] # Time at maximum difference
t_max #Occurs at t