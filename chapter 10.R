#### 题目一 ####
# 生成数据
n <- 1000
x1 <- rnorm(n, 1, 2)
x2 <- rnorm(n, 12, 2)
x3 <- rnorm(n, 7 , 2)
u <- runif(n)
k1 <- as.integer(u <0.2) #vector of 0’s and 1’s
k2 <- as.integer(u>=0.2 & u<=0.5)
x <- k1 * x1 + k2 * x2 + (1-k1-k2)*x3#the mixture
hist(x, prob=TRUE, xlim=c(-10,20), ylim=c(0,0.15))

em_mixnorm <- function(x,seed=520){
set.seed(seed)
# 真实概率(0.2,0.3,0.5)
# 初始化
alpha10 <- 0.16
alpha20 <- 0.36
alpha30 <- 0.48
mu10 <- 1.2
mu20 <- 11.6
mu30 <- 6.8
sigma0 <- 2.3
para <- c(alpha10,alpha20,alpha30,mu10,mu20,mu30,sigma0)
tol <- 1e-8
para.old <- para+1
for (j in 1:1000) {
  vp10<-(para[1]*dnorm(x,para[4],para[7]))/(para[1]*dnorm(x,para[4],para[7])
                                            +para[2]*dnorm(x,para[5],para[7])
                                            +para[3]*dnorm(x,para[6],para[7]))
  vp20<-(para[2]*dnorm(x,para[5],para[7]))/(para[1]*dnorm(x,para[4],para[7])
                                            +para[2]*dnorm(x,para[5],para[7])
                                            +para[3]*dnorm(x,para[6],para[7]))
  vp30<-(para[3]*dnorm(x,para[6],para[7]))/(para[1]*dnorm(x,para[4],para[7])
                                             +para[2]*dnorm(x,para[5],para[7])
                                             +para[3]*dnorm(x,para[6],para[7]))
  
  phi1<-sum(vp10)
  phi2<-sum(vp20)
  phi3 <- sum(vp30)
  alpha11<-phi1/n
  alpha21<-phi2/n
  alpha31 <- phi3/n
  phix1<-sum(x*vp10)
  mu11<-phix1/phi1
  phix2<-sum(x*vp20)
  mu21<-phix2/phi2
  phix3 <- sum(x*vp30)
  mu31<-phix3/phi3
  
  phixmu1<-sum((x-mu11)^2*vp10)
  phixmu2<-sum((x-mu21)^2*vp20)
  phixmu3 <- sum((x-mu31)^2*vp30)
  sigma1 <- sqrt(sum(phixmu1,phixmu2,phixmu3)/sum(phi1,phi2,phi3))
  
  para<-c(alpha11,alpha21,alpha31,mu11,mu21,mu31,sigma1)
  
  if (sqrt(sum((para-para.old)^2))/sqrt(sum(para.old^2))<tol) break
  para.old<-para
  
}

data.frame(estimate=para,iter=j,tol=tol)
}

# bootstrap 估计标准差
B <- 200 #number of replicates
N <- length(x) #sample size
R <- matrix(0,nrow = B,ncol = 7) #storage for replicates
#bootstrap estimate of standard error of R
for (b in 1:B) {
  #randomly select the indices
  i <- sample(1:N, size = N, replace = TRUE)
 
   x <- x[i]
  result <- em_mixnorm(x)
  for (j in 1:7)R[b,j] <- result[j,1]
}
# 输出标准差的估计
print(em_mixnorm(x))
print(round(apply(R, 2, sd),3))


#### 题目二 ####
n <- 300
m <- 200
r <- 80
theta <- 1/3
t <- 1.5 # t的生成要根据r/m和theta决定的分位数函数
y <- rexp(n,theta)
theta0 <- 1
for (j in 1:3000) {
  ht <- exp(-t*theta0)/(1-exp(-t*theta0))
  theta1 <- (n+m)/(sum(y)+(m-r)*(t+1/theta0)+r*(1/theta0-t*ht)) #迭代公式
  theta0 <- theta1
}
print(theta0)
