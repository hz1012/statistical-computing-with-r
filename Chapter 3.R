#### 3.3 ####
n <- 1000
u <- runif(n)
x <- 2/sqrt(1-u)
hist(x, prob = TRUE ,main = expression(f(x)==8*x^(1/3)),
     col = "skyblue",xlim = c(0,50)) #density histogram of sample
y <- seq(2, 30, 0.01)
lines(y, 8/(y^3),col="steelblue") #density curve f(x)

#### 3.4 ####
# 选取一系列sigma
sigma<-c(1,2,4,8,16,32)
for (i in 1:length(sigma)) {
  #设置种子以保证伪随机数的一致性
  set.seed(i)
  # 直方图的标题
  title<-c("Histogram of Rayleigh","parameter is",sigma[i])
  # 生成两个正态分布
  x<-rnorm(1000,0,sigma[i])
  y<-rnorm(1000,0,sigma[i])
  # 生成瑞利分布的随机数
  z<-sqrt(x^2+y^2)
  #绘制并检查
  hist(z,prob=TRUE,breaks = seq(0,6*sigma[i],length.out = 20)
       ,main = title,col = "skyblue")
  # 绘制Rayleigh 密度函数
  x1<-seq(0,6*sigma[i],length.out = 100000)
  y1<-(x1/sigma[i]^2)*exp(-(x1^2)/(2*sigma[i]^2))
  lines(x1,y1,col="steelblue")
}
  
#### 3.9 ####
n <- 1000 # 随机数个数  
y <- rbeta(n,2,2) #令Y=(X+1)/2，Y~Be(2,2),由Y产生随机数
x <- 2*y-1 #将Y产生的随机数结果回代
hist(x,  prob = TRUE,main = expression(f(x)==(3/4)(1-x^2)),ylim = c(0,0.8),
     col = "skyblue") #由此得到直方图
z <- seq(-1, 1, 0.01) #将结果进行拟合
lines(z, (3/4)*(1-z^2),col="steelblue")

n2 <- 1000  #通过题目给出的公式定义得到相同的结果图像并进行拟合
u <- vector(mode="numeric",length=1000)
for (i in 1:n2) {
  u1 <- runif(n2,-1,1)
  u2 <- runif(n2,-1,1)
  u3 <- runif(n2,-1,1)
  ifelse(abs(u3[i]) >= abs(u2[i]) && abs(u3[i]) >= abs(u1[i]),u[i] <- u2[i],
         u[i] <- u3[i])
}
hist(u, prob = TRUE, main = expression(f(x)==(3/4)(1-x^2)))
z <- seq(-1, 1, 0.01)
lines(z, (3/4)*(1-z^2))

#### 3.11 ####
mixturehist<-function(p1){
set.seed(1012)
p<-rbinom(1000,1,prob = p1)
x1<-rnorm(1000,3,1)
x2<-rnorm(1000)
x<-p*x1+(1-p)*x2 #Generate  samples
title<-paste('p1 = ',p1)
hist(x,probability = T,main = title,col = "skyblue",ylim=c(0,0.4))
y<-seq(-3,6,0.1)
densityplot<-function(x){
  p1*dnorm(x)+(1-p1)*dnorm(x,3,1)
  }
lines(y,densityplot(y),col='steelblue')
}
mixturehist(0.75)
for (p1 in seq(0.1,0.9,0.1)){
  mixturehist(p1)
}

#### 3.13 ####
n <- 1000
u <- runif(n)
x <- (2/(1-u)^(1/4))-2 
hist(x, prob = TRUE, main = bquote(f(x)==64/(x+2)^5),col = "skyblue") # 求导得密度函数
y <- seq(0, 20, 0.01) # 得到密度函数曲线
lines(y, 64/(y+2)^5,col="steelblue")  

#### 3.14 ####
mu=c(0,1,2)
Sigma=matrix(c(1,-.5,.5,-.5,1,-.5,.5,-.5,1),ncol = 3,byrow = F)
rmvn.Choleski <-
  function(n, mu, Sigma) {
    # generate n random vectors from MVN(mu, Sigma)
    # dimension is inferred from mu and Sigma
    d <- length(mu)
    Q <- chol(Sigma) # Choleski factorization of Sigma
    Z <- matrix(rnorm(n*d), nrow=n, ncol=d)
    X <- Z %*% Q + matrix(mu, n, d, byrow=TRUE)
    X
  }

X <- rmvn.Choleski(200, mu, Sigma)
# 绘图
pairs(X)

#### 3.16 ###
library(bootstrap)# 没有就先下载
cov(scale(scor[,1:2]))
cov(scale(scor[,3:5]))

#### 3.20 ####
# shape：Gamma分布形状参数；scale:Gamma分布尺度参数
comp_poss <- function(lambda, shape, scale,size = 1000 ,t = 10) {
  # 到达间隔时间随速率λ呈指数分布。  
  pp.exp = function (t0) {
    Tn = rexp(1000, lambda)
    Sn = cumsum(Tn)
    return(min(which(Sn > t0)) - 1)
  }
  
  # 生成服从泊松分布的N（t）
  ns = replicate(size, expr={ pp.exp(t)})
  # 生成题目描述的X(t)
  xs = sapply(ns, function (n) {
    ys = c(rgamma(n = n, shape = shape, scale = scale))
    sum(ys[1:n])
  })
  # 计算模拟值和理论值的差别
  # 样本
  mean.s = mean(xs)
  var.s = var(xs)
  
  # 理论
  mean.t = lambda * t * shape * scale
  var.t = (shape + 1) * shape * scale^2*lambda*t
  df = matrix(c(mean.s,mean.t,var.s,var.t),ncol = 4,
              dimnames = list(c("value"),c("mean.s","mean.t","var.s","var.t")))
  return(df)
}
comp_poss(4,5,6)