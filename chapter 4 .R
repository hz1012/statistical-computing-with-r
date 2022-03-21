#### 5-3 ####
m  <-  1e4
# 均匀分布取样
estimate.unif <- function () {
  g <- function (y) {
    exp(-y)
  }
  xs <- g(runif(m, 0, 0.5))
  var <- var(xs)/m # 计算方差
  
  theta.hat <- mean(xs) * 1/2
  return(data.frame(theta.hat, var))
}
# 指数分布取样
estimate.exp <- function () {
  # theta.hat <- pexp(0.5, rate <- 1) - pexp(0, rate <- 1)<-pexp(0.5,rate<-1)
  y <- rexp(m, rate <- 1) <= 0.5
  var <- var(y)/m # 注意在对逻辑数据做数值运算时，R默认将其转换为01
  
  theta.hat <- mean(y)
  return(data.frame(theta.hat, var))
}

estimate.unif()
estimate.exp()
# 指数分布抽样方差更高 这里用的是SSE

#### 5-6，5-7 ####
m <- 1e3

estimate.5_7 <- function(){
x <- runif(m, min=0, max=1)
theta.hat <- mean(exp(x)) # the simple Monte Carlo method
x1 <- runif(m/2, min=0, max=1)
theta_.hat <- (mean(exp(x1))+mean(exp(1-x1)))/2 # antithetic variate approach
true <- exp(1)-1
c(theta.hat,theta_.hat,true)
}
estimate.5_7()

# 估计方差减小 ESE
res<-sapply(1:1000,FUN = function(o){estimate.5_7()})
# 注：sapply函数的封装效果似乎并不好，这里添加的funciton(o){}只是为了显示识别。
var(res[1,])
var(res[2,])
(var(res[1,])-var(res[2,]))/var(res[1,])

#### 5-13 ####
# 这个寻找方法有很多，不一定用我的
library(bayesmeta) # 这个包可以生成瑞利分布
g = function (x) {
  x ^ 2 / sqrt(2*pi) * exp(-x^2/2)
}

xs = seq(0,10,0.1)

ys.g = g(xs)
ys.rayleigh = drayleigh(xs, scale = 1.5)
ys.norm = dnorm(xs, mean = 1.5)
lim = max(c(ys.g, ys.rayleigh, ys.norm))

plot(xs, ys.g, type = "l", ylim = c(0, lim))
lines(xs, ys.rayleigh, col="red", ylim = c(0, lim))
lines(xs, ys.norm, col="blue", ylim = c(0, lim))

# f1(x) = drayleigh(x, scale = 1.5)
# f2(x) = dnorm(x, mean = 1.5)

# f2 距离g更近，效果应该更好

#### 5-14 ####
# 做着玩的 熟悉一下重要抽样的流程
M <- 1e4

g = function (x) {
  x ^ 2 / sqrt(2*pi) * exp(-x^2/2) * (x > 1)
}

f1 = function (x) {
  drayleigh(x, scale = 1.5) * (x > 1)
}

f2 = function (x) {
  dnorm(x, mean = 1.5) * (x > 1)
}

rf1 = function () {
  rrayleigh(M, scale = 1.5)
}

rf2 = function () {
  rnorm(M, mean = 1.5)
}

is.rayleigh = function () {
  xs = rf1()
  return(mean(g(xs)/f1(xs), na.rm = TRUE))  
}

is.norm = function () {
  xs = rf2()
  return(mean(g(xs)/f2(xs), na.rm = TRUE))  
}

(theta1 = is.rayleigh())
(theta2 = is.norm())

#### 5-15 ####
M <- 1e4
k <- 5 #分成五个区间
r <- M/k 
N <- 50 #重复估计的次数
T2 <- numeric(k) #储存每个区间抽样的结果
est <- matrix(0, N, 2)
g<-function(t)(1-exp(-1))/(1+t^2)*(t>0)*(t<1)#g(x)/f(x)=(1-exp(-1))/(1+x^2)
for (i in 1:N) {
  est[i, 1] <- mean(g(runif(M)))
  for(j in 1:k)T2[j]<-mean(g(runif(r,(j-1)/k,j/k))) #分层体现在这一步
  est[i, 2] <- mean(T2)
}
apply(est,2,mean)#分层重要抽样法的适用范围更广
apply(est,2,var)#example 5.10 result:6.504485e-08
# 对比可见分层重要抽样方差减少更多