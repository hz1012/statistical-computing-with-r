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
M<-1e4
N<-1000
k<-5
# 先利用逆变换方法生成密度函数为f_k（x）的随机数
# 注意这里的(a,b)对应分的不同层的密度函数
inv_fun<-function(n,a,b){  
u<-runif(n)
x<--log(exp(-a)-(exp(-a)-exp(-b))*u)
x
} 
 
res3<-sapply(1:N,FUN = function(o){
x<-inv_fun(M,0,1)
M1<-mean((1-exp(-1))/(1+x^2)) #重要抽样
M2<-numeric(k)
for (j in 0:(k-1)){
 a<-j/k
 b<-(j+1)/k
 xj<-inv_fun(M/k,a,b) #生成随机数
 M2[j+1]<-mean((exp(-a)-exp(-b))/(1+xj^2)) #分层重要抽样，注意此时f/g的形式
}
 c(M1,sum(M2))
})

c(var(res3[1,]),var(res3[2,])) #SSE方法估计方差
