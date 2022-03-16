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
res<-sapply(1:1000,FUN = function(l){estimate.5_7()})
# 注：sapply函数的封装效果似乎并不好，这里添加的funciton(o){}只是为了显示识别。
var(res[1,])
var(res[2,])
(var(res[1,])-var(res[2,]))/var(res[1,])
