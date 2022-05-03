#### 7-2 ####
# 这道题我用的是第二版的方法，可能和第一版方法算的略有差异
example_7_2 <- function(seed = 1012){
set.seed(1012)
# 加载所需函数
library(boot)
# 加载law数据集
library(bootstrap)
set.seed(1111)
r <- function(x, i) {
  #want correlation of columns 1 and 2
  cor(x[i,1], x[i,2])
}

boot.out <- boot(data = law,
                 statistic =r, R=2000)
# jackknife-after-bootstrap 估计 se(se)
A <- boot.array(boot.out)
theta.b <- boot.out$t
n <- NROW(law) # NROW和nrow的区别是前者生成的是一个n*1的矩阵
jack.se <- numeric(n)
for (i in 1:n) {
    #在第i次重复中，用x[i]省略所有样本
  keep <- which(A[, i] == 0)
  jack.se[i] <- sd(theta.b[keep])
}
print(boot.out) #for se_boot
se.bar <- mean(jack.se)
se.se <- sqrt((n-1) * mean((jack.se - se.bar)^2))
print(paste("Jackknife-after-bootstrap est. se(se)=", se.se))
}

example_7_2()
#### 7-3 ####
example_7_3 <- function(seed=1012){
set.seed(seed)
boot.t.ci <-
  function(x, B = 500, R = 100, level = .95, statistic){
    #compute the bootstrap t CI
    x <- as.matrix(x); n <- nrow(x)
    stat <- numeric(B); se <- numeric(B)
    boot.se <- function(x, R, f) {
      #local function to compute the bootstrap
      #estimate of standard error for statistic f(x)
      x <- as.matrix(x); m <- nrow(x)
      th <- replicate(R, expr = {
        i <- sample(1:m, size = m, replace = TRUE)
        f(x[i, ])
      })
      return(sd(th))
    }
    for (b in 1:B) {
      j <- sample(1:n, size = n, replace = TRUE)
      y <- x[j, ]
      stat[b] <- statistic(y)
      se[b] <- boot.se(y, R = R, f = statistic)
    }
    stat0 <- statistic(x)
    t.stats <- (stat - stat0) / se
    se0 <- sd(stat)
    alpha <- 1 - level
    Qt <- quantile(t.stats, c(alpha/2, 1-alpha/2), type = 1)
    names(Qt) <- rev(names(Qt))
    CI <- rev(stat0 - Qt * se0)
  }

dat <- cbind(law$LSAT, law$GPA)
stat <- function(dat){mean(cor(dat[,1],dat[,2]))}
ci <- boot.t.ci(dat, statistic = stat, B=2000, R=200)
print(ci)
}

example_7_3()
#### 7-4 ####
# 指数分布lambda的极大似然估计为n/sum(xi)
example_7_4 <- function(seed=1012){
set.seed(seed)
library(boot)
hours = aircondit$hours
n = length(hours)
B = 200
# MLE 
mle.lambda = function (values) {
  return(length(values)/sum(values))
}

lambda.hat = mle.lambda(hours)

lambda.hats.b = numeric(B)

for (b in 1:B) {
  i = sample(1:n, n, replace = TRUE)
  hours.b = hours[i]
  lambda.hats.b[b] = mle.lambda(hours.b)
}

lambda.hats.b.mean = mean(lambda.hats.b)
bias = lambda.hats.b.mean - lambda.hat
print(bias)
}

example_7_4()
#### 7-5 ####
example_7_5 <- function(seed=1012){
set.seed(1012)
library(boot) #for boot and boot.ci
dat <- aircondit$hours
theta.boot <- function(dat, ind) {
  #function to compute the statistic
  mean(dat[ind])
}
boot.obj <- boot(dat, statistic = theta.boot, R = 2000)
print(boot.obj)
print(boot.ci(boot.obj,
              type = c("basic", "norm", "perc",'bca')))
}

example_7_5()
# Bca方法产生了一个相对较大的区间，使用的方法不同，结果也不同，
# bootstrap t置信区间是二阶精度的，但不考虑变换。
# bootstrap百分位区间是一阶精度，但仅为一阶精度。
# 标准正态置信区间既不具有一阶精度也不具有二阶精度。
#### 7-7 ####
example_7_7 <- function(seed=1012){
set.seed(seed)
library(bootstrap)
sc<-scor
theta.boot<-function(x){ # 计算theta的函数
  sigma<-cov(x)
  pca.sigma<-prcomp(sigma) 
  theta<-pca.sigma$sdev[1]/sum(pca.sigma$sdev) # 获取主成分分析的主成分
  theta
}
n<-NROW(sc)
theta.b <- numeric(n)
for (b in 1:n){
  i = sample(1:n, size = n, replace = TRUE)
  theta.b[i]= theta.boot(sc[i,])
}
theta.hat <- theta.boot(sc)
return(list(Bias.boot=mean(theta.b)-theta.hat,
             SE.boot=sd(theta.b)))
}

example_7_7()
#### 7-8 ####
example_7_8 <- function(seed=1012){
set.seed(seed)
library(bootstrap)
sc<-scor
theta<-function(x){ # 计算theta的函数
  sigma<-cov(x)
  pca.sigma<-prcomp(sigma) 
  theta<-pca.sigma$sdev[1]/sum(pca.sigma$sdev) # 获取主成分分析的主成分
  theta
}
n<-NROW(sc)
theta.j<- numeric(n)
for (i in 1:n){    
  theta.j[i]<-theta(sc[-i,])
}
theta.hat<-theta(sc)
bias<-(n-1)*(mean(theta.j)-theta.hat) #BIAS
se<-sqrt((n-1)*var(theta.j)) #SE
round(c(bias,se),3)
return(list(Bias.jack=bias,
            SE.jack=se))
}

example_7_8()
#### 7-10 ####
example_7_10 <- function(seed=1012){
set.seed(1012)
# 拟合模型 并绘制拟合图像
par(mfrow=c(2,2))
library(DAAG,quietly=TRUE); attach(ironslag)
a <- seq(10, 40, .1)     #sequence for plotting fits
r<-numeric(4)  
L1 <- lm(magnetic ~ chemical)
plot(chemical, magnetic, main="Linear", pch=16)
yhat1 <- L1$coef[1] + L1$coef[2] * a
lines(a, yhat1, lwd=2)
r[1]<-summary(L1)$adj.r.squared

L2 <- lm(magnetic ~ chemical + I(chemical^2))
plot(chemical, magnetic, main="Quadratic", pch=16)
yhat2 <- L2$coef[1] + L2$coef[2] * a + L2$coef[3] * a^2
lines(a, yhat2, lwd=2)
r[2]<-summary(L2)$adj.r.squared

L3 <- lm(log(magnetic) ~ chemical)
plot(chemical, magnetic, main="Exponential", pch=16)
logyhat3 <- L3$coef[1] + L3$coef[2] * a
yhat3 <- exp(logyhat3)
lines(a, yhat3, lwd=2)
r[3]<-summary(L3)$adj.r.squared

L4 <- lm(magnetic ~ chemical + I(chemical^2)+ I(chemical^3))
plot(chemical, magnetic, main="Cubic", pch=16)
hat4 <- L4$coef[1] + L4$coef[2] * a + L4$coef[3] * a^2 + L4$coef[4] * a^3
lines(a, hat4, lwd=2)
r[4]<-summary(L4)$adj.r.squared

# 使用交叉验证选择最好的模型
n <- length(magnetic) 
e1 <- e2 <- e3 <- e4 <- numeric(n)
#对于n次交叉验证，在留一样本上拟合模型
for (k in 1:n) {
  y <- magnetic[-k]
  x <- chemical[-k]
  J1 <- lm(y ~ x)
  yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
  e1[k] <- magnetic[k] - yhat1

  J2 <- lm(y ~ x + I(x^2))
  yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] +
    J2$coef[3] * chemical[k]^2
  e2[k] <- magnetic[k] - yhat2

  J3 <- lm(log(y) ~ x)
  logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
  yhat3 <- exp(logyhat3)
  e3[k] <- magnetic[k] - yhat3

  J4 <- lm(y ~ x + I(x^2)+I(x^3))
  yhat4 <- J4$coef[1] + J4$coef[2] * chemical[k] +J4$coef[3] * chemical[k]^2 +J4$coef[3] * chemical[k]^3
  e4[k] <- magnetic[k] - yhat4
}

print(c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2)))
# CV表明，选择二次模型
print(r)
# adjust-R^2表明，选择二次模型
}

example_7_10()