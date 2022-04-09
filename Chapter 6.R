#### 6-3 ####
example_7_9 <- function(n){
m <- 1000
mu0 <- 500
sigma <- 100
mu <- c(seq(450, 650, 10)) #alternatives
M <- length(mu)
power <- numeric(M)
for (i in 1:M) {
  mu1 <- mu[i]
  pvalues <- replicate(m, expr = {
    #simulate under alternative mu1
    x <- rnorm(n, mean = mu1, sd = sigma)
    ttest <- t.test(x,
                    alternative = "greater", mu = mu0)
    ttest$p.value } )
  power[i] <- mean(pvalues <= .05)
}
se <- sqrt(power * (1-power) / m)
df <- data.frame(mean=mu, power=power,
                 upper=power+2*se, lower=power-2*se,n=factor(n))
return(df)
}
# 封装成一个函数


data <- data.frame()
for (n in seq(20,50,10)) {
  df <- example_7_9(n)
  data <- rbind(df,data) 
}
# 将不同取值n的df合并成一个data

library(ggplot2)

ggplot(data, aes(x=mean, y=power,col=n)) +
  geom_line() +
  geom_vline(xintercept=500, lty=2) +
  geom_hline(yintercept=c(0,.05), lty=1:2) +
  theme_light() # 主题可以自己改我喜欢用这个
 # 利用ggplot2的颜色映射概念自动生成图例(没学过ggplot2包的可以看看R数据科学)


#### 6-4 ####
exercise_6_4 <- function(seed=123){
set.seed(seed)
n <- 20 # X服从对数正态分布，Y=ln(x)~N(μ,σ^2)，所以可以直接利用Y进行估计，再代入X即可
alpha <- .05
m <- 1000
cv.t<-sapply(1:m,FUN= function(o){
  y<-rnorm(n,0,2)  
  m<-mean(y) # estimate of mean
  se<-sqrt(var(y)) # estimate of standard error
  as.numeric((m-c*se/sqrt(n)<0)&(m+c*se/sqrt(n)>0)) # ci
})
level <- mean(cv.t) # mean of  Monte Carlo experiment

return(data.frame(level=level))
}
exercise_6_4()

#### 6-5 ####
exercise_6_5 <- function(seed=123){
set.seed(seed)
n<-20
c<-qt(0.975,n-1) # 0.975 quantile of t-distribution
m <- 1000
cv.t<-sapply(1:m,FUN= function(o){
x<-rchisq(n,2)  # 注意这里的x是从卡方分布取样
m<-mean(x) # estimate of mean
se<-sqrt(var(x)) # estimate of standard error
as.numeric((m-c*se/sqrt(n)<2)&(m+c*se/sqrt(n)>2)) # ci
})
level1 <- mean(cv.t) # mean of  Monte Carlo experiment


#v我们可以得出概率不等于0.95,小于0.95,example6.4使用卡方分布来估计方差（真值为4)
alpha <- .05
UCL <- replicate(1000, expr = {
x <- rchisq(n,2)
(n-1) * var(x) / qchisq(alpha, df = n-1)
} )
#计算包含sigma^2=4的区间数
level2 <- sum(UCL > 4)/m
return(data.frame(level1,level2))
}

exercise_6_5(1012)
# 我们可以看到结果远小于0.95，因此t-区间更稳健

#### 6-8 ####
count5test <- function(x,y){
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  return(as.integer(max(c(outx,outy)) > 5))
}
n <- c(20,200,1000)#分别对应小样本、中样本和大样本
mu1 <- mu2 <- 0
sigma1 <- 1
sigma2 <- 1.5
m <- 10000
power1 <- power2 <- numeric(length(n))
set.seed(1234)
for(i in 1:length(n)){
  power1[i] <- mean(replicate(m,expr = {
    x <- rnorm(n[i],mu1,sigma1)
    y <- rnorm(n[i],mu2,sigma2)
    x <- x - mean(x)
    y <- y - mean(y)
    count5test(x,y)
  }))
  pvalues <- replicate(m,expr={
    x <- rnorm(n[i],mu1,sigma1)
    y <- rnorm(n[i],mu2,sigma2)
    Ftest <- var.test(x, y, ratio = 1,
                      alternative = c("two.sided", "less", "greater"),
                      conf.level = 0.945, ...)
    Ftest$p.value})
  power2[i] <- mean(pvalues<=0.055)
}
help("var.test")
power1
power2

#### 6-9 ####
exercise_6_9 <- function(distribution=c('rlnorm','uniform','Bernoulli')){
n <- 20
size <- 1000
ginifun <- function()
{
  if (distribution == 'rlnorm')x <- sort(rlnorm(n))
  else if(distribution == 'uniform') x <- sort(runif(n,0,1))
  else x <- sort(rbinom(n,size = 100,prob = .1))
  m=mean(x)
  sum=0
  for (k in n) {
    t=(2*k-n-1)*x[k]
    sum=sum+t
  }
  gini=sum/(n^2*m)
}
gini1 <- ginifun()
res <- replicate(size,expr = ginifun())
hist(as.numeric(res), prob = TRUE, main = distribution)
return(data.frame(mean = mean(res),median = median(res),quantile = quantile(res,seq(.1,.9,.1))))
}
exercise_6_9(distribution = 'rlnorm')
exercise_6_9(distribution = 'uniform')
exercise_6_9(distribution = 'Bernoulli')

#### 6-A ####
exercise_6_A <- function(seed){
set.seed(123)
num<-c(50,100,200,500,1000) # Estimate the Type-I error for different sizes.
m<-10000
 
er<-NULL
for (n in num){
  cv<-qt(0.975,n-1)
  er1<-mean(sapply(1:m,FUN = function(o){
  x<-rchisq(n,1)
  m<-mean(x)
  se<-sqrt(var(x))
  abs((m-1)*sqrt(n)/se)>=cv
}))  # 估计卡方分布的第一类错误
  er2<-mean(sapply(1:m,FUN = function(o){
  x<-runif(n,0,2)
  m<-mean(x)
  se<-sqrt(var(x))
  abs((m-1)*sqrt(n)/se)>=cv
}))   # 估计均匀分布的第一类错误
  er3<-mean(sapply(1:m,FUN = function(o){
  x<-rexp(n,1)
  m<-mean(x)
  se<-sqrt(var(x))
  abs((m-1)*sqrt(n)/se)>=cv 
}))  # 估计指数分布的第一类错误
er<-cbind(er,c(er1,er2,er3))
}
colnames(er)<-num
rownames(er)<-c("chi(1)","U(0,2)","exp(1)")
return(er)                
}
exercise_6_A(1012)

#### 6_B ####
exercise_6_B <- function(){
set.seed(123)
x <- rnorm(20,2,10)
sigma <- rnorm(20,5,50)
y <- 3*x+sigma
cor(x,y)
cor.test(x,y)
cor.test(x,y,method = 'kendall')
cor.test(x,y,method = 'spearman')
data.frame(x,y)
}
exercise_6_B()
