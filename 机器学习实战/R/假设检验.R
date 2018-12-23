# 单正态总体的的检验
install.packages('BSDA')
library(BSDA)
## 方差已知，检验均值，Z检验
### alternative选择检验类型(表示备择假设)；mu为检验的均值；
### sigma.x，sigma.y为标准差；conf.level为置信水平
z.test(x, y = NULL, alternative = "two.sided", mu = 0, 
       sigma.x = NULL, sigma.y = NULL, conf.level = 0.95)

## 函数法，R语言用的是下分位数
u.test<-function(X,mu,sigma,alternative="two.sided"){
  xbar=mean(X)
  n=length(X)
  Se=sigma/sqrt(n)
  z=(xbar-mu)/Se
  if(alternative=="greater"){   #上侧检验
    p=1-pnorm(z) 
  }
  else if(alternative=="less"){  #下侧检验
    p=pnorm(z)
  }
  else if(alternative=="two.sided"){  #双侧检验
    if(z>=0){
      p=2*(1-pnorm(z))
    }
    else{
      p=2*pnorm(z)
    }
  }
  c(z,p)
}

## 9.1
X <- c(793,782,795,802,797,775,768,798,809)
z.test(X,alternative='two.sided',mu=800,conf.level=0.95,sigma.x = 40)

## 9.2
X <- c(32.66,29.86,31.74,30.15,32.88,31.05) 
z.test(X,alternative='two.sided',mu=32.50,conf.level=0.95,sigma.x = 1.21)

## 9.3
X <- rnorm(20,3160,300)

# 方差未知，检验均值：
## alternative选择检验类型；mu为检验的均值；
## paired设置是否为成对检验；var.equal设置双样本时方差是否相等；
## sigma.x，sigma.y为标准差；conf.level为置信水平
t.test(x, y = NULL, alternative=c("two sided","less","greater"), 
       mu = 0,paired = TRUE, var.equal = FALSE, conf.level = 0.95,...)


# 比例p的检验
# 近似检验（样本量较大）：正态检验
# correct设置是否使用Yates连续修正，默认为TRUE
prop.test(x, n, p = NULL, alternative = c("two.sided","less","greater"),
          conf.level = 0.95,correct = TRUE)
# 例子，抛100枚硬币，正面朝上的次数有45次，分析该硬币是否正常
prop.test(45,100,alternative="two.sided")

# 精确检验：二项分布检验
# x为具有特征样本数，n为样本总数，p为检验的比率
binom.test(x, n, p = 0.5, alternative = c("two.sided","less","greater"),
           conf.level= 0.95)


# 正态总体方差的假设检验
## R中没有直接的函数可以做样本方差的卡方检验(只有检验卡方分布的函数)，
chisq.var.test=function(x,var,mu=Inf,alternative="two.sided"){
  n=length(x)
  df=n-1  #均值未知时的自由度
  v=var(x)  #均值未知时的方差估计值
  #总体均值已知的情况
  if(mu<Inf){
    df=n
    v=sum((x-mu)^2)/n
  }
  chi2=df*v/var  #卡方统计量
  options(digits=4)  #结果显示至小数点后4位
  result=list()  #产生存放结果的列表
  result$df=df;result$var=v;result$chi2=chi2;
  result$P=2*min(pchisq(chi2,df),pchisq(chi2,df,lower.tail=F))
  #若是单侧检验，重新计算P值
  if(alternative=="greater"){
    # lower.tail是真的话，得到的是下分位数，为假的话得到上分位数。
    result$P=pchisq(chi2,df,lower.tail=F)
  }
  else if(alternative=="less"){
    result$P=pchisq(chi2,df)
  } 
  result
}

# 两正态总体总体均值差的检验
# 1. 两个总体方差已知的情况
z.test2=function(x,y,sigma1,sigma2,alternative="two.sided"){
  n1=length(x)
  n2=length(y)
  result=list()  #构造一个空的list，用于存放输出结果
  mean=mean(x)-mean(y)
  z=mean/sqrt(sigma1^2/n1+sigma2^2/n2)  #计算z统计量的值
  options(digits=4)  #结果显示至小数点后4位
  result$mean=mean
  result$z=z  #将均值、z值存入结果
  result$P=2*pnorm(abs(z),lower.tail=FALSE)  #根据z计算P值
  #若是单侧检验，重新计算P值
  if(alternative=="greater") result$P=pnorm(z,lower.tail=FALSE)
  else if(alternative=="less") result$P=pnorm(z)
  result 
}

## 用内置函数做
z.test(x, y = NULL, alternative = "two.sided", mu = 0, sigma.x = NULL, 
       sigma.y = NULL, conf.level = 0.95)


# 2.两个总体的方差未知但相等
t.test(x, y = NULL, alternative=c("two sided","less","greater"), mu = 0,
       paired = TRUE, var.equal = TRUE, conf.level = 0.95,...)



# 成对数据，检验区别是否明显：t检验
t.test(x, y = NULL, alternative=c("two sided","less","greater"), mu = 0,
       paired = TRUE, var.equal = FALSE, conf.level = 0.95,...)


# 两总体方差比较：F检验
# ratio为原假设的方差比值，进行两样本比较时可以使用默认值1；
# alternative设置检验类型为双尾或是单尾；conf.level为置信水平
var.test(x, y, ratio = 1, alternative = c("two.sided","less","greater"), 
         conf.level = 0.95,...)

