## R作业
setwd('/Users/kokenhei/Desktop/study/ML/机器学习实战/R/code')
## 2.1 (1)
x <- c(1,2,3)
y <- c(4,5,6)
e <- c(1,1,1)
z <- 2*x + y + e
z
## 2.1(2)计算x与y的内积
x%*%y

## 2.1(3)计算x与y的外积
x%o%y


## 2.2 将1,2...,20构成两个4x5的矩阵，矩阵A按列输入，B按行输入
## (1) C = A+B
A <- matrix(c(1:20),ncol=5);A
B <- matrix(c(1:20),ncol=5,byrow = T);B
C <- A+B;C

## (2) D = AB
D <- t(A) %*% B;D

## (3) 
E <- A * B;E

## (4) F是由A的前3行和前3列构成的矩阵
F <- A[1:3,1:3];F

## (5) G是由矩阵B的各列构成的矩阵，但不含B的第三列
G <- B[,-3];G


## 2.3 构造一个向量x，向量由5个1，3个2，4个3和2个4组成，用rep()函数
x <- rep(c(1,2,3,4),times=c(5,3,4,2));x


## 2.4 生成一个5阶的Hilbert矩阵
A1 <- matrix(rep(1:5,each=5),ncol = 5);A1
B1 <- matrix(rep(1:5,each=5),ncol = 5,byrow = T);B1
H <- 1/(A1+B1-1);H

## 2.4(1) 计算Hilbert矩阵的行列式
det(H)

## 2.4(2) 求H的逆矩阵
solve(H)


## 2.4(3) 求H的特征值和特征向量
eigen(H)


## 2.5 已知有5名学生的数据，用数据框的形式读入数据
studentName <- c('张三','李四','王五','赵六','丁一')
gender <- c('女','男','女','男','女')
age <- c(14, 15, 16, 14,15)
height <- c(156,165,157,162,159)
weight <- c(42.0,49.0,41.5,52.0,45.5)
studentdata<-data.frame(studentName,gender,age,height,weight)
studentdata


## 2.6 将2.5数据写成一个纯文本文件，再用read.table()读取该文件
## 然后再用write.csv()写成一个能用excel表能打开的文件，并用excel打开
write.table(studentdata,'studentdata.txt')
read.table("studentdata.txt",head=T)
write.csv(studentdata,'studentdata.csv')


# 2.7 编写一个函数，输入一个整数n，若n<=0，则中止运算，并输出一句话，
# “要求输入一个正整数”，否则，如果n是偶数，则将n/2，并赋值给n，
# 否则将3n+1赋值给n，不断循环，直到n=1才停止计算，并输出一句话“运算成功”

f <- function(n){
  while(n != 1){
    if(n <= 0){
      print('要求输入一个正整数')
      break
    }
    if(n%%2==0){
      n <- n/2
    }
    else{
      n <- 3*n+1
    }
  }
  if(n==1){
    print('运算成功')
  }
}

f(-3)

## 3.1 计算均值,方差,标准差,极差,标准误,变异系数,偏度,峰度
## 计算均值
x0 <- read.table('exec0301.data');x0
x1 <- as.matrix(x0);x1
x <- as.vector(data1);x
source('data_outline.R')
data_outline(x)
data2.mean <- mean(data2);data2.mean
## 计算方差
data2.var <- var(data2);data2.var
## 计算标准差
data2.sd <- sd(data2);data2.sd
## 计算极差
data2.range <- max(data2) - min(data2);data2.range
## 计算标准误
data2.se <- data2.sd/sqrt(100);data2.se
## 计算变异系数
data2.cv <- 100*data2.sd/data2.mean;data2.cv
## 计算偏度
m <- length(data2);m
data2.sk <- m/((m-1)*(m-2))*sum((data2-data2.mean)^3)/data2.sd^3;data2.sk
## 计算峰度
data2.ku <- (m*(m+1)/(m-1)*(m-2)*(m-3))*sum((data2-data2.mean)^4)/data2.sd^4 - 
  (3*(m-1)^2)/((m-2)*(m-3));data2.ku



## 3.2 绘出3.1的直方图，密度估计曲线，经验分布图和qq图，并将密度估计曲线
## 与正态密度曲线相比较，将经验分布曲线与正态分布曲线相比较(其中正态曲线的均值和标准差取习题3.1计算的值)
# 直方图
hist(x,freq = FALSE)
# 密度估计曲线
lines(density(x),col='orange')
## 将密度估计曲线与正态密度曲线相比较
x0<-60:85
lines(x0, dnorm(x0, mean(x), sd(x)), col="blue")
# 经验分布图
plot(ecdf(x),xlab='x',ylab = 'ecdf',col='red')
lines(x0,pnorm(x0,mean(x),sd(x)),col='blue')
# qq图
qqnorm(x)
qqline(x)


#3.3 
stem(x)
boxplot(x)
fivenum(x)

#3.4
shapiro.test(x)
ks.test(x,'pnorm',mean(x),sd(x))

# 3.5
x1<-c(2,4,3,2,4,7,7,2,2,5,4)
x2<-c(5,6,8,5,10,7,12,12,6,6)
x3<-c(7,11,6,6,7,9,5,5,10,6,3,10)
boxplot(x1,x2,x3,names=c('x1','x2','x3'),vcol=c(2,3,4))
plot(factor(c(rep(1,length(x1)),rep(2,length(x2)),rep(3,length(x3)))),c(x1,x2,x3))

#3.6绘出例3.16关于三项指标的离散图，从图中分析例3.16的结论的合理性
rubber<-read.table('rubber.data')
plot(rubber)

# 3.7(1)
student<-read.table('1.R',header = T)
attach(student)
plot(Weight~Height)
#(2)
coplot(Weight~Height|Sex)
#(3)
coplot(Weight~Height|Age)
#(4)
coplot(Weight~Height|Age+Sex)

#3.8
x<-seq(-2,3,0.5)
y<-seq(-1,7,0.5)
f<-function(x,y){
  x^4-2*x^2*y-2*x*y+2*y^2+9*x/2-4*y+4
}
z<-outer(x,y,f)
contour(x,y,z,levels = c(0,1,2,3,4,5,10,20,30,40,50,60,80,100),col='blue')
persp(x,y,z,theta = 30,phi = 30,expand = 0.7,col = 'red')

# 3.9
cor.test(Height,Weight)

# 3.10
df<-read.table('applicant.data');df
stars(df)
attach(df)
df$G1<-(SC+LC+SMS+DRV+AMB+GSP+POT)/7
df$G2<-(FL+EXP+SUIT)/3
df$G3<-(LA+HON+KJ)/3
df$G4<-AA
df$G5<-APP
a<-scale(df[,16:20])
stars(a)

# 3.11
source('unison.R')
unison(a)
