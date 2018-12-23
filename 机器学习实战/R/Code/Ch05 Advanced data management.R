#------------------------------------------------------------------#
#------------------------------------------------------------------#
Chap5-Advanced data management
#------------------------------------------------------------------#
#------------------------------------------------------------------#


#statement语句：是一条单独的R语句或一组复合语句，包含在花括号{}中的一组R语句，使用分号分隔；
#condition条件：是一条最终被解析为TRUE或假FALSE的表达式；
#expression表达式：是一条数值或字符串的求值语句；
#sequence序列：是一个数值或字符串序列

#------------------------------------------------------------------#

#循环:重复地执行一个或一系列语句，直到某个条件不为真为止。

#------------------------------------------------------------------#
#(1)for循环：for (var in seq) statement
for (i in 1:10) print("Hello")

#构造一个4阶的Hilbert矩阵
n<-4
x<-array(0,dim=c(n,n))
for (i in 1:n){
   for (j in 1:n){
        x[i,j]<-1/(i+j-1)      
   }
}
x

#(2)while循环:while (cond) statement
n<-10
while (n>0) {print("Hello");n<-n-1}

#列出不超过1000的Fibonacci数列
f<-1;f[2]<-1;i<-1
while (f[i]+f[i+1]<1000) {
       f[i+2]<-f[i]+f[i+1]
       i<-i+1
}
f

#(3)repeat循环
f<-1;f[2]<-1;i<-1
repeat{
      f[i+2]<-f[i]+f[i+1]
      i<-i+1
      if (f[i]+f[i+1]>=1000) break
}
f

#或
f<-1;f[2]<-1;i<-1
repeat{
      f[i+2]<-f[i]+f[i+1]
      i<-i+1
      if (f[i]+f[i+1]<1000) next else break
}
f

#------------------------------------------------------------------#

#分支与条件
#在条件执行结构中，一条或一组语句仅在满足一个指定条件时执行

#------------------------------------------------------------------#

#(1)if-else
#在某个给定条件为真时执行语句，也可以同时在条件为假时执行另外的语句
#if (cond) statement
#if (cond) statement1 else statement2

grade<-c("abc")
if (is.character(grade)) grade<-as.factor(grade);grade
if (!is.factor(grade)) as.factor(grade) else print("Grade already is a factor")

#------------------------------------------------------------------#

#(2)ifelse
#是if-else结构比较紧凑的向量化版本
#ifelse (cond,statement1,statment2)
#若cond为真，则执行第一个语句；若cond为假，则执行第二个语句。
#若希望结构的输入和输出均为向量时，请使用ifelse
score<-c(0.3,0.2,0.6,0.1)
ifelse(score>0.5,"Passed","Failed")

#------------------------------------------------------------------#

#(3)switch
#根据一个表达式的值选择语句执行
#switch(statement,list),list可以用有名定义；
#如果表达式的值在1到length(list)，则返回列表相应位置的值。
x<-3;switch(x,2+2,mean(1:10),rnorm(4))
switch(2,2+2,mean(1:10),rnorm(4))

#当list是有名定义时，statement等于变量名时，返回变量名对应的值。
y<-"fruit"
switch(y,fruit="banana",vegetable="broccoli",meat="beaf")

feelings <- c("happy", "afraid")
for (i in feelings)
    print(
      switch(i,
             happy  = "I am glad you are happy",
             afraid = "There is nothing to fear",
             sad    = "Cheer up",
             angry  = "Calm down now"
    )
  )


#------------------------------------------------------------------#
#------------------------------------------------------------------# 

#用户自编函数a user-written function 

#------------------------------------------------------------------# 
#描述性统计量计算函数：计算数据对象的集中趋势和散布情况，可以选择性地给出
参数统计量（均值和标准差）和非参数统计量（中位数和绝对中位差）
mystats <- function(x, parametric=TRUE, print=FALSE) {
  if (parametric) {
    center <- mean(x); spread <- sd(x) 
  } else {
    center <- median(x); spread <- mad(x) 
  }
  if (print & parametric) {
    cat("Mean=", center, "\n", "SD=", spread, "\n")
  } else if (print & !parametric) {
    cat("Median=", center, "\n", "MAD=", spread, "\n")
  }
  result <- list(center=center, spread=spread)
  return(result)
}

# trying it out
set.seed(1234)
x <- rnorm(500) 
y <- mystats(x)
y <- mystats(x, parametric=FALSE, print=TRUE)

#------------------------------------------------------------------# 
# Another switch example
#让用户选择输出当天日期的格式
mydate <- function(type="long") {
    switch(type,
    long =  format(Sys.time(), "%A %B %d %Y"), 
    short = format(Sys.time(), "%m-%d-%y"),
    cat(type, "is not a recognized type\n"))
}
mydate("long")
mydate("short")
mydate()
mydate("medium")
#cat仅会在输入的日期格式类型不匹配"long"或"short"时执行
#使用一个表达式来捕获错误输入的参数值
#有若干函数可以用来为函数添加错误捕获和纠正功能，
函数warning()用来生成一条错误提示信息，
message()用来生成一条诊断信息，
stop()停止当前表达式的执行并提示错误。

#------------------------------------------------------------------# 
#例
#编写一个用二分法求非线性方程根的函数，
#并求方程x^3-x-1=0在区间[1,2]内的根，精度要求为10^(-6)
#二分法计算过程为：取初始区间[a,b]，当f(a)与f(b)异号，做二分法计算；否则停止计算。
取中点x=(a+b)/2，若f(a)与f(x)异号，则置b=x;否则置a=x;
当区间长度小于指定要求时，停止计算。
fzero<-function(f,a,b,eps=1e-5){
   if (f(a)*f(b)>0) list(fail="finding root is fail!")
   else{
       repeat{
       if (abs(b-a)<eps) break
        x<-(a+b)/2
        if (f(a)*f(x)<0) b<-x else a<-x
       }
   list(root=(a+b)/2,fun=f(x))
   }
}

f<-function(x) x^3-x-1
fzero(f,1,2,1e-6)
#或
uniroot(f,c(1,2))



