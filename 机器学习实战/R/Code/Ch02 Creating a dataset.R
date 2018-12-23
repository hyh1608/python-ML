
#--------------------------------------------------------#
# Chapter 2 Creating a dataset                                       
#--------------------------------------------------------#

#数据结构
#--------------------------------------------------------#
#创建向量
a <- c(1, 2, 5, 3, 6, -2, 4)#数值型向量
b <- c("one", "two", "three")#字符型向量
c <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)#逻辑型向量

#使用向量下标
a <- c(1, 2, 5, 3, 6, -2, 4)
a[3]
a[c(1, 3, 5)]
a[2:6]

#--------------------------------------------------------#

#寻找连续出现1的游程
findruns<-function(x,k){
  n<-length(x)
  runs<-NULL
  for (i in 1:(n-k+1)) {
    if (all(x[i:(i+k-1)]==1)) runs<-c(runs,i)
  }
  return(runs)
}
y<-c(1,0,0,1,1,1,0,1,1)
findruns(y,3)
findruns(y,2)

#--------------------------------------------------------#

#创建矩阵

y<-matrix(1:20,nrow=5,ncol=4);y

cells <- c(1, 26, 24, 68)
rnames <- c("R1", "R2")
cnames <- c("C1", "C2")
mymatrix <- matrix(cells, nrow = 2, ncol = 2, byrow = TRUE, 
    dimnames = list(rnames, cnames))
mymatrix
mymatrix <- matrix(cells, nrow = 2, ncol = 2, byrow = FALSE, 
    dimnames = list(rnames, cnames))
mymatrix

#--------------------------------------------------------#

#矩阵下标、脚注

x <- matrix(1:10, nrow = 2)
x
x[2, ]
x[, 2]
x[1, 4]
x[1, c(4, 5)]

#--------------------------------------------------------#

#创建数组

dim1 <- c("A1", "A2")
dim2 <- c("B1", "B2", "B3")
dim3 <- c("C1", "C2", "C3", "C4")
z<-array(1:24,c(2,3,4),dimnames=list(dim1,dim2,dim3))
z
z[2,3,3]
z[]<-0;z #在不改变数组维数的条件下将各元素赋值为0

#--------------------------------------------------------#

#矩阵的运算

#（1）四则运算
x1<-c(100,200)
x2<-1:6
x3<-1:5
x4<-matrix(1:6,nrow=2)
x5<-matrix(1:9,nrow=3)
x1+x2
x1+x3
x1+x4 #向量与矩阵共同运算时，向量按列匹配
x1+x5

7/3    #求商运算
7%/%3  #求整除商
7%%3   #求余数

#（2）转置运算
A<-matrix(1:6,nrow=2);A
t(A)

#(3)求方阵的行列式
det(matrix(1:4,ncol=2))

#(4)生成对角阵和矩阵取对角运算
a<-c(1,4,5)
diag(a)
M<-array(1:9,dim=c(3,3));M
diag(M)

#(5)求矩阵的逆阵及解线性方程组
A<-matrix(c(1:8,10),ncol=3,byrow=T);A
b<-c(1,1,1)
# 求逆
solve(A)
solve(A,b)

#(6)求矩阵的特征值和特征向量
A<-matrix(c(1:8,10),ncol=3,byrow=T);A
eigen(A)

#(7)矩阵的奇异值分解
A<-matrix(c(1:8,10),ncol=3,byrow=T);A
vv<-svd(A) #对矩阵A做奇异值分解，A=UDV',其中U,V是正交矩阵，D为对角阵（奇异值）
# 绑定变量
vv
attach(vv)
# %*%矩阵乘法
u%*%diag(d)%*%t(v)

B<-matrix(c(2,3,4,3,3,5,4,5,7),ncol=3);B#对称矩阵分解
svd(B)

apropos("sv",mode="function")#列出名称中含有sv的所有可用函数

#(8)向量的内积
# 生成序列的优先级更高
x<-1:5;y<-2*1:5
x%*%y
t(x)%*%y
crossprod(x,y)

#(9)向量的外积
x<-1:5;y<-2*1:5
x%o%y
x%*%t(y)
tcrossprod(x,y)
outer(x,y)
#outer(x,y,FUN="")，其中x,y为矩阵或向量，FUN是作外积运算函数，默认值为乘法运算。

#(10)矩阵的乘法
A<-array(1:9,dim=c(3,3));A
B<-array(9:1,dim=c(3,3));B
A*B #点运算
A%*%B #通常意义下的矩阵乘积

t(A)%*%B; crossprod(A,B)
A%*%t(B); tcrossprod(A,B)

#(11)矩阵的合并
x1<-rbind(c(1,2),c(3,4));x1
x2<-10+x1;x2
cbind(x1,x2)
cbind(1,x1)
rbind(x1,x2)

#(12)apply函数
A<-matrix(1:6,nrow=2);A
apply(A,1,sum)  #对行求和
apply(A,2,mean) #对列求均值

#(13)维数、维名称
A<-matrix(1:6,nrow=3);A
dim(A)
nrow(A)
ncol(A)
dimnames(A)<-list(c("one","two","three"),c("First","Second"));A
rownames(A)<-c("A1","A2","A3")
colnames(A)<-c("B1","B2");A

#--------------------------------------------------------#

#创建数据框

patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
patientdata<-data.frame(patientID,age,diabetes,status)
patientdata

patientdata<-data.frame(
   patientID=c(1, 2, 3, 4),age=c(25,34,28,52),
   diabetes=c("Type1", "Type2", "Type1", "Type1"),
   status=c("Poor", "Improved", "Excellent", "Poor")
   )
patientdata

#--------------------------------------------------------#

#选取数据框中的元素
patientdata[1:2]
patientdata[c("diabetes", "status")]

patientdata$age
# 双[[]]不保留变量名
patientdata[[2]]
patientdata[["age"]]

# 保留变量名称
patientdata[2]
patientdata["age"]

#--------------------------------------------------------#
#attach可将数据框添加到R的搜索路径中
attach(mtcars)
plot(mpg,wt)
# $detach  可将数据框从搜索路径中移除
detach(mtcars)
plot(mpg,wt)

mpg<-c(25,36,47)
attach(mtcars)
plot(mpg,wt)
mpg
#在mtcars被绑定前，已有名为mpg的对象，
#此时原始对象将取得优先权

#--------------------------------------------------------#

#with
#with的局限性在于赋值仅在此函数的括号内生效；
#若需创建在with()结构以外存在的对象，使用
#特殊赋值符<<-可将对象保存到with()之外的全局环境中。
with(mtcars,mean(wt))
with(mtcars,{plot(wt,mpg);mean(wt)})

with(mtcars,{summary(wt)->>ee;
             mean(wt)->>ff;
             plot(mpg,wt)}
     )
ee
ff


with(mtcars,{summary(wt);
  mean(wt);
  plot(mpg,wt)}
)
#--------------------------------------------------------#

#因子的使用

patientID<-c(1,2,3,4)
age<-c(25,34,28,52)
diabetes<-c("Type1","Type2","Type1","Type1")
status<-c("Poor","Improved","Excellent","Poor")
#普通因子
diabetes<-factor(diabetes)
diabetes
#有序因子
#要表示有序变量，需要为函数factor()指定参数ordered=T;
#对字符型向量，因子水平默认按照字母顺序创建；
#可以通过指定levels选项来覆盖默认排序。
status1<-factor(status,order=TRUE);status1
status<-factor(status,order=TRUE,
        levels=c("Poor","Improved","Excellent"))
status
patient<-data.frame(patientID, age, diabetes, status)
patient
str(patient)
summary(patient)

#--------------------------------------------------------#

#创建一个列表
g<-"My First List"
h<-c(25, 26, 18, 39)
j<-matrix(1:10, nrow = 5)
k<-c("one", "two", "three")

a1<-list(1:10)
a<-list(g,h,j,k)
typeof(a1)
class(a)
as.data.frame(a1)->b
b
data.frame(g,h,j,k)
mylist<-list(title=g,ages=h,j,k)
mylist


#--------------------------------------------------------#

#数据的输入
install.packages("XQuartz")
mydata<-data.frame(age=numeric(0),
                  gender=character(0),
                  weight=numeric(0))
mydata
edit(mydata)
fix(mydata)

read.table("score.txt",head=T)#导入文本文档数据
read.table("clipboard")#导入剪贴板上的数据

read.delim("score.txt")#导入Excel数据，转成制表符分隔文件
read.csv("score.csv")#导入Excel数据，转成逗号分隔文件



library(foreign)
#导入SPSS数据
read.spss("score.sav")
read.spss("score.sav",to.data.frame=T)
#导入SAS数据
read.xport("score.xpt")

#导入网页数据
install.packages("XML")  #安装解析XML的包
library(XML)
u="http://www.basketball-reference.com/players/j/jamesle01.html"
James=readHTMLTable(readLines(u), which=3, header=TRUE)
dim(James)
James[1:10,2:10]  

a <- c(1:100,)
a

