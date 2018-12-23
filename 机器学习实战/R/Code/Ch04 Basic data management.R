#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
# Chap4 Basic data management                                                                      
# requires that the reshape and sqldf packages have been installed                                                          #
# install.packages(c('reshape', 'sqldf'))                                           

#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#

#按需创建新变量并保存至数据框中
mydata<-data.frame(x1=c(2,2,6,4),x2=c(3,4,2,8));mydata
#方法一
mydata$sumx<-mydata$x1+mydata$x2;mydata
#方法二
attach(mydata)
mydata$meanx<-(x1+x2)/2
detach(mydata)
mydata
#方法三
mydata <- transform(mydata,sums=(x1+x2)^2);mydata

#-----------------------------------------------------------------------------------#

# 创建数据框leader
leader<- data.frame(
     manager=c(1, 2, 3, 4, 5),
     date=c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09"), 
     country=c("US","US","UK","UK","UK"),
     gender=c("M", "F", "F", "M", "F"), 
     age=c(32, 45, 25, 39, 99), 
     q1=c(5, 3, 3, 3, 2), 
     q2=c(4, 5, 5, 3, 2), 
     q3=c(5, 2, 5, 4, 1), 
     q4=c(5, 5, 5, NA, 2), 
     q5=c(5, 5, 2, NA, 1), 
     stringsAsFactors = FALSE)
leader

#---------------------------------------------------------------------------------#

#将连续型变量重编码为分类变量
#方法一
leader$agecat[leader$age>75]<-"Elder"
leader$agecat[leader$age>45&leader$age<=75]<-"Middle Aged"
leader$agecat[leader$age <= 45] <- "Young"
leader
leader$agecat<-NULL;leader
#方法二
leader<- within(leader,{
    agecat<-NULL
    agecat[age>75]<-"Elder"
    agecat[age>=55&age<=75]<-"Middle Aged"
    agecat[age<55]<-"Young"
    })
leader

#---------------------------------------------------------------------------------#

#变量的重命名
#在副本上编辑，原数据未改动
edit(leader)->leadernew
#在原本上编辑
fix(leader)
#使用reshape package
library(reshape)
rename(leader,c(manager="managerID", date="testDate"))
names(leader)
names(leader)[6:10]<-c("item1","item2","item3","item4","item5")

#---------------------------------------------------------------------------------#

#缺失值NA(Not Available)，存在但未知
#R中数值型缺失值和字符串型缺失值符号相同

#缺失值的判断
is.na(leader[,6:10])

#将年龄99重编码为缺失值
leader$age[leader$age==99]<-NA
leader

#多数数值函数均有na.rm=TRUE选项，可在计算前移除缺失值并使用剩余值进行计算
x<-c(1,2,NA,3)
y<-sum(x);y
z<-sum(x,na.rm=T);z

#Using na.omit() to delete incomplete observations
#删除包含缺失值的观测行（行删除）
newdata<-na.omit(leader);newdata

x<-c(5,NA,12)
mode(x[2])
y<-c("abd","def",NA)
mode(y[3])

#---------------------------------------------------------------------------------#

#不存在的值NULL
#NULL是R的一种特殊对象，它没有模式
#NULL的一个用法是在循环中创建向量，其中每次迭代都在这个向量上增加一个元素。
#建立偶数向量
z<-NULL
for (i in 1:10) if (i%%2==0) z<-c(z,i)
z

#NULL与NA的区别
z<-NA
for (i in 1:10) if (i%%2==0) z<-c(z,i)
z

length(NULL)
length(NA)

#不可能出现的值NaN(Not a number)

#---------------------------------------------------------------------------------#

#日期值
mydates <- as.Date(c("2007-06-22", "2004-02-13"));mydates

# Converting character values to dates

strDates<-c("01/05/1965","08/16/1975")
dates<-as.Date(strDates,"%m/%d/%Y");dates

myformat<-"%m/%d/%y"
leader$date<-as.Date(leader$date, myformat);leader

# Useful date functions
#显示当天日期
Sys.Date()
date()

today<-Sys.Date()
format(today,format="%B%d%Y")
format(today,format="%A")

#Calculations with dates

startdate<-as.Date("2004-02-13")
enddate<-as.Date("2011-01-22")
days<-enddate-startdate;days

#Date functions and formatted printing

today<-Sys.Date();today
born<-as.Date("1982-04-13");born
difftime(today,born,units="weeks")
format(born,format="%A")

#---------------------------------------------------------------------------------#

#数据类型的转换
a<-c(1,2,3);a
is.numeric(a)
is.vector(a)
a<-as.character(a);a
is.numeric(a)
is.vector(a)
is.character(a)

#---------------------------------------------------------------------------------#

#排序Sorting a dataset
#(1)sor()t对数据进行排序，数据本身并未改变
x<-c(13,5,12,5)
sort(x)#默认升序
sort(x,decreasing=T)#降序


#(2)order()得到排序后的值在原向量中的索引
#使用order()和索引来对数据框进行排序
y<-data.frame(v1=c("def","ab","zzz"),v2=c(2,5,1))
r<-order(y$v2);r
z<-y[r,];z   #生成以第二列升序排序的数据框

#order函数还能对字符变量排序
d<-data.frame(kids=c("Jack","Jill","Billy"),ages=c(12,10,13));d
d[order(d$kids)]
d[order(d$ages),]

leader
attach(leader)
leader[order(age),]#各行按照经理人的年龄升序排列
leader[order(gender,age),]#各行按照女性到男性，同样性别中按照年龄升序排列
leader[order(gender, -age),]#各行按照女性到男性，同样性别中按照年龄降序排列
detach(leader)
a1<-order(age)
a1
#(3)rank返回原向量中每一个元素的排位
x
rank(x)

#---------------------------------------------------------------------------------#

# 选择变量Selecting variables
leader[6:10]
newdata<-leader[,c(6:10)];newdata

myvars<-c("q1", "q2", "q3", "q4", "q5")
newdata<-leader[myvars];newdata

myvars<-paste("q",1:5,sep="")
newdata<-leader[myvars];newdata

#剔除变量Dropping variables

# names(leader)
# %in%判断变量是否在myvars中，返回逻辑向量
myvars<-names(leader)%in%c("q3","q4")
myvars
newdata<-leader[!myvars];newdata

newdata<-leader[c(-7,-8)];newdata

leader$q3<-leader$q4<-NULL;leader


#选择观测行Selecting observations

newdata<-leader[1:3,]
newdata
# 寻找位置
newdata<-leader[which(leader$gender=="M"&leader$age>30),]

attach(leader)
newdata<-leader[which(gender=="M"&age>30),]
detach(leader)

#根据时间选择观测行Selecting observations based on dates

leader$date<-as.Date(leader$date,"%m/%d/%y")
leader
startdate<-as.Date("2009-01-01")
enddate<-as.Date("2009-10-31")
newdata<-leader[leader$date>=startdate&leader$date <= enddate,]

#使用subset函数Using the subset() function
# 选取子集
newdata<-subset(leader,age>= 35|age < 24,select=c(manager,q1,q2,q3,q4));newdata
newdata<-subset(leader,gender=="M"&age>25,select=gender:q4);newdata

#---------------------------------------------------------------------------------#
#抽样，replace表示是否放回
mysample<-leader[sample(1:nrow(leader),3,replace=F),];mysample

#---------------------------------------------------------------------------------#
#使用SQL语言
#Using SQL statements to manipulate data frames
install.packages("sqldf")
library(sqldf)
newdf<-sqldf("select * from mtcars where carb=1 order by mpg",row.names = TRUE)
newdf<-sqldf("select avg(mpg) as avg_mpg, avg(disp) as avg_disp,
    gear from mtcars where cyl in (4, 6) group by gear")


#---------------------------------------------------------------------------------#

#常用数学函数
abs(-4)#绝对值
sqrt(1:4)#平方根
ceiling(-3.475)#不小于x的最小整数
floor(-3.475)#不大于x的最大整数
trunc(-5.99)#向0的方向截取整数
round(3.475,digits=2)#舍入指定位数的小数
signif(3.475,digits=2)#舍入指定的有效数字位数
log(2.718282)#自然对数
log10(10)#以10为底的对数
log(10,base=10)#以10为底的对数
log(10)
exp(2.302585)

#---------------------------------------------------------------------------------#

#常用统计函数
#mean(x,trim=0.05,na.rm=T)#去掉最大5%和最小5%以及所有缺失值的算术平均值
x<-c(1, 2, 3, 4, 5, 6, 7, 8)
mean(x)
sum(x)
sd(x)
n<-length(x)
meanx<-sum(x)/n;meanx
css<-sum((x-meanx)**2)     
css
sdx<-sqrt(css/(n-1));sdx
var(x)
sqrt(var(x))
median(x)#中位数
quantile(x)#分位数
quantile(x,probs=c(0.3,0.83))#求0.3和0.83分位数
quantile(x,probs=seq(0,1,0.25))
range(x)#求值域
diff(x,lag=3)#滞后差分，默认的lag值为1
min(x)
max(x)
# seq(1,100,2)

#---------------------------------------------------------------------------------#

#正态分布Normal Distribution
#pretty(x,n)创建美观的分割点，通过选取n+1个等间距的取整值，将一个连续型变量x分割为n个区间
# 将-3~3分割为了30份
x<-pretty(c(-3,3),30)
# 密度函数
y<-dnorm(x)
# 分布函数
z<-pnorm(x)
plot(x,y,type='l',xlab="Normal Deviate",ylab="Density")
plot(x,z,type="l",xlab="Normal Deviate",ylab="Dsitribution Function")

#标准正态分布的分位点
pnorm(1.645);pnorm(1.96);pnorm(2.33)
# lower.tail=F 为上a分为数
qnorm(0.05,lower.tail=F);qnorm(0.95)
qnorm(0.025,lower.tail=F);qnorm(0.975)
qnorm(0.01,lower.tail=F);qnorm(0.99)

#---------------------------------------------------------------------------------#

# 设定随机数种子，可以让结果重现
runif(5)
runif(5)
set.seed(1234)                                                     
runif(5)
set.seed(1234)                                                      
runif(5)

#---------------------------------------------------------------------------------#

# 生成服从多元正态分布的数据
#Generating data from a multivariate normal distribution

install.packages("MASS")
library(MASS)
options(digits=3)
set.seed(1234)

mean <- c(230.7, 146.7, 3.6)  #指定均值向量                                         
sigma <- matrix( c(15360.8, 6721.2, -47.1,                              
                    6721.2, 4700.9, -16.5,
                     -47.1,  -16.5,   0.3), nrow=3, ncol=3)  #指定协方差阵
# 生成多元正态分布的随机数，生成500个
mydata <- mvrnorm(500, mean, sigma,empirical=T)
??mvnorm
#empirical=T表示样本均值和样本协方差阵，否则表示总体均值和总体协方差阵                                   
mydata <- as.data.frame(mydata) 
mydata
names(mydata) <- c("y", "x1", "x2")                                       

dim(mydata)                                                             
head(mydata,n=10)
var(mydata)  

#---------------------------------------------------------------------------------#

#字符串String

#计算字符数量
x<-c("ab","cde","fghij")
nchar(x)
nchar(x[3]) 

#提取或替换一个字符向量中的子串
x<-"abcdef"
substr(x,2,4)
substr(x,2,4)<-"22";x
substr(x,2,4)<-"3333333";x

#grep(pattern,x,fixed=F)
#在x中搜索某种模式，若fixed=F，则pattern位一个正则表达式；
# 若fixed=T，则pattern为一个文本字符串，返回值为匹配的下标。
grep("A",c("b","A","a","aA","Aa","c"),fixed=T)

sub("\\s",",","Hello There")

#大写转换
toupper("abc")
#小写转换
tolower("ABC")

#连接字符串，分隔符为sep
paste("x",1:3,sep="")
paste("x",1:3,sep="M")
paste("Today is",date())

#分隔字符串
strsplit("abc","")
strsplit("abc"," ")
strsplit("a-b-c","-")

#连接对象\n表示新行,\b表示退格，\'表示单引号，\t表示制表符
firstname<-c("Jane")
cat("Hello","\n",firstname)
cat("Hello","Bob","\b.\n-","Isn\'t R","\t","GREAT?\n")

#---------------------------------------------------------------------------------#

#将函数应用于数据对象Applying functions to data objects
a<-5
sqrt(a)
b<-c(1.243, 5.654, 2.99)
round(b)
c<-matrix(runif(12), nrow=3);c
log(c)
mean(c)

#---------------------------------------------------------------------------------#

#将一个函数应用到矩阵的所有行（列）
#Applying a function to the rows (columns) of a matrix

mydata <- matrix(rnorm(30), nrow=6)
mydata
# 针对行进行运算
apply(mydata, 1, mean)    
# 针对列进行运算
apply(mydata, 2, mean) 
apply(mydata,2,var)
# 表示计算每一列的截尾均值，忽略了最高和最低的20%。
apply(mydata, 2, mean, trim=0.2)   

#---------------------------------------------------------------------------------#

#学生成绩问题处理:
#将学生的各科考试成绩组合为单一的成绩衡量标准；
#基于相对名次给出从A到E的评分；
#根据学生姓氏和名字的首字母对花名册进行排序。

options(digits=2)#限定了输出小数点后数字的位数
Student <- c("John Davis", "Angela Williams", 
    "Bullwinkle Moose", "David Jones", 
    "Janice Markhammer", "Cheryl Cushing",
    "Reuven Ytzrhak", "Greg Knox", "Joel England",
    "Mary Rayburn")
Math<-c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science<-c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English<-c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)
roster<-data.frame(Student, Math, Science, English,stringsAsFactors=FALSE)
roster#花名册
    
# scale将数据标准化
z<-scale(roster[,2:4]);z #将分数标准化
score<-apply(z, 1, mean)
score
#计算各行的均值以获得综合得分，并使用cbind()将其添加到花名册中                                            
roster<-cbind(roster, score);roster

y<-quantile(score, c(.8,.6,.4,.2));y #给出了学生综合得分的百分位数
y[1]
roster$grade[score>= y[1]]<-"A"                                     
roster$grade[score<y[1] & score>=y[2]]<-"B"
roster$grade[score<y[2] & score>=y[3]]<-"C"
roster$grade[score<y[3] & score>=y[4]]<-"D"
roster$grade[score<y[4]]<-"E"
roster#将学生的百分位数重编码为一个新的类别型成绩变量

name<-strsplit((roster$Student), " ");name
# 返回的是一个列表
class(name)
#使用函数strsplit()以空格为界把学生姓名拆分为姓氏和名字，应用到字符串组成的向量上返回一个列表
Firstname<-sapply(name, "[", 1)
#提取列表中每个成分的第一个元素                                
Lastname<-sapply(name, "[", 2)
#提取列表中每个成分的第二个元素

roster<-cbind(Firstname,Lastname, roster[,-1])
#去掉student变量
roster<-roster[order(Lastname,Firstname),]
roster<-roster[order(roster$score),]
#依姓氏和名字对数据集进行排序
roster

#---------------------------------------------------------------------------------#

#整合与重构
#R提供了许多用来整合(aggregate)和重塑(reshape)数据的方法。在整合数据时，一般将多组
#观测替换为根据这些观测计算的描述性统计量；在重塑数据时，则会通过修改数据的结构（行和列）
#来决定数据的组织方式。

#---------------------------------------------------------------------------------#
#数据集的转置Transposing a dataset
cars <- mtcars[1:5, 1:4]      
cars
t(cars)

#---------------------------------------------------------------------------------#
#整合数据Aggregating data
#aggregate(x,by,FUN) x是待折叠的数据对象，
#by是一个变量名组成的列表（by中的变量必须在列表中，即使只有一个变量），
#这些变量将被去掉以形成新的观测，FUN则是用来计算描述性统计量的标量函数
#根据气缸数和档位数整合mtcars数据，并返回各个数值型变量的均值。
options(digits=3)
attach(mtcars)
aggdata <-aggregate(mtcars, by=list(Group.cyl=cyl,Gruoup.gear=gear), 
    FUN=mean, na.rm=TRUE)
aggdata

#---------------------------------------------------------------------------------#
#reshape包
#首先将数据“融合”melt，以使每一行都是一个唯一的标识符-变量组合，然后将数据“重铸”cast为你想要的任何形状。
install.packages("reshape2")
library(reshape2)
#测量是指最后两列中的值(5,6,3,5,6,1,2,4)，每个测量都能被标识符变量唯一确定，
#本例中的标识符是指ID,Time以及观测属于X1还是X2.
mydata<-data.frame(id=c(1,1,2,2),time=c(1,2,1,2),
                   X1=c(5,3,6,2),X2=c(6,5,1,4));mydata
#融合:将数据及重构为这样一种格式：每个测量变量独占一行，行中带有唯一确定这个测量所需的标识符变量。
md<-melt(mydata,id=c("id","time"));md

#cast读取已经融合的数据，使用提供的公式和可选的用于整合数据的函数将其重塑
#newdata<-cast(md,formula,FUN)
#formula的格式为：rowvar1+rowvar2+...~colvar1+colvar2+...
#rowvar1+rowvar2+...定义了要划掉的变量集合，以确定各行的内容
#colvar1+colvar2+...定义了要划掉的变量集合，以确定各行的内容

dcast(md,id~variable,mean)#给出了每个观测所有时刻在X1和X2上的均值
dcast(md,time~variable,mean)#给出了X1和X2在时刻1和时刻2的均值
cast(md,id~time,mean)#每个观测在时刻1和时刻2的均值，对不同的的X1和X2进行了平均

dcast(md,id+time~variable)
cast(md,id+variable~time)
cast(md,id~variable+time)
    
x <- 1:10
"["(x, 2)
