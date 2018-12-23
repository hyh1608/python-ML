#------------------------------------------------------#
#Chap-1 Introduction to R                                       #
#------------------------------------------------------#
#R官网：http://www.r-project.org
#Auckland大学：Ross Ihaka & Robert Gentleman

# In the following code q() is commented out so that
# you don't quit the session
#------------------------------------------------------#

library(car)
Duncan
scatterplotMatrix(~income+education+prestige,data=Duncan)

#------------------------------------------------------#
# Listing 1.1 - A Sample R session

age <- c(1, 3, 5, 2, 11, 9, 3, 9, 12, 3)
weight <- c(4.4, 5.3, 7.2, 5.2, 8.5, 7.3, 6, 10.4, 10.2, 6.1)
mean(weight)
sd(weight)
sqrt(sum((weight-mean(weight))^2)/(length(weight)-1))
cor(age, weight)
plot(age, weight)

#------------------------------------------------------#

x<-c(1,2,4);x
y<-c(x,x,8);y
x[1]
x[2:3]
mean(x)
sd(x)
sqrt(sum(x-mean(x))^2/(length(x)-1))
sqrt(sum((x-mean(x))^2)/(length(x)-1))

#------------------------------------------------------#
#set graphical output file
pdf("xh.pdf")
#generate 100 N(0,1) variates and plot their histogram  
hist(rnorm(100)) 
#close the graphical output file
dev.off() 
#------------------------------------------------------#

#演示例子
demo()
demo(graphics)
demo(persp)
demo(image)
demo(Hershey)
#R区分大小写，索引从1开始

#--------------------------------------------------------------#

#获取帮助
help.start() #打开帮助文档首页

help(persp)
?persp #查看函数persp的帮助
 
?"<" #获取<运算符的帮助信息

?"for" #查看for循环的帮助信息（特殊字符和一些保留字必须用引号闭合）

help.search("multivariate normal")#搜索某个主题

??persp#以persp为关键词搜索本地帮助文档

example(persp)#函数persp的使用示例

apropos("sol",mode="function")#列出名称中含有sol的所有可用函数

help(package=MASS)#获得整个包的帮助信息
#----------------------------------------------------------------#

#管理R工作空间的函数
# Listing 1.2 - An example of commands used to manage the R Workspace.

#设置当前工作目录，使用正斜杠或双反斜杠
setwd("G:\\Book and PPT\\AR统计分析\\Code")
getwd()#获取当前工作目录
options()#显示当前的选项设置情况
help(options)#显示可用选项的说明
options(digits=3)#数字将被格式化，显示为具有三位有效数字的格式
x<-runif(20)
summary(x)
hist(x)
data()#列出当前已加载包中所含的所有可用示例数据集
ls()#列出当前工作空间中的对象
rm(objectlist)#移除一个或多个对象
rm(list=ls())#移除所有对象
history()#显示最近使用过的命令（默认25条）
q()#退出R，会询问是否保存工作空间
savehistory("myfile")#保存命令历史到文件myfile中（默认值为.Rhistory）
loadhistory("myfile")#载入一个命令历史文件
save.image("myfile")#保存工作空间到文件myfile中（默认值为.RData）
save(objectlist,file="myfile")#保存指定对象到一个文件中
load("myfile")#读取一个工作空间到当前会话中（默认值为.RData）

mean(Nile)
sd(Nile)
hist(Nile)
savehistory()
save.image()
#在独立的目录中保存项目，在启动一个R会话时使用setwd()指定到该项目的路径，
#后接不加选项的load()命令，即可从上一次会话结束的地方重新开始，且保证
#各个项目之间的数据和设置互不干扰，双击保存的镜像文件即可。


#--------------------------------------------------------------#
#用户贡献包的安装与加载
.libPaths()#显示库所在的位置
library()#显示库中有哪些包
install.packages("")#首次安装一个包（一个包仅需安装一次）
#将显示一个CRAN镜像站点的列表，选择其中一个镜像站点后，将看到所有
#可用包的列表，选择其中的一个包即可进行下载和安装。
installed.packages()#列出已安装的包，以及它们的版本号，依赖关系等信息。
update.packages()#更新已安装的包
search()#Gives a list of attached packages and R objects
#R启动时自动加载base,datasets,utils,grDevices,graphics,stats,methods
#显示当前已经加载的包及其所在的位置
path.package()

#--------------------------------------------------------------#

# Listing 1.3-Working with a new package
install.packages("vcd")
help(package=vcd)
library(grid)
library(vcd)
help(Arthritis)
Arthritis
example(Arthritis)
#q()

#--------------------------------------------------------------#
#求积分
integrate(function(x) x,0,1)
integrate(dnorm,-Inf,Inf)
#求导
D(expression(x^3),"x")

#--------------------------------------------------------------#
#将输出用为输入-结果的重用
lm.sol<-lm(mpg~wt,data=mtcars)
summary(lm.sol)
plot(lm.sol)
cook<-cooks.distance(lm.sol)
cook
plot(cook)
predict(lm.sol,data.frame(wt=3))

#--------------------------------------------------------------#
#位b(bit)--存储器的最小单位
#字节B(Byte)--1B=8b
#千字节(KB)--1KB=1024B
#兆字节(MB)--1MB=1024KB
#吉字节(GB)--1GB=1024MB
#太字节(TB)--1TB=1024GB

#--------------------------------------------------------------#