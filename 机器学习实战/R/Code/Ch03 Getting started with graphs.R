#-----------------------------------------------------------------------#

#Chapter3-Getting started with graphs
                            
# requires that the Hmisc package has been installed                    #
# install.packages('Hmisc')                                             #

#-----------------------------------------------------------------------#

# pause after each graph
par(ask = TRUE)

#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

attach(mtcars)#绑定数据框mtcars
wt
hist(wt)
plot(wt,mpg)#绘制散点图
abline(lm(mpg~wt))#添加最优拟合曲线
title("Regression of mpg on weight")#添加标题
detach(mtcars)#解除数据框的绑定

#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

#3.1创建和保存图形

#通过代码保存图形，将绘图语句夹在开启目标图形设备的语句和
 #关闭目标图形设备的语句之间；
#将图形保存在当前工作目录中名为“mm.pdf”的PDF文件中
pdf("mm.pdf") 
attach(mtcars)
plot(wt, mpg)
abline(lm(mpg ~ wt))
title("Regression of mpg on wt")
detach(mtcars)
dev.off()

#将图形保存在当前工作目录中名为“mm.jpeg”的JPEG文件中
jpeg("mm.jpeg") attach(mtcars)
plot(wt, mpg)
abline(lm(mpg ~ wt))
title("Regression of mpg on wt")
detach(mtcars)
dev.off()

#通过图形用户界面保存图形：
#“文件”-“另存为”，在弹出的对话框中选择想要的保存格式和位置。

#创建多个图形并随时查看每一个：
dev.new()
plot(Nile)
dev.new()
hist(Nile)
#通过图形用户界面，“history-Recording-Previous/Next”

#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

#5.2图形参数

dose<-c(20,30,40,45,60)  #药物剂量
drugA<-c(16,20,27,40,60) #对药物A的响应
drugB<-c(15,18,25,31,40) #对药物B的响应
# b为both，既有点又有线
plot(dose,drugA,type="b",col='lightgreen')#绘制点集(dose,drugA)，同时绘制点和线

par()
#par(optionname=value,...)用来修改图形参数；
#不加参数地执行par()将生成一个含有当前图形参数设置的列表；
#par(no.readonly=TRUE)生成一个可修改的当前图形参数列表

opar<-par(no.readonly=TRUE)#复制一份当前的图形参数设置
par(lty=2,pch=17) #将默认的线条类型修改为虚线，将默认的点符号改为实心三角。
plot(dose,drugA,type="b")
par(opar) #还原原始设置

#或用高级绘图函数直接提供optionname=value键值对，
 #此时指定的选项仅对这幅图形本身有效。
plot(dose,drugA,type="b",lty=2,pch=17)
#并不是所有的高级绘图函数都允许指定全部可能的图形参数，需要参考每个特定绘图函数
 #的帮助以确定哪些参数可以以这种方式设置。

#-----------------------------------------------------------------------#

#符号和线条：
#pch:指定绘制点时使用的符号
#cex:指定符号的大小，表示绘图符号相对于默认大小的缩放倍数
#lty:指定线条类型
#lwd:指定线条宽度，表示相对于默认大小的缩放倍数

plot(dose,drugA,type="b",lty=3,lwd=3,pch=15,cex=2)
colors()

#-----------------------------------------------------------------------#

#颜色：
#col:绘图颜色
#col.axis:坐标轴刻度文字的颜色
#col.lab:坐标轴标签的颜色
#col.main:标题颜色
#col.sub：副标题颜色
#fg:图形的前景色
#bg:图形的背景色
#colors()返回所有可用颜色的名称

n<-10
mycolors<-rainbow(n)#生成10种连续的“彩虹型”颜色
pie(rep(1,n),labels=mycolors,col=mycolors)

#生成n阶灰度色，通过一个元素值为0和1之间的向量来指定各颜色的灰度。
mygrays<-gray(0:n/n)
pie(rep(1, n),labels=mygrays,col=mygrays)

#-----------------------------------------------------------------------#

#文本属性
#cex:表示相对于默认大小缩放倍数的数值
#cex.axis:坐标轴刻度文字的缩放倍数
#cex.lab:坐标轴标签的缩放倍数
#cex.main:标题的缩放倍数
#cex.sub:副标题的缩放倍数

#font:指定字体样式，1=常规，2=粗体，3=斜体，4=粗斜体，5=符号字体
#font.axis:坐标轴刻度文字的字体样式
#font.lab:坐标轴标签的字体样式
#font.main:标题的字体样式
#font.sub:副标题的字体样式
#ps:字体磅值
#family:绘制文本时使用的字体族


#-----------------------------------------------------------------------#

#图形尺寸和边界尺寸
#pin:以英寸表示的图形尺寸（宽和高）
#mai:以数值向量表示的边界大小（下、左、上、右），单位为英寸
#mar:以数值向量表示的边界大小（下、左、上、右），单位为英分
     #（1英分等于十二分之一英寸），默认值为c(5,4,4,2)+0.1

#使用图形参数控制图形外观
dose<-c(20,30,40,45,60)
drugA<-c(16,20,27,40,60)
drugB<-c(15,18,25,31,40)
opar<-par(no.readonly = TRUE)
par(pin=c(2,3),mai=c(1,0.5,1,0.2))
par(lwd=2,cex=1.5)
par(cex.axis=0.75,font.axis=3)
#使用红色实心圆圈和虚线创建第一幅图形
plot(dose,drugA,type="b",pch=19,lty=2,col="red")
#使用绿色填充的绿色菱形加蓝色边框和蓝色虚线创建第二幅图形
plot(dose,drugB,type="b",pch=23,lty=6,col="blue",fg="green")
#还原初始图形参数设置
par(opar)

#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

# 5.3 添加文本、自定义坐标轴和图例

plot(dose,drugA,type="b", 
    col="red",lty=2,pch=2,lwd=2, 
    main="Clinical Trials for Drug A", 
    sub="This is hypothetical data", 
    xlab="Dosage",ylab="Drug Response", 
    xlim=c(0,60),ylim=c(0,70))

plot(dose,drugA,type="b",
     main="My Title",col.main="red",
     sub="My Sub-title",col.sub="blue",
     xlab="My X label",ylab="My Y label",
     col.lab="green",cex.lab=0.75)
#生成红色的标题和蓝色的副标题，以及较默认大小小25%的绿色x轴、y轴标签。

#-----------------------------------------------------------------------#

#自定义坐标轴
x<-c(1:10)
y<-x
z<-10/x
opar<-par(no.readonly=TRUE)
par(mar=c(5,4,4,8)+0.1)

# yaxt='n'表示禁用y轴,ann=FALSE移除默认的标题和标签
plot(x,y,type="b",pch=21,col="red",yaxt="n",lty=3,ann=TRUE)
lines(x,z,type="b",pch=22,col="blue",lty=2)

# side表示在图形的哪边绘制刻度线的位置，1=下，2=左，3=上，4=右
# at 数值型向量，表示需要绘制刻度线的位置
# label 字符型向量，表示置于刻度线旁边的文字标签，若为NULL，则将直接使用at中的值
# las=0 标签平行于坐标轴，las=1，标签垂直于坐标轴
# tck刻度线的长度，以相对于绘图区域大小的分数表示，负值表示在图形外侧，
# 0表示禁用刻度，1表示绘制网格线，默认值为-0.01

axis(2,at=x,labels=x,col.axis="red",las=2)
axis(4,at=z,labels=round(z,digits=2),col.axis="blue", 
    las=2,cex.axis=0.7,tck=-0.01)

# 添加文本，m表示在图形外侧，内侧添加文本没有m
mtext("y=1/x",side=4,line=3,cex.lab=1,las=2,col="blue")
title("An Example of Creative Axes",xlab="X values",ylab="Y=X")

par(opar)

#-----------------------------------------------------------------------#

#依剂量对比药物A和药物B的响应情况
dose<-c(20,30,40,45,60)
drugA<-c(16,20,27,40,60)
drugB<-c(15,18,25,31,40)
opar<-par(no.readonly = TRUE)
par(lwd=2,cex=1.5,font.lab=2)
plot(dose, drugA, type = "b", pch = 15, lty = 1, col = "red", 
    ylim = c(0, 60), main = "Drug A vs. Drug B", 
    xlab = "Drug Dosage", ylab = "Drug Response")
lines(dose, drugB, type = "b", pch = 17, lty = 2, col = "blue")
# h表示水平参考线
abline(h = c(30), lwd = 1.5, lty = 2, col = "grey") #添加水平参考线
abline(v = c(30), lwd = 1.5, lty = 2, col = "green") #添加水平参考线

install.packages('Hmisc')
library(Hmisc)
minor.tick(nx = 3, ny = 3, tick.ratio = 0.5)
legend("topleft", inset = 0.05, title = "Drug Type", c("A", "B"), 
       lty = c(1, 2), pch = c(15, 17), col = c("red","blue"))
par(opar)

#-----------------------------------------------------------------------#

#文本标注
#text()可向绘图区域内部添加文本
#mtext()可向图形的四个边界之一添加文本
#location:文本的位置参数
#pos:文本相对于位置参数的方位
#side:指定用来放置文本的边，1=下，2=左，3=上，4=右
attach(mtcars)
plot(wt, mpg, main = "Milage vs. Car Weight", xlab = "Weight", 
    ylab = "Mileage", pch = 18, col = "blue")
text(wt, mpg, row.names(mtcars), cex = 0.6, pos = 4, col = "red")
detach(mtcars)

#-----------------------------------------------------------------------#

#展示不同字体族
opar <- par(no.readonly = TRUE)
par(cex = 1.5)
plot(1:7, 1:7, type = "n")
text(3, 3, "Example of default text")
text(4, 4, family = "mono", "Example of mono-spaced text")
text(5, 5, family = "serif", "Example of serif text")
par(opar)

#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

#5.4 组合图形combining graphs

#创建4幅图形并将其排布在2行2列中：
attach(mtcars)
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
plot(wt,mpg,main="Scatterplot of wt vs. mpg")
plot(wt,disp,main="Scatterplot of wt vs disp")
hist(wt,main="Histogram of wt")
boxplot(wt,main="Boxplot of wt")
par(opar)
detach(mtcars)

#-----------------------------------------------------------------------#

#创建3幅图形并将其排布在3行1列中：
attach(mtcars)
opar <- par(no.readonly = TRUE)
par(mfrow = c(3, 1))
hist(wt,ann=F)#或ann=F来禁用默认的所有标题和标签
hist(mpg,main="MPG") #使用main=""来更改默认的标题
hist(disp)
par(opar)
detach(mtcars)

#-----------------------------------------------------------------------#

#一幅图被置于第1行，另两幅图被置于第2行：
attach(mtcars)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)

#-----------------------------------------------------------------------#

#精确控制每幅图形大小
attach(mtcars)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE), 
       widths = c(3, 1), heights = c(1, 2))
#第1行图形高度是第2行图形高度的二分之一，右下角图形宽度是左下角图形宽度的三分之一
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)

#-----------------------------------------------------------------------#

#图形布局的精细控制
#通过在散点图上添加两幅箱线图，创建增强型图形

opar <- par(no.readonly = TRUE)
par(fig = c(0, 0.8, 0, 0.8))
plot(mtcars$wt, mtcars$mpg, 
     xlab = "Car Weight", ylab = "Miles Per Gallon")
par(fig = c(0, 0.8, 0.55, 1), new = TRUE)
boxplot(mtcars$wt, horizontal = TRUE, axes = FALSE)
par(fig = c(0.65, 1, 0, 0.8), new = TRUE)
boxplot(mtcars$mpg, axes = FALSE)
mtext("Enhanced Scatterplot", side = 3, outer = TRUE, 
    line = -3)
par(opar)

#------------------------------------------------------------------# 

#绘制二元正态分布的联合密度曲面图和等高线图
x<-y<-seq(-2*pi,2*pi,pi/15);
f<-function(x,y) {1/(2*pi)}*exp(-(1/2)*(x^2+y^2));
z<-outer(x,y,f)
persp(x,y,z)
contour(x,y,z,col="blue")

