#----------------------------------------------------------------#
# Chapter6 基本统计图形                                             # 
# requires that the vcd, plotrix, sm, vioplot packages have been installed                                                      #
# install.packages(c('vcd', 'plotrix', 'sm', 'vioplot'))         #
#----------------------------------------------------------------#

# pause after each graph
par(ask = TRUE)
#----------------------------------------------------------------#

#6.1 条形图
#通过垂直的或水平的条形展示分类变量的分布（频数）

#保存当前的图形参数设置save original graphic settings
opar <- par(no.readonly = TRUE)

#Load vcd package
library(grid)
install.packages('vcd')
library(vcd)

# Get cell counts for improved variable
counts <- table(Arthritis$Improved);counts#提取各单元的计数

#简单条形图simple bar plot
barplot(counts, main = "Simple Bar Plot", 
        xlab = "Improvement", ylab = "Frequency")

#水平条形图horizontal bar plot
barplot(counts, main = "Horizontal Bar Plot", 
        xlab = "Frequency",ylab = "Improvement", horiz = TRUE)

#若分类变量是因子，则可使用函数plot()快速绘制条形图
Arthritis$Improved
plot(Arthritis$Improved, main = "Simple Bar Plot", 
        xlab = "Improvement", ylab = "Frequency")

#水平条形图horizontal bar plot
plot(Arthritis$Improved, main = "Horizontal Bar Plot", 
        xlab = "Frequency",ylab = "Improvement", horiz = TRUE)

#----------------------------------------------------------------#
#堆砌条形图和分组条形图

#治疗类型和改善情况的列联表get counts for Improved by Treatment table
counts <- table(Arthritis$Improved, Arthritis$Treatment)
counts

#堆砌条形图stacked barplot
barplot(counts, main = "Stacked Bar Plot", xlab = "Treatment", 
    ylab = "Frequency", col = c("red", "yellow", "green"), 
    legend = rownames(counts))

#分组条形图grouped barplot
dev.new()
barplot(counts, main = "Grouped Bar Plot", xlab = "Treatment", 
    ylab = "Frequency", col = c("red", "yellow", "green"), 
    legend = rownames(counts), beside = TRUE)
#beside=T表示矩阵中的每一列都表示一个分组，各列中的值将并列而不是堆砌。

#----------------------------------------------------------------#
# 均值条形图Mean bar plots
# 条形图不一定要基于计数数据或频率数据，可以使用数据整合函数并将结果传递给barplot()函数，
# 来创建表示均值、中位数、标准差等的条形图。
states <- data.frame(state.region, state.x77);states
means <- aggregate(states$Illiteracy, 
    by = list(state.region), 
    FUN = mean)
means

means <- means[order(means$x), ]
means

barplot(means$x, names.arg = means$Group.1)#names.arg指定一个字符向量作为标签名
title("Mean Illiteracy Rate")
#----------------------------------------------------------------#

#条形图的微调Fitting labels in bar plots

par(mar = c(5, 8, 4, 2))#使用mar增加了y边界的大小
par(las = 2)
counts <- table(Arthritis$Improved)

barplot(counts, main = "Treatment Outcome", horiz = TRUE, 
    cex.names = 0.8, names.arg = c("No Improvement", 
    "Some Improvement", "Marked Improvement"))#cex.names=0.8缩小了字体的大小
#----------------------------------------------------------------#
#棘状图Spinograms

library(vcd)
attach(Arthritis)
counts <- table(Treatment, Improved)
spine(counts, main = "Spinogram Example")
detach(Arthritis)
#----------------------------------------------------------------#


#6.2饼图Pie charts
#----------------------------------------------------------------#
#pie(x,labels)
#其中x是一个非负数值向量，表示每个扇形的面积；labels表示各扇形便签的字符型变量
par(mfrow = c(2, 2))
slices <- c(10, 12, 4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")

pie(slices, labels = lbls, main = "Simple Pie Chart")

pct <- round(slices/sum(slices) * 100)
lbls2 <- paste(lbls, " ", pct, "%", sep = "")
pie(slices, labels = lbls2, col = rainbow(length(lbls)), 
    main = "Pie Chart with Percentages")

library(plotrix)
pie3D(slices, labels = lbls, explode = 0.1, main = "3D Pie Chart ")

#从表格创建饼图
mytable <- table(state.region)
lbls <- paste(names(mytable), "\n", mytable, sep = "")
pie(mytable, labels = lbls, 
    main = "Pie Chart from a Table\n (with sample sizes)")

# restore original graphic parameters
par(opar)#恢复设置，还原初始的图形参数设置
#----------------------------------------------------------------#

#扇形图fan plots（饼图的变种）
library(plotrix)
slices <- c(10, 12, 4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
fan.plot(slices, labels = lbls, main = "Fan Plot")

#----------------------------------------------------------------#

#6.6 直方图Histograms

par(mfrow = c(2, 2))

hist(mtcars$mpg)#简单直方图

hist(mtcars$mpg, breaks = 12, col = "red", 
    xlab = "Miles Per Gallon", 
    main = "Colored histogram with 12 bins")#指定组数和颜色

hist(mtcars$mpg, freq = FALSE, breaks = 12, col = "red", 
    xlab = "Miles Per Gallon", 
    main = "Histogram, rug plot, density curve")
rug(jitter(mtcars$mpg))
lines(density(mtcars$mpg), col = "blue", lwd = 2)#添加轴须图

# Histogram with Superimposed Normal Curve #添加正态密度曲线和外框
x<-mtcars$mpg
h <- hist(x, breaks = 12, col = "red", 
    xlab = "Miles Per Gallon", 
    main = "Histogram with normal curve and box")
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit * diff(h$mids[1:2]) * length(x)
lines(xfit, yfit, col = "blue", lwd = 2)
# 添加外框
box()

# restore original graphic parameters
par(opar)
#----------------------------------------------------------------#
#6.4 核密度图Kernel density plot
#核密度估计是用于估计随机变量概率密度函数的一种非参数方法。

par(mfrow = c(2, 1))
d <- density(mtcars$mpg)

plot(d)

d <- density(mtcars$mpg)
plot(d, main = "Kernel Density of Miles Per Gallon")
polygon(d, col = "red", border = "blue")
rug(mtcars$mpg, col = "brown")

# restore original graphic parameters
par(opar)
#----------------------------------------------------------------#

#可比较的核密度图Comparing kernel density plots

par(lwd = 2)#将线条设置为2倍宽度
install.packages("sm")
library(sm)
attach(mtcars)

cyl.f <- factor(cyl, levels = c(4, 6, 8), 
    labels = c("4 cylinder", "6 cylinder", "8 cylinder"))#创建分组因子

sm.density.compare(mpg, cyl, xlab = "Miles Per Gallon")#绘制密度图
title(main = "MPG Distribution by Car Cylinders")

colfill <- c(2:(2 + length(levels(cyl.f))))#创建一个颜色向量
cat("Use mouse to place legend...", "\n\n")
#通过鼠标单击添加图例
legend(locator(1), levels(cyl.f), fill = colfill)
detach(mtcars)
par(lwd = 1)

#----------------------------------------------------------------#
#6.5箱线图
par(mfrow = c(1, 2))
boxplot(mpg ~ cyl, data = mtcars, 
    main = "Car Milage Data", 
    xlab = "Number of Cylinders", 
    ylab = "Miles Per Gallon")

#添加notch=T得到含凹槽的箱线图，varwidth=T使得箱线图的宽度与它们各自的样本大小成正比
boxplot(mpg ~ cyl, data = mtcars, notch = TRUE, 
    varwidth = TRUE, col = "red", 
    main = "Car Milage Data", 
    xlab = "Number of Cylinders", 
    ylab = "Miles Per Gallon")
par(opar)

#----------------------------------------------------------------#
#两个交叉因子的箱线图Box plots for two crossed factors
#创建气缸数量的因子
mtcars$cyl.f <- factor(mtcars$cyl, 
                levels = c(4, 6, 8), 
                labels = c("4", "6", "8"))
#创建变速箱类型的因子
mtcars$am.f <- factor(mtcars$am, 
               levels = c(0, 1), 
               labels = c("auto", "standard"))

boxplot(mpg ~ am.f * cyl.f , data = mtcars, 
    varwidth = TRUE, col = c("gold", "darkgreen"), 
    main = "MPG Distribution by Auto Type", 
    xlab = "Auto Type")

#----------------------------------------------------------------#
#小提琴图Violin plots（核密度图以镜像方式在箱线图上的叠加，外部形状即为核密度估计）
install.packages("vioplot")
library(vioplot)
#vioplot函数要求将要绘制的不同组分离到不同的变量中
x1 <- mtcars$mpg[mtcars$cyl == 4]
x2 <- mtcars$mpg[mtcars$cyl == 6]
x3 <- mtcars$mpg[mtcars$cyl == 8]
vioplot(x1, x2, x3, 
    names = c("4 cyl", "6 cyl", "8 cyl"), 
    col = "gold")
title("Violin Plots of Miles Per Gallon")

#----------------------------------------------------------------#
#6.6 点图
#在简单水平刻度上绘制大量标签值
dotchart(mtcars$mpg, labels = row.names(mtcars), 
    cex = 0.7, 
    main = "Gas Milage for Car Models", 
    xlab = "Miles Per Gallon")

#----------------------------------------------------------------#
#分组、排序、着色后的点图sorted colored grouped dot chart

x <- mtcars[order(mtcars$mpg), ]
x$cyl <- factor(x$cyl)
x$color[x$cyl == 4] <- "red"
x$color[x$cyl == 6] <- "blue"
x$color[x$cyl == 8] <- "darkgreen"
dotchart(x$mpg, labels = row.names(x), cex = 0.7, 
    pch = 19, groups = x$cyl, 
    gcolor = "black", color = x$color, 
    main = "Gas Milage for Car Models\ngrouped by cylinder", 
    xlab = "Miles Per Gallon")
  
#----------------------------------------------------------------#

set.seed(520)
len<-24
x<-runif(len)
y<-x^3+rnorm(len,0,0.06)
ds<-data.frame(x=x,y=y)
str(ds)
plot(y~x)
s<-seq(0,1,length=100)
lines(s,s^3,lty=2,col="green")