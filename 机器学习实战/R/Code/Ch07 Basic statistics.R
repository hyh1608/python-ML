#----------------------------------------------------------------------#
# Chapter7 Basic Statistics                                            #
# requires that the npmc, ggm, gmodels, vcd, Hmisc,                    #
# pastecs, psych, doBy, and reshape packages have been installed       # 
# install.packages(c('npmc', 'ggm', 'gmodels', 'vcd', 'Hmisc',         #
#     'pastecs', 'psych', 'doBy', 'reshape'))                          #
#----------------------------------------------------------------------#

#7.1 描述性统计分析
vars <- c("mpg", "hp", "wt")
head(mtcars[vars])

# Listing 7.1 - descriptive stats via summary

summary(mtcars[vars])
#----------------------------------------------------------------------#
# Listing 7.2 - descriptive stats via sapply()

mtcars[vars]
mystats <- function(x, na.omit = FALSE) {
    if (na.omit) 
        x <- x[!is.na(x)]
    m <- mean(x)
    n <- length(x)
    s <- sd(x)
    skew <- sum((x - m)^3/s^3)/n
    kurt <- sum((x - m)^4/s^4)/n - 3
    return(c(n = n, mean = m, stdev = s, skew = skew, kurtosis = kurt))
}
mystats(mtcars[vars])
sapply(mtcars[vars], mystats)
#----------------------------------------------------------------------#
# Listing 7.3 - Descriptive statistics (Hmisc package)

install.packages('Hmisc')
library(Hmisc)
describe(mtcars[vars])
#----------------------------------------------------------------------#
# Listing 7.4 - Descriptive statistics (pastecs package)

library(pastecs)
stat.desc(mtcars[vars])
#----------------------------------------------------------------------#
# Listing 7.5 - Descriptive statistics (psych package)

library(psych)
describe(mtcars[vars])
#----------------------------------------------------------------------#
# Listing 7.6 - Descriptive statistics by group with aggregate()
#使用aggregate()分组获取描述性统计量
aggregate(mtcars[vars], by = list(am = mtcars$am), mean)
aggregate(mtcars[vars], by = list(am = mtcars$am), sd)
#----------------------------------------------------------------------#
# Listing 7.7 - Descriptive statistics by group via by()
#使用by()分组计算描述性统计量
dstats <- function(x)(c(mean=mean(x), sd=sd(x)))
by(mtcars[vars], mtcars$am, dstats)
#----------------------------------------------------------------------#
# Listing 7.8 Summary statists by group (doBy package)

library(doBy)
summaryBy(mpg + hp + wt ~ am, data = mtcars, FUN = mystats)
#----------------------------------------------------------------------#
# Listing 7.9 - Summary statistics by group (psych package)

library(psych)
describe.by(mtcars[vars], mtcars$am)
#----------------------------------------------------------------------#
#Summary statistics by group (reshape package)

library(reshape)
dstats<-function(x) (c(n=length(x),mean=mean(x),sd=sd(x)))
dfm<-melt(mtcars,measure.vars=c("mpg","hp","wt"),
          id.vars=c("am","cyl"));dfm
cast(dfm, am + cyl + variable ~ ., dstats)

#----------------------------------------------------------------------#
# 7.2 频数表和列联表
#----------------------------------------------------------------------#
# get Arthritis data
library(vcd)
head(Arthritis)

#一维列联表one way table
mytable<-with(Arthritis,table(Improved))
mytable
prop.table(mytable)#将频数转化为比例值
prop.table(mytable)*100#转化为百分比

#----------------------------------------------------------------------#
#二维列联表two way table
#table:使用N个分类变量（因子）创建一个N维列联表
mytable<-with(Arthritis,table(Treatment,Improved))
mytable
#xtabs：根据一个公式和一个矩阵或数据框创建一个N维列联表
mytable<-xtabs(~Treatment+Improved,data=Arthritis)
mytable
#生成边际频数（行数）
margin.table(mytable, 1)
#生成边际比例（行比）
prop.table(mytable, 1)
#生成边际频数（列数）
margin.table(mytable, 2)
#生成边际比例（列比）
prop.table(mytable, 2)
#各单元格所占比例
prop.table(mytable)
#默认将表中所有的变量创建边际和
addmargins(mytable)
addmargins(prop.table(mytable))
#添加行和
addmargins(prop.table(mytable, 1), 2)
#添加列和
addmargins(prop.table(mytable, 2), 1)
#----------------------------------------------------------------------#
#使用gmodels包中的函数CrossTable生成二维列联表Two-way table using CrossTable

library(gmodels)
CrossTable(Arthritis$Treatment,Arthritis$Improved)
#----------------------------------------------------------------------#

#三维列联表Three-way contingency table
mytable<-xtabs(~Treatment+Sex+Improved,
               data=Arthritis)
mytable
#ftable创建一个紧凑的“平铺式”列联表
ftable(mytable)
#生成边际频数
margin.table(mytable, 1)
margin.table(mytable, 2)
margin.table(mytable, 3)
#Treatment*Improved的边际频数
margin.table(mytable, c(1,3))
#Treatment*Sex的各类改善情况比例
ftable(prop.table(mytable,c(1,2)))
ftable(addmargins(prop.table(mytable,c(1,2)),3))
#得到百分比
ftable(addmargins(prop.table(mytable,c(1,2)),3))*100

#----------------------------------------------------------------------#
#7.2独立性检验 
#----------------------------------------------------------------------#
#卡方独立性检验Chis-square test of independence
#原假设为二维表的行变量和列变量独立
library(vcd)
# xtabs()生成二维列联表
mytable<-xtabs(~Treatment+Improved,data=Arthritis)
chisq.test(mytable)
mytable<-xtabs(~Improved+Sex,data=Arthritis)
chisq.test(mytable)
#----------------------------------------------------------------------#

#Fisher精确检验Fisher's exact test
#原假设为二维表的行变量和列变量独立
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
fisher.test(mytable)
#----------------------------------------------------------------------#
# Chochran-Mantel-Haenszel test
#原假设为两个名义变量在第三个变量的每一层中都是条件独立的
mytable <- xtabs(~Treatment+Improved+Sex, data=Arthritis)
mantelhaen.test(mytable)
#----------------------------------------------------------------------#
#相关性度量Measures of association for a two-way table

library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
assocstats(mytable)
#----------------------------------------------------------------------#
#将表格转化为扁平格式
#converting a table into a flat file via table2flat
#在有列联表的情况下需要原始数据
#mytable<-ftable(xtabs(~Treatment+Improved+Sex,data=Arthritis))

table2flat <- function(mytable) {
    df <- as.data.frame(mytable)
    rows <- dim(df)[1]
    cols <- dim(df)[2]
    x <- NULL
    for (i in 1:rows) {
        for (j in 1:df$Freq[i]) {
            row <- df[i, c(1:(cols - 1))]
            x <- rbind(x, row)
        }
    }
    row.names(x) <- c(1:dim(x)[1])
    return(x)
}
#----------------------------------------------------------------------#
#转换已经发表的数据Using table2flat with published data

treatment <- rep(c("Placebo", "Treated"), 3)
improved <- rep(c("None", "Some", "Marked"), each = 2)
Freq <- c(29, 13, 7, 7, 7, 21)
mytable <- as.data.frame(cbind(treatment, improved, Freq))
mydata <- table2flat(mytable)
head(mydata)
#----------------------------------------------------------------------#

#7.3相关
#----------------------------------------------------------------------#
#协方差和相关系数Covariances and correlations

states <- state.x77[, 1:6]
cov(states)
cor(states)
cor(states, method="spearman")

x <- states[, c("Population", "Income", "Illiteracy", "HS Grad")]
y <- states[, c("Life Exp", "Murder")]
cor(x, y)
#----------------------------------------------------------------------#
#偏相关：控制一个或多个变量时，另外两个变量之间的相互关系
#partial correlation of population and murder rate, controlling
#for income, illiteracy rate, and HS graduation rate

library(ggm)
#前两个数值为要计算相关系数的变量下标，其余数值为条件变量（即要排除影响的变量）
pcor(c(1, 5, 2, 3, 6), cov(states))
#----------------------------------------------------------------------#
#相关性的显著性检验Testing correlations for significance

cor.test(states[, 3], states[, 5])

#Correlation matrix and tests of significance via corr.test

library(psych)
corr.test(states, use = "complete")
#----------------------------------------------------------------------#
#Section7.4 t检验
#----------------------------------------------------------------------#
#独立样本的t检验independent t-test

library(MASS)
t.test(Prob ~ So, data=UScrime)
#----------------------------------------------------------------------#
#非独立样本的t检验
# dependent t-test

library(MASS)
sapply(UScrime[c("U1", "U2")], function(x) (c(mean = mean(x), 
    sd = sd(x))))
with(UScrime, t.test(U1, U2, paired = TRUE))

#----------------------------------------------------------------------#
#Section 7.5 组件差异的非参数检验
#----------------------------------------------------------------------#
# Wilcoxon two group comparison

with(UScrime, by(Prob, So, median))
wilcox.test(Prob ~ So, data=UScrime)
sapply(UScrime[c("U1", "U2")], median)
with(UScrime, wilcox.test(U1, U2, paired = TRUE))

# Kruskal Wallis test

states <- as.data.frame(cbind(state.region, state.x77))
kruskal.test(Illiteracy ~ state.region, data=states)

#Nonparametric multiple comparisons

class <- state.region
var <- state.x77[, c("Illiteracy")]
mydata <- as.data.frame(cbind(class, var))
rm(class,var)
library(npmc)
summary(npmc(mydata), type = "BF")
aggregate(mydata, by = list(mydata$class), median)
