# setwd("~/Google Drive/Tepper/Data Mining/HW1") #Mac
setwd("D:/Users/mstreips/Google Drive/Tepper/Data Mining/HW1") #laptop
install.packages("arules")
install.packages("rpart")
install.packages("party")
install.packages("partykit")
install.packages("vcd")
install.packages("vcdExtra")
install.packages("party", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
install.packages("C50")
install.packages("AppliedPredictiveModeling")
install.packages("caret")
install.packages("e1071")
library(ggplot2)
library(lattice)
library(rpart)
library(party)
# library(partykit)
library(vcd)
library(vcdExtra)
library(arules)
library(plyr)
library(dplyr)
library(C50)
library(AppliedPredictiveModeling)
library(caret)
library(e1071)

## Read Data for First Problem
data <- read.csv("salary-class.csv")

#(a) Read the data set. Our goal is to predict INCOME. How many records do we have?
str(data)  # data.frame':  32561 obs. of  11 variables:

#(b) What are the types of each field be (interval, ordinal, nominal, flag)?
str(data)
# $ AGE     : int  39 50 38 53 28 37 49 52 31 42 ... (ordinal)
# $ EMPLOYER: Factor w/ 9 levels " ?"," Federal-gov",..: 8 7 5 5 5 5 5 7 5 5 ... (nominal)
# $ DEGREE  : Factor w/ 16 levels " 10th"," 11th",..: 10 10 12 2 10 13 7 12 13 10 ... (ordinal)
# $ MSTATUS : Factor w/ 7 levels " Divorced"," Married-AF-spouse",..: 5 3 1 3 3 3 4 3 5 3 ... (nominal)
# $ JOBTYPE : Factor w/ 15 levels " ?"," Adm-clerical",..: 2 5 7 7 11 5 9 5 11 5 ... (nominal)
# $ SEX     : Factor w/ 2 levels " Female"," Male": 2 2 2 2 1 1 1 2 1 2 ... (flag)
# $ C.GAIN  : int  2174 0 0 0 0 0 0 0 14084 5178 ... (ordinal/interval)
# $ C.LOSS  : int  0 0 0 0 0 0 0 0 0 0 ... (ordinal/interval)
# $ HOURS   : int  40 13 40 40 40 40 16 45 50 40 ... (ordinal/interval)
# $ COUNTRY : Factor w/ 42 levels " ?"," Cambodia",..: 40 40 40 40 6 40 24 40 40 40 ...(nominal)
# $ INCOME  : Factor w/ 2 levels " <=50K"," >50K": 1 1 1 1 1 1 1 2 2 2 ...(flag)

# Explore the relationship of INCOME to each of the individual 
# fields AGE, DEGREE, and SEX

## Use boxplot and ggplot to great heat maps
boxplot(AGE ~ INCOME, data, xlab="Income Interval", ylab="Age")
dev.copy(png, file="plot1.png", width=480, height=480)
dev.off()

## Convert to frequency table
freqtbl_age <-as.data.frame((with(data, table(INCOME, AGE))))

## Heat Maps
ggplot(freqtbl_age, aes(INCOME, AGE)) +
        geom_tile(aes(fill = Freq), colour = "black") +
        scale_fill_gradient(low = "white", high = "steelblue")
dev.copy(png, file="plot2.png", width=480, height=480)
dev.off()

freqtbl_sex <- as.data.frame((with(data, table(INCOME, SEX))))
ggplot(freqtbl_sex, aes(INCOME, SEX)) +
        geom_tile(aes(fill = Freq), colour = "black") +
        scale_fill_gradient(low = "white", high = "steelblue")
dev.copy(png, file="plot3.png", width=480, height=480)
dev.off()

freqtbl_degree <-as.data.frame((with(data, table(INCOME, DEGREE))))
ggplot(freqtbl_degree, aes(INCOME, DEGREE)) +
        geom_tile(aes(fill = Freq), colour = "black") +
        scale_fill_gradient(low = "white", high = "steelblue")


dev.copy(png, file="plot4.png", width=480, height=480)
dev.off()

## Bar Charts
barchart(Freq ~ SEX | INCOME, data= freqtbl_sex, layout = c(2,1))
dev.copy(png, file="plot5.png", width=480, height=480)
dev.off()

barchart(Freq ~ DEGREE | INCOME, data= freqtbl_degree, layout = c(1,2))
dev.copy(png, file="plot6.png", width=1080, height=480)
dev.off()


## QUESTION 2 ##

## Read data and expand counts
data2 <- read.table("employee_data.txt", sep=" ", header=TRUE)
data2df <- expand.dft(data2, freq="count")

## Export expanded table for use with SPSS
write.table(data2df, file ="hw1table.txt", sep=" ", col.names=TRUE, row.names=FALSE, 
            quote=FALSE)

## Use rpart to create tree
fit <-rpart(salary ~ department + status + age, method="class", data=data2df)

## plot dendrite tree
plot(fit, uniform=TRUE)
plot(fit)
text(fit, use.n=TRUE, all=TRUE, cex=.8)


frmla <- salary ~ .
fit2 <- ctree(frmla, data = data2df)

plot(fit2)
dev.copy(png, file="plot7.png", width=1480, height=480)
dev.off()

#Table of prediction errors
table(predict(fit2), data2df$age)
table(predict(fit2), data2df$status)
table(predict(fit2), data2df$department)
table(predict(fit2), data2df$age)


###RULES###

rules <- apriori(data2df, parameter=list(support=0.1, confidence=0.01))


##Import rules class into a data.frame for easier handling

rulesf = data.frame(
        lhs = labels(lhs(rules))$elements,
        rhs = labels(rhs(rules))$elements, 
        rules@quality)


## order rules
ordered_rules <-arrange(rulesf, rhs)

target1 <- c("{department=sales}", "{department=systems}", "{department=marketing}", "{department=secretary}")
target2 <- c("{status=senior}", "{status=junior}")
target3 <- c("{age=21..25}", "{age=26..30}", "{age=31..35}", 
             "{age=36..40}","{age=41..45}", "{age=46..50}")

## Filter Views
result1 <- filter(ordered_rules, lhs %in% target1)
result2 <- filter(ordered_rules, lhs %in% target2)
result3 <- filter(ordered_rules, lhs %in% target3)

filter(ordered_rules, rhs %in% "salary")

rules2 <- apriori(data2df, appearance= list(rhs="salary=46K..50K", lhs="department=systems",default="rhs"), 
                  control=list(verbose=F), parameter=list(support=0.1, confidence=0.1))

inspect(rules2)

##C50 Model as per SPSS

## Convert age and salary from nominal to ordinal/ordered
data2df[,3] <- as.ordered(data2df[,3])
data2df[,4] <- as.ordered(data2df[,4])

frmla1 <- salary ~ age  #age -> salary
frmla2 <- salary ~ status #status -> salary
frmla3 <- salary ~ department #department -> salary

summary(oneTree)

## Calculate Rules and Nodes
age_rules <- C5.0(frmla1, data=data2df, rules=TRUE, control= C5.0Control(CF=.01))
status_rules <- C5.0(frmla2, data=data2df, rules=TRUE, control= C5.0Control(CF=.01))
dep_rules <- C5.0(frmla3, data=data2df, rules=TRUE, control= C5.0Control(c(CF=.01,minCases=1)))

## Show Results of C50
summary(age_rules)
summary(status_rules)
summary(dep_rules)

