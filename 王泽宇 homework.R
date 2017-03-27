#Problem 1
library(car)
setwd("~/Downloads")
teach<-read.csv('teach.csv',header = T)

attach(teach)
setwd("~/Desktop")

png('picture1.png')
scatterplot(salary~months|sex, data = teach, main = 'Salary vs. the Number of Months in service',
            xlab = 'months', ylab = 'salary', legend.plot = TRUE, 
            labels = teach$sex,reg.line = F, grid = FALSE,smoother = NULL)
dev.off()

png('pic2.png')
boxplot(salary~sex, data = teach, main = 'Salary vs. Sex', xlab = 'Sex', ylab = 'Salary')
dev.off()

png('pic3.png')
boxplot(salary~marry, data = teach, main = 'Salary vs. Marry', xlab = 'Marry', ylab = 'Salary')
dev.off()

png('pic4.png')
boxplot(salary~degree, data = teach, main = 'Salary vs. Degree', xlab = 'Degree', ylab = 'Salary')
dev.off()

png('pic5.png')
boxplot(salary~type, data = teach, main = 'Salary vs. Type', xlab = 'Type', ylab = 'Salary')
dev.off()

png('pic6.png')
boxplot(salary~train, data = teach, main = 'Salary vs. Train', xlab = 'Train', ylab = 'Salary')
dev.off()

png('pic7.png')
boxplot(salary~brk, data = teach, main = 'Salary vs. Brk', xlab = 'Brk', ylab = 'Salary')
dev.off()

library()
png('pic8.png')
scatterplot(salary~months|degree, data = teach, main = 'Salary vs. the Number of Months in service',
            xlab = 'months', ylab = 'salary', legend.plot = TRUE, 
            labels = teach$degree,reg.line = F, grid = FALSE,smoother = NULL)
dev.off()



detach(teach)
teach0 <- teach[teach$degree == 0,]
attach(teach0)

cor(months, salary)
lma1<-lm(salary~months)
library(stargazer)
stargazer(lma1)



detach(teach0)

#Problem 2
library(MASS)
attach(Boston)
name<-list()
for(i in 2:ncol(Boston)){
  name[[paste0('lma',i)]]<-lm(Boston$crim~Boston[,i])
}

stargazer(name[['lma2']],name[['lma3']],name[['lma4']],name[['lma5']],name[['lma6']],
          name[['lma7']],name[['lma8']],name[['lma9']],name[['lma10']],name[['lma11']],
          name[['lma12']],name[['lma13']],name[['lma14']])

lma100<-lm(crim~zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+medv)
stargazer(lma100)

x<-vector(length = (ncol(Boston)-1))
for(i in 1:(ncol(Boston)-1)){
  x[i]<-name[[i]]$coefficients[2]
}
y<-lma100$coefficients[2:14]
png('pict1.png')
plot(x, y, main = 'Coefficient', xlab = 'Single Linear Regression', 
     ylab = 'Multiple Linear Regression',
     pch = 19, col = 'blue',xlim = c(-3,2), ylim = c(-2,2))
abline(lm(y~x))
dev.off()

name<-list()

for(i in 2:ncol(Boston)){
    name[[paste0('lma',i)]]<-lm(Boston$crim~Boston[,i]+I(Boston[,i]^2)+I(Boston[,i]^3))
}
stargazer(name[['lma2']],name[['lma3']],name[['lma4']],name[['lma5']],name[['lma6']],
                 name[['lma7']],name[['lma8']],name[['lma9']],name[['lma10']],name[['lma11']],
                 name[['lma12']],name[['lma13']],name[['lma14']])

detach(Boston)

#Problem 3
setwd("~/Downloads")
housing<-read.table('housing.dat-2.txt',header = T)
attach(housing)
setwd("~/Desktop")
png('pict2.png')
s3d<-scatterplot3d(Longitude, Latitude, MedianHouseValue,pch = 20,
                   highlight.3d = T, angle = 75,cex.axis = 0.5, main = '3D Scatter Plot')
dev.off()

lma15<-lm(MedianHouseValue~MedianIncome+MedianHouseAge+TotalRooms+
            TotalBedrooms+Population+Households+Latitude+Longitude)
library(stargazer)
stargazer(lma15)

png('pict3.png')
plot(lma15$residuals, cex = 0.05, main = 'Residuals')
dev.off()

lma16<-lm(log(MedianHouseValue)~MedianIncome+MedianHouseAge+TotalRooms+
             TotalBedrooms+Population+Households+Latitude+Longitude)
 stargazer(lma16)

detach(housing)
 
#Problem 4
 
 setwd("~/Downloads")
  mkt<-read.csv('mktmodel.csv')
  for(i in 2:ncol(mkt)){
    name[[paste0('lma',i)]]<-lm(mkt[,i]~mkt$SP500)
   }
stargazer(name[['lma2']],name[['lma3']],name[['lma4']],name[['lma5']],name[['lma6']],
              name[['lma7']],name[['lma8']],name[['lma9']],name[['lma10']],name[['lma11']],
              name[['lma12']],name[['lma13']],name[['lma14']],name[['lma15']],name[['lma16']])
stargazer(name[['lma17']],name[['lma18']],name[['lma19']],name[['lma20']],name[['lma21']],
         name[['lma22']],name[['lma23']],name[['lma24']],name[['lma25']],name[['lma26']],
         name[['lma27']],name[['lma28']],name[['lma29']],name[['lma30']],name[['lma31']])
x<-vector(length = 30)
y<-vector(length = 30)
for(i in 1:30){
  x[i]<-name[[i]]$coefficients[2]
  y[i]<-name[[i]]$coefficients[1]
}
setwd("~/Desktop")
png('pict4.png')
plot(x,y, pch = 19, col = 'red', main = 'Coefficients for each stock',
     xlab = 'Market Sensitivity', ylab = 'Market independent income')
abline(lm(y~x))
dev.off()

ret<-mkt$ENE-mkt$WMT
mean(ret)

name[[10]]$coefficients[1]-name[[29]]$coefficients[1]


