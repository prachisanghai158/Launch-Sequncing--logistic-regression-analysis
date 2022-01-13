setwd("C:\\Users\\sanghaip\\OneDrive - Merck Sharp & Dohme, Corp\\Desktop\\launch sequencing")
x<-read.csv("final.csv")
z<-read.csv("final.csv")
x<-x[,c(-1,-2,-3)]


for(i in 1:ncol(x))
  print(class(x[,i]))



for(i in c(1:8,12:16))
  x[,i]<-as.numeric(x[,i])


y<-x$launch.Gap

y<-as.data.frame(y)

x<-x[,c(-16)]

product1<-product1[,c(-1,-2)]
normalized <- function(x) (x- min(x))/(max(x) - min(x))
for ( i in 1:12){
  product1[,i]<-normalized(product1[,i])
}

product2<-read.xlsx("Test Results.xlsx",sheetIndex = 3,header=TRUE)
product2<-product2[,c(-1,-2)]
normalized <- function(x) (x- min(x))/(max(x) - min(x))
for ( i in 1:12){
  product2[,i]<-normalized(product2[,i])
}


product3<-read.xlsx("Test Results.xlsx",sheetIndex = 4,header=TRUE)
product3<-product3[,c(-1,-2)]
normalized <- function(x) (x- min(x))/(max(x) - min(x))
for ( i in 1:12){
  product3[,i]<-normalized(product3[,i])
}


finalData<-rbind(product1,product2,product3)

f<-cbind(y,x)


for(i in 1:ncol(f)){
  f[,i][is.na(f[,i])] <- round(mean(f[,i], na.rm = TRUE))
}


LR<-lm(y ~ .,data = f )

summary(LR)

#library(installr)
#updateR()

library(jtools)
library(glmnet)
library(ridge)

#exponential

f$y[f$y==0]<-0.5
LR<-lm(log(y) ~ Population..11.year.olds.+GDP.per.capita..current.US..+Current.health.expenditure....of.GDP.+Government.Schemes.and.health.care.financing.schemes+Government.Effectiveness...2.5.to.2.5.+HDI..Human.Development.Index..2005+Death.Rates..Per.100.000.Population..2002.+Time.gap.to.NIP.since.IQVIA.launch..in.months.+VCR..2018.or.latest.+NIP.by.2015..1.Yes..0.No.+Existence.of.national.screening.program.for.cervical.cancer.by.2015..1.Yes..0.No.+Sum.of.Revenue.5.years+Sum.of.Volume.5.years+Launch.Price+BPC.Wtd.Avg.Price , data = f)
summ(LR)

#ridge
mymod <- linearRidge(f$y ~ f$Population..11.year.olds.+f$GDP.per.capita..current.US..+f$Current.health.expenditure....of.GDP.+f$Government.Schemes.and.health.care.financing.schemes+f$Government.Effectiveness...2.5.to.2.5.+f$HDI..Human.Development.Index..2005+f$Death.Rates..Per.100.000.Population..2002.+f$Time.gap.to.NIP.since.IQVIA.launch..in.months.+f$VCR..2018.or.latest.+f$NIP.by.2015..1.Yes..0.No.+f$Existence.of.national.screening.program.for.cervical.cancer.by.2015..1.Yes..0.No.+f$Sum.of.Revenue.5.years+f$Sum.of.Volume.5.years+f$Launch.Price+f$BPC.Wtd.Avg.Price)
summary((mymod))


