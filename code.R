library(xlsx)
?read.xlsx
setwd("C:\\Users\\sanghaip\\OneDrive - Merck Sharp & Dohme, Corp\\Desktop\\launch sequencing")
x<-read.csv("Data_Test.csv")
x<-x[,-1]
LR<-lm(launch.Gap ~ GDP.per.capita..US.dollars.+Current.health.expenditure....of.GDP.+ Government.Schemes.and.health.care.financing.schemes + Government.Effectiveness...2.5.to.2.5.+ Rate.of.Availability + Rate.of.Availability + Average.Time.Between.Marketing.Authorization.and.Patient.Access..days. + Sum.of.Revenue.5.years + Sum.of.Volume.5.years + Launch.Price + Number.of.reference.Countries + Frequency.of.Re.referencing..Months. + Number.of.times.a.country.is.referenced , data = finalData)
y<-table(summary(LR))
summ(LR)
library(jtools)
library(glmnet)
#library(installr)
#updateR()
y<-as.matrix(x[,13])
z<-as.matrix(x[,-13])
lambda_seq <- 10^seq(2, -2, by = -.1)

#exponential
LR<-lm(log(launch.Gap) ~ GDP.per.capita..US.dollars.+Current.health.expenditure....of.GDP.+ Government.Schemes.and.health.care.financing.schemes + Government.Effectiveness...2.5.to.2.5.+ Rate.of.Availability + Rate.of.Availability + Average.Time.Between.Marketing.Authorization.and.Patient.Access..days. + Sum.of.Revenue.5.years + Sum.of.Volume.5.years + Launch.Price + Number.of.reference.Countries + Frequency.of.Re.referencing..Months. + Number.of.times.a.country.is.referenced , data = x)
y<-table(summary(LR))
summ(LR)
#rigid
cv_output <- cv.glmnet(z, y, 
                       alpha = 1, lambda = lambda_seq)
best_lam <- cv_output$lambda.min
lasso_best <- glmnet(z, y, alpha = 0, lambda = best_lam)
summ(coef(lasso_best))
summary(coef(lasso_best))




library(ridge)
mymod <- linearRidge(y ~ z)

summary(mymod)
summ(mymod)
Call:
  linearRidge(formula = y ~ ., data = longley)
library(rJava)
library(glmulti)
test<- glmulti(launch.Gap ~ GDP.per.capita..US.dollars.+Current.health.expenditure....of.GDP.+ Government.Schemes.and.health.care.financing.schemes + Government.Effectiveness...2.5.to.2.5.+ Rate.of.Availability + Rate.of.Availability + Average.Time.Between.Marketing.Authorization.and.Patient.Access..days. + Sum.of.Revenue.5.years + Sum.of.Volume.5.years + Launch.Price + Number.of.reference.Countries + Frequency.of.Re.referencing..Months. + Number.of.times.a.country.is.referenced ,data=x,plotty = F)
bm<- summary(test)$bestmodel
lm(bm,data=testdf)

setwd("C:\\Users\\sanghaip\\OneDrive - Merck Sharp & Dohme, Corp\\Desktop\\launch sequencing")
library(xlsx)
product1<-read.xlsx("Test Results.xlsx",sheetIndex = 2,header=TRUE)

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


x<-finalData
LR<-lm(launch.Gap ~ GDP.per.capita..US.dollars.+Current.health.expenditure....of.GDP.+ Government.Schemes.and.health.care.financing.schemes + Government.Effectiveness...2.5.to.2.5.+ Rate.of.Availability + Rate.of.Availability + Average.Time.Between.Marketing.Authorization.and.Patient.Access..days. + Sum.of.Revenue.5.years + Sum.of.Volume.5.years + Launch.Price + Number.of.reference.Countries + Frequency.of.Re.referencing..Months. + Number.of.times.a.country.is.referenced , data = finalData)
summ(LR)
write.xlsx(finalData,"Normalized dataset.xlsx")
