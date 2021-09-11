rm(list=ls())

#Setting working directory
setwd("C:/Users/agust/Documents/School/Semester III/Machine Learning/ML Project")

#Packages
Packages <- c("knitr", "class", "dplyr", "ggplot2", "plyr", "data.table", "glmnet", "leaps", "foreign", "h2o")
lapply(Packages, library, character.only = TRUE)

###########Loading, Cleaning, and Merging Files

#Movement data
#wcer = read.csv(file="wcer.csv", sep=",", header = T)
#wcer <- filter(wcer, OK=="1") #OK = 0 is unreliable data
#wcer <- wcer[ which( wcer$SALE=="B" | wcer$SALE=="C" | wcer$SALE=="S") , ] #only use when sale equals B,C,or S
#wcer <- filter(wcer,MOVE!="0")

#wcer$lmove = log(wcer$MOVE)
#wcer$lprice = log(wcer$PRICE)

#wcer$revenue = (wcer$PRICE*wcer$MOVE)/wcer$QTY #generate revenues
#wcer$profit = wcer$revenue*wcer$PROFIT/100 #generate profits
#save(wcer, file = "wcer_clean.Rdata")

load("wcer_clean.Rdata")

#UPC characteristic data
upccer = read.csv(file="upccer.csv", sep=",", header = T)

upccer$SIZE = (gsub(" .*$", "", upccer$SIZE ))
upccer$SIZE = (gsub("O.*$", "", upccer$SIZE ))
upccer$SIZE = (gsub("Z.*$", "", upccer$SIZE ))
upccer <- filter(upccer, SIZE!="ASSTD" & SIZE != "end" & SIZE != "ASST")
upccer$SIZE <- as.numeric(upccer$SIZE)
upccer <- upccer[complete.cases(upccer),]

#Demographic data
demo = read.dta(file="demo.dta")

demo <- select(demo, c("store","age9","age60","ethnic","educ","nocar","income","incsigma","hsizeavg","hsize1",
                       "hsize2","hsize34","hsize567","hh3plus","hh4plus","hhsingle","hhlarge","workwom",
                       "sinhouse","density","hval150","hval200","hvalmean","single","retired","unemp",
                       "wrkch5","wrkch17","nwrkch5","nwrkch17","wrkch","nwrkch","wrkwch","wrkwnch",
                       "telephn","mortgage","nwhite","poverty","shpindx"))

demo <- demo[complete.cases(demo),]

store126<- filter(demo, store == "126")
t(store126)
write.csv(store126, file = "store126.csv")



#Customer Count data, create month variable and create dummy for holidays
ccount = read.dta(file="ccount.dta")
ccount <- select(ccount, c("store","date", "week", "custcoun"))
ccount <- ccount[complete.cases(ccount),]
ccount$month <- substr(ccount$date,3,4)

agg = aggregate(ccount$custcoun,by=list(ccount$store,ccount$week),FUN="sum")
names(agg)[c(1:3)] <- c("store","week","custcoun")
#nrow(agg[agg$custcoun>100000,])

ccount <- select(ccount, c("store","date","week","month"))
ccount <- inner_join(ccount,agg, by=c("store","week"))

#Merging datas
data1 <- inner_join(wcer, upccer, by = "UPC")
names(data1)[names(data1) == "STORE"] <- "store"
names(data1)[names(data1) == "WEEK"] <- "week"

data2 <- inner_join(ccount, demo, by="store")

data <- inner_join(data1, data2, by=c("week","store"))

#Adding month, store, and sales type fixed effects
data$month.f <- factor(data$month)
data$store.f <- factor(data$store)
data$sale.f <- factor(data$SALE)
data$upc <- factor(data$UPC)

##########Create main dataset for analysis
main <- select(data,c("lmove","lprice","sale.f", "store.f", "month.f",
                      "SIZE","custcoun","QTY","upc",
                      "age9","age60","ethnic","educ","nocar","income","incsigma","hsizeavg","hsize1",
                      "hsize2","hsize34","hsize567","hh3plus","hh4plus","hhsingle","hhlarge","workwom",
                      "sinhouse","density","hval150","hval200","hvalmean","single","retired","unemp",
                      "wrkch5","wrkch17","nwrkch5","nwrkch17","wrkch","nwrkch","wrkwch","wrkwnch",
                      "telephn","mortgage","nwhite","poverty","shpindx"))
#rm(agg,ccount,data1,data2,demo,upccer,wcer)

save(main, file = "maindata.Rdata")

#################Create splits by sales promotion type
attributes(main$sale.f)
saleb <- filter(main, sale.f=="B")
saleb <- subset(saleb, select = -c(sale.f))

set.seed(0)
samplesize <- floor(0.9 * nrow(saleb))
sample <- sample(seq_len(nrow(saleb)), size = samplesize)

salebt <- saleb[sample, ]
salebv <- saleb[-sample, ]

salec <- filter(main, sale.f=="C")
salec <- subset(salec, select = -c(sale.f))

sales <- filter(main, sale.f=="S")
sales <- subset(sales, select = -c(sale.f))

trainb = model.matrix(lmove~.,data=salebt)
valb = model.matrix(lmove~.,data=salebv)

testc = model.matrix(lmove~.,data=salec)
tests = model.matrix(lmove~.,data=sales)

######################replicate test sample 
salec1=salec
salec1<-salec1[which(salec1$upc !=3800012100),]
salec1<-salec1[which(salec1$upc !=3800012200),]

sales1=sales
a=unique(sales1$upc)
b=unique(salebt$upc)
c=intersect(a,b)
c<-data.frame(c)
names(c)[names(c)=="C"]<-"upc"

sales1<-subset(sales1,sales1$upc%in%c$c)

salebv1=salebv

rm(upccer,agg,demo,wcer,ccount,data1,data2)

#OLS

linear <- lm(formula= lmove~., data=salebt)
mse_linear = mean((salebv$lmove - predict(linear,salebv))^2)
salec1$linear_hat<-predict(linear,salec1)
sales1$linear_hat<-predict(linear,sales1)
salebv1$linear_hat<-predict(linear,salebv1)

stderror_linear=(mean(salebv1$linear_hat)-mean(salebv1$lmove))/sd(salebv1$lmove)

msec_linear = mean((salec1$lmove - predict(linear,salec1))^2)
stderrorc_linear=(mean(salec1$linear_hat)-mean(salec1$lmove))/sd(salec1$lmove)
mses_linear = mean((sales1$lmove - predict(linear,salebv))^2)
stderrors_linear=(mean(sales1$linear_hat)-mean(sales1$lmove))/sd(sales1$lmove)

mse_linear
msec_linear
mses_linear
stderror_linear
stderrorc_linear
stderrors_linear
#################Backward Stepwise
bss <- regsubsets(lmove~.,data=salebt,method ="backward",nvmax=NULL)

for (i in 1:136){
  coefi = coef(bss, id=i)
  salebv1$bss_hat = valb[,names(coefi)]%*%coefi
}
for (i in 1:136){
  coefi = coef(bss, id=i)
  salec1$bss_hat = testc[,names(coefi)]%*%coefi
}
for (i in 1:136){
  coefi = coef(bss, id=i)
  sales1$bss_hat = tests[,names(coefi)]%*%coefi
}
bss.summary=summary(bss)
bss.summary$adjr2
data.frame(Adj.R2 = which.max(bss.summary$adjr2))
mse.bss = rep(NA,12)
for (i in 1:12){
  coefi = coef(bss, id=i)
  yhat = valb[,names(coefi)]%*%coefi
  mse.bss[i] = mean((salebv$lmove - yhat)^2)
}

mse.bss[96] #MSE using Backward Stepwise Best Stepwise based on Adjusted R^2
stderror_bss=(mean(salebv1$bss_hat)-mean(salebv1$lmove))/sd(salebv1$lmove)
stderrorc_bss=(mean(salec1$bss_hat)-mean(salec1$lmove))/sd(salec1$lmove)
stderrors_bss=(mean(salec1$bss_hat)-mean(sales1$lmove))/sd(sales1$lmove)

MSE=mean((salebv1$bss_hat-salebv1$lmove)^2)
MSEc_bss=mean((salec1$bss_hat-salec1$lmove)^2)
MSEs_bss=mean((sales1$bss_hat-sales1$lmove)^2)

MSE
MSEc_bss
MSEs_bss
stderror_bss
stderrorc_bss
stderrors_bss
#############Lasso
#Lasso by 90-10 Split
grid = 10^seq(10,-5,length=100)

lasso = cv.glmnet(trainb, salebt$lmove, type.measure="mse", alpha=0, lambda=grid, nfolds=10)

plot(lasso)
coef(lasso)

lambda.lasso = lasso$lambda.min

salebv1$lasso_hat=predict(lasso, newx=valb, s = lambda.lasso)
salec1$lasso_hat=predict(lasso, newx=testc, s = lambda.lasso)
sales1$lasso_hat=predict(lasso, newx=tests, s = lambda.lasso)

stderror_lasso=(mean(salebv1$lasso_hat)-mean(salebv1$lmove))/sd(salebv1$lmove)
stderrorc_lasso=(mean(salec1$lasso_hat)-mean(salec1$lmove))/sd(salec1$lmove)
stderrors_lasso=(mean(salec1$lasso_hat)-mean(sales1$lmove))/sd(sales1$lmove)

MSE_lasso=mean((salebv1$lasso_hat-salebv1$lmove)^2)
MSEc_lasso=mean((salec1$lasso_hat-salec1$lmove)^2)
MSEs_lasso=mean((sales1$lasso_hat-sales1$lmove)^2)

MSE_lasso
MSEc_lasso
MSEs_lasso
stderror_lasso
stderrorc_lasso
stderrors_lasso
###############################


######################Principle Components Analysis to Reduce Features for H20
salebtpc<-subset(salebt,select=-c(store.f,month.f,lmove,lprice,upc))
pcab=prcomp(salebtpc,scale=TRUE)

floading=pcab$rotation
floading<-subset(floading, select=c(PC1,PC2,PC3))

matrixapcfl<-data.matrix(floading)

####################################### Pca for saleb training

salebt1 <- subset(salebt,select=-c(store.f,month.f,lmove,lprice,upc))

matrixbt1=data.matrix(salebt1)
salebt1=matrixbt1%*% matrixapcfl

salebt1=data.frame(salebt1)
rm(matrixbt1)
salebt1$upc=salebt$upc

pca_train<-subset(salebt,select=c(store.f,month.f,lmove,lprice,upc))
pca_train <- merge(pca_train, salebt1, by=0, all=TRUE)

names(pca_train)[names(pca_train) == "upc.x"] <- "upc"
pca_train$upc.y<-NULL
pca_train$Row.names<-NULL

pca_train <- pca_train[,c(3,1:2,4:8)]

#Validation
salebv2<-subset(salebv,select=-c(store.f,month.f,lmove,lprice,upc))

matrixbv1=data.matrix(salebv2)

salebv2=matrixbv1%*%matrixapcfl
salebv2=data.frame(salebv2)
salebv2$upc=salebv$upc

pca_val <- subset(salebv,select=c(store.f,month.f,lmove,lprice,upc))
pca_val <- merge(pca_val, salebv2, by=0, all=TRUE)

names(pca_val)[names(pca_val) == "upc.x"] <- "upc"
pca_val$upc.y<-NULL
pca_val$Row.names<-NULL

pca_val <- pca_val[,c(3,1:2,4:8)]

####################################### Pca for salec
salecpc<-subset(salec,select=-c(store.f,month.f,lmove,lprice,upc))
matrixc=data.matrix(salecpc)

salec_apc=matrixc%*%matrixapcfl
salec_apc=data.frame(salec_apc)

pca_testc<-subset(salec,select=c(store.f,month.f,lmove,lprice,upc))
pca_testc <- merge(pca_testc, salec_apc, by=0, all=TRUE)

names(pca_testc)[names(pca_testc) == "upc.x"] <- "upc"
pca_testc$upc.y<-NULL
pca_testc$Row.names<-NULL

pca_testc <- pca_testc[,c(3,1:2,4:8)]

####################################### Pca for sales
salespc<-subset(sales,select=-c(store.f,month.f,lmove,lprice,upc))
matrixs=data.matrix(salespc)

sales_apc=matrixs%*%matrixapcfl
sales_apc=data.frame(sales_apc)

pca_tests<-subset(sales,select=c(store.f,month.f,lmove,lprice,upc))
pca_tests <- merge(pca_tests, sales_apc, by=0, all=TRUE)

names(pca_tests)[names(pca_tests) == "upc.x"] <- "upc"
pca_tests$upc.y<-NULL
pca_tests$Row.names<-NULL

pca_tests <- pca_tests[,c(3,1:2,4:8)]

######################Random Forest using h2o
#Random Forest
h2o.shutdown()
h2o.init()

h2o.train = as.h2o(pca_train)
h2o.val = as.h2o(pca_val)

rf = h2o.randomForest(x=2:8,y=1,training_frame=h2o.train,validation_frame=h2o.val,ntrees=100,mtries=4)

h2o.varimp(rf)
h2o.varimp_plot(rf, num_of_features = NULL)

rf_mse = h2o.mse(rf, valid=T)





#Predict for sale = b validation sample
rfbv_yhat = predict(object=rf,newdata=h2o.val)
rfbv_yhat = as.vector(rfbv_yhat)
salebv1$rfbv_hat=rfbv_yhat

#Predict for Sale = c
pca_testc1<-pca_testc[which(pca_testc$upc !=3800012100),]
pca_testc1<-pca_testc1[which(pca_testc1$upc !=3800012200),]
h2o.testc1 = as.h2o(pca_testc1)
salec1<-salec[which(salec$upc !=3800012100),]
salec1<-salec1[which(salec1$upc !=3800012200),]

rfc_yhat = predict(object=rf,newdata=h2o.testc1)
rfc_yhat = as.vector(rfc_yhat)
salec1$rfc_hat=rfc_yhat


#######Predict for sale = s
a=unique(pca_tests$upc)
b=unique(pca_train$upc)
c=intersect(a,b)
c<-data.frame(c)
names(c)[names(c)=="C"]<-"upc"

pca_tests1<-subset(pca_tests,pca_tests$upc%in%c$c)
sales1<-subset(sales1,sales1$upc%in%c$c)

h2o.tests1 = as.h2o(pca_tests1)

rfs_yhat = predict(object=rf,newdata=h2o.tests1)
rfs_yhat = as.vector(rfs_yhat)
sales1$rfs_yhat=rfs_yhat

save(salebv1, file="salebv1.Rdata")
save(salec1, file="salec1.Rdata")
save(sales1, file="sales1.Rdata")

write.csv(salebv1,'salebvrf.csv')
write.csv(salec1,'salec_rf.csv')
write.csv(sales1,'sales_rf.csv')


rm(list=ls())

#Setting working directory
setwd("C:/Users/agust/Documents/School/Semester III/Machine Learning/ML Project")

#Packages
Packages <- c("knitr","class","dplyr","ggplot2","data.table","glmnet","leaps","foreign","h2o","nlmrt","plotrix","magrittr")
lapply(Packages, library, character.only = TRUE)

#Load Files
load("salebv.Rdata")
load("salec_correct.Rdata")
load("sales_correct.Rdata")
load("kelloggs.Rdata")

#Ensemble method to obtain optimal weights using sale = b validation sample
bweight <- select(salebv1, c("lmove","linear_hat","bss_hat","lasso_hat","rfbv_hat"))

ensemble <- nlxb(lmove ~ (b1 * linear_hat + b2 * bss_hat + b3 * lasso_hat + b4 * rfbv_hat)/(b1 + b2 + b3 + b4),
                 data = bweight,
                 lower = numeric(4),
                 start = list(b1 = 1, b2 = 2, b3 = 3, b4 = 4))
ensemble$coefficients / sum(ensemble$coefficients)

salebv1$ensemble_yhatbv = 0.542*salebv1$linear_hat + 0.428*salebv1$bss_hat + 0.028*salebv1$rfbv_hat

mse_bv = mean((salebv1$lmove - salebv1$ensemble_yhatbv)^2)

salebv1$errorbv = salebv1$lmove - salebv1$ensemble_yhatbv
stde.bv = std.error(salebv1$errorbv)


###################Ensemble predictions for sale = c
salec2$ensemble_yhatc = 0.542*salec2$linear_hat + 0.428*salec2$bss_hat + 0.028*salec2$rfc_hat

mse_c = mean((salec2$lmove - salec2$ensemble_yhatc)^2)

salec2$errorc = salec2$lmove - salec2$ensemble_yhatc

salec2$error2c = salec2$errorc^2

stde.c = std.error(salec2$errorc)

#########################MSE's by month, store, and UPC for sale = c

#Error by Month
mse_monthc = aggregate(salec2[c("error2c")],salec2["month.f"],
                       mean, na.rm=TRUE)
msemax_monthc = mse_monthc[which.max(mse_monthc$error2c),]

#Error by Store
mse_storec = aggregate(salec2[c("error2c")],salec2["store.f"],
                       mean, na.rm=TRUE)
msemax_storec = mse_storec[which.max(mse_storec$error2c),]

error_storec = aggregate(salec2[c("errorc")],salec2["store.f"],
                         mean, na.rm=TRUE)

errormax_storec = error_storec[which.max(error_storec$errorc),]


#Error by UPC
mse_upcc = aggregate(salec2[c("error2c")],salec2["UPC"],
                     mean, na.rm=TRUE)

msemax_upcc = mse_upcc[which.max(mse_upcc$error2c),]

error_upcc = aggregate(salec2[c("errorc")],salec2["UPC"],
                       mean, na.rm=TRUE)

errormax_upcc = error_upcc[which.max(error_upcc$errorc),]
errormin_upcc = error_upcc[which.min(error_upcc$errorc),]



######################Ensemble predictions for sale = s
sales2$ensemble_yhats = 0.542*sales2$linear_hat + 0.428*sales2$bss_hat + 0.028*sales2$rfs_hat

mse_s = mean((sales2$lmove - sales2$ensemble_yhats)^2)

sales2$errors = sales2$lmove - sales2$ensemble_yhats

sales2$error2s = sales2$errors^2

stde.s = std.error(sales2$errors)

###########################MSE's by month, store, and UPC for sale = s
#Error by Month
mse_months = aggregate(sales2[c("error2s")],sales2["month.f"],
                       mean, na.rm=TRUE)

msemax_months = mse_months[which.max(mse_months$error2s),]

#Error by Store
mse_stores = aggregate(sales2[c("error2s")],sales2["store.f"],
                       mean, na.rm=TRUE)

msemax_stores = mse_stores[which.max(mse_stores$error2s),]

error_stores = aggregate(sales2[c("errors")],sales2["store.f"],
                         mean, na.rm=TRUE)

errormax_stores = error_stores[which.max(error_stores$errors),]


#Error by UPC
mse_upcs = aggregate(sales2[c("error2s")],sales2["UPC"],
                     mean, na.rm=TRUE)

msemax_upcs = mse_upcs[which.max(mse_upcs$error2s),]

error_upcs = aggregate(sales2[c("errors")],sales2["UPC"],
                       mean, na.rm=TRUE)

errormax_upcs = error_upcs[which.max(error_upcs$errors),]
errormin_upcs = error_upcs[which.min(error_upcs$errors),]


#############################
mse_bv
mse_c
mse_s

stde.bv
stde.c
stde.s

msemax_storec
msemax_stores

msemax_months
msemax_monthc

msemax_upcc
errormax_upcc
msemax_upcs
errormax_upcs

mean(sales2$errors)
mean(salec2$errorc)




