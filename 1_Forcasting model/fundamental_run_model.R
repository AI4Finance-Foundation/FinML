

#install and load package
packages.used=c("readxl", "MASS", "ggplot2", 
                "glmnet", "ISLR", "tree",
                "randomForest", "gbm", "e1071","caret")

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}
library(readxl)
library(MASS)
library(ggplot2)
library(glmnet)
library(ISLR)
library(tree)
library(randomForest)
library(gbm)
library(e1071)
library(caret)

source("fundamental_ML_model.R")
source("fundamental_select_stock.R")

####################get data############################
fundamental_total=read_excel("fundamental_final_table.xlsx",1)
trade_date=unique(fundamental_total$tradedate)
trade_date=sort(trade_date)

sector10_data=read_excel("sector10_clean.xlsx",1)
dim(sector10_data)

sector15_data=read_excel("sector15_clean.xlsx",1)
dim(sector15_data)


sector20_data=read_excel("sector20_clean.xlsx",1)
dim(sector20_data)

sector25_data=read_excel("sector25_clean.xlsx",1)
dim(sector25_data)


sector30_data=read_excel("sector30_clean.xlsx",1)
dim(sector30_data)

sector35_data=read_excel("sector35_clean.xlsx",1)
dim(sector35_data)

sector40_data=read_excel("sector40_clean.xlsx",1)
dim(sector40_data)

sector45_data=read_excel("sector45_clean.xlsx",1)
dim(sector45_data)

sector50_data=read_excel("sector50_clean.xlsx",1)
dim(sector50_data)


sector55_data=read_excel("sector55_clean.xlsx",1)
dim(sector55_data)

sector60_data=read_excel("sector60_clean.xlsx",1)
dim(sector60_data)


###############################################################
#####run model and save as RData
###############################################################

######################################
############sector 10 Energy (5238, 32)
######################################
##1.2 hours to run
start.time=Sys.time()
sector10_result=fundamental_ML_model(sector10_data,trade_date)
end.time=Sys.time()
end.time-start.time
save(sector10_result,file = "sector10_result.RData")

######################################
############sector 15 Materials (5216, 32)
######################################
##1.2 hours to run
start.time=Sys.time()
sector15_result=fundamental_ML_model(sector15_data,trade_date)
end.time=Sys.time()
end.time-start.time
save(sector15_result,file = "sector15_result.RData")

######################################
############sector 20 Industrials (9881, 26)
######################################
#2 hours to run
start.time=Sys.time()
sector20_result=fundamental_ML_model(sector20_data,trade_date)
end.time=Sys.time()
end.time-start.time
save(sector20_result,file = "sector20_result.RData")

######################################
############sector 25 Consumer Discretionary (12595, 26)
######################################
#2.5 hours to run
start.time=Sys.time()
sector25_result=fundamental_ML_model(sector25_data,trade_date)
end.time=Sys.time()
end.time-start.time
save(sector25_result,file = "sector25_result.RData")

######################################
############sector 30 Consumer Staples (5388, 29)
######################################
#1.2 hours to run
start.time=Sys.time()
sector30_result=fundamental_ML_model(sector30_data,trade_date)
end.time=Sys.time()
end.time-start.time
save(sector30_result,file = "sector30_result.RData")

######################################
############sector 35 Health Cares (7615, 29)
######################################
#2 hours to run
start.time=Sys.time()
sector35_result=fundamental_ML_model(sector35_data,trade_date)
end.time=Sys.time()
end.time-start.time
save(sector35_result,file = "sector35_result.RData")

######################################
############sector 40 Financials (9480, 21)
######################################
##1.5 hours to run
start.time=Sys.time()
sector40_result=fundamental_ML_model(sector40_data,trade_date)
end.time=Sys.time()
end.time-start.time
save(sector40_result,file = "sector40_result.RData")

######################################
############sector 45 Information Technology (10243, 29)
######################################
##2.5 hours to run
start.time=Sys.time()
sector45_result=fundamental_ML_model(sector45_data,trade_date)
end.time=Sys.time()
end.time-start.time
save(sector45_result,file = "sector45_result.RData")

######################################
############sector 50 Telecomminucation Services (1127, 32)
######################################
#20 mins to run
start.time=Sys.time()
sector50_result=fundamental_ML_model(sector50_data,trade_date)
end.time=Sys.time()
end.time-start.time
save(sector50_result,file = "sector50_result.RData")

######################################
############sector 55 Utilities (3903, 32)
######################################
##1.2 hours to run
start.time=Sys.time()
sector55_result=fundamental_ML_model(sector55_data,trade_date)
end.time=Sys.time()
end.time-start.time
save(sector55_result,file = "sector55_result.RData")

######################################
############sector 60 Real Estate (3039, 32)
######################################
#31 mins to run
start.time=Sys.time()
sector60_result=fundamental_ML_model(sector60_data,trade_date)
end.time=Sys.time()
end.time-start.time
save(sector60_result,file = "sector60_result.RData")

#############################################
#############################################
#############################################
#############################################

###############################################################
################Stock Selection
###############################################################

#########stock selection sector 10
#load("sector10_result.RData")
selector10_modelStock=select_modelStock(sector10_result)
selector10_topStock=select_topStock(selector10_modelStock$selected_stocks)
#########stock selection sector 15
#load("sector15_result.RData")
selector15_modelStock=select_modelStock(sector15_result)
selector15_topStock=select_topStock(selector15_modelStock$selected_stocks)
#########stock selection sector 20
#load("sector20_result.RData")
selector20_modelStock=select_modelStock(sector20_result)
selector20_topStock=select_topStock(selector20_modelStock$selected_stocks)
#########stock selection sector 25
#load("sector25_result.RData")
selector25_modelStock=select_modelStock(sector25_result)
selector25_topStock=select_topStock(selector25_modelStock$selected_stocks)
#########stock selection sector 30
#load("sector30_result.RData")
selector30_modelStock=select_modelStock(sector30_result)
selector30_topStock=select_topStock(selector30_modelStock$selected_stocks)
#########stock selection sector 35
#load("sector35_result.RData")
selector35_modelStock=select_modelStock(sector35_result)
selector35_topStock=select_topStock(selector35_modelStock$selected_stocks)
#########stock selection sector 40
#load("sector40_result.RData")
selector40_modelStock=select_modelStock(sector40_result)
selector40_topStock=select_topStock(selector40_modelStock$selected_stocks)
#########stock selection sector 45
#load("sector45_result.RData")
selector45_modelStock=select_modelStock(sector45_result)
selector45_topStock=select_topStock(selector45_modelStock$selected_stocks)
#########stock selection sector 50
#load("sector50_result.RData")
selector50_modelStock=select_modelStock(sector50_result)
selector50_topStock=select_topStock(selector50_modelStock$selected_stocks)
#selector50_topStock[[82]]=selector50_topStock[[81]]
#########stock selection sector 55
#load("sector55_result.RData")
selector55_modelStock=select_modelStock(sector55_result)
selector55_topStock=select_topStock(selector55_modelStock$selected_stocks)
#########stock selection sector 60
#load("sector60_result.RData")
selector60_modelStock=select_modelStock(sector60_result)
selector60_topStock=select_topStock(selector60_modelStock$selected_stocks)



###############combine stocks together
stocks_selected_total=NULL
for (i in 1:89){
  
  #sector 10
  sector10_temp=selector10_topStock[[i]]
  sector10_temp=cbind(names(sector10_temp),unname(sector10_temp),trade_date[i+20])
  colnames(sector10_temp)=c('tic','predicted_return','trade_date')
  
  #sector 15
  sector15_temp=selector15_topStock[[i]]
  sector15_temp=cbind(names(sector15_temp),unname(sector15_temp),trade_date[i+20])
  colnames(sector15_temp)=c('tic','predicted_return','trade_date')

  #sector 20
  sector20_temp=selector20_topStock[[i]]
  sector20_temp=cbind(names(sector20_temp),unname(sector20_temp),trade_date[i+20])
  colnames(sector20_temp)=c('tic','predicted_return','trade_date')

  #sector 25
  sector25_temp=selector25_topStock[[i]]
  sector25_temp=cbind(names(sector25_temp),unname(sector25_temp),trade_date[i+20])
  colnames(sector25_temp)=c('tic','predicted_return','trade_date')
  
  #sector 30
  sector30_temp=selector30_topStock[[i]]
  sector30_temp=cbind(names(sector30_temp),unname(sector30_temp),trade_date[i+20])
  colnames(sector30_temp)=c('tic','predicted_return','trade_date')
  
  #sector 35
  sector35_temp=selector35_topStock[[i]]
  sector35_temp=cbind(names(sector35_temp),unname(sector35_temp),trade_date[i+20])
  colnames(sector35_temp)=c('tic','predicted_return','trade_date')
  
  #sector 40
  sector40_temp=selector40_topStock[[i]]
  sector40_temp=cbind(names(sector40_temp),unname(sector40_temp),trade_date[i+20])
  colnames(sector40_temp)=c('tic','predicted_return','trade_date')
  
  #sector 45
  sector45_temp=selector45_topStock[[i]]
  sector45_temp=cbind(names(sector45_temp),unname(sector45_temp),trade_date[i+20])
  colnames(sector45_temp)=c('tic','predicted_return','trade_date')
  
  #sector 50
  sector50_temp=selector50_topStock[[i]]
  sector50_temp=cbind(names(sector50_temp),unname(sector50_temp),trade_date[i+20])
  colnames(sector50_temp)=c('tic','predicted_return','trade_date')
  
  #sector 55
  sector55_temp=selector55_topStock[[i]]
  sector55_temp=cbind(names(sector55_temp),unname(sector55_temp),trade_date[i+20])
  colnames(sector55_temp)=c('tic','predicted_return','trade_date')

  
  #sector 60
  sector60_temp=selector60_topStock[[i]]
  sector60_temp=cbind(names(sector60_temp),unname(sector60_temp),trade_date[i+20])
  colnames(sector60_temp)=c('tic','predicted_return','trade_date')

  
  stocks_bind=rbind(sector10_temp,
                    sector15_temp,
                    sector20_temp,
                    sector25_temp,
                    sector30_temp,
                    sector35_temp,
                    sector40_temp,
                    sector45_temp,
                    sector50_temp,
                    sector55_temp,
                    sector60_temp)

  stocks_selected_total=rbind(stocks_selected_total,stocks_bind)

}

stocks_selected_total=as.data.frame(stocks_selected_total)


write.csv(stocks_selected_total,"stocks_selected_total.csv")
