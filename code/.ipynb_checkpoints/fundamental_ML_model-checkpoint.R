fundamental_ML_model <- function(sector_data,trade_date){
  #######################################################
  #1. model test error to select models
  #2. trade period predicted return to select stocks
  #3. linear regression features
  #4. random forest features
  #5. ridge features
  #6. stepwise regression features
  #7. gbm features
  #sector_data=sector45_data
  
  #look at the data determine the first factor column number
  start_column=12

  #set the rows to 89, because we have 89 stock selections  
  #may need to adjust and put into function
  
  #model error to select model
  model_error=data.frame(MSE_linear=replicate(89,0))
  model_error[,2]=data.frame(MSE_RF=replicate(89,0))
  model_error[,3]=data.frame(MSE_ridge=replicate(89,0))
  model_error[,4]=data.frame(MSE_step=replicate(89,0))
  model_error[,5]=data.frame(MSE_gbm=replicate(89,0))
  
  #predicte return to select stocks
  predicted_return=list()
  
  
  
  #main model
  LR_features=list()
  RF_features=list()
  ridge_features=list()
  
  Step_features=list()
  GBM_features=list()
  
  #for(i in 1:(length(trade_date)-19)){RF_features[[i]]=c(1:i)}
  
  #understand rolling windows
  #for(i in 1:(length(trade_date)-19)){print(c(i,i+15,i+16,i+19,trade_date[i+20]))}
  
  for(i in 1:(length(trade_date)-21)){
    
    ###############################################
    ###########rolling window########################
    
    ####train the model based on 4 years, 16 quarters data
    #growing window 10 years
    if (i<=25) {
      data_train=sector_data[(sector_data$tradedate <= trade_date[i+15]),]
      train_x=data_train[,c(start_column:(dim(sector_data)[2]-1))]
      train_y=data_train[,dim(sector_data)[2]]
    } else{
      data_train=sector_data[(sector_data$tradedate <= trade_date[i+15]) & sector_data$tradedate >= trade_date[i-25],]
      train_x=data_train[,c(start_column:(dim(sector_data)[2]-1))]
      train_y=data_train[,dim(sector_data)[2]]
    }
    
    ####test the model based on 1 year, 4 quarters data
    data_test=sector_data[(sector_data$tradedate <= trade_date[i+19]) & (sector_data$tradedate >= trade_date[i+16]),]
    test_x=data_test[,c(start_column:(dim(sector_data)[2]-1))]
    test_y=data_test[,dim(sector_data)[2]]
    
    train=cbind(train_y,train_x)
    test=cbind(test_y,test_x)
    
    
    ####trade data for every quarter
    data_trade=sector_data[(sector_data$tradedate == trade_date[i+20]),]
    trade_x=data_trade[,c(start_column:(dim(sector_data)[2]-1))]
    trade_y=data_trade[,dim(sector_data)[2]]
    trade=cbind(trade_x,trade_y)
    
    row.names(trade_x)=data_trade$tic
    
    ###########################################
    ##############linear regression############
    ###########################################
    linear_model=lm(y_return~., data=train)
    linear_pre_y=predict(linear_model,test_x)
    MSE_linear=mean((test_y-linear_pre_y)^2,na.rm=TRUE)
    #MSE_linear
    
    #LR features
    LR_features[[i]]=summary(linear_model)
    
    ###########################################
    ################Random Forest##############
    ###########################################
    # Tune using algorithm tools
    # Tunning the mtry
    bestmtry <- tuneRF(train[,-1],train[,1], stepFactor=1.5, improve=1e-5, ntree=500,trace=0,plot = FALSE)
    #plot(bestmtry,type = "l")
    bestmtry=data.frame(bestmtry)
    mytry_optimal=bestmtry$mtry[which.min(bestmtry$OOBError)]
    #mytry_optimal
    RF_Model=randomForest(y_return~.,data = train,ntree=500,mtry=mytry_optimal,importance=TRUE, na.rm = T,trace=0)
    
    yhat_bag=predict(RF_Model,test_x)
    MSE_RF=mean((yhat_bag-test_y)^2)
    #MSE_RF
    #importance table
    #varImp(RF_Model)
    #varImpPlot(RF_Model,main='Random Forest Importance Table')
    
    ########RF features
    RF_features[[i]]=varImp(RF_Model)
    
    #####################################
    ################ridge################
    #####################################
    x_train_ridge=model.matrix(y_return~., train)[,-1]
    y_train_ridge=train$y_return
    
    x_test_ridge=model.matrix(y_return~.,test)[,-1]
    y_test_ridge=test$y_return
    
    #tunning for lambda
    #first run ridge on training set and pick the best lambda
    cv.out_ridge=cv.glmnet(x_train_ridge,y_train_ridge,alpha=1)
    bestlam_ridge=cv.out_ridge$lambda.min
    
    ridge_model=glmnet(x_train_ridge,y_train_ridge,alpha = 0,lambda = bestlam_ridge)
    ridge_pred_y=predict(ridge_model, newx = x_test_ridge)
    
    MSE_ridge=mean((ridge_pred_y-y_test_ridge)^2,na.rm=TRUE)
    
    #ridge features
    ridge_coeffs <- coef(ridge_model)
    ridge_coef=data.frame(name = ridge_coeffs@Dimnames[[1]][ridge_coeffs@i + 1], coefficient = ridge_coeffs@x)
    
    ridge_features[[i]]=ridge_coef
    
    
    
    ###########################################
    ##############stepwise regression##########
    ###########################################
    #based on linear regresion
    step_model=stepAIC(linear_model, direction="both",trace = 0)
    step_pre_y=predict(step_model,test_x)
    
    MSE_step=mean((test_y-step_pre_y)^2,na.rm=TRUE)
    #MSE_step
    
    #step features
    Step_features[[i]]=summary(step_model)
    
    
    ###################################
    ################GBM################
    ###################################
    #Generalized Boosted Regression Models
    gbm_model=gbm(y_return~.,data = train,
                  dist="gaussian",
                  n.tree = 400,
                  shrinkage=0.1, 
                  cv.folds = 5)
    
    gbm_pred_y = predict(gbm_model, test, n.tree = 400, type = 'response')
    MSE_gbm=mean((gbm_pred_y-test_y)^2,na.rm=TRUE)
    #MSE_gbm
    ########GBM features
    GBM_features[[i]]=  summary(gbm_model,plot=FALSE)
    
    ######################################
    #############get results#############
    ######################################

    
    
    #####################################
    #all model trade data
    #trade using linear regression
    trade_linear_y=predict(linear_model,trade_x)
    #trade using random forest
    trade_RF_y=predict(RF_Model,trade_x)
    #trade using ridge
    x_trade_ridge=model.matrix(y_return~.,trade)[,-1]
    row.names(x_trade_ridge)=data_trade$tic
    trade_ridge_y=predict(ridge_model,x_trade_ridge)
    colnames(trade_ridge_y)=c('trade_ridge_y')

    #trade stepwise regression
    trade_step_y=predict(step_model,trade_x)
    #trade using GBM
    trade_GBM_y=predict(gbm_model,trade_x)
    
    ###########store model error
    if (length(unique(trade_linear_y))<length(trade_linear_y)*0.2){
      MSE_linear=NA
    }
    
    if(length(unique(trade_RF_y))<length(trade_RF_y)*0.2){
      MSE_RF=NA
    }
    
    if(length(unique(trade_ridge_y))<length(trade_ridge_y)*0.2){
      MSE_ridge=NA
    }
    
    
    if(length(unique(trade_step_y))<length(trade_step_y)*0.2){
      MSE_step=NA
    }
    
    if(length(unique(trade_GBM_y))<length(trade_GBM_y)*0.2){
      MSE_gbm=NA
    }
    
    model_error[i,]=c(MSE_linear,MSE_RF,MSE_ridge,MSE_step,MSE_gbm)
    
    #store all the predicted returns
    temp_return=cbind(trade_linear_y,trade_RF_y,trade_ridge_y,trade_step_y,trade_GBM_y)
    predicted_return[[i]]=temp_return
    
  }
  
  output=list(model_error=model_error, 
              predicted_return=predicted_return, 
              
              LR_features=LR_features, 
              RF_features=RF_features, 
              ridge_features=ridge_features,
              
              Step_features=Step_features, 
              GBM_features=GBM_features
              )
  return(output)
  
  
  
}