select_modelStock = function(sector_result){
  #sector_result=sector10_result
  selected_model=NULL
  selected_stocks=list()
  
  for (i in 1:89){
  get_minIndex= apply(sector_result$model_error[i,],1,which.min)
  selected_model[i]=colnames(sector_result$model_error[i,])[apply(sector_result$model_error[i,],1,which.min)]
  selected_stocks[[i]] = sector_result$predicted_return[[i]][,get_minIndex]

  }
  
  output=list(selected_stocks=selected_stocks,selected_model=selected_model)
  return(output)
}



select_topStock=function(selected_stocks){
  selected_topstocks=list()
  
  for (i in 1:89){
    selected_topstocks[[i]]=selected_stocks[[i]][selected_stocks[[i]]>=quantile(selected_stocks[[i]],0.8)]
  } 
  return(selected_topstocks)
}