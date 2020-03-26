# [분산팽창지수를 구하고 VIF 10 이상인 변수 중 가장 큰 값을 순차적으로 제거하는
# R 사용자 정의함수]

# Multi-collinearity check and remove the highly correlated variables step by step

# UDF of stepwise VIF function with preallocated vectors

# code source: https://beckmw.wordpress.com/2013/02/05/collinearity-and-stepwise-vif-selection/
vif_func <- function(in_frame,thresh=10, trace=F,...){
  
  require(fmsb)
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  
  vif_init <- vector('list', length = ncol(in_frame))
  
  names(vif_init) <- names(in_frame)
  
  var_names <- names(in_frame)
  
  for(val in var_names){
    
    regressors <- var_names[-which(var_names == val)]
    
    form <- paste(regressors, collapse = '+')
    
    form_in <- formula(paste(val,' ~ .'))
    
    vif_init[[val]] <- VIF(lm(form_in,data=in_frame,...))
    
  }
  
  vif_max<-max(unlist(vif_init))
  
  if(vif_max < thresh){
    
    if(trace==T){ #print output of each iteration
      
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('', times = nrow(vif_init) ),quote=F)
      
      cat('\n')
      
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
      
    }
    
    return(names(in_frame))
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    
    while(vif_max >= thresh){
      
      vif_vals <- vector('list', length = ncol(in_dat))
      
      names(vif_vals) <- names(in_dat)
      
      var_names <- names(in_dat)
      
      for(val in var_names){
        
        regressors <- var_names[-which(var_names == val)]
        
        form <- paste(regressors, collapse = '+')
        
        form_in <- formula(paste(val,' ~ .'))
        
        vif_add <- VIF(lm(form_in,data=in_dat,...))
        
        vif_vals[[val]] <- vif_add
        
      }
      
      max_row <- which.max(vif_vals)
      
      #max_row <- which( as.vector(vif_vals) == max(as.vector(vif_vals)) )
      
      vif_max<-vif_vals[max_row]
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        
        vif_vals <- do.call('rbind', vif_vals)
        
        vif_vals
        
        prmatrix(vif_vals,collab='vif',rowlab=row.names(vif_vals),quote=F)
        
        cat('\n')
        
        cat('removed: ', names(vif_max),unlist(vif_max),'\n\n')
        
        flush.console()
      }
      in_dat<-in_dat[,!names(in_dat) %in% names(vif_max)]
    }
    return(names(in_dat))
  }
}

X_independent <- vif_func(df_f2, thresh=10, trace=T)
paste(X_independent, collapse = "+")
length(X_independent) # 30
length(df_f2)         # 35
# 제거된 함수  = SHORT_PASSES 25.24992, RIGHT_SIDE 53.54048, SHOT_MIDDLE 93.99055, OUTSIDE_OF_BOX 95.47376, GOALS 174.562 



########
# 다항 분류 지도함수 예측률 사용자 정의 함수
# x = confusion matrix
mul_acc <- function(x){
  total <- 0
  for (i in 1:20) {
    total <-  total + x[i,i]
  }
  acc <- total / sum(x)
}
