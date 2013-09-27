f.upd<-function(data,y,folds=10){  
  selected <- list()
  score_hist <- list()
  while(length(score_hist) < 2 || last(score_hist) < last(score_hist,1)){
    if(length(selected) > 0){
      newdata<-lapply(data,cbind,selected,y=y)
    }
    else{
      newdata<-lapply(data,cbind,y=y)      
    }
    res<-lapply(newdata,cv3,folds)
    min.index <- which.min(res)
    score_hist<-c(score_hist,res[[min.index]])
    cat(names(min.index),res[[min.index]],'\n')
    selected <- c(selected,subset(data,select=min.index))
    data <- data[,-min.index]
  }
  return(list(selected=head(selected,-1),score_hist=score_hist))
}