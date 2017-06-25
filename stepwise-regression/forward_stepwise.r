#1. Write R code implementing the forward variable selection
# procedure in that paper, Section 5.1 "Forward Variable Selection"
# page 132.
#Test your code and explain why you think your code is correct.

### TO DO, FORWARD
# -diff measures besides RSS
# -standard error measurement
# -column index instead of string iv
# -cv / test set
# -full verbosity

# Although you can change this to something besides RSS, if you change it to something like
# R^2, you will need to make changes elsewhere (ie (if < ...) ---> (if > ...) )
lm.performance<-function(mod){
  return(sum(resid(mod)^2))
}

# transforms variables into lm string, ie "w~x+y+..."
my.paste<-function(iv,already.included,new.can){
  str.one<-paste0(iv,"~",new.can)
  if (!is.null(already.included)) {
    for (i in 1:length(already.included)){
      str.one<-paste0(str.one,"+",already.included[i])
    }
  }
  return(str.one)
}

# recreates x~y var list to re-call model on new data
my.paste.two<-function(mod,iv){
  vars<-setdiff(names(mod$model),iv)
  #print(vars)
  #print(length(vars))
  str.one<-paste0(iv,"~",vars[1])
  #print(str.one)
  if (length(vars)>1){
    for (i in 2:length(vars)){
      str.one<-paste0(str.one,"+",vars[i])
    }
  }
  return(str.one)
}

# Workhorse- adds the next best variable
get.best<-function(data,candidates,iv,already.included){
  init.mod<-do.call("lm",list(my.paste(iv,already.included,candidates[1]),data))
  min.rss<-lm.performance(init.mod)
  best.mod<-init.mod
  addl.var<-candidates[1]
  for (i in 2:length(candidates)){
    lin.mod<-do.call("lm",list(my.paste(iv,already.included,candidates[i]),data))
    resids<-lm.performance(lin.mod)
    if (resids<min.rss) {
      min.rss<-resids
      best.mod<-lin.mod
      addl.var<-candidates[i]
    }
  }
  return(best.mod)
}

print_output_old<-function(performance,vars,std.err){
  #print(errors)
  print(performance)
  print(vars)
  print(std.err)
}

print_output<-function(performance,vars,std.err,min,minse){
  print(paste("length_performance=",length(performance)))
  if (length(performance)!=length(vars) || 
      length(vars)!=length(std.err)){
    print("OH FU-")
    # raise error here
  }
  for (i in 1:length(performance)){
    print(paste("i=",i))
    if (i==1){
      print("1 VAR")
    }else{
      print(paste(i,"VARS"))
    }
    print(performance[i])
    print(vars[[i]])
    if (performance[i]==min){
      print("This model has the lowest RSS")
    }
    if (performance[i]==minse){
      print("This model is the simplst within 1 SE of the minimum")
    }
    if (i != length(performance)){
      print("----------")
    }
  }
}

# estimates standard error of RSS
boot.est<-function(data,lin.mod,iv){
  b<-500
  error.vec<-rep(NA,b)
  for (i in 1:b){
    boot.inds<-sample(1:nrow(data),nrow(data),replace=TRUE)
    boot.real<-data[boot.inds,]
    new.mod<-do.call("lm",list(my.paste.two(lin.mod,iv),boot.real))
    error.vec[i]<-lm.performance(new.mod)
  }
  return(sd(error.vec))
}

find.minse<-function(rss,lb,ub,min){
  min.ind<-get.index(rss,min)
  for (i in 1:length(rss)){
    if (rss[i])  
  }
}

get.index<-function(arr,val){
  #print(paste("searching for",val))
  for (i in 1:length(arr)){
    if (arr[i]==val){
      return(i)
    }
  }
  # raise error here
  print("OH SHI-")
  return(NA)
}

# greedy search through possible linear models
# does not consider variable transformations or interactions
# user passes a string denoting which variable is the one to be predicted
# a more robust implementation would allow a column index as well

# Doesn't do CV/test set automatically. so call it on both your training and validation sets.
lm.forward<-function(data,iv,plotting,verbose){
  init.poss<-setdiff(names(data),iv)
  rss<-rep(NA,length(init.poss))
  se<-rep(NA,length(init.poss))
  list.vars<-list()
  lm.best<-get.best(data,init.poss,iv,NULL)
  rss[1]<-lm.performance(lm.best)
  se[1]<-boot.est(data,lm.best,iv)
  best.measure<-rss[1]
  used<-setdiff(names(lm.best$model),iv)
  #list.vars[1]<-names(lm.best$model)
  list1<-list(used)
  list.vars[1]<-list1
  poss<-setdiff(init.poss,used)
  for (i in 2:length(poss)){
    lm.next<-get.best(data,poss,iv,used)
    rss[i]<-lm.performance(lm.next)
    se[i]<-boot.est(data,lm.next,iv)
    if (rss[i]<best.measure){
      best.measure<-rss[i]
      lm.best<-lm.next
    }
    used<-setdiff(names(lm.next$model),iv)
    list1<-list(used)
    list.vars[i]<-list1
    poss<-setdiff(init.poss,used)
  }
  # get.best can't get the final ~. model, so I have to do it outside the loop
  full.mod<-do.call("lm",list(my.paste(iv,init.poss[-1],init.poss[1]),data))
  rss[length(init.poss)]<-lm.performance(full.mod)
  se[length(init.poss)]<-boot.est(data,full.mod,iv)
  list1<-list(setdiff(names(full.mod$model),iv))
  list.vars[length(init.poss)]<-list1
  if (rss[length(init.poss)]<best.measure){
    best.measure<-rss[length(init.poss)]
    lm.best<-full.mod
  }
  lb<-rep(NA,length(se))
  ub<-rep(NA,length(se))
  for (i in 1:length(lb)){
    lb[i]<-rss[i]-se[i]
    ub[i]<-rss[i]+se[i]
  }
  minimum<-min(rss)
  minse<-find.minse(rss,lb,ub,minimum)
  #print(names(fin.mod$model))
  if (verbose){
    print(paste("min=",minimum))
    print(paste("minse=",minse))
    print_output(rss,list.vars,se,minimum,minse)
  }
  if (plotting){
    require(ggplot2)
    num.vars<-seq(1,length(names(data))-1)
    x3<-as.data.frame(cbind(num.vars,rss))
    g<-ggplot(x3,aes(num.vars,rss))+
      geom_point()+ggtitle("RSS vs Predictors in Forward Stepwise Regression")+
      geom_errorbar(aes(ymin=rss-se, ymax=rss+se), width=.1)
    print(g)
  }
  return(lm.best)
}

x<-lm.forward(mtcars,"mpg",TRUE,TRUE)
x<-lm.forward(npk,"yield",TRUE,TRUE)

