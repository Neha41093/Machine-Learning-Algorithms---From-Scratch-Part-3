#!/usr/bin/env Rscript
#Question 6 - Implement decision tree on 3 datasets

#rm(list=ls())
#set according to your working directory
#old.dir <- getwd()
#setwd("C:/Users/Neha Rawat/Desktop/IU-Data Science/Data Mining/Assignment 1")

#Function for calculating Gini index
GiniIndexFunction <- function(data){
  target.labels <- as.matrix(unique(data))
  prob.matrix <- apply(target.labels,1,function(x) length(data[which(data==x)])/length(data))
  label.frame <- data.frame(target.labels,prob.matrix)
  gini.step1 <- apply(as.matrix(prob.matrix),1,function(x) x*x)
  gini.final <- 1 - sum(gini.step1)
  return(gini.final)
}

#Function for calculating weighted Gini index
GiniWeightedFunction <- function(data,target,flag,thresh){
  gini.list <- numeric()
  weight.list <- numeric()
  if(flag==1){
    return (GiniIndexFunction(target))
  } else if (flag==2){
    vals <- unique(data)
    gini.list <- apply(as.matrix(vals),1,function(x) GiniIndexFunction(target[which(data==x)]))
    weight.list <- apply(as.matrix(vals),1,function(x) length(data[which(data==x)])/length(data))
    gini.wt <- (gini.list[1]*weight.list[1]) + (gini.list[2]*weight.list[2])
  } else if (flag==0){
    gini.list[1] <- GiniIndexFunction(target[which(data<=thresh)])
    gini.list[2] <- GiniIndexFunction(target[which(data>thresh)])
    weight.list[1] <- length(data[which(data<=thresh)])/length(data)
    weight.list[2] <- length(data[which(data >thresh)])/length(data)
    gini.wt <- (gini.list[1]*weight.list[1]) + (gini.list[2]*weight.list[2])
  }
  return(gini.wt)
}

#Function for calculating Entropy for Information Gain
EntropyForGainFunction <- function(data){
  target.labels <- as.matrix(unique(data))
  prob.matrix <- apply(target.labels,1,function(x) length(data[which(data==x)])/length(data))
  label.frame <- data.frame(target.labels,prob.matrix)
  ent.step1 <- apply(as.matrix(prob.matrix),1,function(x) -x*(log2(x)))
  ent.final <- sum(ent.step1)
  return(ent.final)
}

#Function for calculating Weighted Entropy for Information Gain
EntropyWeightedFunction <- function(data,target,flag,thresh){
  ent.list <- numeric()
  weight.list <- numeric()
  if(flag==1){
    return (EntropyForGainFunction(target))
  } else if (flag==2){
    vals <- unique(data)
    ent.list <- apply(as.matrix(vals),1,function(x) EntropyForGainFunction(target[which(data==x)]))
    weight.list <- apply(as.matrix(vals),1,function(x) length(data[which(data==x)])/length(data))
    ent.wt <- (ent.list[1]*weight.list[1]) + (ent.list[2]*weight.list[2])
  } else if (flag==0){
    ent.list[1] <- EntropyForGainFunction(target[which(data<=thresh)])
    ent.list[2] <- EntropyForGainFunction(target[which(data>thresh)])
    weight.list[1] <- length(data[which(data<=thresh)])/length(data)
    weight.list[2] <- length(data[which(data >thresh)])/length(data)
    ent.wt <- (ent.list[1]*weight.list[1]) + (ent.list[2]*weight.list[2])
  }
  return(ent.wt)
}

#Splitting and Creating Tree for Training Data
#Convergence criteria: breaks if change less than 0.001 or tree level > 5
SplitTreeTrainFunction <- function(data, target_index,method_num){
  
  data.dim <- dim(data)
  data.att <- c(1:(data.dim[2] - 1))
  att.select <- 0
  old.data <- list(data)
  old.index <- length(old.data)
  thresh.check <- 0.001
  thresh.level <- 5
  purity.valuenew <- numeric()
  train.values <- data.frame(Level=integer(),Datasets=integer(),Attribute=integer(),Threshold=integer(),OrgLabel=integer(),stringsAsFactors=FALSE)
  lab_choose <- integer()
  final.results <- list()
  
  f=0
  a=0
  prev_att_sel <- 0
  
  #Outermost loop - for tree levels
  while(a < thresh.level)
  {
    a=a+1
    m=0
    purity.info <- numeric()
    new.data <- list()

    #old data gini/info gain
    if (method_num == 1){
      purity.valueold1 <- apply(as.matrix(c(1:old.index)),1,function(x) GiniIndexFunction(old.data[[x]][,target_index]))
      purity.valueold <- mean(purity.valueold1)
    } else{
      purity.valueold1 <- apply(as.matrix(c(1:old.index)),1,function(x) EntropyForGainFunction(old.data[[x]][,target_index]))
      purity.valueold <- mean(purity.valueold1)
    }
    
    #For data splits
    for(i in c(1:old.index)){
      data.var <- old.data[[i]]
      t = 1
      if (i == 1){
      data.att <- c(1:(dim(data.var)[2] - 1))
      data.dimrange <- data.att[!data.att %in% prev_att_sel]
      }
      else{
        data.dimrange <- data.att[!data.att %in% prev_att_sel] 
      }
      
      #if only one target label -- pure cluster formed
      if (length(unique(data.var[,target_index])) == 1){
        t = 0
        m <- m+1
        new.data[[m]] <- data.var
        if(method_num == 1){
          purity.info[i] = GiniIndexFunction(data.var[,target_index])
        } else if(method_num == 2){
          purity.info[i] = EntropyForGainFunction(data.var[,target_index])
        }
      } 
      #otherwise first find the suitable attribute to split on
      else{
        p = 0
        check.vector <- numeric()
        break.vector <- numeric()
        
        #Iterating within the different attributes
        for(j in data.dimrange){
          p <- p+1
          unique.vals <- unique(data.var[,j])
         
          if (length(unique.vals) == 1){ #All same attribute values
            break.vector[p] = 999
            if(method_num == 1){
              check.vector[p] <- GiniIndexFunction(data.var[,target_index])
            } else if(method_num == 2){
              check.vector[p] <- EntropyForGainFunction(data.var[,target_index])
              }
          } else if (length(unique.vals) == 2) { #Two distinct attribute values
            break.vector[p] = 888
            if(method_num == 1){
              check.vector[p] <- GiniWeightedFunction(data.var[,j],data.var[,target_index],2,0)
            } else if(method_num == 2){
              check.vector[p] <- EntropyWeightedFunction(data.var[,j],data.var[,target_index],2,0)
            }
          } else if (length(unique.vals) > 2){ #Continuous attribute values
            #for contiuous variables
            target_check <- unique(data.var[,target_index])
            splitby.label <- list()
            for (e in c(1:2)){
            splitby.label[[e]] <- data.var[which(data.var[,target_index]==target_check[e]),j]}
            bring.together <- c(splitby.label[[1]],splitby.label[[2]])
            freq.table <- as.data.frame(table(bring.together))
            thresh.list <- as.integer(paste(freq.table[which(freq.table[,2]>1),1]))
            if (length(thresh.list) == 0){
              thresh.list <- as.integer(paste(freq.table[,1]))
            }
            if(method_num == 1){
              gini.clist <- apply(as.matrix(thresh.list),1,function(x) GiniWeightedFunction(data.var[,j],data.var[,target_index],0,x))
              thresh.fin <- thresh.list[which.min(gini.clist)]
              break.vector[p] = thresh.fin
              check.vector[p] <- GiniWeightedFunction(data.var[,j],data.var[,target_index],0,thresh.fin)
            } else if (method_num == 2){
              ent.clist <- apply(as.matrix(thresh.list),1,function(x) EntropyWeightedFunction(data.var[,j],data.var[,target_index],0,x))
              thresh.fin <- thresh.list[which.min(ent.clist)]
              break.vector[p] = thresh.fin
              check.vector[p] <- EntropyWeightedFunction(data.var[,j],data.var[,target_index],0,thresh.fin)
            }
          }
          
        }
        #selecting best split using the Gini/Info Gain values
        brk.chk <- break.vector[!break.vector %in% c(999,888,1000)] 
        if((length(unique(check.vector)) == 1) & (length(brk.chk) >= 1)){
          min.index <- which(break.vector == median(brk.chk))
        }
        min.index <- which.min(check.vector)
        att.select <- data.dimrange[min.index]
        prev_att_sel <- att.select
        data.att = data.dimrange
        purity.info[i] = check.vector[min.index] 
        
        #Splitting the data according to the best split
        if (break.vector[min.index] == 999){
          m <- m+1
          new.data[[m]] <- data.var
        } else if (break.vector[min.index] == 888){
          vals.two <- sort(unique(data.var[,att.select]))
          m <- m+1
          new.data[[m]] <- data.var[which(data.var[,att.select]== vals.two[1]),]
          m <- m+1
          new.data[[m]] <- data.var[which(data.var[,att.select]!= vals.two[1]),]
        } else {
          m <- m+1
          new.data[[m]] <- data.var[which(data.var[,att.select] <= break.vector[min.index]),]
          m <- m+1
          new.data[[m]] <- data.var[which(data.var[,att.select] > break.vector[min.index]),]
        }   
        
      }
      
      f <- f+1
      
      #Labels of the parent dataset (according to max probability)
      labs_chk1 <- unique(data.var[,target_index])
      labs_probs1 <- apply(as.matrix(labs_chk1),1,function(x) nrow(data.var[which(data.var[,target_index]==x),])/nrow(data.var))
      train.values[f,5] <- labs_chk1[which.max(labs_probs1)]
      
      #Storing parameter values for the tree - tree level, node number, attribute selected, threshold value, label of current node
      train.values[f,1] <- a
      train.values[f,2] <- i
      train.values[f,3] <- ifelse(t==1,att.select,1000)
      train.values[f,4] <- ifelse(t==1,break.vector[min.index],1000)
    }
    
    #Replacing old data with the new split data. Taking the average of the gini/gain for the new level
    old.data <- new.data
    old.index <- length(new.data)
    purity.valuenew[a] <- mean(purity.info)
    
    #Getting the labels of the new datasets/children nodes
    for (g in 1:length(new.data)){
      data_temp <- new.data[[g]]
      labs_chk <- unique(data_temp[,target_index])
      labs_probs <- apply(as.matrix(labs_chk),1,function(x) nrow(data_temp[which(data_temp[,target_index]==x),])/nrow(data_temp))
      lab_choose[g] <- labs_chk[which.max(labs_probs)]
    }
    
    #Consolidating tree information and final labels
    final.results[[1]] <- train.values
    final.results[[2]] <- lab_choose
    
    #Checking for convergence - difference between parent and child level purity factor is negligible
    if((method_num == 1) & (abs(purity.valueold - purity.valuenew[a]) <= thresh.check)){
      print("Gini almost stable")
      return(final.results)
    } else if ((method_num == 2) & (abs(purity.valueold - purity.valuenew[a]) <= thresh.check)){
      print("Information Gain almost stable")
      return(final.results)
    }
    
  }
  #Final result list returned
  return(final.results)
  }


#Main Function with the testing of datasets on the model
MainTreeTestFunction <- function()
{ 
  #Uncomment when using command line
  args <- commandArgs(trailingOnly = TRUE)
  filename <- args[2]
  method_val <- args[1]
  
  #Comment when using command line. Use for running on R Studio.
  #method_val = "Info"
  #filename = "tic_tac_toe.csv"
  
  method_num = 0
  target.ind <- 0
  test_indices <- list()
  remain <- list()
  acc.fin <- numeric()
  acc.val <- numeric()
  acc <- numeric()
  test_data <- list()
  train_data <- list()
  
  data_tree <- read.csv(file = filename,header = FALSE,stringsAsFactors = FALSE)
  data_tree <- as.data.frame(data_tree)
  
  #Parameters for both the method names
  if (method_val == "Gini"){
    method_num = 1
  } else if (method_val == "Info"){
    method_num = 2
  } else {
    print("Incorrect Method Name")
    break
  }
  
  #Data processing for the three datasets
  if (filename == "tic_tac_toe.csv"){
    dum<- data_tree
    tttv1 <- model.matrix(~.-1,data = data.frame(dum$V1))
    tttv2 <- model.matrix(~.-1,data = data.frame(dum$V2))
    tttv3 <- model.matrix(~.-1,data = data.frame(dum$V3))
    tttv4 <- model.matrix(~.-1,data = data.frame(dum$V4))
    tttv5 <- model.matrix(~.-1,data = data.frame(dum$V5))
    tttv6 <- model.matrix(~.-1,data = data.frame(dum$V6))
    tttv7 <- model.matrix(~.-1,data = data.frame(dum$V7))
    tttv8 <- model.matrix(~.-1,data = data.frame(dum$V8))
    tttv9 <- model.matrix(~.-1,data = data.frame(dum$V9))
    ttv10 <- apply(as.matrix(1:nrow(dum)),1,function(x) ifelse(dum[x,10] == "positive",1,0))
    data_tree <- data.frame(tttv1,tttv2,tttv3,tttv4,tttv5,tttv6,tttv7,tttv8,tttv9,ttv10)
    
    #place target class at the last column
    target.ind <- ncol(data_tree)
    set.seed(520)
    all_indices <- c(1:nrow(data_tree))
    test_indices[[1]] <- sample(nrow(data_tree),192)
    remain[[1]] <- all_indices[! all_indices %in% test_indices[[1]]]
    test_indices[[2]] <- sample(remain[[1]],192)
    remain[[2]] <- remain[[1]][! remain[[1]] %in% test_indices[[2]]]
    test_indices[[3]] <- sample(remain[[2]],192)
    remain[[3]] <- remain[[2]][! remain[[2]] %in% test_indices[[3]]]
    test_indices[[4]] <- sample(remain[[3]],192)
    remain[[4]] <- remain[[3]][! remain[[3]] %in% test_indices[[4]]]
    test_indices[[5]] <- sample(remain[[4]],190)
    remain[[5]] <- remain[[4]][! remain[[4]] %in% test_indices[[5]]]
    
    
    #Creating datasets
    for ( i in c(1:5)){
      test_data[[i]] <- data_tree[test_indices[[i]],]
      train_data[[i]] <- data_tree[-test_indices[[i]],]
    }
  }
  
  if (filename == "ionosphere_data.csv"){
    dum<- data_tree
    ttv10 <- apply(as.matrix(1:nrow(dum)),1,function(x) ifelse(dum[x,35] == "g",1,0))
    data_tree <- data.frame(dum[,c(1,3:34)],ttv10)
    
    #place target class at the last column
    target.ind <- ncol(data_tree)
    set.seed(520)
    all_indices <- c(1:nrow(data_tree))
    test_indices[[1]] <- sample(nrow(data_tree),70)
    remain[[1]] <- all_indices[! all_indices %in% test_indices[[1]]]
    test_indices[[2]] <- sample(remain[[1]],70)
    remain[[2]] <- remain[[1]][! remain[[1]] %in% test_indices[[2]]]
    test_indices[[3]] <- sample(remain[[2]],70)
    remain[[3]] <- remain[[2]][! remain[[2]] %in% test_indices[[3]]]
    test_indices[[4]] <- sample(remain[[3]],70)
    remain[[4]] <- remain[[3]][! remain[[3]] %in% test_indices[[4]]]
    test_indices[[5]] <- sample(remain[[4]],71)
    remain[[5]] <- remain[[4]][! remain[[4]] %in% test_indices[[5]]]
    
    
    #Creating datasets
    for ( i in c(1:5)){
      test_data[[i]] <- data_tree[test_indices[[i]],]
      train_data[[i]] <- data_tree[-test_indices[[i]],]
    }
    
  }
  
  
  if (filename == "wisconsin_data.csv"){
    dum<- data_tree
    #cleaning the data -- removing missing value records
    na_value_l <- length(which(dum[,7] == "?"))
    na_values <- which(dum[,7] == "?")
    data_rows <- c(1:699)
    accepted_rows <- as.matrix(which(!(data_rows %in% na_values)))
    wisconsin_data <- t(simplify2array(as.matrix(apply(accepted_rows, 1, function(x) strtoi(dum[x,c(2:11)]))),higher= FALSE))
    data_tree <- as.data.frame(wisconsin_data)
    
    #place target class at the last column
    target.ind <- ncol(data_tree)
    set.seed(520)
    all_indices <- c(1:nrow(data_tree))
    test_indices[[1]] <- sample(nrow(data_tree),137)
    remain[[1]] <- all_indices[! all_indices %in% test_indices[[1]]]
    test_indices[[2]] <- sample(remain[[1]],137)
    remain[[2]] <- remain[[1]][! remain[[1]] %in% test_indices[[2]]]
    test_indices[[3]] <- sample(remain[[2]],137)
    remain[[3]] <- remain[[2]][! remain[[2]] %in% test_indices[[3]]]
    test_indices[[4]] <- sample(remain[[3]],137)
    remain[[4]] <- remain[[3]][! remain[[3]] %in% test_indices[[4]]]
    test_indices[[5]] <- sample(remain[[4]],135)
    remain[[5]] <- remain[[4]][! remain[[4]] %in% test_indices[[5]]]
    
    
    #Creating datasets
    for ( i in c(1:5)){
      test_data[[i]] <- data_tree[test_indices[[i]],]
      train_data[[i]] <- data_tree[-test_indices[[i]],]
    }
    
  }
  
  #Main Testing Part
  for (q in c(1:5)){
    train.results <- SplitTreeTrainFunction(train_data[[q]],target.ind,method_num)
  
    #Testing using the training parameters
    test.dat <- test_data[[q]]
    train.parameters <- train.results[[1]]
    final.labels <- train.results[[2]]
    
    old.dat <- list(test.dat)
    old.indices <- length(old.dat)
    
    chk = 0
    max.levels <- max(train.parameters[,1])
    for (i in c(1:max.levels)){
      new.dat <- list()
      m=0
      
      for (j in c(1:old.indices)){
        dat.var = old.dat[[j]]
        sub.set1 = train.parameters[which(train.parameters[,1] == i),]
        sub.set = sub.set1[which(sub.set1[,2] == j),]
        
        if ((sub.set[,4] == 1000) & (sub.set[,3] == 1000)){
          m <- m+1
          new.dat[[m]] <- dat.var
        }else if (sub.set[,4] == 999){
          m <- m+1
          new.dat[[m]] <- dat.var
        } else if (sub.set[,4] == 888){
          vals.two <- sort(unique(dat.var[,sub.set[,3]]))
          if (length(vals.two) == 1){
            chk <- chk + 1
            m <- m+1
            new.dat[[m]] <- dat.var 
          } else {
          m <- m+1
          new.dat[[m]] <- dat.var[which(dat.var[,sub.set[,3]]== vals.two[1]),]
          m <- m+1
          new.dat[[m]] <- dat.var[which(dat.var[,sub.set[,3]]!= vals.two[1]),]
          }
        } else {
          if (nrow(dat.var[which(dat.var[,sub.set[,3]] <= sub.set[,4]),]) < 1){
            chk <- chk+1
            m <- m+1
            new.dat[[m]] <- dat.var
            } else if (nrow(dat.var[which(dat.var[,sub.set[,3]] > sub.set[,4]),]) < 1){
              chk <- chk + 1
              m <- m+1
              new.dat[[m]] <- dat.var
            } else{
          m <- m+1
          new.dat[[m]] <- dat.var[which(dat.var[,sub.set[,3]] <= sub.set[,4]),]
          m <- m+1
          new.dat[[m]] <- dat.var[which(dat.var[,sub.set[,3]] > sub.set[,4]),]
            } 
        }
        
      }
      
      #Replacing old with new level data and repeating
      old.dat <- new.dat
      old.indices <- length(new.dat)
    }
      
    #Checking accuracy of the tree by comparing target labels with the labels predicted using training tree
      for (b in 1:length(old.dat)){
        data_temp <- old.dat[[b]]
        for (w in c(1:nrow(data_temp))){
          acc[w] <- data_temp[w,target.ind] == final.labels[b]
        }
        acc.fin[b] <- sum(acc)/length(acc)
      }
    
    #Accuracy for each cross-validation set
    acc.val[q] <- mean(acc.fin)
    
  }

  #Average accuracy accross all cross-validation sets
  acc.avg <- mean(acc.val)
  cat("Average Cross-Validation accuracy: ", acc.avg)
  cat("Individual Cross-Validation accuracies: ", acc.val)
  
  #Plotting accuracy
  plot(1:5, acc.val,
       type="b", pch = 19, frame = FALSE, 
       main = paste("Decision Tree on dataset",filename,sep=" "),
       xlab="Cross-validation Test Sets",
       ylab="Accuracy")
  
}

#Call the Main Function to run algorithm
MainTreeTestFunction()
