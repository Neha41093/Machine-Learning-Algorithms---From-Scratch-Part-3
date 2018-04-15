#!/usr/bin/env Rscript
#Q2  -- Apriori Candidate and Rule Generation on Car Dataset

#old.dir <- getwd()
#setwd("C:/Users/Neha Rawat/Desktop/IU-Data Science/Data Mining/Assignment 4")

#*******************************************Car dataset***************************************************************
#Link to car dataset: https://archive.ics.uci.edu/ml/datasets/Car+Evaluation
#Convert into transactional dataset

car_data <- read.csv("car_data.csv",header = FALSE)
class(car_data)
str(car_data)

blank.vec <- as.character(rep("",nrow(car_data)))
for (i in 1:nrow(car_data)){
  for (j in 1:ncol(car_data)){
    blank.vec[i] <- paste(blank.vec[i],paste(paste('V',j,sep=""),car_data[i,j],sep="_"),sep=",")
  }
}

temp.frame <- as.data.frame(blank.vec)

char.vec <- character(length=nrow(temp.frame))
for(i in 1:nrow(temp.frame)){
  char.vec[i] <- substring(as.character(temp.frame$blank.vec)[i],2)
}

#transaction dataset
car.trans <- as.data.frame(char.vec)

#sorting transactions lexicographically
for(i in 1:nrow(car.trans)){
car.trans[i,] <- paste(sort(unlist(strsplit(as.character(car.trans$char.vec)[i],","))),collapse=",")
}

#getting number of unique items
car.mat <- matrix(NA, nrow=nrow(car.trans),ncol=ncol(car_data))
for(i in 1:nrow(car.trans)){
  car.mat[i,] <- sort(unlist(strsplit(as.character(car.trans$char.vec)[i],",")))}
car.frame <- as.data.frame(sort(unique(as.vector(car.mat))))

#**************************Required Functions******************************
#Candidate Set Generation Functions
#Fk-1 X F1
firstgenerationmod <- function(prev.cand, first.cand, k){
  a <- 0
  gen.vec <- character()
  if (k == 2){
     for (i in c(1:nrow(first.cand))){
       for (j in c(1:nrow(first.cand))){
         set.vec <- unique(c(as.character(first.cand[i,]),as.character(first.cand[j,])))
         if (length(set.vec) > 1){
           a <- a+1
           gen.vec[a] <- paste(sort(set.vec),collapse=",")
         }
       }
     }
    gen.frame <- unique(as.data.frame(gen.vec))
  } else{
    for (i in c(1:nrow(first.cand))){
      for (j in c(1:nrow(prev.cand))){
        set.vec <- unique(unlist(strsplit(paste(as.character(prev.cand[j,]),as.character(first.cand[i,]),sep=","),",")))
        if (length(set.vec) > k-1){
          a <- a+1
          gen.vec[a] <- paste(sort(set.vec),collapse=",")
        }
      }
    }
    gen.frame <- unique(as.data.frame(gen.vec))
  }
  return_list <- list(gen.frame,nrow(gen.frame))
  return(return_list)
}


#Fk-1 X Fk-1
secondgenerationmod <- function(prev.cand, k){
  a <- 0
  gen.vec <- character()
  if (k == 2){
    for (i in c(1:nrow(prev.cand))){
      for (j in c(1:nrow(prev.cand))){
        set.vec <- unique(c(as.character(prev.cand[i,]),as.character(prev.cand[j,])))
        if (length(set.vec) > 1){
          a <- a+1
          gen.vec[a] <- paste(sort(set.vec),collapse=",")
        }
      }
    }
    gen.frame <- unique(as.data.frame(gen.vec))
  } else{
    for (i in c(1:nrow(prev.cand))){
      for (j in c(1:nrow(prev.cand))){
        if (length(grep("mismatch",all.equal(prev.cand[i,],prev.cand[j,]))) == 1){
          t1 <- sort(unlist(strsplit(as.character(prev.cand[i,]),",")))
          t2 <- sort(unlist(strsplit(as.character(prev.cand[j,]),",")))
          if (all.equal(t1[-(k-1)],t2[-(k-1)]) == TRUE){
            set.vec <- unique(unlist(strsplit(paste(as.character(prev.cand[j,]),as.character(prev.cand[i,]),sep=","),",")))
            a <- a+1
            gen.vec[a] <- paste(sort(set.vec),collapse=",")
          }
        }
      }
    }
    gen.frame <- unique(as.data.frame(gen.vec))
  }
  return_list <- list(gen.frame,nrow(gen.frame))
  return(return_list)
}

#Support Pruning Function (compare itemsets with transactions and prune)
supportpruning <- function(trans.data,cand.data,support.thresh){
  support.vec <- as.numeric(rep(0,nrow(cand.data)))
  for (i in c(1:nrow(cand.data))){
    for (j in c(1:nrow(trans.data))){
      temp.vec <- sort(unlist(strsplit(as.character(cand.data[i,]),",")))
      count.vec <- apply(as.matrix(temp.vec),1,function(x) length(grep(x,as.character(trans.data[j,]))))
      if (sum(count.vec) == length(temp.vec)){
        support.vec[i] <- support.vec[i] + 1
      }
    }
  }
  supports <- (support.vec)/nrow(trans.data)
  freq.data <- as.data.frame(cand.data[which(supports >= support.thresh),])
  if (is.null(nrow(freq.data)) == TRUE){
    return (NULL)
  } else{
    return (list(freq.data,supports))
  }
}

#Brute force rule generation function
rulegenerationbrute <- function(freq.item,k){
  freq.rules <- list()
  a <- 0
  for (i in c(1:nrow(freq.item))){
    temp.vec <- sort(unlist(strsplit(as.character(freq.item[i,]),",")))
    rule.list <- list()
    rule.vec <- as.character()
    for (m in c(1:(k-1))){
      combs.mat <- combn(temp.vec,m)
      if (m == 1){
        rule.list[[m]] <- as.character(apply(combs.mat,2,function(x) paste("Ant",paste(temp.vec[-which(temp.vec == x)],collapse=","),"Cons",x,sep="-")))
      } else{
        for (g in c(1:ncol(combs.mat))){
          rule.vec[g] <- paste("Ant",paste(subset(temp.vec, !(temp.vec %in% as.character(combs.mat[,g]))),collapse=","),"Cons",
                               paste(as.character(combs.mat[,g]),collapse = ","),sep="-")
        }
        rule.list[[m]] <- rule.vec
      }
    }
    freq.rules[[i]] <- data.frame(rules=unique(unlist(rule.list)))
  }
  a <- nrow(do.call(rbind,freq.rules))
  return(list(rules = unique(do.call(rbind,freq.rules)),count = a))
  }


#Base Rule Generation Function
rulegenerationbase <- function(freq.item,k,m){
  freq.rules <- list()
  a <- 0
  for (i in c(1:nrow(freq.item))){
    temp.vec <- sort(unlist(strsplit(as.character(freq.item[i,]),",")))
    combs.mat <- combn(temp.vec,m)
    if (m == 1){
      rule.list <- as.character(apply(combs.mat,2,function(x) paste("Ant",paste(temp.vec[-which(temp.vec == x)],collapse=","),"Cons",x,sep="-")))
    } else{
      rule.vec <- as.character()
      for (g in c(1:ncol(combs.mat))){
        rule.vec[g] <- paste("Ant",paste(subset(temp.vec, !(temp.vec %in% as.character(combs.mat[,g]))),collapse=","),"Cons",
                             paste(as.character(combs.mat[,g]),collapse = ","),sep="-")
      }
      rule.list <- rule.vec
    }
    freq.rules[[i]] <- data.frame(rules=unique(rule.list))
  }
  a <- nrow(do.call(rbind,freq.rules))
  return(list(rules = unique(do.call(rbind,freq.rules)),count = a))
}

#Confidence Calculation Function
confidencecalc <- function(freq.rules,k,m,cand.list,supports){
  conf <- as.numeric()
  for (i in c(1:nrow(freq.rules))){
    temp.vec <- unlist(strsplit(as.character(freq.rules[i,]),"-"))
    temp.string <- paste(sort(unlist(strsplit(temp.vec[2],","))),collapse = ",")
    whole.string <- paste(sort(unlist(strsplit(c(temp.vec[2],temp.vec[4]),","))),collapse = ",")
    conf.num <- supports[[k]][which(as.vector(cand.list[[k]]) == whole.string)]
    conf.den <- supports[[k-m]][which(as.vector(cand.list[[k-m]]) == temp.string)]
    if ((length(conf.num) == 0) | (length(conf.den) == 0)){
      conf[i] <- 0
    }else{
      conf[i] <- conf.num/conf.den}
  }
  return(conf)
}

#Lift Calculation Function
liftcalc <- function(freq.rules,k,m,cand.list,supports){
  lift <- as.numeric()
  for (i in c(1:nrow(freq.rules))){
    temp.vec <- unlist(strsplit(as.character(freq.rules[i,]),"-"))
    temp.string1 <- paste(sort(unlist(strsplit(temp.vec[2],","))),collapse = ",")
    temp.string2 <- paste(sort(unlist(strsplit(temp.vec[4],","))),collapse = ",")
    whole.string <- paste(sort(unlist(strsplit(c(temp.vec[2],temp.vec[4]),","))),collapse = ",")
    lift.num <- supports[[k]][which(as.vector(cand.list[[k]]) == whole.string)]
    lift.den <- (supports[[k-m]][which(as.vector(cand.list[[k-m]]) == temp.string1)])*(supports[[m]][which(as.vector(cand.list[[m]]) == temp.string2)])
    if ((length(lift.num) == 0) | (length(lift.den) == 0)){
      lift[i] <- 0
    }else{
      lift[i] <- lift.num/lift.den}
  }
  return(lift)
}


#Final Rule Generation Function (using pruning to reduce rule generation)
finalrulegeneration <- function(freq.itemlist,cand.itemlist,support.list,kmax,thresh,flag){
  main.rule.list <- list()
  main.rule.count <- list()
  
  for (k in c(2:kmax)){
      rule.list <- list()
      b <- 0
      rule.gen <- 0
      
      list.1 <- rulegenerationbase(freq.itemlist[[k]],k,1)
      rule.gen <- rule.gen + list.1[[2]]
      if (flag == "confidence"){
        conf.vec <- confidencecalc(list.1[[1]],k,1,cand.itemlist,support.list)
        init.rules <- list.1[[1]]
        prune.rules <- data.frame(rules = init.rules[which(conf.vec >= thresh),], confidence = conf.vec[which(conf.vec >= thresh)])
      } else{
        lift.vec <- liftcalc(list.1[[1]],k,1,cand.itemlist,support.list)
        init.rules <- list.1[[1]]
        prune.rules <- data.frame(rules = init.rules[which(lift.vec >= thresh),], lift = lift.vec[which(lift.vec >= thresh)])
      }
      
      b <- b+1
      rule.list[[b]] <- prune.rules
      prev.rules <- prune.rules
      
      if (k > 2){
      for (m in c(2:(k-1))){
        prev.vec <- as.character(prev.rules[,1])
        new.rulesl <- rulegenerationbase(freq.itemlist[[k]],k,m)
        new.rules <- new.rulesl[[1]]
        rule.gen <- rule.gen + nrow(new.rules)
        if (flag == "confidence"){
          conf.vec <- confidencecalc(new.rules,k,m,cand.itemlist,support.list)
          init.rules <- new.rules
          prune.rules <- data.frame(rules = init.rules[which(conf.vec >= thresh),], confidence = conf.vec[which(conf.vec >= thresh)])
        } else{
          lift.vec <- liftcalc(new.rules,k,m,cand.itemlist,support.list)
          init.rules <- new.rules
          prune.rules <- data.frame(rules = init.rules[which(lift.vec >= thresh),], lift = lift.vec[which(lift.vec >= thresh)])
        }
        
        b <- b+1
        rule.list[[b]] <- prune.rules
        prev.rules <- prune.rules
      }
      }
      
      main.rule.list[[k-1]] <- do.call(rbind,rule.list)
      main.rule.count[[k-1]] <- rule.gen
  }
  return(main.rule.list)
  }


#*****************Main Code**************************************

#First candidate and frequent itemsets
candidate_first <- car.frame
#check for support values and get frequent first itemsets
#support thresholds: 5%, 10% and 20% : try all and see
support.thresh <- 0.10

list1 <- supportpruning(car.trans,candidate_first,support.thresh)
frequent_first <- list1[[1]]
support_first <- list1[[2]]
kmax <- nrow(frequent_first)

#generate other candidate sets using both the methodologies

#First Method
prev_cand <- frequent_first
candidate_counts <- list()
firstmethodcandidates <- list()
firstmethodfrequents <- list()
frequent_counts <- list()
support_values <- list()

for(i in c(2:kmax)){
  list2 <- firstgenerationmod(prev_cand, frequent_first, i)
  if (list2[[2]] == 0){
    break
  }
  firstmethodcandidates[[i-1]] <- list2[[1]]
  candidate_counts[[i-1]] <- list2[[2]]
  list3 <- supportpruning(car.trans,firstmethodcandidates[[i-1]],support.thresh)
  if (nrow(list3[[1]]) == 0){
    break
  }
  firstmethodfrequents[[i-1]] <- list3[[1]]
  support_values[[i-1]] <- list3[[2]]
  frequent_counts[[i-1]] <- nrow(firstmethodfrequents[[i-1]])
  prev_cand <- firstmethodfrequents[[i-1]]
}

sum(do.call(rbind,candidate_counts))
sum(do.call(rbind,frequent_counts))

#Second Method
prev_cand <- frequent_first
candidate_counts2 <- list()
secondmethodcandidates <- list()
secondmethodfrequents <- list()
frequent_counts2 <- list()
support_values2 <- list()

for(i in c(2:kmax)){
  list2 <- secondgenerationmod(prev_cand,i)
  if (list2[[2]] == 0){
    break
  }
  secondmethodcandidates[[i-1]] <- list2[[1]]
  candidate_counts2[[i-1]] <- list2[[2]]
  list3 <- supportpruning(car.trans,secondmethodcandidates[[i-1]],support.thresh)
  if (nrow(list3[[1]]) == 0){
    break
  }
  secondmethodfrequents[[i-1]] <- list3[[1]]
  support_values2[[i-1]] <- list3[[2]]
  frequent_counts2[[i-1]] <- nrow(secondmethodfrequents[[i-1]])
  prev_cand <- secondmethodfrequents[[i-1]]
}

sum(do.call(rbind,candidate_counts2))
sum(do.call(rbind,frequent_counts2))

#demonstrates huge reduction in candidate sets being generated using second methodology

#Maximal Frequent and Closed Frequent Itemsets
#Maximal Itemsets
#Checking among the first itemsets
temp.frame <- secondmethodfrequents[[1]]
check.vec <- as.numeric()
for (i in c(1:nrow(frequent_first))){
  for (j in c(1:nrow(temp.frame))){
    temp.vec <- sort(unlist(strsplit(as.character(frequent_first[i,]),",")))
    count.vec <- apply(as.matrix(temp.vec),1,function(x) length(grep(x,as.character(temp.frame[j,]))))
    if (sum(count.vec) == length(temp.vec)){
      check.vec[i] <- 1
      break
    }
  }
}


nrow(frequent_first) - length(which(check.vec == 1))

#checking among the remaining itemsets (when more than 2-level itemsets)
check.vec2 <- list()
for (v in c(1:(length(secondmethodfrequents)-1))){
  check.vec3 <- as.numeric()
  temp.frame <- secondmethodfrequents[[v]]
  temp.frame2 <- secondmethodfrequents[[v+1]]
  for (i in c(1:nrow(temp.frame))){
    for(j in c(1:nrow(temp.frame2))){
    temp.vec <- sort(unlist(strsplit(as.character(temp.frame[i,]),",")))
    count.vec <- apply(as.matrix(temp.vec),1,function(x) length(grep(x,as.character(temp.frame2[j,]))))
    if (sum(count.vec) == length(temp.vec)){
      check.vec3[i] <- 1
      break
    }
  }
  }
  check.vec2[[v]] <- check.vec3
}

nrow(secondmethodfrequents[[1]]) - length(which(check.vec2[[1]] == 1))

#Closed Itemsets
#check for first itemsets
support.vecfirst <- as.numeric()
for(i in c(1:nrow(frequent_first))){
support.vecfirst[i] <- support_first[which(candidate_first == as.character(frequent_first[i,]))]
}

temp.frame <- secondmethodfrequents[[1]]
support.vecnext <- as.numeric()
for(i in c(1:nrow(temp.frame))){
  support.vecnext[i] <- support_values2[[1]][which(secondmethodcandidates[[1]] == as.character(temp.frame[i,]))]
}

check.vec <- as.numeric()
for (i in c(1:nrow(frequent_first))){
  for (j in c(1:nrow(temp.frame))){
    temp.vec <- sort(unlist(strsplit(as.character(frequent_first[i,]),",")))
    count.vec <- apply(as.matrix(temp.vec),1,function(x) length(grep(x,as.character(temp.frame[j,]))))
    if ((sum(count.vec) == length(temp.vec)) 
  & (support.vecfirst[i] == support.vecnext[j]))
      {
      check.vec[i] <- 1
      break
    }
  }
}

nrow(frequent_first) - length(which(check.vec == 1))

#check for others (when more than 2-level itemsets)
check.vec2 <- list()
for (v in c(1:(length(secondmethodfrequents)-1))){
  check.vec3 <- as.numeric()
  temp.frame <- secondmethodfrequents[[v]]
  temp.frame2 <- secondmethodfrequents[[v+1]]
  for (i in c(1:nrow(temp.frame))){
    for(j in c(1:nrow(temp.frame2))){
      temp.vec <- sort(unlist(strsplit(as.character(temp.frame[i,]),",")))
      count.vec <- apply(as.matrix(temp.vec),1,function(x) length(grep(x,as.character(temp.frame2[j,]))))
      if ((sum(count.vec) == length(temp.vec))
          & ((support_values2[[v]][which(secondmethodcandidates[[v]] == as.character(temp.frame[i,]))]) == support_values2[[v+1]][which(secondmethodcandidates[[v+1]] == as.character(temp.frame2[j,]))]))
        {
        check.vec3[i] <- 1
        break
      }
    }
  }
  check.vec2[[v]] <- check.vec3
}

nrow(secondmethodfrequents[[1]]) - length(which(check.vec2[[1]] == 1))


#Rule Generation: Using Brute Force and by Pruning
#Can use either Confidence (0.20,0.50,0.70) or Lift and can specify 3 values for comparison
conf.thresh <- 0.50

#bruteforce
bruteresults <- list()
for (i in c(1:length(secondmethodfrequents))){
  bruteresults[[i]] <- rulegenerationbrute(secondmethodfrequents[[i]],i+1)
}

bruteresults

#with pruning
freq.itemlist <- c(list(data.frame(frequent_first, stringsAsFactors = FALSE)),secondmethodfrequents)
cand.itemlist <- c(list(data.frame(candidate_first, stringsAsFactors = FALSE)),secondmethodcandidates)
support.list <- c(list(support_first),support_values2)
kmax <- length(freq.itemlist)
thresh <- conf.thresh
flag <- "confidence"

pruneresults <- finalrulegeneration(freq.itemlist,cand.itemlist,support.list,kmax,thresh,flag)
pruneresults  

#Check Top 5 rules
prunerules <- do.call(rbind,pruneresults)
ordered <- prunerules[order(-prunerules[,2]),]
ordered[c(1:5),]
