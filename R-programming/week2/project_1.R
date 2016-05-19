columnmean <- function(x, removeNA = TRUE){
  nc <- ncol(x)
  means <- numeric(nc)
  for(i in 1:nc){
    means[i] <- mean(x[,i], na.rm = removeNA)
  }
  means
}

pollutantmean<-function(directory, pollutant, id = 1:332){
  oldir<-getwd()
  setwd(directory)
  means<-numeric(length(id))
  for (i in length(id)){
    num<-as.character(id[i])
    if (nchar(num)==1){
      num<-paste("00", num,sep="")
    }
    else if(nchar(num)==2){
        num<-paste("0", num,sep="")
    }
    csv<-read.csv(paste(num,"csv",sep="."))
    data<-csv[[pollutant]]
    means[i]<-mean(data,na.rm = TRUE)
  }
  setwd(oldir)
  mean(means)
}

complete <-function(directory, id = 1:332){
  oldir<-getwd()
  setwd(directory)
  dfr<-read.table(text="", col.names = c("id","nobs"))
  for (i in length(id)){
    nobs<-0
    num<-as.character(id[i])
    if (nchar(num)==1){
      num<-paste("00", num,sep="")
      }
    else if(nchar(num)==2){
      num<-paste("0", num,sep="")
      }
    csv1<-read.csv(paste(num,"csv",sep="."))
    csv1<-csv1[complete.cases(csv1),]
    nobs<-nrow(csv1)
    dfr<-rbind(dfr,data.frame("id"=id,"nobs"=nobs))
     }
  setwd(oldir)
  dfr
}

corr <-function(directory, threshhold=0){
  oldir<-getwd()
  setwd(directory)
  id<-1:332
  l<-list()
  q=1
  for (i in length(id)){
    nobs<-0
    num<-as.character(id[i])
    if (nchar(num)==1){
      num<-paste("00", num,sep="")
    }
    else if(nchar(num)==2){
      num<-paste("0", num,sep="")
    }
    csv1<-read.csv(paste(num,"csv",sep="."))
    csv1<-csv1[complete.cases(csv1),]
    nobs<-nrow(csv1)
    if (nobs>threshhold){
      l[q]<-nobs
        q=q+1
    }
  }
  setwd(oldir)
  l
}