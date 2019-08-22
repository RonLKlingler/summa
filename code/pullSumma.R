rm(list=ls())

################### Prima Pars

bigMatPP <- matrix(nrow=1,ncol=3)

for(a in 1:119){
  print(a)
  con <- curl::curl(paste0("http://www.newadvent.org/summa/1",paste0(rep("0",(3-nchar(as.character(a)))),collapse = ""),a,".htm"))
  open(con)
  raw <- readLines(con)
  close(con)
  
  cleanFun <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
  }
  
  clean_a <- unlist(lapply(raw,cleanFun))
  clean_b <- clean_a[which(nchar(clean_a)>5)]
  clean_c <- clean_b[!grepl("google",clean_b)]
  clean_d <- clean_c[min(which(grepl("Article 1",clean_c))):max(which(grepl("Reply to Objection",clean_c)))]
  
  article_heads <- which(grepl("^Article .",clean_d))
  mat <- matrix(nrow=1,ncol=2)
  
  for(i in 1:length(clean_d)){
    if(i%in%article_heads){
      curArticle <- clean_d[i]
    }else{
      mat<-rbind(mat,matrix(c(curArticle,clean_d[i]),nrow=1))
    }
  }
  mat <- cbind(mat,rep(a,nrow(mat)))
  bigMatPP <- rbind(bigMatPP,mat)
}

primaparsDT <- data.table(bigMatPP)
setnames(primaparsDT,c("Article","Response","Question"))
primaparsDT<-primaparsDT[!is.na(primaparsDT$Article),]

primaparsDT$Part <- "Prima Pars"

################### Prima Secundæ Partis

bigMatPSP <- matrix(nrow=1,ncol=3)

for(a in 1:114){
  print(a)
  con <- curl::curl(paste0("http://www.newadvent.org/summa/1",paste0(rep("0",(3-nchar(as.character(a)))),collapse = ""),a,".htm"))
  open(con)
  raw <- readLines(con)
  close(con)
  
  cleanFun <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
  }
  
  clean_a <- unlist(lapply(raw,cleanFun))
  clean_b <- clean_a[which(nchar(clean_a)>5)]
  clean_c <- clean_b[!grepl("google",clean_b)]
  clean_d <- clean_c[min(which(grepl("Article 1",clean_c))):max(which(grepl("Reply to Objection",clean_c)))]
  
  article_heads <- which(grepl("^Article .",clean_d))
  mat <- matrix(nrow=1,ncol=2)
  
  for(i in 1:length(clean_d)){
    if(i%in%article_heads){
      curArticle <- clean_d[i]
    }else{
      mat<-rbind(mat,matrix(c(curArticle,clean_d[i]),nrow=1))
    }
  }
  mat <- cbind(mat,rep(a,nrow(mat)))
  bigMatPSP <- rbind(bigMatPSP,mat)
}

primasegparsDT <- data.table(bigMatPSP)
setnames(primasegparsDT,c("Article","Response","Question"))
primasegparsDT<-primasegparsDT[!is.na(primasegparsDT$Article),]

primasegparsDT$Part <- "Prima Secundæ Partis"

################### Secunda Secundæ Partis

bigMatSSP <- matrix(nrow=1,ncol=3)

for(a in 1:189){
  print(a)
  con <- curl::curl(paste0("http://www.newadvent.org/summa/3",paste0(rep("0",(3-nchar(as.character(a)))),collapse = ""),a,".htm"))
  open(con)
  raw <- readLines(con)
  close(con)
  
  cleanFun <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
  }
  
  clean_a <- unlist(lapply(raw,cleanFun))
  clean_b <- clean_a[which(nchar(clean_a)>5)]
  clean_c <- clean_b[!grepl("google",clean_b)]
  clean_d <- clean_c[min(which(grepl("Article 1",clean_c))):max(which(grepl("Reply to Objection",clean_c)))]
  
  article_heads <- which(grepl("^Article .",clean_d))
  mat <- matrix(nrow=1,ncol=2)
  
  for(i in 1:length(clean_d)){
    if(i%in%article_heads){
      curArticle <- clean_d[i]
    }else{
      mat<-rbind(mat,matrix(c(curArticle,clean_d[i]),nrow=1))
    }
  }
  mat <- cbind(mat,rep(a,nrow(mat)))
  bigMatSSP <- rbind(bigMatSSP,mat)
}

segsegparsDT <- data.table(bigMatSSP)
setnames(segsegparsDT,c("Article","Response","Question"))
segsegparsDT<-segsegparsDT[!is.na(segsegparsDT$Article),]

segsegparsDT$Part <- "Secunda Secundæ Partis"

################### Tertia Pars

bigMatTP <- matrix(nrow=1,ncol=3)

for(a in 1:90){
  print(a)
  con <- curl::curl(paste0("http://www.newadvent.org/summa/4",paste0(rep("0",(3-nchar(as.character(a)))),collapse = ""),a,".htm"))
  open(con)
  raw <- readLines(con)
  close(con)
  
  cleanFun <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
  }
  
  clean_a <- unlist(lapply(raw,cleanFun))
  clean_b <- clean_a[which(nchar(clean_a)>5)]
  clean_c <- clean_b[!grepl("google",clean_b)]
  clean_d <- clean_c[min(which(grepl("Article 1",clean_c))):max(which(grepl("Reply to Objection",clean_c)))]
  
  article_heads <- which(grepl("^Article .",clean_d))
  mat <- matrix(nrow=1,ncol=2)
  
  for(i in 1:length(clean_d)){
    if(i%in%article_heads){
      curArticle <- clean_d[i]
    }else{
      mat<-rbind(mat,matrix(c(curArticle,clean_d[i]),nrow=1))
    }
  }
  mat <- cbind(mat,rep(a,nrow(mat)))
  bigMatTP <- rbind(bigMatTP,mat)
}

terparsDT <- data.table(bigMatTP)
setnames(terparsDT,c("Article","Response","Question"))
terparsDT<-terparsDT[!is.na(terparsDT$Article),]

terparsDT$Part <- "Tertia Pars"

################### Supplementum Tertiæ Partis

bigMatSTP <- matrix(nrow=1,ncol=3)

for(a in 1:99){
  print(a)
  con <- curl::curl(paste0("http://www.newadvent.org/summa/5",paste0(rep("0",(3-nchar(as.character(a)))),collapse = ""),a,".htm"))
  open(con)
  raw <- readLines(con)
  close(con)
  
  cleanFun <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
  }
  
  clean_a <- unlist(lapply(raw,cleanFun))
  clean_b <- clean_a[which(nchar(clean_a)>5)]
  clean_c <- clean_b[!grepl("google",clean_b)]
  clean_d <- clean_c[min(which(grepl("Article 1",clean_c))):max(which(grepl("Reply to Objection",clean_c)))]
  
  article_heads <- which(grepl("^Article .",clean_d))
  mat <- matrix(nrow=1,ncol=2)
  
  for(i in 1:length(clean_d)){
    if(i%in%article_heads){
      curArticle <- clean_d[i]
    }else{
      mat<-rbind(mat,matrix(c(curArticle,clean_d[i]),nrow=1))
    }
  }
  mat <- cbind(mat,rep(a,nrow(mat)))
  bigMatSTP <- rbind(bigMatSTP,mat)
}

supterparsDT <- data.table(bigMatSTP)
setnames(supterparsDT,c("Article","Response","Question"))
supterparsDT<-supterparsDT[!is.na(supterparsDT$Article),]

supterparsDT$Part <- "Supplementum Tertiæ Partis"

##################


totSumma <- rbind(rbind(rbind(rbind(primaparsDT,primasegparsDT),segsegparsDT),terparsDT),supterparsDT)
write.csv(totSumma,"~/R/nn/summa.csv")









