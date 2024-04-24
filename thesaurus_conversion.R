rm(list=ls())
  
  abundance=read.csv(file="abundance.csv",header=TRUE)
  is.data.frame(abundance)
  
  abundance<-as.data.frame(abundance)
  ID.PAXDB<-as.vector(abundance[,1])
  
  for(i in 1:length(abundance[,1])){
    ID.PAXDB[i]<-substr(ID.PAXDB[i],6,nchar(ID.PAXDB[i]))
  }
  ID.PAXDB2<-ID.PAXDB
  
  thesaurus=read.table(file="Thesaurus-S.cerevisiae.txt",header=TRUE)
  ID.PAXDBinTHESAURUS<-as.vector(thesaurus[,6])
  UniprotID<-as.vector(thesaurus[,1])
  is.data.frame(thesaurus)
  
  for(i in 1:length(ID.PAXDB)){
    for(j in 1:length(ID.PAXDBinTHESAURUS)){
      if(ID.PAXDB[i]==ID.PAXDBinTHESAURUS[j]){
        ID.PAXDB2[i]=UniprotID[j]
      }
    }
  }
  abundance[,1]<-ID.PAXDB2
  
  #version plus rapide
  for(i in 1:length(abundance[,1])){
    if(length(UniprotID[ID.PAXDB[i]==ID.PAXDBinTHESAURUS])==1){
      ID.PAXDB2[i]<-UniprotID[ID.PAXDB[i]==ID.PAXDBinTHESAURUS]
    }
  }
  abundance[,1]<-ID.PAXDB2
  write.table(abundance,file="abundance-v2",sep="\t",row.names = FALSE,col.names = FALSE)