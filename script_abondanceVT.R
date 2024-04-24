rm(list=ls())


library(plyr)

dir=setwd("C:/Users/amiel/OneDrive/Bureau/Stage IPS2/scriptR/abundancedata")
    
files=list.files(path = dir, pattern = ".txt", full.names = T )

# Présentation du tableau de valeurs d'abondance pour chaque protéine et chaque expérience
abundance = NULL

for(file in files) {
  
  print(file)
  
  mat = as.matrix(read.table(file=file, header=FALSE))
  
  rownames(mat) = mat[,2]
  
  mat = as.matrix(mat[,3])
  
  mat = t(mat)
  
  print(length(mat[1,]))
  
  abundance = rbind.fill(abundance, as.data.frame(mat))
  
  
}

# Passage par numeric pour l'apply
for(i in 1:ncol(abundance)){
  abundance[,i]=as.numeric(as.character(abundance[,i]))

}
abundance=as.matrix(abundance)
abundance = t(abundance)


# abundance_med=as.data.frame(apply(abundance,1,median,na.rm=TRUE))

# Choix de l'interval Q1 / Q3 puis application de la médiane

q1=apply(abundance,1,quantile,na.rm=TRUE,probs=2/15)

q3=apply(abundance,1,quantile,na.rm=TRUE,probs=13/15)


abundance_med=NULL
for(i in 1:nrow(abundance))
  abundance_med=c(abundance_med,median(abundance[i,][abundance[i,]>q1[i] & abundance[i,]<q3[i]],na.rm=TRUE))

  
 abundance_med=as.matrix(abundance_med)
rownames(abundance_med)=rownames(abundance)

# Correspondance thesaurus abondance médiane

ID.PAXDB<-rownames(abundance)

for(i in 1:length(abundance[,1])){
  ID.PAXDB[i]<-substr(ID.PAXDB[i],6,nchar(ID.PAXDB[i]))
}
ID.PAXDB2<-ID.PAXDB

thesaurus=read.table(file="C:/Users/amiel/OneDrive/Bureau/Stage IPS2/scriptR/Conv/Thesaurus-S.cerevisiae.txt",header=TRUE)
ID.PAXDBinTHESAURUS<-as.vector(thesaurus[,6])
UniprotID<-as.vector(thesaurus[,1])


for(i in 1:length(abundance)){
  if(length(UniprotID[ID.PAXDB[i]==ID.PAXDBinTHESAURUS])==1){
    ID.PAXDB2[i]<-UniprotID[ID.PAXDB[i]==ID.PAXDBinTHESAURUS]
  }
}
rownames(abundance)<-ID.PAXDB2

# Optionnel : Correspondance thesaurus pour le max de l'abondance
# max=apply(abundance,1,max,na.rm=TRUE)
# max=as.matrix(max)
# ID.PAXDB<-rownames(max)
# 
# for(i in 1:length(max)){
#   if(length(UniprotID[ID.PAXDB[i]==ID.PAXDBinTHESAURUS])==1){
#     ID.PAXDB2[i]<-UniprotID[ID.PAXDB[i]==ID.PAXDBinTHESAURUS]
#   }
# }
# rownames(max)<-ID.PAXDB2


write.table(abundance_med,file="abundance-med.txt",sep="\t",row.names = TRUE,col.names = FALSE)

write.table(abundance,file="abundance-whole.txt",sep="\t",row.names = TRUE,col.names = FALSE)




temp=c()
for(i in 1:ncol(abundance))


temp=c(temp,strsplit(files,"/abundancedata")[[i]][2])

colnames(abundance)=temp


#  Ecart type  et grpahe associé
abundance_sdrow=apply(abundance,1,sd,na.rm=TRUE)
abundance_sdrow=as.matrix(abundance_sdrow)
hist(abundance_sdrow)

# Visualisation de la distribution de l'abondance par histogramme
pdf("HistogramAbundances.pdf")
par(mfrow=c(2,2))
moyenne=c()
for(i in 1:ncol(abundance)){
  hist(log(as.numeric(abundance[,i])),main=temp[i],xlim=c(-5,log(max(as.numeric(abundance),na.rm=TRUE))),xlab="log(Abundance)")
  moyenne=c(moyenne,mean(as.numeric(abundance[,i])))
  
}
dev.off()

# Visualisation de la médiane
pdf("BoxplotsAbundances.pdf")
par(mfrow=c(2,2))
mediane=c()
for(i in 1:ncol(abundance)){
  boxplot(log(as.numeric(abundance[,i])),main=temp[i],na.rm=TRUE)
  mediane=c(mediane,median(as.numeric(abundance[,i])))
}
dev.off()

 # Test ggplot

# library(ggplot2)
# mediane=c()
#  for(i in 1:ncol(abundance))
#   ggplot(abundance,aes(x=exp,y=log(abundance))+geom_boxplot(aes(fill=LABEL)
#   mediane=c(mediane,median(as.numeric(abundance[,i])))       



