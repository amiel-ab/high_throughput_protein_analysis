rm(list=ls())
library(ggplot2)
library(viridis)


# Constantes de seuil sur la taille des ilots
 seuil=6
 
directory_clusterfile = "C:/Users/amiel/OneDrive/Bureau/Stage IPS2/scriptR/dataspot/exemple"

files = list.files(path = directory_clusterfile, pattern = ".dat", full.names = T )

  list_result = NULL

# Extraction du nombre et de la taille des ilots à partir des fichiers .dat (120 fichiers)
for(fichier in files) {

  
  data_cluster = read.table(file = fichier, header = T)
  
  
  data_cluster_clean = NULL

# Optionnel data_cluster_clean

  data_cluster_clean = data_cluster[data_cluster[,1] != 0, ]
  
  result_table = NULL
  
  compteur = 0
  
  ilot = 1
  
  
  for(i in data_cluster_clean[,2]) {
    
    if(ilot == i) {
      
      
      compteur = compteur+1
      
    } else {
      
      if (compteur>seuil){
      result_table = rbind(result_table, cbind(ilot,compteur))
      }
      compteur = 1
      
      ilot= i
      
      
    }
    
  }
  if(compteur>seuil){
  result_table = rbind(result_table, cbind(ilot,compteur))
  }
  list_result[[basename(fichier)]] = result_table
  
}


  
#Somme compteur taille ilots et corrrélation

taille_ilot=NULL
nb_ilots=NULL
#  Optionnel : max et min de la taille des ilots
max=NULL
min=NULL

for(i in 1:length(list_result)){
 taille_ilot=c(taille_ilot,sum(list_result[[i]][,2]))
 nb_ilots=c(nb_ilots,dim(list_result[[i]])[1])
 min=c(min,min(list_result[[i]][,2]))
 max=c(max,max(list_result[[i]][,2]))
}
names(taille_ilot)=substr(files,68,73)


# Test avec fonction tail
# nb_ilots=lapply(list_result,tail,1)



abundance_med=read.table(file="C:/Users/amiel/OneDrive/Bureau/Stage IPS2/scriptR/abundancecorrelation/abundance-max.txt",header=FALSE,sep="\t")                       

# Conception tableau avec l'abondance, taille des ilots , nombre d'ilots , taille max et min des ilots

id_protmissing=NULL
abundance_final=NULL

for(i in 1:length(files)){
   id_files=substr(files[i],68,73)
  if (length(grep(id_files,abundance_med[,1]))==0){
    print(paste(id_files,"protein missing"))
    id_protmissing=c(id_protmissing,id_files)
    taille_ilot=taille_ilot[-i]
    nb_ilots=nb_ilots[-i]
    max=max[-i]
    min=min[-i]
    list_result=list_result[-i]
  }
    
  abundance_final=c(abundance_final,abundance_med[abundance_med[,1]==substr(files[i],68,73),2])

}

tableau=cbind(taille_ilot,abundance_final,nb_ilots,max,min)
rownames(tableau)=names(taille_ilot)




# Attribution d'une valeur identique d'abondance à chaque ilot 

same_ab=NULL
abundance_egal=NULL
for(i in 1:dim(tableau)[1]){
  # print(tableau[i,3])
  abundance_egal=c(abundance_egal,rep(tableau[i,2],tableau[i,3]))
  
  same_ab=rbind(same_ab,list_result[[i]])
}

same_ab=cbind(same_ab,abundance_egal)



#Visualisation graphique
# Graphe corrélation en fonction de la taille (cf. fin de script pour nombre d'ilots)
#Passe par data frame pour mettre en plot



tableau=as.data.frame(tableau)
plot(x=tableau[,1],y=tableau[,2])

# Ti : taille ilots , ab : abondance
ti=rank(tableau[,1])
ab=rank(tableau[,2])

# Test correlation
cor.test(ti,ab,method="pearson")
cor.test(ti,ab,method="spearman")


ggplot(tableau,aes(x =ti,y=ab))+geom_point(aes(color = tableau[,1])) +
  scale_color_viridis(option = "D")+
  theme_minimal() +
  theme(legend.position = "bottom")

same_ab=as.data.frame(same_ab)

ggplot(same_ab,aes(x =log(same_ab[,2]),y=log(same_ab[,3])))+geom_point(aes(color = same_ab[,2])) +
  scale_color_viridis(option = "D")+
  theme_minimal() +
  theme(legend.position = "bottom")


# Option : visualisation en log 
ggplot(tableau,aes(x =ti,y=ab))+geom_point()
ggplot(tableau,aes(x =log(tableau[,1]),y=log(tableau[,2])))+geom_point()

# Graphe corrélation en fonciton du nombre d'ilots

# apn: abundance par nb ilots 
apn=NULL
for(i in 1:max(tableau[,3]))
 apn[[i]]=tableau[,2][tableau[,3]==i]


apn=NULL
for(i in 1:max(tableau[,3]))
  apn[[i]]=ab[tableau[,3]==i]


boxplot(apn,ylim=c(0,100),ylab="abondance",xlab="nombre d'ilots")

apn=as.matrix(apn)
apn=as.numeric(apn)
cor(apn,method = "spearman")



