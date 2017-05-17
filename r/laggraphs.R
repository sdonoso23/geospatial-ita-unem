library(maptools)
library(spdep)
library(tidyverse)
library(tseries)
library(lmtest)
library(ggplot2)
library(ggrepel)
library(sphet)
library(GGally)
library(spgwr)

lag.graphics<-function(dataset,wmatrix,columnid){
    lista<-list()
    originaldataset<-dataset
    ids<-which(colnames(originaldataset)==columnid)
    numeric<-sapply(dataset,is.numeric)
    dataset<-dataset[,numeric]
    colnames<-colnames(dataset)
    colnameslag<-paste(colnames,"LAG",sep="")
    n<-length(dataset)
    
    for(i in 1:n){
        lista[[i]]<-lag.listw(wmatrix,dataset[,i])
        }
    
    names(lista)<-colnameslag
    
    for(i in 1:7){
        print(ggplot(mapping=aes(dataset[,i],lista[[i]]))+geom_point()+
                  geom_smooth(method ="lm",se=F)+geom_text_repel(aes(label=originaldataset[,ids]))+
                  labs(x=names(dataset)[i],y=names(lista)[i]))
    }
}
    







