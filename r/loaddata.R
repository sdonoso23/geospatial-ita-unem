library(tidyverse)

regions<-read.csv2("data/regiondata.csv")


lista<-list.files("data/fulldataset")
data<-data.frame()
for (i in 1:length(lista)){
    data<-bind_rows(data,read.csv2(paste("data/fulldataset/",
                    lista[i],sep=""),dec = ","))
}

workingdata<-data %>%
    select(Codice,Territorio,Variable,X2004) %>%
    filter(is.na(X2004) == FALSE) %>%
    spread(Variable,X2004) %>%
    right_join(regions,by=c("Codice"="Codice")) %>%
    select(-Territorio.x) %>%
    rename(Territorio=Territorio.y)

workingdata<-workingdata[,c(1,40,2:39)]
write.csv(workingdata,"output/itadata2004.csv")

itadata<-workingdata %>%
    select(Codice,Territorio,UNEM,POP,INCAVG,IMP,EXP) %>%
    mutate(EXPCAP=EXP/POP,IMPCAP=IMP/POP)
    
write.csv(itadata,"output/dataprogetto.csv")
