
lmtable<-function(model){
    modellist=list()
    modellist[[1]] = model
    a<-summary(model)
    others<-data.frame("Observations"=length(a$residuals),"Residual Std. Error"=a$sigma,"$R^2$"=a$r.squared, "Adjusted $R^2$"=a$adj.r.squared,"AIC"=AIC(model),row.names = NULL,check.names=FALSE) 
    fstatistic<-data.frame("Statistic"=a$fstatistic[1],"df1"=a$fstatistic[2],"df2"=a$fstatistic[3],
                           "P-Value"=1-pf(a$fstatistic[1],a$fstatistic[2],a$fstatistic[3]),row.names = c("F Statistic"),check.names=FALSE)
    
    modellist[[2]] = others
    modellist[[3]] = fstatistic
    return(modellist)
}


allmoranteststable<-function(morantests){
    moranteststable<-data.frame(c(morantests$LMerr$statistic,morantests$LMlag$statistic,morantests$RLMerr$statistic,
                                  morantests$RLMlag$statistic,morantests$SARMA$statistic),c(morantests$LMerr$parameter,morantests$LMlag$parameter,morantests$RLMerr$parameter,
                                                                                            morantests$RLMlag$parameter,morantests$SARMA$parameter),c(morantests$LMerr$p.value,morantests$LMlag$p.value,morantests$RLMerr$p.value,
                                                                                                                                                      morantests$RLMlag$p.value,morantests$SARMA$p.value))
    
    colnames(moranteststable)<-c("Statistic","df","P-Value")
    row.names(moranteststable)<-c("Spatial Error","Spatial Lag","Robust Spatial Error","Robust Spatial Lag","SARAR")    
    return(moranteststable)    
}




sararclassictable<-function(model){
    
    c<-summary(model)
    
    modellist<-list()
    
    modellist[[1]]<-c$Coef
    modellist[[2]]<-data.frame("Estimate"=c(c$rho,c$lambda),"Std.Error"=c(c$rho.se,c$lambda.se),"z value"=c(c$rho/c$rho.se,c$lambda/c$lambda.se),"Pr(>|z|)"=c(2*(1-pnorm(abs(c$rho/c$rho.se))),2*(1-pnorm(abs(c$lambda/c$lambda.se)))),row.names=c("Rho","Lambda"),check.names=FALSE)
    modellist[[3]]<-data.frame("Observations"=length(c$residuals),"Parameters"=c$parameters,"AIC"=AIC(model),"Residual Variance"=c$s2,row.names=NULL,check.names=FALSE)  
    modellist[[4]]<-data.frame("Estimate"=as.vector(c$LR1$statistic),"P-Value"=as.vector(c$LR1$p.value),row.names="LR Test",check.names=FALSE)
    return(modellist)
}

impactstable<-function(model,wmatrix){
    a<-impacts(model,listw = wmatrix)
    impacts.df<-cbind.data.frame(a$direct,a$indirect,a$total)
    row.names(impacts.df)<-attributes(a)$bnames
    colnames(impacts.df)<-c("Direct","Indirect","Total")
    
    return(impacts.df)
}


lagmodeltable<-function(model){
    
    c<-summary(model)
    
    modellist<-list()
    
    modellist[[1]]<-c$Coef
    modellist[[2]]<-data.frame("Estimate"=c(c$rho),"Std.Error"=c(c$rho.se),"z value"=c(c$rho/c$rho.se),"Pr(>|z|)"=c(2*(1-pnorm(c$rho/c$rho.se))),row.names=c("Rho"),check.names=FALSE)
    modellist[[3]]<-data.frame("Observations"=length(c$residuals),"Parameters"=c$parameters,"AIC"=AIC(model),"Residual Variance"=c$s2,row.names=NULL,check.names=FALSE)  
    modellist[[4]]<-data.frame("Estimate"=as.vector(c$Wald1$statistic),"P-Value"=as.vector(c$Wald1$p.value),row.names="Wald Test",check.names=FALSE)
    return(modellist)
}


jbbptests<-function(model){
    a<-jarque.bera.test(model$residuals)
    b<-bptest(model)
    modellist<-list()
    modellist[[1]]<-data.frame("Statistic"=c(a$statistic,b$statistic),"DF"=c(a$parameter,b$parameter),"P-Value"=c(a$p.value,b$p.value),row.names=c("Jarque-Bera Test","Breusch-Pagan Test"),check.names=FALSE)
    return(modellist)
    }

lagstslstable<-function(model){
    
    c<-summary(model)
    
    modellist<-list()
    
    modellist[[1]]<-c$Coef
    modellist[[2]]<-data.frame("Observations"=length(c$residuals),"Parameters"=length(c$coefficients),"Residual Var."=c$sse/(length(c$residuals)-length(c$coefficients)),row.names=NULL,check.names=FALSE)  
    return(modellist)
}

sarargstslstable<-function(model){
    
    c<-summary(model)
    
    modellist<-list()
    
    modellist[[1]]<-c$Coef
    modellist[[2]]<-data.frame("Lambda Est."=c$lambda,"Observations"=length(c$residuals),"Parameters"=c$parameters,"Residual Variance"=c$s2,row.names=NULL,check.names=FALSE)  
    return(modellist)
}
