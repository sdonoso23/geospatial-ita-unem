library(spdep)
library(sphet)
library(tseries)
library(ggplot2)
library(ggrepel)
library(lmtest)
library(maptools)
library(spgwr)

############ PROJECT - GEOSPATIAL ###########################################################

itadata = read.csv("dataprogetto.csv", sep = ",", dec=".")

italia = readShapePoly("Reg")
plot(italia)
itacoords = coordinates(italia)
itacoords

# OLS Model
model_lm = lm(UNEM ~ INCAVG + EXPCAP + IMPCAP, data = itadata)
summary(model_lm)

# facciamo matrix W

contnb = knearneigh(itacoords,4)
knbita = knn2nb(contnb)
w_ita = nb2listw(knbita,style="W")




# Moran's I test

lm.morantest(model_lm, w_ita)
lm.LMtests(model_lm,w_ita,test="all")

# BP test and JB 

BP = bptest(model_lm)
BP # Since it is not significant, for this model we assume Homoscedasticity

JB = jarque.bera.test(model_lm$residuals)
JB # Since it is not significant, for this model we assume Normality

# Lag Unemployment
UNEMlag = lag.listw(w_ita,itadata$UNEM)

plot(itadata$UNEM,UNEMlag)

model_test=lm(UNEMlag~itadata$UNEM)
abline(model_test)
itadata = cbind.data.frame(itadata,UNEMlag)
ggplot(data = itadata, mapping = aes(UNEM, UNEMlag)) + geom_point() +
  geom_smooth(method="lm",se=T) + geom_text_repel(aes(label=Territorio.y))


# SARAR Model with ML

model_sarar = sacsarlm(UNEM ~ INCAVG + EXPCAP + IMPCAP, data = itadata, listw = w_ita)
summary(model_sarar)

# SARAR Model using the Generalized spatial two-stage least square method
model_sarartsls = gstsls(UNEM ~ INCAVG + EXPCAP + IMPCAP, data = itadata, listw = w_ita)
summary(model_sarartsls)

# Spatial Lag Model with ML

model_lag = lagsarlm(UNEM ~ INCAVG + EXPCAP + IMPCAP, data = itadata, listw = w_ita)
summary(model_sllm)

# Spatial Lag Model with the two-stage least squares estimator

model_lmtsls = stsls(UNEM ~ INCAVG + EXPCAP + IMPCAP, data = itadata, listw = w_ita)
summary(model_lmtsls)

# Spatial Error Model with ML

model_semlm = errorsarlm(UNEM ~ INCAVG + EXPCAP + IMPCAP, data = itadata, listw = w_ita)
summary(model_semlm)

# Spatial Error Model with Generalized 

model_semgls = GMerrorsar(UNEM ~ INCAVG + EXPCAP + IMPCAP, data = itadata, listw = w_ita)
summary(model_semgls)

# Heteroscedastic SARAR Model

model_hetsar = gstslshet(UNEM ~ INCAVG + EXPCAP + IMPCAP data = itadata, listw = w_ita)
summary(model_hetsar)

# Pure Spatial Model

model_auto = spautolm(UNEM ~ 1, data = itadata, listw = w_ita)
summary(model_auto)

#Final Model

model_final <- lagsarlm(UNEM ~ EXPCAP, data = itadata, listw = w_ita)

# Let's now calculate also the indirect impacts
impacts(model_final, listw = w_ita)


######################################################

# GWR - Geographically Weighted Regression Model

bw = gwr.sel(UNEM ~ INCAVG + EXPCAP + IMPCAP, data = itadata, itacoords, gweight = gwr.Gauss, adapt = T)

model_gwr = gwr(UNEM ~ INCAVG + EXPCAP + IMPCAP, data = itadata, itacoords, adapt = bw, hatmatrix = T)
model_gwr


######################## LAG ########################################################

# Lag of EXP

EXPlag = lag.listw(w_ita,itadata$EXP)

plot(itadata$EXP,EXPlag)

model_test=lm(EXPlag~itadata$EXP)
abline(model_test)
itadata = cbind.data.frame(itadata,EXPlag)
ggplot(data = itadata, mapping = aes(EXP, EXPlag)) + geom_point() +
  geom_smooth(method="lm",se=T) + geom_text_repel(aes(label=Territorio.y))

# Lag of EXPCAP

EXPCAPlag = lag.listw(w_ita,itadata$EXPCAP)

plot(itadata$EXPCAP,EXPCAPlag)

model_test=lm(EXPCAPlag~itadata$EXPCAP)
abline(model_test)
itadata = cbind.data.frame(itadata,EXPCAPlag)
ggplot(data = itadata, mapping = aes(EXPCAP, EXPCAPlag)) + geom_point() +
  geom_smooth(method="lm",se=T) + geom_text_repel(aes(label=Territorio.y))



# Lag of IMP

IMPlag = lag.listw(w_ita,itadata$IMP)

plot(itadata$IMP,IMPlag)

model_test=lm(IMPlag~itadata$IMP)
abline(model_test)
itadata = cbind.data.frame(itadata,IMPlag)
ggplot(data = itadata, mapping = aes(IMP, IMPlag)) + geom_point() +
  geom_smooth(method="lm",se=T) + geom_text_repel(aes(label=Territorio.y))

# Lag of IMPCAP

IMPCAPlag = lag.listw(w_ita,itadata$IMPCAP)

plot(itadata$IMPCAP,IMPCAPlag)

model_test=lm(IMPCAPlag~itadata$IMP)
abline(model_test)
itadata = cbind.data.frame(itadata,IMPCAPlag)
ggplot(data = itadata, mapping = aes(IMP, IMPlag)) + geom_point() +
  geom_smooth(method="lm",se=T) + geom_text_repel(aes(label=Territorio.y))

###############################################################################



