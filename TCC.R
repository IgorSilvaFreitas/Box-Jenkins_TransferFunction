#Códigos R do TCC 

#Setando a pasta com as bases de dados
setwd("D:/Users/Igor/Documents/TCC1/AS bases utilizadas")

#Chamando a base de dados
base <- readxl::read_excel("D:/Users/Igor/Documents/TCC1/AS bases utilizadas/Dados_melhorados.xlsx")
base <- base |> dplyr::rename(`Madeira`=`Área desmatada`)

## Gráficos temporalmente dependente para as séries
library(ggplot2)
library(scales)
library(dplyr)

##-----------------------------------------------------------------------------------
#Área de agricultura
g1 <- base |> ggplot(mapping = aes(x=Ano, y = `Agricultura(Ha)`/10000))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm",
              se = FALSE)+
  labs(x = "Ano",
       y = "Área de lavouras (10 mil hec.)",
       title = "")+
  theme_classic()+
  scale_x_continuous(breaks = seq(1985, 2020, 5))+
  scale_y_continuous(labels= scales::comma)


#Área de pastagem
g2 <- base[-36,] |> ggplot(mapping = aes(x=Ano, y = `Pastagem(Ha)`/10000))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm",
              se = FALSE)+
  labs(x = "Ano",
       y = "Área de pastagem (10 mil hec.)",
       title = "")+
  theme_classic()+
  scale_x_continuous(breaks = seq(1985, 2020, 5))+
  scale_y_continuous(labels= scales::comma)


#Área de floresta nativa
g3 <- base[-36,] |> ggplot(mapping = aes(x=Ano, y = `Floresta Natural(Ha)`/10000))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm",
              se = FALSE)+
  labs(x = "Ano",
       y = "Área de Floresta nativa (10 mil hec.)",
       title = "")+
  theme_classic()+
  scale_x_continuous(breaks = seq(1985, 2020, 5))+
  scale_y_continuous(labels= scales::comma)


#Efetivo de Bovinos
g4 <- base[-36,] |> ggplot(mapping = aes(x=Ano, y = `Efetivo Bovino`/10000))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm",
              se = FALSE)+
  labs(x = "Ano",
       y = "Efetivo de gado bovino \n (10 mil cabeças)",
       title = "")+
  theme_classic()+
  scale_x_continuous(breaks = seq(1985, 2020, 5))+
  scale_y_continuous(labels= scales::comma)


#Quantidade de focos de queimadas
g5 <- base[-c(1:14),] |> ggplot(mapping = aes(x=Ano, y = Focos/1000))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm",
              se=FALSE)+
  geom_smooth(data= base[-c(1:14),] |> filter(Ano <= 2005),
              method = "lm",
              se = FALSE,
              color="red")+
  geom_smooth(data= base[-c(1:14),] |> filter(Ano > 2005 & Ano <= 2013),
              method = "lm",
              se = FALSE,
              color="red")+
  geom_smooth(data= base[-c(1:14),] |> filter(Ano >2013),
              method = "lm",
              se = FALSE,
              color="red")+
  labs(x = "Ano",
       y = "Quantidade de focos de \n queimadas(mil focos)",
       title = "")+
  theme_classic()+
  scale_x_continuous(breaks = seq(2000, 2020, 5))+
  scale_y_continuous(labels= scales::comma)


## Metros cúbicos de madeira
g6 <- base[-1,] |> ggplot(mapping = aes(x=Ano, y = `Madeira`/10000))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm",
              se = FALSE)+
  geom_smooth(data= base[-1,] |> filter(Ano <= 2008),
              method = "lm",
              se = FALSE,
              color="red")+
  geom_smooth(data= base[-c(1:14),] |> filter(Ano > 2008),
              method = "lm",
              se = FALSE,
              color="red")+
  labs(x = "Ano",
       y = "Metros cúbicos de madeira \n (10 mil m^3)",
       title = "")+
  theme_classic()+
  scale_x_continuous(breaks = seq(1986, 2020, 4))+
  scale_y_continuous(labels= scales::comma)



library(gridExtra)
pdf("grafser.pdf")
grid.arrange(g1,g2,g3,g4,g5,g6, ncol=2,nrow=3)
dev.off()
##-----------------------------------------------------------------------------------

## Transformando as variáveis em Séries temporais
library(tseries)# carregando o pacote séries temporais
# library(TSA)
# 
# dados.ts <- ts(base)

ts_agro <- ts(base[-36,2], start=1985, end = 2019, frequency=1)

ts_past <- ts(base[-36,3], start=1985, end = 2019, frequency=1)

ts_flor <- ts(base[-36,4], start=1985, end = 2019, frequency=1)

ts_gado <- ts(base[-36,5], start=1985, end = 2019, frequency=1)

ts_focos <- ts(base[-c(1:14),6], start=1999, end = 2020, frequency=1)

ts_made <- ts(base[-1,7], start=1986, end = 2020, frequency=1)

#Transformação devido a grandeza dos números
ts_agro <- ts_agro/10000

ts_past <- ts_past/10000

ts_flor <- ts_flor/10000

ts_gado <- ts_gado/10000

ts_focos <- ts_focos/1000

ts_made <- ts_made/10000
##-----------------------------------------------------------------------------------

#Verificando se as séries são estacionárias através do teste aumentado de Dickey-Fuller
#adf.test(na.omit(diff(dados.ts[,"Agricultura(Ha)"])),k=1, alternative = "stationary")
#Lavouras
adf.test(ts_agro,k=1, alternative = "stationary")
adf.test(diff(ts_agro), k=1, alternative = "stationary")




#Pastagem
adf.test(ts_past,k=1, alternative = "stationary")
adf.test(diff(ts_past, differences = 1), k=1, alternative = "stationary")
adf.test(diff(ts_past, differences = 2), k=1, alternative = "stationary")


#Floresta nativa
adf.test(ts_flor,k=1, alternative = "stationary")
adf.test(diff(ts_flor, differences = 1), k=1, alternative = "stationary")
adf.test(diff(ts_flor, differences = 2), k=1, alternative = "stationary")


#Efetivo bovino
adf.test(ts_gado,k=1, alternative = "stationary")
adf.test(diff(ts_gado, differences = 1), k=1, alternative = "stationary")
adf.test(diff(ts_gado, differences = 2), k=1, alternative = "stationary")


#Quantidade de focos de incendio
adf.test(ts_focos,k=1, alternative = "stationary")
adf.test(diff(ts_focos), k=1, alternative = "stationary")

#Quantidade de focos de incendio
adf.test(ts_made,k=1, alternative = "stationary")
adf.test(diff(ts_made), k=1, alternative = "stationary")
adf.test(diff(ts_made, differences = 2), k=1, alternative = "stationary")
##-----------------------------------------------------------------------------------

#Gráficos das primeiras diferenças
ano <- base[-1,]$Ano
base_diff1 <- data.frame(ano)

agro1 <- diff(base$`Agricultura(Ha)`,differences = 1)
base_diff1$agro1 <- agro1

past1 <- diff(base$`Pastagem(Ha)`, differences = 1)
base_diff1$past1 <- past1

flor1 <- diff(base$`Floresta Natural(Ha)`, differences = 1)
base_diff1$flor1 <- flor1

gado1 <- diff(base$`Efetivo Bovino`, differences = 1)  
base_diff1$gado1 <- gado1

foco1 <- diff(base$Focos, differences = 1)
base_diff1$foco1 <- foco1

made1 <- diff(base$Madeira, differences = 1)
base_diff1$made1 <- made1

#Agricultura
ga <- base_diff1[-35,] |> ggplot(mapping = aes(x=ano, y = agro1/10000))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm",
              se = FALSE)+
  labs(x = "Ano",
       y = "Área de lavouras (10 mil hec.)",
       title = "")+
  theme_classic()+
  scale_x_continuous(breaks = seq(1985, 2020, 5))+
  scale_y_continuous(labels= scales::comma)

#Área de pastagem
gb <- base_diff1[-35,] |> ggplot(mapping = aes(x=ano, y = past1/10000))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm",
              se = FALSE)+
  labs(x = "Ano",
       y = "Área de pastagem (10 mil hec.)",
       title = "")+
  theme_classic()+
  scale_x_continuous(breaks = seq(1985, 2020, 5))+
  scale_y_continuous(labels= scales::comma)


#Área de floresta nativa
gc <- base_diff1[-35,] |> ggplot(mapping = aes(x=ano, y = flor1/10000))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm",
              se = FALSE)+
  labs(x = "Ano",
       y = "Área de Floresta nativa (10 mil hec.)",
       title = "")+
  theme_classic()+
  scale_x_continuous(breaks = seq(1985, 2020, 5))+
  scale_y_continuous(labels= scales::comma)


#Efetivo de Bovinos
gd <- base_diff1[-35,] |> ggplot(mapping = aes(x=ano, y = gado1/10000))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm",
              se = FALSE)+
  labs(x = "Ano",
       y = "Efetivo de gado bovino \n (10 mil cabeças)",
       title = "")+
  theme_classic()+
  scale_x_continuous(breaks = seq(1985, 2020, 5))+
  scale_y_continuous(labels= scales::comma)


#Quantidade de focos de incêndio
ge <- base_diff1[-c(1:14),] |> ggplot(mapping = aes(x=ano, y = foco1/1000))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm",
              se = FALSE)+
  labs(x = "Ano",
       y = "Quantidade de focos de \n queimadas(mil focos)",
       title = "")+
  theme_classic()+
  scale_x_continuous(breaks = seq(2000, 2020, 5))+
  scale_y_continuous(labels= scales::comma)

## Madeira em metros cúbicos
gf <- base_diff1[-1,] |> ggplot(mapping = aes(x=ano, y = made1/10000))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm",
              se = FALSE)+
  labs(x = "Ano",
       y = "Metros cúbicos de madeira \n (10 mil m^3)",
       title = "")+
  theme_classic()+
  scale_x_continuous(breaks = seq(1985, 2020, 5))+
  scale_y_continuous(labels= scales::comma)



library(gridExtra)
pdf("graf_pri_diff.pdf")
grid.arrange(ga,gb,gc,gd,ge, ncol=2,nrow=3)
dev.off()
##-----------------------------------------------------------------------------------

#Gráficos da segunda diferença
ano <- base[-c(1,2),]$Ano
base_diff2 <- data.frame(ano)

agro2 <- diff(base$`Agricultura(Ha)`,differences = 2)
base_diff2$agro2 <- agro2

past2 <- diff(base$`Pastagem(Ha)`, differences = 2)
base_diff2$past2 <- past2

flor2 <- diff(base$`Floresta Natural(Ha)`, differences = 2)
base_diff2$flor2 <- flor2

gado2 <- diff(base$`Efetivo Bovino`, differences = 2)  
base_diff2$gado2 <- gado2

foco2 <- diff(base$Focos, differences = 2)
base_diff2$foco2 <- foco2

made2 <- diff(base$Madeira, differences = 2)
base_diff2$made2 <- made2

#Agricultura
ga2 <- base_diff2[-34,] |> ggplot(mapping = aes(x=ano, y = agro2/10000))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm",
              se = FALSE)+
  labs(x = "Ano",
       y = "Área de lavouras (10 mil hec.)",
       title = "")+
  theme_classic()+
  scale_x_continuous(breaks = seq(1985, 2020, 5))+
  scale_y_continuous(labels= scales::comma)

#Área de pastagem
gb2 <- base_diff2[-34,] |> ggplot(mapping = aes(x=ano, y = past2/10000))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm",
              se = FALSE)+
  labs(x = "Ano",
       y = "Área de pastagem (10 mil hec.)",
       title = "")+
  theme_classic()+
  scale_x_continuous(breaks = seq(1985, 2020, 5))+
  scale_y_continuous(labels= scales::comma)


#Área de floresta nativa
gc2 <- base_diff2[-34,] |> ggplot(mapping = aes(x=ano, y = flor2/10000))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm",
              se = FALSE)+
  labs(x = "Ano",
       y = "Área de Floresta nativa (10 mil hec.)",
       title = "")+
  theme_classic()+
  scale_x_continuous(breaks = seq(1985, 2020, 5))+
  scale_y_continuous(labels= scales::comma)


#Efetivo de Bovinos
gd2 <- base_diff2[-34,] |> ggplot(mapping = aes(x=ano, y = gado2/10000))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm",
              se = FALSE)+
  labs(x = "Ano",
       y = "Efetivo de gado bovino \n (10 mil cabeças)",
       title = "")+
  theme_classic()+
  scale_x_continuous(breaks = seq(1985, 2020, 5))+
  scale_y_continuous(labels= scales::comma)


#Quantidade de focos de incêndio
ge2 <- base_diff2[-c(1:14),] |> ggplot(mapping = aes(x=ano, y = foco2/1000))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm",
              se = FALSE)+
  labs(x = "Ano",
       y = "Quantidade de focos de \n queimadas(mil focos)",
       title = "")+
  theme_classic()+
  scale_x_continuous(breaks = seq(2000, 2020, 5))+
  scale_y_continuous(labels= scales::comma)

## Metros cúbicos de madeira
gf2 <- base_diff2[-1,] |> ggplot(mapping = aes(x=ano, y = made2/10000))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm",
              se = FALSE)+
  labs(x = "Ano",
       y = "Metros cúbicos de madeira \n (10 mil m^3)",
       title = "")+
  theme_classic()+
  scale_x_continuous(breaks = seq(1986, 2020, 4))+
  scale_y_continuous(labels= scales::comma)



library(gridExtra)
pdf("graf_seg_diff.pdf")
grid.arrange(ga2,gb2,gc2,gd2,ge2, ncol=2,nrow=3)
dev.off()
##-----------------------------------------------------------------------------------

## Verificando autocorrelação e autocorrelação parcial

#Floresta nativa
fac_flor <-acf(diff(ts_flor,differences = 2))
facp_flor <-pacf(diff(ts_flor,differences = 2))
par(mfrow=c(2,1))
plot(fac_flor, xlab="Defasagem", ylab="FAC", main = "")
plot(facp_flor, xlab="Defasagem", ylab="FACP", main = "")


#Pastagem
fac_past <- acf(diff(ts_past, differences = 2))
facp_past <- pacf(diff(ts_past, differences = 2))
par(mfrow=c(2,1))
plot(fac_past, xlab="Defasagem", ylab="FAC", main = "")
plot(facp_past, xlab="Defasagem", ylab="FACP", main = "")


#Efetivo bovino
fac_gado <- acf(diff(ts_gado, differences = 2))
facp_gado <- pacf(diff(ts_gado, differences = 2))
par(mfrow=c(2,1))
plot(fac_gado, xlab="Defasagem", ylab="FAC", main = "")
plot(facp_gado, xlab="Defasagem", ylab="FACP", main = "")

## Madeira
fac_made <- acf(diff(ts_made, differences = 2))
facp_made <- pacf(diff(ts_made, differences = 2))
par(mfrow=c(2,1))
plot(fac_made, xlab="Defasagem", ylab="FAC", main = "")
plot(facp_made, xlab="Defasagem", ylab="FACP", main = "")


#Agricultura
fac_agro <- acf(diff(ts_agro, differences = 1))
facp_agro <- pacf(diff(ts_agro, differences = 1))
par(mfrow=c(2,1))
plot(fac_agro, xlab="Defasagem", ylab="FAC", main = "")
plot(facp_agro, xlab="Defasagem", ylab="FACP", main = "")


#Focos de queimadas
fac_focos <- acf(diff(ts_focos, differences = 1))
facp_focos <- pacf(diff(ts_focos, differences = 1))
par(mfrow=c(2,1))
plot(fac_focos, xlab="Defasagem", ylab="FAC", main = "")
plot(facp_focos, xlab="Defasagem", ylab="FACP", main = "")


##-----------------------------------------------------------------------------------

#Arima
#Para o caso (0,2,0) fac e facp decaem a uma taxa relativamente lenta, sugerindo
#que mais diferenciação pode ser necessária. A autocorrelação e autocorrelação parcial
#funções das segundas diferenças ∇2𝑧
#𝑡 (não mostrado) eram bastante pequenos, sugerindo um branco
#processo de ruído para as segundas diferenças (box,jenkins,lung,outro)

#install.packages("forecast")
library(forecast)

## Verificação dos Melhores modelos

#Floresta nativa, d=2
Arima(ts_flor, order = c(0,2,0))
Arima(ts_flor, order = c(0,2,1))
Arima(ts_flor, order = c(1,2,0))
Arima(ts_flor, order = c(1,2,1))
Arima(ts_flor, order = c(0,2,2))
Arima(ts_flor, order = c(2,2,0))
Arima(ts_flor, order = c(1,2,2))
Arima(ts_flor, order = c(2,2,1))
Arima(ts_flor, order = c(2,2,2))


#Área de pastagem, d=2
Arima(ts_past, order = c(0,2,0))
Arima(ts_past, order = c(0,2,1))
Arima(ts_past, order = c(1,2,0))
Arima(ts_past, order = c(1,2,1))
Arima(ts_past, order = c(0,2,2))
Arima(ts_past, order = c(2,2,0))
Arima(ts_past, order = c(1,2,2))
Arima(ts_past, order = c(2,2,1))
Arima(ts_past, order = c(2,2,2))


##Metros cúbicos de madeira, d=2
Arima(ts_made, order = c(0,2,0))
Arima(ts_made, order = c(0,2,1))
Arima(ts_made, order = c(1,2,0))
Arima(ts_made, order = c(1,2,1))
Arima(ts_made, order = c(0,2,2))
Arima(ts_made, order = c(2,2,0))
Arima(ts_made, order = c(1,2,2))
Arima(ts_made, order = c(2,2,1))
Arima(ts_made, order = c(2,2,2))


#Efetivo bovino, d=2
Arima(ts_gado, order = c(0,2,0))
Arima(ts_gado, order = c(0,2,1))
Arima(ts_gado, order = c(1,2,0))
Arima(ts_gado, order = c(1,2,1))
Arima(ts_gado, order = c(0,2,2))
Arima(ts_gado, order = c(2,2,0))
Arima(ts_gado, order = c(1,2,2))
Arima(ts_gado, order = c(2,2,1))
Arima(ts_gado, order = c(2,2,2))


#Área de agricultura, d=1
Arima(ts_agro, order = c(0,1,0))
Arima(ts_agro, order = c(0,1,1))
Arima(ts_agro, order = c(1,1,0))
Arima(ts_agro, order = c(1,1,1))
Arima(ts_agro, order = c(0,1,2))
Arima(ts_agro, order = c(2,1,0))
Arima(ts_agro, order = c(1,1,2), method = "ML")
Arima(ts_agro, order = c(2,1,1), method = "ML")
Arima(ts_agro, order = c(2,1,2), method = "ML")

#Quantidade de focos de queimadas, d=1
Arima(ts_focos, order = c(0,1,0))
Arima(ts_focos, order = c(0,1,1))
Arima(ts_focos, order = c(1,1,0))
Arima(ts_focos, order = c(1,1,1))
Arima(ts_focos, order = c(0,1,2))
Arima(ts_focos, order = c(2,1,0))
Arima(ts_focos, order = c(1,1,2))
Arima(ts_focos, order = c(2,1,1), method = "ML")
Arima(ts_focos, order = c(2,1,2))
auto.arima(ts_focos, test = "adf", trace=T, allowdrift = F)

##-----------------------------------------------------------------------------------

#Medidas de qualidade de ajuste
fit1 <- Arima(ts_flor, order = c(0,2,0))
accuracy(ts_flor,fit1$fitted)

fit11 <- Arima(ts_flor, order = c(0,2,1))
accuracy(ts_flor,fit11$fitted)

fit111 <- Arima(ts_flor, order = c(1,2,0))
accuracy(ts_flor,fit111$fitted)

fit2 <- Arima(ts_past, order = c(0,2,0))
accuracy(ts_past,fit2$fitted)

fit22 <- Arima(ts_past, order = c(0,2,2))
accuracy(ts_past,fit22$fitted)

fit222 <- Arima(ts_past, order = c(2,2,0))
accuracy(ts_past,fit222$fitted)

fit3 <- Arima(ts_gado, order = c(1,2,1))
accuracy(ts_gado,fit3$fitted)

fit4 <- Arima(ts_agro, order = c(1,1,0))
accuracy(ts_agro,fit4$fitted)

fit5 <- Arima(ts_focos, order = c(2,1,1), method = "ML")
accuracy(ts_focos,fit5$fitted)

fit6 <- Arima(ts_made, order = c(0,2,1))
accuracy(ts_made, fit6$fitted)

##-----------------------------------------------------------------------------------

#validação dos modelos
Box.test(fit1$residuals, lag = 1, type = "Box-Pierce", fitdf = 0)
Box.test(fit2$residuals, lag = 1, type = "Box-Pierce", fitdf = 0)
Box.test(fit3$residuals, lag = 1, type = "Box-Pierce", fitdf = 0)
Box.test(fit4$residuals, lag = 1, type = "Box-Pierce", fitdf = 0)
Box.test(fit5$residuals, lag = 1, type = "Box-Pierce", fitdf = 0)
Box.test(fit6$residuals, lag = 1, type = "Box-Pierce", fitdf = 0)

shapiro.test(fit1$residual)
shapiro.test(fit2$residual)
shapiro.test(fit3$residual)
shapiro.test(fit4$residual)
shapiro.test(fit4$residual[-c(30,31)])
shapiro.test(fit5$residual)
shapiro.test(fit6$residual)

##-----------------------------------------------------------------------------------

## Gráficos de valor efetivo vs valor ajustado

#Floresta Nativa
res_flor <- data.frame(Ano=base$Ano[-36], residuos=fit11$fitted)
gea1 <- base |> 
  ggplot(aes(x=Ano, y=`Floresta Natural(Ha)`/10000))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=res_flor, aes(y=x), col="red")+
  geom_line(data=res_flor, aes(y=x, colour="red"))+
  scale_color_manual(name = "", values = c("Efetivo"="Black", "Ajustado"="red"))+
  labs(y="Área de floresta nativa")+ 
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))

#Área de pastagem
res_past <- data.frame(Ano=base$Ano[-36], residuos=fit222$fitted)
gea2 <- base |> 
  ggplot(aes(x=Ano, y=`Pastagem(Ha)`/10000))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=res_past, aes(y=x), col="red")+
  geom_line(data=res_past, aes(y=x, colour="red"))+
  scale_color_manual(name = "", values = c("Efetivo"="Black", "Ajustado"="red"))+
  labs(y="Área de Pastagem")+
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))

#Efetivo Bovino
res_gado <- data.frame(Ano=base$Ano[-36], residuos=fit3$fitted)
gea3 <- base |> 
  ggplot(aes(x=Ano, y=`Efetivo Bovino`/10000))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=res_gado, aes(y=x), col="red")+
  geom_line(data=res_gado, aes(y=x, colour="red"))+
  scale_color_manual(name = "", values = c("Efetivo"="Black", "Ajustado"="red"))+
  labs(y="Efetivo bovino")+
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))


#Metros cúbicos de madeira
res_made <- data.frame(Ano=c(1986:2020), x=fit6$fitted)
gea6 <- base |> 
  ggplot(aes(x=Ano, y=Madeira/10000))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=res_made, aes(y=x), col="red")+
  geom_line(data=res_made, aes(y=x, colour="red"))+
  scale_color_manual(name = "", values = c("Efetivo"="Black", "Ajustado"="red"))+
  labs(y="Metros cúbicos de madeira")+
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))+
  scale_x_continuous(breaks = seq(1986, 2020, 4))+
  scale_y_continuous(labels= scales::comma)

pdf("slide_1.pdf")
grid.arrange(gea1,gea2,gea3,gea6 ,ncol=2,nrow=2)
dev.off()


#Área de Agricultura
res_agro <- data.frame(Ano=base$Ano[-36], residuos=fit4$fitted)
gea4 <- base |> 
  ggplot(aes(x=Ano, y=`Agricultura(Ha)`/10000))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=res_agro, aes(y=x), col="red")+
  geom_line(data=res_agro, aes(y=x, colour="red"))+
  scale_color_manual(name = "", values = c("Efetivo"="Black", "Ajustado"="red"))+
  labs(y="Área de Agricultura")+
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))


#Focos de queimada
res_foco <- data.frame(Ano=base$Ano[-c(1:14)], residuos=fit5$fitted)
gea5 <- base[-c(1:14),] |> 
  ggplot(aes(x=Ano, y=`Focos`/1000))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=res_foco, aes(y=x), col="red")+
  geom_line(data=res_foco, aes(y=x, colour="red"))+
  scale_color_manual(name = "", values = c("Efetivo"="Black", "Ajustado"="red"))+
  labs(y="Quantidade de focos de queimadas")+
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))

pdf("slide_2.pdf")
grid.arrange(gea4,gea5 ,ncol=1,nrow=2)
dev.off()

#todos juntos
pdf("efe_aju.pdf")
grid.arrange(gea1,gea2,gea3,gea4,gea5,gea6 ,ncol=2,nrow=3)
dev.off()

##-----------------------------------------------------------------------------------

## correlação cruzada

#Transformando todas as séries em estacionárias

ts_flor_est <- diff(ts_flor, differences = 2)
ts_past_est <- diff(ts_past, differences = 2)
ts_gado_est <- diff(ts_gado, differences = 2)
ts_made_est <- diff(ts_made, differences = 2)
ts_agro_est <- diff(ts_agro, differences = 1)
ts_foco_est <- diff(ts_focos, differences = 1)



# cross correlation function
arima_past <- Arima(ts_past_est, order=c(2,0,0))
arima_gado <- Arima(ts_gado_est, order=c(1,0,1))
arima_agro <- Arima(ts_agro_est, order=c(1,0,0))
arima_foco <-Arima(ts_foco_est, order=c(2,0,1))
arima_made <-Arima(ts_made_est, order=c(0,0,1))


par(mfrow=c(2,2))

modelo_florpast <- Arima(ts_flor_est, model=arima_past)
ccf(modelo_florpast$residuals, arima_past$residuals, xlim=c(0,13),
    xlab="Defasagem", ylab="FAC", main="Correlação cruzada - Floresta Nativa x Pastagem")

modelo_florgado <- Arima(ts_flor_est, model=arima_gado)
ccf(modelo_florpast$residuals, arima_gado$residuals, xlim=c(0,13),
    xlab="Defasagem", ylab="FAC", main="Correlação cruzada - Floresta Nativa x Efetivo Bovino")

modelo_floragro <- Arima(ts_flor_est, model=arima_agro)
ccf(modelo_floragro$residuals, arima_agro$residuals, xlim=c(0,13),
    xlab="Defasagem", ylab="FAC", main="Correlação cruzada - Floresta Nativa x Agricultura")

modelo_florfoco <- Arima(ts_flor_est, model=arima_foco)
ccf(modelo_florfoco$residuals, arima_foco$residuals, xlim=c(0,13),
    xlab="Defasagem", ylab="FAC", main="Correlação cruzada - Floresta Nativa x Focos")

modelo_flormade <- Arima(ts_made_est, model=arima_made)
ccf(modelo_flormade$residuals, arima_made$residuals, xlim=c(0,13),
    xlab="Defasagem", ylab="FAC", main="Correlação cruzada - Floresta Nativa x Madeira")

##-----------------------------------------------------------------------------------

## Previsão com modelos univariados

plot_flor <- forecast(fit11, h=11)
prev_flor <- data.frame(Ano=c(2020:2030), previsao=plot_flor$mean)
prev1 <- res_flor |> 
  ggplot(aes(x=Ano, y=x))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=prev_flor, aes(y=previsao), col="blue")+
  geom_line(data=prev_flor, aes(y=previsao, colour="blue"))+
  scale_color_manual(name = "", values = c("Previsão"="blue"))+
  labs(y="Floresta Nativa", x="Ano")+
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))

plot_past <- forecast(fit222, h=11)
prev_past <- data.frame(Ano=c(2020:2030), previsao=plot_past$mean)
prev2 <- res_past |> 
  ggplot(aes(x=Ano, y=x))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=prev_past, aes(y=previsao), col="blue")+
  geom_line(data=prev_past, aes(y=previsao, colour="blue"))+
  scale_color_manual(name = "", values = c("Previsão"="blue"))+
  labs(y="Pastagem", x="Ano")+
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))


plot_gado <- forecast(fit3, h=11)
prev_gado <- data.frame(Ano=c(2020:2030), previsao=plot_gado$mean)
prev3 <- res_gado |> 
  ggplot(aes(x=Ano, y=x))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=prev_gado, aes(y=previsao), col="blue")+
  geom_line(data=prev_gado, aes(y=previsao, colour="blue"))+
  scale_color_manual(name = "", values = c("Previsão"="blue"))+
  labs(y="Efetivo bovino", x="Ano")+
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))


plot_agro <- forecast(fit4, h=11)
prev_agro <- data.frame(Ano=c(2020:2030), previsao=plot_agro$mean)
prev4 <- res_agro |> 
  ggplot(aes(x=Ano, y=x))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=prev_agro, aes(y=previsao), col="blue")+
  geom_line(data=prev_agro, aes(y=previsao, colour="blue"))+
  scale_color_manual(name = "", values = c("Previsão"="blue"))+
  labs(y="Agricultura", x="Ano")+
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))



plot_foco <- forecast(fit5, h=10)
prev_foco <- data.frame(Ano=c(2021:2030), previsao=plot_foco$mean)
prev5 <- res_foco |> 
  ggplot(aes(x=Ano, y=x))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=prev_foco, aes(y=previsao), col="blue")+
  geom_line(data=prev_foco, aes(y=previsao, colour="blue"))+
  scale_color_manual(name = "", values = c("Previsão"="blue"))+
  labs(y="Focos de Queimada", x="Ano")+
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))

plot_made <- forecast(fit6, h=10)
prev_made <- data.frame(Ano=c(2021:2030), previsao=plot_made$mean)
prev6 <- res_made |> 
  ggplot(aes(x=Ano, y=x))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=prev_made, aes(y=previsao), col="blue")+
  geom_line(data=prev_made, aes(y=previsao, colour="blue"))+
  scale_color_manual(name = "", values = c("Previsão"="blue"))+
  labs(y="Metros cúbicos de madeira", x="Ano")+
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))

pdf("previsões.pdf")
grid.arrange(prev1,prev2,prev3,prev4,prev5 ,ncol=2,nrow=3)
dev.off()

#-------------------------------------------------------------------------------

library(TSA)

## FT com pastagem
fitduplo <- arimax(ts_flor[,-36], order=c(0,2,1), xtransf=ts_past,
                   transfer=list(c(0,0)))

res_duplo <- data.frame(Ano=base$Ano[-c(36)], x=fitted(fitduplo))
pdf("flor_past_model.pdf")
base[-36,] |> 
  ggplot(aes(x=Ano, y=`Floresta Natural(Ha)`/10000))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=res_duplo, aes(y=x), col="red")+
  geom_line(data=res_duplo, aes(y=x, colour="red"))+
  scale_color_manual(name = "", values = c("Efetivo"="Black", "Ajustado"="red"))+
  labs(y="Área de Floresta Nativa")+
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))


dev.off()

#Métricas de qualidade de ajuste
accuracy(ts_flor,fitted(fitduplo))
fitduplo$aic
#362.3526

# Validação do modelo
Box.test(fitduplo$residuals, lag = 1, type = "Box-Pierce", fitdf = 0)

shapiro.test(fitduplo$residual)



##Previsão modelo de transferência relacionados
pdf("prev_duplo.pdf")
plot_duplo <- forecast(fitted(fitduplo), h=11)
prev_duplo <- data.frame(Ano=c(2020:2030), previsao=plot_duplo$mean/10000)
res_duplo |> 
  ggplot(aes(x=Ano, y=x))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=prev_duplo, aes(y=previsao), col="blue")+
  geom_line(data=prev_duplo, aes(y=previsao, colour="blue"))+
  scale_color_manual(name = "", values = c("Previsão"="blue"))+
  labs(y="Floresta Nativa", x="Ano")+
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))
dev.off()



#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
## Tentando modelo melhor ajustado para a série de focos de queimada


#Exponencial suavizada
fithw <- HoltWinters(x=ts_focos,gamma=F)
fithw
plot(fithw)



#arfima
library(arfima)
fitarfima <- arfima(ts_focos, order=c(2,1,1))
fitted(fitarfima)

plot(tacvf(fitarfima, maxlag=25))
plot(ts_focos)


# Nenhum se ajustou muito bem
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#TF com pastagem e m^3 de madeira

auxiliar <- data.frame(ts_past,ts_made)
auxiliar <- na.omit(auxiliar)
fittriplo <- arimax(ts_flor[-36], order=c(0,2,1), xtransf=auxiliar,
                   transfer=list(c(0,0),c(0,0)))

res_triplo <- data.frame(Ano=base$Ano[-c(36)], x=fitted(fittriplo))
pdf("flor_past_model.pdf")
base[-36,] |> 
  ggplot(aes(x=Ano, y=`Floresta Natural(Ha)`/10000))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=res_triplo, aes(y=x), col="red")+
  geom_line(data=res_triplo, aes(y=x, colour="red"))+
  scale_color_manual(name = "", values = c("Efetivo"="Black", "Ajustado"="red"))+
  labs(y="Área de Floresta Nativa")+
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))


dev.off()

#Métricas de qualidade de ajuste
accuracy(ts_flor[-36],fitted(fittriplo))
fittriplo$aic
#364.3416, AIC aumentou, incrementar a variável madeira não melhorou o modelo


# Validação do modelo
Box.test(fittriplo$residuals, lag = 1, type = "Box-Pierce", fitdf = 0)

shapiro.test(fittriplo$residual)



#---------------------------------------------------------------------------------------------
#TF com m^3 de madeira
fitduplo2 <- arimax(ts_flor[,-36], order=c(0,2,1), xtransf=ts_made,
                   transfer=list(c(0,0)))

res_duplo2 <- data.frame(Ano=base$Ano[-c(36)], x=fitted(fitduplo2))

#Métricas de qualidade de ajuste
accuracy(ts_flor,fitted(fitduplo2))
fitduplo2$aic
#379.4998

#---------------------------------------------------------------------------------------------

## Utilizando todas as séries temporais para predição do desmatamento menos focos
auxiliar2 <- data.frame(ts_past,ts_gado,ts_agro)
fittudo <- arimax(ts_flor[-36], order=c(0,2,1), xtransf=auxiliar2,
                  transfer=list(c(0,0),c(0,2),c(0,2)), metho="ML")

res_tudo <- data.frame(Ano=base$Ano[-36], x=fitted(fittudo))
gtudo <- base[-36,] |> 
  ggplot(aes(x=Ano, y=`Floresta Natural(Ha)`/10000))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=res_tudo, aes(y=x), col="red")+
  geom_line(data=res_tudo, aes(y=x, colour="red"))+
  scale_color_manual(name = "", values = c("Efetivo"="Black", "Ajustado"="red"))+
  labs(y="Quantidade de focos de queimadas")+
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))



#Métricas de qualidade de ajuste
accuracy(ts_flor[-c(1,2)],na.omit(fitted(fittudo)))
fittudo$aic
# 309.3648, modelo melhorou

## Previsão
pdf("prev_tudo.pdf")
plot_tudo <- forecast(fitted(fittudo), h=11)
prev_tudo <- data.frame(Ano=c(2020:2030), previsao=plot_tudo$mean)
res_tudo |> 
  ggplot(aes(x=Ano, y=x))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=prev_tudo, aes(y=previsao), col="blue")+
  geom_line(data=prev_tudo, aes(y=previsao, colour="blue"))+
  scale_color_manual(name = "", values = c("Previsão"="blue"))+
  labs(y="Floresta Nativa", x="Ano")+
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))
dev.off()

## Utilizando todas as séries temporais para predição do desmatamento inclusive focos

auxiliar3 <- data.frame(ts_past[-c(1:14)],ts_gado[-c(1:14)],ts_agro[-c(1:14)], ts_focos[-22])
fitfull <- arimax(ts_flor[-c(1:14,36)], order=c(0,2,1), xtransf=auxiliar3,
                  transfer=list(c(0,0),c(0,2),c(0,2),c(0,2)), metho="ML")

res_full <- data.frame(Ano=base$Ano[-c(1:14,36)], x=fitted(fitfull))
gfull <- base[-c(1:14,36),] |> 
  ggplot(aes(x=Ano, y=`Floresta Natural(Ha)`/10000))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=res_full, aes(y=x), col="red")+
  geom_line(data=res_full, aes(y=x, colour="red"))+
  scale_color_manual(name = "", values = c("Efetivo"="Black", "Ajustado"="red"))+
  labs(y="Quantidade de focos de queimadas")+
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))



#Métricas de qualidade de ajuste
accuracy(ts_flor[-c(1:16,36)],na.omit(fitted(fitfull)))
fitfull$aic
#150.1924


## Previsão
pdf("prev_full.pdf")
plot_full <- forecast(fitted(fitfull), h=11)
prev_full <- data.frame(Ano=c(2020:2030), previsao=plot_full$mean)
res_full |> 
  ggplot(aes(x=Ano, y=x))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=prev_full, aes(y=previsao), col="blue")+
  geom_line(data=prev_full, aes(y=previsao, colour="blue"))+
  scale_color_manual(name = "", values = c("Previsão"="blue"))+
  labs(y="Floresta Nativa", x="Ano")+
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))
dev.off()
