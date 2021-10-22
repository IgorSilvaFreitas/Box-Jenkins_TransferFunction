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



library(gridExtra)
pdf("grafser.pdf")
grid.arrange(g1,g2,g3,g4,g5, ncol=2,nrow=3)
dev.off()

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


#Transformação devido a grandeza dos números
ts_agro <- ts_agro/10000

ts_past <- ts_past/10000

ts_flor <- ts_flor/10000

ts_gado <- ts_gado/10000

ts_focos <- ts_focos/1000



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



library(gridExtra)
pdf("graf_pri_diff.pdf")
grid.arrange(ga,gb,gc,gd,ge, ncol=2,nrow=3)
dev.off()


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



library(gridExtra)
pdf("graf_seg_diff.pdf")
grid.arrange(ga2,gb2,gc2,gd2,ge2, ncol=2,nrow=3)
dev.off()


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


#Agricultura
fac_agro <- acf(diff(ts_agro, differences = 2))
facp_agro <- pacf(diff(ts_agro, differences = 2))
par(mfrow=c(2,1))
plot(fac_agro, xlab="Defasagem", ylab="FAC", main = "")
plot(facp_agro, xlab="Defasagem", ylab="FACP", main = "")


#Focos de queimadas
fac_focos <- acf(diff(ts_focos, differences = 2))
facp_focos <- pacf(diff(ts_focos, differences = 2))
par(mfrow=c(2,1))
plot(fac_focos, xlab="Defasagem", ylab="FAC", main = "")
plot(facp_focos, xlab="Defasagem", ylab="FACP", main = "")


#Arima
#Para o caso (0,2,0) fac e facp decaem a uma taxa relativamente lenta, sugerindo
#que mais diferenciação pode ser necessária. A autocorrelação e autocorrelação parcial
#funções das segundas diferenças ∇2𝑧
#𝑡 (não mostrado) eram bastante pequenos, sugerindo um branco
#processo de ruído para as segundas diferenças (box,jenkins,lung,outro)
install.packages("forecast")
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

#validação dos modelos
Box.test(fit1$residuals, lag = 1, type = "Box-Pierce", fitdf = 0)
Box.test(fit2$residuals, lag = 1, type = "Box-Pierce", fitdf = 0)
Box.test(fit3$residuals, lag = 1, type = "Box-Pierce", fitdf = 0)
Box.test(fit4$residuals, lag = 1, type = "Box-Pierce", fitdf = 0)
Box.test(fit5$residuals, lag = 1, type = "Box-Pierce", fitdf = 0)

shapiro.test(fit1$residual)
shapiro.test(fit2$residual)
shapiro.test(fit3$residual)
shapiro.test(fit4$residual[-c(30,31)])
shapiro.test(fit5$residual)

png("res_agro.png")
qqnorm(fit4$residuals[-c(20,30,31)], xlab="Quantis teóricos", ylab="Quantis amostrais")
qqline(fit4$residuals)
dev.off()

library(moments)
skewness(fit4$residual)


## Gráficos de valor efetivo vs valor ajustado

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

pdf("slide_1.pdf")
grid.arrange(gea1,gea2,gea3 ,ncol=2,nrow=2)
dev.off()



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
par(mfrow=c(2,1))


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
grid.arrange(gea1,gea2,gea3,gea4,gea5 ,ncol=2,nrow=3)
dev.off()
