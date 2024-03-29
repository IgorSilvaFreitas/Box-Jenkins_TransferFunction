#Códigos R do TCC 

#Setando a pasta com as bases de dados
setwd("C:/Users/igorg/Documents/TCC1/AS bases utilizadas")

#Chamando a base de dados
base <- readxl::read_excel("dados_tcc_PR_MT.xlsx")

## Gráficos temporalmente dependente para as séries
library(ggplot2)
library(scales)
library(dplyr)

##-----------------------------------------------------------------------------------
#Área de agricultura
g1 <- base |> ggplot(mapping = aes(x=Ano, y = `Agricultura (Ha)`/10000))+
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
g3 <- base[-36,] |> ggplot(mapping = aes(x=Ano, y = `Floresta Nativa(Ha)`/10000))+
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
  labs(x = "Ano",
       y = "Metros cúbicos de madeira \n (10 mil m^3)",
       title = "")+
  theme_classic()+
  scale_x_continuous(breaks = seq(1986, 2020, 4))+
  scale_y_continuous(labels= scales::comma)



library(gridExtra)
pdf("grafser_e.pdf")
grid.arrange(g1,g2,g6,g3,g4,g5, ncol=2,nrow=3)
dev.off()
##-----------------------------------------------------------------------------------

## Transformando as variáveis em Séries temporais
library(tseries)# carregando o pacote séries temporais
# library(TSA)
# 
# dados.ts <- ts(base)

#Treino
ts_agro_trei <- ts(base[-c(31,32,33,34,35,36),5], start=1985, end = 2014, frequency=1)

ts_past_trei <- ts(base[-c(31,32,33,34,35,36),3], start=1985, end = 2014, frequency=1)

ts_flor_trei <- ts(base[-c(31,32,33,34,35,36),2], start=1985, end = 2014, frequency=1)

ts_gado_trei <- ts(base[-c(31,32,33,34,35,36),4], start=1985, end = 2014, frequency=1)

ts_focos_trei <- ts(base[-c(1:14,31,32,33,34,35,36),6], start=1999, end = 2014, frequency=1)

ts_made_trei <- ts(base[-c(1,31,32,33,34,35,36),7], start=1986, end = 2014, frequency=1)

#Teste
ts_agro_test <- ts(base[c(31,32,33,34,35),5], start = 2015, end = 2019, frequency=1)

ts_past_test <- ts(base[c(31,32,33,34,35),3], start = 2015, end = 2019, frequency=1)

ts_flor_test <- ts(base[c(31,32,33,34,35),2], start = 2015, end = 2019, frequency=1)

ts_gado_test <- ts(base[c(31,32,33,34,35),4], start = 2015, end = 2019, frequency=1)

ts_focos_test <- ts(base[c(31,32,33,34,35),6], start = 2015, end = 2019, frequency=1)

ts_made_test <- ts(base[c(31,32,33,34,35),7], start = 2015, end = 2019, frequency=1)


#Transformação devido a grandeza dos números
ts_agro_trei <- ts_agro_trei/10000

ts_past_trei <- ts_past_trei/10000

ts_flor_trei <- ts_flor_trei/10000

ts_gado_trei <- ts_gado_trei/10000

ts_focos_trei <- ts_focos_trei/1000

ts_made_trei <- ts_made_trei/10000


ts_agro_test <- ts_agro_test/10000

ts_past_test <- ts_past_test/10000

ts_flor_test <- ts_flor_test/10000

ts_gado_test <- ts_gado_test/10000

ts_focos_test <- ts_focos_test/1000

ts_made_test <- ts_made_test/10000
##-----------------------------------------------------------------------------------

#Verificando se as séries são estacionárias através do teste aumentado de Dickey-Fuller
#adf.test(na.omit(diff(dados.ts[,"Agricultura(Ha)"])),k=1, alternative = "stationary")

#Lavouras
adf.test(ts_agro_trei,k=1, alternative = "stationary")
adf.test(diff(ts_agro_trei), k=1, alternative = "stationary")
adf.test(diff(ts_agro_trei, differences = 2), k=1, alternative = "stationary")

#m^3 de Madeira
adf.test(ts_made_trei,k=1, alternative = "stationary")


#Pastagem
adf.test(ts_past_trei,k=1, alternative = "stationary")
adf.test(diff(ts_past_trei, differences = 1), k=1, alternative = "stationary")
adf.test(diff(ts_past_trei, differences = 2), k=1, alternative = "stationary")


#Floresta nativa
adf.test(ts_flor_trei,k=1, alternative = "stationary")
adf.test(diff(ts_flor_trei, differences = 1), k=1, alternative = "stationary")
adf.test(diff(ts_flor_trei, differences = 2), k=1, alternative = "stationary")


#Efetivo bovino
adf.test(ts_gado_trei,k=1, alternative = "stationary")
adf.test(diff(ts_gado_trei, differences = 1), k=1, alternative = "stationary")


#Quantidade de focos de incendio
adf.test(ts_focos_trei,k=1, alternative = "stationary")
adf.test(diff(ts_focos_trei), k=1, alternative = "stationary")

#m^3 de madeira
adf.test(ts_made_trei,k=1, alternative = "stationary")

##-----------------------------------------------------------------------------------

#Gráficos das primeiras diferenças
ano <- base[-1,]$Ano
base_diff1 <- data.frame(ano)

agro1 <- diff(base$`Agricultura (Ha)`,differences = 1)
base_diff1$agro1 <- agro1

past1 <- diff(base$`Pastagem(Ha)`, differences = 1)
base_diff1$past1 <- past1

flor1 <- diff(base$`Floresta Nativa(Ha)`, differences = 1)
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
pdf("graf_pri_diff_e.pdf")
grid.arrange(ga,gb,gf,gc,gd,ge, ncol=2,nrow=3)
dev.off()
##-----------------------------------------------------------------------------------

#Gráficos da segunda diferença
ano <- base[-c(1,2),]$Ano
base_diff2 <- data.frame(ano)

agro2 <- diff(base$`Agricultura (Ha)`,differences = 2)
base_diff2$agro2 <- agro2

past2 <- diff(base$`Pastagem(Ha)`, differences = 2)
base_diff2$past2 <- past2

flor2 <- diff(base$`Floresta Nativa(Ha)`, differences = 2)
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



pdf("graf_seg_diff_e.pdf")
grid.arrange(ga2,gb2,gf2, gc2,gd2,ge2, ncol=2,nrow=3)
dev.off()
##-----------------------------------------------------------------------------------

## Verificando autocorrelação e autocorrelação parcial

#Floresta nativa
fac_flor <-acf(diff(ts_flor_trei,differences = 2))
facp_flor <-pacf(diff(ts_flor_trei,differences = 2))
par(mfrow=c(2,1))
plot(fac_flor, xlab="Defasagem", ylab="FAC", main = "")
plot(facp_flor, xlab="Defasagem", ylab="FACP", main = "")


#Pastagem
fac_past <- acf(diff(ts_past_trei, differences = 2))
facp_past <- pacf(diff(ts_past_trei, differences = 2))
par(mfrow=c(2,1))
plot(fac_past, xlab="Defasagem", ylab="FAC", main = "")
plot(facp_past, xlab="Defasagem", ylab="FACP", main = "")


#Efetivo bovino
fac_gado <- acf(diff(ts_gado_trei, differences = 2))
facp_gado <- pacf(diff(ts_gado_trei, differences = 2))
par(mfrow=c(2,1))
plot(fac_gado, xlab="Defasagem", ylab="FAC", main = "")
plot(facp_gado, xlab="Defasagem", ylab="FACP", main = "")

## Madeira
fac_made <- acf(diff(ts_made_trei, differences = 2))
facp_made <- pacf(diff(ts_made_trei, differences = 2))
par(mfrow=c(2,1))
plot(fac_made, xlab="Defasagem", ylab="FAC", main = "")
plot(facp_made, xlab="Defasagem", ylab="FACP", main = "")


#Agricultura
fac_agro <- acf(diff(ts_agro_trei, differences = 1))
facp_agro <- pacf(diff(ts_agro_trei, differences = 1))
par(mfrow=c(2,1))
plot(fac_agro, xlab="Defasagem", ylab="FAC", main = "")
plot(facp_agro, xlab="Defasagem", ylab="FACP", main = "")


#Focos de queimadas
fac_focos <- acf(diff(ts_focos_trei, differences = 1))
facp_focos <- pacf(diff(ts_focos_trei, differences = 1))
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
Arima(ts_flor_trei, order = c(0,2,0))
Arima(ts_flor_trei, order = c(0,2,1))
Arima(ts_flor_trei, order = c(1,2,0))
Arima(ts_flor_trei, order = c(1,2,1))
Arima(ts_flor_trei, order = c(0,2,2))
Arima(ts_flor_trei, order = c(2,2,0))
Arima(ts_flor_trei, order = c(1,2,2))
Arima(ts_flor_trei, order = c(2,2,1))
Arima(ts_flor_trei, order = c(2,2,2))


#Área de pastagem, d=2
Arima(ts_past_trei, order = c(0,2,0))
Arima(ts_past_trei, order = c(0,2,1))
Arima(ts_past_trei, order = c(1,2,0))
Arima(ts_past_trei, order = c(1,2,1))
Arima(ts_past_trei, order = c(0,2,2))
Arima(ts_past_trei, order = c(2,2,0))
Arima(ts_past_trei, order = c(1,2,2))
Arima(ts_past_trei, order = c(2,2,1))
Arima(ts_past_trei, order = c(2,2,2))


##Metros cúbicos de madeira, d=0
Arima(ts_made_trei, order = c(0,0,0))
Arima(ts_made_trei, order = c(0,0,1))
Arima(ts_made_trei, order = c(1,0,0))
Arima(ts_made_trei, order = c(1,0,1))
Arima(ts_made_trei, order = c(0,0,2))
Arima(ts_made_trei, order = c(2,0,0))
Arima(ts_made_trei, order = c(1,0,2))
Arima(ts_made_trei, order = c(2,0,1))
Arima(ts_made_trei, order = c(2,0,2))


#Efetivo bovino, d=1
Arima(ts_gado_trei, order = c(0,1,0))
Arima(ts_gado_trei, order = c(0,1,1))
Arima(ts_gado_trei, order = c(1,1,0))
Arima(ts_gado_trei, order = c(1,1,1))
Arima(ts_gado_trei, order = c(0,1,2))
Arima(ts_gado_trei, order = c(2,1,0))
Arima(ts_gado_trei, order = c(1,1,2), method = "ML")
Arima(ts_gado_trei, order = c(2,1,1))
Arima(ts_gado_trei, order = c(2,1,2))


#Área de agricultura, d=2
Arima(ts_agro_trei, order = c(0,2,0))
Arima(ts_agro_trei, order = c(0,2,1))
Arima(ts_agro_trei, order = c(1,2,0))
Arima(ts_agro_trei, order = c(1,2,1))
Arima(ts_agro_trei, order = c(0,2,2))
Arima(ts_agro_trei, order = c(2,2,0))
Arima(ts_agro_trei, order = c(1,2,2), method = "ML")
Arima(ts_agro_trei, order = c(2,2,1), method = "ML")
Arima(ts_agro_trei, order = c(2,2,2), method = "ML")

#Quantidade de focos de queimadas, d=1
Arima(ts_focos_trei, order = c(0,1,0))
Arima(ts_focos_trei, order = c(0,1,1))
Arima(ts_focos_trei, order = c(1,1,0))
Arima(ts_focos_trei, order = c(1,1,1))
Arima(ts_focos_trei, order = c(0,1,2))
Arima(ts_focos_trei, order = c(2,1,0))
Arima(ts_focos_trei, order = c(1,1,2))
Arima(ts_focos_trei, order = c(2,1,1), method = "ML")
Arima(ts_focos_trei, order = c(2,1,2))

##-----------------------------------------------------------------------------------

#Medidas de qualidade de ajuste

fit11 <- Arima(ts_flor_trei, order = c(1,2,0))
accuracy(ts_flor_trei,fit11$fitted)


fit222 <- Arima(ts_past_trei, order = c(1,2,0))
accuracy(ts_past_trei,fit222$fitted)


fit3 <- Arima(ts_gado_trei, order = c(0,2,1))
accuracy(ts_gado_trei,fit3$fitted)


fit4 <- Arima(ts_agro_trei, order = c(1,2,1))
accuracy(ts_agro_trei,fit4$fitted)


fit5 <- Arima(ts_focos_trei, order = c(1,1,0))
accuracy(ts_focos_trei,fit5$fitted)


fit6 <- Arima(ts_made_trei, order = c(1,0,0))
accuracy(ts_made_trei, fit6$fitted)

##-----------------------------------------------------------------------------------

#validação dos modelos
Box.test(fit11$residuals, lag = 1, type = "Box-Pierce", fitdf = 0)
Box.test(fit222$residuals, lag = 1, type = "Box-Pierce", fitdf = 0)
Box.test(fit3$residuals, lag = 1, type = "Box-Pierce", fitdf = 0)
Box.test(fit4$residuals, lag = 1, type = "Box-Pierce", fitdf = 0)
Box.test(fit5$residuals, lag = 1, type = "Box-Pierce", fitdf = 0)
Box.test(fit6$residuals, lag = 1, type = "Box-Pierce", fitdf = 0)

shapiro.test(fit11$residual)
shapiro.test(fit222$residual)
shapiro.test(fit3$residual)
shapiro.test(fit4$residual)
shapiro.test(fit5$residual)
shapiro.test(fit6$residual)

##-----------------------------------------------------------------------------------

## Gráficos de valor efetivo vs valor ajustado

#Floresta Nativa
res_flor <- data.frame(Ano=base$Ano[-c(31,32,33,34,35,36)], residuos=fit11$fitted)
gea1 <- base |> 
  ggplot(aes(x=Ano, y=`Floresta Nativa(Ha)`/10000))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=res_flor, aes(y=x), col="red")+
  geom_line(data=res_flor, aes(y=x, colour="red"))+
  scale_color_manual(name = "", values = c("Efetivo"="Black", "Ajustado"="red"))+
  labs(y="Área de floresta nativa")+ 
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))

#Área de pastagem
res_past <- data.frame(Ano=base$Ano[-c(31,32,33,34,35,36)], residuos=fit222$fitted)
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
res_gado <- data.frame(Ano=base$Ano[-c(31,32,33,34,35,36)], residuos=fit3$fitted)
gea3 <- base[-36,] |> 
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
res_made <- data.frame(Ano=base$Ano[-c(1,31,32,33,34,35,36)], x=fit6$fitted)
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

pdf("slide_1_e.pdf")
grid.arrange(gea1,gea2,gea3,gea6 ,ncol=2,nrow=2)
dev.off()


#Área de Agricultura
res_agro <- data.frame(Ano=base$Ano[-c(31,32,33,34,35,36)], residuos=fit4$fitted)
gea4 <- base |> 
  ggplot(aes(x=Ano, y=`Agricultura (Ha)`/10000))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=res_agro, aes(y=x), col="red")+
  geom_line(data=res_agro, aes(y=x, colour="red"))+
  scale_color_manual(name = "", values = c("Efetivo"="Black", "Ajustado"="red"))+
  labs(y="Área de Agricultura")+
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))


#Focos de queimada
res_foco <- data.frame(Ano=base$Ano[-c(1:14,31,32,33,34,35,36)], residuos=fit5$fitted)
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

pdf("slide_2_e.pdf")
grid.arrange(gea4,gea5 ,ncol=1,nrow=2)
dev.off()

#todos juntos
pdf("efe_aju_e.pdf")
grid.arrange(gea1,gea2,gea3,gea4,gea5,gea6 ,ncol=2,nrow=3)
dev.off()

##-----------------------------------------------------------------------------------

## correlação cruzada

#Transformando todas as séries em estacionárias

ts_flor_trei_est <- diff(ts_flor_trei, differences = 2)
ts_past_trei_est <- diff(ts_past_trei, differences = 2)
ts_gado_trei_est <- diff(ts_gado_trei, differences = 1)
ts_agro_trei_est <- diff(ts_agro_trei, differences = 2)
ts_foco_est <- diff(ts_focos_trei, differences = 1)


# cross correlation function
arima_past <- Arima(ts_past_trei_est, order=c(1,0,0))
arima_gado <- Arima(ts_gado_trei_est, order=c(1,0,0))
arima_agro <- Arima(ts_agro_trei_est, order=c(1,0,0))
arima_foco <-Arima(ts_foco_est, order=c(2,0,1), method = "ML")
arima_made <-Arima(ts_made_trei, order=c(2,0,0))


par(mfrow=c(3,2))

modelo_florpast <- Arima(ts_flor_trei_est, model=arima_past)
ccf(modelo_florpast$residuals, arima_past$residuals, xlim=c(0,13),
    xlab="Defasagem", ylab="FAC", main="Correlação cruzada - Floresta Nativa x Pastagem")

modelo_flormade <- Arima(ts_flor_trei_est, model=arima_made)
ccf(modelo_flormade$residuals, arima_made$residuals, xlim=c(0,13),
    xlab="Defasagem", ylab="FAC", main="Correlação cruzada - Floresta Nativa x Extração de Madeira")

modelo_florgado <- Arima(ts_flor_trei_est, model=arima_gado)
ccf(modelo_florpast$residuals, arima_gado$residuals, xlim=c(0,13),
    xlab="Defasagem", ylab="FAC", main="Correlação cruzada - Floresta Nativa x Efetivo Bovino")

modelo_floragro <- Arima(ts_flor_trei_est, model=arima_agro)
ccf(modelo_floragro$residuals, arima_agro$residuals, xlim=c(0,13),
    xlab="Defasagem", ylab="FAC", main="Correlação cruzada - Floresta Nativa x Agricultura")

modelo_florfoco <- Arima(ts_flor_trei_est, model=arima_foco)
ccf(modelo_florfoco$residuals, arima_foco$residuals, xlim=c(0,13),
    xlab="Defasagem", ylab="FAC", main="Correlação cruzada - Floresta Nativa x Focos")


##-----------------------------------------------------------------------------------

# ## Previsão com modelos univariados
# 
# plot_flor <- forecast(fit11, h=11)
# prev_flor <- data.frame(Ano=c(2020:2030), previsao=plot_flor$mean, min=plot_flor$lower, max=plot_flor$upper)
# 
# prev_flor_agr <- full_join(res_flor, prev_flor, by="Ano")
# prev1 <- prev_flor_agr |>
#   ggplot(aes(x=Ano, y=x))+
#   geom_point(col="black")+
#   geom_line(col="black")+
#   geom_point(aes(y=previsao), col="blue")+
#   geom_line(aes(y=previsao, colour="blue"))+
#   geom_errorbar(aes(ymin = min.95.,
#                     ymax = max.95.),
#                 col="Red",width = .1)+
#   scale_color_manual(name = "", values = c("Previs�o"="blue"))+
#   labs(y="Floresta Nativa", x="Ano")+
# 
#   theme_classic()+
#   theme(legend.position = "top", text = element_text(size=15))
# 
# plot_past <- forecast(fit222, h=11)
# prev_past <- data.frame(Ano=c(2020:2030), previsao=plot_past$mean, min=plot_past$lower, max=plot_past$upper)
# 
# prev_past_agr <- full_join(res_past, prev_past, by="Ano")
# prev2 <- prev_past_agr |>
#   ggplot(aes(x=Ano, y=x))+
#   geom_point(col="black")+
#   geom_line(col="black")+
#   geom_point(aes(y=previsao), col="blue")+
#   geom_line(aes(y=previsao, colour="blue"))+
#   geom_errorbar(aes(ymin = min.95.,
#                     ymax = max.95.),
#                 col="Red",width = .1)+
#   scale_color_manual(name = "", values = c("Previs�o"="blue"))+
#   labs(y="Pastagem", x="Ano")+
# 
#   theme_classic()+
#   theme(legend.position = "top", text = element_text(size=15))
# 
# 
# plot_gado <- forecast(fit3, h=11)
# prev_gado <- data.frame(Ano=c(2020:2030), previsao=plot_gado$mean, min=plot_gado$lower, max=plot_gado$upper)
# 
# prev_gado_agr <- full_join(res_gado, prev_gado, by="Ano")
# prev3 <- prev_gado_agr |>
#   ggplot(aes(x=Ano, y=x))+
#   geom_point(col="black")+
#   geom_line(aes(colour="black"))+
#   geom_point(aes(y=previsao), col="blue")+
#   geom_line(aes(y=previsao, colour="blue"))+
#   geom_errorbar(aes(ymin = min.95.,
#                     ymax = max.95.),
#                 col="Red",width = .1)+
#   scale_color_manual(name = "", values = c("Previs�o"="blue"))+
#   labs(y="Efetivo bovino", x="Ano")+
#   theme_classic()+
#   theme(legend.position = "top", text = element_text(size=15))
# 
# 
# plot_agro <- forecast(fit4, h=11)
# prev_agro <- data.frame(Ano=c(2020:2030), previsao=plot_agro$mean, min=plot_agro$lower, max=plot_agro$upper)
# 
# prev_agro_agr <- full_join(res_agro, prev_agro, by="Ano")
# prev4 <- prev_agro_agr |>
#   ggplot(aes(x=Ano, y=x))+
#   geom_point(col="black")+
#   geom_line(col="black")+
#   geom_point(aes(y=previsao), col="blue")+
#   geom_line(aes(y=previsao, colour="blue"))+
#   geom_errorbar(aes(ymin = min.95.,
#                     ymax = max.95.),
#                 col="Red",width = .1)+
#   scale_color_manual(name = "", values = c("Previs�o"="blue"))+
#   labs(y="Agricultura", x="Ano")+
# 
#   theme_classic()+
#   theme(legend.position = "top", text = element_text(size=15))
# 
# 
# plot_foco <- forecast(fit5, h=10)
# prev_foco <- data.frame(Ano=c(2021:2030), previsao=plot_foco$mean, min=plot_foco$lower, max=plot_foco$upper)
# 
# prev_foco_agr <- full_join(res_foco, prev_foco, by="Ano")
# prev5 <- prev_foco_agr |>
#   ggplot(aes(x=Ano, y=x))+
#   geom_point(col="black")+
#   geom_line(col="black")+
#   geom_point(aes(y=previsao), col="blue")+
#   geom_line(aes(y=previsao, colour="blue"))+
#   geom_errorbar(aes(ymin = min.95.,
#                     ymax = max.95.),
#                 col="Red",width = .1)+
#   scale_color_manual(name = "", values = c("Previs�o"="blue"))+
#   labs(y="Focos de queimadas", x="Ano")+
# 
#   theme_classic()+
#   theme(legend.position = "top", text = element_text(size=15))
# 
# plot_made <- forecast(fit6, h=10)
# prev_made <- data.frame(Ano=c(2021:2030), previsao=plot_made$mean, min=plot_made$lower, max=plot_made$upper)
# 
# prev_made_agr <- full_join(res_made, prev_made, by="Ano")
# prev6 <- prev_made_agr |>
#   ggplot(aes(x=Ano, y=x))+
#   geom_point(col="black")+
#   geom_line(col="black")+
#   geom_point(aes(y=previsao), col="blue")+
#   geom_line(aes(y=previsao, colour="blue"))+
#   geom_errorbar(aes(ymin = min.95.,
#                     ymax = max.95.),
#                 col="Red",width = .1)+
#   scale_color_manual(name = "", values = c("Previs�o"="blue"))+
#   labs(y="Extra��o de madeira", x="Ano")+
# 
#   theme_classic()+
#   theme(legend.position = "top", text = element_text(size=15))
# 
# pdf("previs�es_e.pdf")
# grid.arrange(prev1,prev2, prev6, prev3,prev4,prev5 ,ncol=2,nrow=3)
# dev.off()

#-------------------------------------------------------------------------------


## ARIMA simples

plot_flor <- forecast(fit11, h=5)
prev_flor <- data.frame(Ano=c(2015:2019), previsao=plot_flor$mean, min=plot_flor$lower, max=plot_flor$upper)

prev_flor_agr <- full_join(res_flor, prev_flor, by="Ano")
prev_flor_agr |>
  ggplot(aes(x=Ano, y=x))+
  geom_point(col="black")+
  geom_line(col="black")+
  geom_point(aes(y=previsao), col="blue")+
  geom_line(aes(y=previsao, colour="blue"))+
  geom_errorbar(aes(ymin = min.95.,
                    ymax = max.95.),
                col="Red",width = .1)+
  scale_color_manual(name = "", values = c("Previs�o"="blue"))+
  labs(y="Floresta Nativa", x="Ano")+

  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))


#Métricas de qualidade de ajuste
accuracy(ts_flor_test,plot_flor$mean)



#-------------------------------------------------------------------------------

library(TSA)

## FT com pastagem
fitduplo <- arimax(ts_flor_trei, order=c(0,2,1), xtransf=ts_past_trei,
                   transfer=list(c(0,0)))

res_duplo <- data.frame(Ano=base$Ano[-c(31,32,33,34,35,36)], x=fitted(fitduplo))
pdf("flor_past_model.pdf")
base[-c(31,32,33,34,35,36),] |> 
  ggplot(aes(x=Ano, y=`Floresta Nativa(Ha)`/10000))+
  geom_point(col="black")+
  geom_line(aes(colour="black"))+
  geom_point(data=res_duplo, aes(y=eval.object.call.x.), col="red")+
  geom_line(data=res_duplo, aes(y=eval.object.call.x., colour="red"))+
  scale_color_manual(name = "", values = c("Efetivo"="Black", "Ajustado"="red"))+
  labs(y="Área de Floresta Nativa")+
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))


dev.off()

#Métricas de qualidade de ajuste
accuracy(ts_flor_trei,fitted(fitduplo))
fitduplo$aic


# Validação do modelo
Box.test(fitduplo$residuals, lag = 1, type = "Box-Pierce", fitdf = 0)

shapiro.test(fitduplo$residual)



##Previsão modelo de transferência relacionados
pdf("prev_duplo.pdf")
plot_duplo <- forecast(fitted(fitduplo), h=5)
prev_duplo <- data.frame(Ano=c(2015:2019), previsao=plot_duplo$mean, min=plot_duplo$lower, max=plot_duplo$upper)

prev_duplo_agr <- full_join(res_duplo, prev_duplo, by="Ano")
prev_duplo_agr |> 
  ggplot(aes(x=Ano, y=eval.object.call.x.))+
  geom_point(col="black")+
  geom_line(col="black")+
  geom_point(aes(y=previsao), col="blue")+
  geom_line(aes(y=previsao, colour="blue"))+
  geom_errorbar(aes(ymin = min.95., 
                    ymax = max.95.), 
                col="Red",width = .1)+
  scale_color_manual(name = "", values = c("Previs�o"="blue"))+
  labs(y="Floresta Nativa", x="Ano")+
  
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))

#Métricas de qualidade de ajuste
accuracy(ts_flor_test,plot_duplo$mean)

#-----------------------------------------------------------

#TF com pastagem e focos de queimadas

auxiliar <- data.frame(ts_past_trei[-c(1:14)],ts_focos_trei)
auxiliar <- na.omit(auxiliar)
fittriplo <- arimax(ts_flor_trei[-c(1:14)], order=c(0,2,1), xtransf=auxiliar,
                    transfer=list(c(0,0),c(0,2)), method = "ML")

res_triplo <- data.frame(Ano=c(1999:2014), x=fitted(fittriplo))
pdf("flor_past_model.pdf")
base[-c(1:14,31,32,33,34,35,36),] |> 
  ggplot(aes(x=Ano, y=`Floresta Nativa(Ha)`/10000))+
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
accuracy(ts_flor_trei,fitted(fittriplo))
fittriplo$aic


# Validação do modelo
Box.test(fittriplo$residuals, lag = 1, type = "Box-Pierce", fitdf = 0)

shapiro.test(fittriplo$residual)

##Previsão modelo de transferência relacionados
pdf("prev_duplo.pdf")
plot_triplo <- forecast(fitted(fittriplo), h=5)
prev_triplo <- data.frame(Ano=c(2015:2019), previsao=plot_triplo$mean, min=plot_triplo$lower, max=plot_triplo$upper)

prev_triplo_agr <- full_join(res_triplo, prev_triplo, by="Ano")
prev_triplo_agr |> 
  ggplot(aes(x=Ano, y=x))+
  geom_point(col="black")+
  geom_line(col="black")+
  geom_point(aes(y=previsao), col="blue")+
  geom_line(aes(y=previsao, colour="blue"))+
  geom_errorbar(aes(ymin = min.95., 
                    ymax = max.95.), 
                col="Red",width = .1)+
  scale_color_manual(name = "", values = c("Previs�o"="blue"))+
  labs(y="Floresta Nativa", x="Ano")+
  
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))

#Métricas de qualidade de ajuste
prev_triplo_comp <- ts(plot_triplo$mean, start = 2015, end = 2019, frequency=1)
accuracy(ts_flor_test,prev_triplo_comp)



#---------------------------------------------------------------------------------------------
#TF com focos de queimadas
fitduplo2 <- arimax(ts_flor_trei[-c(1:14)], order=c(0,2,1), xtransf=ts_focos_trei,
                    transfer=list(c(0,2)), method = "ML")

res_duplo2 <- data.frame(Ano=c(1999,2014), x=fitted(fitduplo2))

#Métricas de qualidade de ajuste
accuracy(ts_flor_trei,fitted(fitduplo2))
fitduplo2$aic

## Previs�es
plot_duplo2 <- forecast(fitted(fitduplo2), h=5)
prev_duplo2 <- data.frame(Ano=c(2015:2019), previsao=plot_duplo2$mean, min=plot_duplo2$lower, max=plot_duplo2$upper)

prev_duplo2_agr <- full_join(res_duplo2, prev_duplo2, by="Ano")

#Métricas de qualidade de ajuste
prev_duplo2_comp <- ts(plot_duplo2$mean, start = 2015, end = 2019, frequency=1)
accuracy(ts_flor_test,prev_duplo2_comp)



#---------------------------------------------------------------------------------------------

## Utilizando todas as séries temporais para predição do desmatamento

auxiliar3 <- data.frame(ts_past_trei[-c(1:14)],ts_gado_trei[-c(1:14)],ts_agro_trei[-c(1:14)], ts_focos_trei[-22])
fitfull <- arimax(ts_flor_trei[-c(1:14,36)], order=c(0,2,1), xtransf=auxiliar3,
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
accuracy(ts_flor_trei[-c(1:16,36)],na.omit(fitted(fitfull)))
fitfull$aic
#150.1924




## Comparando as previs�es do ARIMA simples com a FT3
plot_flor2 <- forecast(fit11, h=5)
prev_flor2 <- data.frame(Ano=c(2015:2019), previsao=plot_flor$mean, min=plot_flor$lower, max=plot_flor$upper)

plot_triplo2 <- forecast(fitted(fittriplo), h=5)
prev_triplo2 <- data.frame(Ano=c(2015:2019), previsao=plot_triplo$mean, min=plot_triplo$lower, max=plot_triplo$upper)



## Plot previs�es finais com IC


plot_triplo2 <- forecast(fitted(fittriplo), h=16)
prev_triplo2 <- data.frame(Ano=c(2015:2030), previsao=plot_triplo2$mean, min=plot_triplo2$lower, max=plot_triplo2$upper)

prev_triplo_agr2 <- full_join(base, prev_triplo2, by="Ano")
pdf("prev_e_final.pdf")
prev_triplo_agr2 |> 
  ggplot(aes(x=Ano, y=`Floresta Nativa(Ha)`/10000))+
  geom_point(col="black")+
  geom_line(col="black")+
  geom_point(aes(y=previsao), col="blue")+
  geom_line(aes(y=previsao, colour="blue"))+
  geom_errorbar(aes(ymin = min.95., 
                    ymax = max.95.), 
                col="Red",width = .1)+
  scale_color_manual(name = "", values = c("Previs�o"="blue"))+
  labs(y="Floresta Nativa", x="Ano")+
  
  theme_classic()+
  theme(legend.position = "top", text = element_text(size=15))
dev.off()
