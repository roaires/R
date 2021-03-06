  
#load packages
#library(lubridate)
#library(dplyr)
library(ggplot2)
#install.packages("corrplot")
library(corrplot)
#install.packages("zoo")
library(zoo)
#install.packages("xts")
library(xts)


#Definião da pasta de trabalho
setwd("C:/Projetos/r")
getwd()


dados <- read.csv("pm25.csv", TRUE, ",")
class(dados)
ncol(dados) 
head(dados) 
names(dados)


#Nomenclaturas dos rótulos
#No - Id da linha
#year, month, day e hour
#pm2.5 - indicador pm2.5
#DEWP - Ponto de orválho
#TEMP - Temperatúra
#PRES - Pressão
#cbwd - Direção combinada do vento
#Iws - Veloricade do vento acumulada
#Is - Horas acumuladas de neve
#Ir - Horas acumuladas de chuva

#Inclusão do campo data
dados$Data  = (with(dados, ISOdatetime( year, month, day, 0, 0, 0, tz = "" )))

#Criando variaveis para cada campo
dados.no = dados["No"]
dados.year = dados["year"]
dados.month = dados["month"]
dados.day = dados["day"]
dados.hour = dados["hour"]
dados.pm25 = dados["pm2.5"]
dados.DEWP = dados["DEWP"]
dados.TEMP = dados["TEMP"]
dados.PRES = dados["PRES"]
dados.cbwd = dados["cbwd"]
dados.Iws = dados["Iws"]
dados.Is = dados["Is"]
dados.Ir = dados["Ir"]
dados.Data = dados["Data"]

#Criando outro data frame para mudar a ordem de algumas colunas
df = data.frame(dados$cbwd,dados$Data, dados$pm2.5, dados$year,dados$month,dados$day,dados$hour,dados$DEWP, dados$TEMP, dados$PRES,  dados$Iws, dados$Is, dados$Ir )

#Informações do data frame
class(df)
ncol(df) 
head(df) 
names(df)

#Sumário Geral
summary(df)


print("Informações estatísticas por variável: ")
for(i in 1 : ncol(df) ) {
  if (!grepl(colnames(df)[i], "dados.cbwd")) {
    print(paste("", colnames(df)[i],":"))  
    minimo = min(df[, i], na.rm = TRUE)
    maximo = max(df[, i], na.rm = TRUE)
    media = mean(df[, i], na.rm = TRUE)
    mediana = median(df[, i], na.rm = TRUE)
    desviopadrao = sd(df[, i], na.rm = TRUE)
    variancia = var(df[, i], na.rm = TRUE)
    quartis = quantile(df[, i], na.rm = TRUE)
    
    print(paste(" ", "Mínimo: ",minimo))  
    print(paste(" ", "Máximo: ",maximo))  
    print(paste(" ", "Média: ",media))  
    print(paste(" ", "Mediana: ",mediana))  
    print(paste(" ", "Desvio Padrão: ",desviopadrao))  
    print(paste(" ", "Variância: ",variancia))  
    print(paste(" ", "Quartis: ")) 
    print(quartis)
  }
}


#Conhecendo os dados - Visualização
for(i in which(colnames(df) == "dados.pm2.5") : which(colnames(df) == "dados.Ir")) {
  if(is.factor(df[,i])) {
    print(ggplot(df, aes(df[, i])) +
            geom_histogram(stat = "count") +
            xlab(colnames(df)[i]))
  }
  else {
    print(ggplot(df, aes(df[, i])) +
            geom_histogram(binwidth = 2) +
            xlab(colnames(df)[i]))
  }
}


#Correlação Pm 2.5 com demais variáveis importantes
for(i in which(colnames(df) == "dados.year"):which(colnames(df) == "dados.Ir")) {
  if(!is.factor(df[,i])) {
    print(paste("pm2.5 & ", colnames(df)[i], sep = ""))
    print(cor(df$dados.pm2.5, df[, i], use = "complete.obs"))
    
    print(ggplot(df, aes(df[,i], dados.pm2.5)) +
            geom_point() +
            xlab(colnames(df)[i]))
  }
}

#Correlação entre demais váriáveis (sem considerar pm 2.5)
for(y in which(colnames(df) == "dados.year"):which(colnames(df) == "dados.Ir")) {
  for(i in which(colnames(df) == "dados.year"):which(colnames(df) == "dados.Ir")) {
    #Não realizar verificação para mesma coluna em x e Y
    #Removido dados.cbwd por não ser numérico
    if ((!grepl(colnames(df)[i], colnames(df)[y])) &  (!grepl(colnames(df)[i], "dados.cbwd")  &  (!grepl(colnames(df)[y], "dados.cbwd")))) {
      if(!is.factor(df[,i])) {
        print(paste(colnames(df)[y]," & ", colnames(df)[i], sep = ""))
        print(cor(df[, y], df[, i], use = "complete.obs"))
        
        #Criado uma codição para cada Y visando nomear corretamente no eixo do gráfico
        if (grepl(colnames(df)[y], "dados.year")) {
          print(ggplot(df, aes(df[,i], dados.year)) +
                  geom_point() +
                  xlab(colnames(df)[i]))
        }
        else if (grepl(colnames(df)[y], "dados.month")) {
          print(ggplot(df, aes(df[,i], dados.month)) +
                  geom_point() +
                  xlab(colnames(df)[i]))
        }
        else if (grepl(colnames(df)[y], "dados.day")) {
          print(ggplot(df, aes(df[,i], dados.day)) +
                  geom_point() +
                  xlab(colnames(df)[i]))
        }
        else if (grepl(colnames(df)[y], "dados.hour")) {
          print(ggplot(df, aes(df[,i], dados.hour)) +
                  geom_point() +
                  xlab(colnames(df)[i]))
        }
        else if (grepl(colnames(df)[y], "dados.DEWP")) {
          print(ggplot(df, aes(df[,i], dados.DEWP)) +
                  geom_point() +
                  xlab(colnames(df)[i]))
        }
        else if (grepl(colnames(df)[y], "dados.TEMP")) {
          print(ggplot(df, aes(df[,i], dados.TEMP)) +
                  geom_point() +
                  xlab(colnames(df)[i]))
        }
        else if (grepl(colnames(df)[y], "dados.PRES")) {
          print(ggplot(df, aes(df[,i], dados.PRES)) +
                  geom_point() +
                  xlab(colnames(df)[i]))
        }
        else if (grepl(colnames(df)[y], "dados.Iws")) {
          print(ggplot(df, aes(df[,i], dados.Iws)) +
                  geom_point() +
                  xlab(colnames(df)[i]))
        }
        else if (grepl(colnames(df)[y], "dados.Is")) {
          print(ggplot(df, aes(df[,i], dados.Is)) +
                  geom_point() +
                  xlab(colnames(df)[i]))
        }
        else if (grepl(colnames(df)[y], "dados.Ir")) {
          print(ggplot(df, aes(df[,i], dados.Ir)) +
                  geom_point() +
                  xlab(colnames(df)[i]))
        }
        #Se cair no else jogar direto a variável df[,y] mantendo valores corretos mas sem nomear a coluna
        else {
          print(ggplot(df, aes(df[,i], df[,y])) +
                  geom_point() +
                  xlab(colnames(df)[i]))
        }
        
      }
    }
  }
}





#Criando outro data frame para apresentar graficacamente correlação com corrplot
df2 = data.frame(dados$pm2.5, dados$year,dados$month,dados$day,dados$hour,dados$DEWP, dados$TEMP, dados$PRES, dados$Iws, dados$Is, dados$Ir )
head(df2)
m = cor(df2, use = "complete.obs") 
head(m)
head(round(m,9))
print(round(m,9))
corrplot(m, method="number")
corrplot(m, method="color")


#Aplicação do holt linear
horas_estimativa_futura = 30 * 24 # 30 dias
frequencia = 365.25 * 24 #horas por ano
inicio = c(2010,1) #2010/Jan
yt=na.fill(df$dados.pm2.5, 0)
datas= df$dados.Data
plot(yt)
plot(df$dados.Data, yt)
dados.serie = ts(yt,frequency = frequencia, start = inicio )
dados.serie
mod1=HoltWinters(dados.serie, seasonal = "additive")
mod1
plot(mod1)
fitted(mod1)
plot(fitted(mod1))
pred=predict(mod1,horas_estimativa_futura,prediction.interval = TRUE)
pred
plot(mod1,pred)

# modelo regressão múltipla
#Modelo com todo DataSet
regression.model <- lm(dados.pm2.5 ~ dados.year+dados.month+dados.day+dados.hour+dados.DEWP+dados.TEMP+dados.PRES +dados.cbwd + dados.Iws + dados.Is + dados.Ir, data=df)  
print(regression.model)
print(summary(regression.model))
configuracao <- data.frame(dados.year = 2010, dados.month = 2, dados.day = 17, dados.hour = 1, dados.DEWP = -13, dados.TEMP = -5, dados.PRES = 1025,  dados.cbwd  = "cv", dados.Iws = 1.78, dados.Is = 0, dados.Ir = 0 )
print(configuracao)
resultado <-  predict(regression.model,configuracao)
print(paste("PM 2.5 previsto é de ", resultado))

#Modelo com as 2 varáveis com maior correlação positiva com PM 2.5
regression.model <- lm(dados.pm2.5 ~ dados.day+dados.DEWP, data=df)  
print(regression.model)
print(summary(regression.model))
configuracao <- data.frame(dados.day = 17, dados.DEWP = -13 )
print(configuracao)
resultado <-  predict(regression.model,configuracao)
print(paste("PM 2.5 previsto é de ", resultado))

#Modelo de regressão com data concatenada e TEMP
regression.model <- lm(dados.pm2.5 ~ dados.Data+dados.TEMP, data=df)  
print(regression.model)
print(summary(regression.model))
configuracao <- data.frame(dados.Data = ISOdate( 2010, 10, 10), dados.TEMP = 25 )
print(configuracao)
resultado <-  predict(regression.model,configuracao)
print(paste("PM 2.5 previsto é de ", resultado))


#Modelo de regressão com Ano, Mês 
regression.model <- lm(dados.pm2.5 ~ dados.year+dados.month, data=df)  
print(regression.model)
print(summary(regression.model))
configuracao <- data.frame(dados.year = 2018, dados.month = 9 )
print(configuracao)
resultado <-  predict(regression.model,configuracao)
print(paste("PM 2.5 previsto é de ", resultado))

