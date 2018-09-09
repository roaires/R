
##########################################################
# An�lise explorat�ria de dados no Dataset PM2.5         #
# by Rodrigo Aires                                       #
# https://www.linkedin.com/in/rodrigo-aires-2952ab57/    #
##########################################################



#load packages
#library(lubridate)
#library(dplyr)
library(ggplot2)


#Defini�o da pasta de trabalho
setwd("C:/Projetos/r")
getwd()



dados <- read.csv("pm25.csv", TRUE, ",")
class(dados)
ncol(dados) 
head(dados) 
names(dados)


#Nomenclaturas dos r�tulos
#No - Id da linha
#year, month, day e hour  - data e leitura
#pm2.5 - indicador pm2.5
#DEWP - Ponto de orv�lho
#TEMP - Temperat�ra
#PRES - Press�o
#cbwd - Dire��o combinada do vento
#Iws - Veloricade do vento acumulada
#Is - Horas acumuladas de neve
#Ir - Horas acumuladas de chuva

#Inclus�o do campo data
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
#dados.DataCompleta = dados["DataCompleta"]



#Criando outro data frame para mudar a ordem de algumas colunas
df = data.frame(dados$day,dados$Data, dados$pm2.5, dados$year,dados$month,dados$hour,dados$DEWP, dados$TEMP, dados$PRES, dados$cbwd, dados$Iws, dados$Is, dados$Ir )

#Informa��es do data frame
class(df)
ncol(df) 
head(df) 
names(df)

#Sum�rio Geral
summary(df)

print("Informa��es estat�sticas por vari�vel: ")
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
    
    print(paste(" ", "M�nimo: ",minimo))  
    print(paste(" ", "M�ximo: ",maximo))  
    print(paste(" ", "M�dia: ",media))  
    print(paste(" ", "Mediana: ",mediana))  
    print(paste(" ", "Desvio Padr�o: ",desviopadrao))  
    print(paste(" ", "Vari�ncia: ",variancia))  
    print(paste(" ", "Quartis: ")) 
    print(quartis)
  }
}


#Conhecendo os dados - Visualiza��o
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


#Correla��o Pm 2.5 com demais vari�veis importantes
for(i in which(colnames(df) == "dados.year"):which(colnames(df) == "dados.Ir")) {
  if(!is.factor(df[,i])) {
    print(paste("pm2.5 & ", colnames(df)[i], sep = ""))
    print(cor(df$dados.pm2.5, dados[, i], use = "complete.obs"))
    
    print(ggplot(df, aes(df[,i], dados.pm2.5)) +
            geom_point() +
            xlab(colnames(df)[i]))
  }
  else {
    print(paste("N�o gerou COR entre pm2.5 & ", colnames(df)[i], sep = ""))
    print(ggplot(df, aes(df[,i], dados.pm2.5)) +
            geom_boxplot() +
            xlab(colnames(df)[i]))
  }
}


#Correla��o entre demais v�ri�veis (sem considerar pm 2.5)
for(y in which(colnames(df) == "dados.year"):which(colnames(df) == "dados.Ir")) {
  for(i in which(colnames(df) == "dados.year"):which(colnames(df) == "dados.Ir")) {
    #N�o realizar verifica��o para mesma coluna em x e Y
    #Removido dados.cbwd por n�o ser num�rico 
    if ((!grepl(colnames(df)[i], colnames(df)[y])) &  (!grepl(colnames(df)[i], "dados.cbwd")  &  (!grepl(colnames(df)[y], "dados.cbwd")))) {
      if(!is.factor(df[,i])) {
        print(paste(colnames(df)[y]," & ", colnames(df)[i], sep = ""))
        print(cor(dados[, y], dados[, i], use = "complete.obs"))
                
        #Criado uma codi��o para cada Y visando nomear corretamente no eixo do gr�fico
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
        #Se cair no else jogar direto a vari�vel df[,y] mantendo valores corretos mas sem nomear a coluna
        else {
          print(ggplot(df, aes(df[,i], df[,y])) +
                  geom_point() +
                  xlab(colnames(df)[i]))
        }
        
      }
      else {
        print(paste("N�o gerou COR entre ", colnames(df)[y], " & ", colnames(df)[i], sep = ""))
        print(ggplot(df, aes(df[,i], df[,y])) +
                geom_boxplot() +
                xlab(colnames(df)[i]))
      }
    }
  }
}



# modelo preditivo com regress�o m�ltipla
regression.model <- lm(dados.pm2.5 ~ dados.year + dados.month + dados.day + dados.hour + dados.DEWP + dados.TEMP + dados.PRES, dados.cbwd, dados.Iws + dados.Is, dados.Ir, data=df)  
print(regression.model)
print(summary(regression.model))
configuracao <- data.frame(dados.year = 2011, dados.month = 1, dados.day = 3, dados.hour = 19, dados.DEWP = -9, dados.TEMP = -9, dados.PRES = 1022,  dados.cbwd  = "cv", dados.Iws = 1.78, dados.Is = 25, dados.Ir = 0 )
print(configuracao)
resultado <-  predict(regression.model,configuracao)
print(paste("PM 2.5 previsto � de ", resultado))

