#Exibir a pasta de trabalho do R
getwd()

#Alterar a pasta de trabalho
setwd("/Users/leandroescobar/Documents/AnaliseDadosR")
getwd()

##########################################################
#                        VARIÁVEIS                       #
##########################################################
minhavar = "An�lise de dados"
MINHAVAR = "Big Data"
minhavar
MINHAVAR

#N�o requer a declaracao do tipo de dados
n = 2L
m = 2.5

class(n)
class(m)

w = n*m
w
w = m%%n
w
q = m%/%n
q

#Valores inteiros
i = 2L
i
class(i)


##########################################################
#                 FUN��ES DE USUÁRIO                     #
##########################################################
# Estrutura de uma func�o
# nome_funcao = function(arg_1, arg_2, ...) {
#   Instrucoes da funcao 
#}

soma = function(a,b){
  a+b
}

#Invocar a funcao
x=soma(10,5)
print(x)
cat(x)

##########################################################
#                     DESVIOS - IF                       #
##########################################################
x = c("Segredo","Verdade","Mentira")

if("Verdade" %in% x){
  print("Termo Verdade foi encontrado")
}else {
  print("Termo Verdad não foi encontrado")
}


##########################################################
#                 LA�OS DE REPETICAO                     #
##########################################################
v <- c("Segredo","Verdade","Mentira")
#   FOR   #
for(n in v){
  print(n)
}

#   REPEAT   #
n=1
repeat{
  print("Oi!")
  n = n+1
  if(n>=10){
    break()
  }
}


#   WHILE  #
n=10
while(n>=1){
  print(n)
  n = n-1
}

##########################################################
#   VETORES - MULTIPLOS VALORES DO MESMO TIPO            #
##########################################################

#Vetores são criados com a função c()
v = c(1,2,3)
v
for(n in v){
  print(n)
}
frutas = c("laranja", "macã", "banana")
frutas


#operacoes básicas com vetores
#criar um vetor
v = c(1:10) #cria o vertor v com os valores obtidos do intervalor 1 a 10
v

v = c(2,1,2,3,1,2,3,4,1,5,5,3,2,3) #cria o vetor com valores determinados
#imprimir todo o vetor
v

#imprimir um elemento do vetor
v[2]
v[8]

#retornar o tamanho do vetor
length(v)

#retornar a lista de valores únicos
uniquev = unique(v) #unique retorna a lista de dados únicos em um data frame
uniquev

#retornar o menor valor no vetor
which.min(v) #retorna a posicao da primeira ocorrência do maior valor
v[which.min(v)]#retorna o maoir valor do vetor

#retornar o maior valor no vetor
which.max(v) #retorna a posicao da primeira ocorrência do maior valor
v[which.max(v)]#retorna o maoir valor do vetor


###### OPERACOES COM VETORES
i = c(2,5.5,6)
j = c(8,3,4)

#Operadores aritimeticos
cat("Soma")
print(i+j)
cat("Diferença")
print(j-i)
cat("Produto")
print(i*j)
cat("Divisão")
print(i/j)
cat("Resto")
print(i%%j)
cat("Quociente")
print(i%/%j)

#Operadores relacionais
print(i<j)
print(i==j)
print(i<=j)

#===================================
#encontrar valores dentro do vetor
x=c(2)
match(v,x)#match indica a posição em v na qual o elemento em x foi encontrado

x=c(1,3)#marcará as posicões onde 1 e 3 forem encontrados
match(v,x)#atenção, a indicação do match se dá pela posiçao do valor no vetor x

#contar a frequencia de elementos dentro do vetor
v = c("Maria","Pedro", "ana","Maria","Pedro", "Pedro")
uniquev = unique(v)
#uniquev

#match(v, uniquev)
#tabulate(match(v, uniquev))#cria um vetor com a contagem dos elementos (repetições)

##retorna a posição do elemento com maior número de repetições
#which.max(tabulate(match(v, uniquev)))
uniquev[which.max(tabulate(match(v, uniquev)))]#retorna a moda
paste("xxxxx", "eeee")#Concatena conte�dos
#===================================



#MEDIDAS DE CONCENTRAÇÃO E DISPERSÃO
v <- c(2,1,2,3,1,2,3,4,1,5,5,3,2,3,NA)
media = mean(v,na.rm = TRUE)
mediana = median(v,na.rm = TRUE)
desviopadrao = sd(v,na.rm = TRUE)
variancia = var(v,na.rm = TRUE)

media
mediana
desviopadrao
variancia

#CRIAR UM FUNÇÃO PARA ENCONTRAR A MODA EM UM VETOR QUALQUER
getmoda=function(v){
  uniquev = unique(v)
  uniquev[which.max(tabulate(match(v, uniquev)))]
}
#ATIVIDADE PRÁTICA
#Criar vetor com nomes ("Joao","Maria","Ana", "Joao","Ana", "Joao")
#retornar a moda

nomes = c("Joao","Maria","Ana", "Joao","Ana", "Joao")
getmoda(nomes)



  

###############################################################################
#   MATRIZES - ELEMENTOS ARRANJADOS EM UMA ESTRURUTA BIDIMENSIONAL            #
###############################################################################
# matrix(data, nrow, ncol, byrow, dimnames)
# data - vetor com os dados que serão alocados na matriz
# nrow - número de linhas da matriz
# ncol - número de colunas da matriz
# byrow - TRUE | FALSE - indica se os dados devem ser distribuídos por linha (TRUE) ou coluna (FALSE)
# dimnames - lista com os rótulos para as linhas ou colunas

#criar matrizes de dados
dataml = matrix (1:9, byrow=TRUE, ncol=3)
dataml
#1:9 - atalho para a sequencia (1,2,3,4,5,6,7,8,9)
#byrow = TRUE - indica que a matriz será preenchida pelas linhas. Alterar para FALSE muda a direçao do preenchimento
#3 - indica que a matriz terá 3 linhas
datamc = matrix(1:9, byrow = FALSE, 3)
datamc
class(datamc)
head(datamc)
ncol(datamc)

#Criar rótulos para linhas e colunas
rownames = c("row1", "row2", "row3", "row4")
colnames = c("col1", "col2", "col3")
P = matrix(c(3:14), nrow = 4, byrow = TRUE, dimnames = list(rownames, colnames))
P

q = matrix(c(3:14), nrow = 4, byrow = TRUE)
colnames(q)= c("x1","x2","y")
q

s = matrix(c(3:14), nrow=4,byrow = TRUE)
rownames(s) = c("l1","l2","l3","l4")
s

w = matrix(c(3:14), nrow=4,byrow = TRUE)
rownames(w) = c("l1","l2","l3","l4")
colnames(w) = c("x1","x2","y")
w


###############################################################################
#                              DATA FRAMES                                    #
###############################################################################
# Criar o data frame.
emp.dados = data.frame(
  emp_id = c (1:5), 
  emp_nome = c("Ricardo","Daniele","Michelle","Reinaldo","Gabriela"),
  salario = c(6233,5152,6110,7290,8432), 
  data_inicio = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11","2015-03-27")),
  stringsAsFactors = FALSE
)
# mostra o data frame.			
print(emp.dados)

# motra a estrutura do data frame
str(emp.dados)

# mostra o sumário do data frame (estatisticas dos dados)
summary(emp.dados)
emp.salariomedio = mean(emp.dados$salario)
emp.salariomedio

emp.sd = sd(emp.dados$salario)
emp.sd
emp.salario.quartis = quantile(emp.dados$salario)
emp.salario.quartis

# Extrair colunas específicas do data frame
result <- data.frame(emp.dados$emp_nome,emp.dados$salario)
print(result)

# Extrair as duas primeiras linhas do data frame
result <- emp.dados[1:2,c("salario","emp_nome")]
print(result)
class(result)

# Extrair determinadas primeiras linhas do data frame
result <- emp.dados[c(3,5),c(3)]
print(result)
class(result)

##########################################
#        DATA FRAME - reshaping          #
##########################################

# Adicionar coluna ao data frame
# basta atribuir valores a uma coluna inexistente
emp.dados$depto = c("TI","Comercial","TI","RH","Financeiro")
emp.dados

# Adicionar as linhas
# Atenção: as linhas devem ter a mesma estrutura do data frame destino
# Cria o segundo data frame
emp.novosdados = 	data.frame(
  emp_id = c (6:8), 
  emp_nome = c("Roberta","Tatiana","Carlos"),
  salario = c(5780,7225,6328), 
  data_inicio = as.Date(c("2013-05-21","2013-07-30","2014-06-17")),
  depto = c("TI","Marketing","Financeiro"),
  stringsAsFactors = FALSE
)

# Conecta os dois data frames.
emp.finaldados = rbind(emp.dados,emp.novosdados)
print(emp.finaldados)


###############################################################################
#                 DATA FRAMES - CRIAR A PARTIR DE VETORES                     #
###############################################################################
#Criar três vetores
cidade <- c("Salvador","Fortaleza","Ceres","Campo Grande")
estado <- c("BA","CE","GO","MT")
cep <- c("33602-200","98104-223","66161-300","80294-340")

# Combinar os três vetores em um data frame
enderecos <- cbind(cidade,estado,cep)
class(enderecos)



###############################################################
###############################################################
###############################################################
###############################################################


###############################################################################
#                              ABRIR ARQUIVOS  CSV                            #
###############################################################################
#4 ===========================================
#abrir um arquivo
dados <- read.csv("pasageiros.csv", TRUE, ";")
class(dados)#imprime o tipo da variável dados
ncol(dados) #imprime o número de colunas do data.frame
head(dados) #exibe as primeiras linhas do data.frame
names(dados)#imprime os rótulos das colunas do data frame

yt_dados = dados["yt"] #copia a coluna com rótulo "yt"para o novo data frame
names(yt_dados)#imprime o rótulo do novo data frame
yt_dados

dados_2 = dados[1:2]
names(dados_2)
dados_2


###############################################################################
#                                 ESTATÍSTICAS                                #
###############################################################################

# ATENÇÃO
# quando o data frame possui rótulos, é necessário indicar o rótulo,
# evitando que o dado do rótulo seja utilzado no cálculo

# MÉDIA
yt.media = mean(yt_dados$yt, na.rm = TRUE)
yt.media

# MEDIANA
yt.mediana = median(yt_dados$yt, na.rm = TRUE)
yt.mediana

# QUARTIS
quantile(yt_dados$yt, na.rm = TRUE)



# EXERCÍCIOS
# UTILIZE O ARQUIVO OldFaithFullGeyser_YellowstoneNationalPark.CSV
# Determine e tempo médio, a mediana, quartis e desvio padrão das duracoes das erupções e dos intervalos do geyser
# Atributos
# Eruptions = duração em minutos de cada erupção
# Waiting = intervalo entre as erupçoes, em minutos


###############################################################################
#                                 GRÁFICOS                                 #
###############################################################################
# Setores (Pizza)
# Dados para o grafico.
x <- c(21, 62, 10, 53)
labels <- c("London", "New York", "Singapore", "Mumbai")
dev.off()
pie(x,labels)

#Nome do arquivo para gravar em disco.
#png(file = "city.png")

# Plotar o gráfico.


# Plotar o gráfico com título, rótulos de dados e esquema de cores.
dev.off(dev.cur())
pie(x, labels, main = "Cidades", col = rainbow(length(x)))


# Plotar o gráfico com título, rótulos de dados, esquema de cores e legenda.
pie(x, labels = piepercent, main = "City pie chart",col = rainbow(length(x)))
legend("topright", c("London","New York","Singapore","Mumbai"), cex = 0.8,
       fill = rainbow(length(x)))



###############################################################################
#                              SÉRIES TEMPORAIS
###############################################################################

# As observações são organizadas no formato de vetor.
rainfall <- c(799,1174.8,865.1,1334.6,635.4,
              918.5,685.5,998.6,784.2,985,882.8,1071,1233,322,544,675)

# Converter para um objeto time series
rainfall.timeseries=ts(rainfall,start = c(2012,1),frequency = 4)
#start determina a data da primeira obervação
#frequency determina o tipo de período
#frequency = 12 coloca os pontos de dados a cada mês do ano
#frequency = 4 coloca os pontos de dados a cada trimestre do ano.
#frequency = 6 coloca os pontos de dados a cada  10 minutes, hora a hora.
#frequency = 24*6 coloca os pontos de dados a cada 10 minutos do dia.


# Imprime a série.
print(rainfall.timeseries)
plot(rainfall.timeseries)

# Nome para o arquivo do gráfico.
png(file = "rainfall.png")

# Imprime o gráfico da série.
plot(rainfall.timeseries)


###############################################################################
#                                 HOLT LINEAR                                 #
###############################################################################
x = 1:24
x
y=5*x+rnorm(24,2,8) # simulaçao de dados
plot(y)
dados.serie = ts(y,frequency = 4,start = c(2010,1))
dados.serie
plot(dados.serie)
mod1=HoltWinters(dados.serie,seasonal = "additive")
plot(mod1)
fitted(mod1)
plot(fitted(mod1))
pred=predict(mod1,4,prediction.interval = TRUE)
#pred
plot(mod1,pred)

#com os dados de incidentes
incidentes = c(739,742,738,747,751,752,756,768,766)
plot.ts(incidentes)
incidentes.serie = ts(incidentes,start = c(2018,1),frequency = 4)
incidentes.serie
plot(incidentes.serie)

mod2 = HoltWinters(incidentes.serie, alpha = 0.2,
                   beta = 0.5, gamma = 0.3,  
                   seasonal = "additive", start.periods = 2)
print(mod1)
class(mod1)
fitted(mod2)
plot(mod2)



###############################################################################
#                                 CORRELAÇÃO                                 #
###############################################################################

x <- c(3,3,6)
y <- c(2,3,4)
cor(x,y)

#ATIVIDADE
#Utilize o arquivo AIRQUALITY para estabelecer a correlação entre as variáveis

###############################################################################
#                                 REGRESS�O                                #
###############################################################################

altura <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
peso <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

# Utilizar lm().
regression.model <- lm(peso~altura)

print(regression.model)
print(summary(regression.model))

#Prever o valor do peso, dada a altura da pessoa
a <- data.frame(altura = 170)
resultado <-  predict(regression.model,a)
print(resultado)

###################################################
#             ATIVIDADE AVALIATIVA 
#                   EM EQUIPE
#               ENTREGA EM 09/09/2018
###################################################

#Com a base pm25 (em materiais), elabore um relat�rio para:
#a) an�lise descritiva (m�dias, desvios, quartis e o que for
# relevante) para as variáveis do arquivo

#b) Elabore uma an�lise de correla��o entre as vari�veis

#c) Crie um modelo preditivo, com regress�o m�ltipla, para
#prever a quantidade futura de particulas pm25 suspensas no ar.


# EXERC�CIOS REMANESCENTES PARA ENTREGA
# PARTE1 - SLIDE 32
# PARTE2 - SLIDE 12
# PARTE3 - SLIDE 32





