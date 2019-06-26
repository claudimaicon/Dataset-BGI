library(keras)
library(ggplot2)

calculaLucro <- function(direcao, valorReal){
  SaldoInicial<-1000
  diferenca <- vector()
  diferenca2 <- vector()
  anterior <- 1
  pcompra <- 0
  for (i in 1:length(direcao)){
    if (direcao[i] == 1){
      diferenca[i] <- valorReal[i + 1] - valorReal[i]
    }
    if (direcao[i] == 1){
      pcompra <- pcompra + valorReal[i + 1] - valorReal[i]
    }
    diferenca2[i] <- valorReal[i + 1] - valorReal[i]
    print(pcompra)
  }
  print(diferenca)
  return(pcompra)
}

treino <- 1:200
validacao <- 199:248
total <- 1:248

#{# Funções
#funcao que ajusta outliers de qualquer amostra
ajustaOutliers <- function(x, na.rm = TRUE, ...)
{
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  
  for(i in 1:length(y)) 
  {
    #caso o primeiro valor seja NA procura o proximo valor nao NA e coloca
    #no lugar do NA
    if (is.na(y[1]) == TRUE)
    {
      encontrou = FALSE
      cont = 1
      posterior = NA
      #procura o primeiro numero POSTERIOR ao valor atual que nao seja NA
      while (encontrou == FALSE)
      {
        if (is.na(y[1+cont]) == TRUE)
        {
          cont <- cont + 1
        }
        else
        {
          posterior <- y[1+cont];
          encontrou <- TRUE
        }
      }
      
      y[1] <- posterior
    }
    
    #caso o ultimo valor seja NA procura o primeiro valor anterior que nao NA e coloca
    #no lugar do NA
    if (is.na(y[length(y)]) == TRUE)
    {
      encontrou <- FALSE
      cont <- 1
      anterior <- NA
      
      #procura o primeiro numero ANTERIOR ao valor atual que nao seja NA
      while (encontrou == FALSE)
      {
        if (is.na(y[length(y)-cont]) == TRUE)
        {
          cont <- cont + 1
        }
        else
        {
          anterior <- y[length(y)-cont];
          encontrou <- TRUE
        }
      }
      
      y[length(y)] <- anterior
    }
    
    
    
    if (is.na(y[i])==TRUE)
    {
      encontrou <- FALSE
      cont <- 1
      anterior <- NA
      
      #procura o primeiro numero ANTERIOR ao valor atual que nao seja NA
      while (encontrou == FALSE)
      {
        if (is.na(y[i-cont]) == TRUE)
        {
          cont <- cont + 1
        }
        else
        {
          anterior <- y[i-cont];
          encontrou <- TRUE
        }
      }
      
      encontrou = FALSE
      cont = 1
      posterior = NA
      
      #procura o primeiro numero POSTERIOR ao valor atual que nao seja NA
      while (encontrou == FALSE)
      {
        if (is.na(y[i+cont]) == TRUE)
        {
          cont <- cont + 1
        }
        else
        {
          posterior <- y[i+cont];
          encontrou <- TRUE
        }
      }
      
      #executa uma media entre o anterior e posterior valor valido na serie e insere no lugar do outlier
      y[i] <- (anterior+posterior)/2
    }
  }
  
  return(y)
}

#Coloca amostra nos intervalos proporcionais entre 0 e 1
padroniza <- function(s)
{
  retorno <- (s - min(s))/(max(s)-min(s))
  return(retorno)
}

#carrega, trata e separa apenas uma parte do dataset
carrega_subset <- function(arquivo,inicio,fim, colunas)
{
  dados <- read.table(arquivo,
                      header=TRUE,
                      sep=",",
                      colClasses=c("character", rep("numeric",colunas)),
                      na="?")
  
  #converte para datetime
  dados$DATE <- as.Date(dados$DATE, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
  
  dados <- subset(dados, dados$DATE >= inicio & dados$DATE <= fim)
  
  return(dados)
}

geraABx <- function(s)
{
  altasbaixas <- vector()
  for(i in 1:(length(s)-1))
  {
    if (s[i]<=s[i+1])
    {
      altasbaixas[i] <- 1
    }
    else
    {
      altasbaixas[i] <- 0
    }
  }
  
  return(altasbaixas)
}
#}

#{#Carregamento----

#separa a base de dados do ano de 2016
BGI_1_2016 <- carrega_subset("completa.csv","2015/12/30","2017/01/01",31)
BGI_2_2016 <- carrega_subset("completa.csv","2016/01/01","2017/01/01",31)
#}

#{#Tratamento Dados de 2016----
#Pardonização Primeira base de dados----
BGI_1_2016_ajus <- BGI_1_2016

BGI_1_2016_ajus$DATE <-padroniza(as.integer(format(BGI_1_2016$DATE,"%j")))
BGI_1_2016_ajus[2:5]<-padroniza(BGI_1_2016[2:5])
BGI_1_2016_ajus$TICKVOL<-padroniza(ajustaOutliers(BGI_1_2016$TICKVOL))
BGI_1_2016_ajus$VOL<-padroniza(ajustaOutliers(BGI_1_2016$VOL))

#Retira a primeiro valor, por não ter uma entrada
y1 <- BGI_1_2016_ajus$CLOSE[-1]

#Retira a ultima entrada, pois não tem saida prevista
BGI_1_2016_ajus <- BGI_1_2016_ajus[-length(BGI_1_2016_ajus$OPEN),]

#x1 <- cbind(BGI_1_2016_ajus$DATE, BGI_1_2016_ajus$OPEN, BGI_1_2016_ajus$HIGH, BGI_1_2016_ajus$LOW, BGI_1_2016_ajus$CLOSE, BGI_1_2016_ajus$TICKVOL, BGI_1_2016_ajus$VOL)
x1 <- data.matrix(BGI_1_2016_ajus, rownames.force = NA)
x1_treino <- array(data = x1[treino,2:6], dim = c(length(x1[treino,2]), 1, 5))
x1_validacao <- array(data = x1[validacao,2:6], dim = c(length(x1[validacao,2]), 1, 5))

x1_treino[1,,]
#Pardonização segunda base de dados----
BGI_2_2016_ajus <- BGI_2_2016

BGI_2_2016_ajus$DATE <-padroniza(as.integer(format(BGI_2_2016$DATE,"%j")))
BGI_2_2016_ajus[2:11]<-padroniza(BGI_2_2016[2:11])

y2 <- diff(BGI_2_2016$D)
#y2_ajus <- ajustaOutliers(y2)

# x2 <- cbind(BGI_2_2016_ajus$D.1,BGI_2_2016_ajus$D.2,BGI_2_2016_ajus$D.3,BGI_2_2016_ajus$D.4,BGI_2_2016_ajus$D.5,BGI_2_2016_ajus$D.6,BGI_2_2016_ajus$D.7,BGI_2_2016_ajus$D.8,BGI_2_2016_ajus$D.9)
x2_original <- cbind(BGI_2_2016$D.9,BGI_2_2016$D.8,BGI_2_2016$D.7,BGI_2_2016$D.6,BGI_2_2016$D.5,BGI_2_2016$D.4,BGI_2_2016$D.3,BGI_2_2016$D.2,BGI_2_2016$D.1)

x2 <- diff(x2_original)


head(x2_original)
head(x2)

x2_treino <- array(data = x2[treino,], dim = c(length(treino), 9, 1))
x2_validacao <- array(data = x2[validacao,], dim = c(length(validacao),9,1))

#Movimento da primeira e segunda base de dados----
y1_direcao = geraABx(y1)
y2_direcao = ifelse(y2 >= 0, 1,0)
#}

#{#Configuração do Treino/Modelo----
build_model2 <- function(time_steps,entrada) {

  model <- keras_model_sequential()
  
  model %>%
    layer_lstm(units             = 256, 
               input_shape       = c(time_steps, entrada), 
               batch_size        = batch_size)%>% 
    layer_dense(units = 1,  activation = NULL)
  
  model %>% 
    compile(loss = 'mse', optimizer = 'adam')
  
  model
}
#}


#{#Treino----
# # Treino com parada em relação ao numero de épocas----
batch_size = 25
epochs <- 200
arquivo <- "DDN_3"
model1 <- build_model2(1,5)
model1 %>% summary()
#model1 <- build_model2(1)

dim(x1_treino)
cat('Train...\n')
history1 <- model1 %>% fit(
  x1_treino, 
  y1[treino],
  batch_size = batch_size,
  epochs = epochs,
  verbose    = 1, 
  shuffle    = FALSE,
  callbacks = callback_tensorboard("logs/run3a_1")
)

scores1 <- model1 %>% evaluate(
  x1_treino, y1[treino],
  batch_size = batch_size
)

model2 <- build_model2(9,1)
model2 %>% summary()

cat('Train...\n')

history2 <- model2 %>% fit(
  x= x2_treino, y= y2[treino],
  batch_size = batch_size,
  epochs = epochs,
  verbose    = 1, 
  shuffle    = FALSE,
  callbacks = callback_tensorboard("logs/run3b_1")
)

scores2 <- model2 %>% evaluate(
  x2_treino, y2[treino],
  batch_size = batch_size
)

 plot(history1, metrics = "loss", smooth = TRUE) +
   coord_cartesian(xlim = c(0, 200), ylim = c(0, 0.15))
 
plot(history2, metrics = "loss", smooth = TRUE) +
  coord_cartesian(xlim = c(0, 200), ylim = c(0, 5))

compare_cx <- data.frame(
  data_treino = history1$metrics$loss
) %>%
  rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  gather(key = "Tipo", value = "value", -rowname)

ggplot(compare_cx, aes(x = rowname, y = value, color = Tipo)) +
  geom_line() +
  xlab("epoch") +
  ylab("loss") + ylim(c(0, 0.5))
#}

#{#Calculo Erro----
# Calculo erro primeira amostra ----

# c(loss, mae) %<-% (model %>% evaluate(x1, y1, verbose = 0))
# 
# paste0("Mean absolute error on test set: $", sprintf("%.2f", mae * 1000))

yhat1_treino <- model1 %>% predict(x1_treino, batch_size = batch_size)

yhat1_validacao <- model1 %>% predict(x1_validacao, batch_size = batch_size)

erro1 = sqrt((y1[treino]-yhat1_treino)^2)
erro1_treino = mean(erro1)

erro1 = sqrt((y1[validacao]-yhat1_validacao)^2)
erro1_validacao = mean(erro1)

yhat1_direcao <- yhat1_treino %>% geraABx()

yhat1_direcaoValidacao <- yhat1_validacao %>% geraABx()

cont = ifelse(yhat1_direcao == y1_direcao[treino[-length(treino)]], 1, 0)
pocid1_treino = sum(cont)/length(cont)

cont = ifelse(yhat1_direcaoValidacao == y1_direcao[validacao[-length(validacao)]], 1, 0)
pocid1_validacao = sum(cont)/length(cont)

yhat1_direcao[2:239]== y1_direcao[1:238]

# Calculo erro segunda amostra ----
#TREINO
yhat2_treino <- model2 %>% predict(x2_treino, batch_size=batch_size)

erro2 = sqrt((y2[treino]-yhat2_treino)^2)
erro2_treino = mean(erro2)

yhat2_direcao <- ifelse(yhat2_treino>=0, 1,0)

cont_treino = ifelse(yhat2_direcao == y2_direcao[treino], 1, 0)
pocid2_treino = sum(cont_treino)/length(cont_treino)

# yhat2_direcao == y2_direcao[treino]

#VALIDAÇÃO
yhat2_validacao <- model2 %>% predict(x2_validacao, batch_size=batch_size)

erro2 = sqrt((y2[validacao]-yhat2_validacao)^2)
erro2_validacao = mean(erro2)

yhat2_direcao_validacao <- ifelse(yhat2_validacao>=0, 1,0)

cont_validacao = ifelse(yhat2_direcao_validacao == y2_direcao[validacao], 1, 0)
pocid2_validacao = sum(cont_validacao)/length(cont_validacao)

# yhat2_direcao_validacao== y2_direcao[validacao]

#Descoverter erro----
min = min(BGI_1_2016[2:5])
max = max(BGI_1_2016[2:5])

# erro1_treino_valor = erro1_treino*(max - min)
# erro1_teste_valor = erro1_teste*(max - min)
erro1_validacao_valor = erro1_validacao*(max - min)

min = min(BGI_2_2016[2:11])
max = max(BGI_2_2016[2:11])

erro2_treino_valor = erro2_treino*(max - min)
# erro2_teste_valor = erro2_teste*(max - min)
erro2_validacao_valor = erro2_validacao*(max - min)
#}

#{#Imprimir Informações----
# print(paste("Erro treino 1: Padronizado: ",erro1_treino, ", Valor:",erro2_treino_valor))
print(paste("Erro treino 2: Padronizado: ",erro2_treino, ", Valor:",erro2_treino_valor))
# print(paste("Erro teste 1:  Padronizado: ",erro1_teste, ", Valor:",erro1_teste_valor))
# print(paste("Erro teste 2:  Padronizado: ",erro2_teste, ", Valor:",erro2_teste_valor))
print(paste("Erro Validação 1: Padronizado: ",erro1_validacao, ", Valor:",erro1_validacao_valor))
print(paste("Erro Validação 2: Padronizado: ",erro2_validacao, ", Valor:",erro2_validacao_valor))

print(paste("POCID 1: Padronizado: ",pocid1_treino))
table(y1_direcao[treino[-length(treino)]],yhat1_direcao)

print(paste("POCID 1: Padronizado: ",pocid1_validacao))
table(y1_direcao[validacao[-length(validacao)]],yhat1_direcaoValidacao)

print(paste("POCID 2 Treino ",pocid2_treino))
table(y2_direcao[treino],yhat2_direcao)

print(paste("POCID 2 Validacao ",pocid2_validacao))
table(y2_direcao[validacao],yhat2_direcao_validacao)
#}

#{#Plots dos resultados----
plot(c(yhat1_treino,yhat1_validacao), type = "l", col = "red", xlim = c(0,250), ylim = c(-1, 1))
par(new = T)
plot(y1, type = "l", col = "black", xlim = c(0,250), ylim = c(-1, 1))

teste<-cbind(y2,y2>0)

plot(y2, type = "l", col = "black", xlab="",ylab="Differença", xlim = c(0,250),  ylim = c(-3, 3), main = "Diferença de Preço de fechamento")
par(new = T)
plot(yhat2_treino, x = treino, type = "l", col = "red", xlab="", ylab="", xlim = c(0,250),  ylim = c(-3, 3))
par(new = T)
plot(yhat2_validacao, x = validacao, type = "l", col = "red", xlab="", ylab="", xlim = c(0,250),  ylim = c(-3, 3))
#}

plot(BGI_1_2016$DATE, BGI_1_2016$CLOSE,type="l",col="black",xlab="",ylab="Preço", main = "Preço de fechamento")
abline(v=c(BGI_1_2016$DATE[200]), col=c("red"), lty=c(2), lwd=c(3))
text(BGI_1_2016$DATE[170], 157, "Treino",cex=1.8, pos=1,col="Blue")
text(BGI_1_2016$DATE[230], 157, "Validação",cex=1.8, pos=1,col="blue")

# tensorboard(c("logs/run3a_1","logs/run3b_1"))

# tensorboard("logs/run_2")
 #model1 %>% save_model_hdf5("DNN3 1.h5")

 calculaLucro(ifelse(diff(BGI_1_2016$CLOSE)>=0, 1,0), BGI_1_2016$CLOSE)
 
 calculaLucro(yhat2_direcao_validacao, BGI_1_2016$CLOSE[(length(BGI_1_2016$CLOSE)-50):length(BGI_1_2016$CLOSE)]) 
 