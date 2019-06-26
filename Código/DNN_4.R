library(keras)
library(ggplot2)

treino <- 1:200
validacao <- 200:248
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

#Gera Atrasos na série
generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 1) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  if (shuffle) {
    rows <- sample(c((min_index+lookback):max_index), size = batch_size)
  } else {
    if (i + batch_size >= max_index)
      i <<- min_index + lookback
    rows <- c(i:min(i+batch_size-1, max_index))
    i <<- i + length(rows)
  }
  
  samples <- array(0, dim = c(length(rows), 
                              lookback / step,
                              dim(data)[[-1]]))
  targets <- array(0, dim = c(length(rows)))
  
  for (j in 1:length(rows)) {
    indices <- seq(rows[[j]] - lookback, rows[[j]]-1, 
                   length.out = dim(samples)[[2]])
    samples[j,,] <- data[indices,]
    targets[[j]] <- data[rows[[j]] + delay,5]
  }            
  
  list(samples, targets)
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
BGI_1_2016 <- carrega_subset("completa.csv","2015/12/15","2017/01/01",31)
BGI_2_2016 <- carrega_subset("BGI2.csv","2016/01/01","2017/01/01",10)
#}

#{#Tratamento Dados de 2016----
#Pardonização Primeira base de dados----

data <- data.matrix(BGI_1_2016)

lookback <- 10
step <- 1
delay <- 0
batch_size <- 259

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 259,
  shuffle = FALSE,
  step = step, 
  batch_size = batch_size
)

a <- array(data = unlist(train_gen[1]), dim = c(249, 10, 19))
a[1,10,5]
y1 <- c(a[1,10,5],array(data = unlist(train_gen[2]))) %>% diff()

BGI_1_2016_ajus <- BGI_1_2016

BGI_1_2016_ajus$DATE <-padroniza(as.integer(format(BGI_1_2016$DATE,"%j")))
BGI_1_2016_ajus[c(2:5, 8:12)]<-padroniza(BGI_1_2016[c(2:5, 8:12)])
BGI_1_2016_ajus$TICKVOL<-padroniza(ajustaOutliers(BGI_1_2016$TICKVOL))
BGI_1_2016_ajus$VOL<-padroniza(ajustaOutliers(BGI_1_2016$VOL))
for(i in 13:32){
  BGI_1_2016_ajus[i] <- padroniza(BGI_1_2016[i])
}

data <- data.matrix(BGI_1_2016_ajus)

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 259,
  shuffle = FALSE,
  step = step, 
  batch_size = batch_size
)

x1<-array(data = unlist(train_gen[1]), dim = c(249, 10, 19))

x1_treino <- array(data = x1[treino,,], dim = c(length(treino), 10, 19))
x1_validacao <- array(data = x1[validacao,,], dim = c(length(validacao),10,19))
x1_treino[1,,]



#Pardonização segunda base de dados----
BGI_2_2016_ajus <- BGI_2_2016

BGI_2_2016_ajus$DATE <-padroniza(as.integer(format(BGI_2_2016$DATE,"%j")))
BGI_2_2016_ajus[2:11]<-padroniza(BGI_2_2016[2:11])

y2 <- diff(BGI_2_2016$D)

x2_original <- cbind(BGI_2_2016$D.9,BGI_2_2016$D.8,BGI_2_2016$D.7,BGI_2_2016$D.6,BGI_2_2016$D.5,BGI_2_2016$D.4,BGI_2_2016$D.3,BGI_2_2016$D.2,BGI_2_2016$D.1)
x2 <- diff(x2_original)

head(x2_original)
head(x2)

x2_treino <- array(data = x2[treino,], dim = c(length(treino), 9, 1))
x2_validacao <- array(data = x2[validacao,], dim = c(length(validacao),9,1))

#Movimento da primeira e segunda base de dados----
y1_direcao = ifelse(y1 >= 0, 1,0)
y2_direcao = ifelse(y2 >= 0, 1,0)

y1 <- to_categorical(!y1_direcao)
y2 <- to_categorical(!y2_direcao)

#}

#{#Configuração do Treino/Modelo----
build_model2 <- function(time_steps,entrada) {
  
    model <- keras_model_sequential() %>%
      layer_conv_1d(filters = 16, kernel_size = 4, activation = 'relu',
                    input_shape = c(time_steps, entrada)) %>% 
      layer_conv_1d(filters = 16, kernel_size = 4, activation = 'relu') %>% 
      layer_max_pooling_1d(pool_size = 2) %>% 
      layer_dropout(rate = 0.25) %>% 
      layer_flatten() %>% 
      layer_dense(units = 128, activation = 'relu') %>% 
      layer_dropout(rate = 0.5) %>% 
      layer_dense(units = 2, activation = 'softmax')
    
    # Compile model
    model %>% compile(
      loss = loss_categorical_crossentropy,
      optimizer = optimizer_adadelta(),
      metrics = c('accuracy')
    )
  
    model
}


#{#Treino----
# Treino com parada em relação ao numero de épocas----
batch_size = 5
epochs <- 200
arquivo <- "DDN_4"
model1 <- build_model2(10,19)
model1 %>% summary()

cat('Train...\n')

history1 <- model1 %>% fit(
  x= x1_treino, y= y1[treino,],
  batch_size = batch_size,
  epochs = epochs,
  validation_data = list(x1_validacao, y1[validacao,]),
  callbacks = callback_tensorboard("logs/run4a_1")
)

 scores1 <- model1 %>% evaluate(
  x1_treino, y1[treino,],
  batch_size = batch_size
)

model2 <- build_model2(9,1)
model2 %>% summary()

cat('Train...\n')

history2 <- model2 %>% fit(
  x= x2_treino, y= y2[treino,],
  batch_size = batch_size,
  epochs = epochs,
  validation_data = list(x2_validacao, y2[validacao,]),
  callbacks = callback_tensorboard("logs/run4b_1")
)

scores2 <- model2 %>% evaluate(
  x2_treino, y2[treino,],
  batch_size = batch_size
)

scores2 <- model2 %>% evaluate(
  x2_validacao, y2[validacao,],
  batch_size = batch_size
)

plot(history1, metrics = "acc", smooth = FALSE) +
  coord_cartesian(xlim = c(0, 200))

plot(history2, metrics = ("acc"), smooth = FALSE) +
  coord_cartesian(xlim = c(0, 200))
#}

#{#Calculo Erro----
# Calculo erro primeira amostra ----
yhat1_treino <- model1 %>% predict(x1_treino, batch_size=batch_size)

erro1 = sqrt((y1[treino]-yhat1_treino)^2)
erro1_treino = mean(erro1)

yhat1_direcao <- ifelse(yhat1_treino>=0.5, 1,0)

cont_treino = ifelse(yhat1_direcao[,1] == y1[treino,1], 1, 0)
pocid1_treino = sum(cont_treino)/length(cont_treino)

yhat1_direcao == y1_direcao[treino]

#VALIDAÇÃO
yhat1_validacao <- model1 %>% predict(x1_validacao, batch_size=batch_size)
round(yhat1_validacao,2)
erro1 = sqrt((y1[validacao,1]-yhat1_validacao[,1])^2)
erro1_validacao = mean(erro1)

yhat1_direcao_validacao <- ifelse(yhat1_validacao>=0.5, 1,0)

cont_validacao = ifelse(yhat1_direcao_validacao[,1] == y1[validacao,1], 1, 0)
pocid1_validacao = sum(cont_validacao)/length(cont_validacao)

yhat1_direcao_validacao == y1_direcao[validacao]

# Calculo erro segunda amostra ----
#TREINO
yhat2_treino <- model2 %>% predict(x2_treino, batch_size=batch_size)

erro2 = sqrt((y2[treino,1]-yhat2_treino[,1])^2)

erro2_treino = mean(erro2)

yhat2_direcao <- ifelse(yhat2_treino>=0.5, 1,0)

cont_treino = ifelse(yhat2_direcao[,1] == y2[treino,1], 1, 0)
pocid2_treino = sum(cont_treino)/length(cont_treino)

yhat2_direcao == y2_direcao[treino]

#VALIDAÇÃO
yhat2_validacao <- model2 %>% predict(x2_validacao, batch_size=batch_size)

erro2 = sqrt((y2[validacao,1]-yhat2_validacao[,1])^2)
erro2_validacao = mean(erro2)

yhat2_direcao_validacao <- ifelse(yhat2_validacao>=0.4, 1,0)

cont_validacao = ifelse(yhat2_direcao_validacao[,1] == y2[validacao,1], 1, 0)
pocid2_validacao = sum(cont_validacao)/length(cont_validacao)

yhat2_direcao_validacao== y2_direcao[validacao]

#Descoverter erro----
min = min(BGI_1_2016[2:5])
max = max(BGI_1_2016[2:5])

# erro1_treino_valor = erro1_treino*(max - min)
# erro1_teste_valor = erro1_teste*(max - min)
# erro1_validacao_valor = erro1_validacao*(max - min)

# min = min(BGI_2_2016[2:11])
# max = max(BGI_2_2016[2:11])

erro2_treino_valor = erro2_treino*(max - min)
# erro2_teste_valor = erro2_teste*(max - min)
erro2_validacao_valor = erro2_validacao*(max - min)
#}

#{#Imprimir Informações----
# print(paste("Erro treino 1: Padronizado: ",erro1_treino, ", Valor:",erro2_treino_valor))
print(paste("Erro treino 2: Padronizado: ",erro2_treino, ", Valor:",erro2_treino_valor))
# print(paste("Erro teste 1:  Padronizado: ",erro1_teste, ", Valor:",erro1_teste_valor))
# print(paste("Erro teste 2:  Padronizado: ",erro2_teste, ", Valor:",erro2_teste_valor))
# print(paste("Erro Validação 1: Padronizado: ",erro1_validacao, ", Valor:",erro1_validacao_valor))
# print(paste("Erro Validação 2: Padronizado: ",erro2_validacao, ", Valor:",erro2_validacao_valor))

# print(paste("POCID 1: Padronizado: ",pocid1_validacao))
# table(y1_direcao[treino[-length(treino)]],yhat1_direcao)

print(paste("POCID 1 Treino ",pocid1_treino))
table(y1[treino,1],yhat1_direcao[,1])

print(paste("POCID 1 Validacao ",pocid1_validacao))
table(y1[validacao],yhat1_direcao_validacao[,1])

print(paste("POCID 2 Treino ",pocid2_treino))
table(y2[treino,1],yhat2_direcao[,1])

print(paste("POCID 2 Validacao ",pocid2_validacao))
table(y2_direcao[validacao],yhat2_direcao_validacao[,1])
#}

#{#Plots dos resultados----
plot(yhat1, type = "l", col = "red", xlim = c(0,250), ylim = c(-1, 1))
par(new = T)
plot(y1, type = "l", col = "black", xlim = c(0,250), ylim = c(-1, 1))

plot(y2[,1], type = "l", col = "black", xlim = c(0,250),  ylim = c(-3, 3))
par(new = T)
plot(yhat2_treino[,1], x = treino, type = "l", col = "red", xlim = c(0,250),  ylim = c(-3, 3))
par(new = T)
plot(yhat2_validacao[,1], x = validacao, type = "l", col = "red", xlim = c(0,250),  ylim = c(-3, 3))
#}


# tensorboard("logs/run_1")
# tensorboard("logs/run_2")
# save_model_hdf5(model2, "dnn 4 media", TRUE)
# save_model_weights_hdf5(model2, "dnn 4 algum", overwrite = TRUE)
# salvo<- build_model2(9,1)
# load_model_weights_hdf5(salvo, "dnn 4 algum")

#loss_mean_squared_error(y1, yhat1)
plot(yhat1_treino)

