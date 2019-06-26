library(keras)
library(ggplot2)
library(tibble)
library(dplyr)
library(tidyr)

treino <- 1:150
teste <- 151:198
validacao <- 199:249

{# Funções
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
}

{#Carregamento----
  
  #separa a base de dados do ano de 2016
  BGI_1_2016 <- carrega_subset("Tecnica.csv","2015/12/15","2017/01/01",18)
  BGI_2_2016 <- carrega_subset("completa.csv","2016/01/01","2017/01/01",10)
}

{#Tratamento Dados de 2016----
  #Pardonização Primeira base de dados----
  BGI_1_2016_ajus <- BGI_1_2016
  
  BGI_1_2016_ajus$DATE <-padroniza(as.integer(format(BGI_1_2016$DATE,"%j")))
  BGI_1_2016_ajus[c(2:5, 8:12)]<-padroniza(BGI_1_2016[c(2:5, 8:12)])
  # BGI_1_2016_ajus[2:5]<-padroniza(BGI_1_2016[2:5])
  BGI_1_2016_ajus$TICKVOL<-scale(ajustaOutliers(BGI_1_2016$TICKVOL))
  BGI_1_2016_ajus$VOL<-scale(ajustaOutliers(BGI_1_2016$VOL))
  for(i in 13:19){
    BGI_1_2016_ajus[i] <- scale(BGI_1_2016[i])
  }
  
  # #Retira a primeiro valor, por não ter uma entrada
  # y1 <- BGI_1_2016_ajus$CLOSE[-1]
  # 
  # #Retira a ultima entrada, pois não tem saida prevista
  # BGI_1_2016_ajus <- BGI_1_2016_ajus[-length(BGI_1_2016_ajus$OPEN),]
  # 
  # #x1 <- cbind(BGI_1_2016_ajus$DATE, BGI_1_2016_ajus$OPEN, BGI_1_2016_ajus$HIGH, BGI_1_2016_ajus$LOW, BGI_1_2016_ajus$CLOSE, BGI_1_2016_ajus$TICKVOL, BGI_1_2016_ajus$VOL)
  # x1 <- data.matrix(BGI_1_2016_ajus, rownames.force = NA)
  # x1_test_arr <- array(data = x1[1:240,1:19], dim = c(length(x1[1:240,2]), 1, 19))
  # x1_test_arr[1,,]
  #Pardonização segunda base de dados----
  data <- data.matrix(BGI_1_2016_ajus)
  
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
  train <- train_gen[1]
  y1 <- array(data = unlist(train_gen[2]))
  x1<-array(data = unlist(train_gen[1]), dim = c(249, 10, 19))
  
  x1_test_arr <- array(data = x1[1:240,,], dim = c(length(1:240), 10, 19))
  x1_test_arr[1,,]
  
  x1_test_arr[,,5] %>% head(6)
  y1 %>% head(5)
  x1_test_arr[,1,]
  x1_test_arr[1,,]
  
  #tr[,,5] %>% head(6)
  #y %>% head(5)
  #tr[,1,]
  #tr[1,,]
  
  BGI_2_2016_ajus <- BGI_2_2016
  
  BGI_2_2016_ajus$DATE <-padroniza(as.integer(format(BGI_2_2016$DATE,"%j")))
  BGI_2_2016_ajus[2:11]<-padroniza(BGI_2_2016[2:11])
  
  y2 <- BGI_2_2016_ajus$D
  
  x2 <- cbind(BGI_2_2016_ajus$D.1,BGI_2_2016_ajus$D.2,BGI_2_2016_ajus$D.3,BGI_2_2016_ajus$D.4,BGI_2_2016_ajus$D.5,BGI_2_2016_ajus$D.6,BGI_2_2016_ajus$D.7,BGI_2_2016_ajus$D.8,BGI_2_2016_ajus$D.9)
  x2_test_arr <- array(data = x2[1:240,], dim = c(length(x2[1:240,1]), 9, 1))
  #Movimento da primeira e segunda base de dados----
  y1_direcao = geraABx(y1)
  y2_direcao = geraABx(y2)
}

{#Configuração do Treino/Modelo----
  build_model2 <- function(time_steps,entrada) {
    
    # fit1 = Sequential ()
    # fit1.add (LSTM (  1000 , activation = 'tanh', inner_activation = 'hard_sigmoid' , input_shape =(len(cols), 1) ))
    # fit1.add(Dropout(0.2))
    # fit1.add (Dense (output_dim =1, activation = 'linear'))
    #
    # fit1.compile (loss ="mean_squared_error" , optimizer = "adam")   
    # fit1.fit (x_train, y_train, batch_size =16, nb_epoch =25, shuffle = False)
    #http://intelligentonlinetools.com/blog/2018/01/19/machine-learning-stock-prediction-lstm-keras/
    
    model <- keras_model_sequential()
    
    model %>%
      layer_lstm(units             = 128, 
                 input_shape       = c(time_steps, entrada), 
                 batch_size        = batch_size)%>% 
      layer_dense(units = 1)
    
    model %>% 
      compile(loss = 'mse', optimizer = 'adam')
    
    model
  }
  
  # model1 <- build_model2(dim(x1)[2])
  # model1 %>% summary()
  # 
  
}


# {#Treino----
  # # Treino com parada em relação ao numero de épocas----
  batch_size = 30
  epochs <- 1000
  arquivo <- "DDN_2 Batch = 30"
  model1 <- build_model2(10,19)
  model1 %>% summary()
  #model1 <- build_model2(1)
  
  dim(x1_test_arr)
  cat('Train...\n')
  history1 <- model1 %>% fit(
    x1_test_arr, 
    y1[1:240],
    batch_size = batch_size,
    epochs = epochs,
    validation_split = 2/16,
    verbose = 0,
    shuffle    = TRUE,
    callbacks = callback_tensorboard("logs/run2a_1")
  )
  
  scores <- model1 %>% evaluate(
    x1_test_arr, y1[1:240],
    batch_size = batch_size,
    validation_split = 2/8
  )
  
  model2 <- build_model2(9,1)
  model2 %>% summary()
  
  cat('Train...\n')

  history <- model2 %>% fit(
    x= x2_test_arr, y= y2[1:240],
    batch_size = batch_size,
    epochs = epochs,
    #validation_data = list(x2[teste], y2[teste]),
    verbose    = 0, 
    shuffle    = FALSE,
    callbacks = callback_tensorboard("logs/run2b_1")
  )
  
  scores <- model2 %>% evaluate(
    x2_test_arr, y2[1:180],
    batch_size = batch_size
  )
  stop()
  
   plot(history1, smooth = TRUE) +
     coord_cartesian(ylim = c(0, 0.010))
   
   compare_cx <- data.frame(
     data_treino = history1$metrics$loss,
     data_validacao = history1$metrics$val_loss
   ) %>%
     rownames_to_column() %>%
     mutate(rowname = as.integer(rowname)) %>%
     gather(key = "Tipo", value = "value", -rowname)
   
   ggplot(compare_cx, aes(x = rowname, y = value, color = Tipo)) +
     geom_line() +
     xlab("epoch") +
     ylab("loss") + ylim(c(0, 0.05))
  # 
  # plot(history2, metrics = "mean_absolute_error", smooth = FALSE) +
  #   coord_cartesian(xlim = c(0, 50), ylim = c(0, 0.15))
# }

{#Calculo Erro----
  # Calculo erro primeira amostra ----
  
  # c(loss, mae) %<-% (model %>% evaluate(x1, y1, verbose = 0))
  # 
  # paste0("Mean absolute error on test set: $", sprintf("%.2f", mae * 1000))
  
  yhat1 <- model1 %>% predict(x1_test_arr, batch_size = batch_size)
  
  erro1 = sqrt((y1[1:240]-yhat1[1:240])^2)
  erro1_validacao = mean(erro1)
  
  yhat1_direcao <- yhat1 %>% geraABx()
  
  cont = ifelse(yhat1_direcao == y1_direcao[1:239], 1, 0)
  pocid1_validacao = sum(cont)/length(cont)
  
  yhat1_direcao[2:239]== y1_direcao[1:238]
  
  # Calculo erro segunda amostra ----
  
  yhat2 <- model2 %>% predict(x2_test_arr, batch_size=batch_size)
  
  erro2 = sqrt((y2[1:240]-yhat2)^2)
  erro2_validacao = mean(erro2)
  
  yhat2_direcao <- yhat2 %>% geraABx()
  
  cont = ifelse(yhat2_direcao == y2_direcao[1:239], 1, 0)
  pocid2_validacao = sum(cont)/length(cont)
  
  yhat2_direcao[1:239]== y2_direcao[1:239]
  
  #Descoverter erro----
  min = min(BGI_1_2016[2:5])
  max = max(BGI_1_2016[2:5])
  
  # erro1_treino_valor = erro1_treino*(max - min)
  # erro1_teste_valor = erro1_teste*(max - min)
  erro1_validacao_valor = erro1_validacao*(max - min)
  
  min = min(BGI_2_2016[2:11])
  max = max(BGI_2_2016[2:11])
  # erro2_treino_valor = erro2_treino*(max - min)
  # erro2_teste_valor = erro2_teste*(max - min)
  erro2_validacao_valor = erro2_validacao*(max - min)
}

#Imprimir Informações----
  # print(paste("Erro treino 1: Padronizado: ",erro1_treino, ", Valor:",erro2_treino_valor))
  # print(paste("Erro treino 2: Padronizado: ",erro2_treino, ", Valor:",erro2_treino_valor))
  # print(paste("Erro teste 1:  Padronizado: ",erro1_teste, ", Valor:",erro1_teste_valor))
  # print(paste("Erro teste 2:  Padronizado: ",erro2_teste, ", Valor:",erro2_teste_valor))
  print(paste("Erro Validação 1: Padronizado: ",erro1_validacao, ", Valor:",erro1_validacao_valor))
  print(paste("Erro Validação 2: Padronizado: ",erro2_validacao, ", Valor:",erro2_validacao_valor))
  
  print(paste("POCID 1: Validação: ",pocid1_validacao))
  table(y1_direcao[191:240],yhat1_direcao[190:239])
  
  print(paste("POCID 1: Teste: ",pocid1_validacao))
  table(y1_direcao[1:239],yhat1_direcao[1:239])

{
  print(paste("POCID 2: Padronizado: ",pocid2_validacao))
  table(y2_direcao[2:240],yhat2_direcao)
}

{#Plots dos resultados----
  plot(yhat1[1:240], type = "l", col = "red")
  par(new = T)
  plot(y1[1:240], type = "l", col = "black")
  
  plot(yhat2, type = "l", col = "red", ylim = c(0, 1))
  par(new = T)
  plot(y2[1:240], type = "l", col = "black", ylim = c(0, 1))
}


 #tensorboard("logs")
 #tensorboard("logs/run_e")
 #find DIRECTORY_PATH | grep tfevents
  
  #model1 %>% save_model_hdf5("my_model3.h5")
  #If you only wanted to save the weights, you could replace that last line by
  
  #model1 %>% save_model_weights_hdf5("my_model_weights3.h5")
  #Now recreate the model from that file:
    
  #new_model <- load_model_hdf5("my_model.h5")
  #new_model %>% summary()

   compare_cx <- data.frame(
     BGI_1_2016$CLOSE
   ) %>%
     rownames_to_column() %>%
     mutate(rowname = as.integer(rowname)) %>%
     gather(key = "Tipo", value = "value", -rowname)
   
   ggplot(BGI_1_2016$CLOSE, type = 'l')
   
     geom_line() +
     xlab("epoch") +
     ylab("loss") + ylim(c(0, 0.05))
   
     
     plot(  data.frame(as.numeric(BGI_1_2016$CLOSE), 1:259), aes(1:259) ) + 
       geom_line( ) +
       labs( title = "S&P 500 (ggplot2::ggplot)")
     
    
     as.numeric(format(index(sp500.monthly)[1]))
     
     