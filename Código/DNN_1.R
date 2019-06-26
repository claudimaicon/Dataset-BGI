library(keras)
library(ggplot2)
library(RSNNS)
library(quantmod)

treino <- 1:150
teste <- 151:198
validacao <- 199:249

{# Funções ----
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
    dados$DATE <- as.Date(dados$DATE, "%Y-%m-%d")
    
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
  BGI_1_2016 <- carrega_subset("completa.csv","2015/12/30","2017/01/01",31)
  BGI_2_2016 <- carrega_subset("completa.csv","2016/01/01","2017/01/01",10)
}

{#Tratamento Dados de 2016----
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
  #Pardonização segunda base de dados----
  BGI_2_2016_ajus <- BGI_2_2016
  
  BGI_2_2016_ajus$DATE <-padroniza(as.integer(format(BGI_2_2016$DATE,"%j")))
  BGI_2_2016_ajus[2:11]<-padroniza(BGI_2_2016[2:11])
  
  y2 <- BGI_2_2016_ajus$D
  
  x2 <- cbind(BGI_1_2016_ajus$DATE,BGI_2_2016_ajus$D.1,BGI_2_2016_ajus$D.2,BGI_2_2016_ajus$D.3,BGI_2_2016_ajus$D.4,BGI_2_2016_ajus$D.5,BGI_2_2016_ajus$D.6,BGI_2_2016_ajus$D.7,BGI_2_2016_ajus$D.8,BGI_2_2016_ajus$D.9)
  
  #Movimento da primeira e segunda base de dados----
  y1_direcao = geraABx(y1)
  y2_direcao = geraABx(y2)
}

{#Configuração do Treino/Modelo----
  build_model <- function(entrada) {
    #rede (site = https://medium.com/machine-learning-world/neural-networks-for-algorithmic-trading-1-2-correct-time-series-forecasting-backtesting-9776bfd9e589)
    # model = Sequential()  
    # model.add(Dense(64, input_dim=30, activity_regularizer=regularizers.l2(0.01))) model.add(BatchNormalization()) 
    # model.add(LeakyReLU()) 
    # model.add(Dropout(0.5)) 
    # model.add(Dense(16,activity_regularizer=regularizers.l2(0.01))) model.add(BatchNormalization()) 
    # model.add(LeakyReLU()) 
    # model.add(Dense(2)) 
    # model.add(Activation('softmax'))
    #compila
    # reduce_lr = ReduceLROnPlateau(monitor='val_loss', factor=0.9, patience=5, min_lr=0.000001, verbose=1) 
    # model.compile(optimizer=opt,loss='categorical_crossentropy', metrics=['accuracy'])
    #treino
    # history = model.fit(X_train, Y_train, nb_epoch = 50,batch_size = 128, verbose=1, 
    #                     validation_data=(X_test, Y_test), shuffle=True, callbacks=[reduce_lr])
    
    model <- keras_model_sequential() %>%
      layer_dense(units = 64, activation = "relu",
                  input_shape = entrada) %>%
      layer_dense(units = 64, activation = "relu") %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = "mse",
      optimizer = optimizer_rmsprop(),
      metrics = list("mean_absolute_error")
    )
    
    model
  }
  model1 <- build_model(dim(x1)[2])
  model1 %>% summary()
  
  # Display training progress by printing a single dot for each completed epoch.
  print_dot_callback <- callback_lambda(
    on_epoch_end = function(epoch, logs) {
      if (epoch %% 80 == 0) cat("\n")
      cat(".")
    }
  )
  
  epochs <- 800
  
  # The patience parameter is the amount of epochs to check for improvement.
  early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)
  
}

{#Treino----
  # # Treino com parada em relação ao numero de épocas----
  # history <- model %>% fit(
  #   x1,
  #   y1,
  #   epochs = epochs,
  #   validation_split = 0.4,
  #   verbose = 0,
  #   callbacks = list(print_dot_callback)
  # )
  # 
  # library(ggplot2)
  # 
  # plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
  #   coord_cartesian(ylim = c(0,0.15))
  # 
  # Treino com parada em relação ao erro----
  arquivo <- "DDN_1"
  model1 <- build_model(dim(x1)[2])
  history1 <- model1 %>% fit(
    x1,
    y1,
    epochs = epochs,
    validation_split = 0.2,
    verbose = 0,
    callbacks = list(early_stop, print_dot_callback, callback_tensorboard("logs/run1a_1"))
  )
  
  model2 <- build_model(dim(x2)[2])
  history2 <- model2 %>% fit(
    x2,
    y2,
    epochs = epochs,
    validation_split = 0.2,
    verbose = 0,
    callbacks = list(early_stop, print_dot_callback, callback_tensorboard("logs/run1b_1"))
  )
  
  ggplot(history1, metrics = "mean_absolute_error", smooth = FALSE) +
    coord_cartesian(xlim = c(0, 50), ylim = c(0, 0.15))
  
  ggplot(history2, metrics = "mean_absolute_error", smooth = FALSE) +
    coord_cartesian(xlim = c(0, 50), ylim = c(0, 0.15))
}

{#Calculo Erro----
  # Calculo erro primeira amostra ----
  
  # c(loss, mae) %<-% (model %>% evaluate(x1, y1, verbose = 0))
  # 
  # paste0("Mean absolute error on test set: $", sprintf("%.2f", mae * 1000))
  
  yhat1 <- model1 %>% predict(x1)
  
  erro1 = sqrt((y1-yhat1)^2)
  erro1_validacao = mean(erro1[validacao])
  
  yhat1_direcao <- yhat1 %>% geraABx()
  
  cont = ifelse(yhat1_direcao[validacao -1] == y1_direcao[validacao -1], 1, 0)
  pocid1_validacao = sum(cont)/length(cont)
  
  # Calculo erro segunda amostra ----
  
  yhat2 <- model2 %>% predict(x2)
  
  erro2 = sqrt((y2-yhat2)^2)
  erro2_validacao = mean(erro2[validacao])
  
  yhat2_direcao <- yhat2 %>% geraABx()
  
  cont = ifelse(yhat2_direcao[validacao -1] == y2_direcao[validacao -1], 1, 0)
  pocid2_validacao = sum(cont)/length(cont)
  
  #Descoverter erro
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

{#Imprimir Informações----
  # print(paste("Erro treino 1: Padronizado: ",erro1_treino, ", Valor:",erro2_treino_valor))'
  # print(paste("Erro treino 2: Padronizado: ",erro2_treino, ", Valor:",erro2_treino_valor))
  # print(paste("Erro teste 1:  Padronizado: ",erro1_teste, ", Valor:",erro1_teste_valor))
  # print(paste("Erro teste 2:  Padronizado: ",erro2_teste, ", Valor:",erro2_teste_valor))
  print(paste("Erro Validação 1: Padronizado: ",erro1_validacao, ", Valor:",erro1_validacao_valor))
  print(paste("Erro Validação 2: Padronizado: ",erro2_validacao, ", Valor:",erro2_validacao_valor))
  
  print(paste("POCID 1: Padronizado: ",pocid1_validacao))
  table(y1_direcao[validacao - 1],yhat1_direcao[validacao -1])
  
  print(paste("POCID 2: Padronizado: ",pocid2_validacao))
  confusionMatrix(y2_direcao[validacao - 1],yhat2_direcao[validacao -1])
}

{#Plots dos resultados----
  plot(yhat1, type = "l", col = "red", xlim = c(200, 250), ylim = c(0, 1))
  par(new = T)
  plot(y1, type = "l", col = "black", xlim = c(200, 250), ylim = c(0, 1))
  
  plot(yhat2, type = "l", col = "red", xlim = c(200, 250), ylim = c(0, 1))
  par(new = T)
  plot(y2, type = "l", col = "black", xlim = c(200, 250), ylim = c(0, 1))
}


# tensorboard("logs/run_1")
# tensorboard("logs/run_2")

