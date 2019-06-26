library(keras)
library(ggplot2)

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

{#Configuração do Treino/Modelo----
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
}


#{#Treino----
# # Treino
batch_size = 25
epochs <- 150
arquivo <- "DDN_3_Janela"

j_treino <- 200   # janela de treino (-1 para funcionar certo)
j <- 1                # janela de previs?o
i <- 1
treino <- 1:j_treino
teste <- tail(treino,batch_size)

yhat1_teste = vector()
yhat1_teste_direcao = vector()
yhat2_teste = vector()
yhat2_teste_direcao = vector()

print("Inicio For")
barra <- progress::progress_bar$new(total = 49)
for(i in 1:49){
  #teste
  barra$tick()
  print("Train")
  print(i)
  
  #Separa
  x1_treino <- array(data = x1[treino,2:6], dim = c(length(x1[treino,2]), 1, 5))
  x1_teste <- array(data = x1[teste + i,2:6], dim = c(length(x1[teste + i,2]), 1, 5))
  
  #Treina
  model1 <- build_model2(1,5)
  model1 %>% fit(
    x1_treino,
    y1[treino],
    batch_size = batch_size,
    epochs = epochs,
    verbose    = 0, 
    shuffle    = FALSE,
    callbacks = callback_tensorboard("logs/run3j_1")
  )
  
  #Preve e Guarda
  yhat1_aux <- model1 %>% predict(x1_teste, batch_size = batch_size)
  yhat1_teste[i] = tail(yhat1_aux,1)
  yhat1_teste_direcao[i] <- geraABx(c(y1[tail(treino,1)], yhat1_teste[i]))
  
  #########################
  #Separa
  if(i != 49){
    x2_treino <- array(data = x2[treino,], dim = c(length(treino), 9, 1))
    x2_teste <- array(data = x2[teste + i,], dim = c(length(teste), 9, 1))
    
    #Treina
    model2 <- build_model2(9,1)
    history2 <- model2 %>% fit(
      x= x2_treino, y= y2[treino],
      batch_size = batch_size,
      epochs = epochs,
      validation_split = 0.125,
      verbose    = 0,
      shuffle    = FALSE
    )
    
    #Preve e Guarda
    yhat2_aux <- model2 %>% predict(x2_teste, batch_size = batch_size)
    yhat2_teste[i] = tail(yhat2_aux,1)
    yhat2_teste_direcao[i] <- ifelse(yhat2_teste[i] >= 0, 1,0)
  }

  treino = treino + 1
}
# scores1 <- model1 %>% evaluate(
#   x1_treino, y1[treino],
#   batch_size = batch_size
# )
# 
# scores2 <- model2 %>% evaluate(
#   x2_treino, y2[treino],
#   batch_size = batch_size
# )

# plot(history1, metrics = "mean_absolute_error", smooth = FALSE) +
#   coord_cartesian(xlim = c(0, 50), ylim = c(0, 0.15))
# 
# plot(history2, metrics = "loss", smooth = FALSE) +
#   coord_cartesian(xlim = c(0, 250), ylim = c(0, 5))
#}

#{#Calculo Erro----
# Calculo erro primeira amostra ----
erro1 = sqrt((y1[1:49 + j_treino]-yhat1_teste)^2)
erro1_validacao = mean(erro1)
cont = ifelse(yhat1_teste_direcao == y1_direcao[1:49 + (j_treino - 1)], 1, 0)
pocid1_validacao = sum(cont)/length(cont)


erro2 = sqrt((y2[1:48 + j_treino]-yhat2_teste)^2)
erro2_validacao = mean(erro2)
cont = ifelse(yhat2_teste_direcao == y2_direcao[1:48 + (j_treino)], 1, 0)
pocid2_validacao = sum(cont)/length(cont)


# Calculo erro segunda amostra ----
# #TREINO
yhat2_treino <- model2 %>% predict(x2_treino, batch_size=batch_size)

erro2 = sqrt((y2[treino]-yhat2_treino)^2)
erro2_treino = mean(erro2)

yhat2_direcao <- ifelse(yhat2_treino>=0, 1,0)

cont_treino = ifelse(yhat2_direcao == y2_direcao[treino], 1, 0)
pocid2_treino = sum(cont_treino)/length(cont_treino)
# 
# yhat2_direcao == y2_direcao[treino]
# 
# #VALIDAÇÃO
# yhat2_validacao <- model2 %>% predict(x2_validacao, batch_size=batch_size)
# 
# erro2 = sqrt((y2[validacao]-yhat2_validacao)^2)
# erro2_validacao = mean(erro2)
# 
# yhat2_direcao_validacao <- ifelse(yhat2_validacao>=0, 1,0)
# 
# cont_validacao = ifelse(yhat2_direcao_validacao == y2_direcao[validacao], 1, 0)
# pocid2_validacao = sum(cont_validacao)/length(cont_validacao)
# 
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

scores1 <- model1 %>% evaluate(
  x1_treino, y1[treino],
  batch_size = batch_size
)

scores2 <- model2 %>% evaluate(
  x2_treino, y2[treino],
  batch_size = batch_size
)

print(paste("POCID 1 Treino ",pocid1_treino))
table(y2_direcao[treino],yhat2_direcao)

print(paste("POCID 1 Validacao ",pocid1_validacao))
table(y1_direcao[1:49 + j_treino],yhat1_teste_direcao)

print(paste("POCID 2 Treino ",pocid2_treino))
table(y2_direcao[treino],yhat2_direcao)

print(paste("POCID 2 Validacao ",pocid2_validacao))
table(y2_direcao[1:48 + j_treino+1],yhat2_teste_direcao)
#}

#{#Plots dos resultados----
plot(yhat1_teste,x = 1:49 + j_treino, type = "l", col = "red", xlim = c(0,250), ylim = c(-1, 1))
par(new = T)
plot(y1, type = "l", col = "black", xlim = c(0,250), ylim = c(-1, 1))

plot(y2, type = "l", col = "black", xlim = c(0,250),  ylim = c(-3, 3))
par(new = T)
plot(yhat2_teste, x = 1:48 + j_treino, type = "l", col = "red", xlim = c(0,250),  ylim = c(-3, 3))
#}


# tensorboard("logs/run_1")
# tensorboard("logs/run_2")

