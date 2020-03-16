

#Definindo pasta do projeto
setwd("~/Documentos/Firma_Things/R/Mini-Projeto-4")
# Verificando pasta do projeto
getwd()


# Carregando o dataset de credito 
credit.df <- read.csv("credit_dataset.csv", header = TRUE)

head(credit.df)


## Criando a função de converter as variaveis para o tipo fator (categorico)
to.factors <- function(df,variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return (df)
}

#criando a função de Normalização


  scale.features <- function(df,variables){
    for (variable in variables){
      df[[variable]] <- scale(df[[variable]],center = T, scale = T)
    }
    return (df)
  }


#Normalizando as variaveis para credit.df
  ## Lembrando que quando falamos de normalização, todas as variaveis precisam estar na MESMA ESCALA, se eu tiver uma variavel na casa da dezena, outra da unidade outra no milhar, precisamos NORMALIZAR ELAS!

numeric.vars <- c("credit.duration.months","age","credit.amount")
credit.df <- scale.features(credit.df,numeric.vars)

# Variáveis do tipo fator
categorical.vars <- c('credit.rating', 'account.balance', 'previous.credit.payment.status',
                      'credit.purpose', 'savings', 'employment.duration', 'installment.rate',
                      'marital.status', 'guarantor', 'residence.duration', 'current.assets',
                      'other.credits', 'apartment.type', 'bank.credits', 'occupation', 
                      'dependents', 'telephone', 'foreign.worker')
#fazendo a conversão
credit.df <- to.factors(df = credit.df,variables = categorical.vars)

head(credit.df)

#dividindo os dados em treino e teste 60-40

indexes <- sample(1:nrow(credit.df),size = 0.6*nrow(credit.df))

train.data <- credit.df[indexes,]
test.data <- credit.df [-indexes,] # lembrando que o - é pra pegor o que "não foi pego" do indexes. 


#Feature selection ( seleção de variaveis)

library(caret)
library(randomForest)

#Função para seleção das variaveis utilizando random forest

run.feature.selection <- function(num.iters=20,feature.vars,class.var){
  set.seed(10)
  variable.sizes <-1:10
  control <- rfeControl(functions = rfFuncs,method = "cv",verbose = FALSE,returnResamp = "all",number = num.iters)
  results.rfe <- rfe(x=feature.vars, y= class.var,sizes = variable.sizes,rfeControl = control)
  return(results.rfe)
}

#Exec a função
rfe.results <-run.feature.selection(feature.vars = train.data[,-1],class.var = train.data[,1])

#Visualizando os resultados
rfe.results
varImp((rfe.results))

#Criando e avaliando o modelo
#install.packages("ROCR")
library(ROCR)


#Biblioteca de utilitarios para construção de gráficos
source("plot_utils.R")

#Separate feature and class variables
test.features.vars <- test.data[,-1]
test.class.var <- test.data[,1]


#construindo um modelo de regressão logistica
formula.init <- "credit.rating ~." # aqui estou passando todas as variaveis do modelo.
formula.init <- as.formula(formula.init)
lr.model <- glm(formula = formula.init, data = train.data, family = "binomial")

summary(lr.model)

#Testando o modelo nos dados de teste
lr.predictions <- predict(lr.model, test.data, type = "response")
lr.predictions <- round(lr.predictions)

#avaliando o modelo pela confusion matrix
confusionMatrix(table(data= lr.predictions,reference = test.class.var), positive = '1')

##feature selection ( tentando melhorar o modelo)

formula <- "credit.rating ~ ."
formula <- as.formula(formula)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
model <- train(formula,data = train.data,method = "glm", trControl = control)
importance <- varImp(model,scale =FALSE)
plot(importance)

#Construind o modelo com as variaveis selecionadas

formula.new <- "credit.rating ~ account.balance + credit.purpose + previous.credit.payment.status + credit.purpose + savings + credit.duration.months"
formula.new <- as.formula(formula.new)
lr.model.new <- glm(formula = formula.new,data = train.data,family = "binomial")

#Visualizando o modelo

summary(lr.model.new)

#Testando o modelo nos dados de teste

lr.predictions.new <- predict(lr.model.new, test.data, type = "response")
lr.predictions.new <- round(lr.predictions.new)

#Avaliando o modelo na confusion matrix

confusionMatrix(table(data= lr.predictions.new,reference = test.class.var), positive = '1')


#CRIANDO A CURVA ROC

lr.model.best <- lr.model
lr.prediction.values <- predict(lr.model.best,test.features.vars, type = "response")
predictions <- prediction(lr.prediction.values,test.class.var)
  par(mfrow = c(1,2))
  
  plot.roc.curve(predictions,title.text =  "Curva ROC")
  plot.pr.curve(predictions,title.tex = "curva precisio/recall")