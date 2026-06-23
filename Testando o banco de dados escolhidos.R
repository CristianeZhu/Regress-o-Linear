library(readr)
dados <- read_csv("gym_members_exercise_tracking.csv")
View(dados)

# Qual variável resposta vamos escolher?
## Qual faz sentido? Fat_percentagem (p mim). Quero entender oq afeta a gordura corporal

## Verificando a distribuição
hist(dados$Fat_Percentage) #Parece um pouco assimétrica (fazer uma transformação?)
shapiro.test(dados$Fat_Percentage) ## p-valor bem pequeno (tem efeito)

## Correlação com outras variáveis

correlacao <- cor(dados[, sapply(dados, is.numeric)]) 
data.frame(correlacao) ## Péssimo de analisar por tabela


## Grafico de correlação
library(corrplot)
corrplot(cor(dados[, sapply(dados, is.numeric)]),
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.cex = 0.9,      # tamanho dos nomes das variáveis
         number.cex = 0.8,  # tamanho dos números
         cl.cex = 0.9)      # tamanho da legenda
png("correlacao.png", width = 1400, height = 1200, res = 150)


## Qual variável teve mais correlações:- Nível de experiencia (porém ela é categórica)
##                                     - Porcentagem de gordura com: horas_duraçao, calorias queimadas, 
##                                        frequencia de treino, nivel de experiencia e consumo de agua (correlaçoes negativas)

## Possiveis problemas de multicolinearidade entre horas duração e calorias queimadas (pois tem correlação = 0.9)
## Testando se tem problema de multicolinearidade

modelo <- lm(Fat_Percentage ~ Calories_Burned + `Session_Duration (hours)` + 
               `Workout_Frequency (days/week)` + `Water_Intake (liters)` + Experience_Level, data = dados)

library(car)
vif(modelo) ## todos valores menores que 10, Logo, n há problemas

## Deu duas variaveis n significativas (p-valor mto alto)
## vai ter q mudar ou tirar uma delas pois pode estar correlacionada

summary(modelo)
cor(dados[,c("Fat_Percentage",
             "Calories_Burned",
             "Session_Duration (hours)",
             "Workout_Frequency (days/week)",
             "Water_Intake (liters)",
             "Experience_Level")])













############
#### Analise de residuo (fiz só p eu entender um pouco melhor)
modelo3 <- lm(Fat_Percentage ~ Calories_Burned + 
                `Water_Intake (liters)` + Experience_Level, data = dados)


par(mfrow = c(2,2), family = "serif")
plot(modelo3) ####3 heterocedasticidade e leve nao normalidade

## Medida corretiva (n sei se ajudou)
modelo_log <- lm(log(Fat_Percentage) ~ Calories_Burned + 
                   `Water_Intake (liters)`+ Experience_Level, data = dados)
par(mfrow = c(2,2))
plot(modelo_log)



#### testando calorias queimada como variável resposta


hist(dados$Calories_Burned)
shapiro.test(dados$Calories_Burned)


correlacao <- cor(dados[, sapply(dados, is.numeric)]) 
data.frame(correlacao) ## Péssimo de analisar por tabela

modelo4 <- lm(Calories_Burned ~  Fat_Percentage + `Session_Duration (hours)` + Avg_BPM +
                `Water_Intake (liters)` + Experience_Level, data = dados)
summary(modelo4)

vif(modelo4)
