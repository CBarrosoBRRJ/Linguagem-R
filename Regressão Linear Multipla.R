# Regressão Linear Multipla
# Regrasão Linear serve para fazer uma previsão de um novo valor, com base em mais de uma variavel.

# Regra de ouro - Sugestão > 20 observação para cada Variaveis.


# Job [Projeto Analisado] > Verificar a Relações para prever preço das casas.


# Primeira Etapa > Carregar Pacote.
if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpubur, cowplot, star_regline_equation,
               pairs.panels)

install.packages("pairs.panels")
library(pairs.panels)

install.packages("lm.beta")
library(lm.beta)

install.packages("scatterplot3d")
library(scatterplot3d)

# Segunda Etapa > Carregar Banco de Dados.
dados <- read.csv(file.choose() , sep = ',', header = T)# Carregando Base de Dados.
# A base precisa de tratamento nos dados antes de ser analisado.
dados$id <- NULL
dados$date <- NULL
#--

# Proximo Passo >>>> Analisando a Estrutura do  Banco de Dados Carregado.
View(dados) # Visualizando os Dados em Janela Separada.
glimpse(dados) # Visualizando o resumo dos Dados.
head(dados) # Visualiza os 6 primeiros dados.
dim(dados)
cor(dados[1:3])

#--

# Proximo Passo >>>>  Criando um Modelo
# Quero saber o quanto a quantidade de banheiro e quartos afetam o preço.
# Estou informando que o preço será explicado pelas variaveis escolhidas.
mod <- lm(price ~ bedrooms + bathrooms , data =  dados)

#--

# Proximo Passo >>>>  Analise Grafica
par(mfrow = c(1,1))
plot(mod)

#--

# Proximo Passo >>>>  Analise Descritiva

## Normalidade dos Residuos:
shapiro.test(mod$residuals)

## Outlier no Residuos:
summary(rstandard(mod))

## Independência dos Residuos: (Durbin-Watson)
durbinWatsonTest(mod)

## Homocedasticidade (Breusch-Pagan)
bptest(mod)

## Ausência de Multicolinearidade:
pairs.panels(dados) # Multicolinearidade r > 0.9 (ou 0.8)
vif(mod) # Multicolinearidade:  VIF > (Maior que) 10

#--

# Proximo Passo >>>>  Criando Segundo Modelo para Comparação:
mod2 <- lm(price ~ bedrooms , data =  dados)

#--

# Proximo Passo >>>> Análise do Modelo:
summary(mod)
summary(mod2)

## Obtenção dos coeficientes padronizados:
lm.beta(mod)
lm.beta(mod2)

## Obtenção do IC 95% para os Coeficientes:
confint(mod)
confint(mod2)

## Comparação entre quaisquer modelo:
AIC(mod, mod2) # Quanto MENOR melhor.
BIC(mod, mod2) # Quanto MENOR melhor.

## Para comparação entre modelos aninhados ou hierarquicos:
anova(mod, mod2) # O melhor será com o menor valor de RSS (residual sum of squares)
### Modelo que são derivados do mesmo modelo.

#--

# Proximo Passo >>>> Criando um Gráfico de Dispersão 3D para prever um plano (Eixo X, Y,Z)
graph <- scatterplot3d(dados$price ~ dados$bedrooms + dados$bathrooms,
                       pch = 16, angle = 30 , color = "steelblue", box = FALSE,
                       xlab = "Quartos", ylab = "Banheiros", zlab = "Preços")


graph$plane3d(mod, col = "black", draw_polygon = TRUE) # Os pontos que ficaram dentro do modelo , é sinal que o modelo acertou a previsão.

#--

# Proximo Passo >>>> Descrevendo Resultado do Modelo.
summary(mod)
summary(mod)$adj.r.squared # Coeficiente de Determinação

#--

# Proximo Passo >>>> Previsões
predict(mod, data.frame( bedrooms = 5 , bathrooms = 3, data = dados))



###################### Métodos de Seleção (Escolhendo) de Modelo Automatico ######################
pacman::p_load(MASS)

mod_inicial <- lm(dados$price ~ dados$bedrooms + dados$bathrooms)
mod_simples <-  lm(dados$price ~ 1)
stepAIC(mod_inicial, scope = list(upper = mod_inicial,
                                  lower = mod_simples), direction = "backward")

# Call : É o modelo mais adequado.

