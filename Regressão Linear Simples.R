# Regressão Linear Simples
# Regrasão Linear serve oara fazer uma previsão de um novo valor.

# Regra de ouro - Sugestão > 20 observação para cada Variaveis.

# O Banco de dados contem informações de 200 CD's comercializados por uma gravadora.
# Job [Projeto Analisado] > Verificar a Relação entre o Gasto em Publicidade com os Resultados de Venda.
# Objetivo > Prever vendas de CD's.

# Primeira Etapa > Carregar Pacote.
if(!require(pacman)) install.packages("pacman")
install.packages("star_regline_equation")
library(pacman)
library(star_regline_equation)

pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpubur, cowplot, star_regline_equation)

# Segunda Etapa > Carregar Banco de Dados.
dados <- read.csv(file.choose() , sep = ';', header = T)# Carregando Base de Dados.

#--

View(dados) # Visualizando os Dados em Janela Separada.
glimpse(dados) # Visualizando o resumo dos Dados.
cor(dados) # Verifica a Correlação entre as Variaveis (Ela explica 0.6028226%)
head(dados) # Visualiza os 6 primeiros dados.
dim(dados)

# Terceira Etapa > Verificação dos Pressupostos para a Regreção Linear.

## Relação Linear entre a VD e a VI
### VD > Variavel Dependente é o que quero Prever , nesse caso "Vendas".
### VI > Variavel Independente é o que imagino que tenha uma influência no resultado, nesse caso "Publicidade".
plot(dados$Publicidade, dados$Vendas)

## Construção do Modelo
mod <- lm(Vendas ~ Publicidade,data = dados)
summary(mod)
#--
## Análise Gráfica > https://data.library.virginia.edu/diagnostic-plots/
plot_grid(mod, ncol = 2, nrow = 2)
par(mfrow=c(2,2))# Programando para sair 4 graficos em uma mesma imagem.
plot(mod)

# Residuals VS Leverage > Ele permite analisar a Linearidade, e a homocedasticidade.

### Linearidade > Olhando a linha vermelha , quanto mais reta e horizontal, ...
### ...  caindo em cima da linha cinza horizonta pontilhada, então temos uma relação linear.
### Então meu pressuposto de linearidade esta atendido.

### homocedasticidade > descreve uma situação em que o termo de erro (ou seja, o “ruído” ...
### ... ou perturbação aleatória na relação entre as variáveis independentes e a variável dependente)... 
### ... é o mesmo em todos os valores das variáveis independentes.
### Dispersão dos pontos no eixo y, formando uma especie de "retangulo"
### Se o resultado for uam especie de "triangulo" é sinal de resultado ruim.


# Normal Q-Q > Permite verificar se os residu-os têm distribuição normal.
## Caso os residu-os tenham distribuição normal > Eles devem estar na linha pontilhada em cinza.


# Scale Location > Caso exista homocedasticidade , deve se analisar a linha vermelha horizontal...
## Ela deve estar aproximadamente horizontal
## Podemos analisar novamente o padrão retangular de dispersão dos pontos.


# Residuals VS Leverage > Verificar se existe residuos que são outliers, ou pontos de alavancagem.

## outliers > são dados que se diferenciam drasticamente de todos os outros...
## ... Em outras palavras, um outlier é um valor que foge da normalidade e que pode ...
## ...(e provavelmente irá) causar anomalias nos resultados obtidos por meio de algoritmos e sistemas de análise.

## Caso haja outliers, iriamos visualizar no eixo y, valores acima e igual de 03 (nesse caso).
## Ponto de Alavancagem > Irimos visualizar uma linha pontilhada vermelha ....
## ... Se houver pontos fora da linha pontilhada vermelha ele é um ponto que devemos nos preocupar.

par(mfrow=c(1,1))

# Quarta Etapa > Analise com Teste Estatistico 

## Coeficientes
mod$coefficients[1] # Intercept (Venda)
mod$coefficients[2] # Inclinação (Publicidade)

## Normalidade dos resíduos.
shapiro.test(mod$residuals) # Se P-Value for maior que 0.05 (5%) , não iremos rejeitar as hipoteses nulas.

## Outliers nos resíduos padronizados.
summary(rstandard(mod)) # Visualizar os valores min e max, caso eles sejam semelhantes em seus extremos....
# ... e a medianda deve ser bem proxima de zero.

## Independência dos resíduos (Durbin-Watson)
durbinWatsonTest(mod)# Statistic deve estar mais proximo possivel de 02 (Entre 01 e 03) > ....
# ... o P-Value deve ser maior que 0.05 (5%)

## Homocedasticidade (Breusch - Pagan)
bptest(mod) # Se P-Vaule for maior que 0.05 (5%) os dados podem ser aceitos.


# Quinta Etapa > Análise do Modelo
summary(mod)
# Responsta Analisada > A cada 01 real gasto em publicidade ele aumenta 0.10495 em vendas.
# R-Square > O investimento em publicidade explica o resultado de R-Square (0.3634%) das vendas.


# Sexta Etapa > Gráfico de Dispersão

ggplot(data = dados, mapping = aes(x = Publicidade, y = Vendas))+ 
  geom_point() + 
  geom_smooth(method = "lm", col = "red") + 
  star_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "*plain(\",\")~~")),
                        label.x = 0 , label.y = 400) + 
  theme_classic ()

#--
plot(Vendas ~ Publicidade , data = dados) # Imprimindo o grafico na tela.
abline(mod, col = "red") # Inserindo a linha com base no modelo com a cor vermelha

### Como reportar 
# Pode colocar as informações do summary
# Responsta Analisada > A cada 01 real gasto em publicidade ele aumenta 0.10495 em vendas.
# R-Square > O investimento em publicidade explica o resultado de R-Square (0.3634%) das vendas.
summary(mod)$r.squared


# Setima e Ultima  Etapa > Realizando Modelo de Previsão
# Predição
## Pregunta > Quero saber se investirmos 1000, quanto sera minha venda?
predict(mod, data_frame(Publicidade = 1000))
