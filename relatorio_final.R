
library(dplyr)
library(broom)
library(gt)
library(ggplot2)
library(writexl)

dt <- dados_plano_de_saude
dt


## Padronização das variáveis

#As variáveis foram padronizadas e armazenadas em dados.

dados <- dt %>%
  mutate(
    sexo = factor(sexo, levels = c("feminino","masculino")),
    tabagismo = factor(tabagismo, levels = c(0,1), labels = c("Nao","Sim")),
    regiao = factor(regiao,
                    levels = c("nordeste","sudeste","noroeste","sudoeste")),
    uso = as.integer(uso)
  )


# **Verificação do Cliente**

##Cliente que paga o menor preço no plano de saúde, servindo como **ponto de referência mínimo com relação ao preço** para o banco de dados.

menor_preco <- dt %>% 
  arrange(dependentes, preco) %>% 
  slice(1)
write_xlsx(menor_preco, "menor_preco.xlsx")


#Cliente que paga o maior preço no plano de saúde, servindo como **ponto de referência máximo com relação ao preço** para o banco de dados.


maior_preco <- dt %>% 
  arrange(desc(dependentes), desc(preco)) %>% 
  slice(1)
write_xlsx(maior_preco, "maior_preco.xlsx")


# Número de registros

dt %>% 
  filter(sexo == "masculino") %>% 
  summarise(masculino = n())

dt %>% 
  filter(sexo == "feminino") %>% 
  summarise(feminino = n())


#Foram identificados **676 registros do sexo masculino** e **662 registros do sexo feminino** no conjunto de dados.

# Regressão linear

m_lin <- lm(preco ~ idade + sexo + imc + dependentes + tabagismo + regiao, data = dados)
summary(m_lin)
linear <- as.data.frame(summary(m_lin)$coefficients)
write_xlsx(linear, "linear.xlsx")


#Equação:
  
 # preco​=−6957,33+256,90⋅idade−5125,03⋅1(sexo=masculino)+339,61⋅IMC+476,88⋅dependentes+23842,49⋅1(tabagismo=Sim)−1041,41⋅1(regia˜o=sudeste)−356,57⋅1(regia˜o=noroeste)−957,95⋅1(regia˜o=sudoeste)

  
 # -   R² = 0,75(o modelo explica \~75% da variação do preço).


  
#  F global altamente significativo (p-value \< 2.2e-16)

#As variáveis idade, sexo masculino, imc, dependentes e tabagismo foram altamente significativas (\*\*\*) onde.: PR (\>\|T\|) \<0,001

#-   Preço sobe com idade, IMC, nº de dependentes e (fortemente) com tabagismo;

#-   Preço é menor para sexo masculino.

#Exemplificando subida do preço com relação à idade

ggplot(dt, aes(x = idade, y = preco, group = cut_width(idade, width = 5))) +
  geom_boxplot() +
  labs(x = "Idade (agrupada em janelas de 5 anos)", y = "Preço",
       title = "Boxplot — Preço por idade (janelas de 5 anos)") +
  theme_bw()


# Regressão Logística

#A regressão logística foi usada para modelar e prever a probabilidade de uso do plano (Uso=1) a partir de variáveis como idade, sexo, IMC, nº de dependentes e tabagismo.

m_log2 <- glm(uso ~ idade + sexo + imc + dependentes + tabagismo, data=dt, family = binomial(link="logit"))
m_log2
Rlogistica <- as.data.frame(summary(m_log2)$coefficients)
write_xlsx(Rlogistica, "modelo_logistico.xlsx")

#Equação:
  
#n=-31.5942 + 0.33760,3376⋅idade−0,1015⋅1(sexo=masculino)+0,4928⋅IMC+0,2263⋅dependentes+1,6248⋅1(tabagismo=Sim)

#Tabela de OR com IC95% (sem intercepto) + destaque
or_tab <- tidy(m_log2, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Variavel = dplyr::recode(term,
                             "idade"         = "Idade",
                             "imc"           = "IMC",
                             "dependentes"   = "Dependentes",
                             "sexomasculino" = "Sexo (masculino vs feminino)",
                             "tabagismoSim"  = "Tabagismo (Sim vs Não)"
    ),
    `OR (IC95%)` = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high),
    Destaque = if_else(conf.low > 1 | conf.high < 1, "✓", "")
  ) %>%
  select(Variavel, `OR (IC95%)`, Destaque)

#versão formatada em tabela  com destaque em negrito
or_tab %>%
  gt() %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(rows = Destaque == "✓")
  )


# OR (Odds Ratio) = *razão de chances*.

# OR\>1 indica aumento das odds de uso por unidade do preditor

# Cada OR quantifica o tamanho do impacto de uma variável

# Nesse caso, o OR com mai impacto ao uso do plano é o tabagismo (OR≈5). Ou seja, \~5× mais chances de uso.

dt$prob_uso <- predict(m_log2, type = "response")

# Exemplo 1: boxplot por sexo
ggplot(dt, aes(x = sexo, y = prob_uso, fill = sexo)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Probabilidade prevista de uso por sexo",
       x = "Sexo", y = "Probabilidade prevista de uso") +
  theme_minimal()

# Exemplo 2: boxplot por tabagismo
ggplot(dt, aes(x = tabagismo, y = prob_uso, fill = tabagismo)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Probabilidade prevista de uso por tabagismo",
       x = "Tabagismo", y = "Probabilidade prevista de uso") +
  theme_minimal()


########################################### 
# correlação  mulher e dependente 
pearson <- cor.test(dt$idade, dt$preco, method = "pearson")
pearson


dt3 <- dt %>%
  mutate(
    idade40 = ifelse(idade >= 40, 1, 0),
    idade40 = factor(idade40)
  )

dt3

install.packages("caret")
library(caret)


# constrói dt3 a partir de dt e já calcula prob_uso com o modelo
dt3 <- dt %>%
  dplyr::mutate(
    idade60  = factor(ifelse(idade >= 60, 1, 0), levels = c(0,1)),
    prob_uso = predict(m_log2, newdata = ., type = "response"),
    uso_prev = factor(as.integer(prob_uso >= 0.5), levels = c(0,1)),
    uso      = factor(uso, levels = c(0,1))
  )

# 2) Checagens rápidas
nrow(dt3)                         # deve ser 1338
length(dt3$prob_uso)              # deve ser 1338
table(dt3$uso_prev, useNA="ifany")
str(dt3$uso)                      # fator com níveis 0/1



dt3


matriz_confusao <- confusionMatrix(dt3$uso_prev, dt3$uso, positive = "1")
matriz_confusao$table   # tabela da matriz
matriz_confusao$overall # métricas gerais (acurácia, etc.)
matriz_confusao$byClass 
