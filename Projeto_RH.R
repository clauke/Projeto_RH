###---- Projeto RH Analytics ----###

# Pacotes
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

# 1. Carga dos dados
dados <- read_csv('dataset_rh.csv')

# 2. Análise Exploratória
head(dados)
glimpse(dados)
summary(dados)

# 2.1 Tratamento da coluna Idade
unique(dados$Idade)
class(dados$Idade)

is_num <- suppressWarnings(!is.na(as.numeric(dados$Idade))) # quais valores são números
table(is_num, useNA = 'ifany') # contagem de números e não-números

dados <- dados %>%
  mutate(Idade = str_trim(Idade),  # remove espaços em branco
         Idade = case_when(
           str_to_lower(Idade) == 'quarenta' ~ '40',
           TRUE ~ Idade
         ),
         Idade = as.numeric(Idade)) # converte para número

str(dados$Idade)
sum(is.na(dados$Idade))

# Substituindo NA's pela média de idade
media_idade <- mean(dados$Idade, na.rm = TRUE)
media_idade

dados$Idade[is.na(dados$Idade)] <- round(media_idade, 0)
sum(is.na(dados$Idade))

# 2.2 Tratamento da coluna Gênero
names(dados)[names(dados) == 'Gênero'] <- 'Genero' # retira o acento do nome da coluna

unique(dados$Genero)

dados <- dados %>%
  mutate(
    Genero = str_trim(str_to_lower(Genero)),
    Genero = case_when(
      Genero %in% c('feminino', 'feminina', 'fem') ~ 'F',
      Genero %in% c('masc', 'masculino', 'm') ~ 'M',
      TRUE ~ NA_character_
    )
  )

# 2.3 Tratamento da coluna Data_Admissao
dados <- dados %>%
  mutate(
    Data_Admissao = case_when(
      str_detect(Data_Admissao, '^\\d{4}-\\d{2}-\\d{2}$') ~ ymd(Data_Admissao),
      str_detect(Data_Admissao, '^\\d{2}/\\d{2}/\\d{4}$') ~ dmy(Data_Admissao),
      TRUE ~ NA_Date_
    )
  )

class(dados$Data_Admissao)

# 2.4 Tratamento da coluna Ultima_Avaliacao
sum(is.na(dados$Ultima_Avaliacao))

unique(dados$Ultima_Avaliacao)
str(dados$Ultima_Avaliacao)

# Categorizando as notas
dados <- dados %>%
  mutate(
    Fx_Avaliacao = case_when(
      is.na(Ultima_Avaliacao) ~ NA_character_,
      Ultima_Avaliacao < 3 ~ 'Ruim',
      Ultima_Avaliacao < 4 ~ 'Média',
      Ultima_Avaliacao <= 4.5 ~ 'Boa',
      TRUE ~ 'Excelente'
    )
  )

# 2.5 Tratamento da coluna Salario
class(dados$Salario)

dados <- dados %>%
  mutate(
    Salario = str_replace_all(Salario, 'R\\$\\s*', ''),
    Salario = str_replace_all(Salario, '\\.', ''),
    Salario = str_replace_all(Salario, ',', '.'),
    Salario = str_trim(Salario),
    Salario = as.numeric(Salario)
  )

summary(dados$Salario)
sum(is.na(dados$Salario))

# Preenchendo valores ausentes com a média salarial
media_salario <- round(mean(dados$Salario, na.rm = TRUE), 2)

dados$Salario[is.na(dados$Salario)] <- media_salario
sum(is.na(dados$Salario))

# Correção de Outlier
which(dados$Salario == max(dados$Salario, na.rm = TRUE))
dados$Salario[dados$Salario == 120807] <- 12807

# Criando faixas salariais
dados <- dados %>%
  mutate(
    Faixa_Salarial = case_when(
      Salario <= 3000 ~ 'Até R$3.000',
      Salario < 5000 ~ 'De R$3.001 a R$4.999',
      Salario < 7000 ~ 'De R$5.000 a R$6.999',
      Salario < 10000 ~ 'De R$7.000 a R$9.999',
      Salario >= 10000 ~ 'Acima de R$10.000',
      TRUE ~ 'Não informado'
    )
  )

class(dados$Faixa_Salarial)

# Convertendo Faixa_Salarial em fator ordenado
dados$Faixa_Salarial <- factor(
  dados$Faixa_Salarial,
  levels = c(
    'Até R$3.000',
    'De R$3.001 a R$4.999',
    'De R$5.000 a R$6.999',
    'De R$7.000 a R$9.999',
    'Acima de R$10.000',
    'Não informado'
  ),
  ordered = TRUE
)

# 2.6 Tratamento da coluna Dias_Afastado
unique(dados$Motivo_Saida)
sum(is.na(dados$Dias_Afastado))

dados$Dias_Afastado[is.na(dados$Dias_Afastado)] <- 0
dados$Dias_Afastado[dados$Dias_Afastado < 0] <- 0 # corrigindo valores negativos

# 3. KPIs
# 3.1 Total de colaboradores
total_colab <- nrow(dados)

# 3.2 Média Salarial
media_sal <- mean(dados$Salario, na.rm = TRUE)

# 3.3 Distribuição por faixa salarial
dist_fxSal <- dados %>%
  count(Faixa_Salarial) %>%
  mutate(Percent = round(n / sum(n) * 100, 1))

# 3.4 % de colaboradores por Status
perc_status <- dados %>%
  count(Status) %>%
  mutate(Percent = round(n / sum(n) * 100, 1))

# 3.5 Turnover (taxa de rotatividade)
total_inativos <- sum(dados$Status == 'Desligado')
tx_turn <- round((total_inativos / total_colab) * 100,  1)

# 4. Gráficos
# 4.1 Distribuição de colaboradores por faixa salarial
dados %>%
  count(Faixa_Salarial) %>%
  ggplot(aes(x = reorder(Faixa_Salarial, n), y = n)) +
  geom_col(fill = '#2E86C1') +
  geom_text(aes(label = n), hjust = -0.1, size = 4) +
  coord_flip() +
  labs(
    title = 'Distribuição de Colaboradores por Faixa Salarial',
    x = 'Faixa Salarial',
    y = 'Quantidade'
  ) +
  theme_minimal()

# 4.2 % de colaboradores por status
df_status <- dados %>%
  count(Status) %>%
  mutate(Percent = round(n / sum(n) * 100, 1),
         Label = paste(Status, ' (', Percent, '%)'))

ggplot(df_status, aes(x = '', y = Percent, fill = Status)) +
  geom_col(width = 1, color = 'white') +
  coord_polar(theta = 'y') +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5), size = 4) +
  labs(title = '% de Colaboradores por Status') +
  theme_void() +
  scale_fill_manual(values = c('#27AE60', '#b4b4b4'))

# 4.3 Salário por Gênero
ggplot(dados, aes(x = Genero, y = Salario, fill = Genero)) +
  geom_boxplot(outlier.color = 'red', outlier.shape = 8, alpha = 0.7) +
  scale_fill_manual(values = c('F' = '#F1948A', 'M' = '#85C1E9')) +
  labs(
    title = 'Distribuição Salarial por Gênero',
    x = 'Gênero',
    y = 'Salário (R$)'
  ) +
  theme_minimal() +
  theme(legend.position = 'none')

# 4.4 Distribuição de idades
ggplot(dados, aes(x = Idade)) +
  geom_histogram(binwidth = 5, fill = '#5DADE2', color = 'white', alpha = 0.8) +
  labs(
    title = 'Distribuição de Idade',
    x = 'Idade',
    y = 'Frequência'
  ) +
  theme_minimal()

# 4.5 Status por Faixa Salarial
prop_dados <- dados %>%
  count(Faixa_Salarial, Status) %>%
  group_by(Faixa_Salarial) %>%
  mutate(Proporcao = n / sum(n))

ggplot(prop_dados, aes(x = Faixa_Salarial, y = Proporcao, fill = Status)) +
  geom_bar(stat = 'identity', position = 'fill') +
  geom_text(aes(label = scales::percent(Proporcao, accuracy = 1)),
            position = position_fill(vjust = 0.5),
            color = 'white', size = 3.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = 'Status por Faixa Salarial',
    x = 'Faixa Salarial',
    y = 'Proporção',
    fill = 'Status'
  ) +
  theme_minimal() +
  scale_fill_manual(values = c('Ativo' = '#27AE60', 'Desligado' = '#b4b4b4'))

# Gerando o dataset tratado
write.csv(dados, 'df_RH_tratado.csv', row.names = FALSE)
