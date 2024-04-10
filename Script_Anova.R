# Exemplo 1
# Carregue o pacote ggplot2
library(ggplot2)

# Dados fictícios
dados <- data.frame(
  Grupos = c("Grupo A", "Grupo B", "Grupo C"),
  Medias = c(10, 15, 20),
  ErroPadrao = c(2, 1, 3)
)

# Crie um gráfico de barras usando ggplot2
grafico <- ggplot(dados, aes(x = Grupos, y = Medias, fill = Grupos)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Gráfico de Barras com Erro Padrão", x = "Grupos", y = "Médias") +
  geom_errorbar(aes(ymin = Medias - ErroPadrao, ymax = Medias + ErroPadrao),
                position = position_dodge(width = 0.9), width = 0.25,
                color = "red") +
  theme_minimal()

# Exiba o gráfico
print(grafico)

#Exemplo 2

library(ggplot2)
library(Hmisc)

# Dados fictícios
dados <- data.frame(
  Grupos = rep(c("Grupo A", "Grupo B", "Grupo C"), each = 50),
  Valores = rnorm(150, mean = rep(c(10, 15, 20), each = 50), sd = rep(c(2, 1, 3), each = 50))
)

# Crie um gráfico de barras com erro padrão calculado dinamicamente
grafico <- ggplot(dados, aes(x = Grupos, y = Valores, fill = Grupos)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", color = "black") +
  stat_summary(fun.data = "mean_sdl", geom = "errorbar", width = 0.2, color = "red") +
  labs(title = "Gráfico de Barras com Erro Padrão Dinâmico", x = "Grupos", y = "Valores") +
  theme_minimal()

# Exiba o gráfico
print(grafico)



## Estudo
library(tidyverse)

dados_original = data.table::fread("C:/Users/RhayaniPaiuta/Downloads/Material.csv"
                  , header = T
                  , sep = ';'
                  , stringsAsFactors = F
                  , colClasses = "character"
                  , encoding = "Latin-1")
View(dados_original)

dados_original = dados_original %>% mutate(Area_consumida=as.numeric(Area_consumida),
                                           Area_restante=as.numeric(Area_restante),
                                           Area_total=as.numeric(Area_total))

####### FOLHAS NOVAS
## Entre as folhas novas, verificar se existe preferencia alimentar por algum estadio:

dados_novas = dados_original %>% filter(Idade_1=='Nova')

resumo_dados <- dados_novas %>%
  group_by(Estadio) %>%
  summarise(
    media = mean(Area_consumida),
    erro_padrao = sd(Area_consumida) / sqrt(n())
  )

# Criando o gráfico usando ggplot2
# Define o tema do gráfico
theme_defesa <- theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 10)
  )

# Defina uma paleta de cores adequada para um trabalho de defesa de mestrado
palette_cores <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")

# Plota o gráfico com as devidas customizações
p = ggplot(resumo_dados, aes(x = Estadio, y = media, fill = Estadio)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = media - erro_padrao, ymax = media + erro_padrao),
                width = 0.25, position = position_dodge(0.7)) +
  geom_text(aes(label = c("a", "b", "a"), y = media + erro_padrao), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4, color = "black") +
  labs(title = "",
       x = NULL, y = "Média de consumo foliar por estágio (cm²)",
       fill = "Estágios") +  # Alterando o nome da legenda
  scale_fill_manual(values = palette_cores) + 
  scale_y_continuous(limits = c(0, 35)) +# Usando a paleta de cores definida
  theme_defesa

print(p)

anova_novas = aov(Area_consumida~Estadio,dados_novas)
summary(anova_novas)

library(stats) # Existe diferenca se o zero nao esta no intervalo
TukeyHSD(anova_novas)

# Salva o gráfico como um arquivo PNG com resolução de 300 DPI
ggsave("grafico_novas.png", plot = p, width = 10, height = 8, dpi = 300)

####### FOLHAS VELHAS
## Entre as folhas velhas, verificar se existe preferencia alimentar por algum estadio:

dados_velhas = dados_original %>% filter(Idade_1=='Velha')

resumo_dados <- dados_velhas %>%
  group_by(Estadio) %>%
  summarise(
    media = mean(Area_consumida),
    erro_padrao = sd(Area_consumida) / sqrt(n())
  )

# Criando o gráfico usando ggplot2
# Define o tema do gráfico
theme_defesa <- theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 10) 
  )

# Defina uma paleta de cores adequada para um trabalho de defesa de mestrado
palette_cores <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")

# Plota o gráfico com as devidas customizações
p = ggplot(resumo_dados, aes(x = Estadio, y = media, fill = Estadio)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = media - erro_padrao, ymax = media + erro_padrao),
                width = 0.25, position = position_dodge(0.7)) +
  geom_text(aes(label = c("a", "b", "a"), y = media + erro_padrao), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4, color = "black") +
  labs(title = "",
       x = NULL, y = "Média de consumo foliar por estágio (cm²)",
       fill = "Estágios") +  # Alterando o nome da legenda
  scale_fill_manual(values = palette_cores) + 
  scale_y_continuous(limits = c(0, 4)) +# Usando a paleta de cores definida
  theme_defesa

print(p)

anova_velhas = aov(Area_consumida~Estadio,dados_velhas)
summary(anova_velhas)

# Existe diferenca se o zero nao esta no intervalo
TukeyHSD(anova_velhas)

# Salva o gráfico como um arquivo PNG com resolução de 300 DPI
ggsave("grafico_velhas.png", plot = p, width = 10, height = 8, units = "in", dpi = 300)

####### FOLHAS PRIMEIRA TRIFOLIOLADA
## Entre as folhas Primeira Trifoliolada, verificar se existe preferencia alimentar por algum estadio:

dados_primeira_trif = dados_original %>% filter(Idade_2=='Trifoliolada',
                                                Ordem=='Primeira',
                                                Estadio %in% c("R2",'V4'))

resumo_dados <- dados_primeira_trif %>%
  group_by(Estadio) %>%
  summarise(
    media = mean(Area_consumida),
    erro_padrao = sd(Area_consumida) / sqrt(n())
  )

# Criando o gráfico usando ggplot2
# Define o tema do gráfico
theme_defesa <- theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 10)
  )

# Defina uma paleta de cores adequada para um trabalho de defesa de mestrado
palette_cores <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")

# Plota o gráfico com as devidas customizações
p = ggplot(resumo_dados, aes(x = Estadio, y = media, fill = Estadio)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = media - erro_padrao, ymax = media + erro_padrao),
                width = 0.25, position = position_dodge(0.7)) +
  geom_text(aes(label = c("a", "b"), y = media + erro_padrao), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4, color = "black") +
  labs(title = "",
       x = NULL, y = "Média de consumo foliar por estágio (cm²)",
       fill = "Estágios") +  # Alterando o nome da legenda
  scale_fill_manual(values = palette_cores) + 
  scale_y_continuous(limits = c(0, 2)) +# Usando a paleta de cores definida
  theme_defesa

print(p)

anova_primeira_trif = aov(Area_consumida~Estadio,dados_primeira_trif)
summary(anova_primeira_trif)

# Existe diferenca se o zero nao esta no intervalo
TukeyHSD(anova_primeira_trif)

# Salva o gráfico como um arquivo PNG com resolução de 300 DPI
ggsave("grafico_primeira_trif.png", plot = p, width = 10, height = 8, units = "in", dpi = 300)


####### FOLHAS SEGUNDA TRIFOLIOLADA
## Entre as folhas Segunda Trifoliolada, verificar se existe preferencia alimentar por algum estadio:

dados_segunda_trif = dados_original %>% filter(Idade_2=='Trifoliolada',
                                                Ordem=='Segunda',
                                                Estadio %in% c("R2",'V4'))

resumo_dados <- dados_segunda_trif %>%
  group_by(Estadio) %>%
  summarise(
    media = mean(Area_consumida),
    erro_padrao = sd(Area_consumida) / sqrt(n())
  )

# Criando o gráfico usando ggplot2
# Define o tema do gráfico
theme_defesa <- theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 10)
  )

# Defina uma paleta de cores adequada para um trabalho de defesa de mestrado
palette_cores <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")

# Plota o gráfico com as devidas customizações
p = ggplot(resumo_dados, aes(x = Estadio, y = media, fill = Estadio)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = media - erro_padrao, ymax = media + erro_padrao),
                width = 0.25, position = position_dodge(0.7)) +
  geom_text(aes(label = c("a", "a"), y = media + erro_padrao), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4, color = "black") +
  labs(title = "",
       x = NULL, y = "Média de consumo foliar por estágio (cm²)",
       fill = "Estágios") +  # Alterando o nome da legenda
  scale_fill_manual(values = palette_cores) + 
  scale_y_continuous(limits = c(0, 2)) +# Usando a paleta de cores definida
  theme_defesa

print(p)

anova_segunda_trif = aov(Area_consumida~Estadio,dados_segunda_trif)
summary(anova_segunda_trif)

# Existe diferenca se o zero nao esta no intervalo
TukeyHSD(anova_segunda_trif)

# Salva o gráfico como um arquivo PNG com resolução de 300 DPI
ggsave("grafico_segunda_trif.png", plot = p, width = 10, height = 8, units = "in", dpi = 300)

####### FOLHAS V2
## No mesmo estadio V2, verificar se existe preferencia alimentar entre as diferentes idades:

dados_V2 = dados_original %>% filter(Estadio=='V2')

resumo_dados <- dados_V2 %>%
  group_by(Idade_1) %>%
  summarise(
    media = mean(Area_consumida),
    erro_padrao = sd(Area_consumida) / sqrt(n())
  )

# Criando o gráfico usando ggplot2
# Define o tema do gráfico
theme_defesa <- theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 10)
  )

# Defina uma paleta de cores adequada para um trabalho de defesa de mestrado
palette_cores <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")

# Plota o gráfico com as devidas customizações
p = ggplot(resumo_dados, aes(x = Idade_1, y = media, fill = Idade_1)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = media - erro_padrao, ymax = media + erro_padrao),
                width = 0.25, position = position_dodge(0.7)) +
  geom_text(aes(label = c("a", "b"), y = media + erro_padrao), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4, color = "black") +
  labs(title = "",
       x = NULL, y = "Média de consumo foliar por idade (cm²)",
       fill = "Idade") +  # Alterando o nome da legenda
  scale_fill_manual(values = palette_cores) + 
  scale_y_continuous(limits = c(0, 35)) +# Usando a paleta de cores definida
  theme_defesa

print(p)

anova_V2 = aov(Area_consumida~Idade_1,dados_V2)
summary(anova_V2)

# Salva o gráfico como um arquivo PNG com resolução de 300 DPI
ggsave("grafico_V2.png", plot = p, width = 10, height = 8, units = "in", dpi = 300)

####### FOLHAS V4
## No mesmo estadio V4, verificar se existe preferencia alimentar entre as diferentes idades:

dados_V4 = dados_original %>% filter(Estadio=='V4')

resumo_dados <- dados_V4 %>%
  group_by(Idade_1) %>%
  summarise(
    media = mean(Area_consumida),
    erro_padrao = sd(Area_consumida) / sqrt(n())
  )

# Criando o gráfico usando ggplot2
# Define o tema do gráfico
theme_defesa <- theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 10)
  )

# Defina uma paleta de cores adequada para um trabalho de defesa de mestrado
palette_cores <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")

# Plota o gráfico com as devidas customizações
p = ggplot(resumo_dados, aes(x = Idade_1, y = media, fill = Idade_1)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = media - erro_padrao, ymax = media + erro_padrao),
                width = 0.25, position = position_dodge(0.7)) +
  geom_text(aes(label = c("a", "a"), y = media + erro_padrao), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4, color = "black") +
  labs(title = "",
       x = NULL, y = "Média de consumo foliar por idade (cm²)",
       fill = "Idade") +  # Alterando o nome da legenda
  scale_fill_manual(values = palette_cores) + 
  scale_y_continuous(limits = c(0, 3)) +# Usando a paleta de cores definida
  theme_defesa

print(p)

anova_V4 = aov(Area_consumida~Idade_1,dados_V4)
summary(anova_V4)

# Salva o gráfico como um arquivo PNG com resolução de 300 DPI
ggsave("grafico_V4.png", plot = p, width = 10, height = 8, units = "in", dpi = 300)

####### FOLHAS R2
## No mesmo estadio R2, verificar se existe preferencia alimentar entre as diferentes idades:

dados_R2 = dados_original %>% filter(Estadio=='R2')

resumo_dados <- dados_R2 %>%
  group_by(Idade_1) %>%
  summarise(
    media = mean(Area_consumida),
    erro_padrao = sd(Area_consumida) / sqrt(n())
  )

# Criando o gráfico usando ggplot2
# Define o tema do gráfico
theme_defesa <- theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 10)
  )

# Defina uma paleta de cores adequada para um trabalho de defesa de mestrado
palette_cores <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")

# Plota o gráfico com as devidas customizações
p = ggplot(resumo_dados, aes(x = Idade_1, y = media, fill = Idade_1)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = media - erro_padrao, ymax = media + erro_padrao),
                width = 0.25, position = position_dodge(0.7)) +
  geom_text(aes(label = c("a", "a"), y = media + erro_padrao), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4, color = "black") +
  labs(title = "",
       x = NULL, y = "Média de consumo foliar por idade (cm²)",
       fill = "Idade") +  # Alterando o nome da legenda
  scale_fill_manual(values = palette_cores) + 
  scale_y_continuous(limits = c(0, 10)) +# Usando a paleta de cores definida
  theme_defesa

print(p)

anova_R2 = aov(Area_consumida~Idade_1,dados_R2)
summary(anova_R2)

# Salva o gráfico como um arquivo PNG com resolução de 300 DPI
ggsave("grafico_R2.png", plot = p, width = 10, height = 8, units = "in", dpi = 300)
