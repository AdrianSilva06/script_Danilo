# ---
# TÍTULO: Script para Análise de Diversidade de Insetos e Heterogeneidade de Vegetação
#
# DESCRIÇÃO: Este script compara métricas de diversidade taxonômica em 
# diferentes níveis de heterogeneidade da vegetação entre habitats aquáticos e terrestres.
# ---

# ---
# 1. CARREGAR PACOTES NECESSÁRIOS
# ---
library(dplyr)
library(vegan)
library(ggplot2)

# ---
# 2. SIMULAÇÃO DE DADOS (SUBSTITUA PELOS SEUS DADOS REAIS)
# ---
set.seed(42) # Para reprodutibilidade da simulação

# 2.1. DADOS AMBIENTAIS (Vegetação e Habitat)
dados_ambientais <- data.frame(
  SampleID = paste0("Amostra_", 1:16),
  Ponto = rep(1:8, each = 2),
  Habitat = rep(c("Aquatico", "Terrestre"), times = 8)
)

dados_ambientais <- dados_ambientais %>%
  mutate(
    Herbacea = ifelse(Habitat == "Terrestre", sample(c(TRUE, FALSE), 16, replace = TRUE), NA),
    Arbustiva = ifelse(Habitat == "Terrestre", sample(c(TRUE, FALSE), 16, replace = TRUE), NA),
    Emersa = ifelse(Habitat == "Aquatico", sample(c(TRUE, FALSE), 16, replace = TRUE), NA),
    Flutuante = ifelse(Habitat == "Aquatico", sample(c(TRUE, FALSE), 16, replace = TRUE), NA),
    Anfibia = ifelse(Habitat == "Aquatico", sample(c(TRUE, FALSE), 16, replace = TRUE), NA)
  )

# 2.2. DADOS DE ESPÉCIES (Contagem de Insetos)
n_amostras <- 16
n_especies <- 15
nomes_especies <- paste0("Especie_", LETTERS[1:n_especies])

dados_especies_matrix <- matrix(
  rpois(n_amostras * n_especies, lambda = 3), 
  nrow = n_amostras, 
  dimnames = list(dados_ambientais$SampleID, nomes_especies)
)

# ---
# 3. CLASSIFICAR HETEROGENEIDADE DA VEGETAÇÃO
# ---
dados_ambientais <- dados_ambientais %>%
  mutate(
    Nivel_Heterogeneidade = case_when(
      Habitat == "Terrestre" & Herbacea & Arbustiva ~ "MUITO HETEROGÊNEO",
      Habitat == "Terrestre" & (Herbacea | Arbustiva) ~ "HETEROGÊNEO",
      Habitat == "Terrestre" & !Herbacea & !Arbustiva ~ "HOMOGÊNEO",
      Habitat == "Aquatico" & Emersa & Flutuante & Anfibia ~ "MUITO HETEROGÊNEO",
      Habitat == "Aquatico" & (Emersa & Flutuante) | (Emersa & Anfibia) | (Flutuante & Anfibia) ~ "HETEROGÊNEO",
      Habitat == "Aquatico" & (Emersa | Flutuante | Anfibia) ~ "POUCO HETEROGÊNEO",
      Habitat == "Aquatico" & !Emersa & !Flutuante & !Anfibia ~ "HOMOGÊNEO",
      TRUE ~ NA_character_
    )
  )

# Converter a heterogeneidade para um fator ordenado
niveis_ordem <- c("HOMOGÊNEO", "POUCO HETEROGÊNEO", "HETEROGÊNEO", "MUITO HETEROGÊNEO")
dados_ambientais$Nivel_Heterogeneidade <- factor(
  dados_ambientais$Nivel_Heterogeneidade,
  levels = niveis_ordem,
  ordered = TRUE
)

# Visualizar o resultado da classificação
print("Classificação da Heterogeneidade:")
print(table(dados_ambientais$Habitat, dados_ambientais$Nivel_Heterogeneidade, useNA = "ifany"))

# ---
# 4. CALCULAR MÉTRICAS DE DIVERSIDADE
# ---
# Riqueza (S): Número de espécies por amostra
dados_ambientais$Riqueza <- specnumber(dados_especies_matrix)

# Diversidade de Shannon (H')
dados_ambientais$Diversidade_Shannon <- diversity(dados_especies_matrix, index = "shannon")

# Ver o data.frame final com os cálculos
head(dados_ambientais)

#Escrever um arquivo csv
write.csv(dados_ambientais, "dados_ambientais_com_metricas.csv", row.names = FALSE)

# ---
# 5. ANÁLISE E VISUALIZAÇÃO
# ---
grafico <- ggplot(dados_ambientais, aes(x = Habitat, y = Diversidade_Shannon, fill = Habitat)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.8) +
  labs(
    title = "Comparação da Diversidade de Insetos (Shannon) entre Habitats",
    x = "Tipo de Habitat",
    y = "Índice de Diversidade de Shannon (H')"
  ) +
  theme_classic() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = "none")

# Salvar o gráfico em um arquivo (ex: PNG)
ggsave("grafico_diversidade_shannon.jpg", plot = grafico, width = 8, height = 6, units = "in", dpi = 300)

