# Carregar os pacotes necessários
library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(scales)

# Definir o diretório de trabalho
setwd("D:\\aeae\\SHA (2010 - 2019)")


######################################### SOB A �TICA DO PRESTADOR ##########################
# Carregar os dados 

## prestador total ## 
prestador_total_2015_2019 <- read_xlsx("SHA prestador 2015-2019 por natureza.xlsx", sheet = 4)
prestador_total_2010_2014 <- read_xlsx("SHA prestador 2010-2014 por natureza.xlsx", sheet = 4)

## prestador municipal ## 
prestador_municipal_2015_2019 <- read_xlsx("SHA prestador 2015-2019 por natureza.xlsx", sheet = 3)
prestador_municipal_2010_2014 <- read_xlsx("SHA prestador 2010-2014 por natureza.xlsx", sheet = 3)

## prestador estadual ## 
prestador_estadual_2015_2019 <- read_xlsx("SHA prestador 2015-2019 por natureza.xlsx", sheet = 2)
prestador_estadual_2010_2014 <- read_xlsx("SHA prestador 2010-2014 por natureza.xlsx", sheet = 2)

## prestador federal ## 
prestador_federal_2015_2019 <- read_xlsx("SHA prestador 2015-2019 por natureza.xlsx", sheet = 1)
prestador_federal_2010_2014 <- read_xlsx("SHA prestador 2010-2014 por natureza.xlsx", sheet = 1)

# Remover a coluna 'QTD' que não é necessária
prestador_total_2015_2019 <- prestador_total_2015_2019 %>%
  select(-QTD)

prestador_municipal_2015_2019 <- prestador_municipal_2015_2019 %>%
  select(-QTD)

prestador_estadual_2015_2019 <- prestador_estadual_2015_2019 %>%
  select(-QTD)

prestador_federal_2015_2019 <- prestador_federal_2015_2019 %>%
  select(-QTD)

# Empilhar os dataframes de 2015-2019 e 2010-2014 (usando bind_rows)
prestador_total <- bind_rows(prestador_total_2015_2019, prestador_total_2010_2014)
prestador_municipal <- bind_rows(prestador_municipal_2015_2019, prestador_municipal_2010_2014)
prestador_estadual <- bind_rows(prestador_estadual_2015_2019, prestador_estadual_2010_2014)
prestador_federal <- bind_rows(prestador_federal_2015_2019, prestador_federal_2010_2014)

# Unir todos os dataframes em um único dataframe final
prestador_final <- bind_rows(prestador_total, prestador_municipal, prestador_estadual, prestador_federal)

# Identificar as categorias que aparecem em todos os anos (2010-2019)
categorias_completas <- prestador_final %>%
  group_by(Prestador) %>%
  filter(all(c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019) %in% Ano)) %>%
  ungroup() %>%
  distinct(Prestador)

# Filtrar o dataframe para incluir apenas as categorias
prestador_completo <- prestador_final %>%
  filter(Prestador %in% categorias_completas$Prestador) %>%
  mutate(Ano = factor(Ano, levels = 2010:2019))


grafico_prestador <- ggplot(prestador_completo %>% filter(Prestador != "N�o classificado"), 
                            aes(x = Ano, y = Valor, color = Prestador, group = Prestador)) +
  geom_line(size = 1.0) +  # Aumenta a espessura da linha
  geom_point() +
  facet_wrap(~`Ente Federativo`, scales = "free_y") +  # Separar por 'Ente Federativo'
  labs(title = "Evolu��o do Valor para Prestadores por Ente Federativo (2010-2019)",
       x = "Ano",
       y = "Valor") +
  scale_x_discrete(labels = as.character(2010:2019)) +  # Garante que o eixo 'x' seja discreto
  scale_y_log10(labels = scales::label_comma()) +  # Aplica a escala logar�tmica ao eixo Y
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotaciona as labels do eixo X
    strip.text = element_text(size = 10),  # Ajusta o tamanho do texto dos t�tulos das facetas
    panel.spacing = unit(1, "lines")  # Aumenta o espa�amento entre as facetas
  )

# Salvar o gr�fico como imagem PNG
ggsave("grafico_evolucao_prestadores.png", plot = grafico_prestador, width = 10, height = 8, dpi = 300)

# Gráfico de barras para comparar o valor total por ano entre os entes federativos (municipal, estadual, federal)
prestador_completo_soma <- prestador_completo %>%
  group_by(Ano, `Ente Federativo`) %>%
  summarise(Valor_Total = sum(Valor, na.rm = TRUE)) %>%
  ungroup()

ggplot(prestador_completo_soma, aes(x = factor(Ano), y = Valor_Total, fill = `Ente Federativo`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Valor Total por Ente Federativo por Ano (2010-2019)",
       x = "Ano",
       y = "Valor Total") +
  theme_minimal() +
  theme(legend.position = "bottom")


################################# SOB A OTICA DAS FUN��ES DE CUIDADO ######################


hc_2010_federal <- read_xlsx("GERAL SHA FUN��O 19_11_2016.xlsx", sheet = 1)
hc_2010_estadual <- read_xlsx("GERAL SHA FUN��O 19_11_2016.xlsx", sheet = 2)
hc_2010_municipal <- read_xlsx("GERAL SHA FUN��O 19_11_2016.xlsx", sheet = 3)
hc_2010_total <- read_xlsx("GERAL SHA FUN��O 19_11_2016.xlsx", sheet = 4)


hc_2015_federal <- read_xlsx("Gasto SIA+SIH_SHA_FUN�AO com fator por ano 2015 a 2019.xlsx", sheet = 1)
hc_2015_estadual <- read_xlsx("Gasto SIA+SIH_SHA_FUN�AO com fator por ano 2015 a 2019.xlsx", sheet = 2)
hc_2015_municipal <- read_xlsx("Gasto SIA+SIH_SHA_FUN�AO com fator por ano 2015 a 2019.xlsx", sheet = 3)
hc_2015_total <- read_xlsx("Gasto SIA+SIH_SHA_FUN�AO com fator por ano 2015 a 2019.xlsx", sheet = 4)

# Adicionar a coluna "Ente Federativo"
hc_2010_federal$Ente_Federativo <- "Federal"
hc_2010_estadual$Ente_Federativo <- "Estadual"
hc_2010_municipal$Ente_Federativo <- "Municipal"
hc_2010_total$Ente_Federativo <- "Total"

hc_2015_federal$Ente_Federativo <- "Federal"
hc_2015_estadual$Ente_Federativo <- "Estadual"
hc_2015_municipal$Ente_Federativo <- "Municipal"
hc_2015_total$Ente_Federativo <- "Total"

# Empilhar os dataframes
hc_2010_2015 <- bind_rows(hc_2010_federal, hc_2010_estadual, hc_2010_municipal, hc_2010_total,
                         hc_2015_federal, hc_2015_estadual, hc_2015_municipal, hc_2015_total)


funcoes_principais <- c("HC.1 - Aten��o curativa", "HC.2 - Atendimentos de reabilita��o", 
                        "HC.3 - Cuidados de longo prazo", "HC.4 - Atividades complementares ao diagn�stico e tratamento", 
                        "HC.5 - Medicamentos e outros produtos m�dicos", "HC.6 - Preven��o,promo��o e vigil�ncia em sa�de", 
                        "HC.7 - Gest�o e governan�a do sistema de sa�de", "HC.9 - Demais atividades de sa�de")

# Filtrar o dataframe para manter apenas as fun��es principais
hc_2010_2015_filtrado <- hc_2010_2015 %>%
  filter(`Fun��es de cuidado` %in% funcoes_principais)

# Agrupar por Fun��es de Cuidado, Ano e Ente Federativo e somar os valores
hc_2010_2015_agregado <- hc_2010_2015_filtrado %>%
  group_by(`Fun��es de cuidado`, Ano, Ente_Federativo) %>%
  summarise(Valor = sum(Valor, na.rm = TRUE)) %>%
  ungroup()


# Garantindo que a vari�vel 'Ano' seja tratada como fator
hc_2010_2015_agregado$Ano <- as.factor(hc_2010_2015_agregado$Ano)

shapes <- c("HC.1 - Aten��o curativa" = 16,  # C�rculo preenchido
            "HC.2 - Atendimentos de reabilita��o" = 17,  # Tri�ngulo preenchido
            "HC.3 - Cuidados de longo prazo" = 18,  # Quadrado preenchido
            "HC.4 - Atividades complementares ao diagn�stico e tratamento" = 15,  # Quadrado
            "HC.5 - Medicamentos e outros produtos m�dicos" = 19,  # C�rculo
            "HC.6 - Preven��o,promo��o e vigil�ncia em sa�de" = 8,  # Estrela
            "HC.7 - Gest�o e governan�a do sistema de sa�de" = 3,  # Tri�ngulo
            "HC.9 - Demais atividades de sa�de" = 4)  # Losango


ggplot(hc_2010_2015_agregado %>% filter(Ente_Federativo == "Total"), 
       aes(x = Ano, y = Valor, color = `Fun��es de cuidado`, shape = `Fun��es de cuidado`, group = `Fun��es de cuidado`)) +
  geom_line(size = 0.8) +  # Tornando as linhas mais finas
  geom_point(size = 3) +  # Adiciona pontos com diferentes marcadores
  labs(title = "Evolu��o das Fun��es de Cuidado ao Longo do Tempo (Total)", 
       x = "Ano", y = "Valor") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("HC.1 - Aten��o curativa" = "blue", 
                                "HC.2 - Atendimentos de reabilita��o" = "red", 
                                "HC.3 - Cuidados de longo prazo" = "green", 
                                "HC.4 - Atividades complementares ao diagn�stico e tratamento" = "purple", 
                                "HC.5 - Medicamentos e outros produtos m�dicos" = "orange", 
                                "HC.6 - Preven��o,promo��o e vigil�ncia em sa�de" = "brown", 
                                "HC.7 - Gest�o e governan�a do sistema de sa�de" = "pink", 
                                "HC.9 - Demais atividades de sa�de" = "gray")) +
  scale_shape_manual(values = shapes) +  # Aplica os diferentes marcadores
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 8),  # Reduz o tamanho da fonte da legenda
        legend.box.spacing = unit(1, "cm"),  # Ajusta ainda mais o espa�amento entre a legenda e o gr�fico
        plot.margin = margin(20, 20, 20, 20)) +
  scale_y_continuous(labels = label_number(scale = 1e-9, suffix = "B"))

ggplot(hc_2010_2015_agregado, 
       aes(x = Ano, y = Valor, fill = `Fun��es de cuidado`)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Ente_Federativo, scales = "free") +  # Divide por Ente Federativo e ajusta as escalas
  labs(title = "Compara��o das Fun��es de Cuidado entre os Entes Federativos (2010-2019)", 
       x = "Ano", y = "Valor") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Garante a rota��o do eixo X
    strip.text = element_text(size = 10)  # Ajusta o tamanho do texto das facetas
  ) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = label_number(scale = 1e-9, suffix = "B"))

ggplot(hc_2010_2015_agregado, 
       aes(x = Ano, y = Valor, color = Ente_Federativo, group = interaction(`Fun��es de cuidado`, Ente_Federativo))) +
  geom_line(size = 1) +  # Linhas mais grossas para melhorar a visualiza��o
  geom_point(size = 2) +  # Marcadores para os pontos
  labs(title = "Evolu��o do Gasto das Fun��es de Cuidado por Ente Federativo (2010-2019)", 
       x = "Ano", y = "Valor") +
  facet_wrap(~`Fun��es de cuidado`, scales = "free_y") +  # Divide por Fun��es de Cuidado
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Ajusta o eixo X
    legend.position = "bottom", 
    strip.text = element_text(size = 10),  # Ajusta o tamanho do t�tulo das facetas
    panel.spacing = unit(1, "lines"),  # Ajusta o espa�o entre as facetas
    axis.title.x = element_text(size = 12),  # Ajusta o tamanho do t�tulo do eixo X
    axis.text = element_text(size = 10)  # Ajusta o tamanho do texto dos eixos
  ) +
  scale_color_manual(values = c("Federal" = "blue", 
                                "Estadual" = "red", 
                                "Municipal" = "green", 
                                "Total" = "purple")) +
  scale_y_continuous(labels = label_number(scale = 1e-9, suffix = "B"))
