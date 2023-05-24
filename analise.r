library(rnaturalearth)
library(ggplot2)
library(dplyr)

# carregando banco de dados
dados <- read.csv("caminho",header=T)

# gerando o mapa
mapa <- ne_countries(scale = "medium", returnclass = "sf")

# Mesclando os dados com o mapa
dados_mapa <- merge(mapa, dados, by.x = "name", by.y = "Location", all.x = TRUE)

# Criando o mapa utilizando o ggplot
ggplot() +
  geom_sf(data = dados_mapa, aes(fill = Rate)) +
  scale_fill_gradient(low = "green", high = "red") +
  theme_void()+labs(fill="Porcentagem")

#Criando nova coluna com nº de homicidios intencionais
dados$intencional=round(dados$Count*dados$Rate/100,0)

#Contagem por regiao e subregiao
dados %>%
  group_by(Region) %>% #Subregion
  summarise(TotalCount = sum(Count))

dados %>%
  group_by(Region) %>% #Subregion
  summarise(TotalCount = sum(intencional))

#Criando novo dataframe para plotagem de grafico de homicidios totais e intencionais
regiao=c("America do Norte","America Central","America do Sul","Africa","Asia","Europa","Oceania")
homicidios=c(22317,47371,78872,99481,105552,19869,347)
intencionais=c(1417,13318,17859,19241,7446,1027,5)
df=data.frame(regiao,homicidios,intencionais)
df$Percentual <- (df$intencionais / df$homicidios) * 100


ggplot(df, aes(x = regiao)) +
  geom_col(aes(y = homicidios, fill = "Total de Homicídios"), width = 0.5) +
  geom_col(aes(y = intencionais, fill = "Homicídios Intencionais"), width = 0.5) +
  geom_text(aes(y = homicidios, label = homicidios), vjust = -0.5, color = "black", size = 3) +
  geom_text(aes(y = intencionais, label = paste0(round(Percentual, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "black", size = 3) +
  xlab("Continente") +
  ylab("Número de Homicídios") +
  scale_fill_manual(values = c("Total de Homicídios" = "blue", "Homicídios Intencionais" = "red")) +
  theme_minimal()+labs(fill="")+theme(axis.text.x=element_text(angle=45, hjust=0.9))

#Ordenando paises que tiveram maiores percentuais de homicidios intencionais
arrange(dados,desc(Rate))[1:30,]
