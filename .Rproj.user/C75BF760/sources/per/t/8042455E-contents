install.packages("datapasta")
install.packages("reprex")
install.packages("ggplot2")
install.packages("ggplot")
library("ggplot2")
install.packages("dplyr")
library("dplyr")
library('readxl')
install.packages("colorspace")
install.packages("tidyverse")
library("tidyverse")



setwd('C:\\Users\\lucas-stefano\\Documents\\pesquisa-mercado\\pesquisa-mercado')
bd <- as_tibble(read.csv("pesquisademercado.csv"))

itens <-read.table("pergunta.csv",header=T,sep=";")
bdfiltro<-filter(bd, as.data.frame(bd[1]) != 1)
bdfiltro2<-filter(bd, as.data.frame(bd[1]) == 1)

library(readxl)

dados <- read.csv("pesquisademercado.csv")

color_paleta <- colorRampPalette(c("black","gray"))

#### SEM O PRIMEIRO ANO ####

bdfiltro %>%
  select(`Você.recomendaria.o.curso.para.outras.pessoas..`) %>%
  na.omit() %>% # Tira os NA
  count(`Você.recomendaria.o.curso.para.outras.pessoas..`, name = "count") %>%
  mutate(`Você.recomendaria.o.curso.para.outras.pessoas..`= as.factor(`Você.recomendaria.o.curso.para.outras.pessoas..`),
         fraction = count/sum(count),
         ymax = cumsum(count), ymin = c(0, head(ymax, n = -1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(`Você.recomendaria.o.curso.para.outras.pessoas..`, "\nfrequência: ", count)) %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = `Você.recomendaria.o.curso.para.outras.pessoas..`))+
  geom_rect()+
  geom_label(x = 3.5, aes(y = labelPosition, label = label), fill = "white", size = 4)+
  coord_polar(theta = "y")+
  xlim(c(2, 4))+
  theme_void(10)+
  scale_fill_manual(values = color_paleta(5))+
  labs(fill="Você recomendaria o curso para outras pessoas?")+
  ggtitle("Recomendaria o curso para outras pessoas? (Sem 1º Ano)")
ggsave("recomendaria-curso-sem-primeiro-ano.png", width = 5, height = 5, dpi = 700)


#### FIM SEM PRIMEIRO ANO #### 


# O PRIMEIRO ANO ###
dados %>%
  select(`Você.recomendaria.o.curso.para.outras.pessoas..`) %>%
  na.omit() %>% # Tira os NA
  count(`Você.recomendaria.o.curso.para.outras.pessoas..`, name = "count") %>%
  mutate(`Você.recomendaria.o.curso.para.outras.pessoas..`= as.factor(`Você.recomendaria.o.curso.para.outras.pessoas..`),
         fraction = count/sum(count),
         ymax = cumsum(count), ymin = c(0, head(ymax, n = -1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(`Você.recomendaria.o.curso.para.outras.pessoas..`, "\nfrequência: ", count)) %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = `Você.recomendaria.o.curso.para.outras.pessoas..`))+
  geom_rect()+
  geom_label(x = 3.5, aes(y = labelPosition, label = label), fill = "white", size = 4)+
  coord_polar(theta = "y")+
  xlim(c(2, 4))+
  theme_void(10)+
  scale_fill_manual(values = color_paleta(5))+
  labs(fill="Você recomendaria o curso para outras pessoas?")+
  ggtitle("Recomendaria o curso para outras pessoas?")
ggsave("pergunta1.png",width = 9, height = 5, dpi = 700)

bdfiltro2 %>%
  select(`Você.recomendaria.o.curso.para.outras.pessoas..`) %>%
  na.omit() %>% # Tira os NA
  count(`Você.recomendaria.o.curso.para.outras.pessoas..`, name = "count") %>%
  mutate(`Você.recomendaria.o.curso.para.outras.pessoas..`= as.factor(`Você.recomendaria.o.curso.para.outras.pessoas..`),
         fraction = count/sum(count),
         ymax = cumsum(count), ymin = c(0, head(ymax, n = -1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(`Você.recomendaria.o.curso.para.outras.pessoas..`, "\nfrequência: ", count)) %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = `Você.recomendaria.o.curso.para.outras.pessoas..`))+
  geom_rect()+
  geom_label(x = 3.5, aes(y = labelPosition, label = label), fill = "white", size = 4)+
  coord_polar(theta = "y")+
  xlim(c(2, 4))+
  theme_void(10)+
  scale_fill_manual(values = color_paleta(5))+
  labs(fill="Você recomendaria o curso para outras pessoas?")+
  ggtitle("Recomendaria o curso para outras pessoas? (Apenas 1º Ano)")
ggsave("recomendaria-curso-primeiro-ano.png", width = 5, height = 5, dpi = 700)

####fo primeiro ano ###