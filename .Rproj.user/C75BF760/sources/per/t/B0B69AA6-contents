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
  select(`Em.relação.aos.critérios.de.avaliação..qual.você.acharia.mais.interessante.`) %>%
  na.omit() %>% # Tira os NA
  count(`Em.relação.aos.critérios.de.avaliação..qual.você.acharia.mais.interessante.`, name = "count") %>%
  mutate(`Em.relação.aos.critérios.de.avaliação..qual.você.acharia.mais.interessante..`= as.factor(`Em.relação.aos.critérios.de.avaliação..qual.você.acharia.mais.interessante.`),
         fraction = count/sum(count),
         ymax = cumsum(count), ymin = c(0, head(ymax, n = -1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(`Em.relação.aos.critérios.de.avaliação..qual.você.acharia.mais.interessante.`, "\nfrequência: ", count)) %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = `Em.relação.aos.critérios.de.avaliação..qual.você.acharia.mais.interessante.`))+
  geom_rect()+
  geom_label(x = 3.5, aes(y = labelPosition, label = label), fill = "white", size = 4)+
  coord_polar(theta = "y")+
  xlim(c(2, 4))+
  theme_void(10)+
  scale_fill_manual(values = color_paleta(5))+
  labs(fill="Em relação aos critérios de avaliação, qual você acharia mais interessante:")+
  ggtitle("Critérios de Avaliação (Sem 1º Ano)")
ggsave("criterio-avaliacao-sem-primeiro-ano.png")


#### FIM SEM PRIMEIRO ANO #### 


# O PRIMEIRO ANO ###
dados %>%
  select(`Em.relação.aos.critérios.de.avaliação..qual.você.acharia.mais.interessante.`) %>%
  na.omit() %>% # Tira os NA
  count(`Em.relação.aos.critérios.de.avaliação..qual.você.acharia.mais.interessante.`, name = "count") %>%
  mutate(`Em.relação.aos.critérios.de.avaliação..qual.você.acharia.mais.interessante.`= as.factor(`Em.relação.aos.critérios.de.avaliação..qual.você.acharia.mais.interessante.`),
         fraction = count/sum(count),
         ymax = cumsum(count), ymin = c(0, head(ymax, n = -1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(`Em.relação.aos.critérios.de.avaliação..qual.você.acharia.mais.interessante.`, "\nfrequência: ", count)) %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = `Em.relação.aos.critérios.de.avaliação..qual.você.acharia.mais.interessante.`))+
  geom_rect()+
  geom_label(x = 3.5, aes(y = labelPosition, label = label), fill = "white", size = 4)+
  coord_polar(theta = "y")+
  xlim(c(2, 4))+
  theme_void(10)+
  scale_fill_manual(values = color_paleta(5))+
  labs(fill="Em relação aos critérios de avaliação, qual você acharia mais interessante:")+
  ggtitle("Critérios de avaliação")
ggsave("pergunta1.png",width = 9, height = 5, dpi = 700)

bdfiltro2 %>%
  select(`Em.relação.aos.critérios.de.avaliação..qual.você.acharia.mais.interessante.`) %>%
  na.omit() %>% # Tira os NA
  count(`Em.relação.aos.critérios.de.avaliação..qual.você.acharia.mais.interessante.`, name = "count") %>%
  mutate(`Em.relação.aos.critérios.de.avaliação..qual.você.acharia.mais.interessante.`= as.factor(`Em.relação.aos.critérios.de.avaliação..qual.você.acharia.mais.interessante.`),
         fraction = count/sum(count),
         ymax = cumsum(count), ymin = c(0, head(ymax, n = -1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(`Em.relação.aos.critérios.de.avaliação..qual.você.acharia.mais.interessante.`, "\nfrequência: ", count)) %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = `Em.relação.aos.critérios.de.avaliação..qual.você.acharia.mais.interessante.`))+
  geom_rect()+
  geom_label(x = 3.5, aes(y = labelPosition, label = label), fill = "white", size = 4)+
  coord_polar(theta = "y")+
  xlim(c(2, 4))+
  theme_void(10)+
  scale_fill_manual(values = color_paleta(5))+
  labs(fill="Em relação aos critérios de avaliação, qual você acharia mais interessante:")+
  ggtitle("Critérios de Avaliação (Apenas 1º Ano)")
ggsave("criterio-avaliacao-primeiro-ano.png")

####fo primeiro ano ###