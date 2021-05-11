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



setwd('C:\\Users\\lucas-stefano\\Documents\\pesquisa-mercado')
bd <- as_tibble(read.csv("pesquisademercado.csv"))

itens <-read.table("pergunta.csv",header=T,sep=";")
bdfiltro<-filter(bd, as.data.frame(bd[1]) != 1)
bdfiltro2<-filter(bd, as.data.frame(bd[1]) == 1)

library(readxl)

dados <- read.csv("pesquisademercado.csv")

color_paleta <- colorRampPalette(c("black","gray"))


bdfiltro %>%
  select(`Qual.a.sua.pretenção.salarial.para.o.primeiro.emprego.na.área.`) %>%
  na.omit() %>% # Tira os NA
  count(`Qual.a.sua.pretenção.salarial.para.o.primeiro.emprego.na.área.`, name = "count") %>%
  mutate(`Qual.a.sua.pretenção.salarial.para.o.primeiro.emprego.na.área.`= as.factor(`Qual.a.sua.pretenção.salarial.para.o.primeiro.emprego.na.área.`),
         fraction = count/sum(count),
         ymax = cumsum(count), ymin = c(0, head(ymax, n = -1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(`Qual.a.sua.pretenção.salarial.para.o.primeiro.emprego.na.área.`, "\nfrequência: ", count)) %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = `Qual.a.sua.pretenção.salarial.para.o.primeiro.emprego.na.área.`))+
  geom_rect()+
  geom_label(x = 3.5, aes(y = labelPosition, label = label), fill = "white", size = 4)+
  coord_polar(theta = "y")+
  xlim(c(2, 4))+
  theme_void(10)+
  scale_fill_manual(values = color_paleta(5))+
  labs(fill="Qual a sua pretenção salarial para o primeiro emprego na área?")+
  ggtitle("Pretenção salarial para o primeiro emprego na área")
ggsave("pergunta1f.png",width = 5, height = 5, dpi = 700)

dados %>%
  select(`Em.qual.ano.você.está.matriculado.`) %>%
  na.omit() %>% # Tira os NA
  count(`Em.qual.ano.você.está.matriculado.`, name = "count") %>%
  mutate(`Em.qual.ano.você.está.matriculado.`= as.factor(`Em.qual.ano.você.está.matriculado.`),
         fraction = count/sum(count),
         ymax = cumsum(count), ymin = c(0, head(ymax, n = -1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(`Em.qual.ano.você.está.matriculado.`, "\nfrequência: ", count)) %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = `Em.qual.ano.você.está.matriculado.`))+
  geom_rect()+
  geom_label(x = 3.5, aes(y = labelPosition, label = label), fill = "white", size = 4)+
  coord_polar(theta = "y")+
  xlim(c(2, 4))+
  theme_void(10)+
  scale_fill_manual(values = color_paleta(5))+
  labs(fill="Ano em que está matriculado:")+
  ggtitle("Ano de Matricula")
ggsave("pergunta1.png",width = 9, height = 5, dpi = 700)

bdfiltro2 %>%
  select(`Em.qual.ano.você.está.matriculado.`) %>%
  na.omit() %>% # Tira os NA
  count(`Em.qual.ano.você.está.matriculado.`, name = "count") %>%
  mutate(`Em.qual.ano.você.está.matriculado.`= as.factor(`Em.qual.ano.você.está.matriculado.`),
         fraction = count/sum(count),
         ymax = cumsum(count), ymin = c(0, head(ymax, n = -1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(`Em.qual.ano.você.está.matriculado.`, "\nfrequência: ", count)) %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = `Em.qual.ano.você.está.matriculado.`))+
  geom_rect()+
  geom_label(x = 3.5, aes(y = labelPosition, label = label), fill = "white", size = 4)+
  coord_polar(theta = "y")+
  xlim(c(2, 4))+
  theme_void(10)+
  scale_fill_manual(values = color_paleta(5))+
  labs(fill="Ano em que está matriculado:")+
  ggtitle("Ano de Matricula")
ggsave("pergunta1f2.png",width = 5, height = 5, dpi = 700)