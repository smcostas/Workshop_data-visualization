library(tidyverse); library(sciplot); 
library(patchwork); library(viridis); library(RColorBrewer)

# Cargando la base de datos
data <- read_csv2("producción soja.csv")
data$Yield <- data$Yield/10000

# 1. Primeros Gráficos
## 1.1. Datos y Mapping  
G1 <- ggplot(data = data,
             mapping = aes(x= Year, y = Yield))  ; G1

## 1.2. Geom
G1 + geom_point()
G1 + geom_point() + geom_line()
G1 + geom_point() + geom_smooth(method = 'lm')
G1 + geom_point() + geom_smooth()

G1 + geom_point() + geom_smooth() + G1 + geom_smooth() + geom_point() #El orden importa

## 1.3. Scales
### Escalas continuas

G1 <- G1 + geom_point() ## Simplemente guardamos el objeto para lo que viene

G1 +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 3.5),
                     breaks = seq(0, 3.5, by = .5))+
  scale_x_continuous(breaks = seq(1960,2016, by=5))

G1 <- G1 +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 3.5),
                     breaks = seq(0, 3.5, by = .5),
                     labels=function(x) format(x, decimal.mark =",", scientific = FALSE))+
  scale_x_continuous(breaks = seq(1960,2016, by=5)) ; G1

G1+ G1 +
  scale_y_continuous(expand = c(0,0),
                     limits = c(1, 3.5),
                     breaks = seq(0, 3.5, by = .5))+
  scale_x_continuous(breaks = seq(1960,2016, by=5))

G1 <- G1+
  labs(x= "Año",
       y = "Rendimiento (Tn/ha)") ; G1 # labs establece los títulos de ejes (y leyenda)

### Modificando las escalas de color, forma y tamaño los geoms

ggplot(data = data,
       aes(x= Year, y = Yield)) +
  geom_point()

ggplot(data = data,
       aes(x= Year, y = Yield)) +
  geom_point(col="aquamarine4", shape=18, size= 5)

# 2. Gráficos con 2 variables explicativas
## 2.1. Clasificando por factores

### Color 
G2 <- ggplot(data = data,
             mapping = aes(x= Year, y = Yield,
                           color = Area)) +
  geom_point() ; G2

G2 <- G2+
  scale_y_continuous(expand = c(0,0), limits = c(0, 3.5), breaks = seq(0, 5, by = .5))+
  scale_x_continuous(breaks = seq(1960,2016, by=5)) +
  labs(x= "Año",
       y = expression(paste("Rendimiento (Tn ",ha^-1, ")")),
       col = "Región") ; G2

### Tamaño    
ggplot(data = data,
       mapping = aes(x= Year, y = Yield,
                     size = Year)) +
  geom_point(show.legend = F) ## se puede eliminar la leyenda utilizando este codigo

## Forma
ggplot(data = data,
       mapping = aes(x= Year, y = Yield,
                     shape = Area)) +
  geom_point()

### Combinando
ggplot(data = data,
       mapping = aes(x= Year, y = Yield,
                     shape = Area, col = Area)) +
  geom_point()


## 2.2. Modificando las escalas correspondientes a las variables de clasificación
### Ajuste manual
G3 <- ggplot(data = data,
             aes(x= Year, y = Yield, col= Area)) +
  geom_point(size=3)+
  scale_color_manual(values = c("skyblue4","red"), 
                     labels=c("Argentina", "Mundo")) + # etiquetas de la leyenda
  scale_y_continuous(expand = c(0,0), limits = c(0, 3.5), breaks = seq(0, 5, by = .5))+
  scale_x_continuous(breaks = seq(1960,2016, by=5)) +
  labs(x = "Año",
       y = expression(paste("Rendimiento (Tn","  ", ha^-1 , ")")),
       col = "Área"); G3 

### Utilizando paletas de color de forma automática
## Viridis
ggplot(data = data,
       aes(x= Year, y = Yield, col= Area)) +
  geom_point(size=3) +
  scale_color_viridis_d()

## Rcolorbrewer
ggplot(data = data,
       aes(x= Year, y = Yield, col= Area)) +
  geom_point(size=3) + scale_color_brewer(type = 'qual', palette = 2)

## 2.3. Facets

G4 <- ggplot(data = data,
             aes(x= Year, y = Yield)) +
  geom_point() +
  facet_grid(.~Area)+
  scale_y_continuous(expand = c(0,0), limits = c(0, 3.5), breaks = seq(0, 5, by = .5))+
  scale_x_continuous(breaks = seq(1960,2016, by=10)) +
  labs(x= "Año",
       y = expression(paste("Rendimiento (Tn ",ha^-1, ")"))) ; G4

## 2.4. Tema o Theme
###  Temas predeterminados

G3 + theme_bw()
G3 + theme_classic()

### Edición manual del tema

G3 <- G3 +
theme(text = element_text(size=12, colour = "black"),
      axis.text = element_text(size=12 , colour = "black"),
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      legend.text = element_text(size=12, colour = "black"),
      legend.background=element_rect(colour="black"),
      axis.line = element_line(colour = "black")) ; G3

#### Ejemplo
MICHAEL <-   theme(
  legend.position = "bottom", legend.title = element_text(colour = "red", size = 10),
  legend.background = element_rect(fill = "black", colour = 'red'),
  legend.key = element_rect(fill = "black", colour = "white"),
  legend.text = element_text(colour = "white", size = 10),
  plot.background = element_rect(fill = "black", colour = "white"),
  panel.background = element_rect(fill = "black"),
  #  panel.background = element_rect(fill = "white"),
  axis.text = element_text(colour = "white"),
  plot.title = element_text(colour = "red", face = "bold", size = 18, vjust = 1, 
                            hjust = 0.5),
  axis.title = element_text(colour = "red", face = "bold", size = 13),
  panel.grid.major.y = element_line(colour = "white"),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  strip.text = element_text(colour = "white"),
  strip.background = element_rect(fill = "white"),
  axis.ticks = element_line(colour = "red")
)


G3 + scale_color_manual(values = c('red','white'), labels = c('Argentina', 'Mundo')) + 
  ggtitle('Jordan theme') + MICHAEL

# 3. Otros gráficos con los mismos datos
## Boxplot
ggplot(data = data,
       aes(x= Area, y = Yield)) +
  geom_boxplot()

ggplot(data = data,###Podemos agregar el extremo y la media (el orden importa)
       aes(x= Area, y = Yield)) +
  stat_boxplot(geom ='errorbar', width=0.1)+ 
  geom_boxplot()+
  stat_summary(fun=mean, geom="point", shape=16, size=2.5, colour= "red")

## Columnas

data_96 <- data %>% filter(data$Year>1995) ## filtramos los años mayores a 1995

ggplot(data = data_96,
       aes(x= Year, y = Yield)) +
  geom_col()

### Clasificando por variables
G5 <- ggplot(data = data_96,
             aes(x= Year, y = Yield, fill= Area)) +
  geom_col(col= "black")+
  scale_fill_manual(values = c("paleturquoise4","cornsilk2"), 
                    labels=c("Argentina", "Mundo")); G5
G5 <- ggplot(data = data_96,
             aes(x= Year, y = Yield, fill= Area)) +
  geom_col(col= "black", position = 'dodge')+
  scale_fill_manual(values = c("paleturquoise4","cornsilk2"), 
                    labels=c("Argentina", "Mundo")); G5

### Incorporando cuestiones de escala y estéticas vistas anteriormente,
### mejoramos nuestro gráfico:
G5 <- G5 +
  scale_y_continuous(expand = c(0,0), limits = c(0, 3.5), breaks = seq(0, 5, by = .5))+
  scale_x_continuous(expand = c(0,0.2), breaks = seq(1995,2016, by=2)) +
  labs(x= "Año",
       y = expression(paste("Rendimiento (Tn","  ", ha^-1, ")")))+
  theme_bw()+
  theme(text = element_text(size=12, colour = "black"),
        axis.text = element_text(size=12 , colour = "black"),
        legend.text = element_text(size=12, colour = "black"),
        legend.background=element_rect(colour="black"),
        axis.line = element_line(colour = "black")) ; G5

# 4. Un poquito más de ggplot2 y un poquito más allá dentro de tidyverse
## 4.1. Transformando los datos para graficar medias y desvíos

G6 <- data_96 %>%  select(Area, Yield) %>% # seleccionamos la variables de interés 
  group_by(Area) %>%  # marcamos las variables que agrupan
  summarise_all(funs(mean, se, sd)) %>% # calculamos los parámetros
  ggplot(aes(Area, mean))+ # y las graficamos directamente
  geom_col(fill=c("paleturquoise4","cornsilk2"), col="black")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2)+
  scale_y_continuous(expand = c(0,0), limits = c(0, 3.5), breaks = seq(0, 5, by = .5))+
  scale_x_discrete(labels=c("Argentina", "Mundo"))+
  labs(x= "Región",
       y = expression(paste("Rendimiento (Tn   ",ha^-1, ")")))+
  theme_bw()+
  theme(text = element_text(size=12, colour = "black"),
        axis.text = element_text(size=12 , colour = "black"),
        legend.text = element_text(size=12, colour = "black"),
        legend.background=element_rect(colour="black"),
        axis.line = element_line(colour = "black")) ; G6

## 4.2. Agregando texto
G6 + geom_text(mapping = aes(y=mean+se, label= c("a", "b")),
               vjust=-0.8, size=5)

## 4.3 Combinando representaciones (geoms)

### Generamos una base de datos para cada región usando *dplyr*
data_argentina <- data %>% filter(data$Area == "Argentina")
data_mundo <- data %>% filter(data$Area != "Argentina")

### Luego vamos a superponer un gráfico base de columnas de Argentina
### y uno de puntos y de lineas del mundo

G7 <- ggplot(data_argentina, aes(Year, Yield)) +
  geom_col()
G8 <- ggplot(data_mundo, aes(Year, Yield)) +
  geom_line() +
  geom_point()
G7 + G8

### ¿Cómo los combinamos? Cambiando los **datos** y el **mapping** dentro de los geoms,
### es decir no utilizando el global para uno de ellos. Veamos

G7 <- ggplot(data = data_argentina, mapping = aes(Year, Yield)) + ## funcion global 
  geom_col(fill="skyblue3") + ## para el geom_col utilizamos los datos globales
  geom_line(data = data_mundo, aes(Year, Yield), col= "red")+ ## cambiamos los datos
  geom_point(data = data_mundo, aes(Year, Yield), col= "red", size=2) ; G7 ## igual que antes

### Copiando y pegando lo previo podemos mejorar la estética de nuestro gráfico
G7 <- G7 +
  scale_y_continuous(expand = c(0,0), limits = c(0, 3.5, breaks = seq(0, 5, by = .5))) +
  scale_x_continuous(expand = c(0,0.2), breaks = seq(1960,2016, by=5)) +
  labs(x= "Año",
       y = expression(paste("Rendimiento (Tn  ",ha^-1, ")")))+
  theme_bw()+
  theme(text = element_text(size=12, colour = "black"),
        axis.text = element_text(size=12 , colour = "black"),
        legend.text = element_text(size=12, colour = "black"),
        legend.background=element_rect(colour="black"),
        axis.line = element_line(colour = "black")) ; G7

### Falta lograr identificar en el gráfico las dos bases de datos.
### Agregamos texto aclarando
G7 <- G7 +
  annotate("text", x=1990, y= 2.5, label = "Argentina", col="skyblue3", size=5)+
  annotate("text", x=1965, y= 1.6, label = "Mundo", col="red", size=5) ; G7

# 5. Dividiendo la pantalla gráfica para mostrar más de un gráfico
## 5.1. Algunos ejemplos
G1 + G2 ## en dos columnas

G3 + G4 / G5 # grafico G3 en 1 columna y 2 filas, G4 y G5 en 1 fila y 1 columna 

G7 / (G5 + G6) # G7 en 1 fila y 2 columnas, G5 y G6 en 1 fila y 1 columna

## 5.2. Dividiendo la pantalla gráfica y agregando referencia a los gráficos

wrap_plots(G7 + ggtitle("(a)") +
             G6 + ggtitle("(b)")) #agregamos etiquetas manualmente

wrap_plots(G7 + ggtitle("(a)") , 
           G6 +  ggtitle("(b)"), 
           ncol = 2, 
           widths = c(3,1)) #relativizamos los anchos

wrap_plots(G7, 
           G6,
           ncol = 2, 
           widths = c(3,1)) &
  plot_annotation(tag_levels = 'a')  # etiquetas automáticas

G8 <- wrap_plots(G7, 
                 G6 +
                   theme(axis.title.y = element_blank(), # eliminamos un eje repetido
                         axis.text.y = element_blank()), 
                 ncol = 2, 
                 widths = c(3,1)) &
  plot_annotation(tag_levels = 'a') ; G8

# 7. Exportando los gráficos
## 7.1. Utilizando *ggplot2*
ggsave("Gráfico 8.png") # exporta tal cual se ve en la ventana Plots
ggsave("Gráfico 8.pdf", width = 10, height = 8, dpi= 600) # esta es el que usaremos
### en inkscape

## 7.2. Utilizando **R base**
svg("Gráfico 8.svg", width = 12, height = 10)
G8
dev.off()

