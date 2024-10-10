# Cargado de las librerías --------------------

library(tidyverse); library(sciplot)

# Cargando la database -----------------
setwd("/DATOS/R/Encuentro")
setwd("D:/R/Encuentro")
data <- read_csv2("producción soja.csv")

# Gráfico de puntos -----

# Primer gráfico básico
G1 <- ggplot(data = data,
       aes(x= Year, y = Yield)) +
  geom_point() ; G1

# Los geom agregar formas gráficas.
G1 + geom_line()
G1 + geom_smooth()
G1 + geom_smooth(method = "lm")

# scales permite establecer los valores de los ejes

G1 <- G1+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 35000),
                     breaks = seq(0, 50000, by = 5000))+
  scale_x_continuous(breaks = seq(1960,2016, by=5)) ; G1

# labs establece los títulos de ejes (y leyenda)
G1 <- G1+
  labs(x= "Año",
       y = "Rendimiento (Tn/ha)") ; G1

# coloreando diferentes factores
G2 <- ggplot(data = data,
       aes(x= Year, y = Yield,
           col = Area)) +
  geom_point() ; G2

# podemos agregar ejes y modificar leyenda
G2 <- G2+
  scale_y_continuous(expand = c(0,0), limits = c(0, 35000), breaks = seq(0, 50000, by = 5000))+
  scale_x_continuous(breaks = seq(1960,2016, by=5)) +
  labs(x= "Año",
       y = expression(paste("Rendimiento (Tn ",ha^-1, ")")),
       col = "Región") ; G2

# También se puede dividir la ventana del gráfico, con un facet
G3 <- ggplot(data = data,
       aes(x= Year, y = Yield)) +
  geom_point() +
  facet_grid(.~Area)+
  scale_y_continuous(expand = c(0,0), limits = c(0, 35000), breaks = seq(0, 50000, by = 5000))+
  scale_x_continuous(breaks = seq(1960,2016, by=5)) +
  labs(x= "Año",
       y = expression(paste("Rendimiento (Tn ",ha^-1, ")"))) ; G3


# Podemos cambiar el tamaño, color, forma, etc de los puntos dentro del geom
ggplot(data = data,
       aes(x= Year, y = Yield)) +
  geom_point(size=3, col= c("aquamarine4"), shape=18)

# Si colocamos el parámetro en el aes, genera una variación por ese factor, lo que 
# genera una leyenda
ggplot(data = data,
       aes(x= Year, y = Yield, size=Year)) +
  geom_point(col= c("aquamarine4"), shape=18)

ggplot(data = data,
       aes(x= Year, y = Yield, shape=Area)) +
  geom_point(size= 3, col= c("aquamarine4"))

ggplot(data = data,
       aes(x= Year, y = Yield, col= Area)) +
  geom_point(size=3, shape=18)

# se pueden combinar
ggplot(data = data,
       aes(x= Year, y = Yield, col= Area, shape=Area)) +
  geom_point(size=3)

# Podemos elegir manualmente esa variación con scale_colour_manual, scale_shape_manual, etc
# Por ejemplo en el último caso
G4 <- ggplot(data = data,
       aes(x= Year, y = Yield, col= Area)) +
  geom_point(size=3)+
  scale_color_manual(values = c("skyblue4","red"), 
                     labels=c("Argentina", "Mundo")) + # también modificamos la etiquetas de la leyenda
  scale_y_continuous(expand = c(0,0), limits = c(0, 35000), breaks = seq(0, 50000, by = 5000))+
  scale_x_continuous(breaks = seq(1960,2016, by=5)) +
  labs(x= "Año",
       y = expression(paste("Rendimiento (Tn  ",ha^-1, ")"))); G4 

# se puede setear el aspecto general del gráfico con theme_
G4 <- G4 + theme_bw() ; G4

# dentro del parámetro theme() se ajustan muchas variables estéticas, por ejemplo
G4 <- G4 +
  theme(text = element_text(size=12, colour = "black"),
        axis.text = element_text(size=12 , colour = "black"),
        legend.text = element_text(size=12, colour = "black"),
        legend.background=element_rect(colour="black"),
        axis.line = element_line(colour = "black")) ; G4

# Gráficos de cajas ---
ggplot(data = data,
       aes(x= Area, y = Yield)) +
  geom_boxplot()

# Podemos agregar el extremo
ggplot(data = data,
       aes(x= Area, y = Yield)) +
  stat_boxplot(geom ='errorbar', width=0.1)+ 
  geom_boxplot()


# Gráficos de columnas ----
# Siguen la misma lógica que los gráficos de puntos, solo cambian algunos detalles.
# El geom a utilizar es geom_col
data_96 <- data %>% filter(data$Year>1995)

ggplot(data = data_96,
       aes(x= Year, y = Yield)) +
  geom_col()

# Podemos dividir por un factor, pero el parámetro de relleno es fill. Col cambia el color
# del contorno
ggplot(data = data_96,
       aes(x= Year, y = Yield, fill= Area)) +
  geom_col(col= "black")

# Por defecto las barras se muestran apiladas, si las queremos al lado debemos cambiar el position
G5 <- ggplot(data = data_96,
       aes(x= Year, y = Yield, fill= Area)) +
  geom_col(col= "black", position = "dodge")+
  scale_fill_manual(values = c("paleturquoise4","cornsilk2"), 
                    labels=c("Argentina", "Mundo")); G5

# agregarmos los ejes acomodados
G5 <- G5 +
  scale_y_continuous(expand = c(0,0), limits = c(0, 35000), breaks = seq(0, 50000, by = 5000))+
  scale_x_continuous(expand = c(0,0.2), breaks = seq(1995,2016, by=2)) +
  labs(x= "Año",
       y = expression(paste("Rendimiento (Tn  ", ha^-1, ")")))+
  theme_bw()+
  theme(text = element_text(size=12, colour = "black"),
        axis.text = element_text(size=12 , colour = "black"),
        legend.text = element_text(size=12, colour = "black"),
        legend.background=element_rect(colour="black"),
        axis.line = element_line(colour = "black")) ; G5

# cambiamos la inclinación del texto en el eje
G5 + scale_x_continuous(expand = c(0,0.2), breaks = seq(1995,2016, by=1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Transformando datos para graficar medias y desvíos
G6 <- data_96 %>%  select(Area, Yield) %>% 
group_by(Area) %>% 
  summarise_all(funs(mean, se, sd)) %>% #las graficamos directamente
  ggplot(aes(Area, mean))+
  geom_col(fill=c("paleturquoise4","cornsilk2"), col="black")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2)+
  scale_y_continuous(expand = c(0,0), limits = c(0, 35000), breaks = seq(0, 50000, by = 5000))+
  scale_x_discrete(labels=c("Argentina", "Mundo"))+
  labs(x= "Región",
       y = expression(paste("Rendimiento (Tn   ",ha^-1, ")")))+
  theme_bw()+
  theme(text = element_text(size=12, colour = "black"),
        axis.text = element_text(size=12 , colour = "black"),
        legend.text = element_text(size=12, colour = "black"),
        legend.background=element_rect(colour="black"),
        axis.line = element_line(colour = "black")) ; G6

# También podemos agregar letras de análisis estadísticos  
G6 + geom_text(aes(y=mean+se,
                   label= c("a", "b")),
               vjust=-0.8, size=5)


# Combinando diferentes tipos de gráficos -----
# Una buena forma de visualizar algunas variables es superponiendo diferentes tipos
# de gráficos. Supongamos una vez más que queremos comparar el rendimiento en Argentina y el mundo
# por años en los últimos 20 años. Para esto ya habíamos generado el gráfico G5, veamos otra opción.
# Vamos a generar una base de datos para cada región:
data_argentina <- data %>% filter(data$Area == "Argentina")
data_mundo <- data %>% filter(data$Area != "Argentina")

# Luego vamos a superponer un gráfico base de columnas de Argentina
ggplot(data_argentina, aes(Year, Yield))+
  geom_col()

# con uno de líneas y puntos del mundo
ggplot(data_mundo, aes(Year, Yield))+
  geom_line()+
  geom_point()

# El resultado es así
G7 <- ggplot(data_argentina, aes(Year, Yield))+
  geom_col(fill="skyblue3")+
  geom_line(data = data_mundo, aes(Year, Yield), col= "red")+
  geom_point(data = data_mundo, aes(Year, Yield), col= "red", size=2) ; G7

# Podemos agregar algunos aspectos estéticos, copiando y pegando de antes
G7 <- G7 +
  scale_y_continuous(expand = c(0,0), limits = c(0, 35000), breaks = seq(0, 50000, by = 5000))+
  scale_x_continuous(expand = c(0,0.2), breaks = seq(1960,2016, by=5)) +
  labs(x= "Año",
       y = expression(paste("Rendimiento (Tn  ",ha^-1, ")")))+
  theme_bw()+
  theme(text = element_text(size=12, colour = "black"),
        axis.text = element_text(size=12 , colour = "black"),
        legend.text = element_text(size=12, colour = "black"),
        legend.background=element_rect(colour="black"),
        axis.line = element_line(colour = "black")) ; G7

# Falta lograr identificar en el gráfico las dos bases de datos. Agregamos texto aclarando
G7 <- G7 +
  annotate("text", x=1990, y= 25000, label = "Argentina", col="skyblue4", size=5)+
  annotate("text", x=1965, y= 16000, label = "Mundo", col="red", size=5) ; G7


# División de la ventana gráfica: patchwork ------

# Podemos combinar diferentes gráficos en uno gracias al paquete patchwork
library(patchwork)

# Podemos sencillamente escribir y sumar dos gráficos
G1 + G2

G3 + G4 / G5

G7 / (G5 + G6)

wrap_plots(G7 + ggtitle("(a)") +
             G6 + ggtitle("(b)"))

wrap_plots(G7 + ggtitle("(a)") , 
           G6 +  ggtitle("(b)"), 
           ncol = 2, 
           widths = c(3,1))
  

wrap_plots(G7, 
           G6,
           ncol = 2, 
           widths = c(3,1)) &
  plot_annotation(tag_levels = 'a')

G8 <- wrap_plots(G7, 
           G6 +
             theme(axis.title.y = element_blank(),
                   axis.text.y = element_blank()), 
           ncol = 2, 
           widths = c(3,1)) &
  plot_annotation(tag_levels = 'a') ; G8

  
# Exportación --------
ggsave("Gráfico 8.png")

ggsave("Gráfico 8.pdf", width = 10, height = 8, dpi= 600)

svg("Gráfico 8.svg", width = 12, height = 10)
G9
dev.off()