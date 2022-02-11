library(tidyr)
library(gapminder)
library(dplyr)
library(ggplot2)
library(ggcorrplot)

#1
datos <- gapminder %>% 
  mutate(gapminder, pib = pop*gdpPercap)

#2
head(arrange(datos, desc(pib)))
#Estados Unidos en los años 2007, 2002, 1997, 1992, 1987 y 1982. 


#3
paises <- select(datos,  year, country, lifeExp)
paises <- filter(paises, country %in% c("Costa Rica", "Uruguay"))

#4
ggplot (paises,(aes(x=year, y=lifeExp, color = country))) +
  geom_point(alpha=0.5, size=2,) +
  labs(y="Esperanza de vida", x="Año", subtitle="Expectativa de Vida por año en Costa Rica y Uruguay")
#Costa Rica sobrepasó a Uruguay en cuanto a la esperanza de vida en el año 1977. 

#5
continentes <- datos %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(PromedioExpectativaVida = median(lifeExp), Poblacion = sum(pop))
#Oceanía tiene una expectativa de vida de 80.72 años.

#6 
cor(datos$gdpPercap, datos$lifeExp, method = "pearson")
#X = PIB Y = Expectativa de vida 
#0,58.
#Presenta una relación moderadamente positiva. Se intuye que mientras aumente el PIB aumenta la expectativa de vida en los países. Sin embargo, es necesario recordar que la correlación no es sinónimo de causalidad. 
