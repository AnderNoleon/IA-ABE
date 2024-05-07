library(tidyverse)
library(caret)
library(neuralnet)
library(palmerpenguins)
library(ggplot2)
datos =  penguins
# datos <- na.omit(datos)
datos = datos[complete.cases(datos), ]
muestra = createDataPartition(datos$species, p=0.8, list = F)
train = datos[muestra,]
test = datos[-muestra,]
# Escalar características antes de entrenar el modelo
train_scaled <- train %>%
  mutate_at(vars(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g), scale)

# Entrenar el modelo con los datos escalados
red.neuronal <- neuralnet(species ~  bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, 
                          data = train_scaled, 
                          hidden = c(4,3))

red.neuronal$act.fct
plot(red.neuronal)
# aplicar el codgio de prueba
prediccion = predict(red.neuronal, test, type = "class")
# decodifcar la columna valor m y pr ese test rata
decodificarCol = apply(prediccion,1, which.max)
#crear una colum nueva , valores decoficado
codificado = data_frame(decodificarCol)
codificado = mutate(codificado, especie = recode(codificado$decodificarCol, "1" = "Adelie", "2" = "Chinstrap", "3" = "Gentoo"))
test$especie.Pred = codificado$especie



# una vez seleccionado cuente especies
datos %>% 
  count(species)

datos %>%
  group_by(species) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE))

plot(datos)

datos %>%
  count(species) %>% 
  ggplot() + geom_col(aes(x = species, y = n, fill = species))+
  geom_label(aes(x = species, y = n,  label= n))+ 
  scale_fill_manual(values = c('darkorange','purple','cyan')) +
  theme_minimal() +
  labs(title  = 'Distribuccion de pinguinos por especie')

datos %>%
  drop_na() %>% 
  count(sex, species) %>%
  ggplot() + geom_col(aes(x= species, y =n, fill = species))+
  geom_label(aes(x = species, y =n , label = n)) + 
  scale_fill_manual(values = c('darkorange','red','cyan4')) +
  facet_wrap(~sex) +
  theme_minimal() +
  labs(title  = 'Especies de pinguinos sex')


datos %>%
  select_if(is.numeric) %>%
  drop_na %>%
  cor()

ggplot(data = penguins,
       aes(x = flipper_length_mm,
           y = body_mass_g)) + 
  geom_point(aes(color = species,
                 shape = species),
             size = 3,
             alpha = 0.8) +
  #theme_minimal() +
  scale_color_manual(values = c('darkorange','red','cyan4')) +
  labs(title = "Tamanio de pinguino palmer station leter" , 
       subtile= "longitud de aleta y masa corpotral de los" ,
       x = 'longitud de aleta m', 
       y = 'masa corporal',
       color = "especie de pingui",
       shape = "especie de pinguino" ) +
  theme_minimal()

ggplot(data = penguins,
       aes(x = flipper_length_mm,
           y = body_mass_g)) + 
  geom_point(aes(color = island,
                 shape = species),
             size = 3,
             alpha = 0.8) +
  #theme_minimal() +
  scale_color_manual(values = c('cyan','red4','pink2')) +
  labs(title = "Tamanio de pinguino palmer station leter" , 
       subtile= "longitud de aleta y masa corpotral de los" ,
       x = 'longitud de aleta m', 
       y = 'masa corporal',
       color = "isla de pingui",
       shape = "especie de pinguino" ) +
  theme_minimal()

ggplot(data = penguins,
       aes(x = flipper_length_mm,
           y = bill_length_mm)) + 
  geom_point(aes(color =  species,
                 shape = species),
             size = 3,
             alpha = 0.8) +
  #theme_minimal() +
  scale_color_manual(values = c('purple','yellow2','blue4')) +
  labs(title = "Tamanio de pinguino palmer station leter" , 
       subtile= "llongitud de aleta y longitud de pico segun cada specie" ,
       x = 'longitud de aleta m', 
       y = 'masa corporal',
       color = "isla de pingui",
       shape = "especie de pinguino" ) +
  theme_minimal()

