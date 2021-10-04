# Elaborado por: Tania Reina,Hugo Sabogal
# Fecha de elaboracion: 1/10/2021
# Ultima modificacion: 3/10/2021
# Version de R: 4.1.1

rm(list = ls())
if(!require(pacman)) install.packages(pacman) 
if(!require(pacman)) install.packages(pacman)
require(pacman)
p_load(tidyverse,rio,skimr)
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

#=========#
# Punto 1 #
#=========#

Vector_incial=c(1:100)
cat(Vector_incial)
impares=seq(1,99,2)
cat(impares)
pares=Vector_incial[!Vector_incial %in% impares]
#=========#
# Punto 2 #
#=========#
cultivos=import("data/input/cultivos.xlsx")
colnames(cultivos) = cultivos[4,]
cultivos = cultivos %>% drop_na(CODMPIO)
cultivos=cultivos[-c(1,331), ]
cultivos$CODMPIO = as.numeric(cultivos$CODMPIO)
cultivos = cultivos %>% drop_na(CODMPIO)
anuales= 1999:2019 %>% as.character()

for(var in anuales){
  cultivos[,var] = as.numeric(cultivos[,var])
}
cultivos_pivot = cultivos %>% pivot_longer(!CODDEPTO:MUNICIPIO,names_to="ANUAL",values_to="H_CULTIVOS")

#=========#
# Punto 3 #
#=========#
personas=import("data/input/2019/Cabecera - Caracteristicas generales (Personas).rds")
ocupados=import("data/input/2019/Cabecera - Ocupados.rds")  %>%  mutate(ocupado=1)
desocupados=import("data/input/2019/Cabecera - Desocupados.rds")  %>% mutate(desocupado=1)
inactivos=import("data/input/2019/Cabecera - Inactivos.rds")  %>% mutate(inactivo=1)
f_trabajo=import("data/input/2019/Cabecera - Fuerza de trabajo.rds")  %>% mutate(fuerza=1)
unificada=left_join(personas,ocupados,c("secuencia_p","orden","directorio"), suffix = c("", ""))  %>%
  left_join(.,desocupados,c("secuencia_p","orden","directorio"), suffix = c("", "")) %>%
  left_join(.,inactivos,c("secuencia_p","orden","directorio"), suffix = c("", "")) %>%
  left_join(.,f_trabajo,c("secuencia_p","orden","directorio"), suffix = c("", ""))

unificada = unificada %>% select(c(secuencia_p, orden, directorio, P6020, P6040, P6920, INGLABO, DPTO, fex_c_2011, ESC, mes, P6050, ocupado, desocupado, inactivo, fuerza))
skim(unificada)

unificada %>% summarise(total = table(p6020)) 
unificada %>% group_by(P6020) %>% summarise(promedio_ingreso= mean(P6050, na.rm = T)) 
unificada %>% group_by(P6020, ocupado, DPTO) %>% summarise(promedio = mean(P6040), na.rm = T) 
unificada %>% group_by(P6020, ocupado, DPTO) %>% summarise(promedio = mean(ESC), na.rm = T) 
unificada %>% group_by(DPTO) %>% summarise(total = table(ocupado), na.rm = T) 
unificada %>% group_by(DPTO) %>% summarise(total = table(desocupado), na.rm = T)
unificada %>% group_by(DPTO) %>% summarise(total = table(fuerza), na.rm = T)

Graph1=ggplot(data=unificada,
       mapping = aes(x= P6020,
                     y= mean(P6050, na.rm = T))) +
  ggtitle("Ingresos por sexo")

ggsave(plot=Graph1, file = "data/views/graph1.jpeg")
