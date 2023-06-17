#Instalamos los paquetes relevantes
install.packages("academictwitteR")
install.packages("tidyr")
install.packages("ggplot2")

#Cargamos las librerías que necesitaremos para poder procesar y visualizar los datos
library(academictwitteR)
library(tidyverse)
library(data.table)
library(textclean)
library(tm)
library(stringr)
library(dplyr)
library(tidyr)
library(rtweet)
library(ggplot2)
#Añadimos la línea "TWITTER_BEARER=YOURTOKENHERE"en .Renviron, sustituyendo YOURTOKENHERE por el token procedente. Esto es nuestra licencia para poder extraer tweets mediante la API de Twitter
set_bearer()

#Reiniciamos R 

#Cargamos la librería
library(academictwitteR)

## Aplicamos la función que nos sacará los tweets de Twitter
  #Filtramos con determinados hashtags
  #Pedimos que nos extraiga tweets desde el 1 de enero de 2017 hasta el 1 de marzo de 2023 (día en que se hizo la descarga)
  #Le pedimos que únicamente nos extraiga tweets en España 
  #Quitamos el bounding box

tweets <-
  get_all_tweets(
    query = c("MadridconBici", "Movilidad madrid", "MovilidadSostenible madrid", "#EMS madrid", "#MadridZBE", "#MadridZonaBajasEmisiones", "#transporte madrid", "#TransportePublico madrid", "#AsocBiciMadrid", "#somostrafico madrid", "#bicicritica madrid", "#atascos madrid", "#movilidadurbana madrid"),
    #users = c("XX", "XX"),
    start_tweets = "2017-01-01T00:00:00Z",
    end_tweets = "2023-03-01T00:00:00Z",
    bearer_token = get_bearer(),
    bind_tweets=FALSE,
    country = "ES", 
    #lang = "es",
    #bbox= c(-3.838321,40.298529,-3.479205,40.518644),
    file = "tweets_madrid",
    data_path="data_complete",
    n = 1000000,
  )

#Pedimos que todos los tweets nos los guarde en tweets_madrid, usando la función bind que nos combine y cargue los tweets en esa ruta específica
tweets_madrid <- bind_tweets(data_path = "data_complete")


#La siguiente línea de código crea un nuevo DataFrame, y cada columna se asigna para diferentes variables:
  #'author_id' se asigna a partir de tweets_madrid$author_id.
  #'text' se asigna a partir de tweets_madrid$text.
  #'fecha' se asigna a partir de tweets_madrid$created_at.
  #'lang' se asigna a partir de tweets_madrid$lang.
  #'retweet_number' se asigna a partir de tweets_madrid$public_metrics$retweet_count.
  #'reply_number' se asigna a partir de tweets_madrid$public_metrics$reply_count.
  #'like_number' se asigna a partir de tweets_madrid$public_metrics$like_count

df<-data.frame(author_id=tweets_madrid$author_id, text=tweets_madrid$text, fecha=tweets_madrid$created_at, lang=tweets_madrid$lang, retweet_number=tweets_madrid$public_metrics$retweet_count, reply_number=tweets_madrid$public_metrics$reply_count,like_number=tweets_madrid$public_metrics$like_count)

#Agregamos la columna 'hashtags' al DataFrame df que contiene las etiquetas de hashtags extraídas. Si no hay hashtags disponibles, se asigna NA a la celda correspondiente.
df$hashtags=lapply(tweets_madrid$entities$hashtags, function(x) if (length(x) == 0) NA else  toString(unlist(`[`(x, c('tag')))) )

#Creamos una nueva columna, users y utilizamos la función lapply() para iterar sobre los elementos de tweets_madrid$entities$mentions
#Luego aplicamos una función que hace que si la longitud de x es 0,se asigne un NA
#De lo contrario, utiliza unlist() y toString() para extraer los nombres de usuario ('username') de cada elemento x y convertirlos en una cadena de texto
df$users=lapply(tweets_madrid$entities$mentions, function(x) if (length(x) == 0) NA else  toString(unlist(`[`(x, c('username')))) )

#Damos formato a las columnas de fecha, año y mes
df$fecha <- as.Date(df$fecha, format = "%Y-%m-%d")
df$year <- format(df$fecha, "%Y")
df$month <- format(df$fecha, "%m")

#Para saber el tipo de variables 
sapply(df, class)
sapply(tweets_madrid,class)


#Instalamos y cargamos el paquete correspondiente
install.packages("dplyr")
library(dplyr)

#Agrupamos el DataFrame df por la columna 'retweet_number' utilizando la función group_by().
#Creamos una nueva columna llamada 'count' que indica la cantidad de tweets en cada grupo de 'retweet_number'.
number_tweets_bynumberretweets<-df %>% 
  group_by(retweet_number) %>% 
  summarise(count = n())


#Hacemos lo mismo que antes, pero agrupando por fecha
number_tweets_bydate<-df %>% 
  group_by(fecha) %>% 
  summarise(count = n())


#Hacemos lo mismo que antes, pero agrupando por idioma
number_tweets_bylang<-df %>% 
  group_by(lang) %>% 
  summarise(count = n())


#Extraemos todos los hashtags de la columna 'hashtags' del DataFrame df y los almacenamos en el vector 'all_hashtags'.
#Después, encontramos todas las coincidencias de palabras en cada cadena de texto de la columna 'hashtags' del DataFrame df
#Luego, la función unlist() se utiliza para aplanar la lista de palabras y almacenarlas en el vector 'all_hashtags'. Esto significa que 'all_hashtags' contendrá todos los hashtags extraídos de la columna 'hashtags' en una única secuencia de palabras.
all_hashtags <- unlist( 
  regmatches(df$hashtags,  gregexpr('\\w+', df$hashtags)))

#Contamos la frecuencia de cada uno de los hashtags
freq_count <- as.data.frame(table(all_hashtags))

#Ordenamos el DataFrame por la columna Freq en orden descendente
freq_count<-freq_count[order(freq_count$Freq, decreasing = TRUE),]

#Luego, almacenamos el número de filas en el DataFrame freq_count utilizando la función nrow(). 
total_different_hashtags <- nrow(freq_count)


## Frecuencias de emojis
devtools::install_github("hadley/emo")

#Cargamos la librería relevante
library(emo)

#Extraemos todos los emojis y calculamos la frecuencia de uso de cada uno, seleccionando los 20 más frecuentes
emojis<-df %>%
  mutate(emoji = ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE) %>%
  top_n(20)

#Convertimos todas las columnas del DataFrame df en tipo de dato caracter
df <- apply(df,2,as.character)

#Guardamos el DataFrame modificado en un csv
write.csv2(df,"df_completo.csv", row.names = FALSE)

#Se convierte nuevamente a tipo de dato data.frame utilizando la función as.data.frame().
df<-as.data.frame(df)

## Luego hacemos el pre-procesamiento, para ello:


#Le proporcionamos reglas de reemplazamiento específicas, para que muestre bien los datos
custom_rules <- "ñ > \\~;
                 Ñ > \\^;
                 ::Latin-ASCII; #se realiza una transformación general de caracteres latinos a su equivalente ASCII.
                 \\~ > ñ;
                 \\^ > Ñ"

#Cargamos la librería
library(stopwords)

# Añadimos las stopwords que se hayan pasado
stopw<- stopwords("spanish")
stop<- c(stopw,"madrid") # añadimos la palabra madrid porque la mayoria de tweets tienen esta palabra


#Aplicamos funciones de limpieza general de los tweets
#La funcion replace_contraction es para remover las contractions (we've en we have)
#Gsub es para remover
clean <- function(x){
  if (!(is.na(x))){x <- gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", x)} #Remove URLs
  if (!(is.na(x))){x <- gsub("@\\w+", "", x)} #Quitamos menciones
  if (!(is.na(x))){x <- gsub("#\\w+", "", x)} #Quitamos hashtags
  if (!(is.na(x))){x <- gsub("\\d+\\w*\\d*", "", x)}

#De la línea 170 a la 173, de la 174 a la 175 y la 178 las tendremos que comentar para extraer un archivo que podamos usar en el análisis de sentimientos    
  if (!(is.na(x))){x<- stringi::stri_trans_general(x, id = custom_rules, rules = TRUE)}
  if (!(is.na(x))){x <- gsub("[[:punct:]]", " ", x) } #Quitamos signos de puntuación
  if (!(is.na(x))){x <-tolower(x)}
  if (!(is.na(x))){x <-iconv(x, "latin1", "ASCII", sub="")}
  
  if (!(is.na(x))){x <-removeNumbers(x)} #Quitamos números
  if (!(is.na(x))){ x <-removeWords(x,stop)}
  if (!(is.na(x))){x <-gsub('\\b+RT', '', x)} #Quitamos retweets
  
  if (!(is.na(x))){x <- gsub('\\b\\w{1,2}\\s','',x)}
  if (!(is.na(x))){x <- str_replace(gsub("\\s+", " ", str_trim(x)), "B", "b")} #Varios espacios a un único espacio

  return(x)}

#Una vez limpios los datos, cargamos otras dos librerías que necesitamos
library(tm)
library(stringr)

#Limpiamos los datos 
df$cleaned_text<-clean(df$text)
 
#Calculamos la longitud de la columna 
length(df$cleaned_text)
 
#creamos un nuevo DataFrame llamado df_filtered que contiene solo las filas del DataFrame df donde la columna 'cleaned_text' no es un valor nulo (NA) ni una cadena vacía ("") 
#Se utiliza la función is.na() para verificar si la columna 'cleaned_text' es nula y el operador lógico | para combinarla con la condición de que la columna 'cleaned_text' no sea una cadena vacía
df_filtered<-df[!(is.na(df$cleaned_text) | df$cleaned_text==""), ]
 
##Filtramos para que únicamente nos salgan tweets en español
df_filtered<-filter( df_filtered, lang=="es")
 
#Quitamos las columnas de hashtags y users 
drop <- c("hashtags","users")
df_filtered = df_filtered[,!(names(df_filtered) %in% drop)]

#Pedimos que todo nos lo saque en un csv, que es el que utilizaremos en Python Colab
write.csv2(df_filtered,"df_filtered.csv", row.names = FALSE)
   
