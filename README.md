Ánalisis de la División de las comunidades del partido demócrata de los EE.UU (DNC) en Twitter.

Los datos fueron recogidos entre el 24 de abril y el 17 de mayo de 2020. Se realizan análisis de tópicos y análisis de sentimiento de seguidores de la cuenta del Comité Nacional Demócrata, del Partído Republicadno y del grupo progresista dentro del partido demócrata, Justice Démocrats.

### Instalaciones necesarias
- RStudio
- librerias: 
  - tidyverse
  - rtweet
  - tidytext
  - qdap
  - qdapRegex
  - sentimentR
  - tokenizers
  - topicmodels
  - scales
  - ggradar
  - tidygraph
  - remoji
  - tm
 
 Hace falta además, una cuenta de desarrollador para el acceso a la API de Twitter.
 
 ### Descripcion de los archivos
 
 - **00-funciones_auxiliares.R**: contiene todas las funciones creadas para la ejecución de los scripts y para las visualizaciones.
 - **01-creacion_comunidades.R**: contiene el código para la creación de comunidades a partir de identificadores de seguidores de cuentas específicas, en Twitter (GOP, TheDemocrats y Justice Democrats)
 - **02-data_clean.R**: contiene la ejecución de la función auxiliar de limpieza de datos (format_twitter_df_data) que se emplean en en análisis de tópicos, para cada uno de los tópicos descargados
 - **03-sentiment_data_clean.R**: contiene la función auxiliar de la función de limpeza de datos y calculo del scoring de polaridad de sentimiento (lee_datos_sent) para algunos de los sets de datos descargados
 - **04-get_tokenized_data.R**: contiene el proceso de obtención de datos tokenizados y cálculo de frecuencias de de hashtags haciendo uso de algunas de las funciones auxiliares en funciones_auxiliares.R
 - **healthare_blog_post_markdown.rmd**: Archivo Rmarkdown empleado para la entrada del blog, en el que se realiza un análisis para el tópico 'healthcare'. Se puede acceder a este análisis en https://www.martinponsm.com/2020/06/divisi%C3%B3n-dentro-del-partido-dem%C3%B3crata-en-los-ee-uu-polit%C3%ADca-sanitaria.es-es/
 
 No es la intención que se ejecuten estos archivos de manera secuencial. Dependen de unos datos raw y datos procesados que no se han incluido aquí, entre otras cosas por razones de privacidad. El uso de las funciones incluidas en funciones auxiliares, debería ser suficiente si se quiere reproducir este ejercicio con otro set de datos.
 
