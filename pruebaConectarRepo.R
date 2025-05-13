library(httr)
library(jsonlite)

# Realizar la solicitud GET
url <- "https://api.github.com/repos/fcatools/contexts/contents/contexts"
response <- GET(url)
content_text <- content(response, as = "text", encoding = "UTF-8")
content_json <- fromJSON(content_text)
# Verificar que la solicitud fue exitosa
if (status_code(response) == 200) {
  # Convertir la respuesta a texto
  content_text <- content(response, as = "text", encoding = "UTF-8")

  # Parsear el JSON
  content_json <- fromJSON(content_text)

  # Extraer los nombres de los archivos
  archivos <- content_json$name

  # Mostrar la lista de archivos
  print(archivos)
} else {
  cat("Error al obtener los datos: ", status_code(response), "\n")
}

