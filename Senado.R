#Configura el directorio de trabajo
setwd("/Users/mariolp/Documents/Tarea Examen Inferencia")
# Cargar librerías necesarias
library(readr)
library(dplyr)
library(readxl)

# Cargar los datos de cómputos distritales
sen_2024 <- read_delim("SEN_2024.csv", skip = 6, delim = "|")

# Verificar los primeros registros para asegurar una correcta carga de datos
print(head(sen_2024))

# Seleccionar las columnas relevantes para el análisis de los votos por partido
vote_columns <- c('ENTIDAD', 'PAN', 'PRI', 'PRD', 'PVEM', 'PT', 'MC', 'MORENA', 
                  'PAN_PRI_PRD', 'PAN_PRI', 'PAN_PRD', 'PRI_PRD', 'PVEM_PT_MORENA', 
                  'PVEM_PT', 'PVEM_MORENA', 'PT_MORENA', 'CANDIDATO/A NO REGISTRADO/A')
#Inspeccionar los Datos:
#str(sen_2024[vote_columns[2:length(vote_columns)]])
#summary(sen_2024[vote_columns[2:length(vote_columns)]])

# Convertir las columnas de votos a valores numéricos, eliminando caracteres no numéricos
sen_2024[vote_columns[2:length(vote_columns)]] <- lapply(
  sen_2024[vote_columns[2:length(vote_columns)]], 
  function(x) as.numeric(gsub("[^0-9]", "", as.character(x)))
)

# Verificar si todavía hay NAs después de la conversión
#print(sum(is.na(sen_2024[vote_columns[2:length(vote_columns)]])))

# Revisar algunos ejemplos donde hay NAs para entender mejor el problema
#na_positions <- which(is.na(sen_2024[vote_columns[2:length(vote_columns)]]), arr.ind = TRUE)
#print(head(na_positions))


# Agrupar por entidad y sumar los votos por partido en cada entidad
votes_by_entity <- sen_2024 %>%
  group_by(ENTIDAD) %>%
  summarise(across(all_of(vote_columns[-1]), \(x) sum(x, na.rm = TRUE)))

# Leer los convenios de coalición
coaliciones <- read_excel("convenios_coaliciones_senadores_2024.xls")

# Crear una función para determinar el ganador y segundo lugar en cada entidad
get_top_two_parties <- function(row) {
  sorted_parties <- sort(row, decreasing = TRUE)
  winner <- names(sorted_parties)[1]
  second_place <- ifelse(length(sorted_parties) > 1, names(sorted_parties)[2], NA)
  return(c(winner, second_place))
}

# Aplicar la función a cada fila para obtener el ganador y segundo lugar en cada entidad
top_two_parties <- t(apply(votes_by_entity[,-1], 1, get_top_two_parties))
colnames(top_two_parties) <- c("winner", "second_place")
top_two_parties <- cbind(ENTIDAD = votes_by_entity$ENTIDAD, top_two_parties)

# Función mejorada para asignar senadores según convenios de coalición
asignar_senadores <- function(winner, second_place, convenios) {
  senadores <- list(mayoria_relativa = rep(NA, 2), primera_minoria = NA)
  
  # Comprobar si el ganador es NA o no está en las coaliciones
  if (!is.na(winner) && winner %in% convenios$Coalición) {
    partidos_mayoria <- convenios[convenios$Coalición == winner, ]$Partido
    senadores$mayoria_relativa <- head(partidos_mayoria, 2)  # Asignar 2 senadores
  } else if (!is.na(winner)) {
    senadores$mayoria_relativa <- rep(winner, 2)  # Si no hay coalición, asignar al partido directamente
  } else {
    message("Advertencia: 'winner' es NA o no se encuentra en las coaliciones.")
  }
  
  # Comprobar si el segundo lugar es NA o no está en las coaliciones
  if (!is.na(second_place) && second_place %in% convenios$Coalición) {
    partido_minoria <- convenios[convenios$Coalición == second_place, ]$Partido[1]
    senadores$primera_minoria <- partido_minoria
  } else if (!is.na(second_place)) {
    senadores$primera_minoria <- second_place  # Si no hay coalición, asignar al partido directamente
  } else {
    message("Advertencia: 'second_place' es NA o no se encuentra en las coaliciones.")
  }
  
  return(senadores)
}


# Aplicar la función de asignación de senadores a cada fila con manejo de errores
asignaciones <- apply(top_two_parties, 1, function(row) {
  tryCatch({
    asignar_senadores(row['winner'], row['second_place'], coaliciones)
  }, error = function(e) {
    message("Error en la asignación de senadores para la fila: ", row['ENTIDAD'])
    message("Detalles del error: ", e)
    return(list(mayoria_relativa = c(NA, NA), primera_minoria = NA))
  })
})


# Convertir la lista de asignaciones en un dataframe para una visualización más clara
asignaciones_df <- do.call(rbind, lapply(asignaciones, function(x) unlist(x)))
asignaciones_df <- cbind(ENTIDAD = votes_by_entity$ENTIDAD, asignaciones_df)

# Mostrar la asignación de senadores por entidad
print(asignaciones_df)

# Calcular los senadores plurinominales a nivel nacional
# Sumar los votos totales por partido a nivel nacional
national_votes <- colSums(votes_by_entity[,-1], na.rm = TRUE)

# Asignar 32 senadores plurinominales proporcionalmente a los votos obtenidos
total_plurinominal_seats <- 32
plurinominal_seats <- round((national_votes / sum(national_votes, na.rm = TRUE)) * total_plurinominal_seats)

# Ajustar para asegurar que la suma sea exactamente 32
while (sum(plurinominal_seats) != total_plurinominal_seats) {
  if (sum(plurinominal_seats) > total_plurinominal_seats) {
    plurinominal_seats[which.max(plurinominal_seats)] <- plurinominal_seats[which.max(plurinominal_seats)] - 1
  } else {
    plurinominal_seats[which.min(plurinominal_seats)] <- plurinominal_seats[which.min(plurinominal_seats)] + 1
  }
}

# Mostrar la asignación de senadores plurinominales
print(plurinominal_seats)