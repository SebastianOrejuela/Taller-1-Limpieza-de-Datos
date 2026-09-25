# ==============================================================================
#  TALLER 1 - PUNTO 3: Limpieza y preprocesamiento de los datos
#  Datos: Consolidado agrícola por municipios de los cultivos permanentes
#         del Valle del Cauca (datos.gov.co), años 2000-2023
#  Solo usa R base: no hay que instalar paquetes
# ==============================================================================

# ---- 0. Cargar los datos ------------------------------------------------------
# Busca el CSV original en la carpeta de trabajo (ignora el archivo limpio si ya existe)
archivos <- list.files(pattern = "\\.csv$", ignore.case = TRUE)
archivos <- archivos[grepl("antes de la limpieza", archivos)]
print(archivos)          # debe aparecer el archivo original
ruta <- archivos[1]

# Se lee todo como texto (character) para revisar los formatos antes de convertir
datos_crudos <- read.csv(ruta, encoding = "UTF-8", fileEncoding = "UTF-8",
                         colClasses = "character", check.names = FALSE)

dim(datos_crudos)   # 15282 filas x 11 columnas
str(datos_crudos)
head(datos_crudos)

datos <- datos_crudos

# Se dejan nombres cortos y sin caracteres especiales (la "/" y la "ñ" dan problemas)
names(datos) <- c("Tipo_cultivo", "Anio", "Id_municipio", "Municipio",
                  "Id_cultivo", "Cultivo", "Ciclo", "Ha_sembradas",
                  "Ha_cosechadas", "Produccion_t", "Rendimiento_t_ha")

# Registro de cuántas filas se pierden en cada paso (sirve para el informe)
bitacora <- data.frame(Paso = "Datos originales", Filas = nrow(datos))
registrar <- function(paso) {
  bitacora <<- rbind(bitacora, data.frame(Paso = paso, Filas = nrow(datos)))
}


# ==============================================================================
# 1. ERRORES DE FORMATO: los números traen la coma de miles
#    Ejemplos: Año = "2,000", Producción = "4,324.8", Id_municipio = "76,001"
#    Así, R los lee como texto y no se puede calcular nada con ellos
# ==============================================================================
quitar_comas <- function(x) gsub(",", "", trimws(x))

cols_numericas <- c("Anio", "Ha_sembradas", "Ha_cosechadas",
                    "Produccion_t", "Rendimiento_t_ha")
for (col in cols_numericas) {
  datos[[col]] <- as.numeric(quitar_comas(datos[[col]]))
}

# Los códigos (DANE del municipio y código del cultivo) son identificadores,
# no cantidades: se dejan como texto, pero sin la coma
datos$Id_municipio <- quitar_comas(datos$Id_municipio)
datos$Id_cultivo   <- quitar_comas(datos$Id_cultivo)

# Revisión: la conversión no debe producir NA
colSums(is.na(datos[cols_numericas]))   # todo debe dar 0


# ==============================================================================
# 2. INFORMACIÓN FALTANTE (NA o celdas vacías)
# ==============================================================================
colSums(is.na(datos))                                  # NA por columna
sapply(datos, function(x) sum(trimws(x) == "", na.rm = TRUE))   # vacíos
# Resultado: no hay datos faltantes en ninguna variable


# ==============================================================================
# 3. INCONSISTENCIAS EN LAS CATEGORÍAS (errores de digitación)
# ==============================================================================
# Espacios de sobra al inicio o al final
for (col in c("Tipo_cultivo", "Municipio", "Cultivo", "Ciclo")) {
  datos[[col]] <- trimws(datos[[col]])
}

table(datos$Tipo_cultivo)
# ERROR: la misma categoría aparece como "Raíces y Tubérculos" (1202 filas)
#        y como "Raíces y tubérculos" (3 filas), por la mayúscula
datos$Tipo_cultivo[datos$Tipo_cultivo == "Raíces y tubérculos"] <- "Raíces y Tubérculos"
table(datos$Tipo_cultivo)   # ahora son 7 categorías

# ¿Cada código tiene un solo nombre y cada cultivo un solo tipo?
max(tapply(datos$Municipio, datos$Id_municipio, function(x) length(unique(x))))  # 1 = OK
max(tapply(datos$Cultivo,   datos$Id_cultivo,   function(x) length(unique(x))))  # 1 = OK
max(tapply(datos$Tipo_cultivo, datos$Cultivo,   function(x) length(unique(x))))  # 1 = OK
length(unique(datos$Municipio))   # 42 municipios
sort(unique(datos$Anio))          # 2000 a 2023, sin años raros


# ==============================================================================
# 4. VARIABLE SIN VARIABILIDAD
#    "Ciclo" vale "Anual" en todas las filas, así que no aporta información
# ==============================================================================
table(datos$Ciclo)
datos$Ciclo <- NULL


# ==============================================================================
# 5. REGISTROS DUPLICADOS
#    Cada fila debe ser única por (Año, Municipio, Cultivo)
# ==============================================================================
sum(duplicated(datos))                                             # 18 exactos
sum(duplicated(datos[, c("Anio", "Id_municipio", "Id_cultivo")]))  # 18 por llave
datos[duplicated(datos) | duplicated(datos, fromLast = TRUE), ]    # verlos

datos <- datos[!duplicated(datos), ]
registrar("Sin duplicados")


# ==============================================================================
# 6. INCONSISTENCIAS LÓGICAS ENTRE VARIABLES
# ==============================================================================
# 6a. Valores negativos (no pueden existir)
colSums(datos[cols_numericas[-1]] < 0)   # todo 0 -> OK

# 6b. No se puede cosechar más área de la que se sembró
inc_area <- datos$Ha_cosechadas > datos$Ha_sembradas
sum(inc_area)                            # 152 filas
head(datos[inc_area, ], 10)
datos <- datos[!inc_area, ]
registrar("Sin área cosechada > área sembrada")

# 6c. Si no se cosechó nada (0 ha) no puede haber producción
inc_prod <- datos$Ha_cosechadas == 0 & datos$Produccion_t > 0
sum(inc_prod)                            # 7 filas
datos[inc_prod, ]
datos <- datos[!inc_prod, ]
registrar("Sin producción con 0 ha cosechadas")

# 6d. Revisar que Rendimiento = Producción / Área cosechada
rend_calc <- ifelse(datos$Ha_cosechadas > 0,
                    datos$Produccion_t / datos$Ha_cosechadas, 0)
dif <- abs(rend_calc - datos$Rendimiento_t_ha)
sum(dif > 0.5 & dif > 0.10 * rend_calc)  # 0 -> el rendimiento está bien calculado

# 6e. Área cosechada > 0 pero producción = 0: NO se eliminan.
#     Son pérdidas totales de cosecha o flores (que no se miden en toneladas)
sum(datos$Ha_cosechadas > 0 & datos$Produccion_t == 0)


# ==============================================================================
# 7. DATOS ATÍPICOS (outliers) - se IDENTIFICAN, no se borran
#    Se usa el criterio de Tukey (1.5*RIC) sobre el rendimiento POR CULTIVO,
#    porque no tiene sentido comparar la caña (~115 t/ha) con el café (~1 t/ha)
# ==============================================================================
es_atipico <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  ric <- q[2] - q[1]
  x < q[1] - 1.5 * ric | x > q[2] + 1.5 * ric
}
datos$Atipico_rend <- FALSE
con_cosecha <- datos$Ha_cosechadas > 0
datos$Atipico_rend[con_cosecha] <- ave(datos$Rendimiento_t_ha[con_cosecha],
                                       datos$Cultivo[con_cosecha],
                                       FUN = es_atipico) == 1
sum(datos$Atipico_rend)   # cantidad de atípicos

boxplot(Rendimiento_t_ha ~ Tipo_cultivo, data = datos, las = 2,
        cex.axis = 0.6, main = "Rendimiento por tipo de cultivo (t/ha)")

# Los valores más altos (caña de azúcar en Palmira, papaya ~190 t/ha) son reales
# en la región: son extremos, no errores. Por eso se conservan y solo se marcan
head(datos[order(-datos$Rendimiento_t_ha),
           c("Anio", "Municipio", "Cultivo", "Rendimiento_t_ha")], 10)


# ==============================================================================
# 8. TIPOS FINALES DE LAS VARIABLES
# ==============================================================================
datos$Tipo_cultivo <- factor(datos$Tipo_cultivo)
datos$Municipio    <- factor(datos$Municipio)
datos$Cultivo      <- factor(datos$Cultivo)
datos$Anio         <- as.integer(datos$Anio)
rownames(datos) <- NULL

str(datos)
summary(datos)


# ==============================================================================
# 9. RESUMEN Y EXPORTACIÓN
# ==============================================================================
bitacora$Eliminadas <- c(0, -diff(bitacora$Filas))
print(bitacora)
cat("Filas eliminadas en total:", nrow(datos_crudos) - nrow(datos),
    sprintf("(%.2f%%)\n", 100 * (nrow(datos_crudos) - nrow(datos)) / nrow(datos_crudos)))

write.csv(datos, "Datos_limpios_cultivos_Valle.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(bitacora, "Bitacora_limpieza.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
