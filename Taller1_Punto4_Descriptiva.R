# ==============================================================================
#  TALLER 1 - PUNTO 4: Estadística descriptiva univariada
#  Requiere haber corrido antes Taller1_Punto3_Limpieza.R
#  (o tener el archivo Datos_limpios_cultivos_Valle.csv en la carpeta)
# ==============================================================================
options(scipen = 999)   # evita la notación científica (1e+05)

# Si cerraste R, se cargan de nuevo los datos limpios
if (!exists("datos")) {
  datos <- read.csv("Datos_limpios_cultivos_Valle.csv",
                    encoding = "UTF-8", fileEncoding = "UTF-8")
}

# ---- 4.1 VARIABLES CUANTITATIVAS ---------------------------------------------
vars_cuanti <- c("Ha_sembradas", "Ha_cosechadas", "Produccion_t", "Rendimiento_t_ha")

moda <- function(x) {
  t <- table(x)
  as.numeric(names(t)[which.max(t)])
}

resumen <- function(x) {
  c(n          = length(x),
    # Tendencia central
    Media      = mean(x),
    Mediana    = median(x),
    Moda       = moda(x),
    # Dispersión
    Desv_est   = sd(x),
    Varianza   = var(x),
    CV_pct     = 100 * sd(x) / mean(x),
    Rango      = max(x) - min(x),
    RIC        = IQR(x),
    # Posición
    Minimo     = min(x),
    P10        = unname(quantile(x, 0.10)),
    Q1         = unname(quantile(x, 0.25)),
    Q3         = unname(quantile(x, 0.75)),
    P90        = unname(quantile(x, 0.90)),
    Maximo     = max(x))
}

tabla_cuanti <- round(t(sapply(datos[vars_cuanti], resumen)), 2)
tabla_cuanti

# ---- 4.2 VARIABLES CUALITATIVAS (conteo y porcentaje) ------------------------
frecuencias <- function(x, top = NULL) {
  t  <- sort(table(x), decreasing = TRUE)
  df <- data.frame(Categoria = names(t), n = as.vector(t))
  # Si hay muchas categorías se muestran las 'top' y el resto se agrupa en "Otros"
  if (!is.null(top) && nrow(df) > top) {
    otros <- data.frame(Categoria = paste0("Otros (", nrow(df) - top, " categorías)"),
                        n = sum(df$n[-(1:top)]))
    df <- rbind(df[1:top, ], otros)
  }
  df$Porcentaje <- round(100 * df$n / sum(df$n), 2)
  df
}

frec_tipo      <- frecuencias(datos$Tipo_cultivo)
frec_cultivo   <- frecuencias(datos$Cultivo, top = 10)
frec_municipio <- frecuencias(datos$Municipio, top = 10)
frec_anio      <- frecuencias(datos$Anio)
frec_anio      <- frec_anio[order(frec_anio$Categoria), ]   # en orden de año

frec_tipo
frec_cultivo
frec_municipio
frec_anio

# ---- 4.3 TABLA RESUMEN (formato tipo "Tabla 1" del artículo) -----------------
f <- function(v) formatC(v, format = "f", digits = 2, big.mark = ",")
tabla1_cuanti <- data.frame(
  Variable            = c("Hectáreas sembradas (ha)", "Hectáreas cosechadas (ha)",
                          "Producción (t)", "Rendimiento (t/ha)"),
  Media_DE            = paste0(f(tabla_cuanti[, "Media"]), " ± ", f(tabla_cuanti[, "Desv_est"])),
  Mediana_Q1_Q3       = paste0(f(tabla_cuanti[, "Mediana"]), " (", f(tabla_cuanti[, "Q1"]),
                               " – ", f(tabla_cuanti[, "Q3"]), ")"),
  Min_Max             = paste0(f(tabla_cuanti[, "Minimo"]), " – ", f(tabla_cuanti[, "Maximo"])),
  CV_pct              = f(tabla_cuanti[, "CV_pct"])
)
tabla1_cuanti

# ---- 4.4 EXPORTAR LAS TABLAS --------------------------------------------------
write.csv(data.frame(Variable = rownames(tabla_cuanti), tabla_cuanti),
          "Tabla_cuantitativas.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(tabla1_cuanti,  "Tabla1_resumen.csv",        row.names = FALSE, fileEncoding = "UTF-8")
write.csv(frec_tipo,      "Frecuencia_tipo.csv",       row.names = FALSE, fileEncoding = "UTF-8")
write.csv(frec_cultivo,   "Frecuencia_cultivo.csv",    row.names = FALSE, fileEncoding = "UTF-8")
write.csv(frec_municipio, "Frecuencia_municipio.csv",  row.names = FALSE, fileEncoding = "UTF-8")
write.csv(frec_anio,      "Frecuencia_anio.csv",       row.names = FALSE, fileEncoding = "UTF-8")
