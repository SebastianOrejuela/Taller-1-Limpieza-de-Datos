# ==============================================================================
#  TALLER 1 - PUNTO 5: Gráficos
# ==============================================================================

if (!exists("datos")) {
  datos <- read.csv("Datos_limpios_cultivos_Valle.csv",
                    encoding = "UTF-8", fileEncoding = "UTF-8")
}

col1 <- "#2E7D6B"   # verde
col2 <- "#E07B39"   # naranja

# Cambiamos los nombres para que sean más cortos, para que los tipos de cultivo quepan en los gráficos
tipo_corto <- as.character(datos$Tipo_cultivo)
tipo_corto[tipo_corto == "Cultivos para Condimentos y Bebidas Medicinales y Aromáticas"] <- "Condimentos y aromáticas"
tipo_corto[tipo_corto == "Cultivos Tropicales Tradicionales"] <- "Tropicales tradicionales"

# Dibuja el gráfico en pantalla lo guarda como PNG
graficar <- function(archivo, dibujar) {
  dibujar()
  png(archivo, width = 1000, height = 650, res = 120)
  dibujar()
  dev.off()
}

# ---- Figura 1: Registros por tipo de cultivo ---------------------------------
graficar("Fig1_tipo_cultivo.png", function() {
  op <- par(mar = c(5, 12, 3, 2)); on.exit(par(op))
  p <- sort(100 * table(tipo_corto) / length(tipo_corto))
  b <- barplot(p, horiz = TRUE, las = 1, col = col1, border = NA,
               xlim = c(0, max(p) * 1.15), cex.names = 0.85,
               xlab = "Porcentaje de registros (%)",
               main = "Figura 1. Registros por tipo de cultivo")
  text(p, b, paste0(formatC(p, format = "f", digits = 1, decimal.mark = ","), " %"),
       pos = 4, cex = 0.8)
})

# ---- Figura 2: Los 10 cultivos más frecuentes ---------------------------------
graficar("Fig2_top10_cultivos.png", function() {
  op <- par(mar = c(5, 9, 3, 2)); on.exit(par(op))
  t <- rev(sort(table(datos$Cultivo), decreasing = TRUE)[1:10])
  b <- barplot(t, horiz = TRUE, las = 1, col = col1, border = NA,
               xlim = c(0, max(t) * 1.12),
               xlab = "Número de registros",
               main = "Figura 2. Los 10 cultivos con más registros")
  text(t, b, t, pos = 4, cex = 0.8)
})

# ---- Figura 3: Histograma del rendimiento ------------------------------------
graficar("Fig3_hist_rendimiento.png", function() {
  x <- datos$Rendimiento_t_ha
  hist(x, breaks = 40, col = col1, border = "white", xlim = c(0, 200),
       xlab = "Rendimiento (t/ha)", ylab = "Frecuencia",
       main = "Figura 3. Distribución del rendimiento")
  abline(v = mean(x),   col = col2,    lwd = 2, lty = 2)
  abline(v = median(x), col = "black", lwd = 2)
  legend("topright", bty = "n",
         legend = paste(c("Media =", "Mediana ="),
                        formatC(c(mean(x), median(x)), format = "f", digits = 2, decimal.mark = ",")),
         col = c(col2, "black"), lty = c(2, 1), lwd = 2)
})


# ---- Figura 5: Boxplot del rendimiento por tipo de cultivo -------------------
graficar("Fig5_boxplot_rendimiento_tipo.png", function() {
  op <- par(mar = c(5, 12, 3, 2)); on.exit(par(op))
  boxplot(datos$Rendimiento_t_ha ~ tipo_corto, horizontal = TRUE, las = 1,
          col = "#BFDCD3", border = col1, outcol = col2, outpch = 20, outcex = 0.5,
          cex.axis = 0.85, xlab = "Rendimiento (t/ha)", ylab = "",
          main = "Figura 5. Rendimiento por tipo de cultivo")
})

# ---- Figura 6: Área sembrada total por año -----------------------------------
graficar("Fig6_area_por_anio.png", function() {
  area <- tapply(datos$Ha_sembradas, datos$Anio, sum) / 1000
  plot(as.numeric(names(area)), area, type = "b", pch = 19, col = col1, lwd = 2,
       xlab = "Año", ylab = "Área sembrada (miles de ha)",
       main = "Figura 6. Área sembrada total en el Valle del Cauca por año")
  grid()
})
