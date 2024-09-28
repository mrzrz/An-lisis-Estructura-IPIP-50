rm(list = ls())
data <- rio::import("data.csv")

#### Limpieza de datos ####

# Transformación de las respuestas en años históricos en edad
id <- which(data$age %in% 1950:2005)
length(id)
año <- data$age[id]
data$age[id] <- 2012 - año # 2012 porque es cuando se empieza a aplicar el test
data$age[id]


# Descarte de los datos de personas que probablemente están fingiendo su edad y mayores de 80
id <- which(data$age > 80)
length(id)
data <- data[-id, ]
nrow(data[data$age > 100, ])

# Descarte de los datos de personas menores de edad
id <- which(data$age < 18)
data <- data[-id, ]
nrow(data[data$age < 18, ])

# Descarte de valores perdidos - Eliminación por lista
which(apply(data[, -(1:7)], 1, function(x) any(x == 0)))
which(data[, -(1:7)] == 0, arr.ind = TRUE)
data <- data[-which(rownames(data) == "19065"), ] 



#### Descriptivos ####
  #### Muestra 1 ####
  data1 <- data
  
  # Solo me quedo con las personas con inglés nativo
  id <- which(data1$engnat != 1)
  data1 <- data1[-id, ]
  
  # Solo me quedo con las personas caucásicas
  id <- which(data1$race != 3)
  data1 <- data1[-id, ]
  
  # Solo me quedo con las personas de US, GB, AU, CA, NZ, IE e IN
  id <-
    which(data1$country %in% c("US", "GB", "AU", "CA", "NZ", "IE", "IN"))
  data1 <- data1[id, ]
  windowsFonts(fam = windowsFont("Times New Roman"))
  par(family = "fam")
  
  {
    boxplot(data1$age, main = "Distribución de la edad", ylab = "Edad")
    text(
      1,
      mean(data1$age),
      paste("Media:", round(mean(data1$age), 2)),
      pos = 4,
      offset = -1.8
    )
    text(
      1,
      median(data1$age),
      paste("Mediana:", median(data1$age)),
      pos = 4,
      offset = 4
    )
    text(1,
         mean(data1$age) + sd(data1$age),
         paste("Desv. Típica:", round(sd(data1$age), 2)),
         pos = 4)
    text(
      1,
      quantile(data1$age, probs = 0.25),
      paste("Q25:", quantile(data1$age, probs = 0.25)),
      pos = 2,
      offset = 4
    )
    text(
      1,
      quantile(data1$age, probs = 0.75),
      paste("Q75:", quantile(data1$age, probs = 0.75)),
      pos = 2,
      offset = 4
    )
  }
  {
    barplot(
      main = "Frecuencias de cada género",
      table(data1$gender),
      names.arg = c("NR", "Hombre", "Mujer", "Otro"),
      ylim = c(0, 6000)
    )
    text(
      x = barplot(table(data1$gender), plot = FALSE),
      y = table(data1$gender) + 1,
      labels = table(data1$gender),
      pos = 3,
      cex = 1
    )
  }
  {
    datos <- sort(table(data1$country), decreasing = TRUE)
    barplot(main = "Frecuencias de cada país",
            datos,
            ylim = c(0, 6000))
    text(
      x = barplot(datos, plot = FALSE),
      y = datos + 1,
      labels = datos,
      pos = 3,
      cex = 1
    )
  }
  {
    barplot(
      main = "Frecuencias de cada mano dominante",
      table(data1$hand),
      names.arg = c("NR", "Derecha", "Izquierda", "Ambidiestro"),
      ylim = c(0, 6000)
    )
    text(
      x = barplot(table(data1$hand), plot = FALSE),
      y = table(data1$hand) + 1,
      labels = table(data1$hand),
      pos = 3,
      cex = 1
    )
  }
  
  #### Muestra 2 ####
  data2 <- data[!(rownames(data) %in% rownames(data1)),]
  
  # Solo me quedo con las personas con inglés nativo
  id <- which(data2$engnat != 1)
  data2 <- data2[-id, ]
  
  # Solo me quedo con las personas de US, GB, AU, CA, NZ e IE
  id <-
    which(data2$country %in% c("US", "GB", "AU", "CA", "NZ", "IE", "IN"))
  data2 <- data2[id, ]
  
  {
    boxplot(data2$age, main = "Distribución de la edad", ylab = "Edad")
    text(
      1,
      mean(data2$age),
      paste("Media:", round(mean(data2$age), 2)),
      pos = 4,
      offset = -1.8
    )
    text(
      1,
      median(data2$age),
      paste("Mediana:", median(data2$age)),
      pos = 4,
      offset = 4
    )
    text(1,
         mean(data2$age) + sd(data2$age),
         paste("Desv. Típica:", round(sd(data2$age), 2)),
         pos = 4)
    text(
      1,
      quantile(data2$age, probs = 0.25),
      paste("Q25:", quantile(data2$age, probs = 0.25)),
      pos = 2,
      offset = 4
    )
    text(
      1,
      quantile(data2$age, probs = 0.75),
      paste("Q75:", quantile(data2$age, probs = 0.75)),
      pos = 2,
      offset = 4
    )
  }
  {
    barplot(
      main = "Frecuencias de cada género",
      table(data2$gender),
      names.arg = c("NR", "Hombre", "Mujer", "Otro"),
      ylim = c(0, 2500)
    )
    text(
      x = barplot(table(data2$gender), plot = FALSE),
      y = table(data2$gender) + 1,
      labels = table(data2$gender),
      pos = 3,
      cex = 1
    )
  }
  {
    datos <- sort(table(data2$country), decreasing = TRUE)
    barplot(main = "Frecuencias de cada país",
            datos,
            ylim = c(0, 2500))
    text(
      x = barplot(datos, plot = FALSE),
      y = datos + 1,
      labels = datos,
      pos = 3,
      cex = 1
    )
  }
  {
    barplot(
      main = "Frecuencias de cada mano dominante",
      table(data2$hand),
      names.arg = c("NR", "Derecha", "Izquierda", "Ambidiestro"),
      ylim = c(0, 2500),
    )
    text(
      x = barplot(table(data2$hand), plot = FALSE),
      y = table(data2$hand) + 1,
      labels = table(data2$hand),
      pos = 3,
      cex = 1
    )
  }
  
  
  
  
  #### Muestra 3 ####
  
  data3 <-
    data[!(rownames(data) %in% c(rownames(data1), rownames(data2))),]
  
  {
    boxplot(data3$age, main = "Distribución de la edad", ylab = "Edad")
    text(
      1,
      mean(data3$age),
      paste("Media:", round(mean(data3$age), 2)),
      pos = 4,
      offset = -1.8
    )
    text(
      1,
      median(data3$age),
      paste("Mediana:", median(data3$age)),
      pos = 4,
      offset = 4
    )
    text(1,
         mean(data3$age) + sd(data3$age),
         paste("Desv. Típica:", round(sd(data3$age), 2)),
         pos = 4)
    text(
      1,
      quantile(data3$age, probs = 0.25),
      paste("Q25:", quantile(data3$age, probs = 0.25)),
      pos = 2,
      offset = 4
    )
    text(
      1,
      quantile(data3$age, probs = 0.75),
      paste("Q75:", quantile(data3$age, probs = 0.75)),
      pos = 2,
      offset = 4
    )
  }
  {
    barplot(
      main = "Frecuencias de cada género",
      table(data3$gender),
      names.arg = c("NR", "Hombre", "Mujer", "Otro"),
      ylim = c(0, 6000)
    )
    text(
      x = barplot(table(data3$gender), plot = FALSE),
      y = table(data3$gender) + 1,
      labels = table(data3$gender),
      pos = 3,
      cex = 1
    )
  }
  {
    top_paises <- sort(table(data3$country), decreasing = TRUE)[1:21]
    top_paises <- top_paises[-4] # Elimino los que no han contestado
    top_paises <- rev(top_paises)
    barplot(
      main = "Frecuencias de cada país - Top 20",
      top_paises,
      xlim = c(0, 1500),
      horiz = TRUE,
      names.arg = NA
    )
    text(
      y = barplot(top_paises, plot = FALSE),
      x = top_paises + 1,
      labels = top_paises,
      pos = 4,
      cex = 1
    )
    axis(
      2,
      at = barplot(top_paises, plot = FALSE),
      labels = names(top_paises),
      las = 1
    )
  }
  {
    barplot(
      main = "Frecuencias de cada mano dominante",
      table(data3$hand),
      names.arg = c("NR", "Derecha", "Izquierda", "Ambidiestro"),
      ylim = c(0, 7000),
    )
    text(
      x = barplot(table(data3$hand), plot = FALSE),
      y = table(data3$hand) + 1,
      labels = table(data3$hand),
      pos = 3,
      cex = 1
    )
  }
  

#### Tratamiento previo ####
# Selección de los ítems y recodificación de los inversos y el neuroticismo

inversos <- c(
  "E2",
  "E4",
  "E6",
  "E8",
  "E10",
  "N2",
  "N4",
  "A1",
  "A3",
  "A5",
  "A7",
  "C2",
  "C4",
  "C6",
  "C8",
  "O2",
  "O4",
  "O6"
)
items1 <- data1[, -(1:7)]
items2 <- data2[, -(1:7)]
items3 <- data3[, -(1:7)]
items1[which(colnames(items1) %in% inversos)] <-
  6 - items1[which(colnames(items1) %in% inversos)]
items2[which(colnames(items2) %in% inversos)] <-
  6 - items2[which(colnames(items2) %in% inversos)]
items3[which(colnames(items1) %in% inversos)] <-
  6 - items3[which(colnames(items3) %in% inversos)]

# Se recodifica el neuroticismo para interpretarse en términos de estabilidad emocional
items1[, 11:20] <- 6 - items1[, 11:20]
items2[, 11:20] <- 6 - items2[, 11:20]
items3[, 11:20] <- 6 - items3[, 11:20]

items <- rbind(items1, items2, items3)

#### Matriz de correlaciones ####

letras_y <-
  rev(c(
    "Extroversión",
    "Est. Emocional",
    "Cordialidad",
    "Responsabilidad",
    "Intelecto"
  ))
posiciones_y <- c(7, 17, 27, 37, 47)
par(mfrow = c(3, 1), mar = c(5, 12, 4, 2))

corrplot::corrplot(
  cor(items1),
  tl.cex = 0.5,
  method = "color",
  tl.col = "black"
)
title(
  main = "Muestra 1",
  adj = 0,
  line = -15,
  outer = TRUE
)
axis(
  2,
  at = posiciones_y,
  labels = letras_y,
  las = 1,
  lwd.ticks = 0,
  lwd = 0
)

corrplot::corrplot(
  cor(items2),
  tl.cex = 0.5,
  method = "color",
  tl.col = "black"
)
title(
  main = "Muestra 2",
  adj = 0,
  line = -(15 + 35),
  outer = TRUE
)
axis(
  2,
  at = posiciones_y,
  labels = letras_y,
  las = 1,
  lwd.ticks = 0,
  lwd = 0
)

corrplot::corrplot(
  cor(items3),
  tl.cex = 0.5,
  method = "color",
  tl.col = "black"
)
title(
  main = "Muestra 3",
  adj = 0,
  line = -(15 + 35 * 2),
  outer = TRUE
)
axis(
  2,
  at = posiciones_y,
  labels = letras_y,
  las = 1,
  lwd.ticks = 0,
  lwd = 0
)

#### KMO, Bartlett y normalidad univariante ####

kmo1 <- psych::KMO(items1)
kmo2 <- psych::KMO(items2)
kmo3 <- psych::KMO(items3)

psych::cortest.bartlett(cor(items1), nrow(items1))
psych::cortest.bartlett(cor(items2), nrow(items2))
psych::cortest.bartlett(cor(items3), nrow(items3))

psych::describe(items1)
psych::describe(items2)
psych::describe(items3)

#### Número de factores a extraer ####

# Función de análisis paralelo eficiente - Modificación de una función creada originalmente por Paco Abad #

paralelo_eficiente <-
  function(data,
           it = 100,
           autovalores_mostrados = 10) {
    library(Turbofuns)
    library(ggplot2)
    library(tidyverse)
    r <- PolychoricRM(as.matrix(data))$correlation
    pc.values <- eigen(r)$values
    values <- matrix(NA, it, ncol(data))
    for (i in 1:it) {
      cat("iteracion", i, "\r")
      xR <- sapply(1:ncol(data), function(j)
        sample(data[, j]))
      Rs <-
        PolychoricRM(as.matrix(xR))$correlation                       # Utiliza las policóricas
      values[i,] <- eigen(Rs)$values
    }
    out.pa <- data.frame(
      eigen = 1:length(pc.values),
      emp = pc.values,
      # Y componentes principales
      mean = colMeans(values),
      pc95 = apply(values, 2, quantile, .95)
    )
    out.pa_cases <- out.pa %>%
      gather(key = "variable", value = "valor", -eigen)
    plot(
      ggplot(
        out.pa_cases %>% filter(eigen <= autovalores_mostrados),
        # Y filtra hasta los n primeros autovalores
        aes(
          x = factor(eigen),
          y = valor,
          group = variable,
          col = variable
        )
      ) +
        geom_line() +
        geom_point() +
        theme_classic() +
        xlab("autovalor")
    )
    m <-
      max(which(pc.values > apply(values, 2, quantile, .95)))          # Número de factores a extraer
    list(result = out.pa,
         m = m)
  }


pa1 <- paralelo_eficiente(items1)
pa2 <- paralelo_eficiente(items2)
pa3 <- paralelo_eficiente(items3)

pa1$m     # Devuelve el número de factores a extrar
pa2$m
pa3$m


# Análisis de la estructura interna con redes psicométricas

ega1 <- EGAnet::EGA(items1)
ega2 <- EGAnet::EGA(items2)
ega3 <- EGAnet::EGA(items3)

# jega1 <- EGAnet::hierEGA(items1)
# jega2 <- EGAnet::hierEGA(items2)
# jega3 <- EGAnet::hierEGA(items3)



#### Análisis exploratorio ####

efa1_oblimin <-
  psych::fa.poly(items1,
                 nfactors = 7,
                 fm = "uls",
                 rotate = "oblimin")
efa2_oblimin <-
  psych::fa.poly(items2,
                 nfactors = 7,
                 fm = "uls",
                 rotate = "oblimin")
efa3_oblimin <-
  psych::fa.poly(items3,
                 nfactors = 7,
                 fm = "uls",
                 rotate = "oblimin")
# round(efa1_oblimin$fa$loadings, 2)
# round(efa2_oblimin$fa$loadings, 2)
# round(efa3_oblimin$fa$loadings, 2)

cor_efa1 <-
  cor(efa1_oblimin$fa$loadings, efa1_oblimin$fa$loadings)
cor_efa2 <-
  cor(efa2_oblimin$fa$loadings, efa2_oblimin$fa$loadings)
cor_efa3 <-
  cor(efa3_oblimin$fa$loadings, efa3_oblimin$fa$loadings)

par(mfrow = c(1, 3))
corrplot::corrplot(
  cor(cor_efa1),
  tl.cex = 0.5,
  method = "color",
  tl.col = "black",
  addCoef.col = "black"
)
corrplot::corrplot(
  cor(cor_efa2),
  tl.cex = 0.5,
  method = "color",
  tl.col = "black",
  addCoef.col = "black"
)
corrplot::corrplot(
  cor(cor_efa3),
  tl.cex = 0.5,
  method = "color",
  tl.col = "black",
  addCoef.col = "black"
)


# https://www.datanovia.com/en/lessons/heatmap-in-r-static-and-interactive-visualization/
gplots::heatmap.2(
  efa1_oblimin$fa$loadings,
  scale = "none",
  col = gplots::redblue(100),
  key = FALSE,
  trace = "none",
  density.info = "none",
  cellnote = round(efa1_oblimin$fa$loadings, 2),
  notecol = "black",
  notecex = 0.8,
  Rowv = NULL,
  Colv = NULL,
  dendrogram = "none"
)
gplots::heatmap.2(
  efa2_oblimin$fa$loadings,
  scale = "none",
  col = gplots::redblue(100),
  key = FALSE,
  trace = "none",
  density.info = "none",
  cellnote = round(efa2_oblimin$fa$loadings, 2),
  notecol = "black",
  notecex = 0.8,
  Rowv = NULL,
  Colv = NULL,
  dendrogram = "none"
)
gplots::heatmap.2(
  efa3_oblimin$fa$loadings,
  scale = "none",
  col = gplots::redblue(100),
  key = FALSE,
  trace = "none",
  density.info = "none",
  cellnote = round(efa3_oblimin$fa$loadings, 2),
  notecol = "black",
  notecex = 0.8,
  Rowv = NULL,
  Colv = NULL,
  dendrogram = "none"
)


pesos_ega1 <- EGAnet::net.scores(items1, ega1)$loadings
pesos_ega2 <- EGAnet::net.scores(items2, ega2)$loadings
pesos_ega3 <- EGAnet::net.scores(items3, ega3)$loadings

pesos_ega1$std
pesos_ega2$std
pesos_ega3$std

gplots::heatmap.2(
  pesos_ega1$std,
  scale = "none",
  col = gplots::redblue(100),
  key = FALSE,
  trace = "none",
  density.info = "none",
  cellnote = round(pesos_ega1$std, 2),
  notecol = "black",
  notecex = 0.8,
  Rowv = NULL,
  Colv = NULL,
  dendrogram = "none"
)
gplots::heatmap.2(
  pesos_ega2$std,
  scale = "none",
  col = gplots::redblue(100),
  key = FALSE,
  trace = "none",
  density.info = "none",
  cellnote = round(pesos_ega2$std, 2),
  notecol = "black",
  notecex = 0.8,
  Rowv = NULL,
  Colv = NULL,
  dendrogram = "none"
)
gplots::heatmap.2(
  pesos_ega3$std,
  scale = "none",
  col = gplots::redblue(100),
  key = FALSE,
  trace = "none",
  density.info = "none",
  cellnote = round(pesos_ega3$std, 2),
  notecol = "black",
  notecex = 0.8,
  Rowv = NULL,
  Colv = NULL,
  dendrogram = "none"
)

#### Modelos sobre los 50 ítems ####

library(lavaan) # Función modificada desde una creada por Marcos Romero
compute_cfa <- function(mod,
           est = "wlsmv",
           datos,
           graph = "circle",
           res = FALSE,
           plot = FALSE,
           mean = FALSE,
           ord = TRUE){
    fit <-
      cfa(
        model = mod,
        data = datos,
        estimator = est,
        meanstructure = mean,
        ordered = ord
      )
    if (plot)
      print(semPaths(fit, layout = graph, residuals = res))
    return(list(
      lavaan = fit,
      resumen = summary(fit, fit.measures = TRUE, standardized = TRUE)
    ))
  }


# 5 factores ortogonales

CFA_5_ort <- '
E =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10
N =~ N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9 + N10
A =~ A1 + A2 + A3 + A4 + A5 + A6 + A7 + A8 + A9 + A10
C =~ C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10
O =~ O1 + O2 + O3 + O4 + O5 + O6 + O7 + O8 + O9 + O10
E ~~ 0*N
E ~~ 0*A
E ~~ 0*C
E ~~ 0*O
N ~~ 0*A
N ~~ 0*C
N ~~ 0*O
A ~~ 0*C
A ~~ 0*O
C ~~ 0*O
'

cfa_5_ort <- compute_cfa(CFA_5_ort, datos = items)
cfa_5_ort$resumen


# 5 factores correlacionados

CFA_5_cor <- '
E =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10
N =~ N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9 + N10
A =~ A1 + A2 + A3 + A4 + A5 + A6 + A7 + A8 + A9 + A10
C =~ C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10
O =~ O1 + O2 + O3 + O4 + O5 + O6 + O7 + O8 + O9 + O10
'

cfa_5_cor <- compute_cfa(CFA_5_cor, datos = items)
cfa_5_cor$resumen


# Bifactor

CFA_5_bif <- '
G =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10 +
     N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9 + N10 +
     A1 + A2 + A3 + A4 + A5 + A6 + A7 + A8 + A9 + A10 +
     C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 +
     O1 + O2 + O3 + O4 + O5 + O6 + O7 + O8 + O9 + O10
E =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10
N =~ N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9 + N10
A =~ A1 + A2 + A3 + A4 + A5 + A6 + A7 + A8 + A9 + A10
C =~ C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10
O =~ O1 + O2 + O3 + O4 + O5 + O6 + O7 + O8 + O9 + O10
E ~~ 0*N
E ~~ 0*A
E ~~ 0*C
E ~~ 0*O
N ~~ 0*A
N ~~ 0*C
N ~~ 0*O
A ~~ 0*C
A ~~ 0*O
C ~~ 0*O
E ~~ 0*G
N ~~ 0*G
A ~~ 0*G
C ~~ 0*G
O ~~ 0*G
'

cfa_5_bif <- compute_cfa(CFA_5_bif, datos = items)
cfa_5_bif$resumen

ajuste_cfa_5_bif <- BifactorIndicesCalculator::bifactorIndices(cfa_5_bif$lavaan)$ModelLevelIndices[c(1, 2, 4)]
ajuste_cfa_5_bif <- round(rbind(BIFACTOR = ajuste_cfa_5_bif), 2)
colnames(ajuste) <- c("ECV", "PUC", "OmegaH")
ajuste



# Modelo de intercepto aleatorio (con un factor de método)

CFA_5_met1 <- '
E =~ NA * E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10
N =~ NA * N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9 + N10
A =~ NA * A1 + A2 + A3 + A4 + A5 + A6 + A7 + A8 + A9 + A10
C =~ NA * C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10
O =~ NA * O1 + O2 + O3 + O4 + O5 + O6 + O7 + O8 + O9 + O10
Met =~ (1)*E1 + (1)*E2 + (1)*E3 + (1)*E4 + (1)*E5 + (1)*E6 + (1)*E7 + (1)*E8 + (1)*E9 + (1)*E10 +
     (1)*N1 + (1)*N2 + (1)*N3 + (1)*N4 + (1)*N5 + (1)*N6 + (1)*N7 + (1)*N8 + (1)*N9 + (1)*N10 +
     (1)*A1 + (1)*A2 + (1)*A3 + (1)*A4 + (1)*A5 + (1)*A6 + (1)*A7 + (1)*A8 + (1)*A9 + (1)*A10 +
     (1)*C1 + (1)*C2 + (1)*C3 + (1)*C4 + (1)*C5 + (1)*C6 + (1)*C7 + (1)*C8 + (1)*C9 + (1)*C10 +
     (1)*O1 + (1)*O2 + (1)*O3 + (1)*O4 + (1)*O5 + (1)*O6 + (1)*O7 + (1)*O8 + (1)*O9 + (1)*O10

E ~~ 1*E
N ~~ 1*N
A ~~ 1*A
C ~~ 1*C
O ~~ 1*O

E ~~ 0*N
E ~~ 0*A
E ~~ 0*C
E ~~ 0*O
N ~~ 0*A
N ~~ 0*C
N ~~ 0*O
A ~~ 0*C
A ~~ 0*O
C ~~ 0*O

Met ~~ NA*Met
E ~~ 0*Met
N ~~ 0*Met
A ~~ 0*Met
C ~~ 0*Met
O ~~ 0*Met
'

cfa_5_met1 <-
  compute_cfa(CFA_5_met1,
              datos = items,
              mean = TRUE,
              ord = TRUE)
cfa_5_met1$resumen


# Modelo de con un factor de método por cada factor

CFA_5_met2 <- '
E =~ NA * E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10
N =~ NA * N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9 + N10
A =~ NA * A1 + A2 + A3 + A4 + A5 + A6 + A7 + A8 + A9 + A10
C =~ NA * C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10
O =~ NA * O1 + O2 + O3 + O4 + O5 + O6 + O7 + O8 + O9 + O10
MetE =~ (1)*E1 + (1)*E2 + (1)*E3 + (1)*E4 + (1)*E5 + (1)*E6 + (1)*E7 + (1)*E8 + (1)*E9 + (1)*E10
MetN =~ (1)*N1 + (1)*N2 + (1)*N3 + (1)*N4 + (1)*N5 + (1)*N6 + (1)*N7 + (1)*N8 + (1)*N9 + (1)*N10
MetA =~ (1)*A1 + (1)*A2 + (1)*A3 + (1)*A4 + (1)*A5 + (1)*A6 + (1)*A7 + (1)*A8 + (1)*A9 + (1)*A10
MetC =~ (1)*C1 + (1)*C2 + (1)*C3 + (1)*C4 + (1)*C5 + (1)*C6 + (1)*C7 + (1)*C8 + (1)*C9 + (1)*C10
MetO =~ (1)*O1 + (1)*O2 + (1)*O3 + (1)*O4 + (1)*O5 + (1)*O6 + (1)*O7 + (1)*O8 + (1)*O9 + (1)*O10
E ~~ 1*E
N ~~ 1*N
A ~~ 1*A
C ~~ 1*C
O ~~ 1*O
MetE ~~ NA*MetE
MetN ~~ NA*MetN
MetA ~~ NA*MetA
MetC ~~ NA*MetC
MetO ~~ NA*MetO
E ~~ 0*MetE
E ~~ 0*MetN
E ~~ 0*MetA
E ~~ 0*MetC
E ~~ 0*MetO
N ~~ 0*MetE
N ~~ 0*MetN
N ~~ 0*MetA
N ~~ 0*MetC
N ~~ 0*MetO
A ~~ 0*MetE
A ~~ 0*MetN
A ~~ 0*MetA
A ~~ 0*MetC
A ~~ 0*MetO
C ~~ 0*MetE
C ~~ 0*MetN
C ~~ 0*MetA
C ~~ 0*MetC
C ~~ 0*MetO
O ~~ 0*MetE
O ~~ 0*MetN
O ~~ 0*MetA
O ~~ 0*MetC
O ~~ 0*MetO

MetE ~~ 0*MetN
MetE ~~ 0*MetA
MetE ~~ 0*MetC
MetE ~~ 0*MetO

MetN ~~ 0*MetA
MetN ~~ 0*MetC
MetN ~~ 0*MetO

MetA ~~ 0*MetC
MetA ~~ 0*MetO

MetC ~~ 0*MetO
'

cfa_5_met2 <- compute_cfa(CFA_5_met2, datos = items, mean = TRUE)
cfa_5_met2$resumen


#### Modelos quitando ítems que no están funcionando bien ####

# Detalle de las opciones de respuesta
par(mfrow = c(5, 2), mar = c(5, 5, 2, 2))
barplot(main = "A1", table(items$A1), ylim = c(0, 10000))
barplot(main = "A2", table(items$A2), ylim = c(0, 10000))
barplot(main = "A3", table(items$A3), ylim = c(0, 10000))
barplot(main = "A7", table(items$A7), ylim = c(0, 10000))
barplot(main = "A10", table(items$A10), ylim = c(0, 10000))
barplot(main = "C4", table(items$C4), ylim = c(0, 10000))
barplot(main = "O2", table(items$O2), ylim = c(0, 10000))
barplot(main = "O4", table(items$O4), ylim = c(0, 10000))
barplot(main = "O7", table(items$O7), ylim = c(0, 10000))
barplot(main = "O9", table(items$O9), ylim = c(0, 10000))

alt_items <- items[, -c(21, 22, 23, 27, 30, 34, 42, 44, 47, 49)]

# 5 factores ortogonales
alt_CFA_5_ort <- '
E =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10
N =~ N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9 + N10
A =~ A4 + A5 + A6 + A8 + A9
C =~ C1 + C2 + C3 + C5 + C6 + C7 + C8 + C9 + C10
O =~ O1 + O3 + O5 + O6 + O8 + O10
E ~~ 0*N
E ~~ 0*A
E ~~ 0*C
E ~~ 0*O
N ~~ 0*A
N ~~ 0*C
N ~~ 0*O
A ~~ 0*C
A ~~ 0*O
C ~~ 0*O
'

alt_cfa_5_ort <- compute_cfa(alt_CFA_5_ort, datos = alt_items)
alt_cfa_5_ort$resumen


# 5 factores correlacionados

alt_CFA_5_cor <- '
E =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10
N =~ N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9 + N10
A =~ A4 + A5 + A6 + A8 + A9
C =~ C1 + C2 + C3 + C5 + C6 + C7 + C8 + C9 + C10
O =~ O1 + O3 + O5 + O6 + O8 + O10
'

alt_cfa_5_cor <- compute_cfa(alt_CFA_5_cor, datos = alt_items)
alt_cfa_5_cor$resumen


# Bifactor

alt_CFA_5_bif <- '
G =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10 +
     N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9 + N10 +
     A4 + A5 + A6 + A8 + A9 +
     C1 + C2 + C3 + C5 + C6 + C7 + C8 + C9 + C10 +
     O1 + O3 + O5 + O6 + O8 + O10
E =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10
N =~ N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9 + N10
A =~ A4 + A5 + A6 + A8 + A9
C =~ C1 + C2 + C3 + C5 + C6 + C7 + C8 + C9 + C10
O =~ O1 + O3 + O5 + O6 + O8 + O10
E ~~ 0*N
E ~~ 0*A
E ~~ 0*C
E ~~ 0*O
N ~~ 0*A
N ~~ 0*C
N ~~ 0*O
A ~~ 0*C
A ~~ 0*O
C ~~ 0*O
E ~~ 0*G
N ~~ 0*G
A ~~ 0*G
C ~~ 0*G
O ~~ 0*G
'

alt_cfa_5_bif <- compute_cfa(alt_CFA_5_bif, datos = alt_items)
alt_cfa_5_bif$resumen

ajuste_cfa_5_bif <- BifactorIndicesCalculator::bifactorIndices(alt_cfa_5_bif$lavaan)$ModelLevelIndices[c(1, 2, 4)]
ajuste_cfa_5_bif <- round(rbind(BIFACTOR = ajuste_cfa_5_bif), 2)
colnames(ajuste_cfa_5_bif) <- c("ECV", "PUC", "OmegaH")
ajuste_cfa_5_bif


# Modelo de intercepto aleatorio (con un factor de método)

alt_CFA_5_met1 <- '
E =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10
N =~ N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9 + N10
A =~ A4 + A5 + A6 + A8 + A9
C =~ C1 + C2 + C3 + C5 + C6 + C7 + C8 + C9 + C10
O =~ O1 + O3 + O5 + O6 + O8 + O10
Met =~ (1)*E1 + (1)*E2 + (1)*E3 + (1)*E4 + (1)*E5 + (1)*E6 + (1)*E7 + (1)*E8 + (1)*E9 + (1)*E10 +
     (1)*N1 + (1)*N2 + (1)*N3 + (1)*N4 + (1)*N5 + (1)*N6 + (1)*N7 + (1)*N8 + (1)*N9 + (1)*N10 +
     (1)*A4 + (1)*A5 + (1)*A6 + (1)*A8 + (1)*A9 +
     (1)*C1 + (1)*C2 + (1)*C3 + (1)*C5 + (1)*C6 + (1)*C7 + (1)*C8 + (1)*C9 + (1)*C10 +
     (1)*O1 + (1)*O3 + (1)*O5 + (1)*O6 + (1)*O8 + (1)*O10

E ~~ 1*E
N ~~ 1*N
A ~~ 1*A
C ~~ 1*C
O ~~ 1*O

E ~~ 0*N
E ~~ 0*A
E ~~ 0*C
E ~~ 0*O
N ~~ 0*A
N ~~ 0*C
N ~~ 0*O
A ~~ 0*C
A ~~ 0*O
C ~~ 0*O

Met ~~ NA*Met
E ~~ 0*Met
N ~~ 0*Met
A ~~ 0*Met
C ~~ 0*Met
O ~~ 0*Met
'

alt_cfa_5_met1 <-
  compute_cfa(alt_CFA_5_met1,
              datos = alt_items,
              mean = TRUE,
              ord = TRUE)
alt_cfa_5_met1$resumen


# Modelo de con un factor de método por cada factor

alt_CFA_5_met2 <- '
E =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10
N =~ N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9 + N10
A =~ A4 + A5 + A6 + A8 + A9
C =~ C1 + C2 + C3 + C5 + C6 + C7 + C8 + C9 + C10
O =~ O1 + O3 + O5 + O6 + O8 + O10
MetE =~ (1)*E1 + (1)*E2 + (1)*E3 + (1)*E4 + (1)*E5 + (1)*E6 + (1)*E7 + (1)*E8 + (1)*E9 + (1)*E10
MetN =~ (1)*N1 + (1)*N2 + (1)*N3 + (1)*N4 + (1)*N5 + (1)*N6 + (1)*N7 + (1)*N8 + (1)*N9 + (1)*N10
MetA =~ (1)*A4 + (1)*A5 + (1)*A6 + (1)*A8 + (1)*A9
MetC =~ (1)*C1 + (1)*C2 + (1)*C3 + (1)*C5 + (1)*C6 + (1)*C7 + (1)*C8 + (1)*C9 + (1)*C10
MetO =~ (1)*O1 + (1)*O3 + (1)*O5 + (1)*O6 + (1)*O8 + (1)*O10
E ~~ 1*E
N ~~ 1*N
A ~~ 1*A
C ~~ 1*C
O ~~ 1*O
MetE ~~ NA*MetE
MetN ~~ NA*MetN
MetA ~~ NA*MetA
MetC ~~ NA*MetC
MetO ~~ NA*MetO
E ~~ 0*MetE
E ~~ 0*MetN
E ~~ 0*MetA
E ~~ 0*MetC
E ~~ 0*MetO
N ~~ 0*MetE
N ~~ 0*MetN
N ~~ 0*MetA
N ~~ 0*MetC
N ~~ 0*MetO
A ~~ 0*MetE
A ~~ 0*MetN
A ~~ 0*MetA
A ~~ 0*MetC
A ~~ 0*MetO
C ~~ 0*MetE
C ~~ 0*MetN
C ~~ 0*MetA
C ~~ 0*MetC
C ~~ 0*MetO
O ~~ 0*MetE
O ~~ 0*MetN
O ~~ 0*MetA
O ~~ 0*MetC
O ~~ 0*MetO

MetE ~~ 0*MetN
MetE ~~ 0*MetA
MetE ~~ 0*MetC
MetE ~~ 0*MetO

MetN ~~ 0*MetA
MetN ~~ 0*MetC
MetN ~~ 0*MetO

MetA ~~ 0*MetC
MetA ~~ 0*MetO

MetC ~~ 0*MetO
'

alt_cfa_5_met2 <-
  compute_cfa(alt_CFA_5_met2, datos = alt_items, mean = TRUE)
alt_cfa_5_met2$resumen




#### Resultados ####
cfa_5_ort$resumen
cfa_5_cor$resumen
cfa_5_bif$resumen
cfa_5_met1$resumen
cfa_5_met2$resumen

alt_cfa_5_ort$resumen
alt_cfa_5_cor$resumen
alt_cfa_5_bif$resumen
alt_cfa_5_met1$resumen
alt_cfa_5_met2$resumen

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2))
semPlot::semPaths(
  cfa_5_bif$lavaan,
  "std",
  rotation = 2,
  latents = c("G", "O", "C", "A", "N", "E"),
  layout = "tree2",
  bifactor = "G",
  intercepts = FALSE,
  thresholds = FALSE,
  residuals = FALSE,
  sizeMan = 2.5,
  sizeMan2 = 1,
  asize = 1,
  label.cex = 1,
  exoVar = FALSE,
  edge.label.cex = 0.4,
  fade = FALSE,
  curvePivot = FALSE,
  edge.color = "black",
  mar = c(1, 1, 1, 1),
  edge.width = 1
)

semPlot::semPaths(
  alt_cfa_5_bif$lavaan,
  "std",
  rotation = 2,
  latents = c("G", "O", "C", "A", "N", "E"),
  layout = "tree2",
  bifactor = "G",
  intercepts = FALSE,
  thresholds = FALSE,
  residuals = FALSE,
  sizeMan = 2.5,
  sizeMan2 = 1,
  asize = 1,
  label.cex = 1,
  exoVar = FALSE,
  edge.label.cex = 0.4,
  fade = FALSE,
  curvePivot = FALSE,
  edge.color = "black",
  mar = c(1, 1, 1, 1),
  edge.width = 1
)


#### Invarianza ####

# 50 ítems

items$grupo <- NA
items$grupo[1:nrow(items1)] <- 1
items$grupo[(nrow(items1) + 1):(nrow(items1) + nrow(items2))] <- 2
items$grupo[(nrow(items1) + nrow(items2) + 1):(nrow(items1) + nrow(items2) +
                                                 nrow(items3))] <- 3
# table(items$grupo)

bif1 <- compute_cfa(CFA_5_bif, datos = items1)
bif2 <- compute_cfa(CFA_5_bif, datos = items2)
bif3 <- compute_cfa(CFA_5_bif, datos = items3)

bif1$resumen
bif2$resumen
bif3$resumen

cfa.config <-
  cfa(
    model = CFA_5_bif,
    data = items,
    estimator = "WLSMV",
    group = "grupo"
  )
cfa.metric <-
  cfa(
    model = CFA_5_bif,
    data = items,
    estimator = "WLSMV",
    group = "grupo",
    group.equal = "loadings"
  )
cfa.scalar <-
  cfa(
    model = CFA_5_bif,
    data = items,
    estimator = "WLSMV",
    group = "grupo",
    group.equal = c("loadings", "intercepts")
  )
summary(cfa.config, fit.measures = TRUE, standardized = TRUE)
# summary(cfa.metric, fit.measures = TRUE, standardized = TRUE)
comp <- semTools::compareFit(cfa.config, cfa.metric, cfa.scalar)
summary(comp)


# Reducida

alt_items1 <- items1[, -c(21, 22, 23, 27, 30, 34, 42, 44, 47, 49)]
alt_items2 <- items2[, -c(21, 22, 23, 27, 30, 34, 42, 44, 47, 49)]
alt_items3 <- items3[, -c(21, 22, 23, 27, 30, 34, 42, 44, 47, 49)]

alt_items$grupo <- NA
alt_items$grupo[1:nrow(alt_items1)] <- 1
alt_items$grupo[(nrow(alt_items1) + 1):(nrow(alt_items1) + nrow(alt_items2))] <- 2
alt_items$grupo[(nrow(alt_items1) + nrow(alt_items2) + 1):(nrow(alt_items1) + nrow(alt_items2) + nrow(alt_items3))] <- 3
# table(alt_items$grupo)

alt_bif1 <- compute_cfa(alt_CFA_5_bif, datos = alt_items1)
alt_bif2 <- compute_cfa(alt_CFA_5_bif, datos = alt_items2)
alt_bif3 <- compute_cfa(alt_CFA_5_bif, datos = alt_items3)

alt_bif1$resumen
alt_bif2$resumen
alt_bif3$resumen

alt_cfa.config <-
  cfa(
    model = alt_CFA_5_bif,
    data = alt_items,
    estimator = "WLSMV",
    group = "grupo"
  )
alt_cfa.metric <-
  cfa(
    model = alt_CFA_5_bif,
    data = alt_items,
    estimator = "WLSMV",
    group = "grupo",
    group.equal = "loadings"
  )
alt_cfa.scalar <-
  cfa(
    model = alt_CFA_5_bif,
    data = alt_items,
    estimator = "WLSMV",
    group = "grupo",
    group.equal = c("loadings", "intercepts")
  )
summary(alt_cfa.config,
        fit.measures = TRUE,
        standardized = TRUE)
summary(alt_cfa.metric,
        fit.measures = TRUE,
        standardized = TRUE)
alt_comp <-
  semTools::compareFit(alt_cfa.config, alt_cfa.metric, alt_cfa.scalar)
summary(alt_comp)


# Reducida sin N4

alt2_items1 <- items1[, -c(14, 21, 22, 23, 27, 30, 34, 42, 44, 47, 49)]
alt2_items2 <- items2[, -c(14, 21, 22, 23, 27, 30, 34, 42, 44, 47, 49)]
alt2_items3 <- items3[, -c(14, 21, 22, 23, 27, 30, 34, 42, 44, 47, 49)]

alt2_items$grupo <- NA
alt2_items$grupo[1:nrow(alt2_items1)] <- 1
alt2_items$grupo[(nrow(alt2_items1) + 1):(nrow(alt2_items1) + nrow(alt2_items2))] <- 2
alt2_items$grupo[(nrow(alt2_items1) + nrow(alt2_items2) + 1):(nrow(alt2_items1) + nrow(alt2_items2) + nrow(alt2_items3))] <- 3
# table(alt2_items$grupo)

alt2_bif1 <- compute_cfa(alt2_CFA_5_bif, datos = alt2_items1)
alt2_bif2 <- compute_cfa(alt2_CFA_5_bif, datos = alt2_items2)
alt2_bif3 <- compute_cfa(alt2_CFA_5_bif, datos = alt2_items3)

alt2_bif1$resumen
alt2_bif2$resumen
alt2_bif3$resumen

alt2_cfa.config <-
  cfa(
    model = alt2_CFA_5_bif,
    data = alt2_items,
    estimator = "WLSMV",
    group = "grupo"
  )
alt2_cfa.metric <-
  cfa(
    model = alt2_CFA_5_bif,
    data = alt2_items,
    estimator = "WLSMV",
    group = "grupo",
    group.equal = "loadings"
  )
alt2_cfa.scalar <-
  cfa(
    model = alt2_CFA_5_bif,
    data = alt2_items,
    estimator = "WLSMV",
    group = "grupo",
    group.equal = c("loadings", "intercepts")
  )
# summary(alt2_cfa.config,
#         fit.measures = TRUE,
#         standardized = TRUE)
# summary(alt2_cfa.metric,
#         fit.measures = TRUE,
#         standardized = TRUE)
alt2_comp <-
  semTools::compareFit(alt2_cfa.config, alt2_cfa.metric, alt2_cfa.scalar)
summary(alt2_comp)


#### Invarianza parcial ####

inv_parcial <- lavTestScore(alt_cfa.metric)
inv_parcial <- inv_parcial$uni[inv_parcial$uni$p.value < 0.005, ]
inv_parcial <-
  inv_parcial[order(inv_parcial$X2, decreasing = TRUE), ]
par_inv_parcial <- parTable(alt_cfa.metric)
par_inv_parcial <-
  par_inv_parcial[par_inv_parcial$plabel %in% inv_parcial$rhs, ]
par_inv_parcial <-
  par_inv_parcial[match(inv_parcial$rhs, par_inv_parcial$plabel), , drop = FALSE]
par_inv_parcial$X2 <- inv_parcial$X2
par_inv_parcial

par_alt_cfa.metric <-
  cfa(
    alt_CFA_5_bif,
    alt_items,
    estimator = "WLSMV",
    group = "grupo",
    group.equal = "loadings",
    group.partial = c("G =~ N4", "N =~ N4")
  )
summary(par_alt_cfa.metric,
        fit.measures = TRUE,
        standardized = TRUE)
comp2 <- anova(alt_cfa.config, par_alt_cfa.metric, alt_cfa.scalar)

summary(comp2)
summary(alt_comp)

#### Invarianza sin N4 ####

alt2_items <- alt_items[, -14]

alt2_CFA_5_bif <- '
G =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10 +
     N1 + N2 + N3 + N5 + N6 + N7 + N8 + N9 + N10 +
     A4 + A5 + A6 + A8 + A9 +
     C1 + C2 + C3 + C5 + C6 + C7 + C8 + C9 + C10 +
     O1 + O3 + O5 + O6 + O8 + O10
E =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10
N =~ N1 + N2 + N3 + N5 + N6 + N7 + N8 + N9 + N10
A =~ A4 + A5 + A6 + A8 + A9
C =~ C1 + C2 + C3 + C5 + C6 + C7 + C8 + C9 + C10
O =~ O1 + O3 + O5 + O6 + O8 + O10
E ~~ 0*N
E ~~ 0*A
E ~~ 0*C
E ~~ 0*O
N ~~ 0*A
N ~~ 0*C
N ~~ 0*O
A ~~ 0*C
A ~~ 0*O
C ~~ 0*O
E ~~ 0*G
N ~~ 0*G
A ~~ 0*G
C ~~ 0*G
O ~~ 0*G
'

alt2_cfa_5_bif <- compute_cfa(alt2_CFA_5_bif, datos = alt2_items)
alt2_cfa_5_bif$resumen

par(mfrow = c(1, 3), mar = c(5, 5, 2, 2))
barplot(main = "N4 - Muestra 1",
        table(items1$N4),
        ylim = c(0, nrow(items1) / 2))
barplot(main = "N4 - Muestra 2",
        table(items2$N4),
        ylim = c(0, nrow(items2) / 2))
barplot(main = "N4 - Muestra 3",
        table(items3$N4),
        ylim = c(0, nrow(items3) / 2))

alt2_items1 <-
  items1[, -c(14, 21, 22, 23, 27, 30, 34, 42, 44, 47, 49)]
alt2_items2 <-
  items2[, -c(14, 21, 22, 23, 27, 30, 34, 42, 44, 47, 49)]
alt2_items3 <-
  items3[, -c(14, 21, 22, 23, 27, 30, 34, 42, 44, 47, 49)]

alt2_items$grupo <- NA
alt2_items$grupo[1:nrow(alt2_items1)] <- 1
alt2_items$grupo[(nrow(alt2_items1) + 1):(nrow(alt2_items1) + nrow(alt2_items2))] <- 2
alt2_items$grupo[(nrow(alt2_items1) + nrow(alt2_items2) + 1):(nrow(alt2_items1) + nrow(alt2_items2) + nrow(alt2_items3))] <- 3
# table(alt2_items$grupo)

alt2_bif1 <- compute_cfa(alt2_CFA_5_bif, datos = alt2_items1)
alt2_bif2 <- compute_cfa(alt2_CFA_5_bif, datos = alt2_items2)
alt2_bif3 <- compute_cfa(alt2_CFA_5_bif, datos = alt2_items3)

alt2_bif1$resumen
alt2_bif2$resumen
alt2_bif3$resumen

alt2_cfa.config <-
  cfa(
    model = alt2_CFA_5_bif,
    data = alt2_items,
    estimator = "WLSMV",
    group = "grupo"
  )
alt2_cfa.metric <-
  cfa(
    model = alt2_CFA_5_bif,
    data = alt2_items,
    estimator = "WLSMV",
    group = "grupo",
    group.equal = "loadings"
  )
alt2_cfa.scalar <-
  cfa(
    model = alt2_CFA_5_bif,
    data = alt2_items,
    estimator = "WLSMV",
    group = "grupo",
    group.equal = c("loadings", "intercepts")
  )
summary(alt2_cfa.config,
        fit.measures = TRUE,
        standardized = TRUE)
summary(alt2_cfa.metric,
        fit.measures = TRUE,
        standardized = TRUE)
alt2_comp <- semTools::compareFit(alt2_cfa.config, alt2_cfa.metric, alt2_cfa.scalar)
summary(alt2_comp)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2))
semPlot::semPaths(
  alt2_cfa_5_bif$lavaan,
  "std",
  rotation = 2,
  latents = c("G", "O", "C", "A", "N", "E"),
  layout = "tree2",
  bifactor = "G",
  intercepts = FALSE,
  thresholds = FALSE,
  residuals = FALSE,
  sizeMan = 2.5,
  sizeMan2 = 1,
  asize = 1,
  label.cex = 1,
  exoVar = FALSE,
  edge.label.cex = 0.4,
  fade = FALSE,
  curvePivot = FALSE,
  edge.color = "black",
  mar = c(1, 1, 1, 1),
  edge.width = 1
)


#### Fiabilidad ####

# psych::alpha(items[,1:10])$total
# psych::alpha(items[,11:20])$total
# psych::alpha(items[,21:30])$total
# psych::alpha(items[,31:40])$total
# psych::alpha(items[,41:50])$total

# 50 ítems
f <-
  psych::reliability(keys = list(
    E = grep("^E", names(items), value = TRUE),
    N = grep("^N", names(items), value = TRUE),
    A = grep("^A", names(items), value = TRUE),
    C = grep("^C", names(items), value = TRUE),
    O = grep("^O", names(items), value = TRUE)
  ),
  items)
f <- f$result.df

# Reducida
alt_f <-
  psych::reliability(keys = list(
    E = grep("^E", names(alt_items), value = TRUE),
    N = grep("^N", names(alt_items), value = TRUE),
    A = grep("^A", names(alt_items), value = TRUE),
    C = grep("^C", names(alt_items), value = TRUE),
    O = grep("^O", names(alt_items), value = TRUE)
  ),
  alt_items)
alt_f <- alt_f$result.df

# Reducida sin N4
alt2_f <-
  psych::reliability(keys = list(
    E = grep("^E", names(alt2_items), value = TRUE),
    N = grep("^N", names(alt2_items), value = TRUE),
    A = grep("^A", names(alt2_items), value = TRUE),
    C = grep("^C", names(alt2_items), value = TRUE),
    O = grep("^O", names(alt2_items), value = TRUE)
  ),
  alt2_items)
alt2_f <- alt2_f$result.df

fiabilidad <-
  cbind(round(data.frame(f)[c("alpha", "omega.tot")], 2), 
        round(data.frame(alt_f)[c("alpha", "omega.tot")], 2),
        round(data.frame(alt2_f)[c("alpha", "omega.tot")], 2))
names(fiabilidad) <- c("a50", "o50", "aRed", "oRed", "a-N4", "o-N4")
fiabilidad


#################################### Muestra 1
# 50 ítems
f <-
  psych::reliability(keys = list(
    E = grep("^E", names(items1), value = TRUE),
    N = grep("^N", names(items1), value = TRUE),
    A = grep("^A", names(items1), value = TRUE),
    C = grep("^C", names(items1), value = TRUE),
    O = grep("^O", names(items1), value = TRUE)
  ),
  items1)
f <- f$result.df

# Reducida
alt_f <-
  psych::reliability(keys = list(
    E = grep("^E", names(alt_items1), value = TRUE),
    N = grep("^N", names(alt_items1), value = TRUE),
    A = grep("^A", names(alt_items1), value = TRUE),
    C = grep("^C", names(alt_items1), value = TRUE),
    O = grep("^O", names(alt_items1), value = TRUE)
  ),
  alt_items1)
alt_f <- alt_f$result.df

# Reducida sin N4
alt2_f <-
  psych::reliability(keys = list(
    E = grep("^E", names(alt2_items1), value = TRUE),
    N = grep("^N", names(alt2_items1), value = TRUE),
    A = grep("^A", names(alt2_items1), value = TRUE),
    C = grep("^C", names(alt2_items1), value = TRUE),
    O = grep("^O", names(alt2_items1), value = TRUE)
  ),
  alt2_items1)
alt2_f <- alt2_f$result.df


fiabilidad1 <-
  cbind(round(data.frame(f)[c("alpha", "omega.tot")], 2), 
        round(data.frame(alt_f)[c("alpha", "omega.tot")], 2),
        round(data.frame(alt2_f)[c("alpha", "omega.tot")], 2))
names(fiabilidad1) <- c("a50", "o50", "aRed", "oRed", "a-N4", "o-N4")
fiabilidad1

#################################### Muestra 2
# 50 ítems
f <-
  psych::reliability(keys = list(
    E = grep("^E", names(items2), value = TRUE),
    N = grep("^N", names(items2), value = TRUE),
    A = grep("^A", names(items2), value = TRUE),
    C = grep("^C", names(items2), value = TRUE),
    O = grep("^O", names(items2), value = TRUE)
  ),
  items2)
f <- f$result.df

# Reducida
alt_f <-
  psych::reliability(keys = list(
    E = grep("^E", names(alt_items2), value = TRUE),
    N = grep("^N", names(alt_items2), value = TRUE),
    A = grep("^A", names(alt_items2), value = TRUE),
    C = grep("^C", names(alt_items2), value = TRUE),
    O = grep("^O", names(alt_items2), value = TRUE)
  ),
  alt_items2)
alt_f <- alt_f$result.df

# Reducida sin N4
alt2_f <-
  psych::reliability(keys = list(
    E = grep("^E", names(alt2_items2), value = TRUE),
    N = grep("^N", names(alt2_items2), value = TRUE),
    A = grep("^A", names(alt2_items2), value = TRUE),
    C = grep("^C", names(alt2_items2), value = TRUE),
    O = grep("^O", names(alt2_items2), value = TRUE)
  ),
  alt2_items2)
alt2_f <- alt2_f$result.df

fiabilidad2 <-
  cbind(round(data.frame(f)[c("alpha", "omega.tot")], 2), 
        round(data.frame(alt_f)[c("alpha", "omega.tot")], 2),
        round(data.frame(alt2_f)[c("alpha", "omega.tot")], 2))
names(fiabilidad2) <- c("a50", "o50", "aRed", "oRed", "a-N4", "o-N4")
fiabilidad2

##################################### Muestra 3
# 50 ítems
f <-
  psych::reliability(keys = list(
    E = grep("^E", names(items3), value = TRUE),
    N = grep("^N", names(items3), value = TRUE),
    A = grep("^A", names(items3), value = TRUE),
    C = grep("^C", names(items3), value = TRUE),
    O = grep("^O", names(items3), value = TRUE)
  ),
  items3)
f <- f$result.df

# Reducida
alt_f <-
  psych::reliability(keys = list(
    E = grep("^E", names(alt_items3), value = TRUE),
    N = grep("^N", names(alt_items3), value = TRUE),
    A = grep("^A", names(alt_items3), value = TRUE),
    C = grep("^C", names(alt_items3), value = TRUE),
    O = grep("^O", names(alt_items3), value = TRUE)
  ),
  alt_items3)
alt_f <- alt_f$result.df

# Reducida sin N4
alt2_f <-
  psych::reliability(keys = list(
    E = grep("^E", names(alt2_items3), value = TRUE),
    N = grep("^N", names(alt2_items3), value = TRUE),
    A = grep("^A", names(alt2_items3), value = TRUE),
    C = grep("^C", names(alt2_items3), value = TRUE),
    O = grep("^O", names(alt2_items3), value = TRUE)
  ),
  alt2_items3)
alt2_f <- alt2_f$result.df

fiabilidad3 <-
  cbind(round(data.frame(f)[c("alpha", "omega.tot")], 2), 
        round(data.frame(alt_f)[c("alpha", "omega.tot")], 2),
        round(data.frame(alt2_f)[c("alpha", "omega.tot")], 2))
names(fiabilidad3) <- c("a50", "o50", "aRed", "oRed", "a-N4", "o-N4")
fiabilidad3

#################################### Juntar
l_fiab <- list(
  "Total" = fiabilidad,
  "Muestra 1"  = fiabilidad1,
  "Muestra 2"  = fiabilidad2,
  "Muestra 3"  = fiabilidad3
)

datos <- matrix(ncol = 6, nrow = 0)
colnames(datos) <- c("a50", "o50", "aRed", "oRed", "a-N4", "o-N4")
for (i in 1:5){
  datos <- rbind(datos,
                 l_fiab$Total[i, ],
                 l_fiab$`Muestra 1`[i, ],
                 l_fiab$`Muestra 2`[i, ],
                 l_fiab$`Muestra 3`[i, ])
}
datos







#### Patrones aberrantes ####
# Se ejecuta después de haber hecho el otro análisis y se vuelve a hacer todo el análisis sin la limpieza de datos ni el tratamiento previo 
library(mirt)
library(PerFit)

items1r <- items1[,-length(items1)] - 1
modpoly1 <- mirt(data = items1r, model = 1)
par <- coef(modpoly1, IRTpars = TRUE, simplify = TRUE)$items
set.seed(2810)
lz  <-
  lzpoly(
    items1r,
    IRT.PModel = "GRM",
    Ncat = ncol(par),
    Ability.PModel = "EAP"
  )
aberrantes_1 <- length(lz$PFscores[lz$PFscores < -1.64]) # Número de personas con patrones aberrantes
items1 <- items1[-which(lz$PFscores < -1.64), ]


items2r <- items2[,-length(items2)] - 1
modpoly1 <- mirt(data = items2r, model = 1)
par <- coef(modpoly1, IRTpars = TRUE, simplify = TRUE)$items
set.seed(2810)
lz  <-
  lzpoly(
    items2r,
    IRT.PModel = "GRM",
    Ncat = ncol(par),
    Ability.PModel = "EAP"
  )
aberrantes_2 <- length(lz$PFscores[lz$PFscores < -1.64]) # Número de personas con patrones aberrantes
items2 <- items2[-which(lz$PFscores < -1.64), ]


items3r <- items3[,-length(items3)] - 1
modpoly1 <- mirt(data = items3r, model = 1)
par <- coef(modpoly1, IRTpars = TRUE, simplify = TRUE)$items
set.seed(2810)
lz  <-
  lzpoly(
    items3r,
    IRT.PModel = "GRM",
    Ncat = ncol(par),
    Ability.PModel = "EAP"
  )
aberrantes_3 <- length(lz$PFscores[lz$PFscores < -1.64]) # Número de personas con patrones aberrantes
items3 <- items3[-which(lz$PFscores < -1.64), ]


items <- rbind(items1, items2, items3)

#### Modelos de la escala reducida sin N4 ####


alt2_items <- alt_items[, -14]

# 5 factores ortogonales
alt2_CFA_5_ort <- '
E =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10
N =~ N1 + N2 + N3 + N5 + N6 + N7 + N8 + N9 + N10
A =~ A4 + A5 + A6 + A8 + A9
C =~ C1 + C2 + C3 + C5 + C6 + C7 + C8 + C9 + C10
O =~ O1 + O3 + O5 + O6 + O8 + O10
E ~~ 0*N
E ~~ 0*A
E ~~ 0*C
E ~~ 0*O
N ~~ 0*A
N ~~ 0*C
N ~~ 0*O
A ~~ 0*C
A ~~ 0*O
C ~~ 0*O
'

alt2_cfa_5_ort <- compute_cfa(alt2_CFA_5_ort, datos = alt2_items)
alt2_cfa_5_ort$resumen


# 5 factores correlacionados

alt2_CFA_5_cor <- '
E =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10
N =~ N1 + N2 + N3 + N5 + N6 + N7 + N8 + N9 + N10
A =~ A4 + A5 + A6 + A8 + A9
C =~ C1 + C2 + C3 + C5 + C6 + C7 + C8 + C9 + C10
O =~ O1 + O3 + O5 + O6 + O8 + O10
'

alt2_cfa_5_cor <- compute_cfa(alt2_CFA_5_cor, datos = alt2_items)
alt2_cfa_5_cor$resumen


# Bifactor

alt2_CFA_5_bif <- '
G =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10 +
     N1 + N2 + N3 + N5 + N6 + N7 + N8 + N9 + N10 +
     A4 + A5 + A6 + A8 + A9 +
     C1 + C2 + C3 + C5 + C6 + C7 + C8 + C9 + C10 +
     O1 + O3 + O5 + O6 + O8 + O10
E =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10
N =~ N1 + N2 + N3 + N5 + N6 + N7 + N8 + N9 + N10
A =~ A4 + A5 + A6 + A8 + A9
C =~ C1 + C2 + C3 + C5 + C6 + C7 + C8 + C9 + C10
O =~ O1 + O3 + O5 + O6 + O8 + O10
E ~~ 0*N
E ~~ 0*A
E ~~ 0*C
E ~~ 0*O
N ~~ 0*A
N ~~ 0*C
N ~~ 0*O
A ~~ 0*C
A ~~ 0*O
C ~~ 0*O
E ~~ 0*G
N ~~ 0*G
A ~~ 0*G
C ~~ 0*G
O ~~ 0*G
'

alt2_cfa_5_bif <- compute_cfa(alt2_CFA_5_bif, datos = alt2_items)
alt2_cfa_5_bif$resumen

ajuste_alt2_cfa_5_bif <- BifactorIndicesCalculator::bifactorIndices(alt2_cfa_5_bif$lavaan)$ModelLevelIndices[c(1, 2, 4)]
ajuste_alt2_cfa_5_bif <- round(rbind(BIFACTOR = ajuste_alt2_cfa_5_bif), 2)
colnames(ajuste_alt2_cfa_5_bif) <- c("ECV", "PUC", "OmegaH")
ajuste_alt2_cfa_5_bif


# Modelo de intercepto aleatorio (con un factor de método)

alt2_CFA_5_met1 <- '
E =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10
N =~ N1 + N2 + N3 + N5 + N6 + N7 + N8 + N9 + N10
A =~ A4 + A5 + A6 + A8 + A9
C =~ C1 + C2 + C3 + C5 + C6 + C7 + C8 + C9 + C10
O =~ O1 + O3 + O5 + O6 + O8 + O10
Met =~ (1)*E1 + (1)*E2 + (1)*E3 + (1)*E4 + (1)*E5 + (1)*E6 + (1)*E7 + (1)*E8 + (1)*E9 + (1)*E10 +
     (1)*N1 + (1)*N2 + (1)*N3 + (1)*N5 + (1)*N6 + (1)*N7 + (1)*N8 + (1)*N9 + (1)*N10 +
     (1)*A4 + (1)*A5 + (1)*A6 + (1)*A8 + (1)*A9 +
     (1)*C1 + (1)*C2 + (1)*C3 + (1)*C5 + (1)*C6 + (1)*C7 + (1)*C8 + (1)*C9 + (1)*C10 +
     (1)*O1 + (1)*O3 + (1)*O5 + (1)*O6 + (1)*O8 + (1)*O10

E ~~ 1*E
N ~~ 1*N
A ~~ 1*A
C ~~ 1*C
O ~~ 1*O

E ~~ 0*N
E ~~ 0*A
E ~~ 0*C
E ~~ 0*O
N ~~ 0*A
N ~~ 0*C
N ~~ 0*O
A ~~ 0*C
A ~~ 0*O
C ~~ 0*O

Met ~~ NA*Met
E ~~ 0*Met
N ~~ 0*Met
A ~~ 0*Met
C ~~ 0*Met
O ~~ 0*Met
'

alt2_cfa_5_met1 <-
  compute_cfa(alt2_CFA_5_met1,
              datos = alt2_items,
              mean = TRUE,
              ord = TRUE)
alt2_cfa_5_met1$resumen


# Modelo de con un factor de método por cada factor

alt2_CFA_5_met2 <- '
E =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10
N =~ N1 + N2 + N3 + N5 + N6 + N7 + N8 + N9 + N10
A =~ A4 + A5 + A6 + A8 + A9
C =~ C1 + C2 + C3 + C5 + C6 + C7 + C8 + C9 + C10
O =~ O1 + O3 + O5 + O6 + O8 + O10
MetE =~ (1)*E1 + (1)*E2 + (1)*E3 + (1)*E4 + (1)*E5 + (1)*E6 + (1)*E7 + (1)*E8 + (1)*E9 + (1)*E10
MetN =~ (1)*N1 + (1)*N2 + (1)*N3 + (1)*N5 + (1)*N6 + (1)*N7 + (1)*N8 + (1)*N9 + (1)*N10
MetA =~ (1)*A4 + (1)*A5 + (1)*A6 + (1)*A8 + (1)*A9
MetC =~ (1)*C1 + (1)*C2 + (1)*C3 + (1)*C5 + (1)*C6 + (1)*C7 + (1)*C8 + (1)*C9 + (1)*C10
MetO =~ (1)*O1 + (1)*O3 + (1)*O5 + (1)*O6 + (1)*O8 + (1)*O10
E ~~ 1*E
N ~~ 1*N
A ~~ 1*A
C ~~ 1*C
O ~~ 1*O
MetE ~~ NA*MetE
MetN ~~ NA*MetN
MetA ~~ NA*MetA
MetC ~~ NA*MetC
MetO ~~ NA*MetO
E ~~ 0*MetE
E ~~ 0*MetN
E ~~ 0*MetA
E ~~ 0*MetC
E ~~ 0*MetO
N ~~ 0*MetE
N ~~ 0*MetN
N ~~ 0*MetA
N ~~ 0*MetC
N ~~ 0*MetO
A ~~ 0*MetE
A ~~ 0*MetN
A ~~ 0*MetA
A ~~ 0*MetC
A ~~ 0*MetO
C ~~ 0*MetE
C ~~ 0*MetN
C ~~ 0*MetA
C ~~ 0*MetC
C ~~ 0*MetO
O ~~ 0*MetE
O ~~ 0*MetN
O ~~ 0*MetA
O ~~ 0*MetC
O ~~ 0*MetO

MetE ~~ 0*MetN
MetE ~~ 0*MetA
MetE ~~ 0*MetC
MetE ~~ 0*MetO

MetN ~~ 0*MetA
MetN ~~ 0*MetC
MetN ~~ 0*MetO

MetA ~~ 0*MetC
MetA ~~ 0*MetO

MetC ~~ 0*MetO
'

alt2_cfa_5_met2 <-
  compute_cfa(alt2_CFA_5_met2, datos = alt2_items, mean = TRUE)
alt2_cfa_5_met2$resumen
















