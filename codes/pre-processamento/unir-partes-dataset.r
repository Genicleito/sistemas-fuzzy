source("pre-processamento.r")

# As notas nas competencias podem nao ser interessantes pois elas já dão uma noção da nota final da redação
competencias.redacao <- c("NU_NOTA_COMP1", "NU_NOTA_COMP2", "NU_NOTA_COMP3", "NU_NOTA_COMP4", "NU_NOTA_COMP5")

###############################################################################

# Atributos realmente selecionados
atributos.selecionados <- c("NU_IDADE", "TP_SEXO", "TP_COR_RACA", "TP_ESCOLA",
                            "Q006", "Q027", "NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT",
                            "NU_NOTA_REDACAO")
# atributos.selecionados <- c("NO_MUNICIPIO_RESIDENCIA", "NU_IDADE", "TP_SEXO", "TP_COR_RACA", "TP_ESCOLA",
#                           "Q006", "Q027", "NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT",
#                           "NU_NOTA_REDACAO")

# enem2017 <- read.csv("aa.csv", header = TRUE, sep = ";")
enem2017 <- read.csv("aa.csv", header = TRUE, sep = ";")

# Nomes das colunas
atributos.dataset <- colnames(enem2017)

# Remove treineiros
enem2017 <- enem2017[ -listarTreineiros(enem2017[ , "IN_TREINEIRO"]), ]

# Filtra estudantes da Bahia apenas
enem2017 <- enem2017[ enem2017$TP_COR_RACA != 0, ]
enem2017 <- enem2017[ enem2017$TP_ESCOLA != 1, ]
enem2017 <- enem2017[ enem2017$SG_UF_RESIDENCIA == "BA", ]

# Remove atributos que n�o ser�o utilizados
enem2017 <- enem2017[, atributos.selecionados]


files.names <- paste("a", letters[seq(2, 26)], sep = "")
files.names <- c(files.names, "ba")

dataset.atributos.relevantes <- enem2017[ -linhasComNA(enem2017), ]

# write.csv(dataset.atributos.relevantes, file = "partes-sem-na/aa", row.names = FALSE, col.names = TRUE, sep = ";")

for(file.name in files.names) {
  cat("Lendo arquivo...", file.name, "\n")
  file <- read.csv(file.name, header = FALSE, sep = ";")
  cat("Terminou de ler o arquivo", file.name, "\n")
  
  # Atribui o header do dataset original ao dataset part
  colnames(file) = atributos.dataset
  
  # Remove todos os treineiros
  # dataSet[ dataSet$IN_TREINEIRO != 1,  ]
  file[ -listarTreineiros(file[ , "IN_TREINEIRO"]), ]
  
  # Filtra estudantes da Bahia apenas
  file <- file[ file$TP_COR_RACA != 0, ]
  file <- file[ file$TP_ESCOLA != 1, ]
  file <- file[ file$SG_UF_RESIDENCIA == "BA", ]
  
  # Aplica o filtro de atributos selecionados
  file <- file[ , atributos.selecionados]
  
  linhasExcluidas <- linhasComNA(file)
  file <- file[ -linhasExcluidas, ]
  # write.csv(file, file = paste("partes-sem-na/", file.name, sep = ""), row.names = FALSE, col.names = TRUE, sep = ";")
  
  dataset.atributos.relevantes <- rbind(dataset.atributos.relevantes, file)
  # dataset.atributos.relevantes <- rbind(dataset.atributos.relevantes, file)
}

# file <- read.csv("../base-enem-pre-processada.csv", header = TRUE, sep = ",")
# dataset.atributos.relevantes <- file[ -linhasComNA(file), ]

write.csv(dataset.atributos.relevantes, file = "base-enemBA-pre-processada.csv", row.names = FALSE, col.names = TRUE, sep = ";")

