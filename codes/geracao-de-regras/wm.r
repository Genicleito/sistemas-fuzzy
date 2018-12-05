source("../../Funcoes-de-Pertinencia/funcoesPertinenciaFuzzy.r")
source("pre-processamento.r")

# Implementacao do Wang&Mendel como descrito no artigo

## Le o dataset
# dataset <- read.csv("dados-sem-nominais.csv", header = TRUE, sep = ",")
dataset <- read.csv("../../Datasets/base-enemBA-pre-processada.csv", header = TRUE, sep = ",")

# Remove os inscritos que nao informaram o tipo da escola que estudaram
dataset <- dataset[ , -c(4) ]

dataset <- transformarNominalNumerico(dataset)

## Divide o dataset
data.train <- dataset[1 : ( nrow(dataset) * 2 / 3 ), ]
data.tst <- dataset[ ( (nrow(dataset) * 2 / 3) + 1 ) :  nrow(dataset), -ncol(dataset) ]
real.val <- dataset[ ( (nrow(dataset) * 2 / 3) + 1 ) :  nrow(dataset), ncol(dataset) ]

## Define o intervalo dos dados
range.data <- apply(data.train, 2, range)

## data <- iris

data <- data.train

# regions <- c("Extremamente.Baixa", "Baixissima", "Muito.Baixa", "Baixa",
#             "Media",
#             "Alta", "Muito.Alta", "Altissima", "Extremamente.Alta")

ling.terms.NU_IDADE <- c("jovem", "adulto", "idoso")
ling.terms.TP_SEXO <- c("masculino", "feminino")
ling.terms.TP_COR_RACA <- c("branca", "preta", "parda", "amarela", "indigena")
# ling.terms.TP_ESCOLA <- c("publica", "privada", "exterior")
ling.terms.Q006 <- c("baixa_1",  "baixa_2", "baixa_3", "baixa_4", "baixa_5", "baixa_6", "baixa_7", "baixa_8",
                     "media",
                     "alta_1", "alta_2", "alta_2", "alta_4", "alta_5", "alta_6", "alta_7", "alta_8")  ## Renda A = 0, B = ate 937, ...
ling.terms.Q027 <- c("publica", "parcialmente.publica.e.privada.sem.bolsa", "parcialmente.publica.e.privada.com.bolsa", "privada.sem.bolsa", "publica.com.bolsa")
ling.terms.NU_NOTA_CN <- c("muito.baixa", "baixa", "media", "alta", "muito.alta")
ling.terms.NU_NOTA_CH <- c("muito.baixa", "baixa", "media", "alta", "muito.alta")
ling.terms.NU_NOTA_LC <- c("muito.baixa", "baixa", "media", "alta", "muito.alta")
ling.terms.NU_NOTA_MT <- c("muito.baixa", "baixa", "media", "alta", "muito.alta")

## Define o numero de termos linguisticos da variavel de saida NU_NOTA_REDACAO
ling.terms.NU_NOTA_REDACAO <- c("muito.baixa", "baixa", "media", "alta", "muito.alta")

regions <- list(# ling.terms.NU_IDADE,
                # ling.terms.TP_SEXO,
                # ling.terms.TP_COR_RACA,
                # ling.terms.Q006,
                # ling.terms.Q027,
                ling.terms.NU_NOTA_CN,
                ling.terms.NU_NOTA_CH,
                ling.terms.NU_NOTA_LC,
                ling.terms.NU_NOTA_MT,
                ling.terms.NU_NOTA_REDACAO
)

# nRegions <- 3
# nRegions <- c(3, 2, 3, 3, 3, 3, 3, 3, 3, 3)
nRegions <- c(5, 5, 5, 5, 5)

# idInicioRegioes <- (length(regions) - nRegions) / 2
# idInicioRegioes <- (length(regions) / 2) - 1

# Funcao para definir intervalos dos atributos da base de dados
# TODO
defineIntervalos <- function(db) {
  range <- c()
  names <- c()
  for(i in c(1 : ncol(db))) {
    # cat("Interval of ", colnames(db)[i], ": [", min(db[ , i]), ", ", max(db[ , i]), "]\n")
    range <- c(range, min(db[ , i]), max(db[ , i]))
    names <- c(names, colnames(db)[i])
  }
  result <- data.frame(matrix(data = range, nrow = 2, ncol = ncol(db), byrow = FALSE))
  colnames(result) <- names
  return (result)
}


# db ?? a base com os intervalos de cada variavel
defineRegioesFuzzy <- function(db, nRegions) {
  regions <- c()
  nRows <- length( seq( min(db[ , 1]), max(db[ , 1]), (( (max(db[ , 1]) - min(db[ , 1]) ) / (max(nRegions) - 1)) ) ) )
  for(i in c(1 : ncol(db))) {
    minimo <- min(db[ , i])
    maximo <- max(db[ , i])
    tamIntervalos <- ( max(db[ , i]) - min(db[ , i]) )
    if(minimo == maximo) {
      regions <- c(regions, (rep(minimo, nRegions[i])))
      
      # for(j in nRegions[i] : max(nRegions)) {
        regions <- c(regions, rep("NA", max(nRegions) - nRegions[i]))
      # }
    } else {
      regions <- c( regions, seq( minimo, maximo, ( tamIntervalos / (nRegions[i] - 1) ) ) )
      
      # for(j in nRegions[i] : max(nRegions)) {
        regions <- c(regions, rep("NA", max(nRegions) - nRegions[i]))
      # }
    }
    # cat("Regions of ", names(db)[i], ":", regions, "\n\n")
  }
  result <- data.frame(matrix(data = regions, nrow = nRows))
  colnames(result) <- names(db)
  return(result)
}

# Continuar...
# Fazer um metodo para iterar sobre todos os elementos do data.train
# @param fuzzyRegionsXi - Regioes Fuzzy da vari??vel Xi
generateFuzzyRules <- function(elem, fuzzyRegionsXi) {
  result <- c()
  regionsXi <- c()
  idRegiaoGrauMaximo <- c()
  
  for(i in 1:length(fuzzyRegionsXi)) {
    if(!is.na(fuzzyRegionsXi[i])) {
      regionsXi <- c(regionsXi, fuzzyRegionsXi[i])
    }
  }
  fuzzyRegionsXi <- regionsXi
  
  for( i in seq(1, length(fuzzyRegionsXi)) ) {

    if(!is.na(fuzzyRegionsXi)) {
      if(i == 1) {
        grau <- fuzzy_triangular(elem, fuzzyRegionsXi[i], fuzzyRegionsXi[i], fuzzyRegionsXi[i + 1])
        if(grau > max(result)) {
          idRegiaoGrauMaximo <- i
        }
        result <- c( result, grau )
      } else if( i == length(fuzzyRegionsXi) ) {
        grau <- fuzzy_triangular(elem, fuzzyRegionsXi[i - 1], fuzzyRegionsXi[i], fuzzyRegionsXi[i])
        if(grau > max(result)) {
          idRegiaoGrauMaximo <- i
        }
        result <- c(result, grau)
      } else {
        grau <- fuzzy_triangular(elem, fuzzyRegionsXi[i - 1], fuzzyRegionsXi[i], fuzzyRegionsXi[i + 1])
        if(grau > max(result)) {
          idRegiaoGrauMaximo <- i
        }
        result <- c(result, grau)
      }
    }
  }
  cat(nrow(result), " regras geradas!")
  
  return( c(max(result), idRegiaoGrauMaximo) )
}

## Remover regras que são idênticas a outras
removerRegrasAmbiguas <- function(data.graus.maximos) {
  num.inputs <- 1:ncol(data.graus.maximos)
  total.removidas <- 0
  regras.final <- data.frame()
  
  for(linha in 1:nrow(data.graus.maximos)) {
    cat("Analisando Regra ", linha, "...\n")
    
    if(!is.na(data.graus.maximos[linha, 1])) {
      
      igual <- 0
      conflitantes <- c()
      for( linhaAux in 1:nrow(data.graus.maximos) ) {
        
        # Verifica se eh uma regra que nao foi excluida por ser igual a outra
        if(!is.na(data.graus.maximos[linhaAux, 1])) {
          # Verifica se eh a mesma linha
          if(linhaAux != linha) {
            # Verifica se uma linha eh diferente das outras
            if(sum(as.numeric( data.graus.maximos[ linha, ] == data.graus.maximos[ linhaAux, ] )) == ncol(data.graus.maximos))  {
              igual <- 1
              conflitantes <- c(conflitantes, linhaAux)
            }
          }
        }
      }
      
      # Eh igual a pelo menos uma regra
      if(igual == 1) {
        rule.max.degree <- conflitantes[1]
        for(i in 1:length(conflitantes)) {
          if( grausMaximosVariaveis[ conflitantes[i], ncol(grausMaximosVariaveis) ] > max(grausMaximosVariaveis[ conflitantes, ncol(grausMaximosVariaveis) ])) {
            rule.max.degree <- conflitantes[i]
          }
        }
        cat(length(conflitantes), " regras equivalentes a regra ", linha, "\n")
        
        # regraEscolhida = data.graus.maximos[rule.max.degree, ]
        regras.final <- rbind(regras.final, data.graus.maximos[rule.max.degree, ])
        data.graus.maximos[c(conflitantes, linha), ] <- NA
        # data.graus.maximos[rule.max.degree, ] = regraEscolhida
        total.removidas <- total.removidas + length(conflitantes)
      }
      
    }
  }
  
  colnames(regras.final) <- colnames(data.graus.maximos)
  return(regras.final)
}

# Chamada de funcoes
# data <- preProcessar(data)
# data.test <- defineTestSet(data)
# data.train <- defineTrainSet(data)
fuzzy.intervals <- defineIntervalos(data.train)
fuzzy.regions <- defineRegioesFuzzy(fuzzy.intervals, nRegions)

# Define a matriz que contem os graus de todas as intancias das variaveis
grausMaximosVariaveis = data.frame( matrix(nrow = nrow(data.train), ncol = ncol(data.train) + 1) )
colnames(grausMaximosVariaveis) = c(names(data), "Degree.Rule")

# Define uma matriz com as regioes correspondente a cada grau m??ximo obtido por cada elemento do dataset
regioesGrausMaximosVariaveis = data.frame( matrix(nrow = nrow(data.train), ncol = ncol(data.train)) )
colnames(regioesGrausMaximosVariaveis) = names(data)

# Define a base de regras
rules.data <- c()

# # Define os graus máximos e a base de regras (numerica)
for( i in seq(1, ncol(data.train)) ) {
  cat("Learning ", colnames(data.train)[i], "...\n")
  for( j in seq(1, nrow(data.train)) ) {
    retorno <- generateFuzzyRules( data.train[j , i], as.numeric(as.character( fuzzy.regions[, i] )) )
    
    grausMaximosVariaveis[j, i] = as.numeric(retorno[1])
    regioesGrausMaximosVariaveis[j, i] = retorno[2]
  }
}


# Determina o grau de cada regra e o armazena na coluna Degree.Rule do data.frame
grausMaximosVariaveis[ , ncol(grausMaximosVariaveis)] = apply(grausMaximosVariaveis[ , 1:(ncol(grausMaximosVariaveis) - 1)], 1, prod)

rules.base.data <- removerRegrasAmbiguas(regioesGrausMaximosVariaveis)
write.csv(grausMaximosVariaveis, "GrausMaximosVariaveis.csv")
# Cria strings que representam as regras
for(linha in seq(1, nrow(regioesGrausMaximosVariaveis))) {
  regra <- paste("IF ")
  for( coluna in seq( 1, ncol(regioesGrausMaximosVariaveis) ) ) {
    if(coluna == ncol(regioesGrausMaximosVariaveis)) {
      # regra <- paste( regra, names(data)[coluna], " IS ", regioesGrausMaximosVariaveis[linha, coluna], "\n\n" )
      regra <- paste( regra, names(data)[coluna], " IS ", unlist(regions[coluna])[regioesGrausMaximosVariaveis[linha, coluna]] )
    } else if( coluna == ncol(regioesGrausMaximosVariaveis) - 1 ) {
      regra <- paste( regra, names(data)[coluna], " IS ", unlist(regions[coluna])[regioesGrausMaximosVariaveis[linha, coluna]], " THEN " )
    } else {
      regra <- paste( regra, names(data)[coluna], " IS ", unlist(regions[coluna])[regioesGrausMaximosVariaveis[linha, coluna]], " AND " )
    }
  }
  # cat(regra)
  rules.data <- c(rules.data, regra)
}