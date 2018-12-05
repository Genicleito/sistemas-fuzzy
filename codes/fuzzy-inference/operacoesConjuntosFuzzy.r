# Função para calcular a INTERSEÇÃO DE FUNÇÕES TRIANGULARES do conjunto A e B
calculaIntersecaoAxB_triangular <- function (x, aA, mA, bA, aB, mB, bB) {
  intersecaoAB <- c()
  for (i in x) {
    intersecaoAB <- c( intersecaoAB, min(fuzzy_triangular(i, aA, mA, bA),  fuzzy_triangular(i, aB, mB, bB)) )
  }
  return (intersecaoAB)
}

# Função para calcular a UNIÃO do conjunto A e B
calculaUniaoAxB <- function (x, aA, mA, bA, aB, mB, bB) {
  uniaoAB <- c()
  for (i in x) {
    uniaoAB <- c( uniaoAB, max(fT(i, aA, mA, bA),  fT(i, aB, mB, bB)) )
  }
  return (uniaoAB)
}

# Função para calcular o complemento de um certo conjunto fuzzy
complemento <- function (conjuntoFuzzy) {
  plot(x, 1 - conjuntoFuzzy, type = "l", xlim = c(0.5, 7.5), ylim = c(0.039, 1), ylab = "", xlab = "Complemento")
}

# Função para calcular o PRODUTO ALGEBRICO do conjunto A e B
calculaProdutoAB <- function () {
  produtoAB <- c()
  for (i in x) {
    produtoAB <- c( produtoAB, fT(i, aA, mA, bA) *  fT(i, aB, mB, bB) )
  }
  
  plot(x, produtoAB, type = "l", xlim = c(0.5, 7.5), ylim = c(0.039, 1), ylab = "", xlab = "Produto Algébrico de AxB")
}

# Função para calcular a DIFERENÇA LIMITADA do conjunto A e B
CalculaLukasiewiczAB <- function () {
  lukasiewiczAB <- c()
  for (i in x) {
    lukasiewiczAB <- c( lukasiewiczAB, max( 0, fT(i, aA, mA, bA) + fT(i, aB, mB, bB) - 1 ) )
  }
  
  plot(x, lukasiewiczAB, type = "l", xlim = c(0.5, 7.5), ylim = c(0.039, 1), ylab = "", xlab = "Diferença Limitada de AxB")
}

# Função para calcular a DIFERENÇA LIMITADA do conjunto A e B
calculaIntersecaoDrasticaAB <- function () {
  intersecaoDrasticaAB <- c()
  for (i in x) {
    if (fT(i, aA, mA, bA) == 1)
      intersecaoDrasticaAB <- c( intersecaoDrasticaAB, fT(i, aB, mB, bB) )
    else if (fT(i, aB, mB, bB) == 1)
      intersecaoDrasticaAB <- c( intersecaoDrasticaAB, fT(i, aA, mA, bA) )
    else
      intersecaoDrasticaAB <- c( intersecaoDrasticaAB, 0)
  }
  
  plot(x, intersecaoDrasticaAB, type = "l", xlim = c(0.5, 7.5), ylim = c(0.039, 1), ylab = "", xlab = "Interseção Drástica de AxB")
}

# Função para calcular a SOMA ALGEBRICA do conjunto A e B
calculaSomaAlgebricaAxB <- function () {
  somaAlgebricaAB <- c()
  for (i in x) {
    somaAlgebricaAB <- c( somaAlgebricaAB, ( fT(i, aA, mA, bA) + fT(i, aB, mB, bB) - ( fT(i, aA, mA, bA) * fT(i, aB, mB, bB) ) ) )
  }
  
  plot(x, somaAlgebricaAB, type = "l", xlim = c(0.5, 7.5), ylim = c(0.039, 1), ylab = "", xlab = "Soma Algébrica de AxB")
}

# Função para calcular a SOMA LIMITADA do conjunto A e B
calculaSomaLimitadaAxB <- function () {
  somaLimitadaAB <- c()
  for (i in x) {
    somaLimitadaAB <- c( somaLimitadaAB, min( 1, fT(i, aA, mA, bA) + fT(i, aB, mB, bB) ) )
  }
  
  plot(x, somaLimitadaAB, type = "l", xlim = c(0.5, 7.5), ylim = c(0.039, 1), ylab = "", xlab = "Soma Limitada de AxB")
}

# Função para calcular a UNI?O DRÁSTICA do conjunto A e B
calculaUniaoDrasticaAB <- function () {
  uniaoDrasticaAB <- c()
  for (i in x) {
    if (fT(i, aA, mA, bA) == 0)
      uniaoDrasticaAB <- c( uniaoDrasticaAB, fT(i, aB, mB, bB) )
    else if (fT(i, aB, mB, bB) == 0)
      uniaoDrasticaAB <- c( uniaoDrasticaAB, fT(i, aA, mA, bA) )
    else
      uniaoDrasticaAB <- c( uniaoDrasticaAB, 1)
  }
  
  plot(x, uniaoDrasticaAB, type = "l", xlim = c(0.5, 7.5), ylim = c(0.039, 1), ylab = "", xlab = "União Drástica de AxB")
}

# Função que calcula a média aritmética de um conjunto fuzzy
mediaAritmetica <- function (fuzzySet) {
  return( mean(fuzzySet) )
}