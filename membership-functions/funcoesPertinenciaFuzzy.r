# Função Triangular
fuzzy_triangular <- function (x, a, m, b) {
  if (x == m) {
    return (1)
  }
  if (x >= a && x < m) {
    return ( (x - a) / (m - a)  )
  }
  if (x >= m && x < b) {
    return ( (b - x) / (b - m) )
  }
  return (0)
}

# Função trapezoidal
fuzzy_trapezoidal <- function(x, a, m, n, b) {
  if (x < a || x >= b) {
    return(0)
  }
  if (x >= a & x < m) {
    return((x - a) / (m - a))
  }
  if (x >= m & x < n) {
    return(1)
  }
  if (x >= n & x < b) {
    return((b - x) / (b - n))
  }
}