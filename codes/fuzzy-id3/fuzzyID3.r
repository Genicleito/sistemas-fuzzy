calc.m <- function(df, k) {
  values = c()
  for (j in 1:ncol(df)) {
    min.j = min(df[, j])
    max.j = max(df[, j])
    for (i in 1:k) {
      values = c(values, min.j + (max.j - min.j) * (i - 1)/(k - 1))
    }
    # m = cbind(m, values)
  }
  m = as.data.frame(matrix(values, nrow = k))
  # names(m) = names(df)
  m
}

triangular.membership = function(x, m, i) {
    if (i == 1) {
      if (x <= m[1]) {
        return(1)
      }
      if (x >= m[2]) {
        return(0)
      }
      return((m[2] - x) / (m[2] - m[1]))
    } else if (i == length(m)) {
      if (x >= m[i]) {
        return(1)
      }
      if (x <= m[i - 1]) {
        return(0)
      }
      return((x - m[i - 1]) / (m[i] - m[i - 1]))
    } else {
      if (x == m[i]) return(1)
      if (x >= m[i + 1] || x <= m[i - 1]) return(0)
      if (x > m[i] && x < m[i + 1]) return((m[i + 1] - x) / (m[i + 1] - m[i]))
      if (x > m[i - 1] && x < m[i]) return((x - m[i - 1]) / (m[i] - m[i - 1]))
    }
}

adjust.centres = function(centres, df) {
  result = c()
  for (j in 1:ncol(df)) {
    r = 0
    for (x.t in df[, j]) {
      r = r + min(abs(x.t - m[, j]))
    }
    result = c(result, r)
  }
  rbind(result)
}

fill.membership.degree = function(df, m) {
  result = c()
  for (j in 1:ncol(df)) {
    for (k in 1:nrow(m)) {
      for (x in df[, j]) {
        # x = df[i, j]
        # cat("r = ",  triangular.membership(x, m[, j], k), "|", x, "|", m[k, j], "|", k, "\n")
        result = c(result, triangular.membership(x, m[, j], k))
      }
    }
  }
  n = c()
  for(col in colnames(m)) {
    for(k in 1:nrow(m)) {
      n = c(n, paste(col, "_region", k, sep = ""))
    }
  }
  r = as.data.frame(matrix(result, nrow = nrow(df)))
  names(r) = n
  r
}

# Definition 4 -  Vagueness measurement
calc.vagueness = function(U) {
  m = mean(U)
  n = length(U)
  if(m == 0.5) 1
  if(m == 1 || m == 0) 0
  U = U[U != 0]
  U = U[U != 1]
  -(sum(U * log(U) + (1 - U) * log(1 - U))/n)
}

# Definition 7
fuzzy.evidence = function(C, E) {
  sum(pmin(E, C)) / sum(E)
}

# Definition 9
normalized.possible.distribution = function(E, C) {
  r = apply(C, 2, fuzzy.evidence, E)
  r / max(r)
}


train.data = iris[, c(1:4)]
m = calc.m(train.data, 3)
mapping.degrees = fill.membership.degree(train.data, m)
vagueness = apply(mapping.degrees, 2, calc.vagueness)
# normalized.possible.distribution(t.hot, data.frame(p.volleyball, swimming, w_lifting))
