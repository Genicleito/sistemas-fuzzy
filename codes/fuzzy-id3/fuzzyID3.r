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
  sum(pmin(as.numeric(unlist(E)), as.numeric(unlist(C)))) / sum(as.numeric(unlist(E)))
}

# # Definition 9 - C is all sub categories of variable C
# normalized.possible.distribution = function(E, C) {
#   r = apply(C, 2, fuzzy.evidence, E)
#   r / max(r)
# }

# Definition 5 - Ambiguity or nonspecificity measure
attribute.ambiguity = function(p.d) {
  p.d = c(p.d, 0)
  r = c()
  for(i in 1:(length(p.d) - 1)) {
    r = c(r, (p.d[i] - p.d[i + 1])*log(i))
  }
  sum(r)
}

# Ambiguity of A let evidence E
calculateAmbiguityWithEvidence = function(E, A) {
  A = data.frame(A)
  e = data.frame(E)
  if (nrow(A) < nrow(e)) {
    for (i in 1:(nrow(e) - nrow(A))) {
      A = rbind(A, 0)
    }
  }
  attribute.ambiguity(
    sort(normalized.possible.distribution(E, A), decreasing = T)
  )
}

# Calculate the fuzzy classification ambiguity (step 1 of classification) - Checked
calculateAttributeAmbiguity = function(A) {
  A = data.frame(A)
  r = c()
  # attribute.ambiguity(as.numeric(sort(apply(A, 1, max), decreasing = T)))
  for(i in 1:nrow(A)) {
    if (max(A[i, ]) == 0) {
      A[i, 1] = 1
    }
    r = c(r, attribute.ambiguity(as.numeric(sort(A[i, ] / max(A[i, ]), decreasing = T))))
  }
  mean(r)
}

# for calculate intersection between two attributes
calculateIntersection = function(X, Y) {
  pmin(X, Y)
  # r = c()
  # for(i in 1:length(X)) {
  #   x = X[i]
  #   if (x %in% Y) {
  #     r = c(r, x)
  #   }
  # }
  # data.frame(r)
}

# # Definition 12 - Equation 11
# calc.weight.fuzzy.partition = function(e, E, y) {
#   # sum(pmin(e, f)) / sum(apply(E, 2, pmin, f))
#   a = subsethood(e, y)
#   r = c()
#   for (j in 1:ncol(E)) {
#     yj = E[, j]
#     r = c(r, subsethood(e, yj))
#   }
#   b = max(r)
#   a / b
# }

classification.ambiguity = function(E, C) {
  r = c()
  for (j in 1:ncol(C)) {
    r = c(r, normalized.possible.distribution(E, C[, j], C))
  }
  print(r)
  attribute.ambiguity(sort(r, decreasing = T))
}

# Checked
subsethood = function(A, B) {
  sum(pmin(A, B)) / sum(A)
}

# Normalized distribution - C is a set of classes, y is the term to predict
# Definition 12 - Equation 11
normalized.possible.distribution = function(E, Ci, C) {
  r = c()
  a = subsethood(E, Ci)
  for(j in 1:ncol(C)) {
    r = c(r, subsethood(E, C[, j]))
  }
  a / max(r)
}

partition.ambiguity = function(P, f) {
  # # README: A parte que calcula o G(X ^ Y) pode estar errada, olhar melhor. usar calc.weight.fuzzy.partition
  # # # Example:
  # # attribute.ambiguity(calculateIntersection(df[, 3], df[, 4]))
  # # calc.weight.fuzzy.partition(e = df[, 3], E=df[, 1:3], f = df[, 4])
  # r = c()
  # for(j in 1:ncol(P)) {
  #   e = P[, j]
  #   intersection = calculateIntersection(e, f)
  #   # X = calc.weight.fuzzy.partition(e = e, E = P, y = f)
  #   X = calculateAmbiguityWithEvidence(E = intersection, A = P)
  #   Y = attribute.ambiguity(calculateIntersection(f, e))
  #   cat("X = ", X, "\nY = ", Y, "\n\n")
  #   # r = c(r, calc.weight.fuzzy.partition(e = e, E = P, y = f) * attribute.ambiguity(intersection))
  #   r = c(r, X * Y)
  # }
  # r
  # # sum(r)
  col = 3
  x = classification.ambiguity(calculateIntersection(df[, 4], df[, col]), C = df[, 11:13])
  y = sum(calculateIntersection(df[, col], df[, 4]))
  r = 0
  for (j in 1:ncol(df[, 1:3])) {
    r = r + sum(calculateIntersection(df[, 4], df[, j]))
  }
  y/r
}

# # To read my dataset
# train.data = iris[, c(1:4)]
# m = calc.m(train.data, 3)
# mapping.degrees = fill.membership.degree(train.data, m)
# vagueness = apply(mapping.degrees, 2, calc.vagueness)

df = read.csv("../Dropbox/Compartilhados/PGCOMP/Aluno Especial/2019.2/Grafos/Trabalho Final/database.csv")

# Calcule the Gain of partition
partition.ambiguity(df[, 1:3], df[, 4])
