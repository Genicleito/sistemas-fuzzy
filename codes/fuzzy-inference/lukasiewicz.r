# Exemplo 2.9. Consideramos os subconjuntos fuzzy:
# (A = 0.4/x1 + 1.0/x2 + 0.6/x3) e (B = 0.8/y1 + 0.4/y2)

A <- c(0.4, 1, 0.6)
B <- c(0.8, 0.4)

lukasiewicz <- function(A, B) {
  R <- matrix(nrow=length(A), ncol = length(B))
  for (i in seq(1, length(A))) {  # (x1, y1), (x1, y2), (x2, y1), (x2, y2), ...
    for(j in seq(1, length(B))) 
      R[i, j] = min(1 - A[i] + B[j], 1)
  }
  return (R)
}

luka <- lukasiewicz(A, B)

# Para a entrada (A = 0.4/x1 + 1.0/x2 + 0.6/x3) tem-se:
# 
# = max[ min(1.0; 0.4); min(0.8; 1.0); min(1.0; 0.6) ] = 0.8;
# ( a relação de todo xi com y1 )
# 
# max[ min(1.0; 0.4); min(0.4; 1.0); min(0.8; 0.6) ] = 0.6
# ( a relação de todo xi com y2 )

entrada <- c(0.4, 1.0, 0.6)

y <- c()
# Relacionar todo xi da entrada com y1, com y2 ...
for(j in c(1:ncol(luka))) {
  tmp <- c()
  for( i in c(1:nrow(luka)))  {
    tmp <- c( tmp, min(luka[i, j], entrada[i]) )
  }
  y <- c(y, max(tmp) )
}
