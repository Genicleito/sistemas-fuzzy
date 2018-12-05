library("frbs")

## Le o dataset
dataset <- read.csv("dados-sem-nominais.csv", header = TRUE, sep = ",")

# Remove os inscritos que nao informaram o tipo da escola que estudaram
dataset <- dataset[ dataset$TP_ESCOLA != 1,  ]

dataset <- transformarNominalNumerico(dataset)

## Divide o dataset
data.train <- dataset[1 : ( nrow(dataset) * 2 / 3 ), ]
data.tst <- dataset[ ( (nrow(dataset) * 2 / 3) + 1 ) :  nrow(dataset), -ncol(dataset) ]
real.val <- dataset[ ( (nrow(dataset) * 2 / 3) + 1 ) :  nrow(dataset), ncol(dataset) ]

## Define o intervalo dos dados
range.data <- apply(data.train, 2, range)

## 
method.type <- "WM"

default.num.linguistics.terms <- 5

## Considerar aplicar valores padroes em todos e valores especificos para atributos categoricos
num.fvalinput <- matrix(
    c(default.num.linguistics.terms, ## NU_IDADE
        2, ## TP_SEXO
        default.num.linguistics.terms, ## TP_COR_RACA
        # default.num.linguistics.terms, ## TP_ESCOLA
        default.num.linguistics.terms, ## Q006
        default.num.linguistics.terms, ## Q027
        default.num.linguistics.terms, ## NU_NOTA_CN
        default.num.linguistics.terms, ## NU_NOTA_CH
        default.num.linguistics.terms, ## NU_NOTA_LC
        default.num.linguistics.terms, ## NU_NOTA_MT
        default.num.linguistics.terms ## NU_NOTA_REDACAO
    ),
    nrow = 1)

ling.terms.NU_IDADE <- c("jovem", "adulto", "idoso")
ling.terms.TP_SEXO <- c("masculino", "feminino")
ling.terms.TP_COR_RACA <- c("branca", "parda", "preta")
# ling.terms.TP_ESCOLA <- c("publica", "privada", "exterior")
ling.terms.Q006 <- c("baixa", "media", "alta")  ## Renda A = 0, B = ate 937, ...
ling.terms.Q027 <- c("publica", "publica.e.privada", "privada")
ling.terms.NU_NOTA_CN <- c("baixa", "media", "alta")
ling.terms.NU_NOTA_CH <- c("baixa", "media", "alta")
ling.terms.NU_NOTA_LC <- c("baixa", "media", "alta")
ling.terms.NU_NOTA_MT <- c("baixa", "media", "alta")
## Define o numero de termos linguisticos da variavel de saida NU_NOTA_REDACAO
ling.terms.NU_NOTA_REDACAO <- c("baixa", "media", "alta")

# Parametros do aprendizado
type.defuz <- "WAM"
type.tnorm <- "MIN"
type.snorm <- "MAX"
type.implication.func <- "LUKASIEWICZ"
type.mf <- "WM"

# Nome
name <- "Rule-Generation-5regions"

# Nome das variaveis
colnames.var <- colnames(dataset)

# control <- list(num.labels = 3,
#     type.mf = "TRIANGLE",
#     type.tnorm = type.tnorm,
#     type.snorm = type.snorm,
#     type.implication.func = "LUKASIEWICZ",
#     name = name
# )
control <- list(num.labels = 5,
    type.tnorm = "MIN",
    type.snorm = "MAX",
    type.defuz = "WAM",
    type.implication.func = "LUKASIEWICZ",
    name = "Rule-Generation-5_-_regions",
    type.mf = "TRIANGLE"
)

## Inicia o aprendizado
object.reg <- frbs.learn(data.train, range.data, method.type, control)

## Testa o modelo
res.test <- c()
for(i in 1:nrow(data.tst)) {
  cat("Testando... ", i, "\n")
  res.test <- c(res.test, predict(object.reg, data.tst[i, ]))
}
# res.test <- predict(object.reg, data.tst)
write.csv(res.test, file = "predicao-obtida-5regioes.txt", row.names = FALSE, col.names = FALSE)

for(i in 1:length(object.reg)) { write.csv(object.reg[i], paste(i, ".csv"), sep = ",") }

summary(object.reg)