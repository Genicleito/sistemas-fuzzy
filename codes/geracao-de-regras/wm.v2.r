source("https://raw.githubusercontent.com/Genicleito/sistemas-fuzzy/master/membership-functions/funcoesPertinenciaFuzzy.r")
library(rlan)
library(dplyr)

# Dataset to use in algorithm
data = iris[, -ncol(iris)]

######### Functions definition #########
## None


############### Wang & Mendel algorithm ############### 

# Step 1. Divide the input and output spaces into Fuzzy Regions

# Domain interval definition
domain.interval = apply(data, 2, range)
row.names(domain.interval) = c("min", "max")

# Fuzzy regions

# Function to get points of fuzzy triangular functions
triangular.fuzzy.regions.points = function(domain.interval, n.regions) {
  min.value = domain.interval[1]
  max.value = domain.interval[2]
  
  interval.length = abs(max.value - min.value)
  
  if(n.regions > 1) {
    dist.inter.points = interval.length / (n.regions - 1)
  } else if (n.regions == 1) {
    # Only one fuzzy set
    dist.inter.points = interval.length / 2
    points.fuzzy.regions = seq(min.value, max.value, dist.inter.points)
    return (data.frame(values = points.fuzzy.regions, degree = c(0, 1, 0)))
  }
  
  points.fuzzy.regions = seq(min.value, max.value, dist.inter.points)
  
  all.values.points = c()
  all.degrees = c()
  for(i in 1:length(points.fuzzy.regions)) {
    if(i == 1) {
      all.values.points = c(rep(points.fuzzy.regions[i], 2), points.fuzzy.regions[i + 1])
      all.degrees = c(1, 1, 0)
    } else if(i == length(points.fuzzy.regions)) {
      all.values.points = c(all.values.points, points.fuzzy.regions[i - 1], rep(points.fuzzy.regions[i], 2))
      all.degrees = c(all.degrees, 0, 1, 1)
    } else {
      all.values.points = c(all.values.points, points.fuzzy.regions[(i - 1):(i + 1)])
      all.degrees = c(all.degrees, 0, 1, 0)
    }
  }
  data.frame(values = all.values.points, degree = all.degrees)
}

# Function to get max degrees and relatve regions of this max degrees
step2 = function(values.and.fuzzy.points, type.fuzzy.function = "tg") {
  # type.fuzzy.function = "tg" (triangular function)
  # type.fuzzy.function = "tz" (trapezoidal function)
  
  # by default (case type of membership fuzzy function is triangular)
  jump = 3
  
  # doing...
  if (type.fuzzy.function == "tz") {
    jump = 4
  }
  
  # columns of return's dataframe
  max.degrees = c()
  relative.region.of.max.degree = c()
  
  region.values = unlist(values.and.fuzzy.points$values)
  fuzzy.points = unlist(values.and.fuzzy.points$points)
  
  # for each value of this variable
  for (x in region.values) {
    degrees.of.value = c()
    
    # for each fuzzy region
    for (i in seq(to = length(fuzzy.points), by = jump)) {
      a = fuzzy.points[i]
      m = fuzzy.points[i + 1]
      b = fuzzy.points[i + 2]
      
      degrees.of.value = c(degrees.of.value, triangular.memb.function(x, a, m, b))
    }
    # max of degrees obtained for x
    max.degrees = c(max.degrees, max( degrees.of.value ))
    # relative region of the max degree of x
    relative.region.of.max.degree = c(relative.region.of.max.degree, which.max(degrees.of.value))
  }
  # return df with max degrees and relatives fuzzy regions
  data.frame(max.degree = max.degrees, relative.region.of.max.degree = relative.region.of.max.degree)
}

combine.degrees.and.regions = function(max.degrees.and.regions) {
  max.degrees.table = NULL
  regions.max.degree = NULL
  for(i in 1:length(max.degrees.and.regions)) {
    max.degrees.table = cbind(max.degrees.table, max.degrees.and.regions[[i]]$max.degree)
    regions.max.degree = cbind(regions.max.degree, max.degrees.and.regions[[i]]$relative.region.of.max.degree)
  }
  list(max.degrees.table = max.degrees.table, regions.max.degree = regions.max.degree)
}

# Drop duplicates rows in rule base (same antecedents and different consequents)
# Are maintained the rules with the highest degree in the consequent
drop.duplicates = function(rules.base) {
  ant.rules = rules.base %>% colnames %>% rlang::syms()
  
  # consider only the antecedent in deduplication
  ant.rules[[length(ant.rules)]] = NULL
  
  # return the rule base deduplicated, keeping the rules to the highest degree in the consequent
  rules.base %>% arrange(desc(rules.degree)) %>% distinct(!!!cols, .keep_all = TRUE)
}


# Example: testing the method
n.regions = 5
# regions.points = triangular.fuzzy.regions.points(domain.interval, n.regions = n.regions)
regions.points = apply(domain.interval, 2, triangular.fuzzy.regions.points, n.regions)

############ Step 2 - Generate Fuzzy Rules from Givem Data pairs
pairs.values.points = list()
for (i in 1:length(data)){
  pairs.values.points[[i]] = list(values = data[, i], points = regions.points[[i]][1])
}
max.degrees = lapply(pairs.values.points, step2)

############ end step 2 ############

# Combine degrees and fuzzy regions into two dataframes in list
degrees.and.regions = combine.degrees.and.regions(max.degrees)

fuzzy.regions.with.max.degree = as.data.frame(degrees.and.regions$regions.max.degree)
max.degrees.variables = degrees.and.regions$max.degrees.table

############ step 3 ############
# remove ambiguous rules

# getting the rule degree with antecedents product
rules.degree = apply(max.degrees.variables, 1, prod)

# bind of the antecedents fuzzy regions variables with degree of rule
rules = cbind(fuzzy.regions.with.max.degree, rules.degree)

# drop duplicates rows
rules = drop.duplicates(rules)

############ end step 3 ############
