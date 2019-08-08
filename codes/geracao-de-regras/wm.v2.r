source("https://raw.githubusercontent.com/Genicleito/sistemas-fuzzy/master/membership-functions/funcoesPertinenciaFuzzy.r")

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
triangular.fuzzy.regions.points = function(values, domain.interval, n.regions) {
  min.value = domain.interval[[1]]
  max.value = domain.interval[[2]]
  
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
step2 = function(given.data.pairs, points.fuzzy.function, type.fuzzy.function = "tg") {
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
  
  # for each value of this variable
  for (x in given.data.pairs) {
    degrees.of.value = c()
    
    # for each fuzzy region
    for (i in seq(to = length(points.fuzzy.function), by = jump)) {
      a = points.fuzzy.function[i]
      m = points.fuzzy.function[i + 1]
      b = points.fuzzy.function[i + 2]
      
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


# Example: testing the method
n.regions = 5
regions.points = triangular.fuzzy.regions.points(values = data$Sepal.Length, domain.interval = domain.interval[, 1], n.regions = n.regions)


# Step 2 - Generate Fuzzy Rules from Givem Data pairs
step2(data$Sepal.Length, regions.points[, 1])
