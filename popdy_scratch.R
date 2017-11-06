# ************************************************
# *********** Scratch Work to transfer ***********
# *********** to PopDy.r final script ************
# ************************************************

# initialize storage matrix, constants
sim.mat = matrix(nrow = 1000, ncol = 36)
S.full = as.vector(seq(from = 0.15, to = 0.75, by = 0.01))
Z.full = -log(S.full)
i = seq(1:1:1000)
m = matrix(0, nrow = 36, ncol = 36)
# m
stock.numbers = full.sim.release[ ,2]
diag(m)<- stock.numbers
m[upper.tri(m)]<- NA #upper triangular matrix = NA, lower = 0, diag = stocking #s

# *************************************************
# *********** Simulation Development **************
# *************************************************

fish.at.age = function(x){
  non.na.index = as.integer(min(which(!is.na(x$N)))) #find first year of stocking
  x$t = x$t - non.na.index #adjust t to reflect first year
  x = x[complete.cases(x), ] #remove NAs
  for(d in 1:(nrow(x)-1)){
    x$N[d+1] = round(x$N[d]*exp((-x$Z[d]*x$t[d+1])))
  }
  N.export = as.vector(x$N)
  return(x)
}

for(i in 1:length(i)){ #iterate 1000x simulations
  
  # generate parameters for each of 1000 simulation scenarios
  
  for(n in 1:ncol(m)){ #iterate over all columns
    
    test.col = m[ , n]
    N = as.integer(test.col)
    df = as.data.frame(N)
    df$Z = sample(Z.full, 36) #tweak value here to lower mortality rates
    df$t = seq(1:1:36)
    x = df
    
    fish.at.age(x)
    
    # store results for each column
    
    N.export[n] = x[n]$N
      
    }
    # store matrix
  single.sim.result[ , n]<- N.export[n]
  }

  # slice off 2035 row
  # append to row[i] of final df/matrix
}



# ************************************************
# ***************** TESTING **********************
# ************************************************

# testing each for loop

# interior for loop

test.col = m[ , 3]
N = as.integer(test.col)
df = as.data.frame(N)
df$Z = sample(Z.full, 36)
df$t = seq(1:1:36)
df$Nt <- 1

# df$Nt = apply(df, 2, fish.at.age(df$N, df$Z, df$t))

# for(j in 1:length(df)){ #iterate over every entry in col
#   df$Z = sample(Z.full, 1) #different Z every year
#   df$t[j] = j
# } 
# 
# fish.at.age = function(N, Z, j){
#   for(d in 1:length(df)){
#     if(all(is.na(N) == TRUE)){
#       df$Nt[d] = NA
#     }
#     else {
#       df$Nt[d+1] = round((N[d]*(exp(-Z[d]*(t[d])))))
#     }
#   }
# 
# }

ans.col = lapply
non.na.index = min(which(!is.na(test.col))) #find first year of stocking




df$Nt = fish.at.age(df$N, df$Z, df$t)

fish.at.age(df$N, df$Z, df$t)

fish.at.age = function(x){
  non.na.index = as.integer(min(which(!is.na(x$N)))) #find first year of stocking
  x$t = x$t - non.na.index
  x = x[complete.cases(x), ]
  for(d in 1:(nrow(x)-1)){
      x$N[d+1] = x$N[d]*exp((-x$Z[d]*x$t[d+1]))
  }
  return(x)
}

x
x = df
# x$Nt<- NA
fish.at.age(x)

non.na.index = as.integer(min(which(!is.na(x$N)))) #find first year of stocking
x$t = x$t - non.na.index
for(d in 1:(nrow(x)-1)){
  x$Nt[d+1] = x$N[d]*exp((-x$Z[d]*x$t[d+1]))
}

# second for loop 
# over whole matrix

single.sim.result = as.data.frame(matrix(NA, nrow = 36, ncol =36)) # initialize storage dataframe for output

for(n in 1:ncol(m)){ #iterate over all columns

  test.col = m[ , n]
  N = as.integer(test.col)
  df = as.data.frame(N)
  df$Z = sample(Z.full, 36) #tweak value here to lower mortality rate
  df$t = seq(from = 1, to = 36, by = 1)
  x = df
  
  fish.at.age = function(x){
    non.na.index = as.integer(min(which(!is.na(x$N)))) #find first year of stocking
    x$t = x$t - non.na.index
    x = x[complete.cases(x), ]
    for(d in 1:(nrow(x)-1)){
      x$N[d+1] = round(x$N[d]*exp((-x$Z[d]*x$t[d+1])))
    }
    N.export[n] = as.vector(x$N) #export simulated pop numbers
    return(x)
    
  }
  # store results for each column
  single.sim.result[ , n]<- N.export[n]
}

View(single.sim.result)


fish.at.age = function(x){
  non.na.index = as.integer(min(which(!is.na(x$N)))) #find first year of stocking
  x$t = x$t - non.na.index
  x = x[complete.cases(x), ]
  for(d in 1:(nrow(x)-1)){
    x$N[d+1] = round(x$N[d]*exp((-x$Z[d]*x$t[d+1])))
  }
  # N.export[n] = as.vector(x$N) #export simulated pop numbers
  return(x)
  
}
# *******************************************************************
n = 3
test = m[ , 3]
N = as.integer(test)
df = as.data.frame(N)
df$Z = sample(Z.full, 36)
df$t = seq(1:1:36)
x = df

fish.at.age(x) #works on individual columns, need to fix storage
# ********************************************************************

fish.at.age = function(x){
  non.na.index = as.integer(min(which(!is.na(x$N)))) #find first year of stocking
  x$t = x$t - non.na.index #adjust t to reflect first year
  x = x[complete.cases(x), ] #remove NAs
  for(d in 1:(nrow(x)-1)){
    x$N[d+1] = round(x$N[d]*exp((-x$Z[d]*x$t[d+1])))
    if(x)
  }
  # N.export = as.vector(x$N)
  return(x)
}


na.pad = function(x, lent){
  x[1:lent]
}
lent = 36

na.pad(x1$N, lent)

single.sim.result = matrix(NA, nrow = 36, ncol = 36) # initialize storage dataframe for output
lent = 36
# library(rowr)

single.sim.result = matrix(NA, nrow = 36, ncol = 36) # initialize storage dataframe for output
lent = 36

for(n in 1:ncol(m)){ #iterate over all columns
  
  test.col = m[ , n]
  N = as.integer(test.col)
  df = as.data.frame(N)
  df$Z = sample(Z.full, 36) #tweak value here to lower mortality rate
  df$t = seq(1:1:36)
  x = df
  
  if(n <= 36){
    x1 = fish.at.age(x)
    x1.exp = as.vector(na.pad(x1$N, lent))
    single.sim.result[ , n] = x1.exp
  } else {
    test.col[1] <- test.col[36]
    test.col[36] <- NA
    single.sim.result[ , n] = test.col
  }
}

rott = rotate(single.sim.result)
sim.resul = diag(rott)
sim.resul.t = t(sim.resul)



# third loop

# Simulation

# initialize storage matrix, constants
sim.mat = matrix(nrow = 1000, ncol = 36)
S.full = as.vector(seq(from = 0.15, to = 0.75, by = 0.01))
Z.full = -log(S.full) #remove step if Z's are changed
i = seq(1:1:1000)
m = matrix(0, nrow = 36, ncol = 36)
# m
stock.numbers = full.sim.release[ ,2]
diag(m)<- stock.numbers
m[upper.tri(m)]<- NA #upper triangular matrix = NA, lower = 0, diag = stocking #s

for(i in 1:1000){ #iterate 1000x simulations
  
  single.sim.result = matrix(NA, nrow = 36, ncol = 36) # initialize storage dataframe for output
  lent = 36
  
  for(n in 1:ncol(m)){ #iterate over all columns
    
    test.col = m[ , n]
    N = as.integer(test.col)
    df = as.data.frame(N)
    df$Z = sample(Z.full, 36) #tweak value here to lower mortality rate
    df$t = seq(1:1:36)
    x = df
    
    if(n <= 35){
      x1 = fish.at.age(x)
      x1.exp = as.vector(na.pad(x1$N, lent))
      single.sim.result[ , n] = x1.exp
    } else {
      test.col[1] <- test.col[36]
      test.col[36] <- NA
      single.sim.result[ , n] = test.col
    }
  }
  
  rott = rotate(single.sim.result)
  sim.resul = diag(rott)
  sim.resul.t = t(sim.resul)
  
  sim.mat[i, ] = sim.resul.t  
  
  
}

View(sim.mat)

# for(i in 1:1000){ #iterate 1000x simulations
#   
#   single.sim.result = matrix(NA, nrow = 36, ncol = 36) # initialize storage dataframe for output
#   lent = 36
for(i in 1:3){  
  for(n in 1:ncol(m)){ #iterate over all columns
    
    test.col = m[ , n]
    N = as.integer(test.col)
    df = as.data.frame(N)
    df$Z = sample(Z.full, 36) #tweak value here to lower mortality rate
    df$t = seq(1:1:36)
    x = df
    
    if(n <= 35){
      x1 = fish.at.age(x)
      x1.exp = as.vector(na.pad(x1$N, lent))
      single.sim.result[ , n] = x1.exp
    } else {
      test.col[1] <- test.col[36]
      test.col[36] <- NA
      single.sim.result[ , n] = test.col
    }
  }
  
  rott = rotate(single.sim.result)
  sim.resul = diag(rott)
  sim.resul.t = t(sim.resul)
  
  sim.mat[i, ] = sim.resul.t  
  
  
}
View(sim.mat)

# *************************************************
# re-run simulation with fixed Z
# *************************************************

fish.age.fixedZ = function(x){
  
  if(non.na.index < 36){
    x = x[complete.cases(x), ] #remove NAs
    for(d in 1:(nrow(x)-1)){
      x$N[d+1] = round(x$N[d]*exp((-x$Z[d]*x$t[d+1])))
    }
    # N.export = as.vector(x$N)
    return(x)
  } 
  else {
    x$N = as.integer(rnorm(1, 8862, 3616.236))
  }
}  

single.sim.result = matrix(NA, nrow = 36, ncol = 36) # initialize storage dataframe for output
lent = 36

for(n in 1:ncol(m)){ #iterate over all columns
  
  test.col = m[ , n]
  N = as.integer(test.col)
  df = as.data.frame(N)
  df$t = seq(from = 1, to = 36, by = 1)
  x = df
  non.na.index = as.integer(min(which(!is.na(x$N)))) #find first year of stocking
  x$t = x$t - non.na.index #adjust t to reflect first year
  ifelse(n != 36, x = x[complete.cases(x), ], x = x) #remove NAs
  if(n < 36){
    x = x[complete.cases(x), ]
  }
  x$Z = ifelse(x$t <= 3, -log(0.6), -log(0.99))
  
  if(n <= 35){
    x1 = fish.age.fixedZ(x)
    x1.exp = as.vector(na.pad(x1$N, lent))
    single.sim.result[ , n] = x1.exp
  } else {
    x$N[36] = as.integer(rnorm(1, 8862, 3616.236))
    x$N[1] <- x$N[36]
    x$N[36] <- NA
    single.sim.result[ , n] = x$N
  }
  
}
View(single.sim.result)


# **********************************************************
# vary stocking numbers, fixed Z
# **********************************************************
setwd("C:/Users/danwa/OneDrive/Research/Lake Sturgeon/Chapter3/LST Data/LST ABundance")
release = read.csv("UTR.yearly.release.csv")
set.seed(865)

addtl.releases = 22

sim.releases = as.integer(rnorm(addtl.releases, 8862, 3616.236))
year = 2014:2035
sim.releases.df = data.frame(year, sim.releases)
colnames(sim.releases.df) = c("Year", "count.released")
full.sim.release = rbind(release, sim.releases.df)

full.sim.release[15,2]= 14615
full.sim.release[16,2]= 16308

fish.age.fixedZ = function(x){
  
  if(non.na.index < 36){
    x = x[complete.cases(x), ] #remove NAs
    for(d in 1:(nrow(x)-1)){
      x$N[d+1] = round(x$N[d]*exp((-x$Z[d]*x$t[d+1])))
    }
    # N.export = as.vector(x$N)
    return(x)
  } 
  else {
    x$N = as.integer(rnorm(1, 8862, 3616.236))
  }
}  
# 2
na.pad = function(x, lent){
  x[1:lent]
} 

# 3
rotate = function(x){
  t(apply(x, 2, rev))
}



# initialize storage matrix, constants
sim.mat = matrix(nrow = 1000, ncol = 36)
S.full = as.vector(seq(from = 0.15, to = 0.75, by = 0.01))
# Z.full = -log(S.full) #remove step if Z's are changed
i = seq(from = 1, to = 1000, by = 1)
m = matrix(0, nrow = 36, ncol = 36)
# m
stock.numbers = full.sim.release[ ,2]
diag(m)<- stock.numbers
m[upper.tri(m)]<- NA #upper triangular matrix = NA, lower = 0, diag = stocking #s

for(j in 1:1000){ #iterate 1000x simulations
  
  sim.releases = as.integer(rnorm(addtl.releases, 8862, 3616.236))
  year = 2014:2035
  sim.releases.df = data.frame(year, sim.releases)
  colnames(sim.releases.df) = c("Year", "count.released")
  sim.releases.df = rbind(release, sim.releases.df)
  
  sim.releases.df[15,2]= 14615
  sim.releases.df[16,2]= 16308
  
  sim.mat = matrix(nrow = 1000, ncol = 36)
  S.full = as.vector(seq(from = 0.15, to = 0.75, by = 0.01))
  Z.full = -log(S.full) #remove step if Z's are changed
  i = seq(from = 1, to = 1000, by = 1)
  m = matrix(0, nrow = 36, ncol = 36)
  # m
  stock.numbers = sim.releases.df[ ,2]
  diag(m)<- stock.numbers
  m[upper.tri(m)]<- NA #upper triangular matrix = NA, lower = 0, diag = stocking #s
  
  single.sim.result = matrix(NA, nrow = 36, ncol = 36) # initialize storage dataframe for output
  lent = 36
  
  for(n in 1:ncol(m)){ #iterate over all columns
    
    test.col = m[ , n]
    N = as.integer(test.col)
    df = as.data.frame(N)
    df$t = seq(from = 1, to = 36, by = 1)
    x = df
    non.na.index = as.integer(min(which(!is.na(x$N)))) #find first year of stocking
    x$t = x$t - non.na.index #adjust t to reflect first year
    if(n < 36){
      x = x[complete.cases(x), ]
    }
    x$Z = ifelse(x$t <= 3, -log(0.6), -log(0.99))
    
    if(n <= 35){
      x1 = fish.age.fixedZ(x)
      x1.exp = as.vector(na.pad(x1$N, lent))
      single.sim.result[ , n] = x1.exp
    } else {
      x$N[36] = as.integer(rnorm(1, 8862, 3616.236))
      x$N[1] <- x$N[36]
      x$N[36] <- NA
      single.sim.result[ , n] = x$N
    }
    
  }
  
  rott = rotate(single.sim.result)
  sim.resul = diag(rott)
  sim.resul.t = t(sim.resul)
  
  sim.mat[i, ] = sim.resul.t  
  
  
}

View(sim.mat)

sim.mat = matrix(nrow = 1000, ncol = 36)

for(j in 1:1000){ 
  # create new simulated release numbers
  sim.releases = as.integer(rnorm(addtl.releases, 8862, 3616.236))
  year = 2014:2035
  sim.releases.df = data.frame(year, sim.releases)
  colnames(sim.releases.df) = c("Year", "count.released")
  sim.releases.df = rbind(release, sim.releases.df)
  
  sim.releases.df[15,2]= 14615
  sim.releases.df[16,2]= 16308
  
  # sim.mat = matrix(nrow = 1000, ncol = 36)
  # S.full = as.vector(seq(from = 0.15, to = 0.75, by = 0.01))
  # Z.full = -log(S.full) #remove step if Z's are changed
  # i = seq(from = 1, to = 1000, by = 1)
  m = matrix(0, nrow = 36, ncol = 36)
  # m
  stock.numbers = sim.releases.df[ ,2]
  diag(m)<- stock.numbers
  m[upper.tri(m)]<- NA #upper triangular matrix = NA, lower = 0, diag = stocking #s
  
  single.sim.result = matrix(NA, nrow = 36, ncol = 36) # initialize storage dataframe for output
  lent = 36
  
  for(n in 1:ncol(m)){ #iterate over all columns
    
    test.col = m[ , n]
    N = as.integer(test.col)
    df = as.data.frame(N)
    df$t = seq(from = 1, to = 36, by = 1)
    x = df
    non.na.index = as.integer(min(which(!is.na(x$N)))) #find first year of stocking
    x$t = x$t - non.na.index #adjust t to reflect first year
    if(n < 36){
      x = x[complete.cases(x), ]
    }
    x$Z = ifelse(x$t <= 3, -log(0.75), -log(0.99))
    
    if(n <= 35){
      x1 = fish.age.fixedZ(x)
      x1.exp = as.vector(na.pad(x1$N, lent))
      single.sim.result[ , n] = x1.exp
    } else {
      x$N[36] = as.integer(rnorm(1, 8862, 3616.236))
      x$N[1] <- x$N[36]
      x$N[36] <- NA
      single.sim.result[ , n] = x$N
    }
    
  }
  
  rott = rotate(single.sim.result)
  sim.resul = diag(rott)
  sim.resul.t = t(sim.resul)
  
  sim.mat[j, ] = sim.resul.t  
  
  
}
View(sim.mat)
