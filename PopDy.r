# *****************************************************************
# *************** Population Dynamics Modeling ********************
# ******************* UTR Lake Sturgeon ***************************
# ****************** Author: Dan Walker ***************************
# ****************** Began on 27/06/2017 **************************
# *****************************************************************

# FINAL CODE - see popdy_scratch for development and testing

setwd("C:/Users/danwa/OneDrive/Research/Lake Sturgeon/Chapter3/LST Data/LST ABundance")
set.seed(865)


# Data from RMark POPAN Jolly-Seber MLE solution:
# Total UTR population in 2011: 5643
# Weight class - above/below 2500 g
# Length class - <650 mm TL, 650 - 950 mm TL, >950 mm TL
# Phi(survival) for heavy.avg.length = ~100%
# Phi for skinny, average length = 0.42 (0.15 - 0.75 95% CI)


# Annual releases
release = read.csv("UTR.yearly.release.csv")
plot(release)
summary(release) # releases ~ N(8862, 3616.236^2)
# 124,069 fish released 2000-2013

# 20 year classes >15 y.o. (SLSWG) = stock until 2035
addtl.releases = 22

sim.releases = as.integer(rnorm(addtl.releases, 8862, 3616.236))
year = 2014:2035
sim.releases.df = data.frame(year, sim.releases)
colnames(sim.releases.df) = c("Year", "count.released")
full.sim.release = rbind(release, sim.releases.df)

full.sim.release[15,2]= 14615
full.sim.release[16,2]= 16308

# ************************************************************
# end of data entry necessary for Simulation procedures
# ************************************************************

write.csv(full.sim.release, "full.sim.release.csv")

plot(full.sim.release,
     pch = ifelse(full.sim.release$Year >= 2016, 1, 16))
title(main = "Number of Reintroduced Lake Sturgeon")

mean(full.sim.release$count.released) # 9,163 fish released per year on average
sum(full.sim.release$count.released) # 329,872 fish released total

# Catch data
library(readxl)
catch = read_excel("CaptureData2.xlsx", sheet = 3)
View(catch)
l_a = na.omit(data.frame(catch$TL.mm, catch$Age.caught))
colnames(l_a)<-c("TL", "Age")
View(l_a)

boxplot(TL~Age, data = l_a)
plot(TL~Age, data = l_a)
title(main = "Lake Sturgeon Length at Age")

# fit a cubic polynomial to the data NEEDS WORK
l_a_cube = lm(TL~poly(Age, 3), data = l_a)
summary(l_a_cube)

lm.test = data.frame(l_a$Age)
pred.cubic = predict(l_a_cube, x = lm.test, interval = 'confidence', level = 0.99)

plot(TL~Age, data = l_a)
title(main = "Lake Sturgeon Length at Age")
abline(l_a_cube)

lines(pred.cubic[, 1], col = "black", lwd = 3)


cubic = function(x){
  781.7 + 1047.7*(x) + 123.6*(x^2) - 445.9*(x^3)
}
x = l_a$Age

y = cubic(x)

plot(x, y)

# VB growth curve

# *************************************************
# ****************** PopDy ************************
# *************************************************

# initialize storage matrix, constants
sim.mat = matrix(nrow = 1000, ncol = 36)
S.full = as.vector(seq(from = 0.15, to = 0.75, by = 0.01))
Z.full = -log(S.full) #remove step if Z's are changed
i = seq(from = 1 , to = 1000, by = 1000)
m = matrix(0, nrow = 36, ncol = 36)
# m
stock.numbers = full.sim.release[ ,2]
diag(m)<- stock.numbers
m[upper.tri(m)]<- NA #upper triangular matrix = NA, lower = 0, diag = stocking #s

# *************************************************
# *************** Simulation  *********************
# *************************************************
setwd("C:/Users/danwa/OneDrive/Research/Lake Sturgeon/Chapter3/LST Data/LST ABundance")
set.seed(865)
release = read.csv("UTR.yearly.release.csv")


# Necessary functions:
# 1
fish.at.age = function(x){
  
  non.na.index = as.integer(min(which(!is.na(x$N)))) #find first year of stocking
  
  if(non.na.index < 36){
    x$t = x$t - non.na.index #adjust t to reflect first year
    x = x[complete.cases(x), ] #remove NAs
    for(d in 1:(nrow(x)-1)){
      x$N[d+1] = round(x$N[d]*exp((-x$Z[d]*x$t[d+1])))
    } 
  } else {
    x$N = as.integer(rnorm(1, 8862, 3616.236))
    }
    return(x)
}  


# 2
na.pad = function(x, lent){
  x[1:lent]
} 

# 3
rotate = function(x){
  t(apply(x, 2, rev))
}

# Simulation - fixed N0, variable Z
# initialize storage matrix, constants
sim.mat = matrix(nrow = 1000, ncol = 36)
S.full = as.vector(seq(from = 0.15, to = 0.75, by = 0.01))
Z.full = -log(S.full) #remove step if Z's are changed
i = seq(from = 1, to = 1000, by = 1)
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
    df$t = seq(from = 1, to = 36, by = 1)
    x = df
    
    if(n <= 35){
      x1 = fish.at.age(x)
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

write.csv(sim.mat, "Fixed_N0_Varying_Z.csv")

# **********************************************************
# ************ first complete run 20/07/2017 ***************
# **********************************************************
# **********************************************************

# RESET ENVIRONMENT BEFORE RUNNING SECOND SIM!!!

# **********************************************************
# Simulation with fixed N0, fixed Z
# **********************************************************
setwd("C:/Users/danwa/OneDrive/Research/Lake Sturgeon/Chapter3/LST Data/LST ABundance")
set.seed(865)
release = read.csv("UTR.yearly.release.csv")

# New central function:

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
Z.full = -log(S.full) #remove step if Z's are changed
i = seq(from = 1, to = 1000, by = 1)
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
  
  sim.mat[i, ] = sim.resul.t  
  
  
}

View(sim.mat)
write.csv(sim.mat, file = "Fixed_Z_Fixed_N0.csv")

# *************************************************************
# simulation with varying N0, fixed Z
# *************************************************************
setwd("C:/Users/danwa/OneDrive/Research/Lake Sturgeon/Chapter3/LST Data/LST ABundance")
set.seed(865)
release = read.csv("UTR.yearly.release.csv")

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


sim.mat = matrix(nrow = 1000, ncol = 36)
addtl.releases = 22
for(j in 1:1000){ 
  # create new simulated release numbers
  sim.releases = as.integer(rnorm(addtl.releases, 8862, 3616.236))
  year = 2014:2035
  sim.releases.df = data.frame(year, sim.releases)
  colnames(sim.releases.df) = c("Year", "count.released")
  sim.releases.df = rbind(release, sim.releases.df)
  
  sim.releases.df[15,2]= 14615
  sim.releases.df[16,2]= 16308
  
  m = matrix(0, nrow = 36, ncol = 36)
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
sim.mat[sim.mat<0] <- 0
write.csv(sim.mat, file = "Fixed_Z_Varying_N0.csv")


# *************************************************************
# simulation with varying N0, fixed Z, doubled reintroductions
# *************************************************************
setwd("C:/Users/danwa/OneDrive/Research/Lake Sturgeon/Chapter3/LST Data/LST ABundance")
set.seed(865)
release = read.csv("UTR.yearly.release.csv")

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
    x$N = as.integer(rnorm(1, (2*8862), 3616.236))
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


sim.mat = matrix(nrow = 1000, ncol = 36)
addtl.releases = 22
for(j in 1:1000){ 
  # create new simulated release numbers
  sim.releases = as.integer(rnorm(addtl.releases, (8862*2), 3616.236))
  year = 2014:2035
  sim.releases.df = data.frame(year, sim.releases)
  colnames(sim.releases.df) = c("Year", "count.released")
  sim.releases.df = rbind(release, sim.releases.df)
  
  sim.releases.df[15,2]= 14615
  sim.releases.df[16,2]= 16308
  
  m = matrix(0, nrow = 36, ncol = 36)
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
sim.mat[sim.mat<0] <- 0
write.csv(sim.mat, file = "Fixed_Z_Varying_N0_2xReintro.csv")


