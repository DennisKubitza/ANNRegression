# Title: Simulation Routines
# Author: Dennis Oliver Kubitza
# E-Mail: dennis.kubitza@bibb.de / denn_kubi@freenet.de
# -----------------------------------------------------
# Creates one sample datasets based on on different distributions.
# The simulation is model-agnostic, generated variables will be independent
# All variables are named according to their distribution. 
# The regeneration of the dataset can be triggered with the underlying function
# Only input parameters are n and seed
# -----------------------------------------------------
# In general we sample from 11 feasible parameterized sample distributions <dist>.
# If the distribution is parametrized we consider the values <val> for the parameters:  
#       - par \in (0,1) all steps of size 0,1
#       - par \in (0, infinity) steps from size 2^-3 starting from 0 until 2^3
#       - par \in (-infiinity,infinity) steps from size 2^-3 starting from 0 to 2^3
#       - par discrete  \in (0,10)
# For each combination of parameters, we sample at least 25 variables <varNo>. 
# Normal and Uniorm distribution receive 200 variables
# For each N one random number is drawn for all variables
# This leads to a dataset of N rows with variables encoded as <dist>_<varNo>_<val1>_<val2>.....


# A function generating samples based on the 
generate_samples<-function(seed = 1, n=1)
{
  # Integrety Checks for Function
  if(seed <1 || !is.integer(seed)){
    ?warning("seed has to be a natural number bigger than one. Seed is set to 1.")
    seed <- 1
  }
  if(n <1 || !is.integer(n)){
    ?warning("n has to be a natural number bigger than one. n is set to 1.")
    n <- 1
  }
  set.seed(seed)
  # Build up data frame: Create ID
  ID <- 1:n
  df <- data.frame(ID)
  
  # NORMAL Distribution
  for(i in 1:200)
  {
   col <- rnorm(n)
   df <- cbind(df, col)
   colnames(df)[length(colnames(df))] <-  paste0("rnorm_",i) 
  }

  # Uniform Distribution
  for(i in 1:200)
  {
    col <- runif(n)
    df <- cbind(df, col)
    colnames(df)[length(colnames(df))] <-  paste0("runif_",i) 
  }
  
  # Bernoulli Distribution
  for(i in 1:25){
    for(p in ((1:9)/10)){
      col <- rbinom(n,1,p)
      df <- cbind(df, col)
      colnames(df)[length(colnames(df))] <-  paste0("rbern_",i,"_",p) 
    }
  } 
  
  # Binomial Distribution
  for(i in 1:25){
    for(p in ((1:9)/10)){
      for(no in 1:10){
      col <- rbinom(n,no,p)
      df <- cbind(df, col)
      colnames(df)[length(colnames(df))] <-  paste0("rbinom_",i,"_",no,"_",p) 
      }
    }
  } 
  
  # Geometric Distribution
  for(i in 1:25){
    for(p in ((1:9)/10)){
        col <- rgeom(n,p)
        df <- cbind(df, col)
        colnames(df)[length(colnames(df))] <-  paste0("rgeom_",i,"_",p) 
    }
  } 
  
  # Poisson Distribution
  for(i in 1:25){
    for(lambda in seq(0,8,0.125)){
      col <- rpois(n,lambda)
      df <- cbind(df, col)
      colnames(df)[length(colnames(df))] <-  paste0("rpois_",i,"_",lambda) 
    }
  } 
  
  #Exponential Distribution
  for(i in 1:25){
    for(lambda in seq(0.125,8,0.125)){
      col <- rexp(n,lambda)
      df <- cbind(df, col)
      colnames(df)[length(colnames(df))] <-  paste0("rexp_",i,"_",lambda) 
    }
  } 

  #Hypergeometric Distribution Number on Positive feedbacks on currently open Job applications
  for(i in 1:5){
    for(m in 1:10){
      for(k in 1:10){
        for (l in 1:10)
          {
            if(l<min(k,m)){
              col <- rhyper(n,m,k,l)
              df <- cbind(df, col)
              colnames(df)[length(colnames(df))] <-  paste0("rhyper_",i,"_",m,"_",k,"_",l) 
            }
        }
      }
      }
  }

  
  #Multinomial_with_equal_probabilites
  for(i in 1:10){
    for(m in 1:10){
      col <- t(rmultinom(n, size = 1, prob = rep(1,m)))%*%(1:m)
      df <- cbind(df, col)
      colnames(df)[length(colnames(df))] <-  paste0("rmultinom_uni_",i,"_",m) 
    }
  }
  
  #Multinomial_with_linear_increasing_probabilities
  for(i in 1:10){
    for(m in 1:10){
      col <- t(rmultinom(n, size = 1, prob = (1:m)))%*%(1:m)
      df <- cbind(df, col)
      colnames(df)[length(colnames(df))] <-  paste0("rmultinom_lin_",i,"_",m) 
    }
  }
  
return(df)
}

test <- generate_samples(1,2)