##########
# PART A #
##########

# A.4
#TODO comment
compute_cens <- function(vector, file_to_save){
  f <- table(vector)

  data_to_write <- matrix(c(f[[1]], f[[1]]/length(vector), f[[2]], f[[2]]/length(vector)),
    nrow=2, ncol=2) 
  rownames(data_to_write) <- c("Effectif", "Fréquence")
  colnames(data_to_write) <- c("Censuré","Non censuré")

  sink(file=file_to_save, append=TRUE)
  print(data_to_write, digits=2)
  sink()
}

# A.4
#TODO comment
compute_trt <- function(vector, file_to_save){
  f <- table(vector)
  n <- sum(is.na(vector))
  l <- length(vector)
  data_to_write <- matrix(c(f[[1]], f[[1]]/l, f[[2]], f[[2]]/l, n, n/l),
    nrow=2, ncol=3) 
  rownames(data_to_write) <- c("Effectif", "Fréquence")
  colnames(data_to_write) <- c("Traitement A","Traitement B", "NA")

  sink(file=file_to_save, append=TRUE)
  print(data_to_write, digits=2)
  sink()
}

# A.4
# data is a clean matrix where all of the elements are meaningful
# for the statistical measurement
# for this case it will be data[data["CENS"] == 1,] 
#
stat_descr <- function(data, file_to_save){
  #todo : improve the function to hava generic col name  
  T = gsub("[,]",".", data$T)
  AGE = gsub("[,]",".", data$AGE)
  summary_T <- summary(as.vector(T, mode="numeric"), na.rm=TRUE)
  names <- names(summary_T)
  summary_AGE <- summary(as.vector(AGE, mode="numeric"), na.rm=TRUE)
  sd_T <- sd(T, na.rm=TRUE)
  sd_AGE <- sd(AGE, na.rm=TRUE)

  data_to_write <- matrix(c(summary_T, sd_T, summary_AGE[1:6], sd_AGE),
			  nrow=7, ncol=2)
  rownames(data_to_write) <- c(names, "Ecart-type")
  colnames(data_to_write) <- c("T","AGE")
  sink(file=file_to_save, append=TRUE)
  print(data_to_write, digits=4)
  sink()
  #write.table(data_to_write, file_to_save, append=TRUE) #does shitty print
}

# A.4
# TODO comment
compute_stats <- function(s){
  for(i in 1:length(s)){
    p <- s[i]
    n <- names(p)
    n <- gsub(" ", "_", n)
    filename <- paste(n, ".txt", sep="")
    file.create(filename)

    #Part 1
    title = paste("Statistiques descriptives pour la province" , n, ": \n")
    cat(title, file=filename, append=TRUE)
    cat("_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _  \n\n", file=filename, append=TRUE)
    cat("Nombre de patient ayant subi l'événement versus les patients censurés : \n\n", file=filename, append=TRUE)
    #compute censure freq
    compute_cens(p[[1]]$CENS, filename)

    cat("\nNombre de patient ayant reçu le traitement A vs les patients ayant reçu le traitement B : \n\n", file=filename, append=TRUE)
    #compute treatment freq
    compute_trt(p[[1]]$TRT, filename)

    cat("\nStatistiques descriptives pour les concernant les temps de rechute\nainsi que l'âge des patients ayant subi une rechute :\n\n", file=filename, append=TRUE)
    #compute desc stats
    stat_descr(p[[1]][p[[1]]["CENS"]==1,], filename)
  }
}

# A.5
# Standardize a column. 
# To use the function to standardize age, you have to give data$AGE to 
# the parameter data of the function.
#
standardization <- function(data){
  AGE <- as.vector(gsub("[,]", ".", data), mode="numeric")
  sd_AGE <- sd(AGE, na.rm=TRUE)
  mean_AGE <- mean(AGE, na.rm=TRUE)
  return (na.omit((AGE - mean_AGE)/sd_AGE))
}



##########
# PART B #
##########

# B.1, C.3
#TODO comment
plot_graphs <- function(lambda, beta1, beta2, zone){
  jpeg(paste(zone, "posterieure.jpeg", sep="_"), width = 640, height = 640, units = "px", quality = 90)
  par(mfrow = c(2,3), oma=c(0,0,2,0))
  plot(lambda, type = "l", ylab = "lambda", xlab = "nb iteration")
  plot(beta1, type = "l", ylab = "AGE", xlab = "nb iteration")
  plot(beta2, type = "l", ylab = "TRT", xlab = "nb iteration")
  hist(lambda, probability=TRUE, main = "", ylab = "Density", xlab = "lambda")
  lines(density(lambda), col="blue")
  hist(beta1, probability=TRUE, main = "", ylab = "Density", xlab = "AGE")
  lines(density(beta1), col="blue")
  hist(beta2, probability=TRUE, main = "", ylab = "Density", xlab = "TRT")
  lines(density(beta2), col="blue")
  title("Résumé des distributions postérieures", outer=TRUE)
  op <- dev.off()
}

# B.2, C.1
#TODO comment
cox_S <- function(l, t, x, beta){
  return (exp(-l*t)^exp(x%*%beta))
}

# B.2, C.3
#TODO comment
plot_function <- function(lambda, beta1, beta2, zone, data){
  jpeg(paste(zone, "survie.jpeg", sep="_"), width = 640, height = 640, units = "px", quality = 90)
  t <- seq(from = 0, to = 7, by = 0.1)
  lamb <- median(lambda)
  stAge <- standardization(data$AGE) 
  stAge0 <- standardization(data[data$TRT==0,]$AGE) 
  stAge1 <- standardization(data[data$TRT==1,]$AGE) 
  mBeta1 <- median(beta1)
  mBeta2 <- median(beta2)
  beta <- c(mBeta1, mBeta2)
  x0 <- c(median(stAge), 0) #stAge0
  x1 <- c(median(stAge), 1) #stAge1
  S0 <- cox_S(lamb, t, x0, beta)
  S1 <- cox_S(lamb, t, x1, beta)
  plot(t, S0, type = "l", col="blue", main="Fonctions de survie en fonctions des traitements", 
    xlab="time", ylab="Survie estimée")
  lines(t, S1, type = "l", lty = "longdash", col="red")
  op <- dev.off()
}



##########
# PART C #
##########

# C.1
#TODO comment
cox_h <- function(l, x, beta){
  return (l*exp(x%*%beta))
}

# C.1
#TODO comment
#arg:
# - T : the vector of observed times 
# - delta : the vector of censor (same length as T)
# - l : the median of the lambda vector
# - beta : a vector (b1, b2) with bi = median(beta(i)
# - X : a matrix with the standardized age if the patient in the first column 
#       and the treatement in the second one. (nrow = length(T))
#returns : the logarithm of the distribution
post_dist_log <- function(T, delta, l, beta, X){
  stopifnot(length(T)==length(delta), length(T)==length(X[,1]))
  result <- delta*log(cox_h(l, X, beta)) + log(cox_S(l, T, X, beta))
  return (sum(result)-log(l))
}

# C.2
#TODO comment
taux_acceptation <- function(lambda, omega, T, delta, beta, X, param){
  if(param == 1){
    l <- omega
    b <- beta
  }
  else if(param == 2 || param == 3){
    l <- lambda
    b <- beta
    b[param-1] <- omega
  }
  pi_omega <- post_dist_log(T, delta, l, b, X)
  pi_lambda <- post_dist_log(T, delta, lambda, beta, X)
  v <- exp(pi_omega - pi_lambda)
  if (v < 1)
    return (v)
  else
    return (1)
  
}

# C.2
#TODO comment
#param : parametre d'intéret
metropolis_core <-function(T, lambda, delta, sd, beta, X, param){
  omega <- 0
  if(param == 1){
    omega <- lambda + rnorm(1, 0, sd[1])
    #Guard to avoid lambda going below 0
    if(omega <= 0){
      omega <- 0.000001
    }
  } 
  else if(param == 2 || param == 3){
    omega <- beta[param-1] + rnorm(1, 0, sd[param])
  }

  #omega <- lambda + rnorm(1, 0, sd_lambda/2)
  alpha <- taux_acceptation(lambda, omega, T, delta, beta, X, param)
  U <-runif(1, 0, 1)
  if(U < alpha)
    return (omega)
  else{
    if(param == 1){ 
      return (lambda)
    }
    else if(param == 2 || param == 3){
      return (beta[param-1])
    }
  }
}

# C.2
#TODO comment
# M: Nombre d'iterations 
# N: Nombre de paramètres
# T: Le vecteur de temps observés
# delta : indicateurs de censure
# lambda_init : valeur initiale de lambda
# beta_init : les valeurs initiales de (beta1, beta2)
# X : une matrice dont la premiere colonne corespond a l'age standardise du
# patient et dont la deuxieme colonne correspond au traitement recu
# sd_vect : vecteur d'ecart type
##
metropolis <- function(N, T, delta, X, sd_vect=c(0.2, 0.19, 0.27), M=10000, lambda_init=1, beta_init=c(0.4, -0.4)){
  lambda <- lambda_init
  beta1 <- beta_init[1]
  beta2 <- beta_init[2]

  vect_lambda <- c(lambda)
  vect_beta1 <- c(beta1)
  vect_beta2 <- c(beta2)

  for(i in 1:(M-1)){
    #print(i)
    lambda <- metropolis_core(T, lambda, delta, sd_vect, c(beta1, beta2), X, 1)
    beta1 <- metropolis_core(T, lambda, delta, sd_vect, c(beta1, beta2), X, 2)
    beta2 <- metropolis_core(T, lambda, delta, sd_vect, c(beta1, beta2), X, 3)
    vect_lambda <- append(vect_lambda, lambda)
    vect_beta1 <- append(vect_beta1, beta1)
    vect_beta2 <- append(vect_beta2, beta2)
  }
  accept_lambda <- taux_acceptation(lambda, lambda+rnorm(1, 0, sd_vect[1]), T,
           delta, c(beta1, beta2), X, 1)
  accept_beta1 <- taux_acceptation(lambda, beta1+rnorm(1, 0, sd_vect[2]), T,
           delta, c(beta1, beta2), X, 2)
  accept_beta2 <- taux_acceptation(lambda, beta2+rnorm(1, 0, sd_vect[3]), T,
           delta, c(beta1, beta2), X, 3)
  return (c(vect_lambda, accept_lambda, vect_beta1, accept_beta1, vect_beta2, accept_beta2))
}

# C.3
compute_metro <- function(d, n, iterations=10000){
  offset <- 1
  stAge <- standardization(d$AGE)
  trt <- d$TRT
  m <- matrix(append(stAge, trt), ncol=2)
  t <- as.vector(gsub("[,]", ".", d$T), mode="numeric")
  metro <- metropolis(3, t, d$CENS, m, M=iterations)
  l <- metro[offset:(offset+iterations-1)]
  offset <- offset+iterations+1
  b1 <- metro[offset:(offset+iterations-1)]
  offset <- offset+iterations+1
  b2 <- metro[offset:(offset+iterations-1)]

  plot_graphs(l, b1, b2, n)
  plot_function(l, b1, b2, n, d)
}

########
# Main #
########


# Part A
#--------

# A.1
# TODO: set one before submission
# Pour les testeurs du programmes. Vous pouvez aussi executer R directement dans
# repertoire où se trouve ce fichier, cela fonctionnera très bien.
# stwd("répertoire du script")

# A.2
data <- read.table("resources/ProjetR.txt", header=TRUE, sep="*", skip=1)

# A.3
s <- split(data, data$PROV)

# A.4
compute_stats(s)

# A.5
# fait par la fonction standardization()


# Part B
#--------

#Part B.1
lambda <- read.table("resources/lambda_Flandre.txt")
beta1 <- read.table("resources/beta1_Flandre.txt")
beta2 <- read.table("resources/beta2_Flandre.txt")
plot_graphs(lambda[,2], beta1[,2], beta2[,2], "Flandre")

#Part B.2
plot_function(lambda[,2], beta1[,2], beta2[,2], "Flandre", s[3][[1]])


# Part C
#--------

# C.1
# Cfr fonction post_dist_log()

# C.2
# Cfr fonction metropolis()

# C.3
# Would have done otherwize by iterating on s[i], but I get an out of bound error when
#   accessing the element s[i][[1]], don't know why
d <- na.omit(s[1][[1]])
n <- gsub(" ", "_", names(s[1]))
compute_metro(d, n, iterations=10000)

d <- na.omit(s[2][[1]])
n <- gsub(" ", "_", names(s[2]))
compute_metro(d, n, iterations=10000)
