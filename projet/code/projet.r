#USELESS CHANGE TO PATH
#TODO: set one before submission
#stwd("")
#
#


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

#TODO comment
compute_treat <- function(vector, file_to_save){
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

#
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

#TODO comment
compute_stats <- function(s){
  for(i in 1:length(s)){
    p <- s[i]
    n <- names(p)
    n <- gsub(" ", "_", n)
    filename <- paste(n, ".txt", sep="")
    file.create(filename)

    #Part 1
    cat("Statistiques descriptives pour le province  Brabant Wallon  : \n", file=filename, append=TRUE)
    cat("_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _  \n\n", file=filename, append=TRUE)
    cat("Nombre de patient ayant subi l'événement versus les patients censurés : \n\n", file=filename, append=TRUE)
    #compute censure freq
    compute_cens(p[[1]]$CENS, filename)

    cat("\nNombre de patient ayant reçu le traitement A vs les patients ayant reçu le traitement B : \n\n", file=filename, append=TRUE)
    #compute treatment freq
    compute_treat(p[[1]]$TRT, filename)

    cat("\nStatistiques descriptives pour les concernant les temps de rechute\nainsi que l'âge des patients ayant subi une rechute :\n\n", file=filename, append=TRUE)
    #compute desc stats
    stat_descr(p[[1]][p[[1]]["CENS"]==1,], filename)
  }
}

#
# Fifth point of the Part A. Standardize a column. 
# To use the function to standardize age, you have to give data$AGE to 
# the parameter data of the function.
#
standardization <- function(data){
  AGE <- as.vector(gsub("[,]", ".", data), mode="numeric")
  sd_AGE <- sd(AGE, na.rm=TRUE)
  mean_AGE <- mean(AGE, na.rm=TRUE)
  return (na.omit((AGE - mean_AGE)/sd_AGE))
}

#TODO comment
plot_graphs <- function(lambda, beta1, beta2, zone){
  jpeg(paste(zone, "postérieur.jpeg", sep="_"), width = 640, height = 640, units = "px", quality = 90)
  par(mfrow = c(2,3), oma=c(0,0,2,0))
  plot(lambda[,2], type = "l", ylab = "lambda", xlab = "nb iteration")
  plot(beta1[,2], type = "l", ylab = "AGE", xlab = "nb iteration")
  plot(beta2[,2], type = "l", ylab = "TRT", xlab = "nb iteration")
  hist(lambda[,2], probability=TRUE, main = "", ylab = "Density", xlab = "lambda")
  lines(density(lambda[,2]), col="blue")
  hist(beta1[,2], probability=TRUE, main = "", ylab = "Density", xlab = "AGE")
  lines(density(beta1[,2]), col="blue")
  hist(beta2[,2], probability=TRUE, main = "", ylab = "Density", xlab = "TRT")
  lines(density(beta2[,2]), col="blue")
  title("Résumé des distributions postérieures", outer=TRUE)
  op <- dev.off()
}

#TODO comment
cox_S <- function(l, t, x, beta){
  return (exp(-l*t)^exp(x%*%beta))
}

#TODO comment
cox_h <- function(l, x, beta){
  return (l*exp(x%*%beta))
}

#TODO comment
plot_function <- function(lambda, beta1, beta2, zone, data){
  jpeg(paste(zone, "survie.jpeg", sep="_"), width = 640, height = 640, units = "px", quality = 90)
  t <- seq(from = 0, to = 7, by = 0.1)
  lamb <- median(lambda[,2])
  stAge <- standardization(data$AGE) 
  stAge0 <- standardization(data[data$TRT==0,]$AGE) 
  stAge1 <- standardization(data[data$TRT==1,]$AGE) 
  mBeta1 <- median(beta1[,2])
  mBeta2 <- median(beta2[,2])
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
  stopifnot(length(T)==length(delta), length(T)==legnth(X[,1]))
  acc <- 0
  mX <- median(X[,1])
  for(i in 1:length(T)){
    x <- c(mX, X[i,2])
    iter <- delta[i]*log(cox_h(l, x, beta)) + log(cox_S(l, T[i], x, beta))
    acc <- acc + iter
  }
  return (acc+log(l))
}

########
# Main #
########

#Part A
data <- read.table("../resources/ProjetR.txt", header=TRUE, sep="*", skip=1)
s <- split(data, data$PROV)
compute_stats(s)

#Part B.1
lambda <- read.table("../resources/lambda_Flandre.txt")
beta1 <- read.table("../resources/beta1_Flandre.txt")
beta2 <- read.table("../resources/beta2_Flandre.txt")
plot_graphs(lambda, beta1, beta2, "Flandre")

#Part B.2
plot_function(lambda, beta1, beta2, "Flandre", s[3][[1]])


#Part C.2
##
taux_acceptation <- function(lamda, omega, T, delta, beta, X ){
  pi_omega <- post_dist_log(T, delta, omega, beta, X)
  pi_lamda <- post_dist_log(T, delta, lamda, beta, X)
  v = exp(pi_omega - pi_lamda)
  if (v < 1)
    return v
  else
    return 1
}

#lamda : parametre d'interet de l'algorithme metropolis
#
metropolis_core <-function(T, lamda, delta, sd_lamda, beta, X){
  omega <- lamda + rnorm(1, 0, sd_lamda)
  alpha <- taux_acceptation(lamda, omega, T, delta, beta, X)
  U <-runif(1, 0, 1)
  if(U < alpha)
    return omega
  else
    return lamda
}

##
# M: Nombre d'iterations 
# N: Nombre de paramètres
# T: Le vecteur de temps observé
# delta : indicateurs de censure
# lamda_init : valeur initiale de lamda
# beta_init : les valeurs initiales de (beta1, beta2)
# X : une matrice dont la premiere colonne corespond a l'age standardise du
# patient et dont la deuxieme colonne correspond au traitement recu
# sd_vect : vecteur d'ecart type
##
metropolis <- function(N, T, delta, X, sd_vect, M=10000, lamda_init=1, beta_init=c(0.4, -0.4)){
  lamda <- lamda_init
  beta1 <- beta_init[1]
  beta2 <- beta_init[2]

  for(i in 1:M){
      lamda <- metropolis_core(T, lamda, delta, sd_vect[1], c(beta1, beta2), X)
      beta1 <- metropolis_core(T, beta1, delta, sd_vect[2], c(beta1, beta2), X)
      beta2 <- metropolis_core(T, beta2, delta, sd_vect[3], c(beta1, beta2), X)
  }
  accept_lamda <- taux_acceptation(lamda, lamda+rnorm(1, 0, sd_vect[1]), T,
		 		   delta, c(beta1, beta2), X)
  accept_beta1 <- taux_acceptation(beta1, beta1+rnorm(1, 0, sd_vect[2]), T,
				   delta, c(beta1, beta2), X)
  accept_beta2 <- taux_acceptation(beta2, beta2+rnorm(1, 0, sd_vect[3]), T,
				   delta, c(beta1, beta2), X)
  return c(lamda, accept_lamda, beta1, accept_beta1, beta2, accept_beta2)
}
