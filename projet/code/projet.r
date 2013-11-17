
compute_freq <- function(vector, xlabels, ylabels){
  c <- vector
  f <- table(c)

  res = matrix(c(f[[1]], f[[2]], f[[1]]/length(c), f[[2]]/length(c)),
    nrow=2, ncol=2) 
  write.table(res, file=filename, sep="\t", row.names=FALSE, col.names=FALSE, append=TRUE)
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
  print(data_to_write)
  sink()
  #write.table(data_to_write, file_to_save, append=TRUE) #does shitty print
}




data <- read.table("../resources/ProjetR.txt", header=TRUE, sep="*", skip=1)

s <- split(data, data$PROV)

for(i in 1:length(s)){
  p <- s[i]

  n <- names(p)
  n <- gsub(" ", "_", n)
  filename <- paste(n, ".txt", sep="")

  file.create(filename)
  fd<-file("output.txt")

  cat("Statistiques descriptives pour le province  Brabant Wallon  : \n", file=filename, append=TRUE)
  cat("_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _  \n\n", file=filename, append=TRUE)
  cat("Nombre de patient ayant subi l'événement versus les patients censurés : \n\n", file=filename, append=TRUE)
  #compute censure freq
  compute_freq(p[[1]]$CENS, 0, 0)

  cat("\nNombre de patient ayant reçu le traitement A vs les patients ayant reçu le traitement B : \n\n", file=filename, append=TRUE)
  #compute treatment freq
  compute_freq(p[[1]]$TRT, 0, 0)

  cat("\nStatistiques descriptives pour les concernant les temps de rechute\nainsi que l'âge des patients ayant subi une rechute :\n\n", file=filename, append=TRUE)
  #compute desc stats
  stat_descr(p[[1]][p[[1]]["CENS"]==1,], filename)

}

