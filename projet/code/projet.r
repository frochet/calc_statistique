
compute_freq <- function(vector, xlabels, ylabels){
  c <- vector
  f <- table(c)

  res = matrix(c(f[[1]], f[[2]], f[[1]]/length(c), f[[2]]/length(c)),
    nrow=2, ncol=2) 
  write.table(res, file=filename, sep="\t", row.names=FALSE, col.names=FALSE, append=TRUE)
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
}



# bw <- s[1]
# bxl <- s[2]
# fl <- s[3]

