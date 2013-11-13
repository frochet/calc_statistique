
#TODO
#makes the file to read an argument
#makes the file to write an argument
data <- read.table("../resources/ProjetR.txt", header=TRUE, sep="*", skip=1)

s <- split(x, x$PROV)
# TODO : Ne pas oublier de gerer les valeurs manquantes
for( name in names(s)){
  filtredData = data[data$PROV==name,]
  #3 function call for the 3 part of the report !
  stat_censure(data, paste(name, ".txt")
  stat_trt(data, paste(name, ".txt")
  stat_descr(filtredData[filtredData["CENS"]==1,], paste(name, ".txt"))
}

#
# data is a clean matrix where all of the elements are meaningful
# for the statistical measurement
# for this case it will be data[data["CENS"] == 1,] 
#
stat_descr <- function(data, file_to_save){
  #todo : improve the function to hava generic col name => synthax to search
  #over col names ? 
  summary_T <- summary(data$T)
  names <- names(summary_T)
  names(summary_T) <- NULL
  summary_AGE <- summary(data$AGE)
  names(summary_AGE) <- NULL
  sd_T <- sd(data$T)
  sd_AGE <- sd(data$AGE)

  data_to_write <- matrix(c(summary_T, sd_T), c(summary_AGE, sd_AGE))
  rownames(data_to_write) <- c(names, "Ecart-type")
  colnames(data_to_write) <- c("T","AGE")
  write.table(data_to_write, file_to_save)
}
stat_censure <- function(data, file_to_save){


}
stat_trt(data, file_to_save){

}
