

data <- read.table("../resources/ProjetR.txt", header=TRUE, sep="*", skip=1)

s <- split(x, x$PROV)

for(i in 1:length(s)){
  
}

bw <- s[1]
bxl <- s[2]
fl <- s[3]



#
# data is a clean matrix where all of the elements are meaningful
# for the statistical measurement
# for this case it will be data[data["CENS"] == 1,] 
#
stat_descr <- function(data, file_to_save){
  #todo : improve the function to hava generic col name => synthax to search
  #over col names ? 
  summary_T = summary(data$T)
  summary_AGE = summary(data$AGE)
  sd_T = sd(data$T)
  sd_AGE = sd(data$AGE)

  data_to_write <- matrix(c(summary_T, sd_T), c(summary_AGE, sd_AGE))
  rownames(data_to_write) <- c("Min", "Q25", "Moyenne", "Medianne","Q75",
			       "Max", "Ecart-type")
  colnames(data_to_write) <- c("T","AGE")
  write.table(data_to_write, file_to_save)
}
