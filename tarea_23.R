setwd("S:/0_Universidad_de_Malaga/ingenieria_y_cicncia_de_datos")
#install.packages(readr)
library(readr)
#data <- read_csv('S:\0 Universidad de Malaga\ingenieria y cicncia de datos\Alzheimer.csv')
data <- read_csv('Alzheimer.csv')
nOfRows <- nrow(data)
nOfCols <- ncol(data)
dimensions <- dim(data)
summaryOfData <- summary(data)
summarySex <- summary(data$SEX) # access to column
unique(data$SEX) #unique values in the column / labels 
#useful to do for each column which is of type character

#plot volu/me as function of brain
volume <- data$BRAIN_VOLUME
age <- data$AGE
plot(age, volume, main = "Function: Brain volume (Age)",
     xlab = "Age", ylab = "Brain volume")

#Always try to get visual information if there is correlatiion, to not waste time on mathematical modeling that does not have sens


library(dplyr)
#woman <- 
woman <- filter(data, SEX == "FEMALE")
#Pipe:   |> 
men <- data |> filter(SEX == "MALE")
#men <- filter(data, SEX == "MALE" ) # elmismo que arriba
#pipe w nowszej wersji wygl¹da inaczej, ale jest bardzije czytelny je¿eli chcemy ³ancuchowo wywolywac kilka funkcji


boxplot(woman$BRAIN_VOLUME)

#TEST

#H0:  miAD (mi of the brain volume) = miHEALTHY
#H1:  miAD < miHEALTHY

#for man should accept 0 hypothesis

t.test(woman$BRAIN_VOLUME, men$BRAIN_VOLUME)

# TEST FOR OVER 70

mO7 <- filter(men, AGE >= 70)
wO7 <- filter(woman, AGE >= 70)

t.test(mO7$BRAIN_VOLUME, wO7$BRAIN_VOLUME)



# -----------------------------------------
# Check if sex affects disease
# https://mop.cv.uma.es/mod/resource/view.php?id=833204


#contrasenia de hypothesis: associationTest
install.packages("lsr")
library(lsr)
data$CLASS_AS_FACTOR <- as.factor(data$CLASS)
data$SEX_AS_FACTOR <- as.factor(data$SEX)
associationTest(~ CLASS_AS_FACTOR + SEX_AS_FACTOR, data=data)
class(data$CLASS_AS_FACTOR)
#hymm, to powinno zadzai³¹æ, ale nie chce -> wiec powinienem sprubowac chisq.test bo to powinno mi pozwolic zrobic to samo. Na przyszlosc, moze nlepiej nie uzywac takiej nazwy 'data' jako zmiennej

# el mismo gey chisqu.test