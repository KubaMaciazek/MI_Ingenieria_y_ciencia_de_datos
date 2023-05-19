# https://mop.cv.uma.es/mod/resource/view.php?id=833249

library(arules)
lastfm <- read.csv("S:/0 Universidad de Malaga/ingenieria_y_cicncia_de_datos/lastfm.csv")
View(lastfm)
lastfm[1:20, ]
length(lastfm$user)
class(lastfm$user)
lastfm$user <- factor(lastfm$user)

reglas1 <- apriori(lastfm, parameter = list(support = .01, confidence = .5))
inspect(reglas1)

lista.musica.por.usuario <- split(x = lastfm[, "artist"], f = lastfm$user)
lista.musica.por.usuario[1:2]

lista.musica.por.usuario <- lapply(lista.musica.por.usuario, unique)
lista.musica.por.usuario1 <- as(lista.musica.por.usuario, "transactions")
lista.musica.por.usuario[1:5]


#BTW: lapply(v, mean) -> dla ka¿dego czynnika wektora v uzyj funkcji mean
v <- list(c(1,2,4), c(6,7))
lapply(v, mean)


str(lista.musica.por.usuario1)
write(head(lista.musica.por.usuario1))
write(head(lista.musica.por.usuario1), format = "single")

itfreq1 <- itemFrequency(lista.musica.por.usuario1)
head(itfreq1)

itemFrequencyPlot(lista.musica.por.usuario1, support = .08, cex.names = 1)



#..


r1 <- subset(reglas2, subset = lhs %ain% c("coldplay")) # all in | pin - partial match
inspect(r1)
r1 <- subset(reglas2, subset = lhs %pin% c("pump"))
inspect(r1)


#############################################################################################


# TITANIC
load("S:/0 Universidad de Malaga/ingenieria_y_cicncia_de_datos/titanic.raw.rdata")
View(titanic.raw)

R1 <-apriori(titanic.raw)
system.time(R1 <- apriori(titanic.raw))
inspect(R1[1: 10])

R2 <- apriori(titanic.raw,
              control = list(verbose=FALSE),
              parameter = list(minlen=2,
                               supp=0.005, conf=0.08)) #nie skoñczy³em przepisywaæ -????? czy ktoœ to ma albo cyz jest w nceci

inspect(R2)

R1.sub <- subset(R1, subset = rhs %in%
                   c("Age=Adult")
                 & lift > 2)
R2.sub <- ...