library(arules)
data("Groceries")
inspect(Groceries[1:2])
library(Matrix)
Groceries@data
Groceries@itemInfo
Groceries@data@i
Groceries@data[, 1:16] %>% rowSums() %>% order(decreasing = TRUE) %>% .[1:6] -> idx

------------------------------------------

data("Adult")
length(Adult)
dim(Adult)
#...

rules <- apriori(Adult, parameter = list(
  supp = 0.5, conf = 0.9, 
  target = "rules"
))

rules <- Adult %>% apriori(parameter = list(
    supp = 0.5, conf = 0.9,
    target = "rules"
))

summary(rules)
inspect(head(rules))

rules1 <- apriori(Adult,
  parameter = list(supp = 0.5, conf = 0.9),
  appearance = list(none = c("income=small", "sex=Male"))
)

rules2 <- apriori(Adult,
                  parameter = list(supp = 0.5, conf = 0.9),
                  appearance = list(rhs = c("relationship=Husband"))
)



#operations on dispers matrixes