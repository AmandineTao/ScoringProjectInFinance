

###################################################################################################
#                   SCORING PROJECT IN FINANCE                                                    #
###################################################################################################

setwd('E:/')

library(dplyr)
library(sas7bdat)
library(ggplot2)
library(summarytools)


data=read.sas7bdat("FinancialData.sas7bdat", debug=FALSE)

data$Y=as.factor(data$Y)
data$NUPER=as.factor(data$NUPER)

var_bin=c("DETVIE", "DETIMMO", "DETCONSO", "DETREV", "DETLIQ", "CPTTIT", "PEA", "PSOC")
numeriques = c("AGE", "ANCCDD", "SURFIN", "SOLMOY", "FLUCRE", "FLUDEE", "NBCRE", "NBDEE", "NBJDE", "SLDLIQ","SLDTIT","SLDPEA","SLDBLO")


for (name in var_bin) {
  data[,name]=as.factor(data[,name])
}
sapply(data, class)


# Summary of the data
summary(data)
#view(dfSummary(data))


### Categorize all numeric variables

num_cut = function(var){
  
  #defining the quantiles
  q1 = quantile(var, probs = 0.25, na.rm = TRUE)
  q2 = quantile(var, probs = 0.50, na.rm = TRUE)
  q3 = quantile(var, probs = 0.75, na.rm = TRUE)
  
  cut_var = factor(cut(var, breaks = unique(c(min(var, na.rm = TRUE),q1, q2, q3, max(var, na.rm = TRUE))),
                       include.lowest = TRUE),exclude = NULL)
  
  cut_var[is.na(cut_var)] <- 0
  
  return(cut_var)
}


num_cat=data.frame(row.names = row.names(data))

for (name in numeriques) {
  num_cat[,paste(name,"cut", sep = "_")]=num_cut(data[,name])
  print(name)
}



# chi square test

chiq=print(chisq.test(table(data$Y, num_cat[,"AGE_cut"])))

chiq$statistic

chistat=data.frame(matrix(ncol = 2, nrow=length(colnames(num_cat)) ))
colnames(chistat)<-c("name","statistic")

i=1
for (name in colnames(num_cat)) {
  chistat$name[i]=name
  chistat$statistic[i]=chisq.test(table(data$Y, num_cat[,name]))$statistic
  i=i+1
}


ggplot(chistat, aes(x=reorder(name,-statistic), y=statistic)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90))


## complex variables : sum of the products


# convert
data[,var_bin]=apply(data[,var_bin], 2, as.numeric)
apply(data[,var_bin], 2, class)


data$produits=apply(data[,var_bin], 1, sum)



groupes1=data %>%
  group_by(produits) %>%
  count(Y) %>%
  filter(Y==1)


groupes= data %>%
  group_by(produits) %>%
  summarise(count=n())


groupes1$prop = groupes1$n / groupes$count

groupes1
colnames(groupes1)<-c("produits", "Y", "subscribers", "prop")
groupes1

ggplot(groupes1, aes(x=produits, y=prop)) +
  geom_point() + geom_line()


table(num_cat$AGE_cut, num_cat$SURFIN_cut)

# meilleure option : faire une indicatrice

# les autres variables à checker : 
# DETTE * AGE
table(num_cat$AGE_cut, num_cat$FLUDEE_cut)


# SURFIN * SLDBLO
table(num_cat$SURFIN_cut, num_cat$SLDBLO_cut)



# SURFIN * SDLPEA
table(num_cat$SURFIN_cut, num_cat$SLDPEA_cut)



# NBCRE * NBDEE
#table(num_cat$AGE_cut, num_cat$SLDBLO_cut)

# slide mais pas sur
table(num_cat$AGE_cut, data$produits)



### MODELIZATION

data = data %>%
  select(-NUPER)

var_NA = c("SLDLIQ", "SLDTIT", "SLDPEA", "SLDBLO")

for (name in var_NA) {
  data[is.na(data[,name]),name]=0
}


# ajustement du modèle sur le dataset entier
rf=randomForest(Y~., data=data)
pred_rf = predict(rf, newdata=data, type="response")
#pred_rf
table(pred_rf, data$Y)


##### creation of train and test
# shuffle
data = data[sample(nrow(data)),]
# indices
indices = 1:(0.20*nrow(data))
train = data[-indices,]
test = data[indices,]

rf=randomForest(Y~., data=train)
pred_rf = predict(rf, newdata=test, type="response")
table(pred_rf, test$Y)


pred_rf = predict(rf, newdata=test, type="prob")
pred_rf=pred_rf[,1]


library(ROCR)
pred = prediction(pred_rf, test$Y)
performance_lift = performance(pred, measure="lift", x.measure = "rpp")
plot(performance_lift, colorize=TRUE)

performance_roc = performance(pred, "tpr", "fpr")
plot(performance_roc, colorize=TRUE)


#return rate: proba des Y==1 dans chaque classe
#corrected return rate = (1/2)*(return rate)
#variables costs: cout de message envoyé
#costs returning: cost pour envoyer le produit au client
#margin: prix du produits



#

groupes2=data %>%
  group_by(PEA) %>%
  count(Y) %>%
  filter(Y==1)


groupes3= data %>%
  group_by(PEA) %>%
  summarise(count=n())


groupes2$prop = groupes2$n / groupes$count

groupes2
colnames(groupes2)<-c("produits", "Y", "subscribers", "prop")
groupes2
