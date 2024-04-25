?beavers
str(beaver2)
View(beaver2)

beavers <- as.factor(beaver2$activ)
beavers
str(beavers)
beaver2$activ <- beavers
str(beaver2) 


beaver2_data <- beaver2

#Null Hypothesis (H0): Activity has no effect on the beaver's body temperature. 
#This implies the mean body temperature of active periods equals the mean body temperature of inactive periods.

#Alternative Hypothesis(H1): Activity does affect the beaver's body temperature. 
#This implies a significant difference between the mean body temperatures during active and inactive periods.


str(beaver2_data)

beaver2_data$activ <- factor(beaver2_data$activ, labels = c("no","yes"))
str(beaver2_data)
View(beaver2_data)


# Creating the histogram
hist(beaver2_data$temp, breaks = 10, col = 'blue', main = 'Histogram of Beaver Body Temperatures',
     xlab = 'Body Temperature (°C)', ylab = 'Frequency')


windows(20,10)
attach(beaver2_data)
histogram(~temp | activ,
          data = beaver2_data,
          main = 'Histogram of Beaver Body Temperatures', 
          xlab = 'Body Temperature', ylab = 'Frequency')
detach(beaver2_data)

qqnorm(temp)
qqline(temp,col = "red")


attach(beaver2_data)
windows(20,10)
par(mfrow = c(1,2))
with(beaver2_data,{
  qqnorm(temp[activ == "yes"],
         main = "beavers activ data")
  qqline(temp[activ=="yes"])
})

with(beaver2_data,{
  qqnorm(temp[activ == "no"],
         main = "beavers inactiv data")
  qqline(temp[activ=="no"])
})
detach(beaver2_data)


normality_test <- shapiro.test(beaver2_data$temp)
normality_test
normality_test$p.value

attach(beaver2_data)
wilcox.test(temp~activ)



library(psych)

windows(16,10)
pairs(beaver2_data, labels = colnames(beaver2_data),main = "beavers dataset correlation plot")

windows(16,10)
pairs.panels(beaver2_data,
             smooth = TRUE,
             scale = FALSE,
             density = TRUE,
             ellipses = TRUE,
             method = "spearman",
             pch = 21,
             lm =FALSE,
             cor = TRUE,
             jiggle = FALSE,
             factor = 2,
             hist.col = 4,
             stars = TRUE,
             ci = TRUE)
