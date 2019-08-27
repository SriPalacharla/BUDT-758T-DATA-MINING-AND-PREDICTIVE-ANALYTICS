
##1
airlines = read.csv("Airlines_Data.csv",header=T)

cc3_miles_factor = as.factor(airlines$cc3_miles)




Years_since_enroll = (airlines$Days_since_enroll)/365

Loyal_customer = ifelse(Years_since_enroll > 5,1,0)

airlines = data.frame(airlines, Years_since_enroll, Loyal_customer, cc3_miles_factor)



mean(airlines$Balance)
median(airlines$Balance)

hist1 = hist(airlines$Balance, col='blue', breaks=15, xlab='Balance',
             main='Histogram of Balance',labels = TRUE)


mean(airlines$Award)
(1 - sum(airlines$Award)/3999)*100




##2
reg1 = lm(Balance ~ Award, data=airlines)
summary(reg1)


plot(airlines$Award, airlines$Balance, xlab = 'Award', ylab = 'Balance',
     main='scatter-plot with regression line')
abline(reg1, lwd=2, col='red')



##3
reg2 = lm(Balance ~ Award + cc3_miles_factor + Years_since_enroll + Loyal_customer + 
          Bonus_miles + Bonus_trans, data = airlines)
summary(reg2)

Loyal_customerXBonus_trans = airlines$Loyal_customer*airlines$Bonus_trans

airlines = data.frame(airlines, Loyal_customerXBonus_trans)

reg3 = lm(Balance ~ Award + cc3_miles_factor + Years_since_enroll + Loyal_customer + 
            Bonus_miles + Bonus_trans + Loyal_customerXBonus_trans, data = airlines)
summary(reg3)



##4
reg4 = lm(Award ~ cc3_miles_factor + Years_since_enroll + Loyal_customer + Bonus_miles 
          + Bonus_trans + Balance, data=airlines)
summary(reg3)



