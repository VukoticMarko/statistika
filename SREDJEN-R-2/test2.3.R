# Test 2, Gr.C

#1.

#a) 20.92569
#b) 17.41716
#c) t-test, parametarski
#d) sr.vrednost racuna tokom vikenda=sr.vrednost racuna radnim danima, m1>m2
#e) 2.793889 
#f) p-vrednost: 0.002812375, odbacujemo

#prosek racuna vikendom
racuni_vikend <- tips$total_bill[tips$day %in% c("Sat", "Sun")]
mean(racuni_vikend)

#prosek racuna radnim danima
racuni_radni_dani <- tips$total_bill[tips$day %in% c("Thur", "Fri")]
mean_radni_dani <- mean(racuni_radni_dani)

#test
t_test <- t.test(racuni_vikend, racuni_radni_dani, alternative = "greater", var.equal = TRUE)

#realizovana vrednost test statistike
t_test$statistic

#2.

#a) nacrtas iz tabele frekvencije
#b) 0.365
#c) neparametarski
#d) Nulta hipoteza glasi da visina napojnica ima normalnu raspodelou
#e) 36.34
#f) Pirsonova x^2
#g) 2.4519, ne prihvata

tip<-tips$tip
intervali <- c(-Inf, 1.5, 2.5, 3.5, 5, Inf)
frekvencije <- table(cut(tip, intervali))
#a)
tabela_frekvencija <- data.frame(Interval = names(frekvencije), Frekvencija = as.vector(frekvencije))
#b)
sum(frekvencije["(2.5,3.5]"]) / sum(frekvencije)
#e)
ocekivane_frekvencije <- diff(pnorm(intervali, mean = 3, sd = 1.4)) * sum(frekvencije)
matrica_kontingencije <- rbind(frekvencije, ocekivane_frekvencije)
chisq.test(matrica_kontingencije)$statistic


#3.
#a) 
summary(lm(napojnice ~ racun, data = tips))
#b) generalna korelacija napojnice i visine racuna (0.6757341)
#c) 5.12125 
koef_korelacije <- cor(tips$tip, tips$total_bill)
procena <- predict(reg_model, newdata = data.frame(racun = 40))
