# Test 2, Gr.B

#1
#a) U zadatku se testira hipoteza o srednjoj vrednosti napojnica koje su muškarci ostavljali.
#b) 3.089618
#c) Nulta hipoteza ovog testa glasi da je srednja vrednost napojnica koje su muškarci ostavljali manja ili jednaka 3.5 dolara.
#d) parametarskih
#e) 0.9996, prihvatiti(valjda)

tips <- read.csv("F:/tips.csv")
nap <- tips$tip[tips$sex == "Male"]
mean_nap <- mean(nap)
t.test(nap, mu = 3.5, alternative = "greater")

#2. 
#a) neparametarski
#b) Empirijska distribucija podataka odgovara normalnoj raspodeli N(20, 9)
#c) 1.994
#d) 0.1276, 1.36 ( ako je lambda 0.99 onda je 1.63)
#e) 0.00070, ne prihvata (P<0.05)

racun<-tips$total_bill
ks_test <- ks.test(racun, "pnorm", mean = 20, sd = 9)
c) dn<-ks_test$statistic * sqrt(length(racun))
d) realizovana_vrednost <- ks_test$statistic
e) p_vrednost <- ks_test$p.value

#3.
#a) H0(m1=m2=m3=m4) - srednje vrednosti visine napojnica po danima su jednake
#b) Nivoi faktora po kojima se radi ANOVA su dani (day - Sun, Sat, Thur, Fri).
#c) F (Fiserova), 3
#d) 1.6724
#e) 0.1736, ne postoje (DA LI JE NULTA HIP. PRIHVACENA(P>0.05) -> JESTE ONDA NE POSTOJE RAZLIKE)


tip <- tips$tip
boxplot(tip~day, data = tips)
anova(lm(tip~day, data = tips))

