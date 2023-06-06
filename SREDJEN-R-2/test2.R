# Test 2, Gr.A

#1.
#a) parametru p binomne raspodele
#b) 18.443%
#c) H0(p=0.15)
#d) parametarskih
#e) 0.1506, prihvatamo, slican je p ko u tekstu zadatka

#f <- file.choose(), pa read.csv(f)
tips <- read.csv("tips.csv")
nap <- tips$tip
n <- length(nap)
K <- length(nap[nap < 2])
binom.test(K, n, 0.15)

#2.
#a) Pirsonov hi-kvadrat test nezavisnosti obelezja, neparametarskih
#b) X i Y su nezavisna
#c) 9.8302
#d) Hi-kvadtat raspodela reda 0.95, stepen slobode 2 ??
#e) 0.007335, ne prihvatamo zbog p<0.05

chisq.test(matrix(c(11, 21, 17, 8, 29, 14), ncol = 3))

#3.
#a) H0(m1=m2=m3=m4) - srednje vrednosti racuna po danima jednake
#b) tips$total_bill - racun
#c) F (Fiserova) dF, 3, 240
#d) 2.7675
#e) 0.04245, ne prihvatamo?

boxplot(total_bill~day, data = tips)
anova(lm(total_bill~day, data = tips))

