# Test 2, Gr.A

#Ucitati fajl tips.csv.
#1. Testirati hipotezu da procenat napojnica koje su manje od 2$ iznosi 15%.
#a) U zadatku se testira hipoteza o:
#b) Na osnovu datog uzorka traˇzeni procenat iznosi:
#c) Nulta hipoteza ovog testa glasi:
#d) Test koji se koristi spada u grupu parametarskih / neparametarskih statistickih testova (zaokruziti).
#e) p-vrednost dobijena testom iznosi: , sto znaci da se H0 prihvata / ne prihvata (zaokruziti).

#1. RESENJE
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

#2. Ispitati nezavisnost obelezja X i Y cije su realizovane vrednosti uzorka: 11 17 29
#                                                                             21 8 14
#a) Test koji se koristi zove se , i spada u grupu parametarskih / neparametarskih testova (zaokruziti).
#b) Nulta hipoteza ovog testa glasi:
#c) Realizovana vrednost ove test statistike iznosi:
#d) Sa kvantilom koje raspodele (i sa koliko stepeni slobode!) se ova vrednost uporeduje?:
#e) p-vrednost dobijena testom iznosi: , sto znaci da se H0 prihvata / ne prihvata (zaokruziti).

#2. RESENJE
#a) Pirsonov hi-kvadrat test nezavisnosti obelezja, neparametarskih
#b) X i Y su nezavisna
#c) 9.8302
#d) Hi-kvadtat raspodela reda 0.95, stepen slobode 2 ??
#e) 0.007335, ne prihvatamo zbog p<0.05

chisq.test(matrix(c(11, 21, 17, 8, 29, 14), ncol = 3))

#3. Testirati hipotezu o jednakosti srednje vrednosti racuna po danima (ANOVA).
#a) Nulta hipoteza ovog testa glasi:
#b) Obelezje koje se ispituje je:
#c) Test statistika ima raspodelu sa stepeni slobode:
#d) Realizovana vrednost test statistike iznosi:
#e) p-vrednost dobijena testom iznosi: , sto znaci da se H0 prihvata / ne prihvata (zaokruziti).

#3. RESENJE
#a) H0(m1=m2=m3=m4) - srednje vrednosti racuna po danima jednake
#b) tips$total_bill - racun
#c) F (Fiserova) dF, 3, 240
#d) 2.7675
#e) 0.04245, ne prihvatamo?

boxplot(total_bill~day, data = tips)
anova(lm(total_bill~day, data = tips))

