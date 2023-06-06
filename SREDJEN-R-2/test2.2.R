# Test 2, Gr.B

#1
#a) parametar p binomne raspodele
#b) 3.089618
#c) H0(p?) NE MOGU P skontati....
#d) parametarskih
#e) Ako p skontam onda moze binom.test proci

tips <- read.csv("tips.csv")
nap <- tips$tip
mean(nap[tips$sex=="Male"])
n <- length(nap)
K<-length(nap[tips$sex=="Male"])
binom.test(K, n, p?)

#2. 
#a) neparametarski
#b) H0(totalbill i dnorm())?
#c) ks.test(


#3.
#a) H0(m1=m2=m3=m4) - srednje vrednosti visine napojnica jednake
#b) nivoi faktora?
#c) F (Fiserova), 3, 240
#d) 1.6724
#e) 0.1736, 

boxplot(tip~day, data = tips)
anova(lm(tip~day, data = tips))

