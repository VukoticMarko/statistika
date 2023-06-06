# Test 2, Gr.C

#1.

#a) 20.92569
#b) 17.41716
#c) t-test, parametarski
#d) H0(mv>mr) ?
#e) p-vrednost: 0.003796, odbacujemo?

#prosek racuna vikendom
vikendSub<-mean(bill[tips$day=="Sat"])
vikendNed<-mean(bill[tips$day=="Sun"])
vikend<-(vikendSub+vikendNed)/2

#prosek racuna radnim danima
rPet<-mean(bill[tips$day=="Fri"])
rCet<-mean(bill[tips$day=="Thur"])
rDani<-(rCet+rPet)/2

#pravimo vektore  vrednosti racuna za t-testove
vikendVektor<- c(bill[tips$day=="Sat"], bill[tips$day=="Sun"])
rDaniVektor<- c(bill[tips$day=="Thur"], bill[tips$day=="Fri"])
result = t.test(vikendVektor, rDaniVektor)
result

#2.

