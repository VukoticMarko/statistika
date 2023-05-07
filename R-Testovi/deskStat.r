

deskstat <- function(x, mi=NULL){
  
  #obim uzorka ----------------------------------------------------------
  n<-length(x) 
  print(paste0("Obim uzorka: ", n))
  
  #medijana ----------------------------------------------------------
  me <- round(median(x), 4)
  print(paste0("Medijana: ", me))
  
  #modus -------------------------------------------------------------
  tx <- table(x)
  mo <- as.numeric(names(tx)[tx==max(tx)])
  mo <- round(mo,4)
  print(paste0("Modus: ", mo))
  
  #aritmeticka sredina ------------------------------------------------
  xn <- mean(x)
  xn <- round(xn, 4)
  print(paste0("Artitmeticka sredina: ", xn))
  
  
  #standardna devijacija ------------------------------------------------
  sn <- sqrt(sd(x)^2*(n-1)/n)
  sn <- round(sn, 4)
  print(paste0("Standardna devijacija: ", sn))
  
  
  #uzoracka disperzija ------------------------------------------------
  sn2 <- sd(x)^2*(n-1)/n
  sn2 <- round(sn2, 4)
  print(paste0("Uzoracka disperzija: ", sn2))
  
  
  #koeficijent spoljostenosti (KURTOSIS) i asimetrije (SKEWNESS) -----------
  mi4 <- mean((x-mean(x))^4)
  mi3 <- mean((x-mean(x))^3)
  mi2 <- mean((x-mean(x))^2)
  
  ksplj <- mi4/mi2^2
  ksplj <- round(ksplj,4)
  
  kasim <- mi3/mi2^(3/2)
  kasim <- round(kasim, 4)
  
  #uzoracka funkcija raspodele - empirijska ----------------------------
  
  plot.ecdf(x)
  f <- ecdf(x) #iz f izvlacis konkretne vrednosti
  
  #VREDNOSTI EMPIRIJSKE FUNKCIJE RASPODELE: todo
  cumsum(tx)/n
  
  
  #histogram ----------------------------------------------------------
  liberary(agricolae)
  k = 1
  if (is.null(mi)){
    h <- hist(x)
  }
  else{
    h<-hist(x, breaks=mi, probability = TRUE, labels = TRUE)
  }
  
  
  #summary ----------------------------------------------------------
  print(summary(x))
  
  
  #Kvantili i IQM ----------------------------------------------------------
  
  q1 <- quantile(x, 0.25)
  q1 <- round(q1, 4)
  
  q2 <- quantile(x, 0.50)
  q2 <- round(q2,4)
  
  q3 <- quantile(x, 0.75)
  q3 <- round(q3, 4)
  
  print(paste0("Q1 = ", q1))
  print(paste0("Q2-medijana= ", q2))
  print(paste0("Q3 = ", q3))
  
  iqm <- IQM(x)
  iqm <- round(iqm,4)
  print(paste0("Interkvartilni razmak = ", iqm))
  

  #QQ-PLOT, BOXPLOT ----------------------------------------------------------
  qqnorm(x)
  qqline(x)
  
  boxplot(x)

        
}

# *nekorigovana uzoracka disperzija 
sn2 <- sd(x)^2*(n-1)/n
sn2 <- sn^2 #preko standardne devijacije ako je imamo

# *nekorigovana standardna devijacija:
sn <- sqrt(sd(x)^2*(n-1)/n) #ovako smo na vezbama
sn <- sqrt(sn2)


# **korigovana uzoracka disperzija
sn2korig <- sd(x)^2
sn2korig <- var(x)

# **korigovana standardna devijacija 
snkorig <- sd(x)