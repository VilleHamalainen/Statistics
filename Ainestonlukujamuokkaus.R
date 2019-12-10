#Alkusessio - pakettien lataus ja ajaminen sessioon
install.packages("corrgram", dependencies = TRUE)
install.packages("psych")
install.packages("lavaan")
install.packages("memisc")
install.packages("sjPlot")
install.packages("semPlot")
install.packages("GPArotation")


library(GPArotation)
library(lavaan)
library(semPlot)
library(sjPlot)
library(foreign)
library(corrgram)
library(psych)
library(MASS)
library(dplyr)

#DATAN UUDELLEENKOODAUS OSIO

#Luetaan data
dataset = read.spss("C:\\Lataukset\\Oppilaskysely_DATA.sav", to.data.frame=TRUE)
dataset = Oppilaskysely_DATA
#Poistetaan kaikki joilla on tyhji? vastauksia meit? kiinnostavissa osioissa
dataset<- na.omit(dataset[,c(2:6,26:98)])

#Reversearvot
LikertMinValue = 1
LikertMaxValue = 7

dataset$SDT7COMPR_LIKERT = LikertMinValue + LikertMaxValue - dataset$SDT7COMPR_LIKERT
dataset$SDT6AUTR_LIKERT = LikertMinValue + LikertMaxValue - dataset$SDT6AUTR_LIKERT
dataset$MOT6EFFORTR_LIKERT = LikertMinValue + LikertMaxValue - dataset$MOT6EFFORTR_LIKERT
dataset$MOT4INTERESTR_LIKERT = LikertMinValue + LikertMaxValue - dataset$MOT4INTERESTR_LIKERT

#summamuuttujat motivaatiojutuille summien keskiarvo
#tee testit esim kompetenssin osa-alueille: SDT_1, SDT_2, SDT_3 -> testaa muuttujien lataukset
# faktorloading? Faktorianalyysi
#Aikaisemmin tehdyt chronbachin alffat mittaria valaittaessa, mutta varmaan hyv? tehd?

dataset$Vertailu <- rowMeans(dataset[,c("GOAL1PAPP_LIKERT", "GOAL5PAPP_LIKERT", "GOAL7PAPP_LIKERT") ], na.rm = FALSE)
dataset$Valttely <- rowMeans(dataset[,c("GOAL2PAV_LIKERT", "GOAL4PAV_LIKERT", "GOAL8PAV_LIKERT")], na.rm = FALSE)
dataset$Mastery <- rowMeans(dataset[,c("GOAL3MAST_LIKERT", "GOAL6MAST_LIKERT", "GOAL9MAST_LIKERT")], na.rm = FALSE) 
dataset$Kompetenssi <- rowMeans(dataset[,c ("SDT1COMP_LIKERT", "SDT5COMP_LIKERT")], na.rm = FALSE) # 33 reversekysymys 
dataset$Autonomia <- rowMeans(dataset[,c("SDT2AUT_LIKERT", "SDT3AUT_LIKERT")], na.rm = FALSE) # 32 reversekysymys 
dataset$Yhteenkuuluvuus <- rowMeans(dataset[,c("SDT4REL_LIKERT", "SDT8REL_LIKERT")], na.rm = FALSE)
dataset$Mielenkiinto <- rowMeans(dataset[,c("MOT1INTEREST_LIKERT", "MOT10INTEREST_LIKERT","MOT4INTERESTR_LIKERT"), ], na.rm = FALSE) #38 reversekysymys -
dataset$Vaivannako <- rowMeans(dataset[,c("MOT2EFFORT_LIKERT", "MOT3EFFORT_LIKERT")], na.rm = FALSE) # 40 reversekysymys 
dataset$Arvo <- rowMeans(dataset[,c("MOT5VALUE_LIKERT", "MOT9VALUE_LIKERT", "MOT12VALUE_LIKERT")], na.rm = FALSE)
dataset$Amotivaatio <- rowMeans(dataset[,c("MOT7AMOTIV_LIKERT", "MOT8AMOTIV_LIKERT", "MOT11AMOTIV_LIKERT")], na.rm = FALSE)


#Poisteetu NA vastaukset alku datasta luoduista summamuuttujista ja valittu mukaan sukupuoli ja ik?, sek? pepe-osaalueet
cleanedLikert <- na.omit(dataset[,c (1:4, 23, 25, 27, 29, 31, 33,78:87 )])
cleanedLikert2 <- dataset[,c("GOAL1PAPP_LIKERT", "GOAL5PAPP_LIKERT", "GOAL7PAPP_LIKERT","GOAL2PAV_LIKERT", "GOAL4PAV_LIKERT", "GOAL8PAV_LIKERT","GOAL3MAST_LIKERT", "GOAL6MAST_LIKERT", "GOAL9MAST_LIKERT",
                                  "SDT1COMP_LIKERT", "SDT5COMP_LIKERT", "SDT7COMPR_LIKERT","SDT2AUT_LIKERT", "SDT3AUT_LIKERT", "SDT6AUTR_LIKERT", "SDT4REL_LIKERT", "SDT8REL_LIKERT", "MOT1INTEREST_LIKERT", "MOT10INTEREST_LIKERT","MOT4INTERESTR_LIKERT",
                                  "MOT2EFFORT_LIKERT", "MOT3EFFORT_LIKERT", "MOT6EFFORTR_LIKERT", "MOT5VALUE_LIKERT", "MOT9VALUE_LIKERT", "MOT12VALUE_LIKERT","MOT7AMOTIV_LIKERT", "MOT8AMOTIV_LIKERT", "MOT11AMOTIV_LIKERT")]

cleanedLikert3 <- dataset[,c(1:6,8,10,12,14,16,18,20,22, 24, 26, 28, 30,32,34,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77)]

cleanedLikert3 <- dataset[,c(1:5,79:88)]

#Uusi muuttuja Engagement, joka on mielenkiinnon ja vaivann??n summamuuttuja
cleanedLikert3$engagement = rowMeans(cleanedLikert3[,c("Mielenkiinto", "Vaivannako")], na.rm=FALSE)
#Varmistetaan, ett? luokka on num muodossa
dataset[,c(4)] <- sapply(dataset[,c(4)], as.numeric)
#Luodaan oma opsin mukainen luokkaryhmittely
dataset$Luokka_num <- (dataset$GRADE)
#L?ydetyt poikkeukset ja niiden muokkaus
dataset$Luokka_num[(dataset$Luokka_num == 11) & (dataset$AGE == 9)] <- 3
dataset$Luokka_num[(dataset$Luokka_num == 11) & (dataset$AGE == 13)] <-7
dataset$GRADE[(dataset$GRADE == 11) & (dataset$AGE == 9)] <- 3
dataset$GRADE[(dataset$GRADE == 11) & (dataset$AGE == 13)] <-7
#UUdelleen koodataan luokat kolmeen ryhm??n, 1-2, 3-6. 7-9  
dataset$Luokka_num[(dataset$Luokka_num < 3)] <- 11
dataset$Luokka_num[(dataset$Luokka_num <= 6) & (dataset$Luokka_num >= 3)] <- 12
dataset$Luokka_num[(dataset$Luokka_num <= 9) & (dataset$Luokka_num >= 7)] <- 13
#J?tet??n pois 3 todella pient? oppilasta, nyt data 3-9 luokkalaiset
dataset<- subset(dataset, Luokka_num >=12)

#POistetaan luokkajuttu analyysin kannalta
dataset<-subset(dataset[,-c(1,3,41)])




#Nimet??n kaikki indikaattorit paremmin 
cleanedLikert3<-plyr::rename(cleanedLikert3,c("SCHOOL"="Koulu", "GENDER"="Sukupuoli","AGE"="Ik?", "GRADE"="Luokka", "BGVIDEOGAME_LIKERT"="Digipelit","GOAL1PAPP_LIKERT"="Vertailu1",
                        "GOAL2PAV_LIKERT"="V?lttely1",  "GOAL3MAST_LIKERT"="Taito1", "GOAL4PAV_LIKERT"="V?lttely2",
                        "GOAL5PAPP_LIKERT"="Vertailu2", "GOAL6MAST_LIKERT"="Taito2","GOAL7PAPP_LIKERT"="Vertailu3",
                        "GOAL8PAV_LIKERT"="V?lttely3","GOAL9MAST_LIKERT"="Taito3","PEPEROLE_LIKERT"="Roolipelit", 
                        "PEPEMAKE_LIKERT"="Pelientekeminen", "PEPEEDUGAME_LIKERT"="Opetuspelit", 
                        "PEPEENTERTGAME_LIKERT"="Viihdepelit","PEPEBOARDGAME_LIKERT"="Lautapelit",
                        "PEPEEXP_LIKERT"="Pelillist?minen",
                        "SDT1COMP_LIKERT" ="Kyvykkyys1",  "SDT2AUT_LIKERT"="Autonomia1", "SDT3AUT_LIKERT"="Autonomia2",
                        "SDT4REL_LIKERT" ="Yhteis?llisyys1", "SDT5COMP_LIKERT"="Kyvykkyys2", "SDT6AUTR_LIKERT"="Autonomia3r",
                        "SDT7COMPR_LIKERT"="Kyvykkyys3r", "SDT8REL_LIKERT" ="Yhteis?llisyys2",  "MOT1INTEREST_LIKERT"="Motiintenjoy1",  
                        "MOT2EFFORT_LIKERT" ="Motivaiva1","MOT3EFFORT_LIKERT" ="Motivaiva2", "MOT4INTERESTR_LIKERT"="Motiintejoy2r", 
                        "MOT5VALUE_LIKERT"="MotiArvo1", "MOT6EFFORTR_LIKERT" ="Motivaiva2r","MOT7AMOTIV_LIKERT"="Motiamotiv1",
                        "MOT8AMOTIV_LIKERT"="Motiamotiv2","MOT9VALUE_LIKERT"="Motiarvo2","MOT10INTEREST_LIKERT" ="Motiintenjoy3",
                        "MOT11AMOTIV_LIKERT"="Motiamotiv3", 
                        "MOT12VALUE_LIKERT"="Motiarvo3","Vertailu" ="Vertailu","Valttely"="V?lttely",   "Mastery"="Taito",
                        "Kompetenssi"="Kyvykkyys","Autonomia" ="Autonomia",            
                         "Yhteenkuuluvuus"="Yhteenkuuluvuus","Mielenkiinto"= "Kiinnostus/Nautinto","Vaivannako"="Vaivannako","Arvo"= "Arvo/Sis?ist?minen",  "Amotivaatio"= "Amotivaatio",          
                        "engagement" ="Kiinnittyminen",  "Luokka_num" = "OPS-luokkajako"))



#
#chronbach testi: Reverseluokat ja kysymykset cleanedLikert3 datassa(Reverset on k??nnetty aikaisemmin). Kompetenssi (22), Autonomia(25) Interest(30), Vaivannako (33)
psych::alpha(cleanedLikert3[c(33,37,40)], keys = NULL,  title = cleanedLikert$engagement, max = 10, na.rm = TRUE, check.keys = TRUE, n.iter = 1, n.obs = 211)
#choronbach reversearvoille
reverseArvot = cleanedLikert3[c(21,22,25)]
psych::alpha(reverseArvot, keys = c(1,1,-1))

#SEM
#faktorianalyysi?
fit <- princomp(cleanedLikert3[c(15:17,19,50)], cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)

#Lis?? faktorianalyysi? lis?? ominasiarvot osaksi sem tai korrelatio scripti?

ev=eigen(cor(cleanedLikert3[,c(21,25,27,29,32,35,37,39,40)]))
ev
plot(rep(1:length(ev$values)),ev$values)
     
fit1 <- factanal(cleanedLikert3[,c(5:40)], factors=20, rotation="none")
fit1
fit2 <- factanal(cleanedLikert3[,c(26,27,34,41:50)], factors=2, rotation="none")
fit2
fit3 <- factanal(cleanedLikert3[,c(26,27,34,41:50)], factors=7, rotation="varimax")
fit3
     

#mallintestausta

#Explatory factoryanalysis
fit <- factanal(cleanedLikert3[5:39],12 , rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(cleanedLikert3),cex=.7) # add variable names


#I?n vaikutus engagement kuvaaja + testit
barplot(table(pepemuodot2$engagement, pepemuodot2$Luokka_num), beside = T, cex.names = 0.8,  legend.text = c("Tytt?", "Poika"), args.legend=list(x = 15, y = 15, cex=0.4), col=c("pink" , "green"))

kruskal.test(engagement~Luokka_num, data = pepemuodot2)

#Suokupuolen testaus wilcoxonilla
wilcox.test(engagement~GENDER, data = cleanedLikert3)
wilcox.test(engagement~Luokkajako, data = cleanedLikert3)

#Selke? tilastollinen ero luokkien 3-6 ja 7-9 v?lill?!
boxplot(engagement~Luokkajako, data=cleanedLikert3, col = "lightgray", xlab = "Luokat 3-6 ja 7-9")


#Oppilaiden lukkum??r?t i?n/luokan perusteella

table(cleanedLikert3$Luokkajako)
table(pepemuodot$Luokka_num)
table(pepemuodot2$Luokka_num)



#Valitaan vain oppilaat, jotka kokevat osallistuneensa johonkin pepe muotoon paljon, jolloin uutuuden vieh?tys v?henee. Oma arvio siis >= 5
pepemuodot2 <- subset(cleanedLikert, cleanedLikert$Roolipelit >=5  | cleanedLikert$Pelientekeminen >=5 | cleanedLikert$Opetuspelit >=5 | cleanedLikert$Viihdepelit >=5 | cleanedLikert$Lautapelit >=5 | cleanedLikert$Pelillist?minen >= 5)

#valitaan kaikki eri pepemuodot joilla engagement todella iso tai pieni? Onko j?rke??
pepemuodot <- subset(cleanedLikert, cleanedLikert$Roolipelit & cleanedLikert$Pelientekeminen & cleanedLikert$Opetuspelit & cleanedLikert$Viihdepelit & cleanedLikert$Lautapelit & cleanedLikert$Pelillist?minen & cleanedLikert$engagement>= 5 | cleanedLikert$engagement <= 2)



#Testataan lineaarinen regressio, mutta ei usean l?hteen mukaan sovi likerteille #+Pelientekeminen + Viihdepelit + Lautapelit + Pelillist?minen
malli <- (lm(Kiinnittyminen ~  Kyvykkyys + Autonomia + Yhteenkuuluvuus  , data = cleanedLikert3))
reducedmodel <- (lm(Arvo ~ Kompetenssi + Yhteenkuuluvuus, data =cleanedLikert3))
plot(reducedmodel)
  plot(malli)
                 
                 
                 
  summary(malli)
                 hsummary(reducedmodel)
                 anova(reducedmodel, malli)
                 confint(reducedmodel)
                 
                 #Outliertestausta
                 outlier(reducedmodel)
                 
                 #Multikolinearisuuden testaus
                 vif()
                 sqrt(vif(malli))
                 
                 
                 
                 #Ordinal logistic regression selitett?v?n muuttujana engagement ja selitt?v?n? pepe:n muodot paskaa
                 engagementTulos <- polr(as.factor(engagement) ~ Roolipelit  + Opetuspelit,  data = pepemuodot2, Hess = T)
                 summary(engagementTulos)
                 
                 #Ordinal logistic regression selitett?v?n muuttujana engagement ja selitt?v?n? pepe:n muodot paskaa...
                 engagementTulos <- polr(as.factor(engagement) ~ Roolipelit + Pelientekeminen + Opetuspelit + Viihdepelit + Lautapelit + Pelillist?minen, data = cleanedLikert3, Hess = T)
                 
                 summary(engagementTulos)
                 testausta <- profile(engagementTulos) 
                 summary(testausta)
                 pairs(testausta)
                 
                 #P-arvot mukaan regressioon
                 korrelaatiot <- coef(summary(engagementTulos))
                 p <- pnorm(abs(korrelaatiot[, "t value"]), lower.tail = FALSE) * 2
                 cbind(korrelaatiot, "p value" = round(p,3))
                 
                 predict(engagementTulos)
                 
                 #Goodness of fit testi. Pit?isi ilmeisesti olla yli 0.05, jotta Null hypoteesi kumotaan.
                 1-pchisq(deviance(reducedmodel), df.residual(reducedmodel))
                 
                 
                 #Joku logit testi s 213
                 #house.cpr <- apply(house.pr, 1, cumsum)
                 #logit <- function(x) log(x/(1-x))
                 #house.ld <- logit(house.cpr[2, ]) - logit(house.cpr[1, ])
                 #sort(drop(house.ld))
                 #regressiotesto
                 model <- polr(engagement~  Opetuspelit + Viihdepelitt + Pelillist?minen, data=cleanedLikert3, Hess = T)
                 n1 = data.frame(1,2,4)
                 
                 #jotain oletuksen arviointeja liittyuen l??k?ridataan en tied? onko iloa?
                 oletus <- predict(engagementTulos, pepemuodot2, type = 'prob')
                 print(oletus, digits = 3)
                 
                 #Engagement ja sdt
                 # Yhteenkuuluvuus + Autonomia + Vertailu
                 sdt1 <- polr(as.factor(Arvo) ~    Yhteenkuuluvuus + Autonomia + Kompetenssi  , data = cleanedLikert3, Hess = T, model = T, method = "logistic")
                 summary(sdt1)
                 
                 coeffs <- coef(summary(sdt1))
                 p <- pnorm(abs(coeffs[, "t value"]), lower.tail = FALSE) * 2
                 cbind(coeffs, "p value" = round(p,3))
                 
                 
                 # kevennetty malli sdt ja engagement
                 
                 sdt2 <- polr(as.factor(engagement) ~ Amotivaatio + Arvo + Mastery , data = cleanedLikert3, Hess = T, model = T, method = "logistic")
                 summary(sdt2)
                 
                 coeffs <- coef(summary(sdt2))
                 p <- pnorm(abs(coeffs[, "t value"]), lower.tail = FALSE) * 2
                 cbind(coeffs, "p value" = round(p,3))
                 
                 anova(sdt2, sdt1)
                 #Sem
                 boxplot(korrelaatiot)
                 
                 
                 corrgram(cleanedLikert, order=FALSE, lower.panel=panel.shade,
                          upper.panel=panel.pie, text.panel=panel.txt,
                          main="SDT tavoiteorientaatio ja motivaatio")
                 pairs(cleanedLikert3[4:30])
                 pairs(cleanedLikert3[16:20])
                 pairs(cleanedLikert3[40:51])
                 scatter.hist(cleanedLikert[11:22])
                 
                 
                 datasetpojat <- subset(dataset, dataset$GENDER==1)
                 datasetytot <- subset(dataset, dataset$GENDER==2)
                 #t-testej?, mann whithney (wilcox) SDT-summamuuttujat
                 #mann whtiney testi/wilcox esim erottamaan tytt?jen ja poikien eroja eri osa-alueilla
                 
                 boxplot(Yhteenkuuluvuus~GENDER,data=cleanedLikert3,names=c("TPoja","Pojat"), yla="Vastausten summa")
                 t.test(Yhteenkuuluvuus~GENDER, data=cleanedLikert)
                 hist(cleanedLikert3$Yhteenkuuluvuus, ylim = c(0,60), breaks = 10)
                 hist((as.numeric(cleanedLikert$GENDER)))$counts
                 
                 wilcox.test(Yhteenkuuluvuus~GENDER, data=dataset)
                 
                 #### 
                 boxplot(Autonomia~GENDER,data=dataset,names=c("Tyt?t","Pojat"), yla="Vastausten summa")
                 t.test(Autonomia~GENDER, data=cleanedLikert)
                 hist(dataset$Autonomia, ylim = c(0,60), breaks = 10)
                 wilcox.test(Arvo~GENDER, data=dataset)
                 
                 #### Tilastollisesti merkitt?v? tyt?t enemm?n kompetenssia
                 boxplot(Autonomia~GENDER,data=dataset,names=c("Pojat","Tytöt"), yla="Vastausten summa")
                 t.test(Kompetenssi~GENDER, data=cleanedLikert)
                 hist(dataset$Kompetenssi, ylim = c(0,80), breaks = c(0,1,2,3,4,5,6,7))
                 wilcox.test(Amotivaatio~GENDER, data=dataset)
                 
                 
                 
                 #tavoiteorientaatio
                 
                 #performance approach, vertaillaan muihin oppilaisiin
                 boxplot(Vertailu~GENDER,data=dataset,names=c("Tyt?t","Pojat"), yla="Vastausten summa")
                 t.test(Vertailu~GENDER, data=dataset)
                 hist(dataset$Vertailu, ylim = c(0,60), breaks = 7)
                 wilcox.test(Vertailu~GENDER, data=dataset)
                 
                 #Avoidance
                 boxplot(Valttely~GENDER,data=dataset,names=c("Tyt?t","Pojat"), yla="Vastausten summa")
                 t.test(Valttely~GENDER, data=cleanedLikert)
                 hist(dataset$Valttely, ylim = c(0,60), breaks = 7)
                 wilcox.test(Valttely~GENDER, data=dataset)
                 
                 #Mastery melkein tilastollisesti merkitt?v?
                 boxplot(Mastery~GENDER,data=cleanedLikert,names=c("Tyt?t","Pojat"), yla="Vastausten summa")
                 t.test(Mastery~GENDER, data=cleanedLikert)
                 hist(cleanedLikert$Mastery, ylim = c(0,60), breaks = 10)
                 wilcox.test(Mastery~GENDER, data=dataset)
                 
                 
                 #Mielienkiinto sis. motivaatio
                 boxplot(Mielenkiinto~GENDER,data=dataset,names=c("Tyt?t","Pojat"), yla="Vastausten summa")
                 t.test(Mielenkiinto~GENDER, data=cleanedLikert)
                 hist(dataset$Mielenkiinto, ylim = c(0,60), breaks = 10)
                 wilcox.test(Mielenkiinto~GENDER, data=dataset)
                 
                 #Vaivann?k?
                 boxplot(Vaivannako~GENDER,data=dataset,names=c("Tyt?t","Pojat"), yla="Vastausten summa")
                 t.test(Vaivannako~GENDER, data=cleanedLikert)
                 hist(dataset$Vaivannako, ylim = c(0,60), breaks = 10)
                      wilcox.test(Vaivannako~GENDER, data=dataset)
                      
                      #Arvo
                      
                      boxplot(Arvo~GENDER,data=dataset,names=c("Tyt?t","Pojat"), yla="Vastausten summa")
                      t.test(Arvo~GENDER, data=cleanedLikert)
                      hist(dataset$Arvo, ylim = c(0,60), breaks = 10)
                      wilcox.test(Arvo~GENDER, data=dataset)
                      
                      #Amotivaatio
                      boxplot(Amotivaatio~GENDER,data=dataset,names=c("Tyt?t","Pojat"), yla="Vastausten summa")
                      t.test(Amotivaatio~GENDER, data=cleanedLikert)
                      hist(dataset$Amotivaatio, ylim = c(0,60), breaks = 10)
                      wilcox.test(Amotivaatio~GENDER, data=dataset)
                      
                      #Engagement/engagement
                      
                      boxplot(engagement~luokka,data=dataset,names=c("Tyt?t","Pojat"), yla="Vastausten summa")
                      #Luokkajakokojen vaikutus kiinnittymiseen!
                      boxplot(engagement~luokka,data=dataset,names=c("Tyt?t","Pojat"), yla="Vastausten summa")
                      t.test(engagement~GENDER, data=dataset)
                      hist(cleanedLikert3$engagement, ylim = c(0,60), breaks = 7)
                      wilcox.test(engagement~Luokka_num, data=cleanedLikert3)
                      
                      
                      kiinnittyminen <- subset(cleanedLikert3, cleanedLikert3$interest >= 4.5, na.rm = TRUE)
                      Effort <- subset(cleanedLikert3, cleanedLikert3$Vaivannako >= 5, na.rm = TRUE)
                      Arvo <- subset(cleanedLikert3, cleanedLikert3$Arvo >= 5, na.rm = TRUE)
                      Kompetenssi <- subset(cleanedLikert3, cleanedLikert3$SDTCOMP_LIKERTSUMMA_KA >= 4, na.rm = TRUE)
                      
                      
                      motiinterest <-  (nrow(subset(Mielenkiinto)))/(nrow(dataset))
                      effort <- (nrow(subset(Effort)))/(nrow(cleanedLikert3))
                      arvo <- (nrow(subset(Arvo)))/(nrow(cleanedLikert3)) 
                      
                      #Luokkajako ja sen vaikutus
                      datasetalakoulu <- subset(dataset, dataset$Luokka_num == 12)
                      datasetylakoulu <- subset(dataset, dataset$Luokka_num == 13)
                      wilcox.test(Amotivaatio~Luokka_num,data=dataset)
                      
                      barplot(table(cleanedLikert3$GRADE,cleanedLikert3$engagement), beside=T,args.legend=list(cex=0.5),cex.names=0.7,legend.text=c("4 luokka", "5 luokka", "6 luokka", "7 luokka", "8 luokka", "9 luokka"), col=c("blue","red","green","pink","yellow","orange"))
                      barplot(table(cleanedLikert3$Luokka_num,cleanedLikert3$engagement), beside=T,args.legend=list(cex=0.5),cex.names=0.7,legend.text=c("4-6","7-9"), col=c("blue", "red"))
                      
                      
                      
                      
