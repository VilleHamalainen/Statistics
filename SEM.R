#Motivaation mallinnusta Lavaan paketin avulla


####Explatory faktori analyysi, jolla tutkitaan dataa olettamatta mit??n ja katsotaan faktoreiden m??r??###

rownames(cleanedLikert4) <- colnames(cleanedLikert4)

#Parallel analysis suggests that the number of factors =  5,6  and the number of components =  NA 
parallele = fa.parallel(semMuuttujat[c(2,4:38)], fm="ml", main = "Aineiston ominaisarvot", ylabel = "Omainaisarvojen määrä", show.legend = TRUE, fa="fa")
trace("fa.parallel", edit=TRUE)
kakkaa(semMuuttujat[c(2,4:38)], fm="ml", main = "Aineiston ominaisarvot", show.legend = TRUE, fa="fa")
fa.parallelOma()
#kaivetaan arvot parallele valuesta # .7 ja 1 breakpoints
parallele$fa.values

#Rakenne loadings yli .3 muodostavat faktorin
sevenfactor = fa(cleanedLikert3[c(2,4:15)], nfactors = 4, rotate = "oblimin", fm = "ml")
sevenfactor

#multihistogrammi latenteille muuttujille(multivariatenormality)
x_vars <- semLatentit[,c(6:15)]
uniPlot(x_vars, type = "histogram")

#Outlierien tarkistusta
latentit2 <- semMuuttujat[6:38]
# Mahalanobis distance
result1 <- mvOutlier(latentit2, qqplot = TRUE, method = "quan")
# Adjusted Mahalanobis distance
result2 <- mvOutlier(latentit2, qqplot = TRUE, method = "adj.quan")
outFree <- as.data.frame(result1[['newData']])
ok<-hzTest(semLatentit, qqplot = FALSE)
ok
###POLKU ANALYYSI###

# input covariances tai korrelaatiot joiden avulla tutkitaan suhteita. Summamuuttujien korrelaatiotable
example.cor <- lav_matrix_lower2full(c(1, 0.512, 1, 0.286, 0.4, 1, 0.244, 0.210, 0.304, 1, 0,235,0.215,0.262, 0.654,
                                       1, 0.236, 0.257, 0.188, 0.598, 0.617, 1, -0.009, 0.096, 0.275, 0.473, 0.371, 0.355,1
                                       ,0.218, 0.220, 0.386, 0.445, 0.466, 0.444, 0.543,1, 0.126,	0.194,	0.235,	0.664,	0.524,	0.535,	0.720,	0.551,1,
                                       0.166,	0.115,	-0.164	,-0.171,	-0.101,	-0.081,	-0.574,	-0.268,	-0.393))

summary(example.cor)

#otetaan suoraan datasta spearman
example2.cor <-lavCor(semLatentit[1:15], method = "spearman")

# name the rows and columns 
rownames(example2.cor) <- colnames(example2.cor)

#Amotivaatio =~ MOT8AMOTIV_LIKERT + MOT11AMOTIV_LIKERT + MOT7AMOTIV_LIKERT
sismotivaatio <- '

Taito =~ GOAL3MAST_LIKERT + GOAL6MAST_LIKERT + GOAL9MAST_LIKERT
Vertailu =~ GOAL2PAV_LIKERT + GOAL4PAV_LIKERT + GOAL8PAV_LIKERT

Kompetenssi =~ SDT1COMP_LIKERT + SDT5COMP_LIKERT 
Autonomia =~ SDT2AUT_LIKERT + SDT3AUT_LIKERT
Yhteenkuuluvuus =~  SDT4REL_LIKERT + SDT8REL_LIKERT  

Interest =~ MOT1INTEREST_LIKERT  + MOT10INTEREST_LIKERT
Usefulness =~  MOT5VALUE_LIKERT +  MOT9VALUE_LIKERT + MOT12VALUE_LIKERT 
Effort =~ MOT2EFFORT_LIKERT + MOT3EFFORT_LIKERT 

Sismotivaatio =~ Interest + Usefulness + Effort
Sismotivaatio ~ Taito + Vertailu + Kompetenssi + Autonomia+ Yhteenkuuluvuus 
'

#M1 gradussa on tämä malli, jossa testataan yhtä motivaatiofaktoria
yhdenfaktorinmalli <-'
    Moti =~ MOT1INTEREST_LIKERT  + MOT10INTEREST_LIKERT +  MOT5VALUE_LIKERT +  MOT9VALUE_LIKERT + MOT12VALUE_LIKERT + MOT2EFFORT_LIKERT + MOT3EFFORT_LIKERT 
     
    Kyv =~ SDT1COMP_LIKERT + SDT5COMP_LIKERT 
    Aut =~ SDT2AUT_LIKERT + SDT3AUT_LIKERT
    Yht =~  SDT4REL_LIKERT + SDT8REL_LIKERT                  
    Tai =~  GOAL6MAST_LIKERT + GOAL9MAST_LIKERT + GOAL3MAST_LIKERT
    Ver =~ GOAL1PAPP_LIKERT + GOAL5PAPP_LIKERT + GOAL7PAPP_LIKERT 
    Väl =~ GOAL2PAV_LIKERT + GOAL4PAV_LIKERT + GOAL8PAV_LIKERT 

     Moti ~ Kyv +Aut+ Yht +Tai+  Ver + Väl
  

'

#Amotivaatiomalli, jossa yhdistetty motivaation osa-alueita eli M2
yhdenfaktorinmalli <-'
    Moti =~ MOT1INTEREST_LIKERT  + MOT10INTEREST_LIKERT +  MOT5VALUE_LIKERT +  MOT9VALUE_LIKERT + MOT12VALUE_LIKERT + MOT2EFFORT_LIKERT + MOT3EFFORT_LIKERT 
    Amot =~ MOT8AMOTIV_LIKERT + MOT11AMOTIV_LIKERT + MOT7AMOTIV_LIKERT 
    PsyT =~ SDT1COMP_LIKERT + SDT5COMP_LIKERT + SDT4REL_LIKERT + SDT8REL_LIKERT + SDT2AUT_LIKERT + SDT3AUT_LIKERT                  
    Tai =~  GOAL6MAST_LIKERT + GOAL9MAST_LIKERT + GOAL3MAST_LIKERT

     Moti ~ PsyT +Tai
     Amot ~ PsyT +Tai


'
# laaja malli latausten tutkimiseen
perusmalli <-'

    Mie =~ MOT1INTEREST_LIKERT  + MOT10INTEREST_LIKERT 
    Hyö =~  MOT5VALUE_LIKERT +  MOT9VALUE_LIKERT + MOT12VALUE_LIKERT 
    Vai =~ MOT2EFFORT_LIKERT + MOT3EFFORT_LIKERT 
    Kyv =~ SDT1COMP_LIKERT + SDT5COMP_LIKERT 
    Aut =~ SDT2AUT_LIKERT + SDT3AUT_LIKERT
    Yht =~  SDT4REL_LIKERT + SDT8REL_LIKERT                  
    Tai =~  GOAL6MAST_LIKERT + GOAL9MAST_LIKERT + GOAL3MAST_LIKERT

    Mie ~ Kyv+ Aut+ Yht
    Hyö ~ Kyv + Aut + Yht
    Vai ~ Kyv + Aut + Tai 
                
'
#Malli, joss sis. moti selitetään psyk tarpeilla ja taito-orientaatiolla Yhteenkuuluvuus =~ SDT2AUT_LIKERT + SDT3AUT_LIKERT
#Engagement muodostettu mielenkiinto ja vaivnnäöstä
malli.motivaatio1 <- '  


Tai =~  GOAL6MAST_LIKERT + GOAL9MAST_LIKERT + GOAL3MAST_LIKERT
PsyT =~ SDT1COMP_LIKERT + SDT5COMP_LIKERT + SDT4REL_LIKERT + SDT8REL_LIKERT + SDT2AUT_LIKERT + SDT3AUT_LIKERT 
Hyö =~  MOT5VALUE_LIKERT +  MOT9VALUE_LIKERT + MOT12VALUE_LIKERT 
Eng =~ MOT1INTEREST_LIKERT  + MOT10INTEREST_LIKERT + MOT2EFFORT_LIKERT + MOT3EFFORT_LIKERT 

PsyT ~ Tai 
Eng ~ PsyT 
Hyö ~ PsyT 
 
                
'
#Malli jos testaillaa erikseen latauksia ja selityksiä 
#Ver =~ GOAL2PAV_LIKERT + GOAL4PAV_LIKERT + GOAL8PAV_LIKERT 
#Väl =~ GOAL1PAPP_LIKERT + GOAL5PAPP_LIKERT + GOAL7PAPP_LIKER #effortr sopii hyvin amotivaationn eo sovikkaan t kivi
perusmalli2<-'
Mie =~ MOT1INTEREST_LIKERT  + MOT10INTEREST_LIKERT 
Hyö =~  MOT5VALUE_LIKERT +  MOT9VALUE_LIKERT + MOT12VALUE_LIKERT 
Vai =~ MOT2EFFORT_LIKERT + MOT3EFFORT_LIKERT 
KyvAut =~ SDT1COMP_LIKERT + SDT5COMP_LIKERT + SDT2AUT_LIKERT + SDT3AUT_LIKERT 
Yht =~  SDT4REL_LIKERT + SDT8REL_LIKERT                  
Tai =~  GOAL6MAST_LIKERT + GOAL9MAST_LIKERT + GOAL3MAST_LIKERT
Amot =~ MOT8AMOTIV_LIKERT + MOT11AMOTIV_LIKERT + MOT7AMOTIV_LIKERT 

Amot ~ KyvAut + Yht
Mie ~ KyvAut + Yht
Hyö ~ KyvAut + Yht
Vai ~ KyvAut 


'



#M3 gradussa
malli.motivaatio6 <- '  

Mie =~ MOT1INTEREST_LIKERT  + MOT10INTEREST_LIKERT 
Hyö =~  MOT5VALUE_LIKERT +  MOT9VALUE_LIKERT + MOT12VALUE_LIKERT 
Vai =~ MOT2EFFORT_LIKERT + MOT3EFFORT_LIKERT 
PsyT =~ SDT1COMP_LIKERT + SDT5COMP_LIKERT + SDT4REL_LIKERT + SDT8REL_LIKERT + SDT2AUT_LIKERT + SDT3AUT_LIKERT 
Tai =~ GOAL6MAST_LIKERT + GOAL9MAST_LIKERT + GOAL3MAST_LIKERT


Mie ~ PsyT + Tai
Hyö ~ PsyT + Tai
Vai ~ PsyT + Tai

'


#M3 gradussa uusi mallli
malli.motivaatio2 <- '  

 
PsyT =~ SDT1COMP_LIKERT + SDT5COMP_LIKERT + SDT4REL_LIKERT + SDT8REL_LIKERT + SDT2AUT_LIKERT + SDT3AUT_LIKERT 
Tav =~ GOAL2PAV_LIKERT + GOAL4PAV_LIKERT + GOAL8PAV_LIKERT +GOAL6MAST_LIKERT + GOAL9MAST_LIKERT + GOAL3MAST_LIKERT+GOAL1PAPP_LIKERT + GOAL5PAPP_LIKERT + GOAL7PAPP_LIKERT 
Amot =~ MOT8AMOTIV_LIKERT + MOT11AMOTIV_LIKERT + MOT7AMOTIV_LIKERT
Moti =~ MOT1INTEREST_LIKERT  + MOT10INTEREST_LIKERT +  MOT5VALUE_LIKERT +  MOT9VALUE_LIKERT + MOT12VALUE_LIKERT + MOT2EFFORT_LIKERT + MOT3EFFORT_LIKERT 

Moti ~ PsyT + Tav
Amot ~ PsyT

'
#Psyktarpeet =~ Kompetenssi + Autonomia + Yhteenkuuluvuus  Usefulness ~ Kompetenssi + Autonomia + Yhteenkuuluvuus +Tavoiteorientaatiot
#Effort ~ Kompetenssi + Autonomia + Yhteenkuuluvuus + Tavoiteorientaatiot
malli.motivaatio4 <- '  

Kyv =~ SDT1COMP_LIKERT + SDT5COMP_LIKERT 
Aut =~  SDT2AUT_LIKERT + SDT3AUT_LIKERT
Yht =~ SDT4REL_LIKERT + SDT8REL_LIKERT 
Tai =~ GOAL6MAST_LIKERT + GOAL9MAST_LIKERT + GOAL3MAST_LIKERT
Mie =~ MOT1INTEREST_LIKERT  + MOT10INTEREST_LIKERT 
Hyö =~  MOT5VALUE_LIKERT +  MOT9VALUE_LIKERT + MOT12VALUE_LIKERT 
Vai =~ MOT2EFFORT_LIKERT + MOT3EFFORT_LIKERT 

Mie ~ Kyv+ Aut
Hyö ~ Kyv+ Aut
Vai ~ Kyv+ Aut + Tai
'
#Kyvykkyys erikseen engageentin kanssa
malli.motivaatio5 <-'


Tai =~  GOAL6MAST_LIKERT + GOAL9MAST_LIKERT + GOAL3MAST_LIKERT
PsyTKA =~ SDT1COMP_LIKERT + SDT5COMP_LIKERT + SDT2AUT_LIKERT + SDT3AUT_LIKERT 
Yht =~ SDT4REL_LIKERT + SDT8REL_LIKERT 
Hyö =~  MOT5VALUE_LIKERT +  MOT9VALUE_LIKERT + MOT12VALUE_LIKERT 
Eng =~ MOT1INTEREST_LIKERT  + MOT10INTEREST_LIKERT + MOT2EFFORT_LIKERT + MOT3EFFORT_LIKERT 

PsyTKA ~ Tai 
Eng ~ PsyTKA + Yht
Hyö ~ PsyTKA + Yht
 

'

 #+ GOAL1PAPP_LIKERT  + GOAL5PAPP_LIKERT + GOAL7PAPP_LIKERT  Amot =~ MOT8AMOTIV_LIKERT + MOT11AMOTIV_LIKERT + MOT7AMOTIV_LIKERT 
malli.motivaatio3 <- '  

Kyv =~ SDT1COMP_LIKERT + SDT5COMP_LIKERT 
Aut =~  SDT2AUT_LIKERT + SDT3AUT_LIKERT
Yht =~ SDT4REL_LIKERT + SDT8REL_LIKERT 
Tai =~ GOAL6MAST_LIKERT + GOAL9MAST_LIKERT + GOAL3MAST_LIKERT
Ver =~ GOAL1PAPP_LIKERT + GOAL5PAPP_LIKERT + GOAL7PAPP_LIKERT
Väl =~ GOAL2PAV_LIKERT + GOAL4PAV_LIKERT + GOAL8PAV_LIKERT
Mie =~ MOT1INTEREST_LIKERT  + MOT10INTEREST_LIKERT 
Hyö =~  MOT5VALUE_LIKERT +  MOT9VALUE_LIKERT + MOT12VALUE_LIKERT 
Vai =~ MOT2EFFORT_LIKERT + MOT3EFFORT_LIKERT 

Kyv ~~ Tai + Ver
PsykT ~ Kyv + Aut + Yht    
Mie ~ PsykT  + Tai
Hyö ~ PsykT  + Tai
Vai ~ PsykT  + Tai

'
#Erilaisia malli-ideoita, eli psyk tarpeet ja tavoiteorientaatio selittää nautintoa

#Interest ~~ Usefulness GOAL1PAPP_LIKERT +  GOAL5PAPP_LIKERT + GOAL7PAPP_LIKERT
#GOAL2PAV_LIKERT+ GOAL4PAV_LIKERT + GOAL8PAV_LIKERT 
#Interest ~~ Effort
#Usefulness ~~ Effort

# regressions

#Kiinnittyminen =~ Mielenkiinto + Vaivannako + Arvo  

#variances and covariance
#Mielenkiinto ~~ Vaivannako
#Vaivannako ~~ Arvo
#Mielenkiinto ~~ Arvo




#psyt ja orientaaitot pelkästään
malli.uusi <- '
Kyv =~ SDT1COMP_LIKERT + SDT5COMP_LIKERT 
Aut =~  SDT2AUT_LIKERT + SDT3AUT_LIKERT
Yht =~ SDT4REL_LIKERT + SDT8REL_LIKERT 
Tai =~  GOAL6MAST_LIKERT + GOAL9MAST_LIKERT + GOAL3MAST_LIKERT
Ver =~ GOAL1PAPP_LIKERT + GOAL5PAPP_LIKERT + GOAL7PAPP_LIKERT 
Väl =~ GOAL2PAV_LIKERT + GOAL4PAV_LIKERT + GOAL8PAV_LIKERT 



'
malli.uusi2<-as.data.frame(malli.uusi)

#soveltuvuudet
testifit<-sem(malli.motivaatio4, data=semMuuttujat, std.lv=TRUE)# estimator = "MLM
testifit<-sem(perusmalli, data=semMuuttujat,  std.lv=TRUE)
testifit<-sem(yhdenfaktorinmalli, data=semMuuttujat, std.lv=TRUE) 
testifit<-cfa(malli.uusi, data = semMuuttujat, std.lv=TRUE)
testifit5<-sem(malli.motivaatio2, data=semMuuttujat) #mielenkiinto

#Yhteenvedot
summary(testifit)
Summary(testifit2)
summary(testifit3)

measurementInvariance() 
summary(testifit, fit.measures=TRUE, standardized = TRUE) #Käytä tätä
testifit
parasmalli<-sem(malli.sdt, data=cleanedLikert3)
summary(parasmalli)
inspect(testifit,what="std")
#tutkitaan asioita#X^2 ei merkityksellinen, RMSEA, SRMR = pieni? lukuja, CFI/TLI = isoja lukuja

is.atomic(malli.uusi)
fitted(testifit)
residuals(malli.uusi)
fitmeasures(testifit)
lavaan::modificationindices(malli.uusi)
xtable(semMuuttujat)
#mallin arviointia MAlli1, Malli2, Malli2
#rmsr =
#rmsea =
#tli =  Tucker-Lewis Index (TLI)                       0.948 < 0.9 on bäd
#cfi=  Comparative Fit Index (CFI)                    0.960
#chisquare = 161.625

#chi square laskenta manuaalisesti chisq - degrees of freedom mallin arvioinnin vuoksi yleens? korkeampi kuin tli
1-((fivefactor$STATISTIC -fivefactor$dof)/(fivefactor$null.chisq-fivefactor$null.dof))
#4 eigenarvoa -> faktorit analyysille 4-5? 

#fit summary
summary(testifit, fit.measures=TRUE)
fitMeasures(testifit)
AIC(testifit)
nfi(testifit)
#piirret??n selkeytt?vi? kuvioita edge.color = "black"
semPaths(testifit, intercepts = TRUE, whatlabels = "par", layout = "tree")
semPaths(testifit, intercepts = TRUE, whatlabels = "par", layout = "tree2"  , posCol = "green", rotation = 2,"std", edge.color = "black")
inspect(testifit, what="std")
fitted(testifit)
