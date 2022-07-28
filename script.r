#library(funModeling)
library(dplyr)
library(ggplot2)

comicsData <- read.csv(file = 'Comics G5.csv')

#Analisis exploratorio de los datos

#df_status(comicsData)
#freq(comicsData)

#Personajes que pertenecen a las minorias sexuales
gsm     <- filter(comicsData, !is.na(gsm) )

#df_status(gsm)
#freq(gsm)

#Personajes que NO pertenecen a las minorias sexuales

noGsm <- filter(comicsData, is.na(gsm) )

#Numero total de apariciones de los heroes que pertenecen a una minoria sexual

gsmApperances  <- sum(
	filter(gsm, !is.na(appearances))$appearances
)

print(
	paste(
		"Los personajes que pertenecen a las minorias sexuales acumulan ",
		gsmApperances,
		" apariciones"
	)
)


#Numero total de apariciones de los heroes que NO pertenecen a una minoria sexual

noGsmApperances  <- sum(
	filter(noGsm, !is.na(appearances))$appearances
)

print(
	paste(
		"Los personajes que NO pertenecen a las minorias sexuales acumulan ",
		noGsmApperances,
		" apariciones"
	)
)

#Proporcion de apariciones 
appearancesProportion <- noGsmApperances/gsmApperances

print(
	paste(
		"Por cada aparicion de un personaje que pertenecen a las minorias sexuales hay ",
		appearancesProportion, 
		" apariciones de personajes heterosexuales"
	)
)

#Personajes con mas apariciones

mostfamousHeros <- filter( comicsData , appearances > 3000 & is.na(gsm))

print(paste(
		"4 de los personajes que NO pertenecen a las minorias sexuales acumulan ", 
		sum(mostfamousHeros$appearances),
		", un ",
		sum(mostfamousHeros$appearances)/gsmApperances*100-100,
		"% mas que los que pertenecen a las minorias sexuales"
	)
)

#Grafico de barras I proporcion de personajes gsm 
Frecuecia <- c(gsmApperances , noGsmApperances)
Porcentajes <- c(
	paste(round(gsmApperances*100/(gsmApperances+noGsmApperances),1),"%"),
	paste(round(noGsmApperances*100/(gsmApperances+noGsmApperances),1),"%")
)

Grupo <- c("GSM","Heterosexuales")
#prop <-c(1,1)

#demo <- tribble(
#  ~Grupo,         ~freq,
#  "GSM",       gsmApperances,
#  "Heterosexual",       noGsmApperances
#)

#ggplot(data = demo) +
#  geom_bar(mapping = aes(x = Grupo, y = freq, fill = Grupo), stat = "identity")



gsmProportionDf <- data.frame(Grupo,Frecuecia)

ggplot(gsmProportionDf,mapping = aes(x="",y=Frecuecia, fill=Grupo))+
  geom_bar(stat = "identity")+
  coord_polar(theta="y")+
  theme_classic()+
	scale_x_discrete("") +     # configuración eje X (etiqueta del eje)
    scale_y_continuous("") +
    geom_text(aes(label=Porcentajes),
              position=position_stack(vjust=0.5))+
    theme_void()+
    labs(
    	title="Grafico circular I",
       subtitle = "Frecuencia de las apariciones 
       de personajes heterosexuales y no heterosexuales")+
    theme(plot.margin = margin(1, 1, 1, 1, "cm"))

#Personajes gsm a lo largo de las decadas

  #Personajes gsm en los 40
fortiesDecade <- 
c("40", 
  "41", 
  "42",
  "43",
  "44",
  "45",
  "46",
  "47",
  "48",
  "49")

gsmInFortiesDecade <- filter(gsm, 
	grepl(paste(fortiesDecade, collapse="|"), 
	first_appear))

print(
	paste(
		length(gsmInFortiesDecade$name),
		" de los personajes gsm fueron creados en los 40"
	)
)

  #Personajes gsm en los 50
fitieshDecade <- 
c("50", 
  "51", 
  "52",
  "53",
  "54",
  "55",
  "56",
  "57",
  "58",
  "59")

gsmInFitiesDecade <- filter(gsm, 
	grepl(paste(fitieshDecade, collapse="|"), 
	first_appear))

print(
	paste(
		length(gsmInFitiesDecade$name),
		" de los personajes gsm fueron creados en los 50"
	)
)

  #Personajes gsm en los 60
sixtiesDecade <- 
c("60", 
  "61", 
  "62",
  "63",
  "64",
  "65",
  "66",
  "67",
  "68",
  "69")

gsmInSixtiesDecade <- filter(gsm, 
	grepl(paste(sixtiesDecade, collapse="|"), 
	first_appear))

print(
	paste(
		length(gsmInSixtiesDecade$name),
		" de los personajes gsm fueron creados en los 60"
	)
)

  #Personajes gsm en los 70
seventiesDecade <- 
c("70", 
  "71", 
  "72",
  "73",
  "74",
  "75",
  "76",
  "77",
  "78",
  "79")

gsmInSeventiesDecade <- filter(gsm, 
	grepl(paste(seventiesDecade, collapse="|"), 
	first_appear))

print(
	paste(
		length(gsmInSeventiesDecade$name),
		" de los personajes gsm fueron creados en los 70"
	)
)

  #Personajes gsm en los 80
eightiesDecade <- 
c("80", 
  "81", 
  "82",
  "83",
  "84",
  "85",
  "86",
  "87",
  "88",
  "89")

gsmInEightiesDecade <- filter(gsm, 
	grepl(paste(eightiesDecade, collapse="|"), 
	first_appear))

print(
	paste(
		length(gsmInEightiesDecade$name),
		" de los personajes gsm fueron creados en los 80"
	)
)

  #Personajes gsm en los 90
ninetiesDecade <- 
c("90", 
  "91", 
  "92",
  "93",
  "94",
  "95",
  "96",
  "97",
  "98",
  "99")

gsmInNinetiesDecade <- filter(gsm, 
	grepl(paste(ninetiesDecade, collapse="|"), 
	first_appear))

print(
	paste(
		length(gsmInNinetiesDecade$name),
		" de los personajes gsm fueron creados en los 90"
	)
)

  #Personajes gsm en los 2000
secondMileniumDecade <- 
c("00", 
  "01", 
  "02",
  "03",
  "04",
  "05",
  "06",
  "07",
  "08",
  "09")

gsmIn2milDecade <- filter(gsm, 
	grepl(paste(secondMileniumDecade, collapse="|"), 
	first_appear))

print(
	paste(
		length(gsmIn2milDecade$name),
		" de los personajes gsm fueron creados en la decada del 2010"
	)
)

#Grafico de lineas I personajes GSM a lo largo de las decadas

decadesResults <- c(
	length(gsmInFortiesDecade$name),
	length(gsmInFitiesDecade$name),
	length(gsmInSixtiesDecade$name),
	length(gsmInSeventiesDecade$name),
	length(gsmInEightiesDecade$name),
	length(gsmInNinetiesDecade$name),
	length(gsmIn2milDecade$name)
)

decades<-c(
	"1940",
	"1950",
	"1960",
	"1970",
	"1980",
	"1990",
	"2000"
)

gsmDecadesDf <- data.frame(decades,decadesResults)

ggplot(mapping=aes(decades,decadesResults, group=1,color=decadesResults))+
	
	geom_point()+
	geom_line()+
	theme_classic()+
	scale_x_discrete("Decadas") +     # configuración eje X (etiqueta del eje)
    scale_y_continuous("Frecuencia") +
    labs(
    	title="Grafico de linea I",
       subtitle = "Frecuencia 
       absoluta de la creacion de 
       personajes gsm a lo largo del sigo XX y XXI")

#Personajes GSM hombre/mujeres

gsmMen    <- filter(comicsData, !is.na(gsm) & gender=="Male" )
gsmWomen  <- filter(comicsData, !is.na(gsm) & gender=="Female" )

gsmAmounts <- c(
	length(gsmMen$name), 
	length(gsmWomen$name)
)

groups     <- c("Hombres","Mujeres")

gsmSexPercentages <- c(
	paste(round(length(gsmMen$name)*100/(length(gsmMen$name)+length(gsmWomen$name)),1),"%"),
	paste(round(length(gsmWomen$name)*100/(length(gsmMen$name)+length(gsmWomen$name)),1),"%")
)

gsmSexDf <- data.frame(groups,gsmAmounts)

ggplot(gsmSexDf,mapping = aes(x="",y=gsmAmounts, fill=groups))+
  geom_bar(stat = "identity")+
  coord_polar(theta="y")+
  theme_classic()+
	scale_x_discrete("") +     # configuración eje X (etiqueta del eje)
    scale_y_continuous("") +
    geom_text(aes(label=gsmSexPercentages),
              position=position_stack(vjust=0.5))+
    theme_void()+
    scale_fill_manual(values=c("green","orange"))+
    labs(
    	title="Grafico circular II",
       subtitle = "Cantidad de personajes no heterosexuales hombres y mujeres")+
    theme(plot.margin = margin(1, 1, 1, 1, "cm"))


#Personajes hetero y no hetero villanos

gsmVillians     <- filter(comicsData, !is.na(gsm) & align=="Bad" )

print(
	paste(
		length(gsmVillians$name),
		" personajes que pertenecen a las minorias sexuales son super villanos "
	)
)

noGsmVillians     <- filter(comicsData, is.na(gsm) & align=="Bad" )


print(
	paste(
		length(noGsmVillians$name),
		" personajes que NO pertenecen a las minorias sexuales son super villanos"
	)
)

gsmVillianAmounts <- c(
	length(gsmVillians$name), 
	length(noGsmVillians$name)
)

villianGroups     <- c("Villanos GSM","Villanos heterosexuales")

gsmVilliansPercentages <- c(
	paste(round(length(gsmVillians$name)*100/(length(gsmVillians$name)+length(noGsmVillians$name)),1),"%"),
	paste(round(length(noGsmVillians$name)*100/(length(gsmVillians$name)+length(noGsmVillians$name)),1),"%")
)

gsmVilliansDf <- data.frame(villianGroups,gsmVillianAmounts)

ggplot(gsmVilliansDf,mapping = aes(x="",y=gsmVillianAmounts, fill=villianGroups))+
  geom_bar(stat = "identity")+
  coord_polar(theta="y")+
  theme_classic()+
	scale_x_discrete("") +     # configuración eje X (etiqueta del eje)
    scale_y_continuous("") +
    geom_text(aes(label=gsmVilliansPercentages),
              position=position_stack(vjust=0.5))+
    theme_void()+
    scale_fill_manual(values=c("darkblue","yellow"))+
    labs(
    	title="Grafico circular III",
       subtitle = "Cantidad de villanos heterosexuales y no heterosexuales")+
    theme(plot.margin = margin(1, 1, 1, 1, "cm"))

#Personajes hetero y no hetero heroes

gsmHeroes     <- filter(comicsData, !is.na(gsm) & align=="Good" )

print(
	paste(
		length(gsmHeroes$name),
		" personajes que pertenecen a las minorias sexuales son super Heroes "
	)
)

noGsmHeroes     <- filter(comicsData, is.na(gsm) & align=="Good" )


print(
	paste(
		length(noGsmHeroes$name),
		" personajes que NO pertenecen a las minorias sexuales son super Heroes"
	)
)

gsmHeroesAmounts <- c(
	length(gsmHeroes$name), 
	length(noGsmHeroes$name)
)

heroesGroups     <- c("Heroes no heterosexuales","Heroes heterosexuales")

gsmHeroesPercentages <- c(
	paste(round(length(gsmHeroes$name)*100/(length(gsmHeroes$name)+length(noGsmHeroes$name)),1),"%"),
	paste(round(length(noGsmHeroes$name)*100/(length(gsmHeroes$name)+length(noGsmHeroes$name)),1),"%")
)

gsmHeroesDf <- data.frame(heroesGroups,gsmHeroesAmounts)

ggplot(gsmHeroesDf,mapping = aes(x="",y=gsmHeroesAmounts, fill=heroesGroups))+
  geom_bar(stat = "identity")+
  coord_polar(theta="y")+
  theme_classic()+
	scale_x_discrete("") +     # configuración eje X (etiqueta del eje)
    scale_y_continuous("") +
    geom_text(aes(label=gsmHeroesPercentages),
              position=position_stack(vjust=0.5))+
    theme_void()+
    scale_fill_manual(values=c("lightblue","salmon"))+
    labs(
    	title="Grafico circular IV",
       subtitle = "Cantidad de heroes heterosexuales y no heterosexuales")+
    theme(plot.margin = margin(1, 1, 1, 1, "cm"))

#Proporcion villanos gsm

heroesGsmProportion <- length(noGsmHeroes$name)/length(gsmHeroes$name)

print(
	paste(
		"Por cada heore que pertenece a las minorias sexuales hay ",
		round(heroesGsmProportion,1), 
		" villanos heterosexuales"
	)
)



#Personajes mujeres

womenChars      <- filter(comicsData, gender=="Female" )

womenVillians   <- filter(womenChars, align=="Bad" )



