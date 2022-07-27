library(funModeling)
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

gsmApperances  <- sum(filter(gsm, !is.na(appearances))$appearances)
print(
	paste(
		"Los personajes que pertenecen a las minorias sexuales acumulan ",
		gsmApperances,
		" apariciones"
	)
)


#Numero total de apariciones de los heroes que NO pertenecen a una minoria sexual

noGsmApperances  <- sum(filter(noGsm, !is.na(appearances))$appearances)

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
count <- c(gsmApperances , noGsmApperances)
#GenderSex <- c("gsm","heterosexuales")
#prop <-c(1,1)

demo <- tribble(
  ~Grupo,         ~freq,
  "GSM",       gsmApperances,
  "Heterosexual",       noGsmApperances
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = Grupo, y = freq, fill = Grupo), stat = "identity")+
  	theme_classic()+
	scale_x_discrete("Grupo") +     # configuración eje X (etiqueta del eje)
    scale_y_continuous("Frecuencia absoluta") +
    labs(
    	title="Grafico de barras I",
       subtitle = "Frecuencia absoluta de las apariciones de personajes gsm y no gsm")

#gsmProportionDf <- data.frame(GenderSex,count,prop)


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

gsmInFortiesDecade <- filter(gsm, grepl(paste(fortiesDecade, collapse="|"), first_appear))

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

gsmInFitiesDecade <- filter(gsm, grepl(paste(fitieshDecade, collapse="|"), first_appear))

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

gsmInSixtiesDecade <- filter(gsm, grepl(paste(sixtiesDecade, collapse="|"), first_appear))

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

gsmInSeventiesDecade <- filter(gsm, grepl(paste(seventiesDecade, collapse="|"), first_appear))

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

gsmInEightiesDecade <- filter(gsm, grepl(paste(eightiesDecade, collapse="|"), first_appear))

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

gsmInNinetiesDecade <- filter(gsm, grepl(paste(ninetiesDecade, collapse="|"), first_appear))

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

gsmIn2milDecade <- filter(gsm, grepl(paste(secondMileniumDecade, collapse="|"), first_appear))

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
       subtitle = "Frecuencia absoluta de la creacion de personajes gsm a lo largo del sigo XX y XXI")

#Personajes villanos

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

#Proporcion villanos gsm

villiansGsmProportion <- length(noGsmVillians$name)/length(gsmVillians$name)

print(
	paste(
		"Por cada personaje que pertenece a las minorias sexuales hay ",
		villiansGsmProportion, 
		" personajes heterosexuales"
	)
)


#Personajes mujeres

womenChars      <- filter(comicsData, gender=="Female" )

womenVillians   <- filter(womenChars, align=="Bad" )
