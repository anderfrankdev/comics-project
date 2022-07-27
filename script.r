library(funModeling)
library(dplyr)

comicsData <- read.csv(file = 'Comics G5.csv')

#Analisis exploratorio de los datos

#df_status(comicsData)
#freq(comicsData)

#Personajes que pertenecen a las minorias sexuales
homosexuals     <- filter(comicsData, !is.na(gsm) )

df_status(homosexuals)
freq(comicsData)

#Personajes que NO pertenecen a las minorias sexuales

heterosexuals <- filter(comicsData, is.na(gsm) )

#Numero total de apariciones de los heroes que pertenecen a una minoria sexual

homoApperances  <- sum(filter(homosexuals, !is.na(appearances))$appearances)
print(
	paste(
		"Los personajes que pertenecen a las minorias sexuales acumulan ",
		homoApperances,
		" apariciones"
	)
)


#Numero total de apariciones de los heroes que NO pertenecen a una minoria sexual

heteroApperances  <- sum(filter(heterosexuals, !is.na(appearances))$appearances)

print(
	paste(
		"Los personajes que NO pertenecen a las minorias sexuales acumulan ",
		heteroApperances,
		" apariciones"
	)
)

#Proporcion de apariciones 
appearancesProportion <- heteroApperances/homoApperances

print(
	paste(
		"Por cada aparicion de un personaje que pertenecen a las minorias sexuales hay ",
		appearancesProportion, 
		" apariciones de personajes heterosexuales"
	)
)

