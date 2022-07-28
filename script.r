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

Grupo <- c("No heterosexuales","Heterosexuales")
#prop <-c(1,1)



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
       subtitle = "Frecuencia de las apariciones de personajes heterosexuales y no heterosexuales")+
    theme(plot.margin = margin(1, 1, 1, 1, "cm"))+
    guides(fill=guide_legend(title="Orientación sexual"))

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
    labs(title="Grafico de linea I",subtitle = "Frecuencia absoluta de la creacion de personajes gsm a lo largo del sigo XX y XXI")+
    theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))+
    guides(color=guide_legend(title="Frecuencia"))


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
    theme(plot.margin = margin(1, 1, 1, 1, "cm"))+
    guides(fill=guide_legend(title="Sexo"))

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

villianGroups     <- c("No heterosexuales","Heterosexuales")

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
    scale_fill_manual(values=c("yellow","darkblue"))+
    labs(
    	title="Grafico circular III",
       subtitle = "Cantidad de villanos heterosexuales y no heterosexuales")+
    theme(plot.margin = margin(1, 1, 1, 1, "cm"))+
    guides(fill=guide_legend(title="Orientación sexual"))

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
    theme(plot.margin = margin(1, 1, 1, 1, "cm"))+
    guides(fill=guide_legend(title="Orientación sexual"))


#Proporcion villanos gsm

heroesGsmProportion <- length(noGsmHeroes$name)/length(gsmHeroes$name)

print(
	paste(
		"Por cada heore que pertenece a las minorias sexuales hay ",
		round(heroesGsmProportion,1), 
		" villanos heterosexuales"
	)
)



#Personajes mujeres y mujeres

womenChars      <- filter(comicsData, gender=="Female" )

womenVillians   <- filter(womenChars, align=="Bad" )

menChars   <- filter(comicsData, gender=="Male")


#Grafico de barras

demo <- tribble(
  ~Grupo,         ~Sexo  ,        ~freq,
  "Nro. de heroes",       "Hombres",        length(filter(menChars, align=="Good")$name),
  "Nro. de heroes",       "Mujeres",        length(filter(womenChars,    align=="Good")$name),
  "Nro. de villanos",     "Hombres",         length(filter(menChars, align=="Bad")$name),
  "Nro. de villanos",		 "Mujeres",        length(womenVillians$name),
  "Nro. de apariciones",		 "Hombres",        length(filter(menChars, !is.na(appearances))$name),
  "Nro. de apariciones",		 "Mujeres",        length(filter(womenChars, !is.na(appearances))$name)

)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = Grupo, y = freq, fill = Sexo), stat = "identity",position="dodge")+
  scale_y_continuous(breaks=0:32/2*1000,name="")+
  theme_classic()+
	scale_x_discrete("") +     # configuración eje X (etiqueta del eje)

    labs(
    	title="Grafico de barras I",
       subtitle = "Cantidad de heroes heterosexuales y no heterosexuales")+
    theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))+
    guides(fill=guide_legend(title="Sexo"))


#Grafico de caja y bigotes
gsmAndNoGsmApperancesDf <- filter(gsm, !is.na(appearances) )

ggplot(data = gsmAndNoGsmApperancesDf, mapping = aes(x = gsm, y = appearances,fill=gsm)) + 
  geom_boxplot()+
	coord_flip()+
  theme_classic()+
	scale_x_discrete("") +     # configuración eje X (etiqueta del eje)
    scale_y_continuous("") +
    labs(
    	title="Grafico de caja I",
       subtitle = "Dispersión de apariciones de personajes no heterosexuales")+
    theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))+
    guides(fill=guide_legend(title="Orientación sexual"))

#Medidas para el Grafico de caja y bigotes
print(unique(gsm$gsm))

#Bisexuales
bisexualsAppearences = filter(gsm, !is.na(appearances) & gsm=="Bisexual Characters")

bisexualsAppearencesMax = max(bisexualsAppearences$appearances)
bisexualsAppearencesMin = min(bisexualsAppearences$appearances)


profiling_num(bisexualsAppearences)


print(bisexualsAppearencesMax)
print(bisexualsAppearencesMin)

#Homo
homosexualAppearences = filter(gsm, !is.na(appearances) & gsm=="Homosexual Characters")

homosexualAppearencesMax = max(homosexualAppearences$appearances)
homosexualAppearencesMin = min(homosexualAppearences$appearances)


profiling_num(homosexualAppearences)

print(homosexualAppearencesMax)
print(homosexualAppearencesMin)

#Fluid
fluidAppearences = filter(gsm, !is.na(appearances) & gsm=="Genderfluid Characters")

fluidAppearencesMax = max(fluidAppearences$appearances)
fluidAppearencesMin = min(fluidAppearences$appearances)

print(fluidAppearences$gsm)

profiling_num(fluidAppearences)

print(fluidAppearencesMax)
print(fluidAppearencesMin)

#Pan
PansexualAppearences = filter(gsm, !is.na(appearances) & gsm=="Pansexual Characters")

PansexualAppearencesMax = max(PansexualAppearences$appearances)
PansexualAppearencesMin = min(PansexualAppearences$appearances)

print(PansexualAppearences$gsm)

profiling_num(PansexualAppearences)

print(PansexualAppearencesMax)
print(PansexualAppearencesMin)

#Pan
TransgenderAppearences = filter(gsm, !is.na(appearances) & gsm=="Transgender Characters")

print(TransgenderAppearences$name)

profiling_num(TransgenderAppearences)

TransgenderAppearencesMax = max(TransgenderAppearences$appearances)
TransgenderAppearencesMin = min(TransgenderAppearences$appearances)

print(TransgenderAppearencesMax)
print(TransgenderAppearencesMin)

#Personajes women en los 40

womenInFortiesDecade <- filter(womenChars, 
	grepl(paste(fortiesDecade, collapse="|"), 
	first_appear))

print(
	paste(
		length(womenInFortiesDecade$name),
		" de los personajes mujeres fueron creados en los 40"
	)
)

  #Personajes women en los 50

womenInFitiesDecade <- filter(womenChars, 
	grepl(paste(fitieshDecade, collapse="|"), 
	first_appear))

print(
	paste(
		length(womenInFitiesDecade$name),
		" de los personajes mujeres fueron creados en los 50"
	)
)

#Personajes women en los 60

womenInSixtiesDecade <- filter(womenChars, 
	grepl(paste(sixtiesDecade, collapse="|"), 
	first_appear))

print(
	paste(
		length(womenInSixtiesDecade$name),
		" de los personajes mujeres fueron creados en los 60"
	)
)

  #Personajes women en los 70

womenInSeventiesDecade <- filter(womenChars, 
	grepl(paste(seventiesDecade, collapse="|"), 
	first_appear))

print(
	paste(
		length(womenInSeventiesDecade$name),
		" de los personajes mujeres fueron creados en los 70"
	)
)

  #Personajes women en los 80

womenInEightiesDecade <- filter(womenChars, 
	grepl(paste(eightiesDecade, collapse="|"), 
	first_appear))

print(
	paste(
		length(womenInEightiesDecade$name),
		" de los personajes mujeres fueron creados en los 80"
	)
)

  #Personajes women en los 90

womenInNinetiesDecade <- filter(womenChars, 
	grepl(paste(ninetiesDecade, collapse="|"), 
	first_appear))

print(
	paste(
		length(womenInNinetiesDecade$name),
		" de los personajes mujeres fueron creados en los 90"
	)
)

  #Personajes women en los 2000

womenIn2milDecade <- filter(womenChars, 
	grepl(paste(secondMileniumDecade, collapse="|"), 
	first_appear))

print(
	paste(
		length(womenIn2milDecade$name),
		" de los personajes mujeres fueron creados en la decada del 2010"
	)
)

#Grafico de lineas II personajes mujeres a lo largo de las decadas

decadesWomenResults <- c(
	length(womenInFortiesDecade$name),
	length(womenInFitiesDecade$name),
	length(womenInSixtiesDecade$name),
	length(womenInSeventiesDecade$name),
	length(womenInEightiesDecade$name),
	length(womenInNinetiesDecade$name),
	length(womenIn2milDecade$name)
)

womenDecadesDf <- data.frame(decades,decadesWomenResults)

ggplot(mapping=aes(decades,decadesWomenResults, group=1,color=decadesWomenResults))+
	
	geom_point()+
	geom_line()+
	theme_classic()+
	scale_x_discrete("Decadas") +     # configuración eje X (etiqueta del eje)
    scale_y_continuous("Frecuencia") +
    labs(title="Grafico de lineas II",subtitle = "Frecuencia absoluta de la creacion de personajes mujeres a lo largo del sigo XX y XXI")+
    theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))+
    guides(color=guide_legend(title="Frecuencia"))