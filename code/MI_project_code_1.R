#Rohan Carter Rau
#main file to do XYZ

library(readstata13)
library(countrycode)
library(dplyr)
library(plyr)
library(WriteXLS)
library(readxl)
library(foreign)

#works but not preferred
setwd('./raw_data/TSCS')
temp = list.files(pattern =".dta")
for (i in 1:length(temp)) assign(temp[i], read.dta13(temp[i]))
setwd('..')
setwd('..')

#doesn't work?
#temp = list.files(path = "./raw_data/TSCS", pattern =".dta")
#for (i in 1:length(temp)) assign(temp[i], read.dta13(temp[i]))

#Country-year data. ccodes = ISO3-Numeric. 
#Made up iso-3 codes:
EU <- 999
ASL <- 1000
CHK <- 1001
CHM <- 1002 
IMY <- 1003
KBI <- 1004
.M  <- 1005
RUM <- 1006
SER <- 1007
TMP <- 1008
VIU <- 1009
BelgiumAndLuxembourg <- 1010
EastAfrica <- 1011 
zanzibar <- 1012
tanganika <- 1013
mecklenburg  <- 1014
HesseGd  <- 1015
HesseEl  <- 1016
Hannover <- 1017
Channel  <- 1018
OrangeFreeState  <- 1019
tuscany  <- 1020
parma  <- 1021
modena  <- 1022
sicily  <- 1023
sardinia  <- 1024
wuttemberg  <- 1025
saxony  <- 1026
baden  <- 1027
bavaria  <- 1028
CentralAmericanUnion <- 1029
Kosovo <- 1030
UnitedProvinceCA <- 1040 
Prussia <- 1041

A2008  <- A2008_IO_RepData.dta
A2008$ccode <- countrycode(A2008$country, "country.name", "iso3n", warn = TRUE)
#id column
A2008 <- cbind(A2008 = "A2008", A2008)


AP2011   <- AP2011_IO_RepData.dta
AP2011$ccode <- countrycode(AP2011$country, "country.name", "iso3n", warn = TRUE)
AP2011$ccode[AP2011$country == "Serbia and Montenegro"] <- 891
#Missing codes: Belgium and Luxembourg, East Africa 
AP2011$ccode[AP2011$country == "Belgium and Luxembourg"] <- BelgiumAndLuxembourg
AP2011$ccode[AP2011$country == "East Africa"] <- EastAfrica
#id column
AP2011 <- cbind(AP2011 = "AP2011", AP2011)


AS2012   <- AS2012_IO_RepData.dta
AS2012$ccode <- countrycode(AS2012$country, "country.name", "iso3n", warn = TRUE)
AS2012$ccode[AS2012$country == "European Union (15)"] <- EU
AS2012$ccode[AS2012$country == "Yugoslavia, FR"] <- 890
#id column
AS2012 <- cbind(AS2012 = "AS2012", AS2012)


B2008    <- B2008_WP_RepData.dta
trim <- function (x) sub("\\s+$", "", x)
B2008$abbreviation <- trim(B2008$abbreviation)
B2008$country <- tolower(B2008$country)
B2008$ccode <- NA
B2008$ccode <- countrycode(B2008$country, "country.name", "iso3n", warn = TRUE)
B2008$ccode[B2008$abbreviation == "VIRG"] <- 850 #virgin islands
B2008$ccode[B2008$abbreviation == "PYF"] <- 258 #french polynesia
B2008$ccode[B2008$abbreviation == "ASM"] <- 016 #american samoa
B2008$ccode[B2008$abbreviation == "LIE"] <- 438 #liechstenstein
B2008$ccode[B2008$abbreviation == "UAE"] <- 784 #united arab emirates
B2008$ccode[B2008$abbreviation == "YAR"] <- 886 #yemen arab republic
B2008$ccode[B2008$abbreviation == "EQG"] <- 226 #equatorial guinea
B2008$ccode[B2008$abbreviation == "YGS"] <- 890 #yugoslavia
for(i in 1:nrow(B2008)){
  if(is.na(B2008[i, "ccode"])){
    B2008[[i, "ccode"]] <- countrycode(B2008[i, "imfcode"], "imf", "iso3n", warn = FALSE)
  }
}
for(i in 1:nrow(B2008)){
  if(is.na(B2008[i, "ccode"])){
    B2008[[i, "ccode"]] <- countrycode(B2008[i, "imfcode"], "imf", "iso3n", warn = FALSE)
  }
}
#'county' that didn't convert: zanzibar tanganika mecklenburg 'hesse gd' 'hesse el' hannover channel 
#'orange free state' tuscany parma modena sicily sardinia wuttemberg saxony baden bavaria 
#'central american union' 
B2008$ccode[B2008$abbreviation == "ZANZ"] <- zanzibar
B2008$ccode[B2008$abbreviation == "TANGK"] <- tanganika
B2008$ccode[B2008$abbreviation == "MECK"] <- mecklenburg
B2008$ccode[B2008$abbreviation == "HESS-GD"] <- HesseGd
B2008$ccode[B2008$abbreviation == "HESS-EL"] <- HesseEl
B2008$ccode[B2008$abbreviation == "HANN"] <- Hannover
B2008$ccode[B2008$abbreviation == "CHI"] <- Channel
B2008$ccode[B2008$abbreviation == "OFS"] <- OrangeFreeState
B2008$ccode[B2008$abbreviation == "TUS"] <- tuscany
B2008$ccode[B2008$abbreviation == "PMA"] <- parma
B2008$ccode[B2008$abbreviation == "MOD"] <- modena
B2008$ccode[B2008$abbreviation == "SIC"] <- sicily
B2008$ccode[B2008$abbreviation == "SAR"] <- sardinia
B2008$ccode[B2008$abbreviation == "WRT"] <- wuttemberg
B2008$ccode[B2008$abbreviation == "SAX"] <- saxony
B2008$ccode[B2008$abbreviation == "BAD"] <- baden
B2008$ccode[B2008$abbreviation == "BAV"] <- bavaria
B2008$ccode[B2008$abbreviation == "UPC"] <- CentralAmericanUnion
#id column
B2008 <- cbind(B2008 = "B2008", B2008)


BK2012   <- BK2012_IO_RepData.dta
BK2012$ccode <- countrycode(BK2012$country, "country.name", "iso3n", warn = TRUE)
#id column
BK2012 <- cbind(BK2012 = "BK2012", BK2012)


BR2007   <- BR2007_IO_RepData.dta
BR2007$ccode <- countrycode(BR2007$country, "country.name", "iso3n", warn = TRUE)
#id column
BR2007 <- cbind(BR2007 = "BR2007", BR2007)


CP2010 <- read.delim("./raw_data/TSCS/CP2010_IO_RepData.tab")
CP2010$ccode <- countrycode(CP2010$country, "cowc", "iso3n", warn = FALSE)
for(i in 1:nrow(CP2010)){
  if(is.na(CP2010[i, "ccode"])){
    CP2010[[i, "ccode"]] <- countrycode(CP2010[i, "country"], "iso3c", "iso3n", warn = FALSE)
    
  }
}
CP2010$ccode[CP2010$country == "CAY"] <- 136 #Cayman islands
#still missing: CHK, CHM, IMY, KBI, .M, RUM, SER, TMP, VIU 
CP2010$ccode[CP2010$country == "CHK"] <- CHK
CP2010$ccode[CP2010$country == "CHM"] <- CHM
CP2010$ccode[CP2010$country == "IMY"] <- IMY
CP2010$ccode[CP2010$country == "KBI"] <- KBI
CP2010$ccode[CP2010$country == ".M"] <- .M
CP2010$ccode[CP2010$country == "RUM"] <- RUM
CP2010$ccode[CP2010$country == "SER"] <- SER
CP2010$ccode[CP2010$country == "TMP"] <- TMP
CP2010$ccode[CP2010$country == "VIU"] <- VIU
#id column
CP2010 <- cbind(CP2010 = "CP2010", CP2010)


CRA2012  <- CRA2012_IO_RepData.dta
#CRA2012$ccode <- countrycode(CRA2012$ccode, "", "country.name", warn = TRUE)
#Only a numeric ccode present. Don't know type...
#id column
CRA2012 <- cbind(CRA2012 = "CRA2012", CRA2012)


DG2012   <- DG2012_IO_RepData.dta
DG2012$ccode <- countrycode(DG2012$wdi, "iso3c", "iso3n", warn = TRUE)
DG2012$ccode[DG2012$wdi == "ZAR"] <- 180 #Dem. rep. Congo
DG2012$ccode[DG2012$wdi == "YUH"] <- 890 #yugslavia
DG2012$ccode[DG2012$wdi == "ROM"] <- 642 #romania
#id column
DG2012 <- cbind(DG2012 = "DG2012", DG2012)


E2007    <- E2007_IO_RepData.dta
E2007$ccode <- countrycode(E2007$ctryn, "country.name", "iso3n", warn = TRUE)
E2007$ccode[E2007$ctryn == "NZ"] <- 554 #New Zealand
#id column
E2007 <- cbind(E2007 = "E2007", E2007)


GS2010   <- GS2010_IO_RepData.dta 
GS2010$ccode <- countrycode(GS2010$wbname, "country.name", "iso3n", warn = TRUE)
GS2010$ccode[GS2010$wbname == "Yugoslavia"] <- 890
GS2010$ccode[GS2010$wbname == "Channel Islands"] <- 830
GS2010$ccode[GS2010$wbname == "Montenegro (and Serbia)"] <- 891
#id column
GS2010 <- cbind(GS2010 = "GS2010", GS2010)


HHB2010  <- HHB2010_IO_RepData.dta
HHB2010$ccode <- countrycode(HHB2010$country, "country.name", "iso3n", warn = TRUE)
HHB2010$ccode[HHB2010$country == "YUGOSLAVIA"] <- 890
HHB2010$ccode[HHB2010$country == "YEMEN ARAB REP."] <- 886
HHB2010$ccode[HHB2010$country == "GERMAN DEM. REP."] <- 278
HHB2010$ccode[HHB2010$country == "VIETNAM, S."] <- 714
HHB2010$ccode[HHB2010$country == "N. KOREA"] <- 408
#id column
HHB2010 <- cbind(HHB2010 = "HHB2010", HHB2010)


K2007    <- K2007_IO_RepData.dta  
K2007$ccode <- countrycode(K2007$ifs, "iso3c", "iso3n", warn = TRUE)
K2007$ccode[K2007$ifs == "ROM"] <- 642 #romania
K2007$ccode[K2007$ifs == "YUG"] <- 890 #yugoslavia
K2007$ccode[K2007$ifs == "ZAR"] <- 180 #Dem. rep. Congo
K2007$ccode[K2007$ifs == "ADO"] <- 20  #Andorra
#id column
K2007 <- cbind(K2007 = "K2007", K2007)


KB2008   <- KB2008_WP_RepData.dta 
KB2008$ccode <- countrycode(KB2008$ifs, "iso3c", "iso3n", warn = TRUE)
#id column
KB2008 <- cbind(KB2008 = "KB2008", KB2008)


KR2008   <- KR2008_IO_RepData.dta 
KR2008$ccode <- countrycode(KR2008$stnam1, "country.name", "iso3n", warn = TRUE)
KR2008$ccode[KR2008$stnam1 == "Yemen Arab Republic"] <- 886
KR2008$ccode[KR2008$stnam1 == "Yugoslavia"] <- 890
KR2008$ccode[KR2008$stnam1 == "European Union"] <- EU
KR2008$ccode[KR2008$stnam1 == "Sae Tome & Principe"] <- 678
#id column
KR2008 <- cbind(KR2008 = "KR2008", KR2008)


M2009    <- M2009_IO_RepData.dta  
M2009$ccode <- countrycode(M2009$countryname, "country.name", "iso3n", warn = TRUE)
M2009$ccode[M2009$countryname == "Serbia and Montenegro"] <- 891
#id column
M2009 <- cbind(M2009 = "M2009", M2009)


O2011    <- O2011_IO_RepData.dta  
O2011$ccode <- countrycode(O2011$country, "imf", "iso3n", warn = TRUE)
colnames(O2011)[3] <- "year"
O2011$ccode[O2011$country == "188"] <- 890 #yugoslavia
O2011$ccode[O2011$country == "473"] <- 886 #Yemen
O2011$ccode[O2011$country == "965"] <- 891 #SerbiaandMontenegro
#id column
O2011 <- cbind(O2011 = "O2011", O2011)


R2008    <- R2008_WP_RepData.dta  
R2008$ccode <- countrycode(R2008$country, "iso3c", "iso3n", warn = TRUE)
R2008$ccode[R2008$country == "DEN"] <- 208 #denmark
R2008$ccode[R2008$country == "GER"] <- 276 #Germany
R2008$ccode[R2008$country == "NTH"] <- 528 #Netherlands
R2008$ccode[R2008$country == "NTH"] <- 528 #Netherlands
#missing code: ASL
R2008$ccode[R2008$country == "ASL"] <- ASL #no idea
#id column
R2008 <- cbind(R2008 = "R2008", R2008)


R2011    <- R2011_IO_RepData.dta  
R2011$ccode <- countrycode(R2011$country_name, "country.name", "iso3n", warn = TRUE)
#id column
R2011 <- cbind(R2011 = "R2011", R2011)


W2010    <- W2010_WP_RepData.dta  
W2010$ccode <- countrycode(W2010$NAMES_STD, "country.name", "iso3n", warn = TRUE)
W2010$ccode[W2010$NAMES_STD == "Yugoslavia"] <- 890
#id column
W2010 <- cbind(W2010 = "W2010", W2010)


#dyad year data structure - not sure how to merge.
#AF2008 <- read.delim("AF2008_WP_RepData.tab")
#BDMS2009 <- BDMS2009_IO_RepData.dta

#Not quite country year data. Not sure how this is formatted.
#GKO2009  <- GKO2009_IO_RepData.dta
#GKO2009$ccode1 <- countrycode(GKO2009$cname, "country.name", "iso3n", warn = TRUE)
#WED2011  <- WED2011_WP_RepData.dta
#WED2011$ccode <- countrycode(WED2011$country, "country.name", "iso3n", warn = TRUE)


#getting variable information from stata files
A2008labs   <- cbind("A2008",  attr(A2008_IO_RepData.dta,  "var.labels"), names(A2008_IO_RepData.dta))
AS2012labs  <- cbind("AS2012", attr(AS2012_IO_RepData.dta, "var.labels"), names(AS2012_IO_RepData.dta))
AP2011labs  <- cbind("AP2011", attr(AP2011_IO_RepData.dta, "var.labels"), names(AP2011_IO_RepData.dta))
B2008labs   <- cbind("B2008",  attr(B2008_WP_RepData.dta,  "var.labels"), names(B2008_WP_RepData.dta))
BK2012labs  <- cbind("BK2012", attr(BK2012_IO_RepData.dta, "var.labels"), names(BK2012_IO_RepData.dta))
BR2007labs  <- cbind("BR2007", attr(BR2007_IO_RepData.dta, "var.labels"), names(BR2007_IO_RepData.dta))
CP2010labs  <- cbind("CP2010", NA, names(CP2010))
DG2012labs  <- cbind("DG2012", attr(DG2012_IO_RepData.dta, "var.labels"), names(DG2012_IO_RepData.dta))
E2007labs   <- cbind("E2007",  attr(E2007_IO_RepData.dta,  "var.labels"), names(E2007_IO_RepData.dta))
GS2010labs  <- cbind("GS2010", attr(GS2010_IO_RepData.dta, "var.labels"), names(GS2010_IO_RepData.dta))
HHB2010labs <- cbind("HHB2010", attr(HHB2010_IO_RepData.dta, "var.labels"), names(HHB2010_IO_RepData.dta))
K2007labs   <- cbind("K2007",  attr(K2007_IO_RepData.dta,  "var.labels"), names(K2007_IO_RepData.dta))
KB2008labs  <- cbind("KB2008", attr(KB2008_WP_RepData.dta, "var.labels"), names(KB2008_WP_RepData.dta))
KR2008labs  <- cbind("KR2008", attr(KR2008_IO_RepData.dta, "var.labels"), names(KR2008_IO_RepData.dta))
M2009labs   <- cbind("M2009",  attr(M2009_IO_RepData.dta,  "var.labels"), names(M2009_IO_RepData.dta))
O2011labs   <- cbind("O2011",  attr(O2011_IO_RepData.dta,  "var.labels"), names(O2011_IO_RepData.dta))
R2008labs   <- cbind("R2008",  attr(R2008_WP_RepData.dta,  "var.labels"), names(R2008_WP_RepData.dta))
R2011labs   <- cbind("R2011",  attr(R2011_IO_RepData.dta,  "var.labels"), names(R2011_IO_RepData.dta))
W2010labs   <- cbind("W2010",  attr(W2010_WP_RepData.dta,  "var.labels"), names(W2010_WP_RepData.dta))

Varinfo <- as.data.frame(rbind(A2008labs,AS2012labs, AP2011labs, B2008labs, BK2012labs, BR2007labs, CP2010labs, 
                 DG2012labs, E2007labs, GS2010labs, HHB2010labs, K2007labs, KB2008labs, KR2008labs, 
                 M2009labs, O2011labs, R2008labs, R2011labs, W2010labs))
#WriteXLS(Varinfo, ExcelFileName = "VariablesDescriptions.xls")

#clearing original files to save RAM
rm(A2008_IO_RepData.dta, AP2011_IO_RepData.dta, AS2012_IO_RepData.dta, 
  B2008_WP_RepData.dta , BDMS2009_IO_RepData.dta, BK2012_IO_RepData.dta, BR2007_IO_RepData.dta, 
  CRA2012_IO_RepData.dta, DG2012_IO_RepData.dta, E2007_IO_RepData.dta , 
  GKO2009_IO_RepData.dta, GS2010_IO_RepData.dta, HHB2010_IO_RepData.dta, K2007_IO_RepData.dta, 
  KB2008_WP_RepData.dta, KR2008_IO_RepData.dta, M2009_IO_RepData.dta, O2011_IO_RepData.dta, 
  R2008_WP_RepData.dta, R2011_IO_RepData.dta, W2010_WP_RepData.dta, WED2011_WP_RepData.dta)


#second dataset: Vdem, Polity, Haber-Mayer, World Bank
Vdem <- read.dta13("./raw_data/V-Dem_STATA_v6.2/V-Dem-DS-CY-v6.2.dta")
Vdem$ccode <- countrycode(Vdem$country_name, "country.name", "iso3n", warn = TRUE)
Vdem$ccode[Vdem$country_text_id == "VNM"] <- 704 #Vietnam
Vdem$ccode[Vdem$country_text_id == "VDR"] <- 714 #South Vietnam
Vdem$ccode[Vdem$country_text_id == "XKX"] <- Kosovo
Vdem <- cbind(Vdem = "Vdem", Vdem)

HaberMenaldo <- read_excel("./raw_data/Haber_Menaldo_2011.xls", sheet = 2)
HaberMenaldo$ccode <- countrycode(HaberMenaldo$cnamehabmen, "country.name", "iso3n", warn = TRUE)
HaberMenaldo$ccode[HaberMenaldo$cnamehabmen == "Yugoslavia"] <- 890
HaberMenaldo$ccode[HaberMenaldo$cnamehabmen == "Pakisan"] <- 586
HaberMenaldo <- HaberMenaldo[!(is.na(HaberMenaldo$cnamehabmen)),]
HaberMenaldo <- cbind(HaberMenaldo = "HaberMenaldo", HaberMenaldo)

PolityIV <- read_excel("./raw_data/Polity IV Project.xls")
PolityIV$ccode <- countrycode(PolityIV$country, "country.name", "iso3n", warn = TRUE)
PolityIV$ccode[PolityIV$scode == "YUG"] <- 890 #Yugoslavia
PolityIV$ccode[PolityIV$scode == "YGS"] <- 890 #Yugoslavia
PolityIV$ccode[PolityIV$country == "Serbia and Montenegro"] <- 891 #Serbia and montenegro
PolityIV$ccode[PolityIV$scode == "YAR"] <- 886 #North Yemen
PolityIV$ccode[PolityIV$scode == "WRT"] <- wuttemberg
PolityIV$ccode[PolityIV$scode == "TUS"] <- tuscany
PolityIV$ccode[PolityIV$scode == "SIC"] <- sicily 
PolityIV$ccode[PolityIV$scode == "SAX"] <- saxony
PolityIV$ccode[PolityIV$scode == "SAR"] <- sardinia 
PolityIV$ccode[PolityIV$scode == "PMA"] <- parma
PolityIV$ccode[PolityIV$scode == "OFS"] <- OrangeFreeState
PolityIV$ccode[PolityIV$scode == "MOD"] <- modena
PolityIV$ccode[PolityIV$scode == "KOS"] <- Kosovo
PolityIV$ccode[PolityIV$scode == "BAV"] <- bavaria
PolityIV$ccode[PolityIV$scode == "BAD"] <- baden
PolityIV$ccode[PolityIV$scode == "UPC"] <- UnitedProvinceCA
PolityIV$ccode[PolityIV$scode == "GMY"] <- Prussia
PolityIV <- cbind(PolityIV = "PolityIV", PolityIV)

#WorldBank


#use to replace above full_joins with more concise method.
ToMerge <- list(A2008, AS2012, AP2011, B2008, BK2012, BR2007, CP2010, DG2012, 
                E2007,  GS2010, HHB2010, K2007,  KB2008, KR2008, M2009,  O2011,  
                R2008,  R2011,  W2010, Vdem, HaberMenaldo, PolityIV)
#merge datasets
#need to use reduce and 
MergeFromList <- function(List){
  for(i in 2:length(ToMerge)){
    List[[1]] <- full_join(List[[1]], List[[i]], by = c("ccode", "year"), all.x = TRUE, all.y = TRUE)
  }
  return(List[[1]])
}
MIDataset <- MergeFromList(ToMerge)

#giving MIDataset a complete list country name column.
MIdataset$country.x <- countrycode(MIdataset$ccode, "iso3n", "country.name", warn = TRUE)
MIdataset$country.x[MIdataset$ccode == "890"] <- "Yugoslavia"
MIdataset$country.x[MIdataset$ccode == EU] <- "European Union"
MIdataset$country.x[MIdataset$ccode == "830"] <- "Channel Islands"
MIdataset$country.x[MIdataset$ccode == "886"] <- "Yemen Arab Republic"
MIdataset$country.x[MIdataset$ccode == "891"] <- "Serbia and Montenegro"

#find the years each dataset covers
Year.Range <- function(df){
  year.max <- max(df$year, na.rm = TRUE)
  year.min <- min(df$year, na.rm = TRUE)
  c(year.min, year.max)
}

YearsCovered <- lapply(ToMerge, Year.Range)

#creating datasets to use for correlation matrices.
CorDataset <- MIdataset[, sapply(MIdataset, class) != c("factor")]
CorDataset <- CorDataset[, sapply(CorDataset, class) != c("character")]

Dummies <- list()
for(i in 1:length(CorDataset)){
  if(length(unique(na.omit(CorDataset[,i]))) == 2){
    Dummies <- c(Dummies, colnames(CorDataset[i]))
  }
}
#change 10 to whatever we decide to use as determinant of categorical
Categoricals <- list()
for(i in 1:length(CorDataset)){
  if(length(unique(na.omit(CorDataset[,i]))) > 2 & length(unique(na.omit(CorDataset[,i]))) < 10){
    Categoricals <- c(Categoricals, colnames(CorDataset[i]))
  }
}
DummyCorData <- CorDataset[, (names(CorDataset) %in% Dummies)]
CategoricalCorData <- CorDataset[, (names(CorDataset) %in% Categoricals)]
CorDataset <- CorDataset[ , !(names(CorDataset) %in% Dummies)]

#Creating the matrices
CorMatrix <- cor(CorDataset, use = "pairwise.complete.obs")
CorMatrix <- as.data.frame(CorMatrix)
CorMatrixBackup <- CorMatrix

CorMatrix[lower.tri(CorMatrix,diag=TRUE)] <- NA  #Prepare to drop duplicates and meaningless information
CorList <- as.data.frame(as.table(as.matrix(CorMatrix)))  #Turn into a 3-column table
CorList <- na.omit(CorList)  #Get rid of redundant dyads
CorList <- CorList[abs(CorList$Freq) > .95,] #choose level of correlation to keep

#create correlation matrix for categorical data. Using Kendall method.
#CategoricalCorMatrix <- cor(CategoricalCorData, method = "kendall", use = "pairwise.complete.obs")
#CategoricalCorMatrix <- as.data.frame(CategoricalCorMatrix)
#CategoricalCorMatrixBackup <- CategoricalCorMatrix
#
#CategoricalCorMatrix[lower.tri(CategoricalCorMatrix,diag=TRUE)] <- NA  #Prepare to drop duplicates and meaningless information
#CategoricalCorList <- as.data.frame(as.table(as.matrix(CategoricalCorMatrix)))  #Turn into a 3-column table
#CategoricalCorList <- na.omit(CategoricalCorList)  #Get rid of redundant dyads
#CategoricalCorList <- CategoricalCorList[abs(CategoricalCorList$Freq) > .95,]

##create correlation matrix for dummy data - this is using pearson method
##I currently have the understand that it will return the phi coefficient.
#DummyCorMatrix <- cor(DummyCorData,use = "pairwise.complete.obs")
#DummyCorMatrix <- as.data.frame(DummyCorMatrix)
#DummyCorMatrixBackup <- DummyCorMatrix
#
#DummyCorMatrix[lower.tri(DummyCorMatrix,diag=TRUE)] <- NA  #Prepare to drop duplicates and meaningless information
#DummyCorList <- as.data.frame(as.table(as.matrix(DummyCorMatrix)))  #Turn into a 3-column table
#DummyCorList <- na.omit(DummyCorList)  #Get rid of redundant dyads
#DummyCorList <- DummyCorList[abs(DummyCorList$Freq) > .95,]