setwd("C:/Users/gabij/Desktop/Mokslai/4SEMESTRAS/Programavimas_viz/LAB1")

        ## 1 uzduotis

# a) DUOMENU NUSKAITYMAS + BIBLIOTEKU ISSAUKIMAS

library(dplyr)
library(lubridate)
library(haven)
library(tidyr)

propinfo<-read.csv("Real_Property_Information.csv")

# b) DUOMENU STRUKTURA

dim(propinfo)
str(propinfo[,30:40]) 
# parinkau tik 10 stulpeliu, kuriuose yra ivairaus tipo duomenu, kad nebutu spausdinami 80
# nes tai butu labai daug, atsizvelgiant i tai, kad yra stulpeliu, kuriuose yra ilgi link'ai
# duomenys turi laukeliu, kurie nera NA, taciau duomenyse yra "  " 
# yra tarpai su skirtingais ilgiais

# c) DUOMENU VALYMAS

# tarpu panaikinimas
charcols <- names(propinfo)[sapply(propinfo, is.character)] #randa char stulpelius
propinfo[,charcols] <- sapply(propinfo[,charcols], trimws) #naikina tarpus

# tusciu reiksmiu ("") vertimas i NA
propinfo[propinfo==""]<-NA

# nuliu vertimas i NA is skaiciaus tipo kintamuju
propinfo[propinfo==0]<-NA

str(propinfo[,30:40]) 


# del failo aprasymo trukumo, kategorinius kintamuosius parinkau savo nuoziura
# kategoriniu kintamuju priskyrimas
factors<- c("PERMHOME", "ASSESGRP", 
            "NO_IMPRV", "EXMPTYPE", "USEGROUP", "DIST_ID",
            "DISTSWCH", "AR_OWNER", "OWNER_ABBR",
            "STDIRPRE", "ST_TYPE", "SRVCCNTR",
            "SUBTYPE_GEODB", "VACIND", "RESPAGCY")

# kintamajame NO_IMPRV yra tik "Y" (yes) ir NA, tad NA paverciamas i kintamaji N (no)
propinfo[,"NO_IMPRV"][is.na(propinfo[,"NO_IMPRV"])]<-"N"

propinfo[, factors] <- lapply(propinfo[, factors], as.factor)

# char kintamuju priskyrimas
characters<- c("OBJECTID", "ZIP_CODE", "EXTD_ZIP")
propinfo[,characters]<-lapply(propinfo[,characters], as.character)

# datu tipu priskyrimas
propinfo$SALEDATE<-mdy(propinfo$SALEDATE)
#kazkodel lubridate nenuskaito datos "1172021", tad paverciau i "11-7-2021"
propinfo$LDATE<-sub("(.{2})(.*)","\\1-\\2", propinfo$LDATE)
propinfo$LDATE<-sub("(.{4})(.*)","\\1-\\2", propinfo$LDATE)
propinfo$LDATE<-dmy(propinfo$LDATE) 

# d) KINTAMUJU TIPAI

sapply(propinfo, class)

# e) KATEGORINIU KINT DAZNIU LENTELES

sapply(propinfo[,factors], table)

# f) SKAICIAUS TIPO CHARAKTERISTIKOS

numcols <- names(propinfo)[sapply(propinfo, is.numeric)]
# pasalinamas year kintamasis is numcols, kad neskaiciuotu jam vidurkio, max, mix
numcols <- numcols[-(numcols=="YEAR_BUILD")]

Mean<-sapply(propinfo[,numcols], mean, na.rm = T)
Max_val<-sapply(propinfo[,numcols], max, na.rm = T)
Min_val<-sapply(propinfo[,numcols], min, na.rm = T)

info_numeric<-cbind(Mean, Max_val, Min_val)
info_numeric<-apply(info_numeric, 2, round, digits=2)
print(info_numeric)

        #2 uzduotis

# a) KINTAMUJU SU TRUKSTAMOMIS REIKSMEMIS ISRASYMAS

nasum<-colSums(is.na(propinfo))
nacols<-names(propinfo)[nasum>0]
print(nacols)

# b) REIKSMIU SU >50 PROC NA PASALINIMAS
# I) stulpeliai
propinfo1 <- propinfo

miss <- c()
for(i in 1:ncol(propinfo1)) {
        if(length(which(is.na(propinfo1[,i]))) > 0.5*nrow(propinfo1)) miss <- append(miss,i) 
}
propinfo1 <- propinfo1[,-miss]

# II) eilutes
miss2 <- c()
for(i in 1:nrow(propinfo1)) {
        if(length(which(is.na(propinfo1[i,]))) > 0.5*ncol(propinfo1)) miss2 <- append(miss2,i)
}
# atlikus komanda prie miss2 metama reiksme yra NULL, t.y. nera eiluciu su >50% NA.

nasum1<-colSums(is.na(propinfo1))
nacols1<-names(propinfo1)[nasum1>0]

setdiff(nacols,nacols1) #stulpeliai, kurie buvo pasalinti

# c) KEITIMAS I VIDURKI PAGAL KATEGORINI KINTAMAJI
propinfo2<-propinfo1
numcols2 <- names(propinfo2)[sapply(propinfo2, is.numeric)]
# pasalinamas year kintamasis is numcols
numcols2 <- numcols2[-(numcols2=="YEAR_BUILD")]

for (a in numcols2)
{
        for (i in 1:dim(propinfo2)[1])
        {
                if (is.na(propinfo2[i, a]))
                {
                        propinfo2[i, a]<-mean(propinfo2[,a][propinfo2$USEGROUP==propinfo2$USEGROUP[i]], na.rm=T)
                }
        }
}
        
        #3 UZDUOTIS

propinfo3<- propinfo2 %>% mutate(day=day(SALEDATE), month=month(SALEDATE), year=year(SALEDATE))
head(propinfo3[,c("OBJECTID","SALEDATE", "day", "month", "year")], n=10)

        #4 UZDUOTIS

propinfo4<-propinfo3

# a) GRUBIU ISSKIRCIU RADIMAS

par(mfrow=c(2,2))
for (i in numcols2[1:4])
{
        boxplot(propinfo4[,i], xlab=i)
}
par(mfrow=c())

# b) GRUBIU ISSKIRCIU SALINIMAS
remove_outliers <- function(duom, x, na.rm = TRUE, ...) 
{
        qnt <- quantile(duom[,x], probs=c(.25, .75), na.rm = na.rm, ...)
        H <- 3 * IQR(duom[,x], na.rm = na.rm)
        y <- duom[,x]
        y[duom[,x] < (qnt[1] - H)] <- NA
        y[duom[,x] > (qnt[2] + H)] <- NA
        y
}

for (a in numcols2)
{
        propinfo4[,a]<-remove_outliers(propinfo4, a)
}

# palyginimui
colSums(is.na(propinfo3[,numcols2]))
colSums(is.na(propinfo4[,numcols2]))

        #5 UZDUOTIS

# Duomenu failas neatitinka tvarkingo (tidy) duomenu failo apibudinimo.
# Pazeistas kriterijus: stulpelio pavadinimai turi papildomos informacijos apie kintamaji.
# Pavyzdys: Stulpeliai STATETAX ir CITY_TAX, turintys papildoma informacija city ir state.

        #6 UZDUOTIS
# select funkcija parinkta tik todel, kad nespausdintu 53 stulpeliu (labai netvarkingai atrodo)
# realus ats butu gaunami pasalinus sia funkcija is visu uzduoties punktu
# a) top 10 city tax objektai
propinfo4 %>% top_n(n=10, CITY_TAX) %>% select(OBJECTID, CITY_TAX)

# b) deedpage yra daugiau uz vidutine verte pagal owner_1 irasus abeceles nuo galo tvarka;
propinfo4 %>% filter(DEEDPAGE > mean(DEEDPAGE, na.rm=T)) %>%
        arrange(desc(OWNER_1)) %>%
        select(DEEDPAGE,OWNER_1) %>%
        head(n=10)

# c) objektai, kuriu lot_size pavadinime yra simbolis X, o statetax virsija vidurki
propinfo4 %>%
        filter(grepl("X",LOT_SIZE) & STATETAX>mean(STATETAX, na.rm=T)) %>%
        select(LOT_SIZE, STATETAX) %>% 
        head(n=10)

# d) objektai, kuriu st_name kode simbolis C yra pirmas arba paskutinis, duomenis, be
# visu kintamuju, kuriu pavadinime yra simboliai curr
propinfo4 %>%
        filter(grepl("^C",ST_NAME)|grepl(".*C$",ST_NAME)) %>%
        select(-contains("curr",ignore.case=T)) %>% select(OBJECTID, ST_NAME) %>%
        head(10)

# e) atskiri duomenu rinkiniai pagal kintamojo st_type kategorijas
for (lev in levels(propinfo4$ST_TYPE))
{
       print(propinfo4 %>% select(OBJECTID, ST_TYPE) %>% filter(ST_TYPE==lev) %>% head(n=2))
}
        #7 UZDUOTIS

d1<-read_sas("internationalflights.sas7bdat")
d2<-read_sas("marchflights.sas7bdat")
d3<-read_sas("flightdelays.sas7bdat")

flights<-merge(d1,d2, by=c("Date", "FlightNumber"), all=T)
flights2<-merge(flights,d3, by=c("Date", "FlightNumber"), all=T)
flights3<- flights2 %>% select(FlightNumber, Freight, Distance, DelayCategory)

head(flights3, n=10)
                