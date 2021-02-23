d1<-read.csv("data6089.csv")
d2<-read.csv("data9013.csv")
# duomenu failu stulpeliu pavadinimai identiskai sutampa
data<-merge(d1,d2, by=colnames(d1), all=T)