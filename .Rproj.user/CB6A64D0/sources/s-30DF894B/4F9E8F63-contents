#Paketti datan editoimiseen ja visualisointiin
library(tidyverse)

#Paketti datan lukemiseen
library(jsonlite)

#Luetaan aineisto sisään
koronafrek<- fromJSON("https://sampo.thl.fi/pivot/prod/fi/epirapo/covid19case/fact_epirapo_covid19case.json?row=dateweek2020010120201231-443686&row=hcdmunicipality2020-445222&row=ttr10yage-444309&row=sex-444328")

#puretaan kategoriat paloiksi
label_age <- as.data.frame(unlist(koronafrek$dataset$dimension$ttr10yage$category$label))
label_week <- as.data.frame(unlist(koronafrek$dataset$dimension$dateweek2020010120201231$category$label))
label_sex <- as.data.frame(unlist(koronafrek$dataset$dimension$sex$category$label))
label_municip <- as.data.frame(unlist(koronafrek$dataset$dimension$hcdmunicipality2020$category$label))

index_age <- as.data.frame(unlist(koronafrek$dataset$dimension$ttr10yage$category$index))
index_week<-as.data.frame(unlist(koronafrek$dataset$dimension$dateweek2020010120201231$category$index))
index_sex<-as.data.frame(unlist(koronafrek$dataset$dimension$sex$category$index))
index_municip<-as.data.frame(unlist(koronafrek$dataset$dimension$hcdmunicipality2020$category$index))
#Nimetään palaset
names(label_age)<-"label_age"
names(label_week)<-"label_week"
names(label_sex)<-"label_sex"
names(label_municip)<-"label_municip"

names(index_age)<-"index_age"
names(index_week)<-"index_week"
names(index_sex)<-"index_sex"
names(index_municip)<-"index_municip"


#Laitetana vielä rivinumerot sarakkeiksi, jotta nämä saadaan yhteen.
label_age<-rownames_to_column(label_age)
label_week<-rownames_to_column(label_week)
label_sex<-rownames_to_column(label_sex)
label_municip<-rownames_to_column(label_municip)

index_age<-rownames_to_column(index_age)
index_week<-rownames_to_column(index_week)
index_sex<-rownames_to_column(index_sex)
index_municip<-rownames_to_column(index_municip)


#Yhdistetään rivinimeä käyttäen
kategoriat_age <- index_age %>% left_join(label_age,by="rowname")
kategoriat_week <- index_week %>% left_join(label_week,by="rowname")
kategoriat_sex <- index_sex %>% left_join(label_sex,by="rowname")
kategoriat_municip <- index_municip %>% left_join(label_municip,by="rowname")

#otetaan data
age_vals <- (unlist(fromJSON("https://sampo.thl.fi/pivot/prod/fi/epirapo/covid19case/fact_epirapo_covid19case.json?column=ttr10yage-444309")$dataset$value))
week_vals <- (unlist(fromJSON("https://sampo.thl.fi/pivot/prod/fi/epirapo/covid19case/fact_epirapo_covid19case.json?column=dateweek2020010120201231-443686")$dataset$value))
sex_vals <- (unlist(fromJSON("https://sampo.thl.fi/pivot/prod/fi/epirapo/covid19case/fact_epirapo_covid19case.json?column=sex-444328")$dataset$value))
municip_vals <- (unlist(fromJSON("https://sampo.thl.fi/pivot/prod/fi/epirapo/covid19case/fact_epirapo_covid19case.json?column=hcdmunicipality2020-445222")$dataset$value))

#vikalla rivillä aiempien sarakesummat (ns. kaikki)
age_vals<-rownames_to_column(as.data.frame(age_vals))
week_vals<-rownames_to_column(as.data.frame(week_vals))
sex_vals<-rownames_to_column(as.data.frame(sex_vals))
municip_vals<-rownames_to_column(as.data.frame(municip_vals))

age_vals$rowname<-as.numeric(age_vals$rowname)
week_vals$rowname<-as.numeric(week_vals$rowname)
sex_vals$rowname<-as.numeric(sex_vals$rowname)
municip_vals$rowname<-as.numeric(municip_vals$rowname)

data <- as.data.frame(unlist(aineisto$dataset$value))

#Nimetään
names(data)<-"tapaus_lkm"
data<-rownames_to_column(data)
data$rowname<-as.numeric(data$rowname)

#names(data1)<-"tapaus_lkm"
#data1<-rownames_to_column(data1)
#data1$rowname<-as.numeric(data1$rowname)


#Yhdistetään muuhun aineistoon
dataset <- kategoriat_age[-10,] %>% left_join(age_vals,by=c("index_age"="rowname"))
dataset1 <- kategoriat_week[-c(1:4,35:54),] %>% left_join(week_vals,by=c("index_week"="rowname"))
dataset2 <- kategoriat_sex[-3,] %>% left_join(sex_vals,by=c("index_sex"="rowname"))
dataset3 <- kategoriat_municip[-22,] %>% left_join(municip_vals,by=c("index_municip"="rowname"))
dataset <- kategoriat[,-4] %>% left_join(data,by=c("index"="rowname"))

plot(dataset1$index_week,as.numeric(as.character(dataset1$week_vals)),col="navy",main="Weekly cases",type="l")

visual_c19<-function(covid_cat){
  if(covid_cat==0){
    plot(dataset[,2],as.numeric(as.character(dataset[,4])),main="Cases by age",type="l",col="navy",xaxt="na",xlab="Age group",ylab="Case #")
    axis(side=1,at=dataset[,2],labels = as.character(dataset[,3]),las=1)
  }
  if(covid_cat==1){
    plot(dataset1[,2],as.numeric(as.character(dataset1[,4])),main="Weekly cases",type="l",col="navy")
  }
  if(covid_cat==2){
    plot(dataset1[,2],as.numeric(as.character(dataset2[,4])),main="Weekly cases",type="l",col="navy")
  }
  if(covid_cat==3){
    plot(dataset1[,2],as.numeric(as.character(dataset3[,4])),main="Weekly cases",type="l",col="navy")
  }
}

visual_c19(0)
