installed.packages("rvest")
library(rvest)
library(dplyr)

#web scraping for given task by using selectorGadget extension

link= "https://www.sahibinden.com/ilan/emlak-konut-satilik-sisli-gulbahar-mahallesinde-site-icerisinde-1-plus1-kiracili-daire-1067776846/detay"
page = read_html(link)
page
data<-page %>% html_nodes("li:nth-child(4) span , li:nth-child(4) strong") %>% html_text()
data

col<-page %>% html_nodes("li:nth-child(17) strong , li:nth-child(10) strong , li:nth-child(9) strong , li:nth-child(8) strong , li:nth-child(7) strong , li:nth-child(5) strong , li:nth-child(4) strong") %>% html_text()
col
row<-page %>% html_nodes("li:nth-child(17) span , li:nth-child(10) span , li:nth-child(9) span , #classifiedDetail li:nth-child(8) span , #classifiedDetail li:nth-child(7) span , #classifiedDetail li:nth-child(5) span , .classifiedInfoList li:nth-child(4) span") %>% html_text()
row[4:10]
row
row[4]<-gsub("\n                \t","",row[4])
row[5]<-gsub("\n                \t","",row[5])
row[6]<-gsub("\n                \t","",row[6])
row[7]<-gsub("\n                \t","",row[7])
row[8]<-gsub("\n                \t","",row[8])
row[9]<-gsub("\n                \t","",row[9])
row[10]<-gsub("\n                \t","",row[10])

row<-row[4:10]
df<-rbind(col,row)
colnames(df)<-col
df<-df[-1,]
df
View(df)

####
