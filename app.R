
#loading dependencies
library(shiny)
library(dplyr)
library(shinythemes)
library(plotly)
library(shinydashboard)
library(ggplot2)
library(scales)
library(zoo)
library(raster)
library(sp)
library(RColorBrewer)
library(leaflet)
library(DT)
library(lubridate)
library(shinyjs)
library(reshape2)
library(leaflet.minicharts)

holidays <- read.csv("holiday.csv")
holidays <- holidays[,c(2,4)]
class(holidays$Date)
holidays$Date <- as.Date(as.character(holidays$Date),"%Y-%m-%d")
holidays$Holiday <- as.character(holidays$Holiday)

monthweeks <- function(x) {
  UseMethod("monthweeks")
}
monthweeks.Date <- function(x) {
  ceiling(as.numeric(format(x, "%d")) / 7)
}
monthweeks.POSIXlt <- function(x) {
  ceiling(as.numeric(format(x, "%d")) / 7)
}
monthweeks.character <- function(x) {
  ceiling(as.numeric(format(as.Date(x), "%d")) / 7)
}

x<-seq(as.Date("2016-01-01"), by = "day", length.out = 365)
class(x)
holiday<-NULL

holiday$Date <- x
holiday<- as.data.frame(holiday)
colnames(holiday)<-c("Date")

holiday$wkd <- weekdays(as.Date(holiday$Date,"%Y-%m-%d"))
holiday$mon<- month.abb[month(as.Date(holiday$Date,"%Y-%m-%d"))]
holiday$qtr <- quarters(as.Date(holiday$Date,"%Y-%m-%d"))
holiday$wkomon <- monthweeks(as.Date(holiday$Date,"%Y-%m-%d"))
holiday$wkoyr <- week(as.Date(holiday$Date,"%Y-%m-%d"))

hol<-merge(holiday,holidays,by="Date",all=T)
#write.csv(hol,"Holiday_2018.csv",row.names = FALSE)

hol$Holiday[is.na(hol$Holiday)] <- "No Events"
#f<-read.csv("C:/Users/User/Downloads/forcasted_data.csv",header= T,stringsAsFactors = FALSE)
ABP<- read.csv("WB_ALL_Data_final.csv",header= T,stringsAsFactors = FALSE)
district<-read.csv("district.csv",header=T)
#removing 2016
ABP<-ABP[ABP$year!=2016,]

ABP<-ABP[!ABP$Indent==0,]
ABP <- ABP[!(ABP$IndentSold<0),]
ABP$sold_indent<-ABP$TotalSold-ABP$Indent
ABP$CalendarDay<-as.Date((ABP$CalendarDay),"%Y-%m-%d")
ABP$customer_id_name <- paste0(ABP$CustomerDescription, "_", ABP$Customer)
ABP$weekday <-weekdays(ABP$CalendarDay)
#

Dis<-c("All Districts",as.character(sort(unique(ABP$District),decreasing = FALSE)))
Dis1<-c(as.character(sort(unique(ABP$District),decreasing = FALSE)))
Year<-sort(unique(ABP$year),decreasing = FALSE)
Cust<-sort(unique(ABP$customer_id_name),decreasing = FALSE)
Var<-as.factor(c("AdhocUnsold","Adhocboost","Wastage", "AdhocSold", "Indent","Sold","Adhoc","Unsold"))
Var1<-as.factor(c("AdhocUnsold","Adhocboost","Wastage", "AdhocSold","Sold","Adhoc","Unsold"))


Abp3<-ABP[,c("CalendarDay","Quarter","Week","Month","year","Unsold","District","AdhocSold","AdhocUnsold","TotalSold","Indent","Adhoc","sold_indent","IndenUnsold","IndentSold")]

abp_daily <- Abp3 %>% group_by(CalendarDay,District,year,Quarter,Month,Week) %>% summarise(sold_indent= sum(sold_indent),Indent=sum(Indent),Adhoc=sum(Adhoc),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(TotalSold),IndenUnsold=sum(IndenUnsold),IndentSold=sum(IndentSold))
abp_daily$Adhocboost <- round(1000*(abp_daily$sold_indent/abp_daily$Indent),0)
abp_daily$Wastage <- 1000*(round(((abp_daily$AdhocUnsold+abp_daily$IndenUnsold)/(abp_daily$AdhocSold+abp_daily$IndentSold)),digits=2))
abp_daily[,-c(1:6)] <- round(abp_daily[,-c(1:6)],0)
weeks <- unique(abp_daily$Week)

#week
abp_daily$Week <- formatC(abp_daily$Week,width=2,flag="0")
abp_daily$year_wk<-paste0(substr(abp_daily$year,3,4),"-","Wk",abp_daily$Week)
abp_daily$wk <- paste0("Wk-",abp_daily$Week)

#month
abp_daily$monthly <-abp_daily$Month
abp_daily$month <-abp_daily$Month
abp_daily$Month <- formatC(abp_daily$Month,width=2,flag="0")
abp_daily$mon <-month.abb[abp_daily$month]
abp_daily$year_mon<-paste0(substr(abp_daily$year,3,4),"-",abp_daily$Month)

#quarter
abp_daily$year_qtr<-paste0(substr(abp_daily$year,3,4),"-","Q",abp_daily$Quarter)
abp_daily$qtr <- paste0("Q",abp_daily$Quarter)

#year
abp_daily$year_yr <- paste0("Year-",abp_daily$year)


#abp_daily[,-c(1:6)] <- format(abp_daily[,-c(1:6)],big.mark = ",",trim = TRUE)
dly_daily <- abp_daily %>% group_by(CalendarDay) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold),IndenUnsold=sum(IndenUnsold),IndentSold=sum(IndentSold))
dly_daily[,-1] <- round(dly_daily[,-1],0)
dly_daily_map <- abp_daily %>% group_by(CalendarDay) %>% summarise(Adhocboost= sum(Adhocboost),Wastage=sum(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold),IndenUnsold=sum(IndenUnsold),IndentSold=sum(IndentSold))
dly_daily_map[,-1] <- round(dly_daily_map[,-1],0)

#Year

abp_yr <- abp_daily %>% group_by(year_yr,year,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
abp_yr[,-c(1,2,3)] <- round(abp_yr[,-c(1,2,3)],0)

abp_yr$year_yr<-as.character(abp_yr$year_yr)
abp_yr$year<-as.character(abp_yr$year)
yr_year <- abp_yr %>% group_by(year_yr,year) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
yr_year[,-c(1,2)] <- round(yr_year[,-c(1,2)],0)
yr_year_map <- abp_yr %>% group_by(year_yr,year) %>% summarise(Adhocboost= sum(Adhocboost),Wastage=sum(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
yr_year_map[,-c(1,2)] <- round(yr_year_map[,-c(1,2)],0)
year1<-sort(unique(abp_yr$year),decreasing = FALSE)


#quarter

abp_qtr <- abp_daily %>% group_by(year_qtr,year,qtr,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))

abp_qtr[,-c(1,2,3,4)] <- round(abp_qtr[,-c(1,2,3,4)],0)
abp_qtr$year_qtr<-as.character(abp_qtr$year_qtr)
abp_qtr$year<-as.character(abp_qtr$year)
abp_qtr$qtr<-as.character(abp_qtr$qtr)

qtr_quarter <- abp_qtr %>% group_by(year_qtr,year,qtr) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
qtr_quarter[,-c(1,2,3)] <- round(qtr_quarter[,-c(1,2,3)],0)
qtr_quarter_map <- abp_qtr %>% group_by(year_qtr,year,qtr) %>% summarise(Adhocboost= sum(Adhocboost),Wastage=sum(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
qtr_quarter_map[,-c(1,2,3)] <- round(qtr_quarter_map[,-c(1,2,3)],0)
qtr1<-sort(unique(abp_qtr$qtr),decreasing = FALSE)

#Month
# abp_daily$monthly <-abp_daily$Month
# abp_daily$month <-abp_daily$Month
# abp_daily$Month <- formatC(abp_daily$Month,width=2,flag="0")
# abp_daily$mon <-month.abb[abp_daily$month]
#
# abp_daily$year_mon<-paste0(substr(abp_daily$year,3,4),"-",abp_daily$Month)
abp_mon <- abp_daily %>% group_by(year_mon,year,mon,monthly,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
abp_mon[,-c(1,2,3,4,5)] <- round(abp_mon[,-c(1,2,3,4,5)],0)
mon_month <- abp_mon %>% group_by(year_mon,year,mon,monthly) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
mon_month[,-c(1,2,3,4)] <- round(mon_month[,-c(1,2,3,4)],0)
mon_month_map <- abp_mon %>% group_by(year_mon,year,mon,monthly) %>% summarise(Adhocboost= sum(Adhocboost),Wastage=sum(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
mon_month_map[,-c(1,2,3,4)] <- round(mon_month_map[,-c(1,2,3,4)],0)

mon1<-unique(abp_mon$mon)

#week
abp_wk <- abp_daily %>% group_by(year_wk,year,wk,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
abp_wk[,-c(1,2,3,4)] <- round(abp_wk[,-c(1,2,3,4)],0)
abp_wk$year_wk<-as.character(abp_wk$year_wk)
abp_wk$wk<-as.character(abp_wk$wk)

wk_week <- abp_wk %>% group_by(year_wk,year,wk) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
wk_week[,-c(1,2,3)] <- round(wk_week[,-c(1,2,3)],0)
wk_week_map <- abp_wk %>% group_by(year_wk,year,wk) %>% summarise(Adhocboost= sum(Adhocboost),Wastage=sum(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
wk_week_map[,-c(1,2,3)] <- round(wk_week_map[,-c(1,2,3)],0)

wk1<-sort(unique(abp_wk$wk),decreasing = FALSE)

#Customer_analysis
abp_group <-aggregate(Indent~customer_id_name,ABP,mean)
abp_group <- abp_group[order(abp_group$Indent, decreasing = TRUE),]
abp_group$cum_ind <- cumsum(abp_group$Indent)
abp_group$cum_per <- ((abp_group$cum_ind)/sum(abp_group$Indent))*100
c <- cut(abp_group$cum_per,breaks=c(0,20,40,60,80,100),dig.lab=2,include.lowest=T)
#summary(c)
levels(c)<-c("Very Large","Large","Medium","Small","Very Small")
abp_group$group<-as.factor(c)
abp_group$Indent <- NULL
abp_total <- merge(ABP,abp_group,by="customer_id_name")

#Group Summary
abp_group_summ <-aggregate(Indent~customer_id_name,ABP,mean)
abp_group_summ <- abp_group_summ[order(abp_group_summ$Indent, decreasing = TRUE),]
abp_group_summ$cum_ind <- cumsum(abp_group_summ$Indent)
abp_group_summ$cum_per <- ((abp_group_summ$cum_ind)/sum(abp_group_summ$Indent))*100
c <- cut(abp_group_summ$cum_per,breaks=c(0,20,40,60,80,100),dig.lab=2,include.lowest=T)
#summary(c)
levels(c)<-c("Very Large","Large","Medium","Small","Very Small")
abp_group_summ$group<-as.factor(c)

#District Summary
dist_df <- abp_total[,c(1,22)]
dist_df <- dist_df[!duplicated(dist_df),]
abp_dist_df <- merge(abp_group_summ,dist_df,by="customer_id_name")

##
monthweeks <- function(x) {
  UseMethod("monthweeks")
}
monthweeks.Date <- function(x) {
  ceiling(as.numeric(format(x, "%d")) / 7)
}
monthweeks.POSIXlt <- function(x) {
  ceiling(as.numeric(format(x, "%d")) / 7)
}
monthweeks.character <- function(x) {
  ceiling(as.numeric(format(as.Date(x), "%d")) / 7)
}

d <- as.Date("2015-12-31","%Y-%m-%d")
d1<-as.Date("2015-12-31","%Y-%m-%d")-27

t<- as.Date("2015-12-31","%Y-%m-%d")+1
wkd_t <- weekdays(t)
mon_t <- month.abb[month(t)]
qtr_t <- quarters(t)
wkomon_t <- monthweeks(t)
wkoyr_t <- week(t)

ABP_prkd <- ABP[ABP$CalendarDay>=d1 & ABP$weekday==wkd_t,]
ABP_today <- ABP[ABP$CalendarDay==d,]

x1<-unique(ABP_today$customer_id_name)
ABP_prwkd<-NULL
for(i in x1){
  ABP_prwkd1 <- ABP_prkd[ABP_prkd$customer_id_name==i,]
  ABP_prwkd <- rbind(ABP_prwkd,ABP_prwkd1)
}


Dis2<-c(as.character(sort(unique(ABP_today$District),decreasing = FALSE)))
cust1<-sort(unique(ABP_today$customer_id_name),decreasing = FALSE)

#Data Preparation for Summary in the Wastage Pareto
adun_not_0 <- ABP[(ABP$AdhocUnsold!=0),]
adun_group <-aggregate(AdhocUnsold~customer_id_name,adun_not_0,mean)
adun_group[,-c(1)] <- round(adun_group[,-c(1)],0)
adun_dat <- adun_group[order(adun_group$AdhocUnsold, decreasing=TRUE),]
adun_dat$customer_id_name <- factor(adun_dat$customer_id_name, levels=adun_dat$customer_id_name)
adun_dat$cum <- cumsum(adun_dat$AdhocUnsold)
count.sum<-sum(adun_dat$AdhocUnsold)
adun_dat$cum_perc<-100*adun_dat$cum/count.sum
cust_group <- abp_group[,c(1,4)]
aduns_group <- merge(cust_group,adun_dat, by="customer_id_name")

#Data Preparation for Customer Glance in the Customer Tab
abp_group <-aggregate(Indent~customer_id_name,ABP,mean)
abp_group <- abp_group[order(abp_group$Indent, decreasing = TRUE),]
abp_group$cum_ind <- cumsum(abp_group$Indent)
abp_group$cum_per <- ((abp_group$cum_ind)/sum(abp_group$Indent))*100
c <- cut(abp_group$cum_per,breaks=c(0,20,40,60,80,100),dig.lab=2,include.lowest=T)
#summary(c)
levels(c)<-c("Very Large","Large","Medium","Small","Very Small")
abp_group$group<-as.factor(c)
summ_df_wb <- abp_group %>% group_by(group) %>% summarise(Members= length(unique(customer_id_name)), Minimum = min(Indent), Maximum = max(Indent), Average = mean(Indent), Median = median(Indent))
summ_df_wb[,-c(1,2)] <- round(summ_df_wb[,-c(1,2)],0)
summ_df_wb$Range <- paste0(summ_df_wb$Minimum,"-",summ_df_wb$Maximum)
summ_df_wb[,c(3,4)] <- NULL
group_range <- summ_df_wb[,c("group", "Range")]
cust_summ <- merge(abp_group,group_range, by = "group", all=TRUE)

#Data Preparation for Customer Distribution on the map
abp_group <-aggregate(Indent~customer_id_name,ABP,mean)
abp_group <- abp_group[order(abp_group$Indent, decreasing = TRUE),]
abp_group$cum_ind <- cumsum(abp_group$Indent)
abp_group$cum_per <- ((abp_group$cum_ind)/sum(abp_group$Indent))*100
c <- cut(abp_group$cum_per,breaks=c(0,20,40,60,80,100),dig.lab=2,include.lowest=T)
#summary(c)
levels(c)<-c("Very Large","Large","Medium","Small","Very Small")
abp_group$group<-as.factor(c)
abp_group$Indent <- NULL
abp_total <- merge(ABP,abp_group,by="customer_id_name")
India<-readRDS("C:/Adak/Projects/04. OTHERS/R/version1.6/GADM_2.8_IND_adm2.rds")
wb <- subset(India, NAME_1 == "West Bengal")
map <- fortify(wb);
map$id <- as.integer(map$id);
wb_dist<-wb$NAME_2
wb_dist<-as.data.frame(wb_dist)
colnames(wb_dist)<-"District"
wb1<-as.data.frame(wb)
wb1<-wb1[,c("ID_2","NAME_2")]
colnames(wb1)<-c("id","District")
wb1$lat<-district$Latitude
wb1$lng<-district$Longitude
abp_total_1 <- abp_total[,c("District","customer_id_name","Customer","CustomerDescription","group")]
cust_distribution<-as.data.frame(table(abp_total_1$District,abp_total_1$group))
colnames(cust_distribution)<- c("District","group","freq")
cust_distribution<-dcast(cust_distribution, formula = District ~ group,value.var="freq")
cust_distribution<-merge(cust_distribution,wb1,by="District",all=T)
cust_distribution$total<- cust_distribution$`Very Large`+cust_distribution$Large+cust_distribution$Medium+cust_distribution$Small+cust_distribution$`Very Small`
cust_distribution$`Very Large`<-round(100*cust_distribution$`Very Large`/cust_distribution$total,1)
cust_distribution$Large<-round(100*cust_distribution$Large/cust_distribution$total,1)
cust_distribution$Medium<-round(100*cust_distribution$Medium/cust_distribution$total,1)
cust_distribution$Small<-round(100*cust_distribution$Small/cust_distribution$total,1)
cust_distribution$`Very Small`<-round(100*cust_distribution$`Very Small`/cust_distribution$total,1)
colnames(cust_distribution)<-c("District","VeryLarge(%)","Large(%)","Medium(%)","Small(%)","VerySmall(%)","id","lat","lng","Total")

#Forecasting Model
# abp_casea <- ABP[(ABP$Adhoc>0 & ABP$AdhocUnsold>0),]
# abp_casea$monday <- ifelse(abp_casea$weekday=="Monday", 1,0)
# abp_casea$tuesday <- ifelse(abp_casea$weekday=="Tuesday", 1,0)
# abp_casea$wednesday <- ifelse(abp_casea$weekday=="Wednesday", 1,0)
# abp_casea$thursday <- ifelse(abp_casea$weekday=="Thursday", 1,0)
# abp_casea$saturday <- ifelse(abp_casea$weekday=="Saturday", 1,0)
# abp_casea$sunday <- ifelse(abp_casea$weekday=="Sunday", 1,0)
# abp_casea$indent_sq <- (abp_casea$Indent)^2
# reg_mod <- lm(abp_casea$Adhoc~abp_casea$Indent+abp_casea$indent_sq+abp_casea$monday+abp_casea$tuesday+abp_casea$wednesday+abp_casea$thursday+abp_casea$saturday+abp_casea$sunday)
# summ <- summary(reg_mod)
# coeff <- data.frame(summ$coefficients)
# est <- coeff$Estimate
# min_loss_1 <- function(par) {
#   a <- par[1]
#   b1 <- par[2]
#   b2 <- par[3]
#   b3 <- par[4]
#   b4 <- par[5]
#   b5 <- par[6]
#   b6 <- par[7]
#   b7 <- par[8]
#   b8 <- par[9]
#   abp_casea$adhoc_pred <- a+b1*abp_casea$Indent+b2*abp_casea$indent_sq+b3*abp_casea$monday+b4*abp_casea$tuesday+b5*abp_casea$wednesday+b6*abp_casea$thursday+b7*abp_casea$saturday+b8*abp_casea$sunday
#   abp_casea$indicator <- ifelse(abp_casea$Adhoc <= abp_casea$adhoc_pred,1,0)
#   abp_casea$loss <- (abs(abp_casea$Adhoc-abp_casea$adhoc_pred))*5*abp_casea$indicator+(abs(abp_casea$Adhoc-abp_casea$adhoc_pred))*2.5*(1-abp_casea$indicator)
#   sum(abp_casea$loss)
# }
# loss_res_new <- hjk(par=c(est[1],est[2],est[3],est[4],est[5],est[6],est[7],est[8],est[9]),min_loss_1, control=list(info=TRUE))
# para <- loss_res_new$par

test_data <- read.csv("test1.csv")
test_data$customer_id_name <- paste0(test_data$CustomerDescription, "_", test_data$Customer)
test_data$Calendarday <- as.Date(as.character(test_data$Calendarday),"%Y%m%d")
test_data <- test_data[(test_data$customer_id_name != 'P KESHRI_101114'),]
test_data$District <- as.character(test_data$District)
test_data$District[test_data$District=="Purulia"] <- "Puruliya"
test_data$District[test_data$District=="Bardhaman"] <- "Barddhaman"
test_data$District[test_data$District=="Hooghly"] <- "Hugli"
test_data$District[test_data$District=="Malda"] <- "Maldah"
test_data$District[test_data$District=="Howrah"] <- "Haora"
test_data$District[test_data$District=="West Midnapur"] <- "Pashchim Medinipur"
test_data$District[test_data$District=="Cooch Behar"] <- "Koch Bihar"
test_data$District[test_data$District=="East Midnapur"] <- "Purba Medinipur"
test_data$District[test_data$District=="Darjeeling"] <- "Darjiling"
test_req <- test_data
test_req_a <- test_req[(test_req$Adhoc>0 & test_req$AdhocUnsold>0),]
# test_req_a$District <- as.character(test_req_a$District)
# test_req_a$District[test_req_a$District=="Purulia"] <- "Puruliya"
# test_req_a$District[test_req_a$District=="Bardhaman"] <- "Barddhaman"
# test_req_a$District[test_req_a$District=="Hooghly"] <- "Hugli"
# test_req_a$District[test_req_a$District=="Malda"] <- "Maldah"
# test_req_a$District[test_req_a$District=="Howrah"] <- "Haora"
# test_req_a$District[test_req_a$District=="West Midnapur"] <- "Pashchim Medinipur"
# test_req_a$District[test_req_a$District=="Cooch Behar"] <- "Koch Bihar"
# test_req_a$District[test_req_a$District=="East Midnapur"] <- "Purba Medinipur"
# test_req_a$District[test_req_a$District=="Darjeeling"] <- "Darjiling"
test_req_a$monday <- ifelse(test_req_a$weekday=="Monday", 1,0)
test_req_a$tuesday <- ifelse(test_req_a$weekday=="Tuesday", 1,0)
test_req_a$wednesday <- ifelse(test_req_a$weekday=="Wednesday", 1,0)
test_req_a$thursday <- ifelse(test_req_a$weekday=="Thursday", 1,0)
test_req_a$saturday <- ifelse(test_req_a$weekday=="Saturday", 1,0)
test_req_a$sunday <- ifelse(test_req_a$weekday=="Sunday", 1,0)
test_req_a$indent_sq <- (test_req_a$Indent)^2
test_req_a$adhoc_pred <- -0.68411590808+0.03918308609*test_req_a$Indent+(-0.00000325155)*test_req_a$indent_sq+0.11701335542*test_req_a$monday+0.06461980427*test_req_a$tuesday+(-0.02243625776)*test_req_a$wednesday+0.10578674927*test_req_a$thursday+0.66076173573*test_req_a$saturday+0.79832309946*test_req_a$sunday
test_req_a$indicator <- ifelse(test_req_a$Adhoc <= test_req_a$adhoc_pred,1,0)
test_req_a$loss <- (abs(test_req_a$Adhoc-test_req_a$adhoc_pred))*5*test_req_a$indicator+(abs(test_req_a$Adhoc-test_req_a$adhoc_pred))*2.5*(1-test_req_a$indicator)
test_req_a$adhoc_pred <- round(test_req_a$adhoc_pred)
# test_req_a$customer_id_name <- paste0(test_req_a$CustomerDescription, "_", test_req_a$Customer)
# test_req_a$Calendarday <- as.Date(as.character(test_req_a$Calendarday),"%Y%m%d")
district1 <- as.character(unique(test_req_a$District))
customer_id <- as.character(unique(test_req_a$customer_id_name))

#Forecast Loss date
test_date_s <- test_req_a %>% group_by(Calendarday) %>% summarise(unsold=sum(AdhocUnsold),loss=sum(loss))
test_date_s$Unsold_loss <-test_date_s$unsold*5


#Forecast Loss week
test_week_s <- test_req_a %>% group_by(year,Week) %>% summarise(unsold=sum(AdhocUnsold),loss=sum(loss))
test_week_s$Unsold_loss <-test_week_s$unsold*5

#Forecast Loss
test_month_s <- test_req_a %>% group_by(year,Month) %>% summarise(unsold=sum(AdhocUnsold),loss=sum(loss))
test_month_s$Unsold_loss <-test_month_s$unsold*5


##intution result
test_month <- subset(test_data,Month == 1|Month==2|Month==3)
month_result <- test_month %>% group_by(Month) %>% summarise(Indent=sum(Indentsold),TotalSold=sum(Sold),AdhocUnsold=sum(AdhocUnsold))
month_result$month <- month.abb[month_result$Month]
month_result$AdhocSold <- month_result$TotalSold-month_result$Indent
month_result$Profit <- month_result$AdhocSold*2.5
month_result$Loss <- month_result$AdhocUnsold*5
month_result$Month <- NULL
month_result <- data.frame(month_result[,c(4,1:3,5:7)])
month_result[,-1] <-format(month_result[,-1],big.mark=",",trim=TRUE)
colnames(month_result) <- c("Month","Indent(in pcs)","Total Sold(in pcs)","AdhocUnsold(in pcs)","AdhocSold(in pcs)","Profit from AdhocSold(in Rs.)","Loss from AdhocUnsold(in Rs.)")
month_result$Month <-paste0(month_result$Month,"-","2016")

###-------------------------------------------------
test1<- read.csv("test1.csv")
test1$customer_id_name <- paste0(test1$CustomerDescription, "_", test1$Customer)
test1$Calendarday <- as.Date(as.character(test1$Calendarday),"%Y%m%d")
# test1$Calendarday <- as.Date(as.character(test1$Calendarday),"%Y%m%d")
day_result <- test1 %>% group_by(Calendarday) %>% summarise(Indent=sum(Indentsold),TotalSold=sum(Sold),AdhocUnsold=sum(AdhocUnsold))
day_result$AdhocSold <- day_result$TotalSold-day_result$Indent
day_result$Profit <- day_result$AdhocSold*2.5
day_result$Loss <- day_result$AdhocUnsold*5
colnames(day_result) <- c("CalendarDay", "Indent", "TotalSold", "AdhocUnsold", "AdhocSold","Profit", "Loss")
day_result$CalendarDay <- as.Date(as.character(day_result$CalendarDay),"%Y-%m-%d")
day_all <- ABP %>% group_by(CalendarDay) %>% summarise(Indent=sum(IndentSold),TotalSold=sum(TotalSold),AdhocUnsold=sum(AdhocUnsold))
day_all$AdhocSold <- day_all$TotalSold-day_all$Indent
day_all$Profit <- day_all$AdhocSold*2.5
day_all$Loss <- day_all$AdhocUnsold*5
day_all$CalendarDay <- as.Date(as.character(day_all$CalendarDay),"%Y-%m-%d")
day_final <- rbind(day_all,day_result)
day_final$Indent <- as.numeric(day_final$Indent)
day_final$Indent <- format(day_final$Indent, big.mark=",", trim=TRUE)
day_final$TotalSold <- as.numeric(day_final$TotalSold)
day_final$TotalSold <- format(day_final$TotalSold, big.mark=",", trim=TRUE)
day_final$AdhocUnsold <- as.numeric(day_final$AdhocUnsold)
day_final$AdhocUnsold <- format(day_final$AdhocUnsold, big.mark=",", trim=TRUE)
day_final$AdhocSold <- as.numeric(day_final$AdhocSold)
day_final$AdhocSold <- format(day_final$AdhocSold, big.mark=",", trim=TRUE)
day_final$Profit <- as.numeric(day_final$Profit)
day_final$Profit <- format(day_final$Profit, big.mark=",", trim=TRUE)
day_final$Loss <- as.numeric(day_final$Loss)
day_final$Loss <- format(day_final$Loss, big.mark=",", trim=TRUE)
colnames(day_final) <- c("CalendarDay","Indent (in pcs)","Total Sold (in pcs)","AdhocUnsold (in pcs)","AdhocSold (in pcs)","Profit from AdhocSold (in Rs.)","Loss from AdhocUnsold (in Rs.)")


#Weekwise View
test1$Week <- formatC(test1$Week, width = 2,flag="0")
test1$week <- paste0(test1$year,"-","Wk",test1$Week)
week_result <- test1 %>% group_by(week) %>% summarise(Indent=sum(Indentsold),TotalSold=sum(Sold),AdhocUnsold=sum(AdhocUnsold))
week_result$AdhocSold <- week_result$TotalSold-week_result$Indent
week_result$Profit <- week_result$AdhocSold*2.5
week_result$Loss <- week_result$AdhocUnsold*5
ABP$Weeks <- formatC(ABP$Week, width = 2,flag="0")
ABP$week <- paste0(ABP$year,"-","Wk",ABP$Weeks)
week_all <- ABP %>% group_by(week) %>% summarise(Indent=sum(IndentSold),TotalSold=sum(TotalSold),AdhocUnsold=sum(AdhocUnsold))
week_all$AdhocSold <- week_all$TotalSold-week_all$Indent
week_all$Profit <- week_all$AdhocSold*2.5
week_all$Loss <- week_all$AdhocUnsold*5
week_final <- rbind(week_all,week_result)
week_final$Indent <- as.numeric(week_final$Indent)
week_final$Indent <- format(week_final$Indent, big.mark=",", trim=TRUE)
week_final$TotalSold <- as.numeric(week_final$TotalSold)
week_final$TotalSold <- format(week_final$TotalSold, big.mark=",", trim=TRUE)
week_final$AdhocUnsold <- as.numeric(week_final$AdhocUnsold)
week_final$AdhocUnsold <- format(week_final$AdhocUnsold, big.mark=",", trim=TRUE)
week_final$AdhocSold <- as.numeric(week_final$AdhocSold)
week_final$AdhocSold <- format(week_final$AdhocSold, big.mark=",", trim=TRUE)
week_final$Profit <- as.numeric(week_final$Profit)
week_final$Profit <- format(week_final$Profit, big.mark=",", trim=TRUE)
week_final$Loss <- as.numeric(week_final$Loss)
week_final$Loss <- format(week_final$Loss, big.mark=",", trim=TRUE)
colnames(week_final) <- c("Week","Indent (in pcs)","Total Sold (in pcs)","AdhocUnsold (in pcs)","AdhocSold (in pcs)","Profit from AdhocSold (in Rs.)","Loss from AdhocUnsold (in Rs.)")

week_final$wk <- substr(week_final$Week,6,9)
week_final$year <- substr(week_final$Week,1,4)


#Monthwise result
month_result <- test1 %>% group_by(Month) %>% summarise(Indent=sum(Indentsold),TotalSold=sum(Sold),AdhocUnsold=sum(AdhocUnsold))
month_result$month <- month.abb[month_result$Month]
month_result$AdhocSold <- month_result$TotalSold-month_result$Indent
month_result$Profit <- month_result$AdhocSold*2.5
month_result$Loss <- month_result$AdhocUnsold*5
month_result$Month <- NULL
month_result <- data.frame(month_result[,c(4,1:3,5:7)])
# month_result[,-1] <-format(month_result[,-1],big.mark=",",trim=TRUE)
# colnames(month_result) <- c("Month","Indent(in pcs)","Total Sold(in pcs)","AdhocUnsold(in pcs)","AdhocSold(in pcs)","Profit from AdhocSold(in Rs.)","Loss from AdhocUnsold(in Rs.)")
month_result$month <-paste0(month_result$month,"-","2016")
# month_result$month <- NULL
# month_result <- data.frame(month_result[,c(7,2:6)])
ABP$month <- month.abb[ABP$Month]
ABP$Months <-paste0(ABP$month,"-",ABP$year)
month_all <- ABP %>% group_by(Months) %>% summarise(Indent=sum(IndentSold),TotalSold=sum(TotalSold),AdhocUnsold=sum(AdhocUnsold))
month_all$AdhocSold <- month_all$TotalSold-month_all$Indent
month_all$Profit <- month_all$AdhocSold*2.5
month_all$Loss <- month_all$AdhocUnsold*5
colnames(month_all) <- c("month","Indent","TotalSold","AdhocUnsold","AdhocSold","Profit","Loss")
month_final <- rbind(month_all,month_result)
month_final$Indent <- as.numeric(month_final$Indent)
month_final$Indent <- format(month_final$Indent, big.mark=",", trim=TRUE)
month_final$TotalSold <- as.numeric(month_final$TotalSold)
month_final$TotalSold <- format(month_final$TotalSold, big.mark=",", trim=TRUE)
month_final$AdhocUnsold <- as.numeric(month_final$AdhocUnsold)
month_final$AdhocUnsold <- format(month_final$AdhocUnsold, big.mark=",", trim=TRUE)
month_final$AdhocSold <- as.numeric(month_final$AdhocSold)
month_final$AdhocSold <- format(month_final$AdhocSold, big.mark=",", trim=TRUE)
month_final$Profit <- as.numeric(month_final$Profit)
month_final$Profit <- format(month_final$Profit, big.mark=",", trim=TRUE)
month_final$Loss <- as.numeric(month_final$Loss)
month_final$Loss <- format(month_final$Loss, big.mark=",", trim=TRUE)
colnames(month_final) <- c("Month","Indent(in pcs)","Total Sold(in pcs)","AdhocUnsold(in pcs)","AdhocSold(in pcs)","Profit from AdhocSold(in Rs.)","Loss from AdhocUnsold(in Rs.)")
month_final$year <- substr(month_final$Month,5,8)
month_final$mon <- substr(month_final$Month,1,3)
###-------------------------------------------------

#ShinyDashboard
header <- dashboardHeader(title = "Dashboard for XYZ Company", tags$li(class = "dropdown", uiOutput("logout")))
sidebar <- dashboardSidebar(uiOutput("sidebarpanel"))
body <- dashboardBody(useShinyjs(),tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "style1.css")
),uiOutput("body"))

#log-in ids
login_details <- data.frame(user = c("Mainak", "Soma", "Gautam","sss"),
                            pswd = c("Mainakpass", "Somapass", "Gautampass","sss"))
login <- box(
  title = "Login",
  textInput("userName", "Username"),
  passwordInput("passwd", "Password"),
  br(),
  actionButton("Login", "Log in")
  
)

##

#ui
ui <-dashboardPage(header, sidebar, body)
##


#server
server <- function(input, output,session) {
  
  
  login.page = paste(
    isolate(session$clientData$url_protocol),
    "//",
    isolate(session$clientData$url_hostname),
    ":",
    isolate(session$clientData$url_port),
    sep = ""
  )
  
  
  USER <- reactiveValues(Logged = F)
  observe({
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(login_details$user %in% Username)
          Id.password <- which(login_details$pswd %in% Password)
          if (length(Id.username) > 0 & length(Id.password) > 0){
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
            }
          }
        }
      }
    }
  })
  
  output$logout<-renderUI({
    if (USER$Logged == TRUE){
      tags$div(class = "submit",
               tags$a(id="logout2",href = login.page,
                      "Log Out"
               )
      )
    }
  })
  
  addClass(selector = "body", class = "sidebar-collapse")
  
  output$body <- renderUI({
    if (USER$Logged == TRUE) {  
      tabItems(
        # First tab content
        # First tab content
        tabItem(tabName = "Map",
                tabsetPanel(type = "tabs",selected="Daily View",fluidRow(column(4,dateRangeInput("dates",
                                                                                                 "Date range",
                                                                                                 start = "2010-01-01",
                                                                                                 end = "2015-12-31",min = "2010-01-01",max = "2015-12-31")),
                                                                         column(2,selectizeInput("Variable1","Select a Variable",choices = Var)),
                                                                         column(2,selectInput("indistrict1","Select a District",choices = Dis))
                                                                         
                ),
                #fluidRow(tags$hr(style="border-color: gray88;")),
                tabPanel("Daily View",fluidRow(
                  column(12,h5("Select a District from the dropdown to see the Time Period Analysis for the selected variable")
                  )),fluidRow(
                    column(4,actionButton("goButton", "Go!")
                    )),
                  
                  fluidRow(
                    column(12,h3(textOutput("map_heading")),
                           fluidRow(
                             column(6,leafletOutput("Map_cust")),
                             column(5,textOutput("mytable_head"),
                                    tableOutput("myTable"),textOutput("error_daily")),
                             
                             fluidRow(
                               column(12,h6("  **Data for the Districts of Alipurduar and Kolkata is not available. ")))
                           ))
                    
                    # column(3,h5(""),
                    #        fluidRow(
                    #          column(12,h5("Summary (Average, per day) for West Bengal - for the selected time period"))),
                    #        fluidRow(
                    #          column(12,tableOutput("map_summary")
                    #          )
                  ),
                  
                  #h6("**Data for the Districts of Alipurduar and Kolkata is not available "),
                  h4(textOutput("map_text")),
                  fluidRow(
                    column(8,h5(textOutput("map_definition")))),
                  h1("Copyright(c) 2018, Business Brio. All rights reserved")
                ),
                tabPanel("Weekly View",
                         #h2("Map of West Bengal"),
                         fluidRow(
                           column(12,h5("Select a District from the dropdown to see the Time Period Analysis for the selected variable"),
                                  
                                  fluidRow(
                                    column(4,actionButton("goButton1", "Go!")
                                    )))),
                         
                         
                         #selectizeInput("Variable1","Select a Variable",choices = Var),h1(""),
                         fluidRow(
                           column(12,h3(textOutput("map_heading1")),
                                  fluidRow(
                                    column(6,leafletOutput("Map_cust1")),
                                    column(5,textOutput("mytable1_head"),
                                           tableOutput("myTable1"),textOutput("error_daily1")),
                                    
                                    fluidRow(
                                      column(12,h6("  **Data for the Districts of Alipurduar and Kolkata is not available. ")))
                                  ))
                           
                           # column(3,h5(""),
                           #        fluidRow(
                           #          column(12,h5("Summary (Average, per day) for West Bengal - for the selected week range"))),
                           #        fluidRow(
                           #          column(12,tableOutput("map_summary1")
                           #          )
                         ),
                         
                         #h6("**Data for the Districts of Alipurduar and Kolkata is not available "),
                         h4(textOutput("map_text1")),
                         fluidRow(
                           column(8,h5(textOutput("map_definition1")))),
                         h1("Copyright(c) 2018, Business Brio. All rights reserved")
                ),
                tabPanel("Monthly View",
                         #h2("Map of West Bengal"),
                         fluidRow(
                           column(12,h5("Select a District from the dropdown to see the Time Period Analysis for the selected variable"),
                                  
                                  fluidRow(
                                    column(4,actionButton("goButton2", "Go!")
                                    )))),
                         
                         
                         #selectizeInput("Variable1","Select a Variable",choices = Var),h1(""),
                         fluidRow(
                           column(12,h3(textOutput("map_heading2")),
                                  fluidRow(
                                    column(6,leafletOutput("Map_cust2")),
                                    column(5,textOutput("mytable2_head"),
                                           tableOutput("myTable2"),textOutput("error_daily2")),
                                    
                                    fluidRow(
                                      column(12,h6("  **Data for the Districts of Alipurduar and Kolkata is not available. ")))
                                  ))
                           
                           # column(3,h5(""),
                           #        fluidRow(
                           #          column(12,h5("Summary (Average, per day) for West Bengal - for the selected month range"))),
                           #        fluidRow(
                           #          column(12,tableOutput("map_summary2")
                           #          )
                         ),
                         
                         #h6("**Data for the Districts of Alipurduar and Kolkata is not available "),
                         h4(textOutput("map_text2")),
                         fluidRow(
                           column(8,h5(textOutput("map_definition2")))),
                         h1("Copyright(c) 2018, Business Brio. All rights reserved")
                ),tabPanel("Quarterly View",
                           #h2("Map of West Bengal"),
                           fluidRow(
                             column(12,h5("Select a District from the dropdown to see the Time Period Analysis for the selected variable"),
                                    
                                    fluidRow(
                                      column(4,actionButton("goButton3", "Go!")
                                      )))),
                           
                           
                           #selectizeInput("Variable1","Select a Variable",choices = Var),h1(""),
                           fluidRow(
                             column(12,h3(textOutput("map_heading3")),
                                    fluidRow(
                                      column(6,leafletOutput("Map_cust3")),
                                      column(5,textOutput("mytable3_head"),
                                             tableOutput("myTable3"),textOutput("error_daily3")),
                                      
                                      fluidRow(
                                        column(12,h6("  **Data for the Districts of Alipurduar and Kolkata is not available. ")))
                                    ))
                             
                             # column(3,h5(""),
                             #        fluidRow(
                             #          column(12,h5("Summary (Average, per day) for West Bengal - for the selected quarter range"))),
                             #        fluidRow(
                             #          column(12,tableOutput("map_summary3")
                             #          )
                           ),
                           
                           #h6("**Data for the Districts of Alipurduar and Kolkata is not available "),
                           h4(textOutput("map_text3")),
                           fluidRow(
                             column(8,h5(textOutput("map_definition3")))),
                           h1("Copyright(c) 2018, Business Brio. All rights reserved")
                ),tabPanel("Yearly View",
                           #h2("Map of West Bengal"),
                           fluidRow(
                             column(12,h5("Select a District from the dropdown to see the Time Period Analysis for the selected variable"),
                                    
                                    fluidRow(
                                      column(4,actionButton("goButton4", "Go!")
                                      )))),
                           
                           
                           #selectizeInput("Variable1","Select a Variable",choices = Var),h1(""),
                           fluidRow(
                             column(12,h3(textOutput("map_heading4")),
                                    fluidRow(
                                      column(6,leafletOutput("Map_cust4")),
                                      column(5,textOutput("mytable4_head"),
                                             tableOutput("myTable4"),textOutput("error_daily4")),
                                      
                                      fluidRow(
                                        column(12,h6("  **Data for the Districts of Alipurduar and Kolkata is not available. ")))
                                    ))
                             
                             # column(3,h5(""),
                             #        fluidRow(
                             #          column(12,h5("Summary (Average, per day) for West Bengal - for the selected time period"))),
                             #        fluidRow(
                             #          column(12,tableOutput("map_summary4")
                             #          )
                           ),
                           
                           #h6("**Data for the Districts of Alipurduar and Kolkata is not available "),
                           h4(textOutput("map_text4")),
                           fluidRow(
                             column(8,h5(textOutput("map_definition4")))),
                           h1("Copyright(c) 2018, Business Brio. All rights reserved")
                ))
        ),
        tabItem(tabName = "Period",
                
                # tbsets
                tabsetPanel(type = "tabs",id="inTabset",
                            
                            tabPanel("Daily View - Selected Time Period",h2(textOutput("dly_htext")),
                                     
                                     tags$h5(""),
                                     
                                     plotlyOutput("plot_dly"),h5(textOutput("dly_text"),align="center"),
                                     h2(verbatimTextOutput("definition5"),align="Left",family="Old Standard TT,serif"),
                                     fluidRow(
                                       column(6,verbatimTextOutput("dly_summary_m1", placeholder = FALSE)),
                                       column(6,verbatimTextOutput("dly_summary_m"))),
                                     h1("Copyright(c) 2018, Business Brio. All rights reserved")
                                     
                            ),tabPanel("Weekly View - Selected Time Period",h2(textOutput("wkly_htext")),
                                       
                                       tags$h5(""),
                                       
                                       plotlyOutput("plot_wkly"),h5(textOutput("wkly_text"),align="center"),
                                       h5(verbatimTextOutput("definition11"),align="Left",family="Old Standard TT,serif"),
                                       fluidRow(
                                         column(6,verbatimTextOutput("wkly_summary_m1", placeholder = FALSE)),
                                         column(6,verbatimTextOutput("wkly_summary_m"))),
                                       h1("Copyright(c) 2018, Business Brio. All rights reserved")
                                       
                            ),
                            tabPanel("Monthly View - Selected Time Period",h2(textOutput("monly_htext")),
                                     
                                     tags$h5(""),
                                     
                                     plotlyOutput("plot_monly"),h5(textOutput("monly_text"),align="center"),
                                     h5(verbatimTextOutput("definition12"),align="Left",family="Old Standard TT,serif"),
                                     fluidRow(
                                       column(6,verbatimTextOutput("monly_summary_m1", placeholder = FALSE)),
                                       column(6,verbatimTextOutput("monly_summary_m"))),
                                     h1("Copyright(c) 2018, Business Brio. All rights reserved")
                                     
                            ),tabPanel("Quarterly View - Selected Time Period",h2(textOutput("qtrly_htext")),
                                       
                                       tags$h5(""),
                                       
                                       plotlyOutput("plot_qtrly"),h5(textOutput("qtrly_text"),align="center"),
                                       h5(verbatimTextOutput("definition13"),align="Left",family="Old Standard TT,serif"),
                                       fluidRow(
                                         column(6,verbatimTextOutput("qtrly_summary_m1", placeholder = FALSE)),
                                         column(6,verbatimTextOutput("qtrly_summary_m"))),
                                       h1("Copyright(c) 2018, Business Brio. All rights reserved")
                                       
                            ),
                            tabPanel("Daily",h2(textOutput("dly_htext1")),
                                     
                                     tags$h5(""),
                                     
                                     plotlyOutput("plot_dly1"),h5(textOutput("dly_text1"),align="center"),
                                     h2(verbatimTextOutput("definition6"),align="Left",family="Old Standard TT,serif"),
                                     fluidRow(
                                       column(6,verbatimTextOutput("dly_summary1", placeholder = FALSE)),
                                       column(6,verbatimTextOutput("dly_summary"))),
                                     h1("Copyright(c) 2018, Business Brio. All rights reserved")
                                     
                            ),
                            tabPanel("Week",h2(textOutput("wk_htext")),
                                     
                                     tags$h5(""),
                                     
                                     plotlyOutput("plot_wk"),h5(textOutput("wk_text"),align="center"),
                                     h2(verbatimTextOutput("definition4"),align="Left",family="Old Standard TT,serif"),
                                     fluidRow(
                                       column(6,verbatimTextOutput("wk_summary1")),
                                       column(6,verbatimTextOutput("wk_summary"))),
                                     h1("Copyright(c) 2018, Business Brio. All rights reserved")
                                     
                            ),
                            tabPanel("Month",h2(textOutput("mon_htext")),
                                     
                                     tags$h5(""),
                                     
                                     plotlyOutput("plot_mon"),h5(textOutput("mon_text"),align="center"),
                                     h2(verbatimTextOutput("definition3"),align="Left",family="Old Standard TT,serif"),
                                     fluidRow(
                                       column(6,verbatimTextOutput("mon_summary1")),
                                       column(6,verbatimTextOutput("mon_summary"))),
                                     h1("Copyright(c) 2018, Business Brio. All rights reserved")
                                     
                            ),
                            tabPanel("Quarter",h2(textOutput("qtr_htext")),
                                     
                                     tags$h5(""),
                                     
                                     plotlyOutput("plot_qtr"),h5(textOutput("qtr_text"),align="center"),
                                     h2(verbatimTextOutput("definition2"),align="Left",family="Old Standard TT,serif"),
                                     fluidRow(
                                       column(6,verbatimTextOutput("qtr_summary1")),
                                       column(6,verbatimTextOutput("qtr_summary"))),
                                     h1("Copyright(c) 2018, Business Brio. All rights reserved")
                                     
                            ),
                            
                            
                            tabPanel("Year",h2(textOutput("yr_htext")),
                                     
                                     tags$h5(""),
                                     
                                     plotlyOutput("plot_yr"),
                                     h5(textOutput("yr_text"),align="center"),
                                     h2(verbatimTextOutput("definition1"),align="Left",family="Old Standard TT,serif"),
                                     fluidRow(
                                       column(6,verbatimTextOutput("yr_summary1")),
                                       column(6,verbatimTextOutput("yr_summary"))),
                                     h1("Copyright(c) 2018, Business Brio. All rights reserved")
                                     
                            )
                            
                            
                )
                
        ),
        
        # Second tab content
        tabItem(tabName = "Customer",
                tabsetPanel(type = "tabs",selected="Indent Pareto",id="inTabset2",
                            tabPanel("Indent Pareto",
                                     
                                     
                                     fluidRow(
                                       column(7,
                                              h2("Customer Wise Average Indent (pcs per day) : Pareto"),
                                              plotlyOutput("pareto_Indent"),
                                              h5("The graph shows the average Indent values (in pcs, per day) for the all customers. This has been used to decide the groups of the customers.
                                                 The customers in the top 20% (cumulative percentage) form the Very Large group, which is followed by the next 20% forming the Large group.
                                                 Then we have the Medium group for the next 20 and this is followed by the Small and Very Small groups."),
                                              
                                              h3("Summary Statistics for the Groups"),
                                              tableOutput("group_summ_wb")),
                                       column(5,h2("Customer Distribution"),leafletOutput("cust_dist_map", width = "100%", height = 687))
                                       
                                       
                                       ),
                                     
                                     h1("Copyright(c) 2018, Business Brio. All rights reserved")
                                     ),
                            tabPanel("Wastage Pareto",
                                     h2("Customer Wise Average AdhocUnsold (pcs per day) : Pareto
                                        "),
                                     
                                     plotlyOutput("pareto_wastage"),
                                     h5("The graph shows the average wastage (in terms of adhocunsold), pieces per day for the customers. There are 4% customers contributing to top 20% of the Wastage."),
                                     fluidRow(
                                       column(6,h3("Top 20% of the Wastage Contributors' Distribution")),
                                       column(6,h3("Bottom 20% of the Wastage Contributors' Distribution"))
                                     ),fluidRow(
                                       column(6,tableOutput("very_large_waste")),
                                       column(6,tableOutput("very_small_waste"))
                                     ),fluidRow(
                                       column(6,h5("The table shows the groupwise customer distribution for the top 20% wastage contributors.
                                                   Here approximately 48% of the top 20% contributors belong to the Very Large group of customers while 9% of them belong to the Very Small group of customers")),
                                       column(6,h5("The table shows the groupwise customer distribution for the bottom 20% wastage contributors.
                                                   Here approximately 80% of the bottom 20% contributors belong to the Very Small group of customers while just 1% of them belong to the Large group of customers"))
                                       ),h1("Copyright(c) 2018, Business Brio. All rights reserved")
                                       ),
                            # tabPanel("Wastage",
                            #          fluidRow(
                            #            column(3,selectizeInput("year1","Select a Year",choices = c(2010,2011,2012,2013,2014,2015))),
                            #            column(3,selectizeInput("week1","Select a week",choices = c(1:54))),
                            #            column(3,uiOutput("wasplot"))
                            #          ),
                            #          h2("AdhocUnsold (per day)"),
                            #          fluidRow(
                            #            plotlyOutput("wkd_plot"),
                            #            p("The graph shows the AdhocUnsold (in pcs) and as a percentage of Adhoc sent for the selected customer on different days of the week.")
                            #          )
                            # ),
                            tabPanel("Customer",h2(textOutput("cust_hhtext")),
                                     fluidRow(
                                       column(4,dateRangeInput("dates1",
                                                               "Date range",
                                                               start = "2015-11-01",
                                                               end = "2015-12-31",min = "2010-01-01",max = "2015-12-31")),
                                       column(3,selectizeInput("indistrict2","Select a District",choices = Dis1)),
                                       column(3,uiOutput("cusdrop")),
                                       column(3,selectizeInput("Variable3","Select a Variable",choices = Var1))
                                     ),
                                     fluidRow(
                                       column(4,actionButton("backButton", "Back")
                                       )),
                                     fluidRow(
                                       column(8,fluidRow(column(12,h3(textOutput("cust_htext"),align="left")),
                                                         column(12,plotlyOutput("cust_plot1")),
                                                         column(12,h5(textOutput("cust_text"),align="left"))),
                                              fluidRow(column(12,h5(class = "fs-1","Customer: At a Glance")),
                                                       column(12,tableOutput("cust_glance")))),
                                       
                                       column(4,fluidRow(column(12,textOutput("cust_summary_htext")),
                                                         column(12,tableOutput("cust_summary"))))
                                     ),
                                     
                                     h4(textOutput("cust_definition")),
                                     h6("Note: The customer groups have been decided based on the Indent values of the customers.Five groups have been created where each group consists of members contributing 20% of the total Indent values.Thus, Group 1 gives the customers having contribution in the range of 0%-20% towards the Indent values")
                                     ,h1("Copyright(c) 2018, Business Brio. All rights reserved")
                                     
                            ))
        ),
        tabItem(tabName = "Forecast",
                tabsetPanel(type = "tabs",selected="Forecasting",id="inTabset3",
                            tabPanel("Forecasting",
                                     fluidPage(h2("Forecasting"),
                                               fluidRow(
                                                 column(3,dateInput("date1", "Date for Forecast:", value = "2016-01-02",min="2016-01-01",max="2016-05-31")),
                                                 column(3,selectizeInput("indistrict5","Select a District",choices = district1)),
                                                 column(3,uiOutput("cusforecast")),
                                                 column(2,uiOutput("allcustomer")),
                                                 column(1,uiOutput("cusdescription"))
                                               ),
                                               textOutput("date_text"),
                                               h2("Forcasting for :"),
                                               # column(8,
                                               #      
                                               #        valueBoxOutput("boxvalue", width = "200%") # value box interface to show the events for the day
                                               #      
                                               #      
                                               # ),
                                               fluidRow(
                                                 column(4,valueBoxOutput("date", width = "100%")),
                                                 column(4,valueBoxOutput("dayweek", width = "100%")),
                                                 column(4,valueBoxOutput("qtr_t", width = "100%")),
                                                 column(4,valueBoxOutput("wkomon_t", width = "100%")),
                                                 column(4,valueBoxOutput("wkoyr_t", width = "100%")),
                                                 column(4,valueBoxOutput("event", width = "100%"))
                                               ),
                                               
                                               fluidRow(
                                                 column(10,textOutput("forecast_htable1"),tableOutput("today"))
                                                 # column(6,textOutput("forecast_htable2"),tableOutput("tomorrow"))
                                                 
                                               ),
                                               
                                               # fluidRow(
                                               
                                               
                                               
                                               fluidRow(
                                                 column(12,h5("Note : The predictions for Adhoc have been made where Adhoc is more than 0 and also the AdhocUnsold is greater than zero. Thus, 'NA' implies that no prediction has been made for those customers. The predicted Adhoc for the customer (for the next day) shows the Adhoc that should be sent in addition to the Indent amount. The predicted Adhoc amount sent to the customer by the company can create an additional impact, in terms of the extra profit that can be earned. But, in cases where the entire amount of Adhoc sent will not be sold will be a loss for the company."))
                                               ),
                                               h1("Copyright(c) 2018, Business Brio. All rights reserved")
                                               
                                               #h6("**Data for the Districts of Alipurduar and Kolkata is not available "),
                                     )
                            )
                )
                
                
                
                
        ),
        
        tabItem(tabName = "BusinessImpact",
                tabsetPanel(type = "tabs",id="inTabset4",
                            
                            tabPanel("Daily",
                                     uiOutput("dates1"),
                                     fluidRow(column(4,
                                                     tableOutput("date_data_table")),
                                              column(8,p("The table shows the Indent sent and the extra sale that the company gets from the Adhoc sent. But as the Adhoc is sent (in addition to the Indent) there is also some,
                                                         unsold component which results in the loss to the company. The cost of each piece to the company is Rs. 5 and the profit earned by selling one piece is Rs. 2.5. Thus if the extra profit earned from sending the Adhoc is less than the loss incurred due to the
                                                         unsold pieces, then the company would be better off by not sending any Adhoc. "))
                                              ),
                                     fluidRow(
                                       h3("Daily: Customer Distribution for Adhoc and AdhocUnsold"),
                                       column(6,plotlyOutput("pie_sent_not_sent")),
                                       p("The graph shows the percentage of customers who do not get any Adhoc on a particular day as well as the percenatge of customers who sell off all the Adhoc sent to them alongwith the customers who cannot sell all the Adhoc sent to them and thus the Adhoc Unsold for them is greater than zero.")
                                       
                                     ),fluidRow(class="mt-30",
                                                column(4,selectizeInput("advar","Select a Variable",choices = c("AdhocSent","AdhocUnsoldEqual0","AdhocUnsoldGreater0")))
                                                
                                     ),
                                     h3(textOutput("chart_text")),
                                     
                                     fluidRow(
                                       column(12,plotlyOutput("trend_adhocSent"))
                                     ),
                                     p(textOutput("adun_text")),
                                     fluidRow(
                                       column(3,dateInput("date_intution","Select a Date", value = "2016-01-02",min="2016-01-01",max="2016-04-30"))
                                       
                                     ),
                                     tableOutput("intution_dates"),
                                     p("The table shows the actual loss from Adhoc Unsold and the predicted/forecasted loss for the particular day"),
                                     
                                     
                                     h1("Copyright(c) 2018, Business Brio. All rights reserved")
                                     
                                              ),
                            tabPanel("Weekly",
                                     uiOutput("year_s"),uiOutput("week_s"),
                                     fluidRow(column(4,
                                                     tableOutput("week_data_table")),
                                              column(8,p("The table shows the Indent sent and the extra sale that the company gets from the Adhoc sent. But as the Adhoc is sent (in addition to the Indent) there is also some,
                                                         unsold component which results in the loss to the company. The cost of each piece to the company is Rs. 5 and the profit earned by selling one piece is Rs. 2.5. Thus if the extra profit earned from sending the Adhoc is less than the loss incurred due to the
                                                         unsold pieces, then the company would be better off by not sending any Adhoc. "))
                                              ),
                                     fluidRow(
                                       h3("Weekly: Customer Distribution for Adhoc and AdhocUnsold"),
                                       column(6,plotlyOutput("pie_sent_not_sent2")),
                                       p("The graph shows the percentage of customers who do not get any Adhoc on a particular week as well as the percenatge of customers who sell off all the Adhoc sent to them alongwith the customers who cannot sell all the Adhoc sent to them and thus the Adhoc Unsold for them is greater than zero.")
                                       
                                     ),h2(""),
                                     fluidRow(
                                       column(3,selectizeInput("yr1_intution","Year",choices = c(2016))),
                                       column(3,selectizeInput("wk1_intution","Select a Week",choices =unique(test_week_s$Week))
                                              
                                       )),
                                     tableOutput("intution_weeks"),
                                     p("The table shows the actual loss from Adhoc Unsold and the predicted/forecasted loss for the particular Week"),
                                     h1("Copyright(c) 2018, Business Brio. All rights reserved")
                                     
                                              ),
                            tabPanel("Monthly",
                                     uiOutput("year_s1"),uiOutput("months"),
                                     fluidRow(column(4,tableOutput("month_data_table")),
                                              
                                              column(8,p("The table shows the Indent sent and the extra sale that the company gets from the Adhoc sent. But as the Adhoc is sent (in addition to the Indent) there is also some,
                                                         unsold component which results in the loss to the company. The cost of each piece to the company is Rs. 5 and the profit earned by selling one piece is Rs. 2.5. Thus if the extra profit earned from sending the Adhoc is less than the loss incurred due to the
                                                         unsold pieces, then the company would be better off by not sending any Adhoc. "))
                                              ),
                                     fluidRow(
                                       h3("Monthly: Customer Distribution for Adhoc and AdhocUnsold"),
                                       column(6,plotlyOutput("pie_sent_not_sent3")),
                                       p("The graph shows the percentage of customers who do not get any Adhoc on a particular month as well as the percenatge of customers who sell off all the Adhoc sent to them alongwith the customers who cannot sell all the Adhoc sent to them and thus the Adhoc Unsold for them is greater than zero.")
                                       
                                     ),h2(""),
                                     fluidRow(
                                       column(3,selectizeInput("yr2_intution","Year",choices = c(2016))),
                                       column(3,selectizeInput("mon1_intution","Select a Month",choices =c(month.abb[sort(unique(test_month_s$Month))]))
                                              
                                       )),
                                     tableOutput("intution_months"),
                                     p("The table shows the actual loss from Adhoc Unsold and the predicted/forecasted loss for the particular Month"),
                                     h1("Copyright(c) 2018, Business Brio. All rights reserved")
                                     
                                              )
                            
                            
                ))
        
      )
    } else {
      login
    }
  })
  output$sidebarpanel <- renderUI({
    if (USER$Logged == TRUE) {
      div(
        
        tags$li(class = "dropdown", actionButton("home", "Home")),
        
        sidebarMenu(id = "tabs",selected = "Map",
                    menuItem("West Bengal:Map", tabName = "Map", icon = icon("map"),selected=TRUE),
                    menuItem("Time Period", tabName = "Period", icon = icon("calendar")),
                    menuItem("Customer Analysis", tabName = "Customer", icon = icon("user-o", lib="font-awesome")),
                    menuItem("Forecasting", tabName = "Forecast",icon = icon("cog", lib = "glyphicon")),
                    menuItem("Business Impact",tabName = "BusinessImpact",icon = icon("industry", lib = "font-awesome"))
        )
        
      )
    }
  })
  observeEvent(input$home, {
    updateTabItems(session, "tabs", "Map")
  })
  
  # if(substr(Sys.time(),12,13)>="12"){
  date<-reactive({
    t<-hol[hol$Date== input$date1,]
    t$Holiday
  })
  # reactive funciton for showing the weekday for a particular date
  date_input1<-reactive({
    input$date1
  })
  date_input2<-reactive({
    weekdays(as.Date(input$date1,'%Y-%m-%d'))
  })
  date_input3<-reactive({
    quarters(as.Date(input$date1,"%Y-%m-%d"))
  })
  date_input4<-reactive({
    monthweeks(as.Date(input$date1,"%Y-%m-%d"))
  })
  date_input5<-reactive({
    week(as.Date(input$date1,"%Y-%m-%d"))
  })
  
  # rendering the value box and using tag to style the value box
  
  output$date <- renderValueBox({
    valueBox(
      "Date",
      date_input1(),icon=(class=NULL),col="light-blue"
    )
  })
  output$dayweek <- renderValueBox({
    valueBox(
      "Day of the Week",date_input2(),icon=(class=NULL),col="light-blue"
    )
  })
  
  output$qtr_t <- renderValueBox({
    valueBox(
      "Quarter",
      date_input3(),icon=(class=NULL),col="light-blue"
    )
  })
  output$wkomon_t <- renderValueBox({
    valueBox(
      "Week of the Month",
      date_input4(),icon=(class=NULL),col="light-blue"
      
    )
  })
  output$wkoyr_t <- renderValueBox({
    valueBox(
      "Week of the year",
      date_input5(),icon=(class=NULL),col="light-blue"
      
    )
  })
  output$event <- renderValueBox({
    valueBox(
      "Event",
      date(),icon=(class=NULL),col="light-blue"
    )
  })
  # }else{
  #   date<-reactive({
  #     t<-hol[hol$Date== Sys.Date(),]
  #     t$Holiday
  #   })
  #   # reactive funciton for showing the weekday for a particular date
  #   date_input1<-reactive({
  #     Sys.Date()
  #   })
  #   date_input2<-reactive({
  #     weekdays(as.Date(Sys.Date(),'%Y-%m-%d'))
  #   })
  #   date_input3<-reactive({
  #     quarters(as.Date(Sys.Date(),"%Y-%m-%d"))
  #   })
  #   date_input4<-reactive({
  #     monthweeks(as.Date(Sys.Date(),"%Y-%m-%d"))
  #   })
  #   date_input5<-reactive({
  #     week(as.Date(Sys.Date(),"%Y-%m-%d"))
  #   })
  #   # rendering the value box and using tag to style the value box
  #   output$date <- renderValueBox({
  #     valueBox(
  #       "Date",
  #       date_input1(),icon=(class=NULL),col="light-blue"
  #     
  #     )
  #   })
  #   output$dayweek <- renderValueBox({
  #     valueBox(
  #       "Day of the Week",date_input2(),icon=(class=NULL),col="light-blue"
  #     
  #     )
  #   })
  # 
  #   output$qtr_t <- renderValueBox({
  #     valueBox(
  #       "Quarter",
  #       date_input3(),icon=(class=NULL),col="light-blue"
  #     
  #     )
  #   })
  #   output$wkomon_t <- renderValueBox({
  #     valueBox(
  #       "Week of the Month",
  #       date_input4(),icon=(class=NULL),col="light-blue"
  #     
  #     )
  #   })
  #   output$wkoyr_t <- renderValueBox({
  #     valueBox(
  #       "Week of the year",
  #       date_input5(),icon=(class=NULL),col="light-blue"
  #     
  #     )
  #   })
  #   output$event <- renderValueBox({
  #     valueBox(
  #       "Event",
  #       date(),icon=(class=NULL),col="light-blue"
  #     )
  #   })
  # }
  
  output$date_text <- renderText({
    
    paste0("Today is : ",input$date1-1)
    
  })
  output$intution2 <- renderTable({
    month_result
  })
  output$today <- renderTable({
    if(input$allcustomeroption==TRUE){
      # today <- (subset(test_req_a,Calendarday == input$date1-1 & District== input$indistrict5))[,c(34,12,13,19)]
      # today[,-1] <- format(today[,-1], big.mark=",",trim=TRUE)
      # colnames(today) <- c("Customer Name and ID", "Indent (in pcs)", "Adhoc (in pcs)", "AdhocUnsold (in pcs)")
      # today
      today <- (subset(test_data,Calendarday == input$date1-1 & District== input$indistrict5))[,c(24,12,13,19)]
      tom1 <-(subset(test_data,Calendarday == input$date1 & District== input$indistrict5))[,c(24,12)]
      tom2 <-(subset(test_req_a,Calendarday == input$date1 & District== input$indistrict5))[,c(24,32)]
      tom <- merge(tom1,tom2, by="customer_id_name", all=TRUE)
      final_to_tom <- merge(today,tom, by ="customer_id_name", all=TRUE)
      final_to_tom[,-1] <- format(final_to_tom[,-1], big.mark=",", trim=TRUE)
      colnames(final_to_tom) <- c("Customer ID and Name", "Indent(current day, in pcs)", "Adhoc Sent(current day, in pcs)", "AdhocUnsold(current day, in pcs)", "Indent(next day, in pcs)", "Adhoc (Predicted, in pcs)")
      final_to_tom
    }
    else{
      today <-(subset(test_data,Calendarday == input$date1-1 & District == input$indistrict5 & customer_id_name == input$Customer5))[,c(24,12,13,19)]
      tom1 <-(subset(test_data,Calendarday == input$date1 & District== input$indistrict5 & customer_id_name == input$Customer5))[,c(24,12)]
      tom2 <-(subset(test_req_a,Calendarday == input$date1 & District== input$indistrict5 & customer_id_name == input$Customer5))[,c(24,32)]
      tom <- merge(tom1,tom2, by="customer_id_name", all=TRUE)
      final_to_tom <- merge(today,tom, by ="customer_id_name", all=TRUE)
      final_to_tom[,-1] <- format(final_to_tom[,-1], big.mark=",", trim=TRUE)
      colnames(final_to_tom) <- c("Customer ID and Name", "Indent(current day, in pcs)", "Adhoc Sent(current day, in pcs)", "AdhocUnsold(current day, in pcs)", "Indent(next day, in pcs)", "Adhoc (Predicted, in pcs)")
      final_to_tom
    }
  })
  # output$tomorrow <- renderTable({
  #   if(input$allcustomeroption==TRUE){
  #    
  #     tom <-(subset(test_req_a,Calendarday == input$date1 & District== input$indistrict5))[,c(34,12,31)]
  #     tom[,-1] <- format(tom[,-1], big.mark = ",", trim = TRUE)
  #     colnames(tom) <- c("Customer Name and ID", "Indent (in pcs)", "Adhoc (Predicted, in pcs)")
  #     tom
  #   }
  #   else{
  #     tom_cus <-(subset(test_req_a,Calendarday == input$date1 & District == input$indistrict5 & customer_id_name == input$Customer5))[,c(34,12,31)]
  #     tom_cus[,-1] <- format(tom_cus[,-1], big.mark = ",", trim = TRUE)
  #     colnames(tom_cus) <- c("Customer Name and ID", "Indent (in pcs)", "Adhoc (Predicted, in pcs)")
  #     tom_cus
  #    
  #   }
  # })
  #ABP_prwkd$CalendarDay <-format(ABP_prwkd$CalendarDay,'%Y-%m-%d')
  # output$pre_week <- renderTable({
  #   if(input$allcustomeroption==TRUE){
  #   
  #   }
  #   else{
  #     dis_cus <- ABP_prwkd[ABP_prwkd$District== input$indistrict5 & ABP_prwkd$customer_id_name ==input$Customer5,]
  #     dis_cus <- dis_cus[order(dis_cus$CalendarDay,decreasing = T),]
  #     dis_cusfil <- dis_cus[c(23,21,2,3,4,5,6,24,7,8,14)]
  #     pre_weekday <- as.data.frame(dis_cusfil[c(2,1,6,9,10,11,7)])
  #     rownames(pre_weekday)<- NULL
  #     pre_weekday<-pre_weekday[order(pre_weekday$Week,decreasing = T),]
  #   
  #     pre_weekday<-pre_weekday[c(2,4,5,6)]
  #     pre_weekday$Loss <- 5*pre_weekday$AdhocUnsold
  #     colnames(pre_weekday) <-c("Customer Name and ID","Indent(in pcs)","Adhoc(in pcs)","AdhocUnsold(in pcs)","Loss(in Rs.)")
  #     # pre_weekday<-transform(pre_weekday, Loss = stri_unescape_unicode('\u20b9531'))
  #     pre_weekday[,-c(1)] <-format(round(pre_weekday[,-c(1)],0),big.mark = ",",trim=TRUE)
  #     pre_weekday$Date <- format(c(Sys.Date()-6,Sys.Date()-13,Sys.Date()-20,Sys.Date()-27),'%Y-%m-%d')
  #     # pre_weekday[,2]<-format(pre_weekday[,2],big.mark = ",",trim=TRUE)
  #     pre_weekday[,c(1,6,2:5)]
  #   
  #   }
  # })
  # output$forecast_htable3 <- renderText({
  #   if(input$allcustomeroption==TRUE){
  #   
  #   }
  #   else{
  #     if(substr(Sys.time(),12,13)>="12"){
  #       paste0("The table shows the values for the last four ", weekdays(as.Date(Sys.Date()+1,'%Y-%m-%d')) ,"'s for the selected customer")
  #     }else{
  #       paste0("The table shows the values for the last four ",weekdays(as.Date(Sys.Date(),'%Y-%m-%d')) ,"'s for the selected customer")
  #     }
  #   }
  # })
  
  # output$forecast_htable2 <- renderText({
  #   if(input$allcustomeroption==TRUE){
  #     paste0("The table shows the predicted value of Adhoc for ( ",as.Date(input$date1,'%Y-%m-%d'),",", weekdays(as.Date(input$date1,'%Y-%m-%d')) ,") for all the customers")
  #   }
  #   else{
  #     paste0("The table shows the predicted value of Adhoc for ( ",as.Date(input$date1,'%Y-%m-%d'),",", weekdays(as.Date(input$date1,'%Y-%m-%d')) ,") for the selected customer")
  #   }
  # })
  output$forecast_htable1 <- renderText({
    if(input$allcustomeroption==TRUE){
      paste0("The table shows the actual values for the current day and predicted Adhoc values (in pcs) for ( ",as.Date(input$date1,'%Y-%m-%d'),", ", weekdays(as.Date(input$date1,'%Y-%m-%d')) ,") for all the customers")
    }
    else{
      paste0("The table shows the actual values for the current day and predicted Adhoc values (in pcs) for ( ",as.Date(input$date1,'%Y-%m-%d'),", ", weekdays(as.Date(input$date1,'%Y-%m-%d')) ,") for the selected customer")
    }
  })
  output$cust_summary_htext <- renderText({
    paste0("Comparison of Average ",input$Variable3,"(per day) for the selected customer with the District and State values of the customers belonging to the same group, for the selected time period:")
  })
  
  output$cust_htext <- renderText({
    
    abp_date <- subset(abp_total,CalendarDay>=input$dates1[1] & CalendarDay <= input$dates1[2])
    
    abp_dates <- subset(abp_date,District== input$indistrict2)
    
    abp_customer <- subset(abp_dates,customer_id_name==input$Customer1)
    abp_customer_aggr<- abp_customer %>% group_by(CalendarDay) %>% summarise(sold_indent=mean(sold_indent),IndenUnsold= mean(IndenUnsold),IndentSold=mean(IndentSold),Indent=mean(Indent),Adhoc=mean(Adhoc),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(TotalSold))
    abp_customer_aggr$Adhocboost <- round(1000*(abp_customer_aggr$sold_indent/abp_customer_aggr$Indent),0)
    abp_customer_aggr$Wastage <- 1000*(round(((abp_customer_aggr$AdhocUnsold+abp_customer_aggr$IndenUnsold)/(abp_customer_aggr$AdhocSold+abp_customer_aggr$IndentSold)),digits=2))
    
    abp_customer_aggrr <- abp_customer_aggr[,c("CalendarDay",input$Variable3)]
    
    colnames(abp_customer_aggrr) <- c("CalendarDay", "VariableCust")
    
    
    group_cust<-as.character(abp_customer$group[1])
    
    if(input$Variable3 == 'Adhocboost' | input$Variable3 == 'Wastage'){
      text <- paste0(input$Variable3, " (pcs per 1000, per day) for the selected Customer (Group:",group_cust,")")
      text
    }
    else{
      text <- paste0(input$Variable3, " (pcs per day) for the selected Customer (Group:",group_cust,")")
      text
    }
  })
  output$allcustomer <-renderUI({
    checkboxInput("allcustomeroption","All Customers", value = TRUE)
  })
  
  observeEvent(input$allcustomeroption, {
    if(input$allcustomeroption==TRUE){
      disable("cusdesc")
      disable("Customer5")
    }else{
      enable("cusdesc")
      enable("Customer5")
    }
    
  })
  
  
  observeEvent(input$Login, {
    if (USER$Logged == TRUE){
      updateTabItems(session, "tabs", "Map")
      removeClass(selector = "body", class = "sidebar-collapse")
    }
    
    
  })
  
  observeEvent(input$goButton, {
    newtab <- switch(input$tabs, "Map" = "Period","Period" = "Map")
    updateTabItems(session, "tabs", newtab)
    updateTabsetPanel(session, "inTabset",
                      selected = "Daily View - Selected Time Period"
    )
    
  })
  observeEvent(input$goButton1, {
    updateTabItems(session, "tabs", "Period")
    updateTabsetPanel(session, "inTabset",
                      selected = "Weekly View - Selected Time Period"
    )
  })
  observeEvent(input$goButton2, {
    updateTabItems(session, "tabs", "Period")
    updateTabsetPanel(session, "inTabset",
                      selected = "Monthly View - Selected Time Period"
    )
  })
  observeEvent(input$goButton3, {
    updateTabItems(session, "tabs", "Period")
    updateTabsetPanel(session, "inTabset",
                      selected = "Quarterly View - Selected Time Period"
    )
  })
  observeEvent(input$goButton4, {
    updateTabItems(session, "tabs", "Period")
    updateTabsetPanel(session, "inTabset",
                      selected= "Year"
    )
  })
  observeEvent(input$backButton, {
    updateTabItems(session, "tabs", "Forecast")
    updateTabsetPanel(session, "inTabset3",
                      selected= "Forecasting"
    )
  })
  observeEvent(input$cusdesc, {
    updateTabItems(session, "tabs", "Customer")
    updateTabsetPanel(session, "inTabset2",
                      selected= "Customer"
    )
  })
  
  observeEvent(c(input$indistrict5, input$Customer5), {
    updateSelectizeInput(session, "indistrict2", choices =Dis1 ,
                         selected = input$indistrict5)
    updateSelectizeInput(session, "Customer1", choices =datadis()[,"customer_id_name"] ,
                         selected = input$Customer5)
    
  })
  # observeEvent(input$Customer5, {
  #   updateSelectizeInput(session, "Customer1", choices =datadis()[,"customer_id_name"] ,
  #                        selected = input$Customer5)
  # 
  # })
  
  
  
  
  
  
  output$cusdescription<-renderUI({ actionButton("cusdesc", "Customer Details")})
  
  #reactive sets
  datadate<-reactive({
    subset(ABP,CalendarDay>=input$dates1[1]&CalendarDay<=input$dates1[2])
  })
  datadis<-reactive({
    filter(datadate(),District==input$indistrict2)
  })
  
  output$cusdrop <- renderUI({
    selectizeInput("Customer1","Select Customer", choices=datadis()[,"customer_id_name"])
  })
  
  
  
  #reactive sets
  dataforecast<-reactive({
    subset(test_req_a,District==input$indistrict5)
  })
  
  output$cusforecast <- renderUI({
    selectizeInput("Customer5","Select Customer", choices=c(dataforecast()[,"customer_id_name"]))
  })
  
  #Tab_chart_below_text
  output$yr_text <- renderText({
    paste("The chart shows the average ",input$Variable1," per day for the years 2010 to 2015 for ",input$indistrict1)
  })
  output$qtr_text<- renderText({
    paste("The chart shows the average ",input$Variable1," per day for the quarters of the years 2010 to 2015 for ",input$indistrict1)
  })
  
  output$mon_text<- renderText({
    paste("The chart shows the average ",input$Variable1," per day for the months of the years 2010 to 2015 for ",input$indistrict1)
  })
  
  output$wk_text<- renderText({
    paste("The chart shows the average ",input$Variable1," per day for the weeks of the years 2010 to 2015 for ",input$indistrict1)
  })
  
  output$dly_text <- renderText({
    if(input$Variable1=="AdhocUnsold"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected time period (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected time period (per day) for ",input$indistrict1)}
      
    }
    if(input$Variable1=="Adhocboost"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (per 1000) for the selected time period (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (per 1000) for the selected time period (per day) for ",input$indistrict1)}
      
    }
    if(input$Variable1=="Wastage"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (per 1000) for the selected time period (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (per 1000) for the selected time period (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="AdhocSold"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected time period (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected time period (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="Indent"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected time period (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected time period (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="Sold"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected time period (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected time period (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="Adhoc"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected time period (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected time period (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="Unsold"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected time period (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected time period (per day) for ",input$indistrict1)}
    }
    z
  })
  output$dly_text1 <- renderText({
    if(input$Variable1=="AdhocUnsold"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the years 2010 to 2015 (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the years 2010 to 2015 (per day) for ",input$indistrict1)}
      
    }
    if(input$Variable1=="Adhocboost"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (per 1000) for the years 2010 to 2015 (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (per 1000) for the years 2010 to 2015 (per day) for ",input$indistrict1)}
      
    }
    if(input$Variable1=="Wastage"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (per 1000) for the years 2010 to 2015 (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (per 1000) for the years 2010 to 2015 (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="AdhocSold"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the years 2010 to 2015 (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the years 2010 to 2015 (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="Indent"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the years 2010 to 2015 (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the years 2010 to 2015 (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="Sold"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the years 2010 to 2015 (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the years 2010 to 2015 (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="Adhoc"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the years 2010 to 2015 (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the years 2010 to 2015 (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="Unsold"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the years 2010 to 2015 (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the years 2010 to 2015 (per day) for ",input$indistrict1)}
    }
    z
  })
  
  
  #map_heading
  output$map_heading <- renderText({
    paste("Average ",input$Variable1,"(pcs per day)")
  })
  
  output$map_heading1 <- renderText({
    paste("Weekwise Average ",input$Variable1,"(pcs per day)")
  })
  
  output$map_heading2 <- renderText({
    paste("Monthwise Average ",input$Variable1,"(pcs per day)" )
  })
  
  output$map_heading3 <- renderText({
    paste("Quarterwise Average ",input$Variable1,"(pcs per day)")
  })
  
  output$map_heading4 <- renderText({
    paste("Yearwise Average ",input$Variable1,"(pcs per day)")
  })
  
  
  
  
  
  
  #map below text
  output$map_text <-renderText({
    
    if(input$Variable1=="AdhocUnsold"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected time range")
    }
    
    if(input$Variable1=="Adhocboost"){
      y<-paste("The map represents the average ",input$Variable1,"(per 1000) per day, in the districts of West Bengal for the selected time range")
    }
    if(input$Variable1=="Wastage"){
      y<-paste("The map represents the average ",input$Variable1,"(per 1000) per day, in the districts of West Bengal for the selected time range")
    }
    if(input$Variable1=="AdhocSold"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected time range")
    }
    if(input$Variable1=="Indent"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the seected time range")
    }
    if(input$Variable1=="Sold"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected time range")
    }
    if(input$Variable1=="Adhoc"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected time range")
    }
    if(input$Variable1=="Unsold"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected time range")
    }
    y
  })
  
  output$map_text1 <- renderText({
    if(input$Variable1=="AdhocUnsold"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected week range")
    }
    
    if(input$Variable1=="Adhocboost"){
      y<-paste("The map represents the average ",input$Variable1,"(per 1000) per day, in the districts of West Bengal for the selected week range")
    }
    if(input$Variable1=="Wastage"){
      y<-paste("The map represents the average ",input$Variable1,"(per 1000) per day, in the districts of West Bengal for the selected week range")
    }
    if(input$Variable1=="AdhocSold"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected week range")
    }
    if(input$Variable1=="Indent"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the seected week range")
    }
    if(input$Variable1=="Sold"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected week range")
    }
    if(input$Variable1=="Adhoc"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected week range")
    }
    if(input$Variable1=="Unsold"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected week range")
    }
    y
  })
  output$map_text2 <- renderText({
    if(input$Variable1=="AdhocUnsold"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected month range")
    }
    
    if(input$Variable1=="Adhocboost"){
      y<-paste("The map represents the average ",input$Variable1,"(per 1000) per day, in the districts of West Bengal for the selected month range")
    }
    if(input$Variable1=="Wastage"){
      y<-paste("The map represents the average ",input$Variable1,"(per 1000) per day, in the districts of West Bengal for the selected month range")
    }
    if(input$Variable1=="AdhocSold"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected month range")
    }
    if(input$Variable1=="Indent"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the seected month range")
    }
    if(input$Variable1=="Sold"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected month range")
    }
    if(input$Variable1=="Adhoc"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected month range")
    }
    if(input$Variable1=="Unsold"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected month range")
    }
    y
  })
  
  output$map_text3 <- renderText({
    if(input$Variable1=="AdhocUnsold"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected quarters")
    }
    
    if(input$Variable1=="Adhocboost"){
      y<-paste("The map represents the average ",input$Variable1,"(per 1000) per day, in the districts of West Bengal for the selected quarters")
    }
    if(input$Variable1=="Wastage"){
      y<-paste("The map represents the average ",input$Variable1,"(per 1000) per day, in the districts of West Bengal for the selected quarters")
    }
    if(input$Variable1=="AdhocSold"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected quarters")
    }
    if(input$Variable1=="Indent"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the seected quarters")
    }
    if(input$Variable1=="Sold"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected quarters")
    }
    if(input$Variable1=="Adhoc"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected quarters")
    }
    if(input$Variable1=="Unsold"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected quarters")
    }
    y
  })
  
  output$map_text4 <- renderText({
    if(input$Variable1=="AdhocUnsold"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected years")
    }
    
    if(input$Variable1=="Adhocboost"){
      y<-paste("The map represents the average ",input$Variable1,"(per 1000) per day, in the districts of West Bengal for the selected years")
    }
    if(input$Variable1=="Wastage"){
      y<-paste("The map represents the average ",input$Variable1,"(per 1000) per day, in the districts of West Bengal for the selected years")
    }
    if(input$Variable1=="AdhocSold"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected years")
    }
    if(input$Variable1=="Indent"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the seected years")
    }
    if(input$Variable1=="Sold"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected years")
    }
    if(input$Variable1=="Adhoc"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected years")
    }
    if(input$Variable1=="Unsold"){
      y<-paste("The map represents the average ",input$Variable1," per day, (in pcs) in the districts of West Bengal for the selected years")
    }
    y
  })
  
  #cust_header_text
  output$cust_hhtext <- renderText({
    paste("Analysis for a Customer :",input$Variable3)
  })
  
  output$cust_text <- renderText({
    paste("The graph shows the ",input$Variable3," (per day) for the selected customer in the district of ",input$indistrict2," for the chosen time period. The green line shows the average ",input$Variable3," (per day) for the customers of the similar group in the chosen district, with the orange line showing the average (per day) for the State of West Bengal.")
    
  })
  #Tab_header_text
  output$yr_htext <- renderText({
    paste("Yearwise Daily Average ",input$Variable1,"for",input$indistrict1)
  })
  output$qtr_htext <- renderText({
    paste("Quarterwise Daily Average ",input$Variable1,"for",input$indistrict1)
  })
  output$mon_htext <- renderText({
    paste("Monthwise Daily Average ",input$Variable1," - ",input$indistrict1)
  })
  output$wk_htext <- renderText({
    paste("Weekwise Daily Average ",input$Variable1,"for",input$indistrict1)
  })
  output$dly_htext <- renderText({
    if(input$Variable1=="AdhocUnsold"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Average ",input$Variable1,"(pcs per day) for West Bengal,(",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste("Total ",input$Variable1,"(pcs per day) for ",input$indistrict1, ",(",input$dates[1]," to ",input$dates[2],")")}
      
    }
    if(input$Variable1=="Adhocboost"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Average ",input$Variable1,"(pcs per 1000, per day per district) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste(input$Variable1,"(pcs per 1000, per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    if(input$Variable1=="Wastage"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Average ",input$Variable1,"(per 1000, per day per district) for West Bengal,(",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste(input$Variable1,"(per 1000, per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    if(input$Variable1=="AdhocSold"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Average ",input$Variable1,"(pcs per day) for",input$indistrict1," of West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste("Total ",input$Variable1,"(pcs per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")} }
    if(input$Variable1=="Indent"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Average ",input$Variable1,"(pcs per day) for",input$indistrict1," of West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste("Total ",input$Variable1,"(pcs per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    if(input$Variable1=="Sold"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Average ",input$Variable1,"(pcs per day) for",input$indistrict1," of West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste("Total ",input$Variable1,"(pcs per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    if(input$Variable1=="Adhoc"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Average ",input$Variable1,"(pcs per day) for",input$indistrict1," of West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste("Total ",input$Variable1,"(pcs per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    if(input$Variable1=="Unsold"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Average ",input$Variable1,"(pcs per day) for",input$indistrict1," of West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      
      else{t<-paste("Total ",input$Variable1,"(pcs per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    t
  })
  output$dly_htext1 <- renderText({
    if(input$Variable1=="AdhocUnsold"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Average ",input$Variable1,"(pcs per day) for ",input$indistrict1," of West Bengal")}
      else{t<-paste("Total ",input$Variable1,"(pcs per day) for ",input$indistrict1)}
      
    }
    if(input$Variable1=="Adhocboost"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Average ",input$Variable1,"(per 1000, per day) for",input$indistrict1," of West Bengal")}
      else{t<-paste("Total ",input$Variable1,"(per 1000, per day) for",input$indistrict1)}}
    if(input$Variable1=="Wastage"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Average ",input$Variable1,"(per 1000, per day) for",input$indistrict1," of West Bengal")}
      else{t<-paste("Total ",input$Variable1,"(per 1000, per day) for",input$indistrict1)}}
    if(input$Variable1=="AdhocSold"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Average ",input$Variable1,"(pcs per day) for",input$indistrict1," of West Bengal")}
      else{t<-paste("Total ",input$Variable1,"(pcs per day) for",input$indistrict1)} }
    if(input$Variable1=="Indent"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Average ",input$Variable1,"(pcs per day) for",input$indistrict1," of West Bengal")}
      else{t<-paste("Total ",input$Variable1,"(pcs per day) for",input$indistrict1)}}
    if(input$Variable1=="Sold"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Average ",input$Variable1,"(pcs per day) for",input$indistrict1," of West Bengal")}
      else{t<-paste("Total ",input$Variable1,"(pcs per day) for",input$indistrict1)}}
    if(input$Variable1=="Adhoc"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Average ",input$Variable1,"(pcs per day) for",input$indistrict1," of West Bengal")}
      else{t<-paste("Total ",input$Variable1,"(pcs per day) for",input$indistrict1)}}
    if(input$Variable1=="Unsold"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Average ",input$Variable1,"(pcs per day) for",input$indistrict1," of West Bengal")}
      
      else{t<-paste("Total ",input$Variable1,"(pcs per day) for",input$indistrict1)}}
    t
  })
  
  #Definitions
  output$definition1 <- renderText({
    
    if(input$Variable1=="AdhocUnsold"){
      y<-paste("AdhocUnsold (in pcs): It is the unsold quantity out of the total Adhoc sent to a Customer.")
    }
    
    if(input$Variable1=="Adhocboost"){
      y<-paste("Adhocboost is given per 1000 of Indent Value here. It is given as the difference between Total Sold and Indent, out of the total Indent, i.e. (Sold-Indent)/Indent")
    }
    if(input$Variable1=="Wastage"){
      y<-paste("Wastage is given per 1000 of Sold. It is the ratio of Unsold to Sold quantity, i.e. Unsold/Sold")
    }
    if(input$Variable1=="AdhocSold"){
      y<-paste("AdhocSold (in pcs) : This is the amount of copies sold out of the Adhoc sent to the Customer")
    }
    if(input$Variable1=="Indent"){
      y<-paste("Indent (in pcs) : This is the regular fixed supply sent to the customer")
    }
    if(input$Variable1=="Sold"){
      y<-paste("TotalSold (in pcs) : This the sum total of the copies of Adhoc and Indent that is sold")
    }
    if(input$Variable1=="Adhoc"){
      y<-paste("Adhoc (in pcs) : This the amount of copies sent to the customer over and above the Indent amount (i.e. in addition to the regular fixed supply)")
    }
    if(input$Variable1=="Unsold"){
      y<-paste("Unsold (in pcs) : This is sum of the copies of Adhoc and Indent that are Unsold")
    }
    y
  })
  output$definition2 <- renderText({
    
    if(input$Variable1=="AdhocUnsold"){
      y<-paste("AdhocUnsold (in pcs): It is the unsold quantity out of the total Adhoc sent to a Customer.")
    }
    
    if(input$Variable1=="Adhocboost"){
      y<-paste("Adhocboost is given per 1000 of Indent Value here. It is given as the difference between Total Sold and Indent, out of the total Indent, i.e. (Sold-Indent)/Indent")
    }
    if(input$Variable1=="Wastage"){
      y<-paste("Wastage is given per 1000 of Sold. It is the ratio of Unsold to Sold quantity, i.e. Unsold/Sold")
    }
    if(input$Variable1=="AdhocSold"){
      y<-paste("AdhocSold (in pcs) : This is the amount of copies sold out of the Adhoc sent to the Customer")
    }
    if(input$Variable1=="Indent"){
      y<-paste("Indent (in pcs) : This is the regular fixed supply sent to the customer")
    }
    if(input$Variable1=="Sold"){
      y<-paste("TotalSold (in pcs) : This the sum total of the copies of Adhoc and Indent that is sold")
    }
    if(input$Variable1=="Adhoc"){
      y<-paste("Adhoc (in pcs) : This the amount of copies sent to the customer over and above the Indent amount (i.e. in addition to the regular fixed supply)")
    }
    if(input$Variable1=="Unsold"){
      y<-paste("Unsold (in pcs) : This is sum of the copies of Adhoc and Indent that are Unsold")
    }
    y
  })
  output$definition3 <- renderText({
    
    if(input$Variable1=="AdhocUnsold"){
      y<-paste("AdhocUnsold (in pcs): It is the unsold quantity out of the total Adhoc sent to a Customer.")
    }
    
    if(input$Variable1=="Adhocboost"){
      y<-paste("Adhocboost is given per 1000 of Indent Value here. It is given as the difference between Total Sold and Indent, out of the total Indent, i.e. (Sold-Indent)/Indent")
    }
    if(input$Variable1=="Wastage"){
      y<-paste("Wastage is given per 1000 of Sold. It is the ratio of Unsold to Sold quantity, i.e. Unsold/Sold")
    }
    if(input$Variable1=="AdhocSold"){
      y<-paste("AdhocSold (in pcs) : This is the amount of copies sold out of the Adhoc sent to the Customer")
    }
    if(input$Variable1=="Indent"){
      y<-paste("Indent (in pcs) : This is the regular fixed supply sent to the customer")
    }
    if(input$Variable1=="Sold"){
      y<-paste("TotalSold (in pcs) : This the sum total of the copies of Adhoc and Indent that is sold")
    }
    if(input$Variable1=="Adhoc"){
      y<-paste("Adhoc (in pcs) : This the amount of copies sent to the customer over and above the Indent amount (i.e. in addition to the regular fixed supply)")
    }
    if(input$Variable1=="Unsold"){
      y<-paste("Unsold (in pcs) : This is sum of the copies of Adhoc and Indent that are Unsold")
    }
    y
  })
  output$definition4 <- renderText({
    
    if(input$Variable1=="AdhocUnsold"){
      y<-paste("AdhocUnsold (in pcs): It is the unsold quantity out of the total Adhoc sent to a Customer.")
    }
    
    if(input$Variable1=="Adhocboost"){
      y<-paste("Adhocboost is given per 1000 of Indent Value here. It is given as the difference between Total Sold and Indent, out of the total Indent, i.e. (Sold-Indent)/Indent")
    }
    if(input$Variable1=="Wastage"){
      y<-paste("Wastage is given per 1000 of Sold. It is the ratio of Unsold to Sold quantity, i.e. Unsold/Sold")
    }
    if(input$Variable1=="AdhocSold"){
      y<-paste("AdhocSold (in pcs) : This is the amount of copies sold out of the Adhoc sent to the Customer")
    }
    if(input$Variable1=="Indent"){
      y<-paste("Indent (in pcs) : This is the regular fixed supply sent to the customer")
    }
    if(input$Variable1=="Sold"){
      y<-paste("TotalSold (in pcs) : This the sum total of the copies of Adhoc and Indent that is sold")
    }
    if(input$Variable1=="Adhoc"){
      y<-paste("Adhoc (in pcs) : This the amount of copies sent to the customer over and above the Indent amount (i.e. in addition to the regular fixed supply)")
    }
    if(input$Variable1=="Unsold"){
      y<-paste("Unsold (in pcs) : This is sum of the copies of Adhoc and Indent that are Unsold")
    }
    y
  })
  output$definition5 <- output$definition1 <- output$definition11 <-output$definition12 <-output$definition13 <-renderText({
    
    if(input$Variable1=="AdhocUnsold"){
      y<-paste("AdhocUnsold (in pcs): It is the unsold quantity out of the total Adhoc sent to a Customer.")
    }
    
    if(input$Variable1=="Adhocboost"){
      y<-paste("Adhocboost is given per 1000 of Indent Value here. It is given as the difference between Total Sold and Indent, out of the total Indent, i.e. (Sold-Indent)/Indent")
    }
    if(input$Variable1=="Wastage"){
      y<-paste("Wastage is given per 1000 of Sold. It is the ratio of Unsold to Sold quantity, i.e. Unsold/Sold")
    }
    if(input$Variable1=="AdhocSold"){
      y<-paste("AdhocSold (in pcs) : This is the amount of copies sold out of the Adhoc sent to the Customer")
    }
    if(input$Variable1=="Indent"){
      y<-paste("Indent (in pcs) : This is the regular fixed supply sent to the customer")
    }
    if(input$Variable1=="Sold"){
      y<-paste("TotalSold (in pcs) : This the sum total of the copies of Adhoc and Indent that is sold")
    }
    if(input$Variable1=="Adhoc"){
      y<-paste("Adhoc (in pcs) : This the amount of copies sent to the customer over and above the Indent amount (i.e. in addition to the regular fixed supply)")
    }
    if(input$Variable1=="Unsold"){
      y<-paste("Unsold (in pcs) : This is sum of the copies of Adhoc and Indent that are Unsold")
    }
    y
  })
  
  output$definition6 <- renderText({
    
    if(input$Variable1=="AdhocUnsold"){
      y<-paste("AdhocUnsold (in pcs): It is the unsold quantity out of the total Adhoc sent to a Customer.")
    }
    
    if(input$Variable1=="Adhocboost"){
      y<-paste("Adhocboost is given per 1000 of Indent Value here. It is given as the difference between Total Sold and Indent, out of the total Indent, i.e. (Sold-Indent)/Indent")
    }
    if(input$Variable1=="Wastage"){
      y<-paste("Wastage is given per 1000 of Sold. It is the ratio of Unsold to Sold quantity, i.e. Unsold/Sold")
    }
    if(input$Variable1=="AdhocSold"){
      y<-paste("AdhocSold (in pcs) : This is the amount of copies sold out of the Adhoc sent to the Customer")
    }
    if(input$Variable1=="Indent"){
      y<-paste("Indent (in pcs) : This is the regular fixed supply sent to the customer")
    }
    if(input$Variable1=="Sold"){
      y<-paste("TotalSold (in pcs) : This the sum total of the copies of Adhoc and Indent that is sold")
    }
    if(input$Variable1=="Adhoc"){
      y<-paste("Adhoc (in pcs) : This the amount of copies sent to the customer over and above the Indent amount (i.e. in addition to the regular fixed supply)")
    }
    if(input$Variable1=="Unsold"){
      y<-paste("Unsold (in pcs) : This is sum of the copies of Adhoc and Indent that are Unsold")
    }
    y
  })
  
  output$map_definition <- output$map_definition1 <- output$map_definition2 <-output$map_definition3 <-output$map_definition4 <- renderText({
    
    if(input$Variable1=="AdhocUnsold"){
      y<-paste("AdhocUnsold (in pcs): It is the unsold quantity out of the total Adhoc sent to a Customer.")
    }
    
    if(input$Variable1=="Adhocboost"){
      y<-paste("Adhocboost is given per 1000 of Indent Value here. It is given as the difference between Total Sold and Indent, out of the total Indent, i.e. (Sold-Indent)/Indent")
    }
    if(input$Variable1=="Wastage"){
      y<-paste("Wastage is given per 1000 of Sold. It is the ratio of Unsold to Sold quantity, i.e. Unsold/Sold")
    }
    if(input$Variable1=="AdhocSold"){
      y<-paste("AdhocSold (in pcs) : This is the amount of copies sold out of the Adhoc sent to the Customer")
    }
    if(input$Variable1=="Indent"){
      y<-paste("Indent (in pcs) : This is the regular fixed supply sent to the customer")
    }
    if(input$Variable1=="Sold"){
      y<-paste("TotalSold (in pcs) : This the sum total of the copies of Adhoc and Indent that is sold")
    }
    if(input$Variable1=="Adhoc"){
      y<-paste("Adhoc (in pcs) : This the amount of copies sent to the customer over and above the Indent amount (i.e. in addition to the regular fixed supply)")
    }
    if(input$Variable1=="Unsold"){
      y<-paste("Unsold (in pcs) : This is sum of the copies of Adhoc and Indent that are Unsold")
    }
    y
  })
  
  
  output$cust_definition <- renderText({
    
    if(input$Variable3=="AdhocUnsold"){
      y<-paste("*AdhocUnsold(in pcs): It is the unsold quantity out of the total Adhoc sent to a Customer.")
    }
    
    if(input$Variable3=="Adhocboost"){
      y<-paste("*Adhocboost(per 1000 of Indent) : It is given per 1000 of Indent Value here. It is given as the difference between Total Sold and Indent, out of the total Indent, i.e. (Sold-Indent)/Indent")
    }
    if(input$Variable3=="Wastage"){
      y<-paste("*Wastage(per 1000 of Sold) : It is given per 1000 of Sold. It is the ratio of Unsold to Sold quantity, i.e. Unsold/Sold")
    }
    if(input$Variable3=="AdhocSold"){
      y<-paste("*AdhocSold (in pcs) : This is the amount of copies sold out of the Adhoc sent to the Customer")
    }
    if(input$Variable3=="Indent"){
      y<-paste("*Indent (in pcs) : This is the regular fixed supply sent to the customer")
    }
    if(input$Variable3=="Sold"){
      y<-paste("*TotalSold (in pcs) : This the sum total of the copies of Adhoc and Indent that is sold")
    }
    if(input$Variable3=="Adhoc"){
      y<-paste("*Adhoc (in pcs) : This the amount of copies sent to the customer over and above the Indent amount (i.e. in addition to the regular fixed supply)")
    }
    if(input$Variable3=="Unsold"){
      y<-paste("*Unsold (in pcs) : This is sum of the copies of Adhoc and Indent that are Unsold")
    }
    y
  })
  
  #charts_for_different_time_periods
  output$plot_yr <- renderPlotly({
    
    if(input$indistrict1=="All Districts"){
      
      
      abp_yr1<-yr_year[,c("year_yr",input$Variable1)]
      
      colnames(abp_yr1) <- c("year_yr", "Yvar")
      
      f2<-list(
        family="Old Standard TT,serif",
        size=12,
        color="black"
      )
      
      a <- list(
        title = "Year",
        showticklabels = TRUE,
        tickfont=f2
      )
      b <- list(
        title = input$Variable1,
        showticklabels = TRUE
      )
      
      myplot<- plot_ly(x=~abp_yr1$year_yr,y=~abp_yr1$Yvar,type = "scatter",mode="lines+markers")%>%
        layout(xaxis = a,yaxis = b)
      
    }
    else{
      #yearly
      abp_yr1<-abp_yr[abp_yr$District==input$indistrict1,]
      abp_yr2<-abp_yr1[,c("year_yr","District",input$Variable1)]
      
      colnames(abp_yr2) <- c("year_yr","District", "Yvar")
      
      f2<-list(
        family="Old Standard TT,serif",
        size=12,
        color="black"
      )
      
      a <- list(
        title = "Year",
        showticklabels = TRUE,
        tickfont=f2
      )
      b <- list(
        title = input$Variable1,
        showticklabels = TRUE
      )
      
      myplot<-plot_ly(x=~abp_yr2$year_yr,y=~abp_yr2$Yvar,type = "scatter",mode="lines+markers")%>%
        layout(xaxis = a,yaxis = b)
    }
    myplot
  })
  
  output$plot_qtr <- renderPlotly({
    
    if(input$indistrict1=="All Districts"){
      
      
      abp_qtr1<-qtr_quarter[,c("year_qtr",input$Variable1)]
      
      colnames(abp_qtr1) <- c("year_qtr", "Yvar")
      
      f2<-list(
        family="Old Standard TT,serif",
        size=10,
        color="black"
      )
      
      a <- list(
        title = "Quarter",
        showticklabels = TRUE,tickangle=45,
        tickfont=f2
      )
      b <- list(
        title = input$Variable1,
        showticklabels = TRUE
      )
      
      myplot<- plot_ly(x=~abp_qtr1$year_qtr,y=~abp_qtr1$Yvar,type = "scatter",mode="lines+markers")%>%
        layout(xaxis = a,yaxis = b)
      
    }
    else{
      #quarterly
      abp_qtr1<-abp_qtr[abp_qtr$District==input$indistrict1,]
      abp_qtr2<-abp_qtr1[,c("year_qtr","District",input$Variable1)]
      
      colnames(abp_qtr2) <- c("year_qtr","District", "Yvar")
      f2<-list(
        family="Old Standard TT,serif",
        size=10,
        color="black"
      )
      a <- list(
        title = "Quarter",
        showticklabels = TRUE,tickangle=45,tickfont=f2
      )
      b <- list(
        title = input$Variable1,
        showticklabels = TRUE
      )
      
      myplot<-plot_ly(x=~abp_qtr2$year_qtr,y=~abp_qtr2$Yvar,type = "scatter",mode="lines+markers")%>%
        layout(xaxis = a,yaxis = b)
    }
    myplot
  })
  
  output$plot_mon <- renderPlotly({
    
    if(input$indistrict1=="All Districts"){
      
      
      abp_mon1<-mon_month[,c("year_mon",input$Variable1)]
      
      colnames(abp_mon1) <- c("year_mon", "Yvar")
      
      f2<-list(
        family="Old Standard TT,serif",
        size=10,
        color="black"
      )
      
      a <- list(
        title = "Month",
        showticklabels = TRUE,tickangle=45,dtick=4,
        tickfont=f2
      )
      b <- list(
        title = input$Variable1,
        showticklabels = TRUE
      )
      
      myplot<- plot_ly(x=~abp_mon1$year_mon,y=~abp_mon1$Yvar,type = "scatter",mode="lines+markers")%>%
        layout(xaxis = a,yaxis = b)
      
    }
    else{
      #monthly
      abp_mon1<-abp_mon[abp_mon$District==input$indistrict1,]
      abp_mon2<-abp_mon1[,c("year_mon","District",input$Variable1)]
      
      colnames(abp_mon2) <- c("year_mon","District", "Yvar")
      f2<-list(
        family="Old Standard TT,serif",
        size=10,
        color="black"
      )
      
      a <- list(
        title = "Month",
        showticklabels = TRUE,tickangle=45,dtick=4,tickfont=f2
      )
      b <- list(
        title = input$Variable1,
        showticklabels = TRUE
      )
      
      myplot<-plot_ly(x=~abp_mon2$year_mon,y=~abp_mon2$Yvar,type = "scatter",mode="lines+markers")%>%
        layout(xaxis = a,yaxis = b)
    }
    myplot
  })
  
  output$plot_wk <- renderPlotly({
    
    if(input$indistrict1=="All Districts"){
      
      abp_wk1<-wk_week[,c("year_wk",input$Variable1)]
      
      colnames(abp_wk1) <- c("year_wk", "Yvar")
      
      f2<-list(
        family="Old Standard TT,serif",
        size=10,
        color="black"
      )
      
      a <- list(
        title = "Week",
        showticklabels = TRUE,tickangle=45,
        autotick = F, dtick = 6,tickfont=f2
      )
      b <- list(
        title = input$Variable1,
        showticklabels = TRUE
      )
      
      myplot<- plot_ly(x=~abp_wk1$year_wk,y=~abp_wk1$Yvar,type = "scatter",mode="lines+markers")%>%
        layout(xaxis = a,yaxis = b)
      
    }
    else{
      #weekly
      abp_wk1<-abp_wk[abp_wk$District==input$indistrict1,]
      abp_wk2<-abp_wk1[,c("year_wk","District",input$Variable1)]
      
      colnames(abp_wk2) <- c("year_wk","District", "Yvar")
      f2<-list(
        family="Old Standard TT,serif",
        size=10,
        color="black"
      )
      
      a <- list(
        title = "Week",
        showticklabels = TRUE,tickangle=45,
        autotick = F, dtick = 6,tickfont=f2
        
      )
      b <- list(
        
        title = input$Variable1,
        showticklabels = TRUE
        
      )
      
      myplot<-plot_ly(x=~abp_wk2$year_wk,y=~abp_wk2$Yvar,type = "scatter",mode="lines+markers")%>%
        layout(xaxis = a,yaxis = b)
    }
    myplot
  })
  
  output$plot_dly <- renderPlotly({
    if(input$indistrict1=="All Districts"){
      
      dly_daily1 <- subset(dly_daily,CalendarDay>=input$dates[1] & CalendarDay <= input$dates[2])
      
      
      abp_dly1<-dly_daily1[,c("CalendarDay",input$Variable1)]
      
      colnames(abp_dly1) <- c("Date", "Yvar")
      
      f2<-list(
        family="Old Standard TT,serif",
        size=8,
        color="black"
      )
      
      a <- list(
        title = "Date"
        
      )
      b <- list(
        title = input$Variable1,
        showticklabels = TRUE
      )
      
      myplot<- plot_ly(x=~abp_dly1$Date,y=~abp_dly1$Yvar,type = "scatter",mode="lines+markers")%>%
        layout(xaxis=a,yaxis = b)
      
    }
    else{
      #daily
      abp_daily1 <- subset(abp_daily,CalendarDay>=input$dates[1] & CalendarDay <= input$dates[2])
      
      abp_dly1<-abp_daily1[abp_daily1$District==input$indistrict1,]
      abp_dly2<-abp_dly1[,c("CalendarDay","District",input$Variable1)]
      
      colnames(abp_dly2) <- c("date","District", "Yvar")
      f2<-list(
        family="Old Standard TT,serif",
        size=8,
        color="black"
      )
      
      a <- list(
        
        title = "Date"
        
        
      )
      b <- list(
        
        title = input$Variable1,
        showticklabels = TRUE
        
      )
      
      myplot<-plot_ly(x=~abp_dly2$date,y=~abp_dly2$Yvar,type = "scatter",mode="lines+markers")%>%
        layout(xaxis=a,yaxis = b)
    }
    myplot
  })
  output$plot_dly1 <- renderPlotly({
    if(input$indistrict1=="All Districts"){
      abp_dly1<-dly_daily[,c("CalendarDay",input$Variable1)]
      colnames(abp_dly1) <- c("Date", "Yvar")
      f2<-list(
        family="Old Standard TT,serif",
        size=9,
        color="black"
      )
      a <- list(
        title = "Date",
        showticklabels = TRUE,
        tickfont=f2
      )
      b <- list(
        title = input$Variable1,
        showticklabels = TRUE
      )
      myplot<- plot_ly(x=~abp_dly1$Date,y=~abp_dly1$Yvar,type = "scatter",mode="lines+markers")%>%
        layout(xaxis = a,yaxis = b)
    }
    else{
      #daily
      abp_dly1<-abp_daily[abp_daily$District==input$indistrict1,]
      abp_dly2<-abp_dly1[,c("CalendarDay","District",input$Variable1)]
      
      colnames(abp_dly2) <- c("date","District", "Yvar")
      f2<-list(
        family="Old Standard TT,serif",
        size=9,
        color="black"
      )
      
      a <- list(
        
        title = "Date",
        showticklabels = TRUE,tickangle=45,tickfont=f2
        
      )
      b <- list(
        
        title = input$Variable1,
        showticklabels = TRUE
        
      )
      
      myplot<-plot_ly(x=~abp_dly2$date,y=~abp_dly2$Yvar,type = "scatter",mode="lines+markers")%>%
        layout(xaxis = a,yaxis = b)
    }
    myplot
  })
  
  
  output$pareto_wastage <- renderPlotly({
    adun_not_0 <- ABP[(ABP$AdhocUnsold!=0),]
    adun_group <-aggregate(AdhocUnsold~customer_id_name,adun_not_0,mean)
    adun_group[,-c(1)] <- round(adun_group[,-c(1)],0)
    
    adun_dat <- adun_group[order(adun_group$AdhocUnsold, decreasing=TRUE),]
    adun_dat$customer_id_name <- factor(adun_dat$customer_id_name, levels=adun_dat$customer_id_name)
    adun_dat$cum <- cumsum(adun_dat$AdhocUnsold)
    count.sum<-sum(adun_dat$AdhocUnsold)
    adun_dat$cum_perc<-100*adun_dat$cum/count.sum
    adun_dat$c1 <- 1
    adun_dat$cum_pop <- cumsum(adun_dat$c1)
    adun_dat$cum_per_pop <- round(((adun_dat$cum_pop/sum(adun_dat$c1))*100),0)
    
    library(plotly)
    ay <- list(
      zeroline = FALSE,
      showline = FALSE,
      
      showgrid = FALSE,
      overlaying = "y",range= c(0,110),
      side = "right",
      title = "Cumulative Percentage"
    )
    
    
    adun_p <- plot_ly() %>%
      add_bars(x = ~adun_dat$customer_id_name, y= ~adun_dat$AdhocUnsold, type = 'bar',name = "Average AdhocUnsold(in pcs, per day)")%>%
      add_lines(x = ~adun_dat$customer_id_name, y = ~adun_dat$cum_perc, name = "Cumulative Percentage",yaxis = "y2",mode = "lines+markers", hoverinfo = 'text', text = ~paste("Customers:", adun_dat$cum_per_pop,"%")) %>%
      layout(
        title = "", yaxis2 = ay,
        xaxis = list(title="Customer",showticklabels = FALSE)
      )%>%
      layout(yaxis  = list(title="Average AdhocUnsold(in pcs, per day)"))
    adun_p
  })
  
  output$pareto_Indent <- renderPlotly({
    abp_group1 <-aggregate(Indent~customer_id_name,ABP,mean)
    
    dat <- abp_group1[order(abp_group1$Indent, decreasing=TRUE),]
    dat$customer_id_name <- factor(dat$customer_id_name, levels=dat$customer_id_name)
    dat$cum <- cumsum(dat$Indent)
    count.sum<-sum(dat$Indent)
    dat$cum_perc<-100*dat$cum/count.sum
    
    library(plotly)
    ay1 <- list(
      zeroline = FALSE,
      showline = FALSE,
      
      showgrid = FALSE,
      overlaying = "y",range= c(0,110),
      side = "right",
      title = "Cumulative Percentage"
    )
    
    Ind_p <- plot_ly() %>%
      add_bars(x = ~dat$customer_id_name, y= ~round(dat$Indent,0), type = 'bar',name = "Average Indent (in pcs, per day)")%>%
      add_lines(x = ~dat$customer_id_name, y = ~round(dat$cum_perc,2), name = "Cumulative Percentage",yaxis = "y2",mode = "lines+markers") %>%
      layout(
        title = "", yaxis2 = ay1,
        xaxis = list(title="Customer",showticklabels = FALSE)
      )%>%
      layout(yaxis  = list(title="Average Indent(in pcs, per day)"))
    Ind_p
    
  })
  
  output$cust_plot1 <- renderPlotly({
    
    # abp_date <- subset(abp_total,CalendarDay>="2010-01-01" & CalendarDay <= "2010-08-01")
    # abp_dates <- subset(abp_date,District== "Jalpaiguri")
    
    abp_date <- subset(abp_total,CalendarDay>=input$dates1[1] & CalendarDay <= input$dates1[2])
    
    abp_dates <- subset(abp_date,District== input$indistrict2)
    
    abp_customer <- subset(abp_dates,customer_id_name==input$Customer1)
    abp_customer_aggr<- abp_customer %>% group_by(CalendarDay) %>% summarise(sold_indent=mean(sold_indent),IndenUnsold= mean(IndenUnsold),IndentSold=mean(IndentSold),Indent=mean(Indent),Adhoc=mean(Adhoc),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(TotalSold))
    abp_customer_aggr$Adhocboost <- round(1000*(abp_customer_aggr$sold_indent/abp_customer_aggr$Indent),0)
    abp_customer_aggr$Wastage <- 1000*(round(((abp_customer_aggr$AdhocUnsold+abp_customer_aggr$IndenUnsold)/(abp_customer_aggr$AdhocSold+abp_customer_aggr$IndentSold)),digits=2))
    
    abp_customer_aggrr <- abp_customer_aggr[,c("CalendarDay",input$Variable3)]
    colnames(abp_customer_aggrr) <- c("CalendarDay", "VariableCust")
    
    group_cust<-as.character(abp_customer$group[1])
    dist_cut <- as.character(abp_customer$District[1])
    
    abp_dates_all_1<-abp_date[abp_date$group==group_cust,]
    
    abp_dates_all <- abp_dates_all_1 %>% group_by(CalendarDay) %>% summarise(sold_indent=mean(sold_indent),IndenUnsold= mean(IndenUnsold),IndentSold=mean(IndentSold),Indent=mean(Indent),Adhoc=mean(Adhoc),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(TotalSold))
    abp_dates_all$Adhocboost <- round(1000*(abp_dates_all$sold_indent/abp_dates_all$Indent),0)
    abp_dates_all$Wastage <- 1000*(round(((abp_dates_all$AdhocUnsold+abp_dates_all$IndenUnsold)/(abp_dates_all$AdhocSold+abp_dates_all$IndentSold)),digits=2))
    
    abp_dates_all[,-1] <- round(abp_dates_all[,-1],2)
    abp_dates_all1 <- abp_dates_all[,c("CalendarDay", input$Variable3)]
    colnames(abp_dates_all1) <- c("CalendarDay", "VariableAll")
    
    abp_dates_final<-merge(abp_dates_all1,abp_customer_aggrr,by="CalendarDay",all=T)
    #colnames(abp_dates_final)<-c("CalendarDay","WastageAll","WastageCust")
    
    abp_dates_all_dist <- abp_dates[abp_dates$group==group_cust,]
    abp_dates_all_new<-abp_dates_all_dist %>% group_by(CalendarDay) %>% summarise(sold_indent=mean(sold_indent),IndenUnsold= mean(IndenUnsold),IndentSold=mean(IndentSold),Indent=mean(Indent),Adhoc=mean(Adhoc),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(TotalSold))
    abp_dates_all_new$Adhocboost <- round(1000*(abp_dates_all_new$sold_indent/abp_dates_all_new$Indent),0)
    abp_dates_all_new$Wastage <- 1000*(round(((abp_dates_all_new$AdhocUnsold+abp_dates_all_new$IndenUnsold)/(abp_dates_all_new$AdhocSold+abp_dates_all_new$IndentSold)),digits=2))
    
    abp_dates_all_new[,-1] <- round(abp_dates_all_new[,-1],2)
    abp_dates_all_new1 <- abp_dates_all_new[,c("CalendarDay", input$Variable3)]
    colnames(abp_dates_all_new1) <- c("CalendarDay", "VariableDist")
    abp_dates_final<-merge(abp_dates_final,abp_dates_all_new1,by="CalendarDay",all=T)
    
    b <- list(
      
      title = input$Variable3,
      showticklabels = TRUE
      
    )
    
    p <- plot_ly(abp_dates_final, x = ~CalendarDay, y = ~abp_dates_final[,3], name=paste0(input$Variable3," - Customer"), type = 'scatter', mode = 'lines+markers')%>% add_trace(y=~round(abp_dates_final[,2],0),name=paste0(input$Variable3," - WB"))%>%add_trace(y=~abp_dates_final[,4],name=paste0(input$Variable3," - ",input$indistrict2))%>%
      layout(title=input$Customer1,yaxis = b)
    p
  })
  
  output$cust_summary <- renderTable({
    
    abp_date <- subset(abp_total,CalendarDay>=input$dates1[1] & CalendarDay <= input$dates1[2])
    
    abp_dates <- subset(abp_date,District== input$indistrict2)
    
    abp_customer <- subset(abp_dates,customer_id_name==input$Customer1)
    abp_customer_aggr<- abp_customer %>% group_by(CalendarDay) %>% summarise(sold_indent=mean(sold_indent,na.rm=TRUE),IndenUnsold= mean(IndenUnsold,na.rm=TRUE),IndentSold=mean(IndentSold,na.rm=TRUE),Indent=mean(Indent,na.rm=TRUE),Adhoc=mean(Adhoc,na.rm=TRUE),AdhocUnsold=mean(AdhocUnsold,na.rm=TRUE),AdhocSold=mean(AdhocSold,na.rm=TRUE),Unsold=mean(Unsold,na.rm=TRUE),Sold=mean(TotalSold,na.rm=TRUE))
    abp_customer_aggr$Adhocboost <- round(1000*(abp_customer_aggr$sold_indent/abp_customer_aggr$Indent),0)
    abp_customer_aggr$Wastage <- 1000*(round(((abp_customer_aggr$AdhocUnsold+abp_customer_aggr$IndenUnsold)/(abp_customer_aggr$AdhocSold+abp_customer_aggr$IndentSold)),digits=2))
    
    abp_customer_aggrr <- abp_customer_aggr[,-2]
    group_cust<-as.character(abp_customer$group[1])
    dist_cut <- as.character(abp_customer$District[1])
    abp_dates_all_1<-abp_date[abp_date$group==group_cust,]
    abp_dates_all <- abp_dates_all_1 %>% group_by(CalendarDay) %>% summarise(sold_indent=mean(sold_indent),IndenUnsold= mean(IndenUnsold),IndentSold=mean(IndentSold),Indent=mean(Indent),Adhoc=mean(Adhoc),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(TotalSold))
    abp_dates_all$Adhocboost <- round(1000*(abp_dates_all$sold_indent/abp_dates_all$Indent),0)
    abp_dates_all$Wastage <- 1000*(round(((abp_dates_all$AdhocUnsold+abp_dates_all$IndenUnsold)/(abp_dates_all$AdhocSold+abp_dates_all$IndentSold)),digits=2))
    abp_dates_all[,-1] <- round(abp_dates_all[,-1],2)
    abp_dates_all1 <- abp_dates_all[,-2]
    
    abp_dates_all_dist <- abp_dates[abp_dates$group==group_cust,]
    abp_dates_all_new<-abp_dates_all_dist %>% group_by(CalendarDay) %>% summarise(sold_indent=mean(sold_indent,na.rm=TRUE),IndenUnsold= mean(IndenUnsold,na.rm=TRUE),IndentSold=mean(IndentSold,na.rm=TRUE),Indent=mean(Indent,na.rm=TRUE),Adhoc=mean(Adhoc,na.rm=TRUE),AdhocUnsold=mean(AdhocUnsold,na.rm=TRUE),AdhocSold=mean(AdhocSold,na.rm=TRUE),Unsold=mean(Unsold,na.rm=TRUE),Sold=mean(TotalSold,na.rm=TRUE))
    abp_dates_all_new$Adhocboost <- round(1000*(abp_dates_all_new$sold_indent/abp_dates_all_new$Indent),0)
    abp_dates_all_new$Wastage <- 1000*(round(((abp_dates_all_new$AdhocUnsold+abp_dates_all_new$IndenUnsold)/(abp_dates_all_new$AdhocSold+abp_dates_all_new$IndentSold)),digits=2))
    abp_dates_all_new[,-1] <- round(abp_dates_all_new[,-1],2)
    abp_dates_all_new1 <- abp_dates_all_new[,-2]
    
    abp_cust_ind <- as.data.frame(cbind(round(mean(abp_customer_aggrr$Indent,na.rm=TRUE),0),round(mean(abp_dates_all_new1$Indent,na.rm=TRUE),0),cbind(round(mean(abp_dates_all1$Indent,na.rm=TRUE),0))))
    colnames(abp_cust_ind) <- c("Customer","District","WestBengal")
    rownames(abp_cust_ind) <- "Indent (in pcs)"
    abp_cust_ind$Variable <- rownames(abp_cust_ind)
    rownames(abp_cust_ind)<-NULL
    abp_cust_ind<-abp_cust_ind[,c(4,1:3)]
    abp_cust_au <- as.data.frame(cbind(round(mean(abp_customer_aggrr$AdhocUnsold,na.rm=TRUE),0),round(mean(abp_dates_all_new1$AdhocUnsold,na.rm=TRUE),0),cbind(round(mean(abp_dates_all1$AdhocUnsold,na.rm=TRUE),0))))
    colnames(abp_cust_au) <- c("Customer","District","WestBengal")
    rownames(abp_cust_au) <- "AdhocUnsold (in pcs)"
    abp_cust_au$Variable <- rownames(abp_cust_au)
    rownames(abp_cust_au)<-NULL
    abp_cust_au <-abp_cust_au[,c(4,1:3)]
    abp_cust_ab <- as.data.frame(cbind(round(mean(abp_customer_aggrr$Adhocboost,na.rm=TRUE),0),round(mean(abp_dates_all_new1$Adhocboost,na.rm=TRUE),0),cbind(round(mean(abp_dates_all1$Adhocboost,na.rm=TRUE),0))))
    colnames(abp_cust_ab) <- c("Customer","District","WestBengal")
    rownames(abp_cust_ab) <- "Adhocboost (pcs per 1000)"
    abp_cust_ab$Variable <- rownames(abp_cust_ab)
    rownames(abp_cust_ab)<-NULL
    abp_cust_ab<-abp_cust_ab[,c(4,1:3)]
    abp_cust_wt <- as.data.frame(cbind(round(mean(abp_customer_aggrr$Wastage,na.rm=TRUE),0),round(mean(abp_dates_all_new1$Wastage,na.rm=TRUE),0),cbind(round(mean(abp_dates_all1$Wastage,na.rm=TRUE),0))))
    colnames(abp_cust_wt) <- c("Customer","District","WestBengal")
    rownames(abp_cust_wt) <- "Wastage (pcs per 1000)"
    abp_cust_wt$Variable <- rownames(abp_cust_wt)
    rownames(abp_cust_wt)<-NULL
    abp_cust_wt<-abp_cust_wt[,c(4,1:3)]
    abp_cust_as <- as.data.frame(cbind(round(mean(abp_customer_aggrr$AdhocSold,na.rm=TRUE),0),round(mean(abp_dates_all_new1$AdhocSold,na.rm=TRUE),0),cbind(round(mean(abp_dates_all1$AdhocSold,na.rm=TRUE),0))))
    colnames(abp_cust_as) <- c("Customer","District","WestBengal")
    rownames(abp_cust_as) <- "AdhocSold (in pcs)"
    abp_cust_as$Variable <- rownames(abp_cust_as)
    rownames(abp_cust_as)<-NULL
    abp_cust_as <-abp_cust_as[,c(4,1:3)]
    abp_cust_ad <- as.data.frame(cbind(round(mean(abp_customer_aggrr$Adhoc,na.rm=TRUE),0),round(mean(abp_dates_all_new1$Adhoc,na.rm=TRUE),0),cbind(round(mean(abp_dates_all1$Adhoc,na.rm=TRUE),0))))
    colnames(abp_cust_ad) <- c("Customer","District","WestBengal")
    rownames(abp_cust_ad) <- "Adhoc (in pcs)"
    abp_cust_ad$Variable <- rownames(abp_cust_ad)
    rownames(abp_cust_ad)<-NULL
    abp_cust_ad<-abp_cust_ad[,c(4,1:3)]
    abp_cust_sl <- as.data.frame(cbind(round(mean(abp_customer_aggrr$Sold,na.rm=TRUE),0),round(mean(abp_dates_all_new1$Sold,na.rm=TRUE),0),cbind(round(mean(abp_dates_all1$Sold,na.rm=TRUE),0))))
    colnames(abp_cust_sl) <- c("Customer","District","WestBengal")
    rownames(abp_cust_sl) <- "TotalSold (in pcs)"
    abp_cust_sl$Variable <- rownames(abp_cust_sl)
    rownames(abp_cust_sl)<-NULL
    abp_cust_sl <-abp_cust_sl[,c(4,1:3)]
    abp_cust_usld <- as.data.frame(cbind(round(mean(abp_customer_aggrr$Unsold,na.rm=TRUE),0),round(mean(abp_dates_all_new1$Unsold,na.rm=TRUE),0),cbind(round(mean(abp_dates_all1$Unsold,na.rm=TRUE),0))))
    colnames(abp_cust_usld) <- c("Customer","District","WestBengal")
    rownames(abp_cust_usld) <- "Unsold (in pcs)"
    abp_cust_usld$Variable <- rownames(abp_cust_usld)
    rownames(abp_cust_usld)<-NULL
    abp_cust_usld <-abp_cust_usld[,c(4,1:3)]
    final_df <- data.frame(rbind(abp_cust_ab,abp_cust_ad,abp_cust_as,abp_cust_au,abp_cust_ind,abp_cust_sl,abp_cust_usld,abp_cust_wt))
    final_df[,-1]<- format(round(final_df[,-1],0),big.mark = ",",trim=TRUE)
    final_df
    
  })
  
 India<-readRDS("C:/Adak/Projects/04. OTHERS/R/version1.6/GADM_2.8_IND_adm2.rds")
  wb <- subset(India, NAME_1 == "West Bengal")
  map <- fortify(wb);
  map$id <- as.integer(map$id);
  wb_dist<-wb$NAME_2
  wb_dist<-as.data.frame(wb_dist)
  colnames(wb_dist)<-"District"
  
  wb1<-as.data.frame(wb)
  wb1<-wb1[,c("ID_2","NAME_2")]
  colnames(wb1)<-c("id","District")
  wb1$lat<-district$Latitude
  wb1$lng<-district$Longitude
  
  abp_merge<-merge(abp_daily,wb1,by="District",all=T)
  
  output$Map_cust <- renderLeaflet({
    
    wb_dist<-cbind(wb$NAME_2,wb$ID_2)
    wb_dist<-as.data.frame(wb_dist)
    colnames(wb_dist) <- c("District","Code")
    abp_daily1 <- abp_daily
    abp_daily1$District <- as.character(abp_daily1$District)
    abp_daily1 <- within(abp_daily1, District[District == "North_24_Parganas"] <- 'North 24 Parganas')
    abp_daily1 <- within(abp_daily1, District[District == 'South_24_Parganas'] <- 'South 24 Parganas')
    abp_daily1 <- within(abp_daily1, District[District == 'Koch_Bihar'] <- 'Koch Bihar')
    abp_daily1 <- within(abp_daily1, District[District == 'Paschim_Medinipur'] <- 'Pashchim Medinipur')
    abp_daily1 <- within(abp_daily1, District[District == 'Dakshin_Dinajpur'] <- 'Dakshin Dinajpur')
    abp_daily1 <- within(abp_daily1, District[District == 'Uttar_Dinajpur'] <- 'Uttar Dinajpur')
    abp_daily1 <- within(abp_daily1, District[District == 'Purba_Medinipur'] <- 'Purba Medinipur')
    #abp1 <- merge(abp_daily1,wb_dist,all=TRUE)
    #ABP_year <- abp1[(abp1$CalendarDay==input$dates),]
    reqd_df <- subset(abp_daily1, CalendarDay>=input$dates[1] & CalendarDay <= input$dates[2])
    abp_map <- reqd_df %>% group_by(District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Indent=mean(Indent),Adhoc=mean(Adhoc),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    abp_map[,-1] <- round(abp_map[,-1],0)
    abp_map1 <- merge(abp_map,wb_dist,all=TRUE)
    map_df <- abp_map1[,c("District",input$Variable1)]
    colnames(map_df) <- c("District","Variable")
    gap <- (max(map_df$Variable,na.rm = TRUE)-min(map_df$Variable, na.rm = TRUE))/3
    v2 <- min(map_df$Variable, na.rm = TRUE)
    v2 <- as.numeric(v2)
    v4 <- v2+gap
    v4 <- as.numeric(v4)
    v5 <- v2+(2*gap)
    v5 <- as.numeric(v5)
    v6 <- max(map_df$Variable, na.rm = TRUE)
    v6 <- as.numeric(v6)
    c3 <- cut(map_df$Variable,breaks=c(v2,v4,v5,v6),include.lowest=T)
    summary(c3)
    c2<-sub(".*\\[ *(.*?) *,.*", "\\1", levels(c3)[1])
    c2<-round(as.numeric(c2),0)
    c4<-sub(".*, *(.*?) *].*", "\\1", levels(c3)[1])
    c4<-round(as.numeric(c4),0)
    c5<-sub(".*, *(.*?) *].*", "\\1", levels(c3)[2])
    c5<-round(as.numeric(c5),0)
    c6<-sub(".*, *(.*?) *].*", "\\1", levels(c3)[3])
    c6<-round(as.numeric(c6),0)
    c7<-sub(".*, *(.*?) *].*", "\\1", levels(c3)[4])
    c7<-round(as.numeric(c7),0)
    bins <- c(c2,c4,c5,c6)
    if(input$Variable1=='Adhocboost'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"(per 1000 Indent)")
    }
    if(input$Variable1=='AdhocUnsold'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='Wastage'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"(per 1000 Sold)")
    }
    if(input$Variable1=='Adhoc'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='AdhocSold'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='Indent'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='Sold'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='Unsold'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    ##############################################
    if(input$Variable1=='Adhocboost'){
      legend_head <- paste("Average", ":","<br>" ,input$Variable1,"<br>", " (per 1000 Indent)")
    }
    if(input$Variable1=='AdhocUnsold'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    if(input$Variable1=='Wastage'){
      legend_head <- paste("Average",":","<br>", input$Variable1,"<br>"," (per 1000 Sold)")
    }
    if(input$Variable1=='Adhoc'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    if(input$Variable1=='AdhocSold'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    if(input$Variable1=='Indent'){
      legend_head <- paste("Average", ":","<br>", input$Variable1, "<br>"," (in pcs)")
    }
    if(input$Variable1=='Sold'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    if(input$Variable1=='Unsold'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    pal <- colorBin(palette = "YlGnBu", domain = map_df$Variable,bins=bins)
    
    my_map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.dark",
      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'),minZoom = 6, maxZoom = 6)) %>%
      addPolygons(color = "#444444",data=wb, weight = 2, fillColor = ~pal(map_df$Variable),fillOpacity = 1, popup=popup,label=map_df$District,highlightOptions = highlightOptions(color = "red", weight = 3,
                                                                                                                                                                                  bringToFront = TRUE),layerId =wb$ID_2)%>% setMaxBounds(85.7, 21, 90, 27.5)%>%setView(88.52783,23.42293,zoom=6) %>%
      addLegend(pal = pal, values = round(map_df$Variable,0), opacity = 0.7, title = legend_head,
                position = "bottomright")
    
    
  })
  data <- reactiveValues(clickedMarker=NULL)
  data_reqd_df_daily <- reactive({
    reqd_df <- subset(abp_merge, CalendarDay>=input$dates[1] & CalendarDay <= input$dates[2])
    abp_map <- reqd_df %>% group_by(District,id) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Indent=mean(Indent),Adhoc=mean(Adhoc),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    abp_map[,-1] <- round(abp_map[,-1],0)
    colnames(abp_map) <- c("District","id","Adhocboost(per 1000)","Wastage(per 1000)","Indent(pcs)","Adhoc(pcs)","AdhocUnsold(pcs)","AdhocSold(pcs)","Unsold(pcs)","Sold(pcs)")
    abp_map
  })
  
  observeEvent(input$Map_cust_shape_click,{
    print("observed map_marker_click")
    data$clickedMarker <- input$Map_cust_shape_click
    print(data$clickedMarker)
    x=subset(data_reqd_df_daily(),id == data$clickedMarker$id)
    updateSelectInput(session, "indistrict1",
                      label = "Select District",
                      choices = c(unique(as.character(x$District)),"All Districts"),
                      selected = unique(as.character(x$District))
    )
    
    
    
    
    
    
  })
  observeEvent(input$indistrict1, {
    
    reqd_df <- subset(abp_merge, CalendarDay>=input$dates[1] & CalendarDay <= input$dates[2])
    abp_map <- reqd_df %>% group_by(District,id) %>% summarise(Adhocboost=  mean(Adhocboost),Wastage=mean(Wastage),Indent=mean(Indent),Adhoc=mean(Adhoc),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    
    abp_map[,-1] <- round(abp_map[,-1],0)
    colnames(abp_map) <- c("District","id","Adhocboost(per 1000)","Wastage(per 1000)","Indent(pcs)","Adhoc(pcs)","AdhocUnsold(pcs)","AdhocSold(pcs)","Unsold(pcs)","Sold(pcs)")
    #abp_map[,-c(1,2)] <- format(abp_map[,-c(1,2)], big.mark = ",", trim = TRUE)
    x=subset(abp_map,District == input$indistrict1)
    if(input$indistrict1=="All Districts"){
      output$mytable_head <- renderText({
        paste("Summary (Average, per day) for West Bengal - for the selected time period")
      })
      output$myTable <- renderTable({
        reqd_df1 <- subset(dly_daily_map, CalendarDay>='2011-05-06' & CalendarDay <= '2011-10-12')
        abp_map1 <- as.data.frame(rbind(mean(reqd_df1$Adhocboost),mean(reqd_df1$Wastage),mean(reqd_df1$Indent),mean(reqd_df1$Adhoc),mean(reqd_df1$AdhocUnsold),mean(reqd_df1$AdhocSold),mean(reqd_df1$Unsold),mean(reqd_df1$Sold)))
        colnames(abp_map1) <- c("Summary")
        rownames(abp_map1) <- c("Adhocboost(per 1000)","Wastage(per 1000)","Indent(pcs)","Adhoc(pcs)","AdhocUnsold(pcs)","AdhocSold(pcs)","Unsold(pcs)","Sold(pcs)")
        abp_map1$Variable <- rownames(abp_map1)
        rownames(abp_map1)<-NULL
        abp_map1<-abp_map1[,c(2,1)]
        abp_map1[,-1] <-round(abp_map1[,-1],0)
        abp_map1[,-1] <- format(abp_map1[,-1],big.mark=",",trim = TRUE)
        colnames(abp_map1) <- c("Variable", "Average Value (per day for WB)")
        abp_map1
      })
      output$error_daily<-renderText({
        paste("Click on a particular district on the map to view the summary.")
      })
    }else{
      output$mytable_head <- renderText({
        paste("Summary (Average, per day) for ",input$indistrict1," for the selected time period in comparison to all the districts of West Bengal")
      })
      output$error_daily<-renderText({
        paste("")
      })
      output$myTable <- renderTable({
        
        y=as.data.frame(t(x))
        y$variable<- colnames(x)
        colnames(y)<-c("Average Value (per day)","Variable")
        rownames(y) <- NULL
        y <- y[-c(1:2),c(2,1)]
        y <- as.data.frame(y)
        class(y$`Average Value (per day)`)
        y$`Average Value (per day)` <- as.numeric(as.character(y$`Average Value (per day)`))
        y[,-1] <- format(y[,-1], big.mark = ",",trim=T)
        reqd_df1 <- subset(dly_daily_map, CalendarDay>='2011-05-06' & CalendarDay <= '2011-10-12')
        abp_map1 <- as.data.frame(rbind(mean(reqd_df1$Adhocboost),mean(reqd_df1$Wastage),mean(reqd_df1$Indent),mean(reqd_df1$Adhoc),mean(reqd_df1$AdhocUnsold),mean(reqd_df1$AdhocSold),mean(reqd_df1$Unsold),mean(reqd_df1$Sold)))
        colnames(abp_map1) <- c("Summary")
        rownames(abp_map1) <- c("Adhocboost(per 1000)","Wastage(per 1000)","Indent(pcs)","Adhoc(pcs)","AdhocUnsold(pcs)","AdhocSold(pcs)","Unsold(pcs)","Sold(pcs)")
        abp_map1$Variable <- rownames(abp_map1)
        rownames(abp_map1)<-NULL
        abp_map1<-abp_map1[,c(2,1)]
        abp_map1[,-1] <-round(abp_map1[,-1],0)
        abp_map1[,-1] <- format(abp_map1[,-1],big.mark=",",trim = TRUE)
        colnames(abp_map1) <- c("Variable", "Average Value (per day for WB)")
        final_summ <- merge(y,abp_map1,by="Variable",all=TRUE)
        final_summ
        
      })
    }
    
    
  })
  
  abp_merge1<-merge(abp_wk,wb1,by="District",all=T)
  
  output$Map_cust1 <- renderLeaflet({
    
    wb_dist<-cbind(wb$NAME_2,wb$ID_2)
    wb_dist<-as.data.frame(wb_dist)
    colnames(wb_dist) <- c("District","Code")
    abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1] & CalendarDay <= input$dates[2])
    abp_wkk <- abp_daily %>% group_by(year_wk,year,wk,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    abp_wkk[,-c(1,2,3,4)] <- round(abp_wkk[,-c(1,2,3,4)],0)
    abp_wkk$year_wk<-as.character(abp_wkk$year_wk)
    abp_wkk$wk<-as.character(abp_wkk$wk)
    abp_wk1 <- abp_wkk
    abp_wk1$District <- as.character(abp_wk1$District)
    abp_wk1 <- within(abp_wk1, District[District == "North_24_Parganas"] <- 'North 24 Parganas')
    abp_wk1 <- within(abp_wk1, District[District == 'South_24_Parganas'] <- 'South 24 Parganas')
    abp_wk1 <- within(abp_wk1, District[District == 'Koch_Bihar'] <- 'Koch Bihar')
    abp_wk1 <- within(abp_wk1, District[District == 'Paschim_Medinipur'] <- 'Pashchim Medinipur')
    abp_wk1 <- within(abp_wk1, District[District == 'Dakshin_Dinajpur'] <- 'Dakshin Dinajpur')
    abp_wk1 <- within(abp_wk1, District[District == 'Uttar_Dinajpur'] <- 'Uttar Dinajpur')
    abp_wk1 <- within(abp_wk1, District[District == 'Purba_Medinipur'] <- 'Purba Medinipur')
    #abp1 <- merge(abp_daily1,wb_dist,all=TRUE)
    #ABP_year <- abp1[(abp1$CalendarDay==input$dates),]
    # abp_wk1 <-subset (abp_wk1,year==input$yr1)
    # reqd_df <- subset(abp_wk1, wk>=input$wk1 & wk<= input$wk2)
    reqd_df <- abp_wk1
    abp_map <- reqd_df %>% group_by(District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Indent=mean(Indent),Adhoc=mean(Adhoc),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    abp_map[,-1] <- round(abp_map[,-1],0)
    abp_map1 <- merge(abp_map,wb_dist,all=TRUE)
    map_df <- abp_map1[,c("District",input$Variable1)]
    colnames(map_df) <- c("District","Variable")
    gap <- (max(map_df$Variable,na.rm = TRUE)-min(map_df$Variable, na.rm = TRUE))/3
    v2 <- min(map_df$Variable, na.rm = TRUE)
    v2 <- as.numeric(v2)
    v4 <- v2+gap
    v4 <- as.numeric(v4)
    v5 <- v2+(2*gap)
    v5 <- as.numeric(v5)
    v6 <- max(map_df$Variable, na.rm = TRUE)
    v6 <- as.numeric(v6)
    c3 <- cut(map_df$Variable,breaks=c(v2,v4,v5,v6),include.lowest=T)
    summary(c3)
    c2<-sub(".*\\[ *(.*?) *,.*", "\\1", levels(c3)[1])
    c2<-round(as.numeric(c2),0)
    c4<-sub(".*, *(.*?) *].*", "\\1", levels(c3)[1])
    c4<-round(as.numeric(c4),0)
    c5<-sub(".*, *(.*?) *].*", "\\1", levels(c3)[2])
    c5<-round(as.numeric(c5),0)
    c6<-sub(".*, *(.*?) *].*", "\\1", levels(c3)[3])
    c6<-round(as.numeric(c6),0)
    c7<-sub(".*, *(.*?) *].*", "\\1", levels(c3)[4])
    c7<-round(as.numeric(c7),0)
    bins <- c(c2,c4,c5,c6)
    if(input$Variable1=='Adhocboost'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"(per 1000 Indent)")
    }
    if(input$Variable1=='AdhocUnsold'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='Wastage'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"(per 1000 Sold)")
    }
    if(input$Variable1=='Adhoc'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='AdhocSold'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='Indent'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='Sold'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='Unsold'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    ##############################################
    if(input$Variable1=='Adhocboost'){
      legend_head <- paste("Average", ":","<br>" ,input$Variable1,"<br>", " (per 1000 Indent)")
    }
    if(input$Variable1=='AdhocUnsold'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    if(input$Variable1=='Wastage'){
      legend_head <- paste("Average",":","<br>", input$Variable1,"<br>"," (per 1000 Sold)")
    }
    if(input$Variable1=='Adhoc'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    if(input$Variable1=='AdhocSold'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    if(input$Variable1=='Indent'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>", " (in pcs)")
    }
    if(input$Variable1=='Sold'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    if(input$Variable1=='Unsold'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    pal <- colorBin(palette = "YlGnBu", domain = map_df$Variable,bins=bins)
    
    my_map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.dark",
      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'),minZoom = 6, maxZoom = 6)) %>%
      addPolygons(color = "#444444",data=wb, weight = 2, fillColor = ~pal(map_df$Variable),fillOpacity = 1, popup=popup, label=map_df$District,highlightOptions = highlightOptions(color = "red", weight = 3,
                                                                                                                                                                                   bringToFront = TRUE),layerId =wb$ID_2)%>% setMaxBounds(85.7, 21, 90, 27.5)%>%setView(88.52783,23.42293,zoom=6) %>%
      addLegend(pal = pal, values = round(map_df$Variable,0), opacity = 0.7, title = legend_head,
                position = "bottomright")
    
    
  })
  data1 <- reactiveValues(clickedMarker=NULL)
  data_reqd_df_weekly <- reactive({
    reqd_df <- subset(abp_merge, CalendarDay>=input$dates[1] & CalendarDay <= input$dates[2])
    abp_map_weekly <- reqd_df %>% group_by(year_wk,year,wk,District,id) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    abp_map_weekly[,-c(1,2,3,4)] <- round(abp_map_weekly[,-c(1,2,3,4)],0)
    abp_map_weekly$year_wk<-as.character(abp_map_weekly$year_wk)
    abp_map_weekly$wk<-as.character(abp_map_weekly$wk)
    
    abp_map <- abp_map_weekly %>% group_by(District,id) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Indent=mean(Indent),Adhoc=mean(Adhoc),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    abp_map[,-1] <- round(abp_map[,-1],0)
    colnames(abp_map) <- c("District","id","Adhocboost(per 1000)","Wastage(per 1000)","Indent(pcs)","Adhoc(pcs)","AdhocUnsold(pcs)","AdhocSold(pcs)","Unsold(pcs)","Sold(pcs)")
    #abp_map[,-c(1,2)] <- format(abp_map[,-c(1,2)], big.mark = ",", trim = TRUE)
    abp_map
  })
  
  observeEvent(input$Map_cust1_shape_click,{
    print("observed map_marker_click")
    data1$clickedMarker <- input$Map_cust1_shape_click
    print(data1$clickedMarker)
    
    x=subset(data_reqd_df_weekly(),id == data1$clickedMarker$id)
    updateSelectInput(session, "indistrict1",
                      label = "Select District",
                      choices = c(unique(as.character(x$District)),"All Districts"),
                      selected = unique(as.character(x$District))
    )
    
    
  })
  
  observeEvent(input$indistrict1, {
    x=subset(data_reqd_df_weekly(),District == input$indistrict1)
    if(input$indistrict1=="All Districts"){
      output$mytable1_head <- renderText({
        paste("Summary (Average, per day) for West Bengal - for the selected week range")
      })
      output$myTable1 <- renderTable({
        abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1] & CalendarDay <= input$dates[2])
        
        abp_wk <- abp_daily %>% group_by(year_wk,year,wk,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
        abp_wk[,-c(1,2,3,4)] <- round(abp_wk[,-c(1,2,3,4)],0)
        abp_wk$year_wk<-as.character(abp_wk$year_wk)
        abp_wk$wk<-as.character(abp_wk$wk)
        
        wk_week_map <- abp_wk %>% group_by(year_wk,year,wk) %>% summarise(Adhocboost= sum(Adhocboost),Wastage=sum(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
        wk_week_map[,-c(1,2,3)] <- round(wk_week_map[,-c(1,2,3)],0)
        
        abp_wk1 <- wk_week_map
        reqd_df <- abp_wk1
        
        abp_map1 <- as.data.frame(rbind(mean(reqd_df$Adhocboost),mean(reqd_df$Wastage),mean(reqd_df$Indent),mean(reqd_df$Adhoc),mean(reqd_df$AdhocUnsold),mean(reqd_df$AdhocSold),mean(reqd_df$Unsold),mean(reqd_df$Sold)))
        colnames(abp_map1) <- c("Summary")
        rownames(abp_map1) <- c("Adhocboost(per 1000)","Wastage(per 1000)","Indent(pcs)","Adhoc(pcs)","AdhocUnsold(pcs)","AdhocSold(pcs)","Unsold(pcs)","Sold(pcs)")
        abp_map1$Variable <- rownames(abp_map1)
        rownames(abp_map1)<-NULL
        abp_map1<-abp_map1[,c(2,1)]
        abp_map1[,-1] <-round(abp_map1[,-1],0)
        abp_map1[,-1] <- format(abp_map1[,-1],big.mark=",",trim = TRUE)
        colnames(abp_map1) <- c("Variable", "Average Value (per day for WB)")
        abp_map1
      })
      output$error_daily1<-renderText({
        paste("Click on a particular district on the map to view the summary.")
      })
    }else{
      output$mytable1_head <- renderText({
        paste("Summary (Average, per day) for ",input$indistrict1," for the selected week range in comparison to all the districts of West Bengal")
      })
      output$error_daily1 <-renderText({
        paste("")
      })
      output$myTable1 <- renderTable({
        
        y=as.data.frame(t(x))
        y$variable<- colnames(x)
        colnames(y)<-c("Average Value (per day)","Variable")
        rownames(y) <- NULL
        y <- y[-c(1:2),c(2,1)]
        y <- as.data.frame(y)
        class(y$`Average Value (per day)`)
        y$`Average Value (per day)` <- as.numeric(as.character(y$`Average Value (per day)`))
        y[,-1] <- format(y[,-1], big.mark = ",",trim=T)
        abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1] & CalendarDay <= input$dates[2])
        
        abp_wk <- abp_daily %>% group_by(year_wk,year,wk,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
        abp_wk[,-c(1,2,3,4)] <- round(abp_wk[,-c(1,2,3,4)],0)
        abp_wk$year_wk<-as.character(abp_wk$year_wk)
        abp_wk$wk<-as.character(abp_wk$wk)
        
        wk_week_map <- abp_wk %>% group_by(year_wk,year,wk) %>% summarise(Adhocboost= sum(Adhocboost),Wastage=sum(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
        wk_week_map[,-c(1,2,3)] <- round(wk_week_map[,-c(1,2,3)],0)
        
        abp_wk1 <- wk_week_map
        reqd_df <- abp_wk1
        
        abp_map1 <- as.data.frame(rbind(mean(reqd_df$Adhocboost),mean(reqd_df$Wastage),mean(reqd_df$Indent),mean(reqd_df$Adhoc),mean(reqd_df$AdhocUnsold),mean(reqd_df$AdhocSold),mean(reqd_df$Unsold),mean(reqd_df$Sold)))
        colnames(abp_map1) <- c("Summary")
        rownames(abp_map1) <- c("Adhocboost(per 1000)","Wastage(per 1000)","Indent(pcs)","Adhoc(pcs)","AdhocUnsold(pcs)","AdhocSold(pcs)","Unsold(pcs)","Sold(pcs)")
        abp_map1$Variable <- rownames(abp_map1)
        rownames(abp_map1)<-NULL
        abp_map1<-abp_map1[,c(2,1)]
        abp_map1[,-1] <-round(abp_map1[,-1],0)
        abp_map1[,-1] <- format(abp_map1[,-1],big.mark=",",trim = TRUE)
        colnames(abp_map1) <- c("Variable", "Average Value (per day for WB)")
        final_week_summ <- merge(y,abp_map1,by="Variable", all=TRUE)
        
        
      })
    }
    
    
  })
  onclick("dates",updateSelectInput(session, "indistrict1",
                                    label = "Select District",
                                    choices ="All Districts",
                                    selected = "All Districts"
  )
  )
  
  
  onclick("wk2",updateSelectInput(session, "indistrict1",
                                  label = "Select District",
                                  choices ="All Districts",
                                  selected = "All Districts"
  )
  )
  onclick("mon2",updateSelectInput(session, "indistrict1",
                                   label = "Select District",
                                   choices ="All Districts",
                                   selected = "All Districts"
  )
  )
  onclick("qtr2",updateSelectInput(session, "indistrict1",
                                   label = "Select District",
                                   choices ="All Districts",
                                   selected = "All Districts"
  )
  )
  onclick("yr2",updateSelectInput(session, "indistrict1",
                                  label = "Select District",
                                  choices ="All Districts",
                                  selected = "All Districts"
  )
  )
  
  observeEvent(input$Map_cust$click,
               {input$Map_cust1_shape_click=NULL
               
               })
  
  
  abp_merge2<-merge(abp_mon,wb1,by="District",all=T)
  
  output$Map_cust2 <- renderLeaflet({
    
    wb_dist<-cbind(wb$NAME_2,wb$ID_2)
    wb_dist<-as.data.frame(wb_dist)
    colnames(wb_dist) <- c("District","Code")
    abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1] & CalendarDay <= input$dates[2])
    abp_monn <- abp_daily %>% group_by(year_mon,year,mon,monthly,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    abp_monn[,-c(1,2,3,4,5)] <- round(abp_monn[,-c(1,2,3,4,5)],0)
    abp_mon1 <- abp_monn
    abp_mon1$District <- as.character(abp_mon1$District)
    abp_mon1 <- within(abp_mon1, District[District == "North_24_Parganas"] <- 'North 24 Parganas')
    abp_mon1 <- within(abp_mon1, District[District == 'South_24_Parganas'] <- 'South 24 Parganas')
    abp_mon1 <- within(abp_mon1, District[District == 'Koch_Bihar'] <- 'Koch Bihar')
    abp_mon1 <- within(abp_mon1, District[District == 'Paschim_Medinipur'] <- 'Pashchim Medinipur')
    abp_mon1 <- within(abp_mon1, District[District == 'Dakshin_Dinajpur'] <- 'Dakshin Dinajpur')
    abp_mon1 <- within(abp_mon1, District[District == 'Uttar_Dinajpur'] <- 'Uttar Dinajpur')
    abp_mon1 <- within(abp_mon1, District[District == 'Purba_Medinipur'] <- 'Purba Medinipur')
    #abp1 <- merge(abp_daily1,wb_dist,all=TRUE)
    #ABP_year <- abp1[(abp1$CalendarDay==input$dates),]
    
    reqd_df <- abp_mon1
    abp_map <- reqd_df %>% group_by(District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Indent=mean(Indent),Adhoc=mean(Adhoc),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    abp_map[,-1] <- round(abp_map[,-1],0)
    abp_map1 <- merge(abp_map,wb_dist,all=TRUE)
    map_df <- abp_map1[,c("District",input$Variable1)]
    colnames(map_df) <- c("District","Variable")
    gap <- (max(map_df$Variable,na.rm = TRUE)-min(map_df$Variable, na.rm = TRUE))/3
    v2 <- min(map_df$Variable, na.rm = TRUE)
    v2 <- as.numeric(v2)
    v4 <- v2+gap
    v4 <- as.numeric(v4)
    v5 <- v2+(2*gap)
    v5 <- as.numeric(v5)
    v6 <- max(map_df$Variable, na.rm = TRUE)
    v6 <- as.numeric(v6)
    c3 <- cut(map_df$Variable,breaks=c(v2,v4,v5,v6),include.lowest=T)
    summary(c3)
    c2<-sub(".*\\[ *(.*?) *,.*", "\\1", levels(c3)[1])
    c2<-round(as.numeric(c2),0)
    c4<-sub(".*, *(.*?) *].*", "\\1", levels(c3)[1])
    c4<-round(as.numeric(c4),0)
    c5<-sub(".*, *(.*?) *].*", "\\1", levels(c3)[2])
    c5<-round(as.numeric(c5),0)
    c6<-sub(".*, *(.*?) *].*", "\\1", levels(c3)[3])
    c6<-round(as.numeric(c6),0)
    c7<-sub(".*, *(.*?) *].*", "\\1", levels(c3)[4])
    c7<-round(as.numeric(c7),0)
    bins <- c(c2,c4,c5,c6)
    if(input$Variable1=='Adhocboost'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"(per 1000 Indent)")
    }
    if(input$Variable1=='AdhocUnsold'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='Wastage'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"(per 1000 Sold)")
    }
    if(input$Variable1=='Adhoc'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='AdhocSold'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='Indent'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='Sold'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='Unsold'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    ##############################################
    if(input$Variable1=='Adhocboost'){
      legend_head <- paste("Average", ":","<br>" ,input$Variable1, "<br>"," (per 1000 Indent)")
    }
    if(input$Variable1=='AdhocUnsold'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    if(input$Variable1=='Wastage'){
      legend_head <- paste("Average",":","<br>", input$Variable1,"<br>"," (per 1000 Sold)")
    }
    if(input$Variable1=='Adhoc'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    if(input$Variable1=='AdhocSold'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    if(input$Variable1=='Indent'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>", " (in pcs)")
    }
    if(input$Variable1=='Sold'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    if(input$Variable1=='Unsold'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    pal <- colorBin(palette = "YlGnBu", domain = map_df$Variable,bins=bins)
    
    my_map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.dark",
      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'),minZoom = 6, maxZoom = 6)) %>%
      addPolygons(color = "#444444",data=wb, weight = 2, fillColor = ~pal(map_df$Variable),fillOpacity = 1, popup=popup,label=map_df$District, highlightOptions = highlightOptions(color = "red", weight = 3,
                                                                                                                                                                                   bringToFront = TRUE),layerId =wb$ID_2)%>% setMaxBounds(85.7, 21, 90, 27.5)%>%setView(88.52783,23.42293,zoom=6) %>%
      addLegend(pal = pal, values = round(map_df$Variable,0), opacity = 0.7, title = legend_head,
                position = "bottomright")
    
    
  })
  data2 <- reactiveValues(clickedMarker=NULL)
  
  
  data_reqd_df_monthly <- reactive({
    reqd_df <- subset(abp_merge, CalendarDay>=input$dates[1] & CalendarDay <= input$dates[2])
    abp_map_monthly <- reqd_df %>% group_by(year_mon,year,mon,monthly,District,id) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    abp_map_monthly[,-c(1,2,3,4,5)] <- round(abp_map_monthly[,-c(1,2,3,4,5)],0)
    
    abp_map <- abp_map_monthly %>% group_by(District,id) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Indent=mean(Indent),Adhoc=mean(Adhoc),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    abp_map[,-1] <- round(abp_map[,-1],0)
    colnames(abp_map) <- c("District","id","Adhocboost(per 1000)","Wastage(per 1000)","Indent(pcs)","Adhoc(pcs)","AdhocUnsold(pcs)","AdhocSold(pcs)","Unsold(pcs)","Sold(pcs)")
    #abp_map[,-c(1,2)] <- format(abp_map[,-c(1,2)], big.mark = ",", trim = TRUE)
    abp_map
  })
  
  observeEvent(input$Map_cust2_shape_click,{
    print("observed map_marker_click")
    data2$clickedMarker <- input$Map_cust2_shape_click
    print(data2$clickedMarker)
    
    x=subset(data_reqd_df_monthly(),id == data2$clickedMarker$id)
    updateSelectInput(session, "indistrict1",
                      label = "Select District",
                      choices = c(unique(as.character(x$District)),"All Districts"),
                      selected = unique(as.character(x$District))
    )
    
  })
  observeEvent(input$indistrict1, {
    x=subset(data_reqd_df_monthly(),District == input$indistrict1)
    if(input$indistrict1=="All Districts"){
      output$mytable2_head <- renderText({
        paste("Summary (Average, per day) for West Bengal - for the selected month range")
      })
      output$myTable2 <- renderTable({
        abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1]&CalendarDay<=input$dates[2])
        
        abp_mon <- abp_daily %>% group_by(year_mon,year,mon,monthly,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
        abp_mon[,-c(1,2,3,4,5)] <- round(abp_mon[,-c(1,2,3,4,5)],0)
        
        mon_month_map <- abp_mon %>% group_by(year_mon,year,mon,monthly) %>% summarise(Adhocboost= sum(Adhocboost),Wastage=sum(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
        mon_month_map[,-c(1,2,3,4)] <- round(mon_month_map[,-c(1,2,3,4)],0)
        
        abp_mon1 <- mon_month_map
        
        reqd_df <- abp_mon1
        
        abp_map1 <- as.data.frame(rbind(mean(reqd_df$Adhocboost,na.rm=TRUE),mean(reqd_df$Wastage,na.rm=TRUE),mean(reqd_df$Indent,na.rm=TRUE),mean(reqd_df$Adhoc,na.rm=TRUE),mean(reqd_df$AdhocUnsold,na.rm=TRUE),mean(reqd_df$AdhocSold,na.rm=TRUE),mean(reqd_df$Unsold,na.rm=TRUE),mean(reqd_df$Sold,na.rm=TRUE)))
        colnames(abp_map1) <- c("Summary")
        rownames(abp_map1) <- c("Adhocboost(per 1000)","Wastage(per 1000)","Indent(pcs)","Adhoc(pcs)","AdhocUnsold(pcs)","AdhocSold(pcs)","Unsold(pcs)","Sold(pcs)")
        abp_map1$Variable <- rownames(abp_map1)
        rownames(abp_map1)<-NULL
        abp_map1<-abp_map1[,c(2,1)]
        abp_map1[,-1] <-round(abp_map1[,-1],0)
        abp_map1[,-1] <- format(abp_map1[,-1],big.mark=",",trim = TRUE)
        colnames(abp_map1) <- c("Variable", "Average Value (per day for WB)")
        abp_map1
      })
      output$error_daily2<-renderText({
        paste("Click on a particular district on the map to view the summary.")
      })
      
    }else{
      output$mytable2_head <- renderText({
        paste("Summary (Average, per day) for ",input$indistrict1," for the selected month range in comparison to all districts of West Bengal")
      })
      output$error_daily2<-renderText({
        paste("")
      })
      output$myTable2 <- renderTable({
        
        y=as.data.frame(t(x))
        y$variable<- colnames(x)
        colnames(y)<-c("Average Value (per day)","Variable")
        rownames(y) <- NULL
        y <- y[-c(1:2),c(2,1)]
        y <- as.data.frame(y)
        class(y$`Average Value (per day)`)
        y$`Average Value (per day)` <- as.numeric(as.character(y$`Average Value (per day)`))
        y[,-1] <- format(y[,-1], big.mark = ",",trim=T)
        abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1]&CalendarDay<=input$dates[2])
        
        abp_mon <- abp_daily %>% group_by(year_mon,year,mon,monthly,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
        abp_mon[,-c(1,2,3,4,5)] <- round(abp_mon[,-c(1,2,3,4,5)],0)
        
        mon_month_map <- abp_mon %>% group_by(year_mon,year,mon,monthly) %>% summarise(Adhocboost= sum(Adhocboost),Wastage=sum(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
        mon_month_map[,-c(1,2,3,4)] <- round(mon_month_map[,-c(1,2,3,4)],0)
        
        abp_mon1 <- mon_month_map
        
        reqd_df <- abp_mon1
        
        abp_map1 <- as.data.frame(rbind(mean(reqd_df$Adhocboost,na.rm=TRUE),mean(reqd_df$Wastage,na.rm=TRUE),mean(reqd_df$Indent,na.rm=TRUE),mean(reqd_df$Adhoc,na.rm=TRUE),mean(reqd_df$AdhocUnsold,na.rm=TRUE),mean(reqd_df$AdhocSold,na.rm=TRUE),mean(reqd_df$Unsold,na.rm=TRUE),mean(reqd_df$Sold,na.rm=TRUE)))
        colnames(abp_map1) <- c("Summary")
        rownames(abp_map1) <- c("Adhocboost(per 1000)","Wastage(per 1000)","Indent(pcs)","Adhoc(pcs)","AdhocUnsold(pcs)","AdhocSold(pcs)","Unsold(pcs)","Sold(pcs)")
        abp_map1$Variable <- rownames(abp_map1)
        rownames(abp_map1)<-NULL
        abp_map1<-abp_map1[,c(2,1)]
        abp_map1[,-1] <-round(abp_map1[,-1],0)
        abp_map1[,-1] <- format(abp_map1[,-1],big.mark=",",trim = TRUE)
        colnames(abp_map1) <- c("Variable", "Average Value (per day for WB)")
        final_month_summ <- merge(y,abp_map1,by="Variable",all=TRUE)
        final_month_summ
        
        
      })
      
    }
    
  })
  abp_merge3 <-merge(abp_qtr,wb1,by="District",all=T)
  
  output$Map_cust3 <- renderLeaflet({
    
    wb_dist<-cbind(wb$NAME_2,wb$ID_2)
    wb_dist<-as.data.frame(wb_dist)
    colnames(wb_dist) <- c("District","Code")
    abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1] & CalendarDay <= input$dates[2])
    
    abp_qtrr <- abp_daily %>% group_by(year_qtr,year,qtr,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    
    abp_qtrr[,-c(1,2,3,4)] <- round(abp_qtrr[,-c(1,2,3,4)],0)
    abp_qtrr$year_qtr<-as.character(abp_qtrr$year_qtr)
    abp_qtrr$year<-as.character(abp_qtrr$year)
    abp_qtrr$qtr<-as.character(abp_qtrr$qtr)
    abp_qtr1 <- abp_qtrr
    abp_qtr1$District <- as.character(abp_qtr1$District)
    abp_qtr1 <- within(abp_qtr1, District[District == "North_24_Parganas"] <- 'North 24 Parganas')
    abp_qtr1 <- within(abp_qtr1, District[District == 'South_24_Parganas'] <- 'South 24 Parganas')
    abp_qtr1 <- within(abp_qtr1, District[District == 'Koch_Bihar'] <- 'Koch Bihar')
    abp_qtr1 <- within(abp_qtr1, District[District == 'Paschim_Medinipur'] <- 'Pashchim Medinipur')
    abp_qtr1 <- within(abp_qtr1, District[District == 'Dakshin_Dinajpur'] <- 'Dakshin Dinajpur')
    abp_qtr1 <- within(abp_qtr1, District[District == 'Uttar_Dinajpur'] <- 'Uttar Dinajpur')
    abp_qtr1 <- within(abp_qtr1, District[District == 'Purba_Medinipur'] <- 'Purba Medinipur')
    #abp1 <- merge(abp_daily1,wb_dist,all=TRUE)
    #ABP_year <- abp1[(abp1$CalendarDay==input$dates),]
    reqd_df <- abp_qtr1
    abp_map <- reqd_df %>% group_by(District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Indent=mean(Indent),Adhoc=mean(Adhoc),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    abp_map[,-1] <- round(abp_map[,-1],0)
    abp_map1 <- merge(abp_map,wb_dist,all=TRUE)
    map_df <- abp_map1[,c("District",input$Variable1)]
    colnames(map_df) <- c("District","Variable")
    gap <- (max(map_df$Variable,na.rm = TRUE)-min(map_df$Variable, na.rm = TRUE))/3
    v2 <- min(map_df$Variable, na.rm = TRUE)
    v2 <- as.numeric(v2)
    v4 <- v2+gap
    v4 <- as.numeric(v4)
    v5 <- v2+(2*gap)
    v5 <- as.numeric(v5)
    v6 <- max(map_df$Variable, na.rm = TRUE)
    v6 <- as.numeric(v6)
    c3 <- cut(map_df$Variable,breaks=c(v2,v4,v5,v6),include.lowest=T)
    summary(c3)
    c2<-sub(".*\\[ *(.*?) *,.*", "\\1", levels(c3)[1])
    c2<-round(as.numeric(c2),0)
    c4<-sub(".*, *(.*?) *].*", "\\1", levels(c3)[1])
    c4<-round(as.numeric(c4),0)
    c5<-sub(".*, *(.*?) *].*", "\\1", levels(c3)[2])
    c5<-round(as.numeric(c5),0)
    c6<-sub(".*, *(.*?) *].*", "\\1", levels(c3)[3])
    c6<-round(as.numeric(c6),0)
    c7<-sub(".*, *(.*?) *].*", "\\1", levels(c3)[4])
    c7<-round(as.numeric(c7),0)
    bins <- c(c2,c4,c5,c6)
    if(input$Variable1=='Adhocboost'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"(per 1000 Indent)")
    }
    if(input$Variable1=='AdhocUnsold'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='Wastage'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"(per 1000 Sold)")
    }
    if(input$Variable1=='Adhoc'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='AdhocSold'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='Indent'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='Sold'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='Unsold'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    ##############################################
    if(input$Variable1=='Adhocboost'){
      legend_head <- paste("Average", ":","<br>" ,input$Variable1, "<br>"," (per 1000 Indent)")
    }
    if(input$Variable1=='AdhocUnsold'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    if(input$Variable1=='Wastage'){
      legend_head <- paste("Average",":","<br>", input$Variable1,"<br>"," (per 1000 Sold)")
    }
    if(input$Variable1=='Adhoc'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    if(input$Variable1=='AdhocSold'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    if(input$Variable1=='Indent'){
      legend_head <- paste("Average", ":","<br>", input$Variable1, "<br>"," (in pcs)")
    }
    if(input$Variable1=='Sold'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    if(input$Variable1=='Unsold'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    pal <- colorBin(palette = "YlGnBu", domain = map_df$Variable,bins=bins)
    
    my_map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.dark",
      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'),minZoom = 6, maxZoom = 6)) %>%
      addPolygons(color = "#444444",data=wb, weight = 2, fillColor = ~pal(map_df$Variable),fillOpacity = 1, popup=popup,label=map_df$District, highlightOptions = highlightOptions(color = "red", weight = 3,
                                                                                                                                                                                   bringToFront = TRUE),layerId =wb$ID_2)%>% setMaxBounds(85.7, 21, 90, 27.5)%>%setView(88.52783,23.42293,zoom=6) %>%
      addLegend(pal = pal, values = round(map_df$Variable,0), opacity = 0.7, title = legend_head,
                position = "bottomright")
    
    
  })
  data3 <- reactiveValues(clickedMarker=NULL)
  
  data_reqd_df_quarterly <- reactive({
    reqd_df <- subset(abp_merge, CalendarDay>=input$dates[1] & CalendarDay <= input$dates[2])
    abp_map_quarterly <- reqd_df %>% group_by(year_qtr,year,qtr,District,id) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    abp_map_quarterly[,-c(1,2,3,4)] <- round(abp_map_quarterly[,-c(1,2,3,4)],0)
    abp_map_quarterly$year_qtr<-as.character(abp_map_quarterly$year_qtr)
    abp_map_quarterly$year<-as.character(abp_map_quarterly$year)
    abp_map_quarterly$qtr<-as.character(abp_map_quarterly$qtr)
    
    abp_map <- abp_map_quarterly %>% group_by(District,id) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Indent=mean(Indent),Adhoc=mean(Adhoc),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    abp_map[,-1] <- round(abp_map[,-1],0)
    colnames(abp_map) <- c("District","id","Adhocboost(per 1000)","Wastage(per 1000)","Indent(pcs)","Adhoc(pcs)","AdhocUnsold(pcs)","AdhocSold(pcs)","Unsold(pcs)","Sold(pcs)")
    #abp_map[,-c(1,2)] <- format(abp_map[,-c(1,2)], big.mark = ",", trim = TRUE)
    abp_map
  })
  
  observeEvent(input$Map_cust3_shape_click,{
    print("observed map_marker_click")
    data3$clickedMarker <- input$Map_cust3_shape_click
    print(data3$clickedMarker)
    
    x=subset(data_reqd_df_quarterly(),id == data3$clickedMarker$id)
    updateSelectInput(session, "indistrict1",
                      label = "Select District",
                      choices = c(unique(as.character(x$District)),"All Districts"),
                      selected = unique(as.character(x$District))
    )
    
  })
  observeEvent(input$indistrict1, {
    x=subset(data_reqd_df_quarterly(),District == input$indistrict1)
    if(input$indistrict1=="All Districts"){
      output$mytable3_head <- renderText({
        paste("Summary (Average, per day) for West Bengal - for the selected quarter range")
      })
      output$myTable3 <- renderTable({
        abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1]& CalendarDay <=input$dates[2])
        abp_qtr <- abp_daily %>% group_by(year_qtr,year,qtr,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
        
        abp_qtr[,-c(1,2,3,4)] <- round(abp_qtr[,-c(1,2,3,4)],0)
        abp_qtr$year_qtr<-as.character(abp_qtr$year_qtr)
        abp_qtr$year<-as.character(abp_qtr$year)
        abp_qtr$qtr<-as.character(abp_qtr$qtr)
        
        qtr_quarter_map <- abp_qtr %>% group_by(year_qtr,year,qtr) %>% summarise(Adhocboost= sum(Adhocboost),Wastage=sum(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
        qtr_quarter_map[,-c(1,2,3)] <- round(qtr_quarter_map[,-c(1,2,3)],0)
        abp_qtr1 <- qtr_quarter_map
        
        reqd_df <- abp_qtr1
        abp_map1 <- as.data.frame(rbind(mean(reqd_df$Adhocboost,na.rm=TRUE),mean(reqd_df$Wastage,na.rm=TRUE),mean(reqd_df$Indent,na.rm=TRUE),mean(reqd_df$Adhoc,na.rm=TRUE),mean(reqd_df$AdhocUnsold,na.rm=TRUE),mean(reqd_df$AdhocSold,na.rm=TRUE),mean(reqd_df$Unsold,na.rm=TRUE),mean(reqd_df$Sold,na.rm=TRUE)))
        colnames(abp_map1) <- c("Summary")
        rownames(abp_map1) <- c("Adhocboost(per 1000)","Wastage(per 1000)","Indent(pcs)","Adhoc(pcs)","AdhocUnsold(pcs)","AdhocSold(pcs)","Unsold(pcs)","Sold(pcs)")
        abp_map1$Variable <- rownames(abp_map1)
        rownames(abp_map1)<-NULL
        abp_map1<-abp_map1[,c(2,1)]
        abp_map1[,-1] <-round(abp_map1[,-1],0)
        abp_map1[,-1] <- format(abp_map1[,-1],big.mark=",",trim = TRUE)
        colnames(abp_map1) <- c("Variable", "Average Value (per day for WB)")
        abp_map1
      })
      output$error_daily3<-renderText({
        paste("Click on a particular district on the map to view the summary.")
      })
    }else{
      output$mytable3_head <- renderText({
        paste("Summary (Average, per day) for ",input$indistrict1," for the selected quarter range in comparison to all the districts of West Bengal")
      })
      output$error_daily3<-renderText({
        paste("")
      })
      output$myTable3 <- renderTable({
        
        y=as.data.frame(t(x))
        y$variable<- colnames(x)
        colnames(y)<-c("Average Value (per day)","Variable")
        rownames(y) <- NULL
        y <- y[-c(1:2),c(2,1)]
        y <- as.data.frame(y)
        class(y$`Average Value (per day)`)
        y$`Average Value (per day)` <- as.numeric(as.character(y$`Average Value (per day)`))
        y[,-1] <- format(y[,-1], big.mark = ",",trim=T)
        abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1]& CalendarDay <=input$dates[2])
        abp_qtr <- abp_daily %>% group_by(year_qtr,year,qtr,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
        
        abp_qtr[,-c(1,2,3,4)] <- round(abp_qtr[,-c(1,2,3,4)],0)
        abp_qtr$year_qtr<-as.character(abp_qtr$year_qtr)
        abp_qtr$year<-as.character(abp_qtr$year)
        abp_qtr$qtr<-as.character(abp_qtr$qtr)
        
        qtr_quarter_map <- abp_qtr %>% group_by(year_qtr,year,qtr) %>% summarise(Adhocboost= sum(Adhocboost),Wastage=sum(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
        qtr_quarter_map[,-c(1,2,3)] <- round(qtr_quarter_map[,-c(1,2,3)],0)
        abp_qtr1 <- qtr_quarter_map
        
        reqd_df <- abp_qtr1
        abp_map1 <- as.data.frame(rbind(mean(reqd_df$Adhocboost,na.rm=TRUE),mean(reqd_df$Wastage,na.rm=TRUE),mean(reqd_df$Indent,na.rm=TRUE),mean(reqd_df$Adhoc,na.rm=TRUE),mean(reqd_df$AdhocUnsold,na.rm=TRUE),mean(reqd_df$AdhocSold,na.rm=TRUE),mean(reqd_df$Unsold,na.rm=TRUE),mean(reqd_df$Sold,na.rm=TRUE)))
        colnames(abp_map1) <- c("Summary")
        rownames(abp_map1) <- c("Adhocboost(per 1000)","Wastage(per 1000)","Indent(pcs)","Adhoc(pcs)","AdhocUnsold(pcs)","AdhocSold(pcs)","Unsold(pcs)","Sold(pcs)")
        abp_map1$Variable <- rownames(abp_map1)
        rownames(abp_map1)<-NULL
        abp_map1<-abp_map1[,c(2,1)]
        abp_map1[,-1] <-round(abp_map1[,-1],0)
        abp_map1[,-1] <- format(abp_map1[,-1],big.mark=",",trim = TRUE)
        colnames(abp_map1) <- c("Variable", "Average Value (per day for WB)")
        final_qtr_summ <- merge(y,abp_map1,by="Variable",all=TRUE)
        final_qtr_summ
        
        
      })
      
    }
    
    
  })
  
  abp_merge4<-merge(abp_yr,wb1,by="District",all=T)
  
  output$Map_cust4 <- renderLeaflet({
    
    wb_dist<-cbind(wb$NAME_2,wb$ID_2)
    wb_dist<-as.data.frame(wb_dist)
    colnames(wb_dist) <- c("District","Code")
    abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1] & CalendarDay <= input$dates[2])
    
    
    abp_yrr <- abp_daily %>% group_by(year_yr,year,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    abp_yrr[,-c(1,2,3)] <- round(abp_yrr[,-c(1,2,3)],0)
    
    abp_yrr$year_yr<-as.character(abp_yrr$year_yr)
    abp_yrr$year<-as.character(abp_yrr$year)
    
    abp_yr1 <- abp_yrr
    abp_yr1$District <- as.character(abp_yr1$District)
    abp_yr1 <- within(abp_yr1, District[District == "North_24_Parganas"] <- 'North 24 Parganas')
    abp_yr1 <- within(abp_yr1, District[District == 'South_24_Parganas'] <- 'South 24 Parganas')
    abp_yr1 <- within(abp_yr1, District[District == 'Koch_Bihar'] <- 'Koch Bihar')
    abp_yr1 <- within(abp_yr1, District[District == 'Paschim_Medinipur'] <- 'Pashchim Medinipur')
    abp_yr1 <- within(abp_yr1, District[District == 'Dakshin_Dinajpur'] <- 'Dakshin Dinajpur')
    abp_yr1 <- within(abp_yr1, District[District == 'Uttar_Dinajpur'] <- 'Uttar Dinajpur')
    abp_yr1 <- within(abp_yr1, District[District == 'Purba_Medinipur'] <- 'Purba Medinipur')
    #abp1 <- merge(abp_daily1,wb_dist,all=TRUE)
    #ABP_year <- abp1[(abp1$CalendarDay==input$dates),]
    
    reqd_df <- abp_yr1
    abp_map <- reqd_df %>% group_by(District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Indent=mean(Indent),Adhoc=mean(Adhoc),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    abp_map[,-1] <- round(abp_map[,-1],0)
    abp_map1 <- merge(abp_map,wb_dist,all=TRUE)
    map_df <- abp_map1[,c("District",input$Variable1)]
    colnames(map_df) <- c("District","Variable")
    gap <- (max(map_df$Variable,na.rm = TRUE)-min(map_df$Variable, na.rm = TRUE))/3
    v2 <- min(map_df$Variable, na.rm = TRUE)
    v2 <- as.numeric(v2)
    v4 <- v2+gap
    v4 <- as.numeric(v4)
    v5 <- v2+(2*gap)
    v5 <- as.numeric(v5)
    v6 <- max(map_df$Variable, na.rm = TRUE)
    v6 <- as.numeric(v6)
    c3 <- cut(map_df$Variable,breaks=c(v2,v4,v5,v6),include.lowest=T)
    summary(c3)
    c2<-sub(".*\\[ *(.*?) *,.*", "\\1", levels(c3)[1])
    c2<-round(as.numeric(c2),0)
    c4<-sub(".*, *(.*?) *].*", "\\1", levels(c3)[1])
    c4<-round(as.numeric(c4),0)
    c5<-sub(".*, *(.*?) *].*", "\\1", levels(c3)[2])
    c5<-round(as.numeric(c5),0)
    c6<-sub(".*, *(.*?) *].*", "\\1", levels(c3)[3])
    c6<-round(as.numeric(c6),0)
    c7<-sub(".*, *(.*?) *].*", "\\1", levels(c3)[4])
    c7<-round(as.numeric(c7),0)
    bins <- c(c2,c4,c5,c6)
    if(input$Variable1=='Adhocboost'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"(per 1000 Indent)")
    }
    if(input$Variable1=='AdhocUnsold'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='Wastage'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"(per 1000 Sold)")
    }
    if(input$Variable1=='Adhoc'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='AdhocSold'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='Indent'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='Sold'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    if(input$Variable1=='Unsold'){
      popup <- paste(map_df$District, ":", input$Variable1, map_df$Variable,"pcs")
    }
    ##############################################
    if(input$Variable1=='Adhocboost'){
      legend_head <- paste("Average", ":","<br>" ,input$Variable1,"<br>", " (per 1000 Indent)")
    }
    if(input$Variable1=='AdhocUnsold'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    if(input$Variable1=='Wastage'){
      legend_head <- paste("Average",":","<br>", input$Variable1,"<br>"," (per 1000 Sold)")
    }
    if(input$Variable1=='Adhoc'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    if(input$Variable1=='AdhocSold'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    if(input$Variable1=='Indent'){
      legend_head <- paste("Average", ":","<br>", input$Variable1, "<br>"," (in pcs)")
    }
    if(input$Variable1=='Sold'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    if(input$Variable1=='Unsold'){
      legend_head <- paste("Average", ":","<br>", input$Variable1,"<br>"," (in pcs)")
    }
    pal <- colorBin(palette = "YlGnBu", domain = map_df$Variable,bins=bins)
    
    my_map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.dark",
      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'),minZoom = 6, maxZoom = 6)) %>%
      addPolygons(color = "#444444",data=wb, weight = 2, fillColor = ~pal(map_df$Variable),fillOpacity = 1, popup=popup,label=map_df$District, highlightOptions = highlightOptions(color = "red", weight = 3,
                                                                                                                                                                                   bringToFront = TRUE),layerId =wb$ID_2) %>% setMaxBounds(85.7, 21, 90, 27.5)%>%setView(88.52783,23.42293,zoom=6)%>%
      addLegend(pal = pal, values = round(map_df$Variable,0), opacity = 0.7, title = legend_head,
                position = "bottomright")
    
    
  })
  data4 <- reactiveValues(clickedMarker=NULL)
  
  data_reqd_df_yearly <- reactive({
    reqd_df <- subset(abp_merge, CalendarDay>=input$dates[1] & CalendarDay <= input$dates[2])
    abp_map_yearly <- reqd_df %>% group_by(year_yr,year,District,id) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    abp_map_yearly[,-c(1,2,3)] <- round(abp_map_yearly[,-c(1,2,3)],0)
    
    abp_map_yearly$year_yr<-as.character(abp_map_yearly$year_yr)
    abp_map_yearly$year<-as.character(abp_map_yearly$year)
    
    abp_map <- abp_map_yearly %>% group_by(District,id) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Indent=mean(Indent),Adhoc=mean(Adhoc),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    abp_map[,-1] <- round(abp_map[,-1],0)
    colnames(abp_map) <- c("District","id","Adhocboost(per 1000)","Wastage(per 1000)","Indent(pcs)","Adhoc(pcs)","AdhocUnsold(pcs)","AdhocSold(pcs)","Unsold(pcs)","Sold(pcs)")
    #abp_map[,-c(1,2)] <- format(abp_map[,-c(1,2)], big.mark = ",", trim = TRUE)
    abp_map
  })
  
  
  
  observeEvent(input$Map_cust4_shape_click,{
    print("observed map_marker_click")
    data4$clickedMarker <- input$Map_cust4_shape_click
    print(data4$clickedMarker)
    x=subset(data_reqd_df_yearly(),id == data4$clickedMarker$id)
    updateSelectInput(session, "indistrict1",
                      label = "Select District",
                      choices = c(unique(as.character(x$District)),"All Districts"),
                      selected = unique(as.character(x$District))
    )
    
  })
  observeEvent(input$indistrict1, {
    x=subset(data_reqd_df_yearly(),District == input$indistrict1)
    if(input$indistrict1=="All Districts"){
      output$mytable4_head <- renderText({
        paste("Summary (Average, per day) for West Bengal - for the selected time period")
      })
      output$myTable4 <- renderTable({
        abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1]&CalendarDay<=input$dates[2])
        
        abp_yr <- abp_daily %>% group_by(year_yr,year,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
        abp_yr[,-c(1,2,3)] <- round(abp_yr[,-c(1,2,3)],0)
        
        abp_yr$year_yr<-as.character(abp_yr$year_yr)
        abp_yr$year<-as.character(abp_yr$year)
        
        yr_year_map <- abp_yr %>% group_by(year_yr,year) %>% summarise(Adhocboost= sum(Adhocboost),Wastage=sum(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
        yr_year_map[,-c(1,2)] <- round(yr_year_map[,-c(1,2)],0)
        
        
        abp_yr1 <- yr_year_map
        
        reqd_df <- abp_yr1
        
        abp_map1 <- as.data.frame(rbind(mean(reqd_df$Adhocboost,na.rm=TRUE),mean(reqd_df$Wastage,na.rm=TRUE),mean(reqd_df$Indent,na.rm=TRUE),mean(reqd_df$Adhoc,na.rm=TRUE),mean(reqd_df$AdhocUnsold,na.rm=TRUE),mean(reqd_df$AdhocSold,na.rm=TRUE),mean(reqd_df$Unsold,na.rm=TRUE),mean(reqd_df$Sold,na.rm=TRUE)))
        colnames(abp_map1) <- c("Summary")
        rownames(abp_map1) <- c("Adhocboost(per 1000)","Wastage(per 1000)","Indent(pcs)","Adhoc(pcs)","AdhocUnsold(pcs)","AdhocSold(pcs)","Unsold(pcs)","Sold(pcs)")
        abp_map1$Variable <- rownames(abp_map1)
        rownames(abp_map1)<-NULL
        abp_map1<-abp_map1[,c(2,1)]
        abp_map1[,-1] <-round(abp_map1[,-1],0)
        abp_map1[,-1] <- format(abp_map1[,-1],big.mark=",",trim = TRUE)
        colnames(abp_map1) <- c("Variable", "Average Value (per day for WB)")
        abp_map1
      })
      output$error_daily4<-renderText({
        paste("Click on a particular district on the map to view the summary.")
      })
    }else{
      output$mytable4_head <- renderText({
        paste("Summary (Average, per day) for ",input$indistrict1," for the selected year range in comparison to all the districts of West Bengal")
      })
      output$error_daily4<-renderText({
        paste("")
      })
      output$myTable4 <- renderTable({
        
        y=as.data.frame(t(x))
        y$variable<- colnames(x)
        colnames(y)<-c("Average Value (per day)","Variable")
        rownames(y) <- NULL
        y <- y[-c(1:2),c(2,1)]
        y <- as.data.frame(y)
        class(y$`Average Value (per day)`)
        y$`Average Value (per day)` <- as.numeric(as.character(y$`Average Value (per day)`))
        y[,-1] <- format(y[,-1], big.mark = ",",trim=T)
        abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1]&CalendarDay<=input$dates[2])
        
        abp_yr <- abp_daily %>% group_by(year_yr,year,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
        abp_yr[,-c(1,2,3)] <- round(abp_yr[,-c(1,2,3)],0)
        
        abp_yr$year_yr<-as.character(abp_yr$year_yr)
        abp_yr$year<-as.character(abp_yr$year)
        
        yr_year_map <- abp_yr %>% group_by(year_yr,year) %>% summarise(Adhocboost= sum(Adhocboost),Wastage=sum(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
        yr_year_map[,-c(1,2)] <- round(yr_year_map[,-c(1,2)],0)
        
        
        abp_yr1 <- yr_year_map
        
        reqd_df <- abp_yr1
        
        abp_map1 <- as.data.frame(rbind(mean(reqd_df$Adhocboost,na.rm=TRUE),mean(reqd_df$Wastage,na.rm=TRUE),mean(reqd_df$Indent,na.rm=TRUE),mean(reqd_df$Adhoc,na.rm=TRUE),mean(reqd_df$AdhocUnsold,na.rm=TRUE),mean(reqd_df$AdhocSold,na.rm=TRUE),mean(reqd_df$Unsold,na.rm=TRUE),mean(reqd_df$Sold,na.rm=TRUE)))
        colnames(abp_map1) <- c("Summary")
        rownames(abp_map1) <- c("Adhocboost(per 1000)","Wastage(per 1000)","Indent(pcs)","Adhoc(pcs)","AdhocUnsold(pcs)","AdhocSold(pcs)","Unsold(pcs)","Sold(pcs)")
        abp_map1$Variable <- rownames(abp_map1)
        rownames(abp_map1)<-NULL
        abp_map1<-abp_map1[,c(2,1)]
        abp_map1[,-1] <-round(abp_map1[,-1],0)
        abp_map1[,-1] <- format(abp_map1[,-1],big.mark=",",trim = TRUE)
        colnames(abp_map1) <- c("Variable", "Average Value (per day for WB)")
        final_year_summ <- merge(y,abp_map1,by="Variable",all=TRUE)
        final_year_summ
        
      })
    }
    
    
  })
  
  # output$map_summary <- renderTable({
  #  
  #   #reqd_df <- subset(abp_daily, CalendarDay>="2010-01-01" & CalendarDay <= "2010-03-01")
  #   reqd_df <- subset(dly_daily_map, CalendarDay>=input$dates[1] & CalendarDay <= input$dates[2])
  #   abp_map <- as.data.frame(rbind(mean(reqd_df$Adhocboost),mean(reqd_df$Wastage),mean(reqd_df$Indent),mean(reqd_df$Adhoc),mean(reqd_df$AdhocUnsold),mean(reqd_df$AdhocSold),mean(reqd_df$Unsold),mean(reqd_df$Sold)))
  #   colnames(abp_map) <- c("Summary")
  #   rownames(abp_map) <- c("Adhocboost (per 1000)","Wastage (per 1000)","Indent (pcs)","Adhoc (pcs)","AdhocUnsold (pcs)","AdhocSold (pcs)","Unsold (pcs)","Sold (pcs)")
  #   abp_map$Variable <- rownames(abp_map)
  #   rownames(abp_map)<-NULL
  #   abp_map<-abp_map[,c(2,1)]
  #   abp_map[,-1] <-round(abp_map[,-1],0)
  #   abp_map[,-1] <- format(abp_map[,-1],big.mark=",",trim = TRUE)
  #   colnames(abp_map) <- c("Variable", "Average Value (per day)")
  #   abp_map
  # })
  
  # output$map_summary1 <- renderTable({
  #  
  #   abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1] & CalendarDay <= input$dates[2])
  #  
  #   abp_wk <- abp_daily %>% group_by(year_wk,year,wk,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
  #   abp_wk[,-c(1,2,3,4)] <- round(abp_wk[,-c(1,2,3,4)],0)
  #   abp_wk$year_wk<-as.character(abp_wk$year_wk)
  #   abp_wk$wk<-as.character(abp_wk$wk)
  #  
  #   wk_week_map <- abp_wk %>% group_by(year_wk,year,wk) %>% summarise(Adhocboost= sum(Adhocboost),Wastage=sum(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
  #   wk_week_map[,-c(1,2,3)] <- round(wk_week_map[,-c(1,2,3)],0)
  #  
  #   abp_wk1 <- wk_week_map
  #   reqd_df <- abp_wk1
  #  
  #   abp_map <- as.data.frame(rbind(mean(reqd_df$Adhocboost),mean(reqd_df$Wastage),mean(reqd_df$Indent),mean(reqd_df$Adhoc),mean(reqd_df$AdhocUnsold),mean(reqd_df$AdhocSold),mean(reqd_df$Unsold),mean(reqd_df$Sold)))
  #   colnames(abp_map) <- c("Summary")
  #   rownames(abp_map) <- c("Adhocboost (per 1000)","Wastage (per 1000)","Indent (pcs)","Adhoc (pcs)","AdhocUnsold (pcs)","AdhocSold (pcs)","Unsold (pcs)","Sold (pcs)")
  #   abp_map$Variable <- rownames(abp_map)
  #   rownames(abp_map)<-NULL
  #   abp_map<-abp_map[,c(2,1)]
  #   abp_map[,-1] <-round(abp_map[,-1],0)
  #   abp_map[,-1] <- format(abp_map[,-1],big.mark=",",trim = TRUE)
  #   colnames(abp_map) <- c("Variable", "Average Value (per day)")
  #   abp_map
  # })
  
  # output$map_summary2 <- renderTable({
  #   abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1]&CalendarDay<=input$dates[2])
  #  
  #   abp_mon <- abp_daily %>% group_by(year_mon,year,mon,monthly,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
  #   abp_mon[,-c(1,2,3,4,5)] <- round(abp_mon[,-c(1,2,3,4,5)],0)
  #  
  #   mon_month_map <- abp_mon %>% group_by(year_mon,year,mon,monthly) %>% summarise(Adhocboost= sum(Adhocboost),Wastage=sum(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
  #   mon_month_map[,-c(1,2,3,4)] <- round(mon_month_map[,-c(1,2,3,4)],0)
  #  
  #   abp_mon1 <- mon_month_map
  #  
  #   reqd_df <- abp_mon1
  #  
  #   abp_map <- as.data.frame(rbind(mean(reqd_df$Adhocboost,na.rm=TRUE),mean(reqd_df$Wastage,na.rm=TRUE),mean(reqd_df$Indent,na.rm=TRUE),mean(reqd_df$Adhoc,na.rm=TRUE),mean(reqd_df$AdhocUnsold,na.rm=TRUE),mean(reqd_df$AdhocSold,na.rm=TRUE),mean(reqd_df$Unsold,na.rm=TRUE),mean(reqd_df$Sold,na.rm=TRUE)))
  #   colnames(abp_map) <- c("Summary")
  #   rownames(abp_map) <- c("Adhocboost (per 1000)","Wastage (per 1000)","Indent (pcs)","Adhoc (pcs)","AdhocUnsold (pcs)","AdhocSold (pcs)","Unsold (pcs)","Sold (pcs)")
  #   abp_map$Variable <- rownames(abp_map)
  #   rownames(abp_map)<-NULL
  #   abp_map<-abp_map[,c(2,1)]
  #   abp_map[,-1] <-round(abp_map[,-1],0)
  #   abp_map[,-1] <- format(abp_map[,-1],big.mark=",",trim = TRUE)
  #   colnames(abp_map) <- c("Variable", "Average Value (per day)")
  #   abp_map
  # })
  # output$map_summary3 <- renderTable({
  #   abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1]& CalendarDay <=input$dates[2])
  #   abp_qtr <- abp_daily %>% group_by(year_qtr,year,qtr,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
  #  
  #   abp_qtr[,-c(1,2,3,4)] <- round(abp_qtr[,-c(1,2,3,4)],0)
  #   abp_qtr$year_qtr<-as.character(abp_qtr$year_qtr)
  #   abp_qtr$year<-as.character(abp_qtr$year)
  #   abp_qtr$qtr<-as.character(abp_qtr$qtr)
  #  
  #   qtr_quarter_map <- abp_qtr %>% group_by(year_qtr,year,qtr) %>% summarise(Adhocboost= sum(Adhocboost),Wastage=sum(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
  #   qtr_quarter_map[,-c(1,2,3)] <- round(qtr_quarter_map[,-c(1,2,3)],0)
  #   abp_qtr1 <- qtr_quarter_map
  #  
  #   reqd_df <- abp_qtr1
  #   abp_map <- as.data.frame(rbind(mean(reqd_df$Adhocboost,na.rm=TRUE),mean(reqd_df$Wastage,na.rm=TRUE),mean(reqd_df$Indent,na.rm=TRUE),mean(reqd_df$Adhoc,na.rm=TRUE),mean(reqd_df$AdhocUnsold,na.rm=TRUE),mean(reqd_df$AdhocSold,na.rm=TRUE),mean(reqd_df$Unsold,na.rm=TRUE),mean(reqd_df$Sold,na.rm=TRUE)))
  #   colnames(abp_map) <- c("Summary")
  #   rownames(abp_map) <- c("Adhocboost (per 1000)","Wastage (per 1000)","Indent (pcs)","Adhoc (pcs)","AdhocUnsold (pcs)","AdhocSold (pcs)","Unsold (pcs)","Sold (pcs)")
  #   abp_map$Variable <- rownames(abp_map)
  #   rownames(abp_map)<-NULL
  #   abp_map<-abp_map[,c(2,1)]
  #   abp_map[,-1] <-round(abp_map[,-1],0)
  #   abp_map[,-1] <- format(abp_map[,-1],big.mark=",",trim = TRUE)
  #   colnames(abp_map) <- c("Variable", "Average Value (per day)")
  #   abp_map
  # })
  
  # output$map_summary4 <- renderTable({
  #   abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1]&CalendarDay<=input$dates[2])
  #  
  #   abp_yr <- abp_daily %>% group_by(year_yr,year,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
  #   abp_yr[,-c(1,2,3)] <- round(abp_yr[,-c(1,2,3)],0)
  #  
  #   abp_yr$year_yr<-as.character(abp_yr$year_yr)
  #   abp_yr$year<-as.character(abp_yr$year)
  #  
  #   yr_year_map <- abp_yr %>% group_by(year_yr,year) %>% summarise(Adhocboost= sum(Adhocboost),Wastage=sum(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
  #   yr_year_map[,-c(1,2)] <- round(yr_year_map[,-c(1,2)],0)
  #  
  #  
  #   abp_yr1 <- yr_year_map
  #  
  #   reqd_df <- abp_yr1
  #  
  #   abp_map <- as.data.frame(rbind(mean(reqd_df$Adhocboost,na.rm=TRUE),mean(reqd_df$Wastage,na.rm=TRUE),mean(reqd_df$Indent,na.rm=TRUE),mean(reqd_df$Adhoc,na.rm=TRUE),mean(reqd_df$AdhocUnsold,na.rm=TRUE),mean(reqd_df$AdhocSold,na.rm=TRUE),mean(reqd_df$Unsold,na.rm=TRUE),mean(reqd_df$Sold,na.rm=TRUE)))
  #   colnames(abp_map) <- c("Summary")
  #   rownames(abp_map) <- c("Adhocboost (per 1000)","Wastage (per 1000)","Indent (pcs)","Adhoc (pcs)","AdhocUnsold (pcs)","AdhocSold (pcs)","Unsold (pcs)","Sold (pcs)")
  #   abp_map$Variable <- rownames(abp_map)
  #   rownames(abp_map)<-NULL
  #   abp_map<-abp_map[,c(2,1)]
  #   abp_map[,-1] <-round(abp_map[,-1],0)
  #   abp_map[,-1] <- format(abp_map[,-1],big.mark=",",trim = TRUE)
  #   colnames(abp_map) <- c("Variable", "Average Value (per day)")
  #   abp_map
  # })
  
  #tab_summary
  output$yr_summary <- renderPrint({
    if(input$indistrict1=="All Districts"){
      x<-"The results are given for all districts"
      x
    }
    else{
      ABP_Gen1 <-abp_yr[,c("District",input$Variable1)]
      colnames(ABP_Gen1)=c("District","Yvar")
      
      Districtfilter <- filter(ABP_Gen1, District == input$indistrict1)
      
      x=as.data.frame(summary(Districtfilter)[,2])
      colnames(x)=c(paste0("Summary for : ",input$indistrict1))
      
      na.omit(x)
    }
    x
  })
  output$yr_summary1 <- renderPrint({
    ABP_Gen1 <-abp_yr[,c("District",input$Variable1)]
    colnames(ABP_Gen1)=c("District","Yvar")
    
    x=as.data.frame(summary(ABP_Gen1)[,2])
    colnames(x)=c(paste0("Summary for : All Districts"))
    
    na.omit(x)
  })
  
  output$qtr_summary <- renderPrint({
    if(input$indistrict1=="All Districts"){
      #x<-"  The results are given for all districts. Districts are       Jalpaiguri,Birbhum,Purulia,Bardhaman,Hoogly,North 24 Parganas,Malda,Haora,Paschim Medinipur,Purba Medinipur,Nadia,Bankura,South 24      Parganas,Murshidabad,Darjiling,Dakshin Dinajpur,Koch Bihar,Uttar    Dinajpur  "
      
      x<-"The results are given for all districts"
      
      x
    }
    else{
      
      ABP_Gen1 <-abp_qtr[,c("District",input$Variable1)]
      colnames(ABP_Gen1)=c("District","Yvar")
      
      Districtfilter <- filter(ABP_Gen1, District == input$indistrict1)
      
      x=as.data.frame(summary(Districtfilter)[,2])
      colnames(x)=c(paste0("Summary for : ",input$indistrict1))
      
      na.omit(x)
    }
    x
    
  })
  
  output$qtr_summary1 <- renderPrint({
    ABP_Gen1 <-abp_qtr[,c("District",input$Variable1)]
    colnames(ABP_Gen1)=c("District","Yvar")
    
    x=as.data.frame(summary(ABP_Gen1)[,2])
    colnames(x)=c(paste0("Summary for : All Districts"))
    
    na.omit(x)
  })
  
  output$mon_summary <- renderPrint({
    if(input$indistrict1=="All Districts"){
      x<-"The results are given for all districts"
      x
    }
    else{
      ABP_Gen1 <-abp_mon[,c("District",input$Variable1)]
      colnames(ABP_Gen1)=c("District","Yvar")
      
      Districtfilter <- filter(ABP_Gen1, District == input$indistrict1)
      
      x=as.data.frame(summary(Districtfilter)[,2])
      colnames(x)=c(paste0("Summary for : ",input$indistrict1))
      
      na.omit(x)
    }
    x
  })
  
  output$mon_summary1 <- renderPrint({
    ABP_Gen1 <-abp_mon[,c("District",input$Variable1)]
    colnames(ABP_Gen1)=c("District","Yvar")
    
    x=as.data.frame(summary(ABP_Gen1)[,2])
    colnames(x)=c(paste0("Summary for : All Districts"))
    
    na.omit(x)
  })
  
  output$wk_summary <- renderPrint({
    if(input$indistrict1=="All Districts"){
      x<-"The results are given for all districts"
      x
    }
    else{
      ABP_Gen1 <-abp_wk[,c("District",input$Variable1)]
      colnames(ABP_Gen1)=c("District","Yvar")
      
      Districtfilter <- filter(ABP_Gen1, District == input$indistrict1)
      
      x=as.data.frame(summary(Districtfilter)[,2])
      colnames(x)=c(paste0("Summary for : ",input$indistrict1))
      
      na.omit(x)
    }
    x
  })
  
  output$wk_summary1 <- renderPrint({
    ABP_Gen1 <-abp_wk[,c("District",input$Variable1)]
    colnames(ABP_Gen1)=c("District","Yvar")
    
    x=as.data.frame(summary(ABP_Gen1)[,2])
    colnames(x)=c(paste0("Summary for : All Districts"))
    
    na.omit(x)
  })
  
  output$dly_summary <- renderPrint({
    if(input$indistrict1=="All Districts"){
      x<-"The results are given for all districts"
      x
    }
    else{
      ABP_Gen1 <- abp_daily[,c("District",input$Variable1)]
      colnames(ABP_Gen1)=c("District","Yvar")
      
      Districtfilter <- filter(ABP_Gen1, District == input$indistrict1)
      
      x=as.data.frame(summary(Districtfilter)[,2])
      colnames(x)=c(paste0("Summary for : ",input$indistrict1))
      
      na.omit(x)
    }
    x
  })
  
  output$dly_summary1 <- renderPrint({
    ABP_Gen1 <-abp_daily[,c("District",input$Variable1)]
    colnames(ABP_Gen1)=c("District","Yvar")
    
    x=as.data.frame(summary(ABP_Gen1)[,2])
    colnames(x)=c(paste0("Summary for : All Districts"))
    
    na.omit(x)
  })
  
  output$dly_summary_m <- renderPrint({
    if(input$indistrict1=="All Districts"){
      x<-"The results are given for all districts"
      x
    }
    else{
      abp_daily_1 <- subset(abp_daily,CalendarDay>=input$dates[1] & CalendarDay <= input$dates[2])
      ABP_Gen1 <-abp_daily_1[,c("District",input$Variable1)]
      colnames(ABP_Gen1)=c("District","Yvar")
      
      Districtfilter <- filter(ABP_Gen1, District == input$indistrict1)
      
      x=as.data.frame(summary(Districtfilter)[,2])
      colnames(x)=c(paste0("Summary for : ",input$indistrict1))
      
      na.omit(x)
    }
    x
  })
  
  output$dly_summary_m1 <- renderPrint({
    abp_daily_1 <- subset(abp_daily,CalendarDay>=input$dates[1] & CalendarDay <= input$dates[2])
    ABP_Gen1 <- abp_daily_1[,c("District",input$Variable1)]
    colnames(ABP_Gen1)=c("District","Yvar")
    
    x=as.data.frame(summary(ABP_Gen1)[,2])
    colnames(x)=c(paste0("Summary for : All Districts"))
    
    na.omit(x)
  })
  
  output$group_summ_wb <- renderTable({
    summ_df_wb <- abp_group_summ %>% group_by(group) %>% summarise(Members= length(unique(customer_id_name)), Minimum = min(Indent), Maximum = max(Indent), Average = mean(Indent), Median = median(Indent))
    summ_df_wb[,-c(1,2)] <- round(summ_df_wb[,-c(1,2)],0)
    summ_df_wb$Range <- paste0(summ_df_wb$Minimum,"-",summ_df_wb$Maximum)
    summ_df_wb[,c(3,4)] <- NULL
    colnames(summ_df_wb) <- c("Group","No. of Customers","Group Average(Indent)","Group Median(Indent)","Group Range(Indent)")
    summ_df_wb$Total <- summ_df_wb$`No. of Customers`*summ_df_wb$`Group Average(Indent)`
    colnames(summ_df_wb) <- c("Group","No. of Customers","Group Average(Indent)","Group Median(Indent)","Group Range(Indent)", "Group Total(Indent)")
    summ_df_wb$`Group Average(Indent)` <- format(summ_df_wb$`Group Average(Indent)`, big.mark = ",", trim = TRUE)
    summ_df_wb$`Group Median(Indent)` <- format(summ_df_wb$`Group Median(Indent)`, big.mark = ",", trim = TRUE)
    summ_df_wb$`Group Total(Indent)` <- format(summ_df_wb$`Group Total(Indent)`, big.mark = ",", trim = TRUE)
    summ_df_wb
  })
  
  
  # output$group_summ_dist <- renderTable({
  #   abp_dist_df1 <- abp_dist_df[(abp_dist_df$District==input$indistrict4),]
  #   summ_df_dist <- abp_dist_df1 %>% group_by(group) %>% summarise(Members= length(unique(customer_id_name)), Minimum = min(Indent),Maximum = max(Indent), Average = mean(Indent), Median = median(Indent))
  #   summ_df_dist[,-c(1,2)] <- round(summ_df_dist[,-c(1,2)],0)
  #   summ_df_dist$Range <- paste0(summ_df_dist$Minimum,"-",summ_df_dist$Maximum)
  #   summ_df_dist[,c(3,4)] <- NULL
  #   summ_df_dist
  # })
  output$very_large_waste <- renderTable({
    
    aduns_group_new <- aduns_group[order(aduns_group$AdhocUnsold, decreasing=TRUE),]
    aduns_group_new$cum_perc <- round(aduns_group_new$cum_perc,0)
    aduns_top_20 <- aduns_group_new[(aduns_group_new$cum_perc <= 20),]
    group_table <- aduns_top_20 %>% group_by(group) %>% summarize(Customers = n(), TotalWastage = sum(AdhocUnsold))
    group_table$perc <- 100*(group_table$Customers/sum(group_table$Customers))
    colnames(group_table) <- c("Group", "No.of Customers", "Total Wastage (in pcs)","Percentage of Customers")
    group_table$`Percentage of Customers` <- round(group_table$`Percentage of Customers`,0)
    group_table$`Percentage of Customers` <- paste0(group_table$`Percentage of Customers`,"%")
    group_table$`Total Wastage (in pcs)`<- format(group_table$`Total Wastage (in pcs)`, big.mark = ",",trim=TRUE)
    group_table
  })
  output$very_small_waste <- renderTable({
    aduns_group_new <- aduns_group[order(aduns_group$AdhocUnsold, decreasing=TRUE),]
    aduns_very_small <- aduns_group_new[(aduns_group_new$cum_perc >80 & aduns_group_new$cum_perc <= 100),]
    group_table_very_small <- aduns_very_small %>% group_by(group) %>% summarize(Customers = n(), TotalWastage = sum(AdhocUnsold))
    group_table_very_small$perc <- 100*(group_table_very_small$Customers/sum(group_table_very_small$Customers))
    colnames(group_table_very_small) <- c("Group", "No.of Customers", "Total Wastage (in pcs)","Percentage of Customers")
    group_table_very_small$`Percentage of Customers` <- round(group_table_very_small$`Percentage of Customers`,0)
    group_table_very_small$`Percentage of Customers` <- paste0(group_table_very_small$`Percentage of Customers`,"%")
    group_table_very_small$`Total Wastage (in pcs)`<- format(group_table_very_small$`Total Wastage (in pcs)`, big.mark = ",",trim=TRUE)
    group_table_very_small
  })
  output$cust_glance <- renderTable({
    cust1 <- subset(cust_summ, customer_id_name==input$Customer1)
    cust_df <- cust1[,c("group", "customer_id_name","Indent", "Range")]
    cust_df$Indent <- round(cust_df$Indent,0)
    cust_df$Customer <- gsub("_.*$", "", cust_df$customer_id_name)
    cust_df$customer_id_name <- NULL
    cust_df <- cust_df[,c(4,1,2,3)]
    cust_df$Indent <- format(cust_df$Indent, big.mark = ",", trim = TRUE)
    colnames(cust_df) <- c("Customer", "Group", "Average Indent (in pcs)", "Group Indent Range (in pcs)")
    cust_df
  })
  output$wkly_htext <- renderText({
    if(input$Variable1=="AdhocUnsold"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Weekwise Daily Average",input$Variable1,"(pcs per day) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste("Weekwise Daily Average",input$Variable1,"(pcs per day) for ",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}
      
    }
    if(input$Variable1=="Adhocboost"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Weekwise Daily Average ",input$Variable1,"(pcs per 1000, per day per district) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste(input$Variable1,"(pcs per 1000, per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    if(input$Variable1=="Wastage"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Weekwise Daily Average ",input$Variable1,"(pcs per 1000, per day per district) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste(input$Variable1,"(per 1000, per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    if(input$Variable1=="AdhocSold"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Weekwise Daily Average ",input$Variable1,"(pcs per day) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste("Weekwise Daily Average ",input$Variable1,"(pcs per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")} }
    if(input$Variable1=="Indent"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Weekwise Daily Average ",input$Variable1,"(pcs per day) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste("Weekwise Daily Average ",input$Variable1,"(pcs per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    if(input$Variable1=="Sold"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Weekwise Daily Average ",input$Variable1,"(pcs per day) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste("Weekwise Daily Average ",input$Variable1,"(pcs per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    if(input$Variable1=="Adhoc"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Weekwise Daily Average ",input$Variable1,"(pcs per day) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste("Weekwise Daily Average ",input$Variable1,"(pcs per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    if(input$Variable1=="Unsold"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Weekwise Daily Average ",input$Variable1,"(pcs per day) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      
      else{t<-paste("Total ",input$Variable1,"(pcs per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    t
  })
  
  output$monly_htext <- renderText({
    if(input$Variable1=="AdhocUnsold"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Monthwise Daily Average ",input$Variable1,"for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste("Monthwise Daily Average ",input$Variable1,"for ",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}
      
    }
    if(input$Variable1=="Adhocboost"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Monthwise Daily Average ",input$Variable1,"(per 1000, per day per district) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste(input$Variable1,"(per 1000, per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    if(input$Variable1=="Wastage"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Monthwise Daily Average ",input$Variable1,"(per 1000, per day per district) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste(input$Variable1,"(per 1000, per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    if(input$Variable1=="AdhocSold"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Monthwise Daily Average ",input$Variable1,"(per day, in pcs) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste("Monthwise Daily Average ",input$Variable1,"(per day, in pcs) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")} }
    if(input$Variable1=="Indent"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Monthwise Daily Average ",input$Variable1,"(per day, in pcs) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste("Monthwise Daily Average ",input$Variable1,"(per day, in pcs) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    if(input$Variable1=="Sold"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Monthwise Daily Average ",input$Variable1,"(per day, in pcs) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste("Total ",input$Variable1,"(per day, in pcs) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    if(input$Variable1=="Adhoc"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Monthwise Daily Average ",input$Variable1,"(per day, in pcs) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste("Monthwise Daily Average ",input$Variable1,"(per day, in pcs) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    if(input$Variable1=="Unsold"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Monthwise Daily Average ",input$Variable1,"(per day, in pcs) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      
      else{t<-paste("Monthwise Daily Average ",input$Variable1,"(per day, in pcs) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    t
  })
  output$qtrly_htext <- renderText({
    if(input$Variable1=="AdhocUnsold"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Quarterwise Daily Average ",input$Variable1,"(pcs per day) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste("Quarterwise Daily Average ",input$Variable1,"(pcs per day) for ",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}
      
    }
    if(input$Variable1=="Adhocboost"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Quarterwise Daily Average ",input$Variable1,"(pcs per 1000, per day per district) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste(input$Variable1,"(per 1000, per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    if(input$Variable1=="Wastage"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Quarterwise Daily Average ",input$Variable1,"(pcs per 1000, per day per district) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste(input$Variable1,"(per 1000, per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    if(input$Variable1=="AdhocSold"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Quarterwise Daily Average ",input$Variable1,"(pcs per day) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste("Quarterwise Daily Average ",input$Variable1,"(pcs per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")} }
    if(input$Variable1=="Indent"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Quarterwise Daily Average ",input$Variable1,"(pcs per day) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste("Quarterwise Daily Average ",input$Variable1,"(pcs per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    if(input$Variable1=="Sold"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Quarterwise Daily Average ",input$Variable1,"(pcs per day) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste("Quarterwise Daily Average ",input$Variable1,"(pcs per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    if(input$Variable1=="Adhoc"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Quarterwise Daily Average ",input$Variable1,"(pcs per day) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      else{t<-paste("Quarterwise Daily Average ",input$Variable1,"(pcs per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    if(input$Variable1=="Unsold"){
      if(input$indistrict1=="All Districts"){
        t<-paste("Quarterwise Daily Average ",input$Variable1,"(pcs per day) for West Bengal, (",input$dates[1]," to ",input$dates[2],")")}
      
      else{t<-paste("Quarterwise Daily Average ",input$Variable1,"(pcs per day) for",input$indistrict1, "(",input$dates[1]," to ",input$dates[2],")")}}
    t
  })
  abp_wk <- abp_daily %>% group_by(year_wk,year,wk,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
  abp_wk[,-c(1,2,3,4)] <- round(abp_wk[,-c(1,2,3,4)],0)
  abp_wk$year_wk<-as.character(abp_wk$year_wk)
  abp_wk$wk<-as.character(abp_wk$wk)
  
  wk_week <- abp_wk %>% group_by(year_wk,year,wk) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
  wk_week[,-c(1,2,3)] <- round(wk_week[,-c(1,2,3)],0)
  
  output$plot_wkly <- renderPlotly({
    if(input$indistrict1=="All Districts"){
      abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1]&CalendarDay<=input$dates[2])
      abp_wk <- abp_daily %>% group_by(year_wk,year,wk,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
      abp_wk[,-c(1,2,3,4)] <- round(abp_wk[,-c(1,2,3,4)],0)
      abp_wk$year_wk<-as.character(abp_wk$year_wk)
      abp_wk$wk<-as.character(abp_wk$wk)
      
      wk_week <- abp_wk %>% group_by(year_wk,year,wk) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
      wk_week[,-c(1,2,3)] <- round(wk_week[,-c(1,2,3)],0)
      
      dly_daily1 <- wk_week
      
      abp_dly1<-dly_daily1[,c("year_wk",input$Variable1)]
      
      colnames(abp_dly1) <- c("Week", "Yvar")
      
      f2<-list(
        family="Old Standard TT,serif",
        size=8,
        color="black"
      )
      
      a <- list(
        title = "Week"
        
      )
      b <- list(
        title = input$Variable1,
        showticklabels = TRUE
      )
      
      myplot<- plot_ly(x=~abp_dly1$Week,y=~abp_dly1$Yvar,type = "scatter",mode="lines+markers")%>%
        layout(xaxis=a,yaxis = b)
      
    }
    else{
      #daily
      abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1]&CalendarDay<=input$dates[2])
      abp_wk <- abp_daily %>% group_by(year_wk,year,wk,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
      abp_wk[,-c(1,2,3,4)] <- round(abp_wk[,-c(1,2,3,4)],0)
      abp_wk$year_wk<-as.character(abp_wk$year_wk)
      abp_wk$wk<-as.character(abp_wk$wk)
      
      abp_daily1 <- abp_wk
      
      abp_dly1<-abp_daily1[abp_daily1$District==input$indistrict1,]
      abp_dly2<-abp_dly1[,c("year_wk","District",input$Variable1)]
      
      colnames(abp_dly2) <- c("Week","District", "Yvar")
      f2<-list(
        family="Old Standard TT,serif",
        size=8,
        color="black"
      )
      
      a <- list(
        
        title = "Week"
        
        
      )
      b <- list(
        
        title = input$Variable1,
        showticklabels = TRUE
        
      )
      
      myplot<-plot_ly(x=~abp_dly2$Week,y=~abp_dly2$Yvar,type = "scatter",mode="lines+markers")%>%
        layout(xaxis=a,yaxis = b)
    }
    myplot
  })
  
  abp_mon <- abp_daily %>% group_by(year_mon,year,mon,monthly,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
  abp_mon[,-c(1,2,3,4,5)] <- round(abp_mon[,-c(1,2,3,4,5)],0)
  mon_month <- abp_mon %>% group_by(year_mon,year,mon,monthly) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
  mon_month[,-c(1,2,3,4)] <- round(mon_month[,-c(1,2,3,4)],0)
  mon_month_map <- abp_mon %>% group_by(year_mon,year,mon,monthly) %>% summarise(Adhocboost= sum(Adhocboost),Wastage=sum(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
  mon_month_map[,-c(1,2,3,4)] <- round(mon_month_map[,-c(1,2,3,4)],0)
  
  output$plot_monly <- renderPlotly({
    if(input$indistrict1=="All Districts"){
      abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1]& CalendarDay<=input$dates[2])
      abp_mon <- abp_daily %>% group_by(year_mon,year,mon,monthly,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
      abp_mon[,-c(1,2,3,4,5)] <- round(abp_mon[,-c(1,2,3,4,5)],0)
      mon_month <- abp_mon %>% group_by(year_mon,year,mon,monthly) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
      mon_month[,-c(1,2,3,4)] <- round(mon_month[,-c(1,2,3,4)],0)
      
      dly_daily1 <-mon_month
      
      abp_dly1<-dly_daily1[,c("year_mon",input$Variable1)]
      
      colnames(abp_dly1) <- c("Month", "Yvar")
      
      f2<-list(
        family="Old Standard TT,serif",
        size=8,
        color="black"
      )
      
      a <- list(
        title = "Month"
        
      )
      b <- list(
        title = input$Variable1,
        showticklabels = TRUE
      )
      
      myplot<- plot_ly(x=~abp_dly1$Month,y=~abp_dly1$Yvar,type = "scatter",mode="lines+markers")%>%
        layout(xaxis=a,yaxis = b)
      
    }
    else{
      #daily
      abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1]& CalendarDay<=input$dates[2])
      abp_mon <- abp_daily %>% group_by(year_mon,year,mon,monthly,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
      abp_mon[,-c(1,2,3,4,5)] <- round(abp_mon[,-c(1,2,3,4,5)],0)
      
      abp_daily1 <- abp_mon
      
      abp_dly1<-abp_daily1[abp_daily1$District==input$indistrict1,]
      abp_dly2<-abp_dly1[,c("year_mon","District",input$Variable1)]
      
      colnames(abp_dly2) <- c("Month","District", "Yvar")
      f2<-list(
        family="Old Standard TT,serif",
        size=8,
        color="black"
      )
      
      a <- list(
        
        title = "Month"
        
        
      )
      b <- list(
        
        title = input$Variable1,
        showticklabels = TRUE
        
      )
      
      myplot<-plot_ly(x=~abp_dly2$Month,y=~abp_dly2$Yvar,type = "scatter",mode="lines+markers")%>%
        layout(xaxis=a,yaxis = b)
    }
    myplot
  })
  output$plot_qtrly <- renderPlotly({
    if(input$indistrict1=="All Districts"){
      abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1]& CalendarDay<=input$dates[2])
      
      abp_qtr <- abp_daily %>% group_by(year_qtr,year,qtr,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
      abp_qtr[,-c(1,2,3,4)] <- round(abp_qtr[,-c(1,2,3,4)],0)
      abp_qtr$year_qtr<-as.character(abp_qtr$year_qtr)
      abp_qtr$year<-as.character(abp_qtr$year)
      abp_qtr$qtr<-as.character(abp_qtr$qtr)
      
      qtr_quarter <- abp_qtr %>% group_by(year_qtr,year,qtr) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=sum(Adhoc),Indent=sum(Indent),AdhocUnsold=sum(AdhocUnsold),AdhocSold=sum(AdhocSold),Unsold=sum(Unsold),Sold=sum(Sold))
      qtr_quarter[,-c(1,2,3)] <- round(qtr_quarter[,-c(1,2,3)],0)
      
      dly_daily1 <- qtr_quarter
      
      
      abp_dly1<-dly_daily1[,c("year_qtr",input$Variable1)]
      
      colnames(abp_dly1) <- c("Quarter", "Yvar")
      
      f2<-list(
        family="Old Standard TT,serif",
        size=8,
        color="black"
      )
      
      a <- list(
        title = "Quarter"
        
      )
      b <- list(
        title = input$Variable1,
        showticklabels = TRUE
      )
      
      myplot<- plot_ly(x=~abp_dly1$Quarter,y=~abp_dly1$Yvar,type = "scatter",mode="lines+markers")%>%
        layout(xaxis=a,yaxis = b)
      
    }
    else{
      #daily
      abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1]& CalendarDay<=input$dates[2])
      
      abp_qtr <- abp_daily %>% group_by(year_qtr,year,qtr,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
      abp_qtr[,-c(1,2,3,4)] <- round(abp_qtr[,-c(1,2,3,4)],0)
      abp_qtr$year_qtr<-as.character(abp_qtr$year_qtr)
      abp_qtr$year<-as.character(abp_qtr$year)
      abp_qtr$qtr<-as.character(abp_qtr$qtr)
      
      abp_daily1 <- abp_qtr
      
      abp_dly1<-abp_daily1[abp_daily1$District==input$indistrict1,]
      abp_dly2<-abp_dly1[,c("year_qtr","District",input$Variable1)]
      
      colnames(abp_dly2) <- c("Quarter","District", "Yvar")
      f2<-list(
        family="Old Standard TT,serif",
        size=8,
        color="black"
      )
      
      a <- list(
        
        title = "Quarter"
        
        
      )
      b <- list(
        
        title = input$Variable1,
        showticklabels = TRUE
        
      )
      
      myplot<-plot_ly(x=~abp_dly2$Quarter,y=~abp_dly2$Yvar,type = "scatter",mode="lines+markers")%>%
        layout(xaxis=a,yaxis = b)
    }
    myplot
  })
  
  output$wkly_text <- renderText({
    if(input$Variable1=="AdhocUnsold"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected week range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected week range (per day) for ",input$indistrict1)}
      
    }
    if(input$Variable1=="Adhocboost"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (per 1000) for the selected week range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (per 1000) for the selected week range (per day) for ",input$indistrict1)}
      
    }
    if(input$Variable1=="Wastage"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (per 1000) for the selected week range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (per 1000) for the selected week range (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="AdhocSold"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected week range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected week range (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="Indent"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected week range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected week range (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="Sold"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected week range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected week range (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="Adhoc"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected week range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected week range (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="Unsold"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected week range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected week range (per day) for ",input$indistrict1)}
    }
    z
  })
  output$monly_text <- renderText({
    if(input$Variable1=="AdhocUnsold"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected month range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected month range (per day) for ",input$indistrict1)}
      
    }
    if(input$Variable1=="Adhocboost"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (per 1000) for the selected month range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (per 1000) for the selected month range (per day) for ",input$indistrict1)}
      
    }
    if(input$Variable1=="Wastage"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (per 1000) for the selected month range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (per 1000) for the selected month range (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="AdhocSold"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected month range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected month range (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="Indent"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected month range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected month range (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="Sold"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected month range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected month range (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="Adhoc"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected month range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected month range (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="Unsold"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected month range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected month range (per day) for ",input$indistrict1)}
    }
    z
  })
  output$qtrly_text <- renderText({
    if(input$Variable1=="AdhocUnsold"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected quarter range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected quarter range (per day) for ",input$indistrict1)}
      
    }
    if(input$Variable1=="Adhocboost"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (per 1000) for the selected quarter range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (per 1000) for the selected quarter range (per day) for ",input$indistrict1)}
      
    }
    if(input$Variable1=="Wastage"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (per 1000) for the selected quarter range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (per 1000) for the selected quarter range (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="AdhocSold"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected quarter range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected quarter range (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="Indent"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected quarter range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected quarter range (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="Sold"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected quarter range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected quarter range (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="Adhoc"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected quarter range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected quarter range (per day) for ",input$indistrict1)}
    }
    if(input$Variable1=="Unsold"){
      if(input$indistrict1=="All Districts"){
        z<-paste("The chart shows the average ",input$Variable1," (in pcs) for the selected quarter range (per day) for ",input$indistrict1,"of West Bengal.")}
      else{z<-paste("The chart shows the total ",input$Variable1," (in pcs) for the selected quarter range (per day) for ",input$indistrict1)}
    }
    z
  })
  
  output$wkly_summary_m1 <- renderPrint({
    
    abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1]&CalendarDay<=input$dates[2])
    abp_wk <- abp_daily %>% group_by(year_wk,year,wk,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    abp_wk[,-c(1,2,3,4)] <- round(abp_wk[,-c(1,2,3,4)],0)
    abp_wk$year_wk<-as.character(abp_wk$year_wk)
    abp_wk$wk<-as.character(abp_wk$wk)
    
    abp_daily_1 <- abp_wk
    ABP_Gen1 <- abp_daily_1[,c("District",input$Variable1)]
    colnames(ABP_Gen1)=c("District","Yvar")
    
    x=as.data.frame(summary(ABP_Gen1)[,2])
    colnames(x)=c(paste0("Summary for : All Districts"))
    
    na.omit(x)
  })
  output$monly_summary_m1 <- renderPrint({
    abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1]& CalendarDay<=input$dates[2])
    abp_mon <- abp_daily %>% group_by(year_mon,year,mon,monthly,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    abp_mon[,-c(1,2,3,4,5)] <- round(abp_mon[,-c(1,2,3,4,5)],0)
    
    abp_daily_1 <- abp_mon
    ABP_Gen1 <- abp_daily_1[,c("District",input$Variable1)]
    colnames(ABP_Gen1)=c("District","Yvar")
    
    x=as.data.frame(summary(ABP_Gen1)[,2])
    colnames(x)=c(paste0("Summary for : All Districts"))
    
    na.omit(x)
  })
  
  output$qtrly_summary_m1 <- renderPrint({
    abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1]& CalendarDay<=input$dates[2])
    abp_qtr <- abp_daily %>% group_by(year_qtr,year,qtr,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
    abp_qtr[,-c(1,2,3,4)] <- round(abp_qtr[,-c(1,2,3,4)],0)
    abp_qtr$year_qtr<-as.character(abp_qtr$year_qtr)
    abp_qtr$year<-as.character(abp_qtr$year)
    abp_qtr$qtr<-as.character(abp_qtr$qtr)
    
    abp_daily_1 <- abp_qtr
    ABP_Gen1 <- abp_daily_1[,c("District",input$Variable1)]
    colnames(ABP_Gen1)=c("District","Yvar")
    
    x=as.data.frame(summary(ABP_Gen1)[,2])
    colnames(x)=c(paste0("Summary for : All Districts"))
    
    na.omit(x)
  })
  
  output$wkly_summary_m <- renderPrint({
    if(input$indistrict1=="All Districts"){
      x<-"The results are given for all districts"
      x
    }
    else{
      abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1]&CalendarDay<=input$dates[2])
      abp_wk <- abp_daily %>% group_by(year_wk,year,wk,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
      abp_wk[,-c(1,2,3,4)] <- round(abp_wk[,-c(1,2,3,4)],0)
      abp_wk$year_wk<-as.character(abp_wk$year_wk)
      abp_wk$wk<-as.character(abp_wk$wk)
      
      abp_daily_1 <- abp_wk
      ABP_Gen1 <-abp_daily_1[,c("District",input$Variable1)]
      colnames(ABP_Gen1)=c("District","Yvar")
      
      Districtfilter <- filter(ABP_Gen1,District== input$indistrict1)
      
      x=as.data.frame(summary(Districtfilter)[,2])
      colnames(x)=c(paste0("Summary for : ",input$indistrict1))
      
      na.omit(x)
    }
    x
  })
  output$monly_summary_m <- renderPrint({
    if(input$indistrict1=="All Districts"){
      x<-"The results are given for all districts"
      x
    }
    else{
      abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1]& CalendarDay<=input$dates[2])
      abp_mon <- abp_daily %>% group_by(year_mon,year,mon,monthly,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
      abp_mon[,-c(1,2,3,4,5)] <- round(abp_mon[,-c(1,2,3,4,5)],0)
      
      abp_daily_1 <- abp_mon
      ABP_Gen1 <-abp_daily_1[,c("District",input$Variable1)]
      colnames(ABP_Gen1)=c("District","Yvar")
      
      Districtfilter <- filter(ABP_Gen1, District == input$indistrict1)
      
      x=as.data.frame(summary(Districtfilter)[,2])
      colnames(x)=c(paste0("Summary for : ",input$indistrict1))
      
      na.omit(x)
    }
    x
  })
  output$qtrly_summary_m <- renderPrint({
    if(input$indistrict1=="All Districts"){
      x<-"The results are given for all districts"
      x
    }
    else{
      abp_daily <- subset(abp_daily,CalendarDay>=input$dates[1]& CalendarDay<=input$dates[2])
      abp_qtr <- abp_daily %>% group_by(year_qtr,year,qtr,District) %>% summarise(Adhocboost= mean(Adhocboost),Wastage=mean(Wastage),Adhoc=mean(Adhoc),Indent=mean(Indent),AdhocUnsold=mean(AdhocUnsold),AdhocSold=mean(AdhocSold),Unsold=mean(Unsold),Sold=mean(Sold))
      abp_qtr[,-c(1,2,3,4)] <- round(abp_qtr[,-c(1,2,3,4)],0)
      abp_qtr$year_qtr<-as.character(abp_qtr$year_qtr)
      abp_qtr$year<-as.character(abp_qtr$year)
      abp_qtr$qtr<-as.character(abp_qtr$qtr)
      
      abp_daily_1 <- abp_qtr
      ABP_Gen1 <-abp_daily_1[,c("District",input$Variable1)]
      colnames(ABP_Gen1)=c("District","Yvar")
      
      Districtfilter <- filter(ABP_Gen1, District == input$indistrict1)
      
      x=as.data.frame(summary(Districtfilter)[,2])
      colnames(x)=c(paste0("Summary for : ",input$indistrict1))
      
      na.omit(x)
    }
    x
  })
  output$cust_dist_map <- renderLeaflet({
    colors <- c("#4fc13c", "orange","orchid","lavenderblush","darkmagenta")
    #legend_map <- paste("Customer","<br>","Distribution           ")
    leaflet(width = "100%",options = leafletOptions(zoomControl = FALSE,
                                                    minZoom = 6.65, maxZoom = 6.65))%>%addLegend(position = 'bottomright',
                                                                                                 colors = colors,
                                                                                                 labels = c("VeryLarge(%)","Large(%)","Medium(%)","Small(%)","VerySmall(%)"),
                                                                                                 opacity = 1)%>%addPolygons(
                                                                                                   color = "#444444",data=wb, weight = 2, label=wb$NAME_2)%>% setMaxBounds(85.7, 21, 90, 27.5)%>%setView(88.52783,23.42293,zoom=6) %>% addMinicharts(
                                                                                                     cust_distribution$lng, cust_distribution$lat,
                                                                                                     type = "pie",
                                                                                                     chartdata = cust_distribution[, c("VeryLarge(%)","Large(%)","Medium(%)","Small(%)","VerySmall(%)")],
                                                                                                     width = 35*sqrt(cust_distribution$Total)/sqrt(max(cust_distribution$Total,na.rm = TRUE)),
                                                                                                     colorPalette = colors)
  })
  output$loss_df <- renderTable({
    l1 <- c(adhoc_unsol_loss_jan,adhoc_unsol_loss_feb,adhoc_unsol_loss_mar)
    l2 <- c(pred_loss_jan,pred_loss_feb,pred_loss_mar)
    my_frame <- data.frame(cbind(l1,l2))
    rownames(my_frame) <- c("Jan", "Feb", "Mar")
    my_frame$loss_per <- paste0(round((l2/l1)*100,0),"%")
    my_frame$Month <-paste0(rownames(my_frame),"-",2016)
    
    my_frame[,-4] <- format(my_frame[,-4],big.mark=",",trim=TRUE)
    colnames(my_frame) <- c("(AdhocUnsold) Loss (in Rs.)", "Predicted Loss (in Rs.)","Predicted Loss (% AdhocUnsold Loss)","Month")
    my_frame[,c(4,1:3)]
  })
  output$intution <-renderTable({
    
    Var <- c("Indent(in pcs)","TotalSold(in pcs)","AdhocUnsold(in pcs)","AdhocSold(in pcs)","Profit from AdhocSold(in Rs.)","Loss from AdhocUnsold(in Rs.)")
    Val <- c(100,120,8,20,40,50)
    
    df <- as.data.frame(cbind(Var,Val))
    df$Val <- format(df$Val,big.mark=",",trim=TRUE)
    colnames(df) <- c("Variable","Value")
    df
  })
  output$case1 <-renderTable({
    Var1 <- c("Adhoc Sent","Adhoc Sold","Adhoc Unsold")
    Val1 <- c(20,17,3)
    
    case1 <- as.data.frame(cbind(Var1,Val1))
    colnames(case1) <- c("Variable","Value")
    case1
  })
  output$case2 <-renderTable({
    Var2 <- c("Adhoc Sent","Adhoc Sold","Adhoc Unsold")
    Val2 <- c(20,20,0)
    
    case11 <- as.data.frame(cbind(Var2,Val2))
    colnames(case11) <- c("Variable","Value")
    case11
  })
  output$case3 <-renderTable({
    Var3 <- c("Adhoc Sent","Adhoc Sold","Adhoc Unsold")
    val3 <- c(0,0,0)
    
    case111 <- as.data.frame(cbind(Var3,val3))
    colnames(case111) <- c("Variable","Value")
    case111
  })
  
  data_year <-reactive({
    subset(ABP,year==input$year1)
  })
  data_week <-reactive({
    subset(data_year(),Week==input$week1)
  })
  output$wasplot <- renderUI({
    selectizeInput("cusid","Select a Customer", choices=c(unique(data_week()[,"customer_id_name"])))
  })
  
  output$wkd_plot <- renderPlotly({
    abp_1 <- ABP[(ABP$year==input$year1),]
    abp_2 <- abp_1[(abp_1$Week == input$week1),]
    abp_3 <- abp_2[(abp_2$customer_id_name == input$cusid),]
    # abp_2$weekday <- weekdays(abp_2$CalendarDay)
    # wkd_agg <- abp_1 %>% group_by(weekday) %>% summarise(AdhocUnsold = sum(AdhocUnsold), Adhoc = sum(Adhoc))
    abp_3$per <- (abp_3$AdhocUnsold/abp_3$Adhoc)*100
    abp_3$per[is.na(abp_3$per)] <-0
    abp_3$per <- round(abp_3$per)
    abp_3$weekday <- factor(abp_3$weekday, levels= c("Sunday", "Monday",
                                                     "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
    
    abp_3 <- abp_3[order(abp_3$weekday), ]
    #wkd_agg$AdhocUnsold <- format(wkd_agg$AdhocUnsold, big.mark = ",", trim = TRUE)
    f2<-list(
      family="Old Standard TT,serif",
      size=10,
      color="black"
    )
    
    a <- list(
      title = "Day of the Week",
      showticklabels = TRUE,tickangle=45,
      tickfont=f2,showgrid = F, zeroline = F
    )
    
    b <- list(
      title = "Total AdhocUnsold (in pcs)",tickformat=",d",
      showticklabels = TRUE,showgrid = F, zeroline = F
    )
    
    wkd_plot <- plot_ly(abp_3, x = ~weekday, y = ~AdhocUnsold, type = "scatter", mode="lines+markers", name = "AdhocUnsold (in pcs)") %>%
      add_trace(abp_3, x = ~weekday, y = ~per, mode = "lines+markers", yaxis = "y2", name = "AdhocUnsold (as % Adhoc)") %>%
      layout(yaxis2 = list(overlaying = "y", side = "right", xaxis=a, yaxis=b))
    
  })
  
  output$pie_sent_not_sent <- renderPlotly({
    if(input$dates_1 < "2016-01-01"){
      day_man <- subset(ABP,CalendarDay == input$dates_1)
      data <- data.frame(
        group = c("Adhoc Not Sent", "Non-zero AdhocUnsold", "Zero AdhocUnsold"),
        value = c(nrow(day_man[day_man$Adhoc==0,]), nrow(day_man[day_man$Adhoc!=0 & day_man$AdhocUnsold != 0,]), nrow(day_man[day_man$Adhoc!=0 & day_man$AdhocUnsold == 0,]))
      )
      plot_ly(data, labels = ~group, values = ~value, type = 'pie') %>%
        layout(xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
               yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
    }
  })
  
  
  output$pie_sent_not_sent2 <- renderPlotly({
    if(input$year_s < 2016){
      yr_man <- subset(ABP,year == input$year_s)
      wk_man <- subset(yr_man,Week == as.numeric(substr(input$week_s,3,4)))
      data <- data.frame(
        group = c("Adhoc Not Sent", "Non-zero AdhocUnsold", "Zero AdhocUnsold"),
        value = c(nrow(wk_man[wk_man$Adhoc==0,]), nrow(wk_man[wk_man$Adhoc!=0 & wk_man$AdhocUnsold != 0,]), nrow(wk_man[wk_man$Adhoc!=0 & wk_man$AdhocUnsold == 0,]))
      )
      plot_ly(data, labels = ~group, values = ~value, type = 'pie') %>%
        layout(xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
               yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
    }
  })
  
  
  output$pie_sent_not_sent3 <- renderPlotly({
    if(input$year_s1 < 2016){
      yr_man <- subset(ABP,year == input$year_s1)
      wk_man <- subset(yr_man,Month == as.numeric(match(input$month_s,month.abb)))
      data <- data.frame(
        group = c("Adhoc Not Sent", "Non-zero AdhocUnsold", "Zero AdhocUnsold"),
        value = c(nrow(wk_man[wk_man$Adhoc==0,]), nrow(wk_man[wk_man$Adhoc!=0 & wk_man$AdhocUnsold != 0,]), nrow(wk_man[wk_man$Adhoc!=0 & wk_man$AdhocUnsold == 0,]))
      )
      plot_ly(data, labels = ~group, values = ~value, type = 'pie') %>%
        layout(xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
               yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
    }
  })
  
  output$trend_adhocSent <- renderPlotly({
    adhoc_not_0 <- ABP[(ABP$Adhoc>0),]
    adhoc_0 <- ABP[(ABP$Adhoc==0),]
    adun_more_0 <- adhoc_not_0[(adhoc_not_0$AdhocUnsold>0),]
    adun_0 <- adhoc_not_0[(adhoc_not_0$AdhocUnsold==0),]
    ad_not0 <- adhoc_not_0 %>% group_by(CalendarDay) %>% summarise(adhoc_sent = n())
    ad_0 <- adhoc_0 %>% group_by(CalendarDay) %>% summarise(adhoc_not_sent = n())
    aduns_more_0 <- adun_more_0 %>% group_by(CalendarDay) %>% summarise(adhoc_unsold_greater_0 = n())
    aduns_0 <- adun_0 %>% group_by(CalendarDay) %>% summarise(aduns_0 = n())
    final_data <- merge(ad_not0,ad_0, by ="CalendarDay", all= TRUE)
    final_data1 <- merge(aduns_more_0, aduns_0, by ="CalendarDay", all= TRUE)
    new_df <- merge(final_data1,final_data, by ="CalendarDay", all= TRUE)
    new_df$weekday <- weekdays(new_df$CalendarDay)
    
    new_df$AdhocUnsoldGreater0 <- round(100*(new_df$adhoc_unsold_greater_0/(new_df$adhoc_sent)),0)
    new_df$AdhocUnsoldEqual0<- round(100*(new_df$aduns_0/(new_df$adhoc_sent)),0)
    new_df$AdhocSent <- round(100*(new_df$adhoc_sent/(new_df$adhoc_sent+new_df$adhoc_not_sent)),0)
    
    
    # plot_ly(new_df, x = ~CalendarDay, y = ~input$advar, type = '', mode = 'lines+markers',text=paste(new_df$weekday)) %>% layout(title = "",
    #                                                                                              xaxis = list(title = "Calendar Day"),
    #                                                                                              yaxis = list (title = "Percentage of Customers"))
    plot_ly(new_df, x = ~CalendarDay) %>%
      add_trace(y = ~new_df[,input$advar], name = input$advar,mode = 'lines+markers',text=paste(new_df$weekday)) %>% layout(title = "",
                                                                                                                            xaxis = list(title = "Calendar Day"),
                                                                                                                            yaxis = list (title = "Percentage of Customers"))
  })
  
  output$chart_text <- renderText({
    if(input$advar=="AdhocSent"){
      y <-paste("Daywise:% of Customers sent Adhoc")
    }
    if(input$advar=="AdhocUnsoldEqual0" ){
      y<- paste("Daywise:% of Customers having Adhoc Unsold more than zero (out of the ones sent Adhoc)")
    }
    if(input$advar=="AdhocUnsoldGreater0"){
      y<- paste("Daywise:% of Customers having Adhoc Unsold as zero (out of the ones sent Adhoc)")
    }
    y
  })
  output$adun_text <- renderText({
    if(input$advar=="AdhocSent"){
      y1<- paste("The graph shows the percentage of customers who get Adhoc out of the total customers on each day")
    }
    if(input$advar=="AdhocUnsoldEqual0"){
      y1<-paste("The graph shows the percentage of customers who have Adhoc Unsold greater than zero out of the customers who get Adhoc on each day")
    }
    if(input$advar=="AdhocUnsoldGreater0"){
      y1<-paste("The graph shows the percentage of customers who do not have any Adhoc Unsold, and thus sell all the Adhoc sent to them out of the customers who get Adhoc on each day. In this case there maybe some loss for the company from lost opportunity; since in case an extra piece were sent, it is not known whether it could be sold or not.")
    }
    y1
  })
  output$scatter_cust <- renderPlotly({
    adhoc_more_0 <- ABP[(ABP$Adhoc>0),]
    no_adhoc <- ABP[(ABP$Adhoc==0),]
    cust_req_df <- adhoc_more_0 %>% group_by(CustomerDescription) %>% summarise(days_of_adhoc = n())
    c1 <- no_adhoc %>% group_by(CustomerDescription) %>% summarise(no_adhoc_days = n())
    # cus_adhoc_sent <- cust_df[(cust_df$Adhoc>0),]
    cust_ad_sent_not_sent <- merge(c1, cust_req_df, by = "CustomerDescription", all= TRUE)
    cust_days <- ABP %>% group_by(CustomerDescription) %>% summarise(cust_days=n())
    cust_final_df <- merge(cust_days,cust_ad_sent_not_sent,by = "CustomerDescription", all= TRUE)
    adhoc_unsold_more_0 <- adhoc_more_0[(adhoc_more_0$AdhocUnsold>0),]
    cust_au_more_0 <- adhoc_unsold_more_0 %>% group_by(CustomerDescription) %>% summarise(adun_more_0 = n())
    cust_final_df$no_adhoc_days[is.na(cust_final_df$no_adhoc_days)] <- 0
    cust_final_df$days_of_adhoc[is.na(cust_final_df$days_of_adhoc)] <- 0
    cust_final_df$per_adhocsent <- round((cust_final_df$days_of_adhoc/cust_final_df$cust_days)*100)
    cust_final_df$per_adhocnotsent <- round((cust_final_df$no_adhoc_days/cust_final_df$cust_days)*100)
    cust_final_df1 <- merge(cust_final_df,cust_au_more_0,by="CustomerDescription", all=TRUE)
    cust_final_df1$adun_more_0[is.na(cust_final_df1$adun_more_0)] <- 0
    cust_final_df1$per_adun_more_0 <- round((cust_final_df1$adun_more_0/cust_final_df1$days_of_adhoc)*100)
    cust_final_df1$per_adun_more_0[is.na(cust_final_df1$per_adun_more_0)] <- 0
    p1 <- plot_ly(data = cust_final_df1, x = ~per_adhocsent, y = ~per_adun_more_0, type = "scatter", mode = "markers", text=paste(cust_final_df1$CustomerDescription))
    p1 %>% layout(title = "",
                  xaxis = list(title = "% of days Adhoc Sent"),
                  yaxis = list (title = "% of days AdhocUnsold greater than 0"))
  })
  values <- reactiveValues(datedata = 0, weekdata = 0, monthdata = 0)
  # Definining and initializing the reactiveValues object with 3 reactive values namely, uno, dos, tres.
  # uno would serve for the reactive value for the first button
  # dos as reactive value for second button
  # tres as reactive value for third button
  # Idea is to set one of the reactive values to 1 when corresponding buttons are clicked and rest to zero. Further based on if conditional statement print which button is pressed.
  
  
  observeEvent(input$dt, {
    values$datedata <- 1
    values$weekdata <- 0
    values$monthdata <- 0
    
  })
  
  observeEvent(input$week, {
    values$datedata <- 0
    values$weekdata <- 1
    values$monthdata <- 0
  })
  
  observeEvent(input$month, {
    values$datedata <- 0
    values$weekdata <- 0
    values$monthdata <- 1
  })
  
  
  output$dates1 <- renderUI({
    dateInput("dates_1","Select a Date ",value = "2010-01-01",min="2010-01-01",max="2015-12-31")
  })
  output$date_data_table <- renderTable({
    day_df <- day_final[day_final$CalendarDay==input$dates_1,]
    day_df$CalendarDay <- NULL
    day_df <- data.frame(t(day_df))
    rownames(day_df) <- NULL
    colnames(day_df) <- c("Values")
    day_df$Variable <- c("Indent (in pcs)","Total Sold (in pcs)","AdhocUnsold (in pcs)","AdhocSold (in pcs)","Profit from AdhocSold (in Rs.)","Loss from AdhocUnsold (in Rs.)")
    
    day_df <- day_df[,c(2,1)]
    day_df
  })
  
  
  output$year_s <- renderUI({
    selectizeInput("year_s","Select a Year",choices =c(2010,2011,2012,2013,2014,2015))
  })
  data_year <-reactive({
    subset(week_final,year==input$year_s)
  })
  
  output$week_s <- renderUI({
    selectizeInput("week_s","Select a Week", choices=c(unique(data_year()[,"wk"])))
  })
  
  output$week_data_table <-renderTable({
    week_df <- week_final[week_final$year==input$year_s & week_final$wk==input$week_s,]
    week_df$Week <- NULL
    week_df$year <-NULL
    week_df$wk <- NULL
    week_df <- data.frame(t(week_df))
    colnames(week_df) <- "Values"
    week_df$Variable <- rownames(week_df)
    rownames(week_df) <- NULL
    week_df <- week_df[,c(2,1)]
    week_df
  })
  
  output$year_s1 <- renderUI({
    selectizeInput("year_s1","Select a Year",choices =c(2010,2011,2012,2013,2014,2015))
  })
  data_year1 <-reactive({
    subset(month_final,year==input$year_s1)
  })
  
  output$months <- renderUI({
    selectizeInput("month_s","Select a Month", choices=c(month.abb[sort(as.numeric(match(unique(data_year1()$mon),month.abb)))]))
  })
  
  output$month_data_table <-renderTable({
    mon_df <- month_final[month_final$year==input$year_s1 & month_final$mon==input$month_s,]
    mon_df$Month <- NULL
    mon_df$year <- NULL
    mon_df$mon <- NULL
    mon_df <- data.frame(t(mon_df))
    colnames(mon_df) <- "Values"
    mon_df$Variable <- rownames(mon_df)
    rownames(mon_df) <- NULL
    mon_df <- mon_df[,c(2,1)]
    mon_df
  })
  
  output$intution_dates <-renderTable({
    table <- test_date_s[test_date_s$Calendarday== input$date_intution,]
    table <- table[,-1]
    table <- table[,c(1,3,2)]
    colnames(table) <- c("AdhocUnsold (in pcs)","AdhocUnsold Loss (in Rs)","Predicted Loss (in Rs.)")
    table$`AdhocUnsold (in pcs)` <- format(table$`AdhocUnsold (in pcs)`, big.mark = ",", trim=TRUE)
    table$`AdhocUnsold Loss (in Rs)` <- format(table$`AdhocUnsold Loss (in Rs)`, big.mark = ",", trim=TRUE)
    table$`Predicted Loss (in Rs.)` <- format(table$`Predicted Loss (in Rs.)`, big.mark = ",", trim=TRUE)
    table
  })  
  output$intution_weeks <-renderTable({
    table <- test_week_s[test_week_s$Week== input$wk1_intution,]
    table <- table[,c(1:3,5,4)]
    colnames(table) <- c("Year", "Week","AdhocUnsold (in pcs)","AdhocUnsold Loss (in Rs)","Predicted Loss (in Rs.)")
    table$`AdhocUnsold (in pcs)` <- format(table$`AdhocUnsold (in pcs)`, big.mark = ",", trim=TRUE)
    table$`AdhocUnsold Loss (in Rs)` <- format(table$`AdhocUnsold Loss (in Rs)`, big.mark = ",", trim=TRUE)
    table$`Predicted Loss (in Rs.)` <- format(table$`Predicted Loss (in Rs.)`, big.mark = ",", trim=TRUE)
    table
  })
  output$intution_months <-renderTable({
    table <- test_month_s[test_month_s$Month== match(input$mon1_intution,month.abb),]
    table$Month1 = month.abb[table$Month]
    table$Month <- NULL
    table <- table[,c(1,5,2,4,3)]
    colnames(table) <- c("Year", "Month","AdhocUnsold (in pcs)","AdhocUnsold Loss (in Rs)","Predicted Loss (in Rs.)")
    table$`AdhocUnsold (in pcs)` <- format(table$`AdhocUnsold (in pcs)`, big.mark = ",", trim=TRUE)
    table$`AdhocUnsold Loss (in Rs)` <- format(table$`AdhocUnsold Loss (in Rs)`, big.mark = ",", trim=TRUE)
    table$`Predicted Loss (in Rs.)` <- format(table$`Predicted Loss (in Rs.)`, big.mark = ",", trim=TRUE)
    table
  })
}



shinyApp(ui, server)







