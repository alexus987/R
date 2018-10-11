# Implementing Apriori Algorithm in R

# reference links
# http://www.rdatamining.com/examples/association-rules
# https://datascienceplus.com/implementing-apriori-algorithm-in-r/
# http://www.salemmarafi.com/code/market-basket-analysis-with-r/

library(haven)
library(plyr)
library(arules)
library(arulesViz)
library(odbc)


#laadime andmed SASist
#tv_kanalid_vaatamised_train <- read_sas("\\\\sasetl\\Data2\\Alexey\\TV_VAATAJA_PAKETT_KANAL.sas7bdat")
#tv_noppekanalid_validate <- read_sas("\\\\sasetl\\Data2\\Alexey\\tv_noppekanalid_validate.sas7bdat")

#laadime andmed Verticast
vertica <- dbConnect(odbc::odbc(), "TeliaDWH")

#TV_SOOVITUSED_TEMP <- dbGetQuery(vertica, "select * from alexsi.TV_SOOVITUSED_TEMP")
system.time(MOB_SOOVITUSED_TEMP1 <- dbGetQuery(vertica,
"
SELECT distinct ID, MODEL 
FROM (

select
--SUBS_SEGMENT.SUBSEGMENT,
hash (MV_BO_CURRENT_SUBSCRIBER.SUSG_REF, MV_BO_CURRENT_SUBSCRIBER.PHONE_NUMBER) as ID, 
--MV_BO_CURRENT_SUBSCRIBER.TRAFFIC_DATA_ANALYSIS_IND,
EQUIPMENT_AGR.START_CALL_DATE::date ,
--EQUIPMENT_AGR.LAST_CALL_DATE,
--EQUIPMENT_AGR.EQUIPMENT_ID,
--EQUIPMENT.MANUFACTURE_NAME,
--EQUIPMENT.CLASS_NAME,
case 
when EQUIPMENT.MODEL like 'P20 Pro%' then  'P20 Pro'
when EQUIPMENT.MODEL like 'P20 EML%' then  'P20 Pro'
when EQUIPMENT.MODEL like 'P20 Lite%' then 'P20 Lite'
else EQUIPMENT.MODEL end as MODEL


from DWH_PROD.MV_BO_CURRENT_SUBSCRIBER as MV_BO_CURRENT_SUBSCRIBER
inner join DWH_PROD.BO_CURRENT_TELNUMBERS as BO_CURRENT_TELNUMBERS on MV_BO_CURRENT_SUBSCRIBER.PHONE_NUMBER = BO_CURRENT_TELNUMBERS.PHONE_NUMBER
inner join DWH_PROD.EQUIPMENT_AGR as EQUIPMENT_AGR  on MV_BO_CURRENT_SUBSCRIBER.SUSG_REF = EQUIPMENT_AGR.SUSG_REF_NUM
inner join DWH_PROD.EQUIPMENT as EQUIPMENT on EQUIPMENT_AGR.EQUIPMENT_ID = EQUIPMENT.ID
inner join DWH_PROD.SUBS_SEGMENT as SUBS_SEGMENT on EQUIPMENT_AGR.SUBS_SEGMENT_ID = SUBS_SEGMENT.ID


where 
/*MV_BO_CURRENT_SUBSCRIBER*/
MV_BO_CURRENT_SUBSCRIBER.PACKAGE_CODE <> 'VIRT' 
/*and MV_BO_CURRENT_SUBSCRIBER.TRAFFIC_DATA_ANALYSIS_IND = 'J'*/
and MV_BO_CURRENT_SUBSCRIBER.EQUIPMENT_ID > 0
and MV_BO_CURRENT_SUBSCRIBER.SUBS_STATUS IN ('AC','TC','Active','Suspended') 
and MV_BO_CURRENT_SUBSCRIBER.PACKAGE_CODE NOT IN ('EUR2', 'EURS', 'M2EE', 'M2EL', 'M2M ', 'M2M1', 'M2M2', 'M2M3', 'M2M5', 'M2M6', 'M2M8', 'M2M9',  
'M2MA', 'M2ME', 'M2MF', 'M2MM', 'M2MN', 'M2MQ', 'M2MR', 'M2MZ', 'M2MT', 'M2OS', 'M2PB', 'M2PC',  
'M2PD', 'M2PF', 'M2PH', 'M2PJ', 'M2PK', 'M2PN', 'M2PT', 'M2RE', 'M2RM', 'M2U ', 'M2UA', 'M2UK', 'M2UM',  
'M2UP', 'SIMM', 'SIMQ', 'SIMR', 'SIMT', 'SIMW', 'TRVF', 'TRVL', 'SIMB', 'SIMD', 'SIME', 'SIMH', 'SIMK',  
'SIMO', 'SIMP', 'SIMU', 'SIPE', 'SIRR', 'SIWA', 'SIWE', 'SIWI', 'M2AR', 'M2MB', 'M2MG', 'M2MJ', 'M2MK',  
'M2MO', 'M2MU', 'M2MY', 'M2P1', 'M2PE', 'M2PG', 'M2PI', 'M2PM', 'M2PP', 'M2PR', 'M2PS', 'M2PU', 'M2UV', 'M2V2')
/*BO_CURRENT_TELNUMBERS*/
and BO_CURRENT_TELNUMBERS.NETY_TYPE_CODE = 'GSM'
/*SUBS_SEGMENT */
/* and SUBS_SEGMENT.SEGMENT IN ('Suurklient','Äriklient') */

and EQUIPMENT.CLASS_NAME in ('Lowend','Tablet','Highend','Smarttouch','Nosurf','Midrange','Smartphone')   
and EQUIPMENT_AGR.START_CALL_DATE > '2010-01-01'

ORDER BY ID, START_CALL_DATE

)a 
;
") 
)



data_train1 <- data.frame(sapply(MOB_SOOVITUSED_TEMP1,as.factor))

head(MOB_SOOVITUSED_TEMP1)


# txn_train = as(data_train, "transactions")
# inspect(txn_train)


# df_sorted <- MOB_SOOVITUSED_TEMP[order(MOB_SOOVITUSED_TEMP$ID),]
# df_sorted$ID <- as.numeric(df_sorted$ID)


# df_itemList <- ddply(data_train,c("ID","START_CALL_DATE"), 
#                     function(df1)paste(df1$MODEL, 
#                                        collapse = ","))



df_itemList_train1 <- ddply(MOB_SOOVITUSED_TEMP1,c("ID"),
                           function(df1)paste(df1$MODEL,    collapse = ","))



df_itemList_train1$ID <- NULL
# df_itemList_train$START_CALL_DATE <- NULL #tehtud SQL tasemel

colnames(df_itemList_train1) <-c('itemlist')

head(df_itemList_train1)

# df_itemList_train1 <- data.frame(sapply(df_itemList_train1,as.factor))
# txn_train1 = as(df_itemList_train1, "transactions")

#salvestame vahetulemusi alla
write.csv(df_itemList_train1,"MOB_itemList_train1.csv", quote = FALSE, row.names = TRUE)
#write.csv(df_itemList_train,"\\\\sasetl\\Data2\\Alexey\\tv_vaatamine_kanal_201710.csv", quote = FALSE, row.names = TRUE)

#laadime faili sisse "trancation" tüübiga
#txn_train1 = read.transactions(file="C:\\Users\\alexsi\\Documents\\MOB_itemList_train1.csv", rm.duplicates = TRUE, format = "basket", sep = ",",cols = 1)
txn_train1 = read.transactions(file="MOB_itemList_train1.csv", rm.duplicates = TRUE, format = "basket", sep = ",",cols = 1)
# txn_train = read.transactions(file="\\\\sasetl\\Data2\\Alexey\\tv_vaatamine_kanal_201710.csv", rm.duplicates = TRUE, format = "basket", sep = ",",cols = 1)

#eemaldame tühikud
txn_train1@itemInfo$labels <- gsub("\"","",txn_train1@itemInfo$labels)


# Graph to display top 5 items
itemFrequencyPlot(txn_train1, topN = 10)


#Apriori() class APparameter maxlen=4


# rhs="P20 Pro"
rules1 <- apriori(txn_train1,parameter = list(sup = 0.00001, conf = 0.0001,target="rules", maxlen=2),
                               appearance = list(default="lhs",rhs="P20 Pro"), 
                                control = list(verbose=F) 
                  )

#rules1<-sort(rules1, decreasing = TRUE, by="confidence")
rules1<-sort(rules1, decreasing = TRUE,  by="count")
inspect(rules1)

inspect(rules1[1:100])


df_rules1<-as(rules1, "data.frame")
write.csv(df_rules1,"df_rules1_P20pro.csv", quote = FALSE, row.names = TRUE)



# rhs="P20 Lite"
rules2 <- apriori(txn_train1,parameter = list(sup = 0.00001, conf = 0.0001,target="rules", maxlen=2),
                  appearance = list(default="lhs",rhs="P20 Lite"), 
                  control = list(verbose=F) 
)

rules2<-sort(rules2, decreasing = TRUE, by="count")

inspect(rules2)

df_rules2<-as(rules2, "data.frame")
write.csv(df_rules2,"df_rules2_P20lite.csv", quote = FALSE, row.names = TRUE)



# rules <- apriori(txn_train,parameter = list(sup = 0.001, conf = 0.5,target="rules", maxlen=4))
# rules <- apriori(txn_train1,parameter = list(sup = 0.0001, conf = 0.001,target="rules", maxlen=2))



# TULEMUSTE VISUALISEERIMINE

#inspect(head(sort(filmzone_rules_validate, by="lift"), 10))

View(df_filmzone_rules_validate)
plot(rules2,measure=c("support","confidence"),shading="lift",interactive=T)
#plot(filmzone_rules_validate,measure=c("support","lift"),shading="confidence",interactive=T)

#plot(basket_rules)
#plot(basket_rules, method = "grouped", control = list(k = 5))
plot(rules2, method="graph", control=list(type="items"))
plot(rules2,measure=c("support","lift"),shading="confidence",interactive=T)
plot(rules2, method="paracoord",  control=list(alpha=.5, reorder=TRUE))

subrules2 <- sample(rules1, 10)
plot(subrules2, method="graph", control=list(type="items"))


# JÄRELANALÜÜS

df_rules <-rbind(df_rules1,df_rules2)
s_rules<-separate(df_rules, rules, into=c("left", "right"), sep='=>' )
head(s_rules)

s_rules<-s_rules[order(s_rules$left),]   

# aggregeerime Support järgi
max_support <- merge(aggregate(support ~ left, data = s_rules, FUN = max),s_rules)
head(max_support)

# aggregeerime Lift järgi
max_lift <- merge(aggregate(lift ~ left, data=s_rules, FUN=max),s_rules)
head(max_lift)

max_lift1 <- merge(aggregate(support ~ left, data=max_lift, FUN=max),max_lift)


write.csv(max_lift1,"max_lift.csv", quote = FALSE, row.names = TRUE)
