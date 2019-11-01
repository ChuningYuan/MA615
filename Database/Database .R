####chinook from R

library(tidyverse)
library(RSQLite)
library(DBI)

library(readxl)

library(XLConnect)
field_des <- read_excel("~/Desktop/MA615/SQL/database/Top MA Donors 2016-2020.xlsx",sheet = 1)
contrib_all <- read_excel("~/Desktop/MA615/SQL/database/Top MA Donors 2016-2020.xlsx",sheet = 2)
JFC <- read_excel("~/Desktop/MA615/SQL/database/Top MA Donors 2016-2020.xlsx",sheet = 3)

#select = "Direct_Contribution_+_JFC Dist"



contributors <- select(contrib_all,
                       contribid,fam,contrib,City,State,Zip,Fecoccemp,orgname)%>%distinct()

orgs <- select(contrib_all,orgname,ultorg) %>% distinct() %>% na.omit() 

contribution <- select(contrib_all,
                       contribid,date,amount,recipient,cycle,type,cmteid)%>%distinct()
recipients <- select(contrib_all,
                     recipient,party,recipcode,cmteid)%>%distinct()



mydb <- dbConnect(SQLite(),"ChuningYuan.sqlite")
dbWriteTable(conn = mydb,value=contributors,name="contributors")
dbWriteTable(conn = mydb,value=orgs,name="orgs")
dbWriteTable(conn = mydb,value=contribution,name="contribution")
dbWriteTable(conn = mydb,value=recipients,name="recipients")



















