#library(readxl)
#rawdata = read_excel("사용변수.xlsx")

#install.packages("haven")
library(haven)
#install.packages("writexl")
library(writexl)
rawdata <- read_spss("Koweps_hpc16_2021_beta1.sav")
#write_xlsx(rawdata, path = "real_rawdata.xlsx")
#write_xlsx(rawdata, path = "rawdata.xlsx")

usedata = rawdata[,c(1,16,17,18,21,24,27,29,30,31,34,35,36,67,68,69,100,103,106,153,159,163,
                     184,186,262,263,264,265,266,267,284,285,286,287,288,289,290,291,292,293,
                     294,295,296,297,298,299,300,301,302,303,305,306,307,308,309,625,626,627,
                     629,631,712,1040,1041,1042,1043,1044)]
usedata
names(usedata)

#library(reshape2)
#install.packages("reshape")
library(reshape)

usedata <- rename(usedata, c("h16_id" = "id",
                             "h16_reg7" = "region",
                             "h16_din" = "disposable_income",
                             "h16_cin" = "current_income",
                             "h16_hc_all" = "income_quintile",
                             "h1601_1" = "member_count",
                             "h16_g1" = "member_num",
                             "h16_g3" = "gender",
                             "h16_g4" = "birth_year",
                             "h16_g6" = "education",
                             "h16_g9" = "disability grade",
                             "h16_g10" = "marriage",
                             "h16_g11" = "religion",
                             "h16_eco9" = "occ_category",
                             "h16_eco10" = "occ_size",
                             "h16_eco11" = "inactive_reason",
                             "h1606_1" = "house_category",
                             "h1606_5" = "house_size",
                             "h1606_6" = "house_price", 
                             "h1607_4" = "tax",
                             "h1607_5" = "welfare_tax",
                             "h1607_9" = "living_cost",
                             "h16_inc2" = "common_yearly_income",
                             "h16_inc3" = "temporary_yearly_income",
                             "h1609_aq1" = "banking_loan",
                             "h1609_aq2" = "loan",
                             "h1609_aq3" = "card_loan",
                             "h1609_aq4" = "deposit",
                             "h1609_aq5" = "credit",
                             "h1609_aq6" = "debt",
                             "h1610_aq1" = "own_house",
                             "h1610_aq2" = "own_etc",
                             "h1610_aq3" = "own_land",
                             "h1610_aq4" = "possess_deposit",
                             "h1610_aq5" = "possess_etc",
                             "h1610_aq6" = "fin_savings",
                             "h1610_aq7" = "fin_install_savings",
                             "h1610_aq8" = "fin_stock",
                             "h1610_aq9" = "fin_mutual_money",
                             "h1610_aq10" = "fin_etc",
                             "h1610_aq11" = "agri1",
                             "h1610_aq12" = "agri2",
                             "h1610_aq13" = "agri3",
                             "h1610_aq14" = "agri4",
                             "h1610_aq15" = "agri5",
                             "h1610_aq16" = "agri6",
                             "h1610_aq17" = "agri7",
                             "h1610_aq18" = "agri8",
                             "h1610_aq19" = "agri9",
                             "h1610_aq20" = "agri10",
                             "h1610_27" = "car",
                             "h1610_aq23" = "asset1",
                             "h1610_aq24" = "asset2",
                             "h1610_aq25" = "asset3",
                             "h1610_aq26" = "asset4",
                             "p1603_5" = "health_s",
                             "p1603_6" = "income_s",
                             "p1603_7" = "house_s",
                             "p1603_9" = "occ_s",
                             "p1603_11" = "leisure_s",
                             "p1607_3aq7" = "univ_location",
                             "h16_pers_income1" = "individual_income1",
                             "h16_pers_income2" = "individual_income2",
                             "h16_pers_income3" = "individual_income3",
                             "h16_pers_income4" = "individual_income4",
                             "h16_pers_income5" = "individual_income5"))
write_xlsx(usedata, path = "usedata2.xlsx")
#write_xlsx(usedata, path = "usedata2_1.xlsx")
#usedata2_1 <- read_excel("usedata2_1.xlsx")
#summary(usedata2_1)

library(readxl)
usedata2 <- read_excel("usedata2.xlsx")
summary(usedata2)

library(dplyr)
usedata2 <- usedata2 %>% mutate(gross_property = house_price + 
                                  own_house + own_etc + own_land + 
                                  possess_deposit + possess_etc + 
                                  fin_savings + fin_install_savings + fin_etc + fin_mutual_money + fin_stock +
                                  agri1 + agri2 + agri3 + agri4 + agri5 + agri6 + agri7 + agri8 + agri9 + agri10 +
                                  car + asset1 + asset2 + asset3 + asset4)
usedata2 <- usedata2 %>% mutate(total_liability = banking_loan + loan + card_loan + deposit + credit + debt)
usedata2 <- usedata2 %>% mutate(net_assets = gross_property - total_liability)
summary(usedata2)

write_xlsx(usedata2, path = "usedata_final.xlsx")





