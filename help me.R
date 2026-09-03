library(dplyr)

library(plyr) 
library(VennDiagram)
library(stringr)
library(tidyverse)
library(writexl)

library(VennDiagram)

library(tidyr)
library(faersquarterlydata)
library(datawizard)
library(data.table)

#2024 data!!!
faers_data <- retrieve_faersascii(ascii_dir = "C:/Users/kjinl/Documents/2024 tri1/research//2021/ASCII",drug_indication_pattern ="Anxiety|Depression|Premenstrual dysphoric disorder|Binge eating disorder|Post-traumatic stress disorder|PTSD|Generalized anxiety disorder|Bulimia|Depression|Major depressive disorder|Panic|OCD|Obsessive-compulsive disorder")
faers_data_unified <- unify_tabular_ascii(ascii_list = faers_data)
faers_data_unified=remove_duplicate_cases(faers_data_unified)[[1]]
#summary_faers <- summary_faersdata(faers_data_unified)

#drugs_summary=summary_faers[["prod_ai"]]

#faers_data_unified$outcome_all = ifelse(faers_data_unified$outcome_OT=="1"|faers_data_unified$outcome_HO == "1"|faers_data_unified$outcome_DE == "1"|faers_data_unified$outcome_LT == "1"|faers_data_unified$outcome_DS == "1"|faers_data_unified$outcome_RI == "1", TRUE, FALSE)


zoloft <- filter( 
  faers_data_unified,drugname_all == "SERTRALINE" | drugname_all == "ZOLOFT" |drugname_all == "SERTRALINE HYDROCHLORIDE") 
zoloft_summary=(summary_faersdata(zoloft))
zoloft_adr=as.data.frame(table(zoloft$pt))
#WHOOPS SHIT THIS STILL HAS ZOLOFT IN IT
#rerun all nonzoloft and analysis
not_zoloft=filter( 
  faers_data_unified,drugname_all != "SERTRALINE" & drugname_all != "ZOLOFT" &drugname_all != "SERTRALINE HYDROCHLORIDE") 
#not_zoloft_summary=summary_faersdata(not_zoloft)
#of patients taking zoloft
length(unique(zoloft[["caseid"]]))
zoloft_unique=distinct(zoloft, primaryid, .keep_all = TRUE)
#gender/indication breakdown of zoloft takers
zoloft_sex=as.data.frame(table(zoloft_unique$sex))
zoloft_indi=as.data.frame(table(zoloft_unique$indi_pt_all))
zoloft_indi_final=count_indications (c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual "),zoloft)
zoloft_report_country=as.data.frame(table(zoloft$reporter_country))
zoloft_occr_country=as.data.frame(table(zoloft_unique$occr_country))
not_zoloft_report_country=as.data.frame(table(not_zoloft_unique$reporter_country))
not_zoloft_occr_country=as.data.frame(table(not_zoloft_unique$occr_country))

#filter for if any outcome is true - this is kind of fucking pointless
zoloft$outcome_all = ifelse(zoloft$outcome_OT=="1"|zoloft$outcome_HO == "1"|zoloft$outcome_DE == "1"|zoloft$outcome_LT == "1"|zoloft$outcome_DS == "1"|zoloft$outcome_RI == "1", TRUE, FALSE)


#indication work

zoloft_unique_druginfo=rbindlist(zoloft_unique$patient_drug)



zoloft_unique_druginfo$dose_amt=zoloft_unique_druginfo$dose_amt%>%as.double()
hist(zoloft_unique_druginfo$dose_amt)
#zoloft_no_overdose=zoloft %>%filter("dose_amt"<900)
#age breakdown 
hist(zoloft_unique$age)
summary(zoloft_unique$age)
#AE table not zoloft
not_zoloft_adr=as.data.frame(table(not_zoloft$pt))
length(unique(not_zoloft[["caseid"]]))

#not zoloft info
not_zoloft_unique=distinct(not_zoloft, primaryid, .keep_all = TRUE)
not_zoloft_druglist=as.data.frame(table(not_zoloft$drugname_ps))
not_zoloft_unique_druginfo=rbindlist(not_zoloft_unique$patient_drug)
not_zoloft_sex=as.data.frame(table(not_zoloft_unique$sex))
not_zoloft_indi=as.data.frame(table(not_zoloft_unique$indi_pt_all))
not_zoloft_unique_druginfo$dose_amt=not_zoloft_unique_druginfo$dose_amt%>%as.double()
not_zoloft_indi=count_indications(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual","Bulimia"),not_zoloft)

hist(not_zoloft_unique_druginfo$dose_amt)
hist(not_zoloft_unique$age)


zoloft_adr_rate=adr_rate(zoloft)
not_zoloft_adr_rate=adr_rate(not_zoloft)
zoloft_indi=count_indications(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual","Bulimia"),zoloft)

zoloft_split=(split(zoloft, zoloft$indi_pt_all))
zoloft_anxiety=bind_rows(zoloft_split$Anxiety,zoloft_split$`Anxiety disorder`,zoloft_split$`Anxiety/Depression`,zoloft_split$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split$`Anxiety/Obsessive-compulsive disorder`)
zoloft_depression=bind_rows(zoloft_split$Depression, zoloft_split$`Depression suicidal`,zoloft_split$`Depression/Obsessive-compulsive disorder`,zoloft_split$`Depression/Panic attack`,zoloft_split$`Anxiety/Depression`,zoloft_split$`Anxiety/Depression/Post-traumatic stress disorder`)
zoloft_other=bind_rows(zoloft_split$`Panic attack`,zoloft_split$`Panic disorder`, zoloft_split$`Depression/Panic attack`,zoloft_split$`Post-traumatic stress disorder`,zoloft_split$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split$`Depression/Obsessive-compulsive disorder`,zoloft_split$`Obsessive-compulsive disorder`)  
zoloft_anxiety_adr=as.data.frame(table(zoloft_anxiety$pt))
zoloft_depression_adr=as.data.frame(table(zoloft_depression$pt))

not_zoloft_split=(split(not_zoloft, not_zoloft$indi_pt_all))
not_zoloft_anxiety=bind_rows(not_zoloft_split$Anxiety,not_zoloft_split$`Anxiety disorder`,not_zoloft_split$`Anxiety/Depression`,not_zoloft_split$`Anxiety/Depression/Post-traumatic stress disorder`,not_zoloft_split$`Anxiety/Obsessive-compulsive disorder`)
not_zoloft_depression=bind_rows(not_zoloft_split$Depression, not_zoloft_split$`Depression suicidal`,not_zoloft_split$`Depression/Obsessive-compulsive disorder`,not_zoloft_split$`Depression/Panic attack`,not_zoloft_split$`Anxiety/Depression`,not_zoloft_split$`Anxiety/Depression/Post-traumatic stress disorder`)
not_zoloft_other=bind_rows(zoloft_split$`Panic attack`,zoloft_split$`Panic disorder`, zoloft_split$`Depression/Panic attack`,zoloft_split$`Post-traumatic stress disorder`,zoloft_split$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split$`Depression/Obsessive-compulsive disorder`,zoloft_split$`Obsessive-compulsive disorder`)  
not_zoloft_anxiety_adr=as.data.frame(table(not_zoloft_anxiety$pt))
not_zoloft_depression_adr=as.data.frame(table(not_zoloft_depression$pt))


# zoloft_split=(split(zoloft, zoloft$indi_pt_all))
# zoloft_anxiety=bind_rows(zoloft_split$Anxiety,zoloft_split$`Anxiety disorder`,zoloft_split$`Anxiety/Depression`,zoloft_split$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split$`Anxiety/Obsessive-compulsive disorder`)
# zoloft_depression=bind_rows(zoloft_split$Depression, zoloft_split$`Depression suicidal`,zoloft_split$`Depression/Obsessive-compulsive disorder`,zoloft_split$`Depression/Panic attack`,zoloft_split$`Anxiety/Depression`,zoloft_split$`Anxiety/Depression/Post-traumatic stress disorder`)

#compare demographics

zoloft_sex=as.data.frame(table(zoloft_unique$sex))
zoloft_indi=as.data.frame(table(zoloft_unique$indi_pt_all))
zoloft_indi_final=count_indications(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual "),zoloft)
report_country=as.data.frame(table(zoloft_unique$reporter_country))
occr_country=as.data.frame(table(zoloft_unique$occr_country))


zoloft_unique_druginfo=rbindlist(zoloft_unique$patient_drug)



zoloft_unique_druginfo$dose_amt=zoloft_unique_druginfo$dose_amt%>%as.double()
hist(zoloft_unique_druginfo$dose_amt)
#zoloft_no_overdose=zoloft %>%filter("dose_amt"<900)
#age breakdown 
hist(zoloft_unique$age)
summary(zoloft_unique$age)

#compare adr rates between indications

ic=ROR_comparison(zoloft,not_zoloft)
ip=PRR_comparison(zoloft,not_zoloft)

#chisq using patients, not cases no

c_table_sex=matrix(c(79, 419,300  ,679   ,3666,2072),ncol=3,byrow=TRUE)
colnames(c_table_sex)=c("U","F","M")
rownames(c_table_sex)=c("zoloft","not-zoloft")
chisq.test(c_table_sex)

#chisq test for indication + country, cases

c_table_ind=matrix(c(715, 1626,35  ,83   ,127,1, 5589,16177,421,674,665,177),ncol=6,byrow=TRUE)
colnames(c_table_ind)=c("Anxiety","Depression","Panic", "Post-traumatic","Obsessive-Compulsive","Other")
rownames(c_table_ind)=c("zoloft","not-zoloft")
chisq.test(c_table_ind)

#demographics tables
d_table_z=return_demographic_table(zoloft)
d_table_c=return_demographic_table(not_zoloft)

#ror/prr indications
AD_ROR=ROR_comparison(zoloft_anxiety,zoloft_depression)
AD_PRR=PRR_comparison(zoloft_anxiety,zoloft_depression)

#depression ROR/prr counts of patients
c_table_ind2=matrix(c(226, 521,5  ,19   ,41,1, 1267,4845,77,139,126,20),ncol=6,byrow=TRUE)
colnames(c_table_ind2)=c("Anxiety","Depression","Panic", "Post-traumatic","Obsessive-Compulsive","Other")
rownames(c_table_ind2)=c("zoloft","not-zoloft")
chisq.test(c_table_ind2,simulate.p.value=TRUE)

#chisquared weight

c_table_ind2=matrix(c(109 , 189,24  ,474   ,90, 156, 1423, 4746),ncol=4,byrow=TRUE)
chisq.test(c_table_ind2,simulate.p.value=FALSE)

#chisquared age
c_table_ind2=matrix(c(104 , 500,1  ,186   ,96, 4598, 89, 1596),ncol=4,byrow=TRUE)
chisq.test(c_table_ind2)

#export to excel
write_xlsx(ip, "zoloft_2024_PRR.xlsx")
write_xlsx(ic, "zoloft_2024_ROR.xlsx")
help_2022=count_qualifying_ADRs(ip_2022,ic_2022)
help_2023=count_qualifying_ADRs(ip_2023,ic_2023)
help_2024=count_qualifying_ADRs(ip_2024,ic_2024)

significant_adrs_merged=merge(merge(help_2022, help_2023,by="ADRs",all=TRUE), help_2024,by="ADRs",all=FALSE)
#significant_adrs_merged=na.omit(significant_adrs_merged)
#do venn diagram for all years, filter for how many meet criteria for 3/6, 4/6, 5/6 years
#venn diagrams
data1=lapply(zoloft_2023_adr$Var1, as.character)
data2=lapply(zoloft_2024_adr$Var1, as.character)
data3=lapply(zoloft_2022_adr$Var1, as.character)
data4=lapply(zoloft_2021_adr$Var1, as.character)
data5=lapply(zoloft_2020_adr$Var1, as.character)
#data6=lapply(zoloft_2019_adr$Var1, as.character)
venn.diagram(x=list(data1,data2,data3),category.names = c("2023","2024","2022"),filename = "overlapping_aes.png", col=c("#440154ff", '#21908dff', '#fde725ff'),fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3), alpha('#fde725ff',0.3)),)
venn.diagram(x=list(lapply(help_2022$ADRs, as.character),lapply(help_2023$ADRs, as.character),lapply(help_2024$ADRs, as.character)),category.names = c("2023","2024","2022"),filename = "overlap_adrs.png", col=c("#440154ff", '#21908dff', '#fde725ff'),fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3), alpha('#fde725ff',0.3)),)
venn.diagram(x=list(data1,data2,data3,data4,data5),
             category.names = c("2023","2024","2022","2021","2020"),
             filename = "overlapping_aes.png", 
             col=c("#440154ff", '#21908dff', '#fde725ff', '#F54927','#55E678'),
             fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3), alpha('#fde725ff',0.3),alpha('#F54927',0.3),alpha('#55E678',0.3)),)
venn.diagram(x=list(lapply(help_2022$ADRs, as.character),lapply(help_2023$ADRs, as.character),lapply(help_2024$ADRs, as.character),lapply(qualifying_adrs_2020$ADRs, as.character),lapply(qualifying_adrs_2021$ADRs, as.character)),
             category.names = c("2023","2024","2022","2021","2020"),
             filename = "overlap_adrs.png", 
             col=c("#440154ff", '#21908dff', '#fde725ff', '#F54927','#55E678'),
             fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3), alpha('#fde725ff',0.3),alpha('#F54927',0.3),alpha('#55E678',0.3)),)

write_xlsx(year_counts, "year_counts.xlsx")

overlap=calculate.overlap(x=list(lapply(help_2022$ADRs, as.character),lapply(help_2023$ADRs, as.character),lapply(help_2024$ADRs, as.character),lapply(qualifying_adrs_2020$ADRs, as.character),lapply(qualifying_adrs_2021$ADRs, as.character)))

qualifying_adrs_2020$year=2020
qualifying_adrs_2021$year=2021
qualifying_adrs_2023$year=2023
help_2022$year=2022
help_2024$year=2024
ADRs_allyears$year=2019
all_adrs=rbind(qualifying_adrs_2020, qualifying_adrs_2021,qualifying_adrs_2023,help_2022, help_2024)

write_xlsx(all_adrs, "all_years_and_pooled_adrs.xlsx")
#be warned this is fucked up because it has the pooled data mixed in, remove pooled data if you need to do year-by-year analysis
year_counts <- all_adrs %>%
  group_by(ADRs) %>%
  dplyr::summarise(year_count = n_distinct(year)) %>%
  filter(year_count >= 0)
hist(year_counts$year_count)
ggplot(year_counts, aes(x = year_count)) + 
  geom_histogram(colour = "black", fill = "deepskyblue3", 
                 bins = 5)+labs(
                   title="Histogram of Significant ADRs",
                   x="Number of Years ADRs Occur",
                   y="Number of ADRs") +theme(panel.background=element_rect(fill="lightblue"))

ggplot(all_adrs, aes(x = year)) + 
  geom_histogram(colour = "black", fill = "deepskyblue3", 
                 bins = 6)+labs(
                   title="Histogram of Significant ADRs",
                   x="Number of Years ADRs Occur",
                   y="Number of ADRs") +theme(panel.background=element_rect(fill="lightblue"))
ADRs_allyears=count_qualifying_ADRs(PRR_dataset = PRR_allyears_not_zoloft, ROR_dataset = ROR_allyears_not_zoloft)

test=merge(ADRs_allyears, year_counts,by="ADRs",all=TRUE)
test$year_count[is.na(test$year_count)]=0
ggplot(test, aes(x = year_count)) + 
  geom_histogram(colour = "black", fill = "deepskyblue3", 
                 bins = 5)+labs(
                   title="Comparison of Yearly ADRs vs Pooled (0 years)",
                   x="Years ADRs Occur",
                   y="Number of ADRs") +theme(panel.background=element_rect(fill="lightblue"))


names(ADRs_allyears)[names(ADRs_allyears) == "ADRs"] <- "pt"
test1=inner_join(ADRs_allyears, zoloft_allyears)

test=zoloft_allyears
test$newcol = duplicated(test[,c('age','sex', 'wt','pt','rept_dt')]) | duplicated(test[,c('age','sex', 'wt','pt','rept_dt')], fromLast = T)

medicationerror=zoloft_allyears[zoloft_allyears$pt=="Medication error",]
pulmonarytox=zoloft_allyears[zoloft_allyears$pt=="Pulmonary toxicity",]
pulmonarysten=zoloft_allyears[zoloft_allyears$pt=="Pulmonary artery stenosis",]

medicationerror_reports=return_all_reports(zoloft_allyears, medicationerror)
pulmonarytox_reports=return_all_reports(zoloft_allyears, pulmonarytox)
pulmonarysten_reports=return_all_reports(zoloft_allyears, pulmonarysten)