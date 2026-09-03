
faers_2019 <- retrieve_faersascii(ascii_dir = "C:/Users/kjinl/Documents/2024 tri1/research/2019/ASCII",drug_indication_pattern ="Anxiety|Depression|Premenstrual dysphoric disorder|Binge eating disorder|Post-traumatic stress disorder|PTSD|Generalized anxiety disorder|Bulimia|Depression|Major depressive disorder|Panic|OCD|Obsessive-compulsive disorder")
faers_data_unified_2019 <- unify_tabular_ascii(ascii_list = faers_2019)

zoloft_2019 <- filter( 
  faers_data_unified_2019,drugname_all == "SERTRALINE" | drugname_all == "ZOLOFT" |drugname_all == "SERTRALINE HYDROCHLORIDE"|drugname_all == "LUSTRAL") 

zoloft_2019_adr=as.data.frame(table(zoloft_2019$pt))
#WHOOPS SHIT THIS STILL HAS ZOLOFT IN IT
#rerun all nonzoloft and analysis
not_zoloft_2019=filter( 
  faers_data_unified_2019,drugname_all != "SERTRALINE" & drugname_all != "ZOLOFT" &drugname_all != "SERTRALINE HYDROCHLORIDE"&drugname_all != "LUSTRAL") 
#not_zoloft_summary=summary_faersdata(not_zoloft)
#of patients taking zoloft
length(unique(zoloft_2019[["caseid"]]))
zoloft_unique_2019=distinct(zoloft_2019, primaryid, .keep_all = TRUE)
#gender/indication breakdown of zoloft takers
zoloft_sex_2019=as.data.frame(table(zoloft_unique_2019$sex))
zoloft_indi_2019=as.data.frame(table(zoloft_unique_2019$indi_pt_all))
zoloft_indi_final_2019=count_indications (c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual "),zoloft_2019)
zoloft_report_country_2019=as.data.frame(table(zoloft_unique_2019$reporter_country))
zoloft_occr_country_2019=as.data.frame(table(zoloft_unique_2019$occr_country))
not_zoloft_unique_2019=distinct(not_zoloft_2019, primaryid, .keep_all = TRUE)

not_zoloft_report_country_2019=as.data.frame(table(not_zoloft_unique_2019$reporter_country))
not_zoloft_occr_country_2019=as.data.frame(table(not_zoloft_unique_2019$occr_country))



#indication work

zoloft_unique_druginfo_2019=rbindlist(zoloft_unique_2019$patient_drug)



zoloft_unique_druginfo_2019$dose_amt=zoloft_unique_druginfo_2019$dose_amt%>%as.double()
hist(zoloft_unique_druginfo_2019$dose_amt)
#zoloft_no_overdose=zoloft %>%filter("dose_amt"<900)
#age breakdown 
hist(zoloft_unique_2019$age)
summary(zoloft_unique_2019$age)
#AE table not zoloft
not_zoloft_adr=as.data.frame(table(not_zoloft_2019$pt))
length(unique(not_zoloft_2019[["caseid"]]))

#not zoloft info
not_zoloft_druglist=as.data.frame(table(not_zoloft_2019$drugname_ps))
not_zoloft_unique_druginfo_2019=rbindlist(not_zoloft_unique_2019$patient_drug)
not_zoloft_sex=as.data.frame(table(not_zoloft_unique_2019$sex))
not_zoloft_indi=as.data.frame(table(not_zoloft_unique_2019$indi_pt_all))
not_zoloft_unique_druginfo_2019$dose_amt=not_zoloft_unique_druginfo_2019$dose_amt%>%as.double()
not_zoloft_indi_2019=count_indications(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual","Bulimia"),not_zoloft_2019)

hist(not_zoloft_unique_druginfo_2019$dose_amt)
hist(not_zoloft_unique_2019$age)


zoloft_adr_rate_2019=adr_rate(zoloft_2019)
not_zoloft_adr_rate_2019=adr_rate(not_zoloft_2019)
zoloft_indi_2019=count_indications(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual","Bulimia"),zoloft_2019)

zoloft_split_2019=(split(zoloft_2019, zoloft_2019$indi_pt_all))
zoloft_anxiety_2019=bind_rows(zoloft_split_2019$Anxiety,zoloft_split_2019$`Anxiety disorder`,zoloft_split_2019$`Anxiety/Depression`,zoloft_split_2019$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2019$`Anxiety/Obsessive-compulsive disorder`)
zoloft_depression_2019=bind_rows(zoloft_split_2019$Depression, zoloft_split_2019$`Depression suicidal`,zoloft_split_2019$`Depression/Obsessive-compulsive disorder`,zoloft_split_2019$`Depression/Panic attack`,zoloft_split_2019$`Anxiety/Depression`,zoloft_split_2019$`Anxiety/Depression/Post-traumatic stress disorder`)
zoloft_other=bind_rows(zoloft_split_2019$`Panic attack`,zoloft_split_2019$`Panic disorder`, zoloft_split_2019$`Depression/Panic attack`,zoloft_split_2019$`Post-traumatic stress disorder`,zoloft_split_2019$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2019$`Depression/Obsessive-compulsive disorder`,zoloft_split_2019$`Obsessive-compulsive disorder`)  
zoloft_anxiety_adr=as.data.frame(table(zoloft_anxiety$pt))
zoloft_depression_adr=as.data.frame(table(zoloft_depression$pt))

not_zoloft_split_2019=(split(not_zoloft, not_zoloft$indi_pt_all))
not_zoloft_anxiety_2019=bind_rows(not_zoloft_split_2019$Anxiety,not_zoloft_split_2019$`Anxiety disorder`,not_zoloft_split_2019$`Anxiety/Depression`,not_zoloft_split_2019$`Anxiety/Depression/Post-traumatic stress disorder`,not_zoloft_split_2019$`Anxiety/Obsessive-compulsive disorder`)
not_zoloft_depression_2019=bind_rows(not_zoloft_split_2019$Depression, not_zoloft_split_2019$`Depression suicidal`,not_zoloft_split_2019$`Depression/Obsessive-compulsive disorder`,not_zoloft_split_2019$`Depression/Panic attack`,not_zoloft_split_2019$`Anxiety/Depression`,not_zoloft_split_2019$`Anxiety/Depression/Post-traumatic stress disorder`)
not_zoloft_other_2019=bind_rows(zoloft_split_2019$`Panic attack`,zoloft_split_2019$`Panic disorder`, zoloft_split_2019$`Depression/Panic attack`,zoloft_split_2019$`Post-traumatic stress disorder`,zoloft_split_2019$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2019$`Depression/Obsessive-compulsive disorder`,zoloft_split_2019$`Obsessive-compulsive disorder`)  
not_zoloft_anxiety_adr_2019=as.data.frame(table(not_zoloft_anxiety_2019$pt))
not_zoloft_depression_adr_2019=as.data.frame(table(not_zoloft_depression_2019$pt))


#zoloft_anxiety_2019=bind_rows(zoloft_split_2019$Anxiety,zoloft_split_2019$`Anxiety disorder`,zoloft_split_2019$`Anxiety/Depression`,zoloft_split_2019$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2019$`Anxiety/Obsessive-compulsive disorder`)
#zoloft_depression=bind_rows(zoloft_split_2019$Depression, zoloft_split_2019$`Depression suicidal`,zoloft_split_2019$`Depression/Obsessive-compulsive disorder`,zoloft_split_2019$`Depression/Panic attack`,zoloft_split_2019$`Anxiety/Depression`,zoloft_split_2019$`Anxiety/Depression/Post-traumatic stress disorder`)

#compare demographics

zoloft_sex_2019=as.data.frame(table(zoloft_unique_2019$sex))
#zoloft_indi=as.data.frame(table(zoloft_unique_2019$indi_pt_all))
#zoloft_indi_final=count_indications(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual "),zoloft)
#report_country=as.data.frame(table(zoloft_unique$reporter_country))
#occr_country=as.data.frame(table(zoloft_unique$occr_country))


zoloft_unique_druginfo_2019=rbindlist(zoloft_unique_2019$patient_drug)



zoloft_unique_druginfo_2019$dose_amt=zoloft_unique_druginfo_2019$dose_amt%>%as.double()
hist(zoloft_unique_druginfo_2019$dose_amt)
#zoloft_no_overdose=zoloft %>%filter("dose_amt"<900)
#age breakdown 
hist(zoloft_unique_2019$age)
summary(zoloft_unique_2019$age)

#compare adr rates between indications

ic_2019=ROR_comparison(zoloft_2019,not_zoloft_2019)
ip_2019=PRR_comparison(zoloft_2019,not_zoloft_2019)

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
d_table_z_2019=return_demographic_table(zoloft_2019)
d_table_c_2019=return_demographic_table(not_zoloft_2019)

#testing gender for demographcis table
#26 cases 5 people
data=not_zoloft_2019[not_zoloft_2019$sex=="P",]
#8 case, 4 people
data=not_zoloft_2019[not_zoloft_2019$sex=="T",]

#ror/prr indications
AD_ROR=ROR_comparison(zoloft_anxiety_2019,zoloft_depression_2019)
AD_PRR=PRR_comparison(zoloft_anxiety_2019,zoloft_depression_2019)

#depression ROR/prr counts of patients
c_table_ind2=matrix(c(243, 460,13  ,11   ,28,0, 1747,5140,136,838,152,29),ncol=6,byrow=TRUE)
colnames(c_table_ind2)=c("Anxiety","Depression","Panic", "Post-traumatic","Obsessive-Compulsive","Other")
rownames(c_table_ind2)=c("zoloft","not-zoloft")
chisq.test(c_table_ind2,simulate.p.value=TRUE)
#chisquRred sexx
c_table_ind2=matrix(c(782 , 391,244  ,7167   ,3465, 1277),ncol=3,byrow=TRUE)
chisq.test(c_table_ind2,simulate.p.value=FALSE)

#chisquared weight

c_table_ind2=matrix(c(123,163,321,808,121,115,4415,7254),ncol=4,byrow=TRUE)
chisq.test(c_table_ind2,simulate.p.value=FALSE)

#chisquared age
c_table_ind2=matrix(c(170,902,3,325,151,6676,1152,3829),ncol=4,byrow=TRUE)
chisq.test(c_table_ind2)


#chisquared indications by patient
c_table_ind2=matrix(c(999,319,59,16,32,1,7742,3347,262,221,473,26),ncol=6,byrow=TRUE)
chisq.test(c_table_ind2,simulate.p.value = TRUE)

c_table_ind2=matrix(c(3382,1284,41,79,473,14,29188,14580,1287,1054,2113,152),ncol=6,byrow=TRUE)
chisq.test(c_table_ind2,simulate.p.value = TRUE)

#find all qualifying adrs for this year
qualifying_adrs_2019=count_qualifying_ADRs(ip_2019,ic_2019)
