
faers_2024 <- retrieve_faersascii(ascii_dir = "C:/Users/kjinl/Documents/2024 tri1/research/2024/ASCII",drug_indication_pattern ="Anxiety|Depression|Premenstrual dysphoric disorder|Binge eating disorder|Post-traumatic stress disorder|PTSD|Generalized anxiety disorder|Bulimia|Depression|Major depressive disorder|Panic|OCD|Obsessive-compulsive disorder")
faers_data_unified_2024<- unify_tabular_ascii(ascii_list = faers_2024)
faers_data_unified_2024=remove_duplicate_cases(faers_data_unified_2024)[[1]]

zoloft_2024 <- filter( 
  faers_data_unified_2024,drugname_all == "SERTRALINE" | drugname_all == "ZOLOFT" |drugname_all == "SERTRALINE HYDROCHLORIDE"|drugname_all == "LUSTRAL") 

zoloft_2024_adr=as.data.frame(table(zoloft_2024$pt))
#WHOOPS SHIT THIS STILL HAS ZOLOFT IN IT
#rerun all nonzoloft and analysis
not_zoloft_2024=filter( 
  faers_data_unified_2024,drugname_all != "SERTRALINE" & drugname_all != "ZOLOFT" &drugname_all != "SERTRALINE HYDROCHLORIDE"&drugname_all != "LUSTRAL") 
#not_zoloft_summary=summary_faersdata(not_zoloft)
#of patients taking zoloft
length(unique(zoloft_2024[["caseid"]]))
zoloft_unique_2024=distinct(zoloft_2024, primaryid, .keep_all = TRUE)
#gender/indication breakdown of zoloft takers
zoloft_sex_2024=as.data.frame(table(zoloft_unique_2024$sex))
zoloft_indi_2024=as.data.frame(table(zoloft_unique_2024$indi_pt_all))
zoloft_indi_final_2024=count_indications (c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual "),zoloft_2024)
zoloft_report_country_2024=as.data.frame(table(zoloft_unique_2024$reporter_country))
zoloft_occr_country_2024=as.data.frame(table(zoloft_unique_2024$occr_country))
not_zoloft_unique_2024=distinct(not_zoloft_2024, primaryid, .keep_all = TRUE)

not_zoloft_report_country_2024=as.data.frame(table(not_zoloft_unique_2024$reporter_country))
not_zoloft_occr_country_2024=as.data.frame(table(not_zoloft_unique_2024$occr_country))



#indication work

zoloft_unique_druginfo_2024=rbindlist(zoloft_unique_2024$patient_drug)



zoloft_unique_druginfo_2024$dose_amt=zoloft_unique_druginfo_2024$dose_amt%>%as.double()
hist(zoloft_unique_druginfo_2024$dose_amt)
#zoloft_no_overdose=zoloft %>%filter("dose_amt"<900)
#age breakdown 
hist(zoloft_unique_2024$age)
summary(zoloft_unique_2024$age)
#AE table not zoloft
not_zoloft_adr=as.data.frame(table(not_zoloft_2024$pt))
length(unique(not_zoloft_2024[["caseid"]]))

#not zoloft info
not_zoloft_druglist=as.data.frame(table(not_zoloft_2024$drugname_ps))
not_zoloft_unique_druginfo_2024=rbindlist(not_zoloft_unique_2024$patient_drug)
not_zoloft_sex=as.data.frame(table(not_zoloft_unique_2024$sex))
not_zoloft_indi=as.data.frame(table(not_zoloft_unique_2024$indi_pt_all))
not_zoloft_unique_druginfo_2024$dose_amt=not_zoloft_unique_druginfo_2024$dose_amt%>%as.double()
not_zoloft_indi_2024=count_indications(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual","Bulimia"),not_zoloft_2024)

hist(not_zoloft_unique_druginfo_2024$dose_amt)
hist(not_zoloft_unique_2024$age)


zoloft_adr_rate_2024=adr_rate(zoloft_2024,cutoff=1)
not_zoloft_adr_rate_2024=adr_rate(not_zoloft_2024)
zoloft_indi_2024=count_indications(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual","Bulimia"),zoloft_2024)

zoloft_split_2024=(split(zoloft_2024, zoloft_2024$indi_pt_all))
zoloft_anxiety_2024=bind_rows(zoloft_split_2024$Anxiety,zoloft_split_2024$`Anxiety disorder`,zoloft_split_2024$`Anxiety/Depression`,zoloft_split_2024$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2024$`Anxiety/Obsessive-compulsive disorder`)
zoloft_depression_2024=bind_rows(zoloft_split_2024$Depression, zoloft_split_2024$`Depression suicidal`,zoloft_split_2024$`Depression/Obsessive-compulsive disorder`,zoloft_split_2024$`Depression/Panic attack`,zoloft_split_2024$`Anxiety/Depression`,zoloft_split_2024$`Anxiety/Depression/Post-traumatic stress disorder`)
zoloft_other=bind_rows(zoloft_split_2024$`Panic attack`,zoloft_split_2024$`Panic disorder`, zoloft_split_2024$`Depression/Panic attack`,zoloft_split_2024$`Post-traumatic stress disorder`,zoloft_split_2024$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2024$`Depression/Obsessive-compulsive disorder`,zoloft_split_2024$`Obsessive-compulsive disorder`)  
zoloft_anxiety_adr=as.data.frame(table(zoloft_anxiety$pt))
zoloft_depression_adr=as.data.frame(table(zoloft_depression$pt))

not_zoloft_split_2024=(split(not_zoloft, not_zoloft$indi_pt_all))
not_zoloft_anxiety_2024=bind_rows(not_zoloft_split_2024$Anxiety,not_zoloft_split_2024$`Anxiety disorder`,not_zoloft_split_2024$`Anxiety/Depression`,not_zoloft_split_2024$`Anxiety/Depression/Post-traumatic stress disorder`,not_zoloft_split_2024$`Anxiety/Obsessive-compulsive disorder`)
not_zoloft_depression_2024=bind_rows(not_zoloft_split_2024$Depression, not_zoloft_split_2024$`Depression suicidal`,not_zoloft_split_2024$`Depression/Obsessive-compulsive disorder`,not_zoloft_split_2024$`Depression/Panic attack`,not_zoloft_split_2024$`Anxiety/Depression`,not_zoloft_split_2024$`Anxiety/Depression/Post-traumatic stress disorder`)
not_zoloft_other_2024=bind_rows(zoloft_split_2024$`Panic attack`,zoloft_split_2024$`Panic disorder`, zoloft_split_2024$`Depression/Panic attack`,zoloft_split_2024$`Post-traumatic stress disorder`,zoloft_split_2024$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2024$`Depression/Obsessive-compulsive disorder`,zoloft_split_2024$`Obsessive-compulsive disorder`)  
not_zoloft_anxiety_adr_2024=as.data.frame(table(not_zoloft_anxiety_2024$pt))
not_zoloft_depression_adr_2024=as.data.frame(table(not_zoloft_depression_2024$pt))


#zoloft_anxiety_2024=bind_rows(zoloft_split_2024$Anxiety,zoloft_split_2024$`Anxiety disorder`,zoloft_split_2024$`Anxiety/Depression`,zoloft_split_2024$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2024$`Anxiety/Obsessive-compulsive disorder`)
#zoloft_depression=bind_rows(zoloft_split_2024$Depression, zoloft_split_2024$`Depression suicidal`,zoloft_split_2024$`Depression/Obsessive-compulsive disorder`,zoloft_split_2024$`Depression/Panic attack`,zoloft_split_2024$`Anxiety/Depression`,zoloft_split_2024$`Anxiety/Depression/Post-traumatic stress disorder`)

#compare demographics

zoloft_sex_2024=as.data.frame(table(zoloft_unique_2024$sex))
#zoloft_indi=as.data.frame(table(zoloft_unique_2024$indi_pt_all))
#zoloft_indi_final=count_indications(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual "),zoloft)
#report_country=as.data.frame(table(zoloft_unique$reporter_country))
#occr_country=as.data.frame(table(zoloft_unique$occr_country))


zoloft_unique_druginfo_2024=rbindlist(zoloft_unique_2024$patient_drug)



zoloft_unique_druginfo_2024$dose_amt=zoloft_unique_druginfo_2024$dose_amt%>%as.double()
hist(zoloft_unique_druginfo_2024$dose_amt)
#zoloft_no_overdose=zoloft %>%filter("dose_amt"<900)
#age breakdown 
hist(zoloft_unique_2024$age)
summary(zoloft_unique_2024$age)

#compare adr rates between indications

ic_2024=ROR_comparison(zoloft_2024,not_zoloft_2024,TRUE,3)
ip_2024=PRR_comparison(zoloft_2024,not_zoloft_2024, TRUE,3)

#find all qualifying adrs for this year
qualifying_adrs_2024=count_qualifying_ADRs(ip_2024,ic_2024)
#chisq using patients, not cases no

c_table_sex=matrix(c(83,641,287  ,521   ,4435,2258),ncol=3,byrow=TRUE)
colnames(c_table_sex)=c("U","F","M")
rownames(c_table_sex)=c("zoloft","not-zoloft")
chisq.test(c_table_sex)

#chisq test for indication + country, cases

c_table_ind=matrix(c(715, 1626,35  ,83   ,127,1, 5589,16177,421,674,665,177),ncol=6,byrow=TRUE)
colnames(c_table_ind)=c("Anxiety","Depression","Panic", "Post-traumatic","Obsessive-Compulsive","Other")
rownames(c_table_ind)=c("zoloft","not-zoloft")
chisq.test(c_table_ind)

#demographics tables
d_table_z_2024=return_demographic_table(zoloft_2024)
d_table_c_2024=return_demographic_table(not_zoloft_2024)

#ror/prr indications
AD_ROR=ROR_comparison(zoloft_anxiety_2024,zoloft_depression_2024,TRUE,3))
AD_PRR=PRR_comparison(zoloft_anxiety_2024,zoloft_depression_2024,TRUE,3))

#depression ROR/prr counts of patients
c_table_ind2=matrix(c(243, 460,13  ,11   ,28,0, 1747,5140,136,838,152,29),ncol=6,byrow=TRUE)
colnames(c_table_ind2)=c("Anxiety","Depression","Panic", "Post-traumatic","Obsessive-Compulsive","Other")
rownames(c_table_ind2)=c("zoloft","not-zoloft")
chisq.test(c_table_ind2,simulate.p.value=TRUE)

#chisquRred sexx
c_table_ind2=matrix(c(641,287,83,4435,2258,521),ncol=3,byrow=TRUE)
chisq.test(c_table_ind2)

#chisquared weight

c_table_ind2=matrix(c(96,180,163,564,155,116,2498,4443),ncol=4,byrow=TRUE)
chisq.test(c_table_ind2,simulate.p.value=FALSE)

#chisquared age
c_table_ind2=matrix(c(104,653,2,213,95,4492,739,1813),ncol=4,byrow=TRUE)
chisq.test(c_table_ind2)
#chisquared indications by patient
c_table_ind2=matrix(c(622,294,72,20,22,3,4966,1853,166,166,161,24),ncol=6,byrow=TRUE)
chisq.test(c_table_ind2,simulate.p.value = TRUE)

c_table_ind2=matrix(c(2051,1136,250,65,67,9,20128,8953,795,825,1000,100),ncol=6,byrow=TRUE)
chisq.test(c_table_ind2,simulate.p.value = TRUE)
