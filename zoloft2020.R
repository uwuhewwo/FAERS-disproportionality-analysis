
faers_2020 <- retrieve_faersascii(ascii_dir = "C:/Users/kjinl/Documents/2024 tri1/research/2020/ASCII",drug_indication_pattern ="Anxiety|Depression|Premenstrual dysphoric disorder|Binge eating disorder|Post-traumatic stress disorder|PTSD|Generalized anxiety disorder|Bulimia|Depression|Major depressive disorder|Panic|OCD|Obsessive-compulsive disorder")
faers_data_unified_2020 <- unify_tabular_ascii(ascii_list = faers_2020)
faers_data_unified_2020=remove_duplicate_cases(faers_data_unified_2020)[[1]]

zoloft_2020 <- filter( 
  faers_data_unified_2020,drugname_all == "SERTRALINE" | drugname_all == "ZOLOFT" |drugname_all == "SERTRALINE HYDROCHLORIDE"|drugname_all == "LUSTRAL") 

zoloft_2020_adr=as.data.frame(table(zoloft_2020$pt))
#WHOOPS SHIT THIS STILL HAS ZOLOFT IN IT
#rerun all nonzoloft and analysis
not_zoloft_2020=filter( 
  faers_data_unified_2020,drugname_all != "SERTRALINE" & drugname_all != "ZOLOFT" &drugname_all != "SERTRALINE HYDROCHLORIDE"&drugname_all != "LUSTRAL") 
#not_zoloft_summary=summary_faersdata(not_zoloft)
#of patients taking zoloft
length(unique(zoloft_2020[["caseid"]]))
zoloft_unique_2020=distinct(zoloft_2020, primaryid, .keep_all = TRUE)
#gender/indication breakdown of zoloft takers
zoloft_sex_2020=as.data.frame(table(zoloft_unique_2020$sex))
zoloft_indi_2020=as.data.frame(table(zoloft_unique_2020$indi_pt_all))
zoloft_indi_final_2020=count_indications (c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual "),zoloft_2020)
zoloft_report_country_2020=as.data.frame(table(zoloft_unique_2020$reporter_country))
zoloft_occr_country_2020=as.data.frame(table(zoloft_unique_2020$occr_country))
not_zoloft_unique_2020=distinct(not_zoloft_2020, primaryid, .keep_all = TRUE)

not_zoloft_report_country_2020=as.data.frame(table(not_zoloft_unique_2020$reporter_country))
not_zoloft_occr_country_2020=as.data.frame(table(not_zoloft_unique_2020$occr_country))



#indication work

zoloft_unique_druginfo_2020=rbindlist(zoloft_unique_2020$patient_drug)



zoloft_unique_druginfo_2020$dose_amt=zoloft_unique_druginfo_2020$dose_amt%>%as.double()
hist(zoloft_unique_druginfo_2020$dose_amt)
#zoloft_no_overdose=zoloft %>%filter("dose_amt"<900)
#age breakdown 
hist(zoloft_unique_2020$age)
summary(zoloft_unique_2020$age)
#AE table not zoloft
not_zoloft_adr=as.data.frame(table(not_zoloft_2020$pt))
length(unique(not_zoloft_2020[["caseid"]]))

#not zoloft info
not_zoloft_druglist=as.data.frame(table(not_zoloft_2020$drugname_ps))
not_zoloft_unique_druginfo_2020=rbindlist(not_zoloft_unique_2020$patient_drug)
not_zoloft_sex=as.data.frame(table(not_zoloft_unique_2020$sex))
not_zoloft_indi=as.data.frame(table(not_zoloft_unique_2020$indi_pt_all))
not_zoloft_unique_druginfo_2020$dose_amt=not_zoloft_unique_druginfo_2020$dose_amt%>%as.double()
not_zoloft_indi_2020=count_indications(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual","Bulimia"),not_zoloft_2020)

hist(not_zoloft_unique_druginfo_2020$dose_amt)
hist(not_zoloft_unique_2020$age)


zoloft_adr_rate_2020=adr_rate(zoloft_2020)
not_zoloft_adr_rate_2020=adr_rate(not_zoloft_2020)
zoloft_indi_2020=count_indications(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual","Bulimia"),zoloft_2020)

zoloft_split_2020=(split(zoloft_2020, zoloft_2020$indi_pt_all))
zoloft_anxiety_2020=bind_rows(zoloft_split_2020$Anxiety,zoloft_split_2020$`Anxiety disorder`,zoloft_split_2020$`Anxiety/Depression`,zoloft_split_2020$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2020$`Anxiety/Obsessive-compulsive disorder`)
zoloft_depression_2020=bind_rows(zoloft_split_2020$Depression, zoloft_split_2020$`Depression suicidal`,zoloft_split_2020$`Depression/Obsessive-compulsive disorder`,zoloft_split_2020$`Depression/Panic attack`,zoloft_split_2020$`Anxiety/Depression`,zoloft_split_2020$`Anxiety/Depression/Post-traumatic stress disorder`)
zoloft_other=bind_rows(zoloft_split_2020$`Panic attack`,zoloft_split_2020$`Panic disorder`, zoloft_split_2020$`Depression/Panic attack`,zoloft_split_2020$`Post-traumatic stress disorder`,zoloft_split_2020$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2020$`Depression/Obsessive-compulsive disorder`,zoloft_split_2020$`Obsessive-compulsive disorder`)  
zoloft_anxiety_adr=as.data.frame(table(zoloft_anxiety$pt))
zoloft_depression_adr=as.data.frame(table(zoloft_depression$pt))

not_zoloft_split_2020=(split(not_zoloft, not_zoloft$indi_pt_all))
not_zoloft_anxiety_2020=bind_rows(not_zoloft_split_2020$Anxiety,not_zoloft_split_2020$`Anxiety disorder`,not_zoloft_split_2020$`Anxiety/Depression`,not_zoloft_split_2020$`Anxiety/Depression/Post-traumatic stress disorder`,not_zoloft_split_2020$`Anxiety/Obsessive-compulsive disorder`)
not_zoloft_depression_2020=bind_rows(not_zoloft_split_2020$Depression, not_zoloft_split_2020$`Depression suicidal`,not_zoloft_split_2020$`Depression/Obsessive-compulsive disorder`,not_zoloft_split_2020$`Depression/Panic attack`,not_zoloft_split_2020$`Anxiety/Depression`,not_zoloft_split_2020$`Anxiety/Depression/Post-traumatic stress disorder`)
not_zoloft_other_2020=bind_rows(zoloft_split_2020$`Panic attack`,zoloft_split_2020$`Panic disorder`, zoloft_split_2020$`Depression/Panic attack`,zoloft_split_2020$`Post-traumatic stress disorder`,zoloft_split_2020$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2020$`Depression/Obsessive-compulsive disorder`,zoloft_split_2020$`Obsessive-compulsive disorder`)  
not_zoloft_anxiety_adr_2020=as.data.frame(table(not_zoloft_anxiety_2020$pt))
not_zoloft_depression_adr_2020=as.data.frame(table(not_zoloft_depression_2020$pt))


#zoloft_anxiety_2020=bind_rows(zoloft_split_2020$Anxiety,zoloft_split_2020$`Anxiety disorder`,zoloft_split_2020$`Anxiety/Depression`,zoloft_split_2020$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2020$`Anxiety/Obsessive-compulsive disorder`)
#zoloft_depression=bind_rows(zoloft_split_2020$Depression, zoloft_split_2020$`Depression suicidal`,zoloft_split_2020$`Depression/Obsessive-compulsive disorder`,zoloft_split_2020$`Depression/Panic attack`,zoloft_split_2020$`Anxiety/Depression`,zoloft_split_2020$`Anxiety/Depression/Post-traumatic stress disorder`)

#compare demographics

zoloft_sex_2020=as.data.frame(table(zoloft_unique_2020$sex))
#zoloft_indi=as.data.frame(table(zoloft_unique_2020$indi_pt_all))
#zoloft_indi_final=count_indications(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual "),zoloft)
#report_country=as.data.frame(table(zoloft_unique$reporter_country))
#occr_country=as.data.frame(table(zoloft_unique$occr_country))


zoloft_unique_druginfo_2020=rbindlist(zoloft_unique_2020$patient_drug)



zoloft_unique_druginfo_2020$dose_amt=zoloft_unique_druginfo_2020$dose_amt%>%as.double()
hist(zoloft_unique_druginfo_2020$dose_amt)
#zoloft_no_overdose=zoloft %>%filter("dose_amt"<900)
#age breakdown 
hist(zoloft_unique_2020$age)
summary(zoloft_unique_2020$age)

#compare adr rates between indications

ic_2020=ROR_comparison(zoloft_2020,not_zoloft_2020)
ip_2020=PRR_comparison(zoloft_2020,not_zoloft_2020)

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
d_table_z_2020=return_demographic_table(zoloft_2020)
d_table_c_2020=return_demographic_table(not_zoloft_2020)

#ror/prr indications
AD_ROR=ROR_comparison(zoloft_anxiety_2020,zoloft_depression_2020)
AD_PRR=PRR_comparison(zoloft_anxiety_2020,zoloft_depression_2020)

#depression ROR/prr counts of patients
c_table_ind2=matrix(c(243, 460,13  ,11   ,28,0, 1747,5140,136,838,152,29),ncol=6,byrow=TRUE)
colnames(c_table_ind2)=c("Anxiety","Depression","Panic", "Post-traumatic","Obsessive-Compulsive","Other")
rownames(c_table_ind2)=c("zoloft","not-zoloft")
chisq.test(c_table_ind2,simulate.p.value=TRUE)

#chisquRred sexx
c_table_ind2=matrix(c(722,384,85,6038,3087,587),ncol=3,byrow=TRUE)
chisq.test(c_table_ind2)

#chisquared weight

c_table_ind2=matrix(c(126,162,376,519,121,115,4415,7254),ncol=4,byrow=TRUE)
chisq.test(c_table_ind2,simulate.p.value=FALSE)

#chisquared age
c_table_ind2=matrix(c(87,873,20,197,121,115,4415,7254),ncol=4,byrow=TRUE)
chisq.test(c_table_ind2)


#find all qualifying adrs for this year
qualifying_adrs_2020=count_qualifying_ADRs(ip_2020,ic_2020)