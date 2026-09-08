
faers_2021 <- retrieve_faersascii(ascii_dir = "C:/Users/kjinl/Documents/2024 tri1/research/2021/ASCII",drug_indication_pattern ="Anxiety|Depression|Premenstrual dysphoric disorder|Binge eating disorder|Post-traumatic stress disorder|PTSD|Generalized anxiety disorder|Bulimia|Depression|Major depressive disorder|Panic|OCD|Obsessive-compulsive disorder")
faers_data_unified_2021 <- unify_tabular_ascii(ascii_list = faers_2021)
faers_data_unified_2021=remove_duplicate_cases(faers_data_unified_2021)[[1]]

zoloft_2021 <- filter( 
  faers_data_unified_2021,drugname_all == "SERTRALINE" | drugname_all == "ZOLOFT" |drugname_all == "SERTRALINE HYDROCHLORIDE"|drugname_all == "LUSTRAL") 

zoloft_2021_adr=as.data.frame(table(zoloft_2021$pt))
#WHOOPS SHIT THIS STILL HAS ZOLOFT IN IT
#rerun all nonzoloft and analysis
not_zoloft_2021=filter( 
  faers_data_unified_2021,drugname_all != "SERTRALINE" & drugname_all != "ZOLOFT" &drugname_all != "SERTRALINE HYDROCHLORIDE"&drugname_all != "LUSTRAL") 
#not_zoloft_summary=summary_faersdata(not_zoloft)
#of patients taking zoloft
length(unique(zoloft_2021[["caseid"]]))
zoloft_unique_2021=distinct(zoloft_2021, primaryid, .keep_all = TRUE)
#gender/indication breakdown of zoloft takers
zoloft_sex_2021=as.data.frame(table(zoloft_unique_2021$sex))
zoloft_indi_2021=as.data.frame(table(zoloft_unique_2021$indi_pt_all))
zoloft_indi_final_2021=count_indications (c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual "),zoloft_2021)
zoloft_report_country_2021=as.data.frame(table(zoloft_unique_2021$reporter_country))
zoloft_occr_country_2021=as.data.frame(table(zoloft_unique_2021$occr_country))
not_zoloft_unique_2021=distinct(not_zoloft_2021, primaryid, .keep_all = TRUE)

not_zoloft_report_country_2021=as.data.frame(table(not_zoloft_unique_2021$reporter_country))
not_zoloft_occr_country_2021=as.data.frame(table(not_zoloft_unique_2021$occr_country))



#indication work

zoloft_unique_druginfo_2021=rbindlist(zoloft_unique_2021$patient_drug)



zoloft_unique_druginfo_2021$dose_amt=zoloft_unique_druginfo_2021$dose_amt%>%as.double()
#hist(zoloft_unique_druginfo_2021$dose_amt)
#zoloft_no_overdose=zoloft %>%filter("dose_amt"<900)
#age breakdown 
#hist(zoloft_unique_2021$age)
summary(zoloft_unique_2021$age)
#AE table not zoloft
not_zoloft_adr=as.data.frame(table(not_zoloft_2021$pt))
length(unique(not_zoloft_2021[["caseid"]]))

#not zoloft info
not_zoloft_druglist=as.data.frame(table(not_zoloft_2021$drugname_ps))
not_zoloft_unique_druginfo_2021=rbindlist(not_zoloft_unique_2021$patient_drug)
not_zoloft_sex=as.data.frame(table(not_zoloft_unique_2021$sex))
not_zoloft_indi=as.data.frame(table(not_zoloft_unique_2021$indi_pt_all))
not_zoloft_unique_druginfo_2021$dose_amt=not_zoloft_unique_druginfo_2021$dose_amt%>%as.double()
not_zoloft_indi_2021=count_indications(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual","Bulimia"),not_zoloft_2021)

#hist(not_zoloft_unique_druginfo_2021$dose_amt)
#hist(not_zoloft_unique_2021$age)


zoloft_adr_rate_2021=adr_rate(zoloft_2021)
not_zoloft_adr_rate_2021=adr_rate(not_zoloft_2021)
zoloft_indi_2021=count_indications(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual","Bulimia"),zoloft_2021)

zoloft_split_2021=(split(zoloft_2021, zoloft_2021$indi_pt_all))
zoloft_anxiety_2021=bind_rows(zoloft_split_2021$Anxiety,zoloft_split_2021$`Anxiety disorder`,zoloft_split_2021$`Anxiety/Depression`,zoloft_split_2021$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2021$`Anxiety/Obsessive-compulsive disorder`)
zoloft_depression_2021=bind_rows(zoloft_split_2021$Depression, zoloft_split_2021$`Depression suicidal`,zoloft_split_2021$`Depression/Obsessive-compulsive disorder`,zoloft_split_2021$`Depression/Panic attack`,zoloft_split_2021$`Anxiety/Depression`,zoloft_split_2021$`Anxiety/Depression/Post-traumatic stress disorder`)
zoloft_other=bind_rows(zoloft_split_2021$`Panic attack`,zoloft_split_2021$`Panic disorder`, zoloft_split_2021$`Depression/Panic attack`,zoloft_split_2021$`Post-traumatic stress disorder`,zoloft_split_2021$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2021$`Depression/Obsessive-compulsive disorder`,zoloft_split_2021$`Obsessive-compulsive disorder`)  
zoloft_anxiety_adr=as.data.frame(table(zoloft_anxiety$pt))
zoloft_depression_adr=as.data.frame(table(zoloft_depression$pt))

not_zoloft_split_2021=(split(not_zoloft, not_zoloft$indi_pt_all))
not_zoloft_anxiety_2021=bind_rows(not_zoloft_split_2021$Anxiety,not_zoloft_split_2021$`Anxiety disorder`,not_zoloft_split_2021$`Anxiety/Depression`,not_zoloft_split_2021$`Anxiety/Depression/Post-traumatic stress disorder`,not_zoloft_split_2021$`Anxiety/Obsessive-compulsive disorder`)
not_zoloft_depression_2021=bind_rows(not_zoloft_split_2021$Depression, not_zoloft_split_2021$`Depression suicidal`,not_zoloft_split_2021$`Depression/Obsessive-compulsive disorder`,not_zoloft_split_2021$`Depression/Panic attack`,not_zoloft_split_2021$`Anxiety/Depression`,not_zoloft_split_2021$`Anxiety/Depression/Post-traumatic stress disorder`)
not_zoloft_other_2021=bind_rows(zoloft_split_2021$`Panic attack`,zoloft_split_2021$`Panic disorder`, zoloft_split_2021$`Depression/Panic attack`,zoloft_split_2021$`Post-traumatic stress disorder`,zoloft_split_2021$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2021$`Depression/Obsessive-compulsive disorder`,zoloft_split_2021$`Obsessive-compulsive disorder`)  
not_zoloft_anxiety_adr_2021=as.data.frame(table(not_zoloft_anxiety_2021$pt))
not_zoloft_depression_adr_2021=as.data.frame(table(not_zoloft_depression_2021$pt))


#zoloft_anxiety_2021=bind_rows(zoloft_split_2021$Anxiety,zoloft_split_2021$`Anxiety disorder`,zoloft_split_2021$`Anxiety/Depression`,zoloft_split_2021$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2021$`Anxiety/Obsessive-compulsive disorder`)
#zoloft_depression=bind_rows(zoloft_split_2021$Depression, zoloft_split_2021$`Depression suicidal`,zoloft_split_2021$`Depression/Obsessive-compulsive disorder`,zoloft_split_2021$`Depression/Panic attack`,zoloft_split_2021$`Anxiety/Depression`,zoloft_split_2021$`Anxiety/Depression/Post-traumatic stress disorder`)

#compare demographics

zoloft_sex_2021=as.data.frame(table(zoloft_unique_2021$sex))
#zoloft_indi=as.data.frame(table(zoloft_unique_2021$indi_pt_all))
#zoloft_indi_final=count_indications(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual "),zoloft)
#report_country=as.data.frame(table(zoloft_unique$reporter_country))
#occr_country=as.data.frame(table(zoloft_unique$occr_country))


zoloft_unique_druginfo_2021=rbindlist(zoloft_unique_2021$patient_drug)



zoloft_unique_druginfo_2021$dose_amt=zoloft_unique_druginfo_2021$dose_amt%>%as.double()
#hist(zoloft_unique_druginfo_2021$dose_amt)
#zoloft_no_overdose=zoloft %>%filter("dose_amt"<900)
#age breakdown 
#hist(zoloft_unique_2021$age)
summary(zoloft_unique_2021$age)

#compare adr rates between indications

ic_2021=ROR_comparison(zoloft_2021,not_zoloft_2021,TRUE,3)
ip_2021=PRR_comparison(zoloft_2021,not_zoloft_2021,TRUE,3)
#find all qualifying adrs for this year
qualifying_adrs_2021=count_qualifying_ADRs(ip_2021,ic_2021)

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
d_table_z_2021=return_demographic_table(zoloft_2021)
d_table_c_2021=return_demographic_table(not_zoloft_2021)

#ror/prr indications
AD_ROR=ROR_comparison(zoloft_anxiety_2021,zoloft_depression_2021)
AD_PRR=PRR_comparison(zoloft_anxiety_2021,zoloft_depression_2021)

#depression ROR/prr counts of patients
c_table_ind2=matrix(c(243, 460,13  ,11   ,28,0, 1747,5140,136,838,152,29),ncol=6,byrow=TRUE)
colnames(c_table_ind2)=c("Anxiety","Depression","Panic", "Post-traumatic","Obsessive-Compulsive","Other")
rownames(c_table_ind2)=c("zoloft","not-zoloft")
chisq.test(c_table_ind2,simulate.p.value=TRUE)
#chisquRred sexx
c_table_ind2=matrix(c(638,294,69,4920,2455,564),ncol=3,byrow=TRUE)
chisq.test(c_table_ind2,simulate.p.value=FALSE)

#chisquared weight

c_table_ind2=matrix(c(113 , 151,26  ,433   ,101, 124, 2117, 4907),ncol=4,byrow=TRUE)
chisq.test(c_table_ind2,simulate.p.value=FALSE)

#chisquared age
c_table_ind2=matrix(c(136,685,1,145,142,4643,980,2071),ncol=4,byrow=TRUE)
chisq.test(c_table_ind2)

#chisquared indications by patient
c_table_ind2=matrix(c(629,270,53,17,41,0,5441,2040,204,177,184,21),ncol=6,byrow=TRUE)
chisq.test(c_table_ind2,simulate.p.value = TRUE)

c_table_ind2=matrix(c(2061,965,147,48,41,0,20400,9640,796,955,961,56),ncol=6,byrow=TRUE)
chisq.test(c_table_ind2,simulate.p.value = TRUE)


#5 cases
homicide=zoloft_2021[zoloft_2021$pt=="Homicide",]
