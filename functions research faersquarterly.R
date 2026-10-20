
#' replace_age_outliers
#' @import dplyr
#' @param faers_dataset dataframe in the unified FAERS tabular format. Output by function unify_tabular_ascii
#'
#' @returns faers_dataset dataframe with all ages below 0 or above 110 replaced with median age
#'
#' @examples return_age_outliers(faers_dataset)
#' 
replace_age_outliers=function(faers_dataset){
  #check null/empty dataset
  if(is.null(faers_dataset)){
    print("Empty dataset!")
    return (NULL)
  }
  if (dim(faers_dataset)[1]==0|dim(faers_dataset)[2]==0){
    print("Empty dataset!")
    return (NULL)

  }
  median_age=median(faers_dataset$age_YR[is.na(faers_dataset$age_YR)==FALSE])
  faers_dataset$age_YR[(faers_dataset$age_YR)<0]=median_age
  faers_dataset$age_YR[(faers_dataset$age_YR)>110]=median_age
  return (faers_dataset)
}

#' Return all entries in current faers dataset with dosage above maximum dosage parameter or below 0. 
#' @import dplyr
#' @param faers_dataset Dataframe in the unified FAERS tabular format. Output by function unify_tabular_ascii.
#' @param max_dose Specified maximum dosage. Any entries with a higher dosage are returned.
#' @param na.rm includes or excludes dosages with values of NA. TRUE by default.
#' @returns Dataframe of patient drug information entries with dosage fitting criteria.
#' @examples
#' return_dosage_outliers(faers_dataset, 200, na.rm=FALSE)
#' return_dosage_outliers(faers_dataset, 100)
return_dosage_outliers=function(faers_dataset,max_dose, na.rm=TRUE){
  if(is.null(faers_dataset)){
    print("Empty dataset!")
    return (NULL)
  }
  if (dim(faers_dataset)[1]==0|dim(faers_dataset)[2]==0){
    print("Empty dataset!")
    return (NULL)
    
  }
  if (na.rm==TRUE){
    drugz= rbindlist(faers_dataset$patient_drug)
    drugz=drugz[!is.na(drugz$dose_amt)]
    drugz$dose_amt=as.double(drugz$dose_amt)
    new_dataset=dplyr::bind_rows(drugz[drugz$dose_amt<0],drugz[drugz$dose_amt>max_dose])}
  else{
    drugz= rbindlist(faers_dataset$patient_drug)
    drugz$dose_amt=as.double(drugz$dose_amt)
    new_dataset=dplyr::bind_rows(drugz[is.na(drugz$dose_amt)==TRUE],drugz[drugz$dose_amt<0],drugz[drugz$dose_amt>max_dose])
    }
    return (new_dataset)
}



#' Return ROR scores of all shared adverse reactions between two faers datasets
#'
#' @param faers_dataset_1 FAERs dataset 1. Group under significance testing.
#' @param faers_dataset_2 FAERs dataset 2. Filtered using different variables than 1. 
#' @param min_cases Boolean variable to specify if minimum number of cases in dataset 1 needed to qualify as significant for the PRR calculation. FALSE by default.
#' @param cases number of minimum cases. defaults to 0
#' @returns Dataframe of all adverse events shared by datasets, ROR associated with them, and 95% confidence interval.
#'
#' @examples ROR_comparison(faers_dataset, faers_dataset2)
ROR_comparison=function(faers_dataset_1, faers_dataset_2,min.cases= FALSE, cases=3){
  if(is.null(faers_dataset_1)|is.null(faers_dataset_2)){
    print("Empty dataset!")
    return (NULL)
  }
  if (dim(faers_dataset_1)[1]==0|dim(faers_dataset_1)[2]==0|dim(faers_dataset_2)[1]==0|dim(faers_dataset_2)[2]==0){
    print("Empty dataset!")
    return (NULL)
    
  }
  adrs1=as.data.frame(table(faers_dataset_1$pt))
  adrs2=as.data.frame(table(faers_dataset_2$pt))
  if (min.cases==TRUE){
    adrs1 = adrs1[adrs1$Freq >=cases,]
  }
  overlapping=merge(adrs1,adrs2, by.x="Var1",by.y="Var1")
  cont_table=table(overlapping$Freq.x, overlapping$Freq.y)
  ROR_table=data.frame(ADRs=overlapping$Var1)

  ROR_table$ROR=estimate_ror(overlapping$Freq.x, overlapping$Freq.y,sum(overlapping$Freq.x), sum(overlapping$Freq.x) + sum(overlapping$Freq.y))$estimate
    
  interval=estimate_ror(overlapping$Freq.x, overlapping$Freq.y,sum(overlapping$Freq.x), sum(overlapping$Freq.x) + sum(overlapping$Freq.y))$ic

  ROR_table$lower=interval[1:(length(interval)/2)]
  
  ROR_table$upper=interval[(length(interval)/2+1):length(interval)]
  
  return (ROR_table)
}

#' Return PRR scores of all shared adverse reactions between two faers datasets
#'
#' @param faers_dataset_1 FAERs dataset 1. Group under significance testing.
#' @param faers_dataset_2 FAERs dataset 2. Filtered using different variables than 1. 
#' @param min_cases Boolean variable to specify if minimum number of cases in dataset 1 needed to qualify as significant for the PRR calculation. FALSE by default.
#' @param cases number of minimum cases. defaults to 0
#' @returns Dataframe of all adverse events shared by datasets, PRR associated with them, and 95% confidence interval.
#'
#' @examples PRR_comparison(faers_dataset, faers_dataset2)
#' @examples PRR_comparison(faers_dataset, faers_dataset2, TRUE, 3)
PRR_comparison=function(faers_dataset_1, faers_dataset_2, min.cases= FALSE, cases=3){
  if(is.null(faers_dataset_1)|is.null(faers_dataset_2)){
    print("Empty dataset!")
    return (NULL)
  }
  if (dim(faers_dataset_1)[1]==0|dim(faers_dataset_1)[2]==0|dim(faers_dataset_2)[1]==0|dim(faers_dataset_2)[2]==0){
    print("Empty dataset!")
    return (NULL)
    
  }
  adrs1=as.data.frame(table(faers_dataset_1$pt))
  adrs2=as.data.frame(table(faers_dataset_2$pt))
  if (min.cases==TRUE){
    adrs1 = adrs1[adrs1$Freq >=cases,]
  }
  overlapping=merge(adrs1,adrs2, by.x="Var1",by.y="Var1")
  PRR_table=data.frame(ADRs=overlapping$Var1)
  
  PRR_table$PRR=estimate_prr(overlapping$Freq.x, overlapping$Freq.y,sum(overlapping$Freq.x), sum(overlapping$Freq.x) + sum(overlapping$Freq.y))$estimate
  
  interval=estimate_prr(overlapping$Freq.x, overlapping$Freq.y,sum(overlapping$Freq.x), sum(overlapping$Freq.x) + sum(overlapping$Freq.y))$ic
  
  PRR_table$lower=interval[1:(length(interval)/2)]
  
  PRR_table$upper=interval[(length(interval)/2+1):length(interval)]
  
  return (PRR_table)
}




#' Counts all indications 
#' @importFrom stringr str_count
#' @param indication Vector of strings listing indications of interest. 
#' @param faers_dataset Dataframe in the unified FAERS tabular format. Output by function unify_tabular_ascii.
#'
#' @returns Dataframe counting all occurrences of indications in faers_dataset. Counts indications by report and by unique patient as well as listing percentages.  
#' @examples PRR_comparison(faers_dataset, faers_dataset2)
count_indications <- function(indication, faers_dataset) {
  if(is.null(faers_dataset)){
    print("Empty dataset!")
    return (NULL)
  }
  if (dim(faers_dataset)[1]==0|dim(faers_dataset)[2]==0){
    print("Empty dataset!")
    return (NULL)
    
  }
  
  count=NULL
  faers_unique=distinct(faers_dataset, primaryid, .keep_all = TRUE)
  new_DF=data.frame(indication=character(), count=double(),by_case=double())
  for (i in indication){
    new_DF=new_DF %>% add_row(indication=i, count=sum(str_count(faers_unique$indi_pt_all,i)),by_case=sum(str_count(faers_dataset$indi_pt_all,i)))
    
  
  }
  
  
  new_DF$pct=100*new_DF$count/length(unique(faers_unique[["caseid"]]))
  new_DF$pct_cases=100*new_DF$by_case/length((faers_dataset[["caseid"]]))
  return (new_DF)
}

#' Return rates of adverse events  for input dataset
#' 
#' @param faers_dataset Dataframe in the unified FAERS tabular format. Output by function unify_tabular_ascii. 
#' @param cutoff Minimum number of adverse event occurences included in the function calculations. Default value is 10.
#' @returns Dataframe of rate of all adverse events (population experiencing event divided by total population) for input dataset.
#' @examples
#' adr_rate(faers_dataset)
#' adr_rate(faers_dataset, cutoff=5)
adr_rate=function(faers_dataset,cutoff=10){
  if(is.null(faers_dataset)){
    print("Empty dataset!")
    return (NULL)
  }
  if (dim(faers_dataset)[1]==0|dim(faers_dataset)[2]==0){
    print("Empty dataset!")
    return (NULL)
    
  }
  faers_adrs=as.data.frame(table(faers_dataset$pt))
  faers_adrs=faers_adrs%>%filter(Freq>cutoff)
  faers_adrs$Var1=as.character(faers_adrs$Var1)
  
  total = length(unique(faers_dataset$primaryid))
  
  new_DF=data.frame(ADRs=faers_adrs$Var1, rate=faers_adrs$Freq/total)
  

  return (new_DF)
}

#' Return demographic statistics of FAERs dataset
#' @import dplyr
#' @param faers_dataset Dataframe in the unified FAERS tabular format. Output by function unify_tabular_ascii.
#' @returns Dataframe containing age, weight, gender, and country by count and percentage. Total patients and cases are also included.
#' @examples
#' return_demographic_table(faers_dataset)
return_demographic_table=function(faers_dataset){
  if(is.null(faers_dataset)){
    print("Empty dataset!")
    return (NULL)
  }
  if (dim(faers_dataset)[1]==0|dim(faers_dataset)[2]==0){
    print("Empty dataset!")
    return (NULL)
    
  }
  faers_dataset$sex[faers_dataset$sex=="UNK"| faers_dataset$sex=="P"|faers_dataset$sex=="T"]=""
  faers_unique=dplyr::distinct(faers_dataset, caseid, .keep_all = TRUE)
  sex_table= as.data.frame(table(faers_unique$sex))
  
  levels(sex_table$Var1)=c("Unknown","Female","Male")
  colnames(sex_table)=c("Variable", "Count")
  sex_table$Category=c("Gender","Gender","Gender")
  sex_table=sex_table%>%dplyr::relocate(Category)
  sex_table$Percent=(sex_table$Count/sum(sex_table$Count))*100
  
  wt_table=as.data.frame(table(faers_unique$wt))
  wt_table$Var1=as.numeric(as.character(wt_table$Var1))
  bin_wt_table=data.frame(Category=c("Weight", "Weight","Weight","Weight"), Variable=c("Unknown","<50",">100","50-100"))

  bin_wt_table$Count=c(sum(is.na(faers_unique$wt)),sum(wt_table$Freq[wt_table$Var1<50]),sum(wt_table$Freq[wt_table$Var1>100]),sum(wt_table$Freq[wt_table$Var1>=50&wt_table$Var1<=100]))
  bin_wt_table$Percent=(bin_wt_table$Count/sum(bin_wt_table$Count))*100
  
  age_table=as.data.frame(table(faers_unique$age_YR))
  age_table$Var1=as.numeric(as.character(age_table$Var1))
  bin_age_table=data.frame(Category=c("Age", "Age","Age","Age"), Variable=c("Unknown","<18",">90","18-90"))

  bin_age_table$Count=c(sum(is.na(faers_unique$age_YR)),sum(age_table$Freq[age_table$Var1<18]),sum(age_table$Freq[age_table$Var1>90]),sum(age_table$Freq[age_table$Var1>=18&age_table$Var1<=90]))
  bin_age_table$Percent=(bin_age_table$Count/sum(bin_age_table$Count))*100
  
  patients=data.frame(Category="Total Patients",Variable="-", Count=nrow(faers_unique),Percent="-")
  cases=data.frame(Category="Total Cases",Variable="-", Count=nrow(faers_dataset),Percent="-")
  
  report_country_table=as.data.frame(table(faers_unique$reporter_country))
  report_country_table=top_n(report_country_table, 5)
  reporting_country= data.frame(Category=c("Reporting Country","Reporting Country","Reporting Country","Reporting Country","Reporting Country"),Variable=report_country_table$Var1, Count=report_country_table$Freq, Percent=(100*report_country_table$Freq/(nrow(faers_unique))))
  
  demographic_table=rbind(sex_table, bin_wt_table)
  demographic_table=rbind(demographic_table, bin_age_table)
  demographic_table=rbind(demographic_table, patients)
  demographic_table=rbind(demographic_table, cases)
  demographic_table=rbind(demographic_table, reporting_country)
  return(demographic_table)
}

#' Filters given PRR and ROR datasets by ADR criteria (PRR > 2, ROR lower 95% confidence interval bound >=1).
#' 
#' @param PRR_dataset Output by function PRR_comparison.
#' @param ROR_dataset Output by function ROR_comparison
#' @returns All ADRs in PRR/ROR datasets whose PRR is greater than 2 AND ROR lower 95% confidence interval is less than or equal to 1.
#' @examples
#' count_qualifying_ADRs(PRR_dataset, ROR_dataset)
count_qualifying_ADRs=function(PRR_dataset, ROR_dataset){
  if(is.null(PRR_dataset)|is.null(ROR_dataset)){
    print("Empty dataset!")
    return (NULL)
  }
  if (dim(PRR_dataset)[1]==0|dim(PRR_dataset)[2]==0|dim(ROR_dataset)[1]==0|dim(ROR_dataset)[2]==0){
    print("Empty dataset!")
    return (NULL)
    
  }
  qualifying_PRR=PRR_dataset[PRR_dataset$PRR>=2,]
  qualifying_ROR=ROR_dataset[ROR_dataset$lower>=1,]
  merged=(merge(qualifying_PRR, qualifying_ROR,
              by = 'ADRs', all = TRUE))
  merged=na.omit(merged)
  return (merged)
}

#' removes rows in FAERs datasets with the same age, sex, weight, 
#' 
#' @param faers_dataset Dataframe in the unified FAERS tabular format. Output by function unify_tabular_ascii.
#' @returns input FAERs dataset with matching rows removed
#' @examples
#' count_qualifying_ADRs(PRR_dataset, ROR_dataset)
remove_duplicate_cases=function(faers_dataset){
  if(is.null(faers_dataset)){
    print("Empty dataset!")
    return (NULL)
  }
  if (dim(faers_dataset)[1]==0|dim(faers_dataset)[2]==0){
    print("Empty dataset!")
    return (NULL)
    
  }
  copy_of_faers=data.frame(faers_dataset)
  key <- copy_of_faers[, c('age','sex','wt','pt','event_dt')]
  keep <- !duplicated(key)
  cleaned <- faers_dataset[keep, ]
  dupes   <- faers_dataset[!keep, ]

  print(paste("Discovered and removed ", nrow(dupes), " duplicate cases."))
  return (list(cleaned, dupes))
}
#' Filters given PRR and ROR datasets by ADR criteria (Input by user)
#' 
#' @param PRR_dataset Output by function PRR_comparison.
#' @param ROR_dataset Output by function ROR_comparison
#' @param PRR Value PRR must be greater than to qualify as significant. Default value 2.
#' @param ROR Value ROR must be greater than to qualify as significant. Default value 1.
#' @returns All ADRs in PRR/ROR datasets whose PRR is greater than input value AND ROR lower 95% confidence interval is less than or equal to input value.
#' @examples
#' count_qualifying_ADRs(PRR_dataset, ROR_dataset, 2, 1)
count_qualifying_ADRs=function(PRR_dataset, ROR_dataset, PRR=2, ROR=1){
  if(is.null(PRR_dataset)|is.null(ROR_dataset)){
    print("Empty dataset!")
    return (NULL)
  }
  if (dim(PRR_dataset)[1]==0|dim(PRR_dataset)[2]==0|dim(ROR_dataset)[1]==0|dim(ROR_dataset)[2]==0){
    print("Empty dataset!")
    return (NULL)
    
  }
  qualifying_PRR=PRR_dataset[PRR_dataset$PRR>=PRR,]
  qualifying_ROR=ROR_dataset[ROR_dataset$lower>=ROR,]
  merged=(merge(qualifying_PRR, qualifying_ROR,
                by = 'ADRs', all = TRUE))
  merged=na.omit(merged)
  return (merged)
}
# outcome summary

#' Counts positive outcomes in FAERS data output. 
#' 
#' @param faers_dataset Dataframe in the unified FAERS tabular format. Output by function unify_tabular_ascii.
#' @returns Dataframe containing number of positive outcomes of reported data. 
#' @examples
#' count_outcomes(faers_dataset)
count_outcomes=function(faers_dataset){
  if(is.null(faers_dataset)){
    print("Empty dataset!")
    return (NULL)
  }
  if (dim(faers_dataset)[1]==0|dim(faers_dataset)[2]==0){
    print("Empty dataset!")
    return (NULL)
    
  }
  other=length(which(faers_dataset$outcome_OT == 1))
  hospitalization=length(which(faers_dataset$outcome_HO == 1))
  lifethreatening=length(which(faers_dataset$outcome_LT == 1))
  dead=length(which(faers_dataset$outcome_DE == 1))
  congenital=length(which(faers_dataset$outcome_CA == 1))
  requiredintervention=length(which(faers_dataset$outcome_RI == 1))
  disability=length(which(faers_dataset$outcome_DS == 1))
  toreturn = data.frame(Outcome=c("Disability", "Death","Congenital Anomaly","Required Intervention","Hospitalization","Life-threatening","Other"), Total=c(disability,dead,congenital,requiredintervention,hospitalization,lifethreatening,other))
  return (toreturn)                    
  
  
}
# outcome summary

#' Counts positive outcomes in FAERS data output. 
#' 
#' @param faers_dataset Dataframe in the unified FAERS tabular format. Output by function unify_tabular_ascii.
#' @returns Dataframe containing number of positive outcomes of reported data. 
#' @examples
#' count_outcomes(faers_dataset)
return_all_reports=function(faers_dataset, faers_dataset_subset){
  if(is.null(faers_dataset)){
    print("Empty dataset!")
    return (NULL)
  }
  if (dim(faers_dataset)[1]==0|dim(faers_dataset)[2]==0){
    print("Empty dataset!")
    return (NULL)
    
  }
  toreturn = faers_dataset%>%filter(primaryid %in% faers_dataset_subset$primaryid)
     
  return (toreturn)
  
}



prr_wrapper=function(faers_dataset_1, faers_dataset_2, min_cases){
  if(is.null(faers_dataset_1)|is.null(faers_dataset_2)){
    print("Empty dataset!")
    return (NULL)
  }
  if (dim(faers_dataset_1)[1]==0|dim(faers_dataset_1)[2]==0|dim(faers_dataset_2)[1]==0|dim(faers_dataset_2)[2]==0){
    print("Empty dataset!")
    return (NULL)
    
  }
  restricted=PRR_comparison(faers_dataset_1 = faers_dataset_1, faers_dataset_2 = faers_dataset_2, min.cases = TRUE,cases=min_cases )
  unrestricted=PRR_comparison(faers_dataset_1 = faers_dataset_1, faers_dataset_2 = faers_dataset_2, min.cases = FALSE)
  return (c(restricted, unrestricted))
}


ror_wrapper=function(faers_dataset_1, faers_dataset_2, min_cases){
  if(is.null(faers_dataset_1)|is.null(faers_dataset_2)){
    print("Empty dataset!")
    return (NULL)
  }
  if (dim(faers_dataset_1)[1]==0|dim(faers_dataset_1)[2]==0|dim(faers_dataset_2)[1]==0|dim(faers_dataset_2)[2]==0){
    print("Empty dataset!")
    return (NULL)
    
  }
  restricted=ROR_comparison(faers_dataset_1 = faers_dataset_1, faers_dataset_2 = faers_dataset_2, min.cases = TRUE,cases=min_cases )
  unrestricted=ROR_comparison(faers_dataset_1 = faers_dataset_1, faers_dataset_2 = faers_dataset_2, min.cases = FALSE)
  return (c(restricted, unrestricted))
}
