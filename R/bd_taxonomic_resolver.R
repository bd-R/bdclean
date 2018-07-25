#'
#' @title Function to resolve biodiveristy data based on taxonomic ranks.
#' 
#' @param df (data.frame) Data frame containing biodiversity data.
#' @return (data.frame) Returns a data frame containing resolved biodiversity data.
#' 
#' @example 
#' occdat1 <- occ_data(
#' country = "AU",     # Country code for australia
#' classKey= 359,      # Class code for mammalia
#' limit=500,         # Get only 5000 records
#' )
#' 
#' #example1
#' df<-bd_taxonomic_resolver(occdat1$data)  #Enter species as taxonomic rank
#' 
#' #exmaple2
#' #Now artificially inserting missing values in taxonRank field in data
#' #occdat1$data$taxonRank[20]<-NA
#' #occdat1$data$taxonRank[40]<-NA
#' #occdat1$data$taxonRank[100]<-NA
#' #occdat1$data$taxonRank[150]<-NA
#' 
#' #Now run the function again
#' 
#' df<-bd_taxonomic_resolver(occdat1$data) #Enter species as taxonomic rank
#' 
#'


bd_taxonomic_resolver<-function(df=NULL){
  
  
  tax_rank<- readline(prompt="Choose the lowest taxonomic rank for resolving the data 
                      1) kingdom
                      2) phylum
                      3) class
                      4) order
                      5) family
                      6) genus 
                      7) species
                      8) subspecies  ")
  
  df<-as.data.frame(df)
  if(nrow(df)==0){
    stop(paste("The data frame is empty."))
  }
  tax_rank<-toupper(tax_rank)
  res_index<-readline(prompt="Do you want to resolve the missing ranks in the data (y/n)")
  
  if(res_index=='n'){
    #
    df_subset<-subset(df,taxonRank==tax_rank)
    return (df_subset)
    
  }else{
    
    df_subset<-subset(df,taxonRank==tax_rank)
    df_na<-subset(df,is.na(taxonRank))
    
    if(nrow(df_na)!=0){
      count1<-nrow(df_na)
      count2<-0
      for (i in 1:nrow(df_na)){
        if(!is.na( df_na$name[i])){
          
          #using only two data bases 'itis', 'ncbi' to keep the function simple
          tax_hierarchy_itis <- suppressMessages( as.data.frame(taxize::classification(taxize::get_uid(df_na$name[i]), db = "itis")[[1]]))
          tax_hierarchy_ncbi <- suppressMessages( as.data.frame(taxize::classification(taxize::get_uid(df_na$name[i]), db = "ncbi")[[1]]))
        }
        
        if(suppressMessages(!is.na(tax_hierarchy_ncbi[[1]]))){
          
          df_na$taxonRank[i]<-tax_hierarchy_ncbi$rank[nrow(tax_hierarchy_ncbi)]
          
        }else if(suppressMessages(!is.na(tax_hierarchy_itis[[1]]))){
          
          df_na$taxonRank[i]<-tax_hierarchy_itis$rank[nrow(tax_hierarchy_itis)]
          
        }else{
          count2=count2+1
          
        }
        
      }
      #Now combine both the data frames to form the final data frame
      
      df_subset<-df_subset[!is.na(df_subset$taxonRank),]
      df_na<-subset(df_na,taxonRank=tax_rank)
      df_final<-rbind(df_subset,df_na)
      
      #cat("The number of records with missing taxon rank were",count1,"\n")
      #cat("The number of records which are resolved:",count2,"\n")
      
      return(df_final)
    }else{
      
      return(df_subset)
    }
    
  }  
  
}