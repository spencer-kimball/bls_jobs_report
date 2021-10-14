women_ind_funct <- function(input){
        df <- data.frame(matrix(nrow=0, ncol=6))
        colnames(df) <- c("year", "period","periodName",
                          "value", "change.12.month",
                          "pctchange.12.month")
        
        alldemo <- data.frame(matrix(nrow=0, ncol=6))
        colnames(alldemo) <- c("year", "period","periodName",
                               "value", "change.12.month",
                               "pctchange.12.month")
        temp_df <- data.frame()
        
        seriesid <- c("LNU02073391", "LNU02072095", "LNU02072103", "LNU02072110", 
                      "LNU02072111", "LNU02072211", "LNU02072235", "LNU02072273", 
                      "LNU02072286", "LNU02072293", "LNU02072312", "LNU02072327", 
                      "LNU02072351","LNU02072356", "LNU02072374", "LNU02072387", "LNU02072409")
        
        group <- c("Total16+", "Agriculture", "Mining", "Construction", "Manufacturing",
                   "Wholesale.Trade", "Retail.Trade", "Transportation.Warehousing",
                   "Utilities", "Information", "Financial.Activities", "Prof.Biz.Service",
                   "Education", "Healthcare.Social.Assistance", "Leisure.Hospitality", "Other", "Public.Admin")  
        
        
        for(x in 1:length(input)){
                temp_df <- input[[x]]
                temp_df <- temp_df[,c(-4,-6)]
                data_vector <- unlist(temp_df)
                counter <- 0
                
                for(j in 1:6){
                        
                        for(i in 1:nrow(input[[1]])){
                                counter <- counter + 1
                                df[i,j] <- data_vector[counter]
                                
                        }
                        
                        
                }
                df[,4:6] <- lapply(df[,4:6], as.numeric)
                df$group.id <- seriesid[x]
                df$group <- group[x]
                alldemo <- rbind(alldemo, df) 
                alldemo2 <- alldemo[,c(8,7,1,2,3,4,5,6)]
        }
        sheet_write(alldemo2, ss = "145k5urPZXczvQR7isiJFxxWku_A3upqwr3kpfof9v-c", sheet = "women_pct_ind" )
        return(alldemo2)
        
}