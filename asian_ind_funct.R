asian_ind_funct <- function(input){
        df <- data.frame(matrix(nrow=0, ncol=6))
        colnames(df) <- c("year", "period","periodName",
                          "value", "change.12.month",
                          "pctchange.12.month")
        
        alldemo <- data.frame(matrix(nrow=0, ncol=6))
        colnames(alldemo) <- c("year", "period","periodName",
                               "value", "change.12.month",
                               "pctchange.12.month")
        temp_df <- data.frame()
        
        seriesid <- c('LNU02073393','LNU02072743','LNU02072751','LNU02072758',
                      'LNU02072759','LNU02072859','LNU02072883','LNU02072921',
                      'LNU02072934','LNU02072941','LNU02072960','LNU02072975',
                      'LNU02072999','LNU02073004','LNU02073022','LNU02073035','LNU02073057')
        
        group <- c("Total16+", "Agriculture", "Mining", "Construction", "Manufacturing",
                   "Wholesale.Trade", "Retail.Trade", "Transportation.Warehousing",
                   "Utilities", "Information", "Financial.Activities", "Prof.Biz.Service",
                   "Education", "Healthcare.Social.Assistance", "Leisure.Hospitality", "Other", "Public.Admin")  
        
        change.feb.2020 <- vector()
        
        
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
        sheet_write(alldemo2, ss = "145k5urPZXczvQR7isiJFxxWku_A3upqwr3kpfof9v-c", sheet = "asian_pct_ind" )
        return(alldemo2)
}     
