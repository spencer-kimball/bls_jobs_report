hispanic_ind_funct <- function(input){
        df <- data.frame(matrix(nrow=0, ncol=6))
        colnames(df) <- c("year", "period","periodName",
                          "value", "change.12.month",
                          "pctchange.12.month")
        
        alldemo <- data.frame(matrix(nrow=0, ncol=6))
        colnames(alldemo) <- c("year", "period","periodName",
                               "value", "change.12.month",
                               "pctchange.12.month")
        temp_df <- data.frame()
        
        seriesid <- c('LNU02073394', 'LNU02073067','LNU02073075','LNU02073082',
                      'LNU02073083','LNU02073183', 'LNU02073207','LNU02073245',
                      'LNU02073258','LNU02073265','LNU02073284','LNU02073299', 
                      'LNU02073323','LNU02073328','LNU02073346','LNU02073359','LNU02073381')
        
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
        sheet_write(alldemo2, ss = "145k5urPZXczvQR7isiJFxxWku_A3upqwr3kpfof9v-c", sheet = "hispanic_pct_ind" )
        return(alldemo2)
}     