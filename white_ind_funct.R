white_ind_funct <- function(input){
        df <- data.frame(matrix(nrow=0, ncol=6))
        colnames(df) <- c("year", "period","periodName",
                          "value", "change.12.month",
                          "pctchange.12.month")
        
        alldemo <- data.frame(matrix(nrow=0, ncol=6))
        colnames(alldemo) <- c("year", "period","periodName",
                               "value", "change.12.month",
                               "pctchange.12.month")
        temp_df <- data.frame()
        
        seriesid <- c("LNU02081921", "LNU02094783", "LNU02094792", "LNU02094806","LNU02094851",   
                      "LNU02094907", "LNU02094931", "LNU02094969", "LNU02094799","LNU02094982",   
                      "LNU02095002","LNU02095017", "LNU02095041", "LNU02095046", "LNU02095064", "LNU02095077", "LNU02095099")
        
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
                df$group <- group[x]
                df$group.id <- seriesid[x]
                alldemo <- rbind(alldemo, df) 
                alldemo2 <- alldemo[,c(7,8,1,2,3,4,5,6)]
        }
        sheet_write(alldemo2, ss = "145k5urPZXczvQR7isiJFxxWku_A3upqwr3kpfof9v-c", sheet = "white_pct_ind" )
        return(alldemo2)
}        
        