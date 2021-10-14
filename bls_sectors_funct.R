nonfarm_sectors_funct <- function(input){
        df <- data.frame(matrix(nrow=0, ncol=12))
        colnames(df) <- c("year", "period","periodName",
                          "value","change.1.month",
                          "change.3.month", "change.6.month", "change.12.month",
                          "pctchange.1.month", "pctchange.3.month", "pctchange.6.month",
                          "pctchange.12.month")
        
        allsectors <- data.frame(matrix(nrow=0, ncol=12))
        colnames(allsectors) <- c("year", "period","periodName",
                                  "value","change.1.month",
                                  "change.3.month", "change.6.month", 
                                  "change.12.month", "pctchange.1.month", 
                                  "pctchange.3.month", "pctchange.6.month",
                                  "pctchange.12.month")
        temp_df <- data.frame()
        
        seriesid <- c("CES0000000001", "CES1000000001", "CES2000000001", 
                       "CES3000000001", "CES4142000001", "CES4200000001", 
                       "CES4300000001", "CES4422000001","CES5000000001", 
                       "CES5500000001", "CES6000000001", "CES7000000001", 
                      "CES9000000001", "CES6562000001")
        
        industryname <- c("Total.Non.Farm","Mining.Logging", "Construction", 
                          "Manufacturing", "Wholesale.Trade","Retail.Trade", 
                          "Transportation.Warehousing","Utilities","Information",
                          "Financial.Activities","Prof.Biz.Services",
                          "Leisure.Hospitality", "Govt", 
                          "Healthcare.Socialassis")
        
        
        for(x in 1:length(input)){
                temp_df <- input[[x]]
                temp_df <- temp_df[,c(-4,-6)]
                data_vector <- unlist(temp_df)
                counter <- 0
                
                for(j in 1:12){
                        
                        for(i in 1:nrow(input[[1]])){
                                 counter <- counter + 1
                                 df[i,j] <- data_vector[counter]
                        
                        }
                        
         
                }
        df[,4:12] <- lapply(df[,4:12], as.numeric)        
        df$industry.id <- seriesid[x]
        df$industry <- industryname[x]
        allsectors <- rbind(allsectors, df) 
        allsectors2 <- allsectors[,c(14,13,1,2,3,4,5,6,7,8,9,10,11,12)]
        }
        sheet_write(allsectors2, ss = "145k5urPZXczvQR7isiJFxxWku_A3upqwr3kpfof9v-c", sheet = "nonfarm_payrolls")
        return(allsectors2)
}

