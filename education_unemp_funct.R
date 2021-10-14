education_unemp_funct <- function(input){
        df <- data.frame(matrix(nrow=0, ncol=12))
        colnames(df) <- c("year", "period","periodName",
                          "value","change.1.month",
                          "change.3.month", "change.6.month", "change.12.month",
                          "pctchange.1.month", "pctchange.3.month", "pctchange.6.month",
                          "pctchange.12.month")
        
        alldemo <- data.frame(matrix(nrow=0, ncol=12))
        colnames(alldemo) <- c("year", "period","periodName",
                               "value","change.1.month",
                               "change.3.month", "change.6.month", 
                               "change.12.month", "pctchange.1.month", 
                               "pctchange.3.month", "pctchange.6.month",
                               "pctchange.12.month")
        temp_df <- data.frame()
        
        seriesid <- c("LNU04000048", "LNU04027659", "LNU04027660", "LNU04027683", "LNU04092221", 
                      "LNU04091113", "LNU04000049", "LNU04027675", "LNU04027676", "LNU04027687", 
                      "LNU04092226", "LNU04091114", "LNU04000050", "LNU04027679", "LNU04027680", 
                      "LNU04027688", "LNU04092231", "LNU04091115", "LNU04000051", "LNU04027667", 
                      "LNU04027668", "LNU04027685", "LNU04092223", "LNU04091116", "LNU04000054", 
                      "LNU04027671", "LNU04027672", "LNU04027686", "LNU04092224", "LNU04091117", 
                      "LNU04027663", "LNU04027664", "LNU04027684", "LNU04092222", "LNU04091119", 
                      "LNU04034985", "LNU04032295", "LNU04032296", "LNU04032299", "LNU04092225", "LNU04091118")
        
        group <- c('Total','Men', 'Women','White', 'Black','Hispanic', 'Asian')
        
        education <- rep(c('Total.25+', 'No.HS.Diploma', 'High.School', 'Associates', 'Bachelors', 'Advanced'), 7 )
        education <- education[-31]
                       
        
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
                
                df$education <- education[x]
                 
                df$seriesid <- seriesid[x]
                
                df$date <- as.Date(paste(df$year, df$periodName, '01', sep = "-"), "%Y-%B-%d")
                
                alldemo <- rbind(alldemo, df)
                
        }
        
        alldemo$group <- group[rep(c(1,2,3,4,5,6, 7), c(nrow(input[[1]]) * 6, nrow(input[[1]]) * 6, nrow(input[[1]]) * 6, nrow(input[[1]]) * 6, nrow(input[[1]]) * 6, nrow(input[[1]]) * 5, nrow(input[[1]]) * 6))]
        
        alldemo2 <- alldemo[c(16,13,14,15,1,2,3,4,5,6,7,8,9,10,11,12)]
        
        sheet_write(alldemo2, ss = "145k5urPZXczvQR7isiJFxxWku_A3upqwr3kpfof9v-c", sheet = "education_unemp" )
        
        return(alldemo2)
        
}
