education_genderrace_funct <- function(input){
        df <- data.frame(matrix(nrow=0, ncol=12))
        colnames(df) <- c("year", "period","periodName",
                          "value","change.1.month",
                          "change.3.month", "change.6.month", "change.12.month",
                          "pctchange.1.month", "pctchange.3.month", "pctchange.6.month",
                          "pctchange.12.month")
        df2 <- data.frame(matrix(nrow=0, ncol=12))
        colnames(df2) <- c("year", "period","periodName",
                          "value","change.1.month",
                          "change.3.month", "change.6.month", "change.12.month",
                          "pctchange.1.month", "pctchange.3.month", "pctchange.6.month",
                          "pctchange.12.month")
        
        alldemo <- data.frame(matrix(nrow=0, ncol=6))
        colnames(alldemo) <- c("year", "period","periodName",
                               "value", "change.12.month", "pctchange.12.month")
        temp_df <- data.frame()
        
        seriesid <- c("LNU04000052", "LNU04092181", "LNU04092189", "LNU04092205", 
                      "LNU04092228", "LNU04091120", "LNU04000053", "LNU04092182", 
                      "LNU04092190", "LNU04092206", "LNU04092233", "LNU04091121",
                      "LNU04000055", "LNU04092183", "LNU04092191", "LNU04092207", 
                      "LNU04092229", "LNU04091122", "LNU04000056", "LNU04092184", 
                      "LNU04092192", "LNU04092208", "LNU04092234", "LNU04091123",
                      "LNU04032324", "LNU04092185", "LNU04092193", "LNU04092209", 
                      "LNU04092230", "LNU04091124", "LNU04032365", "LNU04092186",
                      "LNU04092194", "LNU04092210", "LNU04092235", "LNU04091125",
                      "LNU04000058", "LNU04092187", "LNU04092195", "LNU04092211", "LNU04092227", 
                      "LNU04091126", "LNU04000059", "LNU04092188", "LNU04092196", "LNU04092212", "LNU04092232", "LNU04091127")
        
        group <- c('White.Men.25+', 'White.Women.25+', 'Black.Men.25+', 'Black.Women.25+',
                   'Hispanic.Men.25+', 'Hispanic.Women.25+', 'Asian.Men.25+', 'Asian.Women.25+')
        
        education <- rep(c('Total.25+', 'No.HS.Diploma', 'High.School', 'Associates', 'Bachelors', 'Advanced'), 8 )
      
        
        
        for(x in 1:length(input)){
                temp_df <- input[[x]]
                temp_df <- temp_df[,c(-4,-6)]
                data_vector <- unlist(temp_df)
                counter <- 0
                
                if(nrow(input[[x]]) == nrow(input[[1]])){
                
                        for(j in 1:12){
                       
                                 for(i in 1:nrow(input[[1]])){
                                        counter <- counter + 1
                                        df[i,j] <- data_vector[counter]
                                
                                }
                        
                        
                        }
                  
                  df[,4:12] <- lapply(df[,4:12], as.numeric)
                  
                  df$education <- education[x]
                  
                  df$seriesid <- seriesid[x]
                  
                  alldemo <- rbind(alldemo, df)
               
          
                
                 }else{
                       
                          for(j in 1:6){
                                
                                for(i in 1:nrow(input[[2]])){
                                        counter <- counter + 1
                                        df2[i,j] <- data_vector[counter]
                                        
                                }
                                
                                
                        }
                  
                   df2[,4:6] <- lapply(df2[,4:6], as.numeric)
                   
                   df2$education <- education[x]
                   
                   df2$seriesid <- seriesid[x]
                   
                   alldemo <- rbind(alldemo, df2)
                   
                   
                }
                
                
        }
        
        alldemo$group <- group[rep(c(1,2,3,4,5,6, 7, 8), each = (nrow(input[[1]]) * 3) + (nrow(input[[2]]) *3))]
        
        alldemo[alldemo$periodName == "Annual", "change.12.month"] <- alldemo[alldemo$periodName == "Annual", "change.1.month"]
        
        alldemo[alldemo$periodName == "Annual", "pctchange.12.month"] <- alldemo[alldemo$periodName == "Annual", "change.3.month"]
        
        alldemo[alldemo$periodName == "Annual", "change.1.month"] <- ""
        
        alldemo[alldemo$periodName == "Annual", "change.3.month"] <- ""
        
        alldemo2 <- alldemo[c(15,13,14,1,2,3,4,5,6,7,8,9,10,11,12)]
        
        sheet_write(alldemo2, ss = "145k5urPZXczvQR7isiJFxxWku_A3upqwr3kpfof9v-c", sheet = "gender+race_education_unemp" )
        
        return(alldemo2)
}        