# This script will take the preprocessed data and for each unique combination of
# factors that were simulated, it will produce two plots:

# 1. A "Power" plot: probabilities of supporting H1/H0 or undecided
# 2. Mean/median number of participants needed to reach support for H1/H0 or undecided

plot_results = function(folderName){
        # Load libraries, etc ###############################
        pacman::p_load(tidyverse,
                       rio)
        
        # Load the data ################################################################
        
        # Load the file
        
        # This must correspond to the variable given to the previous scripts
        if (missing(folderName)){
                folderName <- 'multiple_stopping_rule'                
        }       
        
        power_table <- import(file.path('./analysis_results',
                         folderName,
                         'power_table.RData'))
        
        # How many unique combination of factors are here? 
        # For each, make a separate plot
        unique_combs <-
                power_table %>%
                select(-c(altMaxN,
                          cond_1_bf_status,
                          cond_2_bf_status,
                          n_simulations,
                          perc_simulations)) %>%
                distinct()

        n_combs <- nrow(unique_combs)

        print(paste('There are ',
                    n_combs,
                    ' unique combination of factors. They are:',
                    sep=''))
        print(unique_combs)


        
        # Create the plot #############################################################
        
        
        # x tick marks?
        x_ticks <- seq(power_table$minN[1],power_table$limit[1],power_table$batchSize[1])
        
        
        # Classify simulations
        power_table <- power_table %>%
                mutate(bf_status_combined = paste(cond_1_bf_status,cond_2_bf_status,sep='_'))
        
        for (iComb in seq(1,n_combs)){
                
                print(unique_combs[iComb,])
                
                title_string <- paste(
                        'Power curves for the following simulation:',
                        '\n',
                        '\n',
                        'minN = ',unique_combs$minN[iComb],
                        '; batchSize = ',unique_combs$batchSize[iComb],
                        '; limit = ',unique_combs$limit[iComb],
                        '\n',
                        '\n',
                        'cond_1_d = ',unique_combs$cond_1_d[iComb],
                        '; cond_1_crit1 = ',round(unique_combs$cond_1_crit1[iComb],4),
                        '; cond_1_crit2 = ',round(unique_combs$cond_1_crit2[iComb],4),
                        '\n','\n',
                        'cond_1_test_type = ',unique_combs$cond_1_test_type[iComb],
                        '; cond_1_side_type = ',unique_combs$cond_1_side_type[iComb],
                        
                        '\n',
                        '\n',
                        'cond_2_d = ',unique_combs$cond_2_d[iComb],
                        '; cond_2_crit1 = ',round(unique_combs$cond_2_crit1[iComb],4),
                        '; cond_2_crit2 = ',round(unique_combs$cond_2_crit2[iComb],4),
                        '\n',
                        '\n',
                        'cond_2_test_type = ',unique_combs$cond_2_test_type[iComb],
                        '; cond_2_side_type = ',unique_combs$cond_2_side_type[iComb],                        
                        sep=''
                )
                
                fig <- power_table %>%
                        ggplot(aes(x=altMaxN,
                                   y=perc_simulations,
                                   group = bf_status_combined,
                                   color = bf_status_combined)) +
                        geom_line() +
                        geom_point() +
                        scale_x_continuous(breaks=x_ticks) +
                        scale_y_continuous(breaks=seq(0,100,10)) +  
                        theme(axis.text.x = element_text(angle = 90)) + 
                        ylab('% of simulations') +
                        xlab('max N per group') +                 
                        ggtitle(title_string)
                
                print(fig)                        
        
        }

        return(power_table)
}