rm(list=ls())
# Load libraries, etc ###############################
pacman::p_load(tidyverse,
               rio)

# Load the data ################################################################

# Load the file

# This must correspond to the variable given to the previous scripts

power_table_dep <- import(file.path('./analysis_results',
                                'mult_stop_rule_indep_0_effSiz_05_0',
                                'power_table.RData'))
power_table_indep <- import(file.path('./analysis_results',
                                    'mult_stop_rule_indep_1_effSiz_05_0',
                                    'power_table.RData'))

# Combine
power_table_dep['group_independence'] <- F
power_table_indep['group_independence'] <- T

power_table <- rbind(power_table_dep,power_table_indep)


# How many unique combination of factors are here? 
# For each, make a separate plot
unique_combs <-
        power_table_dep %>%
        select(-c(altMaxN,
                  cond_1_bf_status,
                  cond_2_bf_status,
                  n_simulations,
                  perc_simulations)) %>%
        distinct()

n_combs <- nrow(unique_combs)

# Create the plot #############################################################


# x tick marks?
x_ticks <- seq(power_table$minN[1],power_table$limit[1],power_table$batchSize[1])


# Classify simulations
power_table <- power_table %>%
        mutate(bf_status_combined = paste(cond_1_bf_status,cond_2_bf_status,sep='_'))

# Only keep what we need
# power_table <- power_table %>%
#         filter(bf_status_combined %in% c('H1_H1',
#                                          'H0_H0',
#                                          'H1_H0'))

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
                           color = bf_status_combined,
                           linetype = group_independence,
                           )) +
                geom_line(aes(color = bf_status_combined)) +
                geom_point(aes(color = bf_status_combined)) +
                scale_x_continuous(breaks=x_ticks) +
                scale_y_continuous(breaks=seq(0,100,10)) +  
                theme(axis.text.x = element_text(angle = 90)) + 
                ylab('% of simulations') +
                xlab('max N per group') +                 
                ggtitle(title_string)
        
      
        print(fig)

}