# This script will load the preprocessed dataframe from 3_preprocess_output.R
# It will then calculate the statistics on supporting H1/H0/undecided for many 
# alternative maxN stopping rules. 

summary_stats = function(saveDF,nFrom,nTo,nBy,folderName){
        # Libraries ######################################

        # Libraries
        pacman::p_load(data.table,
                       tidyverse,
                       rio,
                       tibble)
        
        # Define the parameters and flags #############################################
        
        # Save the resulting summary statistics datafile?
        if (missing(saveDF)){
                saveDF <- TRUE
        }
        
        # What are the various maxNs we want to analyze?
        # nFrom and nBy must match what was given to the simulation script.
        # nTo can be different than the maxN that was given to the original simulation job,
        # but it cannot be larger than it.
        if (missing(nFrom)){
                nFrom <- 20
        }
        if (missing(nTo)){
                nTo <- 425
        }
        if (missing(nBy)){
                nBy <- 15
        }        
        altNs <- seq(nFrom,nTo,by = nBy)
        
        # Which preprocessed data to load?
        # This must correspond to where the simulation job was saved.
        if (missing(folderName)){
                folderName <- 'multiple_stopping_rule'
        }
        
        # Load the data and get unique factor combinations ############################
        sims_preprocessed <- import(file.path(
                './analysis_results',folderName,'sims_preprocessed.RData')
                )
        
        # How many unique combinations of factors do we have? 
        # For each, we'll have to do the summary stats separately
        unique_combs <- sims_preprocessed %>%
                select(-c(id,cond_1_bf,cond_2_bf,n)) %>%
                distinct()
        
        n_combs <- nrow(unique_combs)
        
        print(paste('There are ', 
                    n_combs, 
                    ' unique combinations of factors. They are:',
                    sep=''))
        print(unique_combs)
        
        # Get the probabilities #####################################################
        # Of supporting H1 or H0 or neither
        
        outdf = list()
        
        # For each combination of simulation parameters:
        for (iComb in seq(1,nrow(unique_combs))){
                
                print(paste('Combination #',as.character(iComb),sep=''))
                print(unique_combs[iComb,])
                
                # Get just this combination as a data frame
                iComb_df <- unique_combs %>%
                        slice(iComb)
                
                # Now, from the overall sims_preprocessed dataframe, get the rows
                # that match iComb_df
                tempDF <- merge(iComb_df,sims_preprocessed, by = colnames(iComb_df))
                
                for (iN in altNs){
                        print(iN)
        
                        outdf[[length(outdf)+1]] <- tempDF %>%
                                filter(n <= iN) %>%
                                group_by(id) %>%
                                slice_tail() %>%
                                mutate(altMaxN = iN)
                }
                
        }
        
        # Concatenate into one dataframe
        outdfbinded <- rbindlist(outdf, idcol = NULL)
        
        # Has the overall logical check of the condition been met?
        outdfbinded <- outdfbinded %>%
                mutate(cond_1_bf_status = as.factor(
                        case_when(
                                cond_1_bf > cond_1_crit1 ~ 'H1',
                                cond_1_bf < cond_1_crit2 ~ 'H0',
                                TRUE ~ 'undecided'
                                
                        )),
                       cond_2_bf_status = as.factor(
                               case_when(
                                       cond_2_bf > cond_2_crit1 ~ 'H1',
                                       cond_2_bf < cond_2_crit2 ~ 'H0',
                                       TRUE ~ 'undecided'
                                       
                               ))
                       )
        
        # Summary statistics ########################################################
        
        # How many iterations were given to the original simulation job? (nIter variable)
        # This is needed to calculate the "power" i.e. percentage of simulations 
        # supporting various outcomes
        nIter <- sims_preprocessed %>% 
                distinct(id, .keep_all = T) %>%
                group_by(minN,
                         batchSize,
                         limit,
                         cond_1_d,
                         cond_1_crit1,
                         cond_1_crit2,
                         cond_1_test_type,
                         cond_1_side_type,
                         cond_2_d,
                         cond_2_crit1,
                         cond_2_crit2,
                         cond_2_test_type,
                         cond_2_side_type) %>% 
                mutate(iter_idx = row_number()) %>%
                ungroup() %>%
                select(iter_idx) %>% max()
        
        ## Calculate the probabilities of supporting various outcomes ---------      
        power_table <- 
                outdfbinded %>%
                group_by(minN,
                         batchSize,
                         limit,
                         cond_1_d,
                         cond_1_crit1,
                         cond_1_crit2,
                         cond_1_test_type,
                         cond_1_side_type,
                         cond_2_d,
                         cond_2_crit1,
                         cond_2_crit2,
                         cond_2_test_type,
                         cond_2_side_type,                         
                         altMaxN,
                         cond_1_bf_status,
                         cond_2_bf_status) %>%
                summarise(n_simulations = n(), 
                          perc_simulations = n_simulations/nIter*100) %>%
                ungroup()
        
        # Save the data ###############################################################
        
        saveNameOutData <- file.path('./analysis_results',
                                     folderName,
                                     'power_table.RData')
        
        if (saveDF){
                save(power_table, file = saveNameOutData)
        }
        
        return(power_table)
}

