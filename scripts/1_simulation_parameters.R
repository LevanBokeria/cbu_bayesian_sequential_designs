# Description:

# This is the main script where you setup simulation parameters, and they get
# passed to slurm to perform fast computation.

# You can also run the simulations locally, without submitting to slurm. But 
# that will be much, much slower. Its advised to do this only for small jobs.


# Global parameters and libraries #######################################

# Clear the environment
rm(list=ls())

# Setting seed
set.seed(654456)

# Libraries
pacman::p_load(rslurm,
               BayesFactor,
               MASS,
               tidyverse)

# Setup simulation parameters and flags #######################################

# Slurm job parameters
n_nodes       <- 1
cpus_per_node <- 16
nIter         <- 10000


## If multiple stopping rules ------------------------------------------------------

# whats the covariance between conditions?
cv <- 0
ind_var <- 1/2

n_sr <- 2

sr_df <- data.frame(condition = numeric(n_sr),
                    minN      = numeric(n_sr),
                    limit     = numeric(n_sr),
                    batchSize = numeric(n_sr),
                    d         = numeric(n_sr),
                    crit1     = numeric(n_sr),
                    crit2     = numeric(n_sr),
                    test_type = character(n_sr),
                    side_type = character(n_sr))

sr_df$condition <- c(1,2)
sr_df$minN      <- c(20)
sr_df$batchSize <- c(16,16)
sr_df$limit     <- c(180,180)
sr_df$d         <- c(0.5,0.5)
sr_df$crit1     <- c(6,6)
sr_df$crit2     <- c(1/6,1/6)
sr_df$test_type <- c('paired','paired')
sr_df$side_type <- c('two_tailed','two_tailed')

logical_check <- '&'

# Name for saving folder
saveFolder <- paste('multiple_stopping_rule_dependent_conditions',
                    cv,
                    paste(sr_df$d,collapse = '_'),
                    sep = '_',
                    collapse = '_')

# Submit the slurm job?
submitJob <- T

# Simulate locally? This will take much longer for large jobs
simLocal <- !submitJob

# Define the function ########################################################
# This function will be applied to specified parameters many times by slurm.

helperfunction <- function(minN,
                           batchSize, 
                           limit,                           
                           cond_1_d,
                           cond_2_d,
                           cond_1_crit1,
                           cond_2_crit1,
                           cond_1_crit2,
                           cond_2_crit2,
                           cond_1_test_type, 
                           cond_2_test_type, 
                           cond_1_side_type,
                           cond_2_side_type,
                           logical_check,
                           cv,
                           ind_var){
        
        # Subfunction to efficiently report the BF
        # From https://github.com/JAQuent/assortedRFunctions/R/reportBF.R
        reportBF <- function(x, digits){
                round(as.numeric(as.vector(x)), digits)
        }
        
        
        bf     <- vector(mode = "list", length = 2)
        
        dataG1 <- vector(mode = "list", length = 2)
        dataG2 <- vector(mode = "list", length = 2)
        
        d         <- c(cond_1_d,cond_2_d)
        crit1     <- c(cond_1_crit1,cond_2_crit1)
        crit2     <- c(cond_1_crit2,cond_2_crit2)
        test_type <- c(cond_1_test_type,cond_2_test_type)
        side_type <- c(cond_1_side_type,cond_2_side_type)
        
        cond_df <- data.frame(minN,batchSize,limit,d,crit1,crit2,test_type,side_type)
        cond_df$logical_check <- logical_check
        cond_df$condition <- 1:nrow(cond_df)
        
        results <- data.frame()
        n_part  <- as.numeric(minN)       
        
        
        # Get group data from a multivariabe normal distribution
        all_data <- mvrnorm(n = minN,
                                   mu = c(-cond_df$d[1],
                                          0,
                                          -cond_df$d[2]),
                                   Sigma <- matrix(c(ind_var,cv,cv,
                                                     cv,ind_var,cv,
                                                     cv,cv,ind_var),3,3))
        
        for (iCond in seq(1,nrow(cond_df))){
                
                # Is this one-sided or two sided
                if (cond_df$side_type[iCond] == 'two_tailed'){
                        null_interval <- NULL
                } else if (cond_df$side_type[iCond] == 'one_tailed'){
                        null_interval <- c(0,Inf)
                }        
                
                # Now get the differences
                if (iCond == 1){
                        dataG1[[iCond]] <- c(dataG1[[iCond]], all_data[,2] - all_data[,1])        
                } else if (iCond == 2){
                        
                        dataG1[[iCond]] <- c(dataG1[[iCond]], all_data[,2] - all_data[,3])        
                }
                
                bf[[iCond]][1] <- reportBF(ttestBF(
                        dataG1[[iCond]],
                        nullInterval = null_interval
                )[1],4)        
                
        }
        
        # Start the while loop
        
        # Defome the condition
        if (logical_check == '&'){
                while_string_log <- '|'
        } else if (logical_check == '|'){
                while_string_log <- '&'
        }
        
        while_string <- paste0('n_part < limit & (bf[[1]][length(bf[[1]])] < cond_1_crit1 & bf[[1]][length(bf[[1]])] > cond_1_crit2)',
                               while_string_log,
                               '(bf[[2]][length(bf[[2]])] < cond_2_crit1 & bf[[2]][length(bf[[2]])] > cond_2_crit2)')
        i <- 1
        
        while (eval(parse(text = while_string))){                

                n_part <- n_part + batchSize
                
                # Get group data from a multivariabe normal distribution
                all_data <- mvrnorm(n = batchSize,
                                    mu = c(-cond_df$d[1],
                                           0,
                                           -cond_df$d[2]),
                                    Sigma <- matrix(c(ind_var,cv,cv,
                                                      cv,ind_var,cv,
                                                      cv,cv,ind_var),3,3))
                
                for (iCond in seq(1,nrow(cond_df))){
                        
                        # Is this one-sided or two sided
                        if (cond_df$side_type[iCond] == 'two_tailed'){
                                null_interval <- NULL
                        } else if (cond_df$side_type[iCond] == 'one_tailed'){
                                null_interval <- c(0,Inf)
                        } 
                        
                        # Now get the differences
                        if (iCond == 1){
                                dataG1[[iCond]] <- c(dataG1[[iCond]], all_data[,2] - all_data[,1])        
                        } else if (iCond == 2){
                                
                                dataG1[[iCond]] <- c(dataG1[[iCond]], all_data[,2] - all_data[,3])        
                        }
                        
                        bf[[iCond]][i + 1] <- reportBF(ttestBF(
                                dataG1[[iCond]],
                                nullInterval = null_interval
                        )[1],4)          
                  
                } # iCond
                
                
        i <- i + 1
                
        } # while loop

        # Return results
        results <- cond_df %>%
                pivot_wider(id_cols = c(minN,
                                        batchSize,
                                        limit,
                                        logical_check),
                            names_from = condition,
                            values_from = c(d,crit1,crit2,test_type,side_type),
                            names_glue = "cond_{condition}_{.value}")
        
        bf_df <- as.data.frame(bf)
        colnames(bf_df) <- c('cond_1_bf','cond_2_bf')
        
        results   <- merge(results,bf_df)
        results$n <- seq(minN,n_part,batchSize)

        return(results)
}

# Create parameters #########################################################
# slurm will iterate over these with the helperfunction

# Now, repeat each of these combinations nIter times
params <- sr_df %>%
        pivot_wider(id_cols = c(minN,
                                batchSize,
                                limit),
                    names_from = condition,
                    names_glue = "cond_{condition}_{.value}",
                    values_from = c(d,crit1,crit2,test_type,side_type))

# Add the logical rule 
params$logical_check <- logical_check

# Add the covariance
params$cv <- cv
params$ind_var <- ind_var

# Now repeat nIter times
params <- do.call("rbind", replicate(nIter, params, simplify = FALSE))

# Run the simulation ##########################################################


## Try locally for every row -----------------------------------------------
if (simLocal){
        print('Simulating locally...')
        
        results <- do.call(Map, c(f=helperfunction,params))
        
        # If the save directory doesn't exist, create it
        ifelse(!dir.exists(paste('./_rslurm_',saveFolder,sep='')),
               dir.create(paste('./_rslurm_',saveFolder,sep=''), recursive = T),
               'Save directory already exists!')
        
        
        saveRDS(results, file = paste(
                './_rslurm_', 
                saveFolder,
                '/results_0.RDS',sep = ''))
}

## Or try SLURM  ------------------------------------------------------------

# Create job
if (submitJob){
        print('Submitting to the cluster...')
        
        sjob1 <- slurm_apply(helperfunction,
                             params, 
                             jobname = saveFolder,
                             nodes = n_nodes, 
                             cpus_per_node = cpus_per_node, 
                             submit = submitJob)        
}


