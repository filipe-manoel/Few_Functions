
reps_joint_ADGBLUP <- function(df, form_fixed, form_random, form_residual, comb_size = 1,
                               trial, rep, ibloc, site, year, pattern = c("pattern_A", "pattern_D")) {
  
  
  df = df
  TRIAL = df[ ,trial]
  REP = df[ , rep]
  IBLC = df[ , ibloc] 
  SITE = df[ , site]
  YEAR = df[ , year]  
  
  results_list_vc <- list()
  results_list_sum <- list()
  
  
  # Loop over levels of YEAR
  for (i in levels(df$YEAR)) {
    data2 <- droplevels(subset(df, YEAR == i))
    
    results_list1 <- list()
    results_list2 <- list()
    
    
    # Number of repetitions
    num_repetitions <- nlevels(data2$REP)
    
    # Generate all combinations of REPetitions
    repetition_comb <- combn(unique(data2$REP), comb_size)
    
    # Loop over combinations of REPetitions
    for (j in 1:ncol(repetition_comb)) {
      current_comb <- repetition_comb[, j]
      
      # Subset the original data frame based on the current combination
      data3 <- data2[data2$REP %in% current_comb, ]
      
      # Fit the model (asreml-R)
      asreml.options(threads = -1, ai.sing= TRUE) 
      
      mod <- asreml(fixed = form_fixed,
                    random = form_random, 
                    residual = form_residual,
                    na.action = na.method(x = "exclude", y = "exclude"),
                    workspace = "4gb",
                    data = data3)
      
      # Obtain variance components summary
      vc_mod <- summary(mod)$varcomp
      
      # Calculate heritability and accuracies
      
      H2a =  vc_mod[2, 1] /(vc_mod[1, 1] + vc_mod[2, 1] + vc_mod[3, 1] + ((vc_mod[4, 1] +vc_mod[5, 1])/2))
      d2 =   vc_mod[3, 1] /(vc_mod[1, 1] + vc_mod[2, 1] + vc_mod[3, 1] + ((vc_mod[4, 1] +vc_mod[5, 1])/2))
      H2g =  (vc_mod[2, 1] + vc_mod[3, 1])  /(vc_mod[1, 1] + vc_mod[2, 1] + vc_mod[3, 1] + ((vc_mod[4, 1] +vc_mod[5, 1])/2))
      
      #BLUP
      BLUP <- data.frame(summary(mod, coef=TRUE)$coef.random)
      
      # Accuracy PEV A
      vector_a = list(vector_a = c(pattern[1], pattern[2]), pattern_X = pattern[1]) # "pattern_A"
      pattern_A = vector_a$pattern_X
      
      # match_pattern <- function(vector_a, pattern) {
      #   return(grep(pattern, vector_a, value = TRUE))
      # }
      # 
      #pattern_A <- do.call(match_pattern, vector_a)
      
      indices_a <- grep(pattern_A, rownames(BLUP))
      BLUP_wide <- BLUP[indices_a, ]
      
      BLUP_wide$PEV_a <- BLUP_wide[ ,2]^2
      BLUP_wide$acc_a <- sqrt(1 - (BLUP_wide$PEV_a/vc_mod[2, 1])) # atenção com o parentesis
      Acc_PEV_a <- mean(BLUP_wide$acc_a)
      
      # Accuracy PEV D
      vector_d = list(vector_d = c(pattern[1], pattern[2]), pattern_X = pattern[2]) # "pattern_D"
      pattern_D = vector_d$pattern_X
      
      indices_d <- grep(pattern_D, rownames(BLUP))
      BLUP_wide <- BLUP[indices_d, ]

      BLUP_wide$PEV_d <- BLUP_wide[ ,2]^2
      BLUP_wide$acc_d <- sqrt(1 - (BLUP_wide$PEV_d/vc_mod[3, 1]))  # atenção com o parentesis
      Acc_PEV_d <- mean(BLUP_wide$acc_d)
      
      # Summary of the model
      sum_mod <- mod
      
      # Store results
      results_list1[[j]] <- list(vc_mod, H2a, d2, H2g, Acc_PEV_a, Acc_PEV_d)
      results_list2[[j]] <- list(sum_mod)
      

    } # fecha o j
    results_list_vc[[i]] <- results_list1
    results_list_sum[[i]] <- results_list2
    
    
  } #fecha o i
  
  return(list(VarComp = results_list_vc, Summary= results_list_sum))
  
} #fecha a funcao

