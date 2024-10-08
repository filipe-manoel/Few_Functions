# This function aims to find discontinuities in the field trials ("holes")
# and fill them with row, column, and genotype information.
# In this way, the trial becomes rectangular, allowing spatial analysis to be
# performed by AR1 model.
# All terms in the function should be declared as strings

# Esta função visa encontrar descontinuidades no croqui ("buracos") 
# e preenchê-las com informações de linha, coluna e genótipos. 
# Desta forma, o croqui fica retangular permitindo que seja realizada a análise 
# espacial usando o modelo autoregressivo.
# Todos os termos da função devem ser declarados como strings


spats_fill <- function(df=df, col_var = "COL", row_var = "ROW", gen_value = "GENx", nest.eff = NULL) {
  
  df$COL_ROW =  paste(df$COL, df$ROW, sep = "_")
  
  df = df
  COL = "COL"
  ROW = "ROW"
  GENx = "GENx"
  
  c = nlevels(df$COL)
  r = nlevels(df$ROW)
  
  c1 = rep(seq(1:c),r)
  c1 = sort(c1)        # Important!
  r1 = rep(seq(1:r),c)
  
  aux1 = data.frame(cbind(c1,r1))
  aux1$COL_ROW =  paste(aux1$c1, aux1$r1, sep = "_")
  
  # Remove duplicates based on the "COL_ROW" column
  df <-  df %>%
    distinct(COL_ROW, .keep_all = TRUE)
  
  aux1 <-  aux1 %>%
    distinct(COL_ROW, .keep_all = TRUE)
  
  # Create aux2
  aux2 = anti_join(aux1, df, by= "COL_ROW")
  names(aux2) = c("COL", "ROW", "COL_ROW")
  
  aux2$COL = as.factor(aux2$COL)
  aux2$ROW = as.factor(aux2$ROW)
  aux2$COL_ROW = as.factor(aux2$COL_ROW)
  aux2$GEN = as.factor("GENx")
  
  
  # If colname is NULL, do nothing and return the data as is
    if (is.null(nest.eff)) {
      message("No nested effect specified.")
    } else {
  
    # If nest.eff is provided, use it
    if (nest.eff %in% colnames(df)) {
        new_col_name = nest.eff
        aux2[[new_col_name]] = i
        aux2[[new_col_name]] = as.factor(aux2[[new_col_name]])
        message("New column '", new_col_name, "' created based on ", nest.eff)
    } else {
      stop("The nested effect provided does not exist on the data.")
    }
  }  
 
  #Create combined df
  combined_df <- bind_rows(df, aux2)
  
  all_columns <- union(names(df), names(aux2))
  
  df1_aligned <- df %>%
    dplyr::mutate(across(setdiff(all_columns, names(df)), ~ NA, .names = "{col}"))
  
  df2_aligned <- aux2 %>%
    dplyr::mutate(across(setdiff(all_columns, names(df)), ~ NA, .names = "{col}"))
  
  results  <- rbind(df1_aligned, df2_aligned)
  
  
  print(results)
}


##### Aplication #####

# zdata = spats_fill(df = df, col_var = "COL", row_var = "ROW", gen_value = "GENX")
# dim(zdata)
# 
# # Now it is possible to run residual AR1 models
#
# zdata = zdata[order(zdata$COL, zdata$ROW), ]
# 
# mod_ar1 = asreml(
#   fixed = Y_std ~ TRIAL:REP + GEN,
#   random = ~  REP:IBLC, 
#   residual = ~ ar1v(COL):ar1(ROW),
#   maxit = 50, #trace = F,
#   workspace = "1gb",
#   na.action = na.method(x = "include", y = "include"),
#   data = zdata)
# 
# summary(mod_ar1)$varcomp

