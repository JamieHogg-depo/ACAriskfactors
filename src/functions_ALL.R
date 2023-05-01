## ----------------------------------------------------------------------------
## FUNCTIONS ## 
## ----------------------------------------------------------------------------

## ----------------------------------------------------------------------------
#dataDict <- readRDS("P:/R Projects/functions/dataDict.rds")

## ----------------------------------------------------------------------------
#' @param dataset
cat_tables <- function(dta){
  # Convert all variables with Haven labels to factors
  t <- dta %>% mutate_all(StataValLabels_2_RFactors)
  # Print the tables
  t %>% select_if(~class(.)== "factor") %>% map(table)
}

## ----------------------------------------------------------------------------
#' @param x 
StataValLabels_2_RFactors <- function(x){
  if(is.labelled(x)){
    return(haven::as_factor(x))
  } else{
    return(x)
  }
}

## ----------------------------------------------------------------------------
#' @param name non-character for the variable that will be added
#' @param concor logical (defaults to F)
#' @param all_combs logical (defaults to T).
addGroupID <- function(.data, name, ..., concor = F, all_combs = T){

	if(concor){
		if(all_combs){
			.data %>%
				dplyr::select(...) %>%
				filter(!duplicated(.)) %>%
				complete(...) %>%
				group_by(...) %>%
				summarise("{{name}}" := cur_group_id(), 
							.groups = "drop") %>%
				filter(complete.cases(.))
		}else{
			.data %>% 
				group_by(...) %>%
				summarise("{{name}}" := cur_group_id(), 
							.groups = "drop")
		}
	}else{
		.data %>% 
			group_by(...) %>%
			mutate("{{name}}" := cur_group_id()) %>%
			ungroup()
	}

}

## ----------------------------------------------------------------------------
#' @param draws matrix of posterior draws (its x obs)
#' @param baseline_odds (defaults to 1) odds of national prevalence
getORs <- function(draws, baseline_odds = 1){
  
  odd_fun <- function(x, baseline_odds){
    (x/(1-x))/baseline_odds
  }
  
  # return draws of ORs
  return(odd_fun(draws, baseline_odds))

}

## ----------------------------------------------------------------------------
#' @param draws matrix of posterior draws (its x obs)
getDPP <- function(draws, null_value = 1, sig_level = 0.6){
	
	foo <- function(x){mean(ifelse(x > null_value, 1, 0))}
	
	# EP
	EP <- apply(draws, 2, foo)
	EP_low <- 1-EP
	# DPP
	DPP <- unname(abs(EP - EP_low))
	# get significance
	DPP_sig <- as.factor(ifelse(DPP > sig_level, 1, 0))
	
	# return list
	return(list(EP = EP,
				DPP = DPP,
				DPP_sig = DPP_sig))
}

## ----------------------------------------------------------------------------
getConcor <- function(.data, ..., .include_n = FALSE){
	if(.include_n){
		.data %>%
			group_by(...) %>%
			tally() %>% ungroup()
	}else{
		.data %>%
			group_by(...) %>%
			tally() %>% ungroup() %>%
			dplyr::select(-n)
	}
}

## ----------------------------------------------------------------------------
#' @param dta dataset
makeVlist <- function(dta){
  list <- sapply(dta, function(x) attr(x, "label"))
  name <- names(list)
  label <- rep(NA, length(name))
  for(i in 1:length(name)){
    if(is.null(list[[i]])){
      label[i] <- NA
    } else{
      label[i] <- list[[i]]
    }
  }
  for(i in 1:length(name)){
    if(is.na(label[i])){
      label[i] <- NA
    } else{
      if(label[i] == name[i]){
        label[i] <- NA
      }
    }
  }
  out <- tibble(name = name, 
                label = label)
  if(length(which(is.na(out$label) == TRUE)) == ncol(dta)){
    return("No useful Stata Labels :(")
  } else{
    out %>% print(n= Inf)
  }
}

## ----------------------------------------------------------------------------
#' @param data
#' @param outcome
#' @param w
#' @param unit_id
#' @param sa2_id
get_HT_estimates <- function(data, outcome, w, unit_id, sa2_id){
  # output the vector of areas IDs
  area_vec <- as.numeric(unlist((data %>% dplyr::select(sa2_id))[1]))
  
  # create a new dataset for use inside the function
  # subset the larger dataset
  d <- data %>% 
    dplyr::select(all_of(c(outcome, w, unit_id, sa2_id))) %>% 
    rename(y = outcome, weights = w, ID = unit_id, area = sa2_id) %>% 
    add_count(area)
  
  # create data.frame to store the means and SEs
  out <- data.frame(mean = as.numeric(rep(NA, length(unique(d$area)))),
                    SE = as.numeric(rep(NA, length(unique(d$area)))))
  
  ## Start for loop ## -----------------------------------------------------------
  for(i in 1:nrow(out)){
    # get the current area code
    current_sa2 <- area_vec[i]
    
    # subset the data
    sub <- d[d$area == current_sa2, ]
    
    # If we have more than 1 sample then get estimates
    if(nrow(sub)>1){
      # Define the survey design
      design <- svydesign(id = ~ID, 
                          weights = ~weights, 
                          data = sub)
      
      # Generate the HT estimate
      t <- svymean(~y, design, na.rm = TRUE)
      
      # append to dataset
      out$mean[i] <- as.numeric(t[1])
      out$SE[i] <- as.numeric(sqrt(attr(t, "var")))
    } else{
      # append to dataset
      out$mean[i] <- as.numeric(sub$y)
      out$SE[i] <- NA
    }
  }
  
  return(out)
  
}

## ----------------------------------------------------------------------------
#' @param data
#' @param var_pattern
pvt_by_pattern <- function(data, var_pattern){
  temp <- data %>% 
    dplyr::select(SA2_MAINCODE_2016, contains(var_pattern)) %>% 
    pivot_longer(-SA2_MAINCODE_2016,
                 names_to = var_pattern, 
                 names_pattern = paste0(var_pattern, "_(.*)"),
                 values_to = "value") %>% 
    mutate(!!var_pattern := as.factor(!!var_pattern))
}

## ----------------------------------------------------------------------------
#' @param var string for variable name
get_sheetrowindex <- function(var){

  # generate an empty list
  sheet_finds <- as.list(rep(NA, length(dataDict)))
  names(sheet_finds) <- names(dataDict)
  
  # for each sheet find the index where 
  # name appears
  for(i in 1:length(dataDict)){
    sheet_finds[[i]] <- which(dataDict[[i]] == var, arr.ind = TRUE)
  }
  
  # remove any list elements with NA's
  sheet_finds <- sheet_finds %>% 
    #purrr::map(discard, is.na) %>% 
    compact()
  
  return(sheet_finds)
  
}

## ----------------------------------------------------------------------------
#' @param var string for variable name
get_variableLABELS <- function(var){
  
  # create the sheet_finds object
  sheet_finds <- get_sheetrowindex(var)
  
  # create objects to store labels
  ll <- list()
  var_NAMES <- data.frame(sheet = rep(NA, length(sheet_finds)),
                          label = rep(NA, length(sheet_finds)))
  
  # iterative over any sheets that have the var
  for(j in 1:length(sheet_finds)){
    # extract the sheet name
    sh_name <- names(sheet_finds)[j]
    # which sheet is it in indexes
    match_id <- which(names(dataDict)==sh_name)
    #extract the row id within the sheet
    sh_id <- sheet_finds[[j]]
    # subset to just the current sheet
    temp_data <- dataDict[[match_id]]
    
    # output the variable NAME
    var_NAMES$label[j] <- as.character(temp_data[sh_id[1],
                                                 sh_id[2]+1])
    var_NAMES$sheet[j] <- sh_name
    
    # value labels
    # collect the values under the full variable name
    # this is an iterative procedure until we reach an NA
    val_LABELS <- NA
    for(i in 1:300){
      if(!is.na(temp_data[sh_id[1]+i, sh_id[2]+1])){
        val_LABELS[i] <- as.character(temp_data[sh_id[1]+i, sh_id[2]+1])
      } else{
        break
      }
    }
    
    # split into dataframe with number label
    # list with two-element vectors for number and label
    va <- str_split(str_replace(val_LABELS, ". ", "_"), "_")
    names(va) <- as.character(1:length(va))
    if(length(va)==1){
      if(is.na(va)){
        values <- NA
      }
    } else{
      values <- as.data.frame(t(bind_cols(va))) %>% rename(value = V1, label = V2)
    }
    
    # update the val_LABELS object
    val_LABELS <- values
    
    ll[[j]] <- val_LABELS
    names(ll)[j] <- sh_name
    message(paste0("Variable Name: ", var_NAMES$label[j]))
  }
  
  ll[[length(ll)+1]] <- unique(var_NAMES$label)
  return(ll)
  
}

## ----------------------------------------------------------------------------
#' @param data data.frame that contains the variable
#' @param var non-string for the variable
get_FACTOR <- function(data, var){
  
  # get character version of var
  val <- deparse(substitute(var))
  
  # get the variable labels
  t <- get_variableLABELS(val)
  
  # get new variable name
  col1 <- paste0(val, "_f")
  
  # create the vector
  OUT <- data %>% 
    mutate(!! col1 := factor({{var}},
                             levels = t[[1]]$value,
                             labels = t[[1]]$label)) %>% 
    dplyr::select(all_of(col1)) %>% 
    pull(1)
  
  # return the vector
  return(OUT)
}

## ----------------------------------------------------------------------------
#' @param x numeric vector
scale1 <- function(x, na.rm = FALSE){
  (x - mean(x, na.rm = na.rm))/sd(x, na.rm = na.rm)
}

## ----------------------------------------------------------------------------
#' @param x numeric vector
scale2 <- function(x, na.rm = FALSE){
  (x - mean(x, na.rm = na.rm))
}

## ----------------------------------------------------------------------------
#' @param x numeric vector
jlogit <- function(p){
	log(p/(1-p))
}

## ----------------------------------------------------------------------------
#' @name getCHIp
#' @param y character vector of the binary outcome
#' @param var character string of variables to be tested
#' @param data character string defining the data where y and var can be found
#' @describe This function will return the CHI2 test statistic and AIC, BIC for
# bivariate logistic models. The returned dataframe is order according to
# AIC. Note that these are NOT weighted models. 
getCHIp <- function(y, var, data, .multi_comp_adj = T){
  outcome <- paste0(data, "$", y)
  
  out <- data.frame(var = as.character(rep(NA, length(var))),
                    p.value = as.numeric(rep(NA, length(var))),
                    N_cat = as.numeric(rep(NA, length(var))),
                    AIC = as.numeric(rep(NA, length(var))),
                    BIC = as.numeric(rep(NA, length(var))))
  for(i in 1:length(var)){
    out$var[i] <- paste0(data, "$", var[i])
    mid <- paste0(outcome, ", ", out$var[i])
    mid2 <- paste0(outcome, " ~ ", out$var[i])
    
    # chi
    out$p.value[i] <- eval(parse(text=(paste0("round(chisq.test(", mid, ")$p.value, 5)"))))
    out$N_cat[i] <- eval(parse(text=(paste0("length(levels(", out$var[i], "))"))))
    
    # logistic
    fit <- glm(as.formula(mid2), family = "binomial")
    out$AIC[i] <- AIC(fit)
    out$BIC[i] <- BIC(fit)
  } 
  
  if(.multi_comp_adj)out$p.value <- out$p.value * length(var)
  out$var <- var
  return(arrange(out, AIC))
}

## ----------------------------------------------------------------------------
force_limits <- function(x, 
                         bottom = 0,
                         top = 1){
  if(is.null(top)){
    ifelse(x < bottom, bottom, x)
  } else if(is.null(bottom)){
    ifelse(x > top, top, x)
  } else {
    ifelse(x > top, top,
           ifelse(x < bottom, bottom, x))
  }
}

## ----------------------------------------------------------------------------
#' @param outcome_name character specifying the variable with the outcome
#' @param area_name character specifying variable with area indicators
#' @param weight_name character specifying variable with weights
#' @param data dataset where the previous three arguments are found
#' @param pop dataset with area variable and population size
jdirect <- function(outcome_name, 
                    area_name, 
                    weight_name, 
                    data, 
                    pop, 
                    ignore_errors = FALSE,
                    verbose = FALSE){
  
  dom <- unlist(data[,area_name])
  y <- as.numeric(unlist(data[,outcome_name]))
  if(ignore_errors == F){
    if(length(which(unique(y) %in% c(0,1))) != 2){
      stop("Ensure that outcome is numeric 0 and 1")
    }
  }
  sweight <- unlist(data[,weight_name])
  did <- unique(dom) # unique areas
  Dsample <- length(did) # number of areas
  
  # create empty vectors
  nds <- rep(0, Dsample)
  dirds <- rep(0, Dsample)
  vardirds <- rep(0, Dsample)
  N <- rep(0, Dsample)
  
  overall <- weighted.mean(y, w = sweight)
  if(verbose) pb <- txtProgressBar(min = 1, max = Dsample, style = 3)
  for(d in 1:Dsample){
    cur_area <- did[d] # current area
    yd <- y[dom == cur_area] # current vector of outcomes
    nds[d] <- length(yd) # sample size of current area
    N[d] <- unlist(pop[pop[,area_name]==cur_area,2]) # population size of current area
    
    sweightd <- sweight[dom == cur_area]
    w <- nds[d] * (sweightd / sum(sweightd)) # sample scaled weights
    
    # direct estimates
    dirds[d] <- sum(yd * sweightd)/sum(sweightd) 
    
    # sampling variance of direct estimates
    vardirds[d] <- (1/nds[d]) * (1 - (nds[d]/N[d])) * (1/(nds[d] - 1)) * sum( w^2 * (yd - dirds[d])^2 )
    
    # progress
    if(verbose) setTxtProgressBar(pb, d)
  }
  if(verbose) close(pb)
  
  # create output dataset
  out <- data.frame(area = did,
                    n = nds, N = N,
                    HT = dirds,
                    HT_VAR = vardirds) %>% 
    mutate(# create unstable indicator
           unstable = HT %in% c(0,1),
           # set variance to NA if unstable 
           HT_VAR = ifelse(unstable, NA, HT_VAR),
           # add small perturbation to HT
           HT = force_limits(HT, bottom = 0.001, top = 0.999),
           # HT = ifelse(HT == 0, 0.001, HT),
           # HT = ifelse(HT == 1, 0.999, HT),
           HT_SE = sqrt(HT_VAR),
           HT_CV = 100*(HT_SE/HT),
           HT_lower = force_limits(HT - 1.96 * HT_SE),
           HT_upper = force_limits(HT + 1.96 * HT_SE),
           # create empirical logit versions
           HT_el = jlogit(HT),
           HT_el_VAR = HT_VAR/((HT * (1 - HT))^2),
           HT_el_SE = sqrt(HT_el_VAR),
           OR = force_limits((HT/(1-HT))/(overall/(1-overall)), top = NULL),
           OR_lower = force_limits((HT_lower/(1-HT_lower))/(overall/(1-overall)), top = NULL),
           OR_upper = force_limits((HT_upper/(1-HT_upper))/(overall/(1-overall)), top = NULL))
	names(out)[1] <- area_name
  
  # return object
  return(out)
  
}

## -----------------------------------------------------------------------------
#' @import spdep and igraph
#' @param sf_data sf data with geometry and 5-digit sa2 codes
#' @param sa2_name character vector specifying the variable with 5digit sa2 codes. Default is "Sa2_5dig16". 
#' @param .forsa3 logical (defaults to FALSE) if fixing sa3 map
#' @return list with nb list, binary weight matrix and group membership list
getConnectedNB <- function(sf_data, 
							sa2_name = "Sa2_5dig16", 
							.forsa3 = FALSE){

require(spdep)
require(igraph)

# create joining list
joining_list <- list(to_join = c(21088, 21091, 31527,
                                 31363, 31402, 31466, 
                                 31483, 41103, 41145, 61093, 
                                 71060, 71062, 61099),
                     join2 = list(21308, 21379, 31013, 31362,
                                  31401, 31483, c(31469,31466),
                                  41100, 41128, c(61094, 21476),
                                  71021, 71063, c(61100, 21092)))

if(.forsa3){
	joining_list <- list(to_join = c(60403, 60203),
						 join2 = c(20503, 21703))
}

# get temporary nb object
nb_original<- poly2nb(sf_data)
nb_temp <- nb_original

# get sf_data ids which we wish to mutate
id_to_join <- which(unlist(sf_data[,sa2_name] %>% st_drop_geometry()) %in% as.character(joining_list$to_join))

# if none to change then proceed with mutation of nb list
if(!is_empty(id_to_join)){
  for(i in 1:length(id_to_join)){
    # find index join2
    id_join2 <- which(unlist(sf_data[,sa2_name] %>% st_drop_geometry()) %in% as.character(unlist(joining_list$join2[i])))
    # update singles
    nb_temp[[id_to_join[i]]] <- as.integer(c(nb_temp[[id_to_join[i]]], id_join2))
    if(!is_empty(-which(nb_temp[[id_to_join[i]]] == 0))){
      # remove zeros
      nb_temp[[id_to_join[i]]] <- nb_temp[[id_to_join[i]]][-which(nb_temp[[id_to_join[i]]] == 0)]
    }
  } 
}

# ensure nb object is symmetric
nb_out <- make.sym.nb(nb_temp)

# check connectedness
W <- nb2mat(nb_out, style = "B", zero.policy = TRUE)
gg <- graph.adjacency(W)
clu <- components(gg)
cc <- igraph::groups(clu)
message("There are ", length(cc), " unique groups of neighbours!")

# return the nb object
return(list(nb = nb_out, 
            W = W,
            group_membership = cc))

}

## ----------------------------------------------------------------------------
#' @param x factor/character vector
#' @return numeric vector of zeros and ones from factor x
fac2NumBin <- function(x, silence = FALSE){
  out <- as.numeric(x) - 1
  if(!silence) print(table(x, out))
  return(out)
}

## ----------------------------------------------------------------------------
first.changes <- function(d){
	p <- cumsum(rle(d)$lengths) + 1
	c(1, p[-length(p)])
}

## ----------------------------------------------------------------------------
jinvlogit <- function(eta){
	exp(eta)/(1+exp(eta))
}

## ----------------------------------------------------------------------------
#' @param hmc_draws matrix of posterior draws for proportions
#' @param nat_prop numeric HT estimate for national average
#' @return returns a dataframe with ps_area, PP of estimates being greater than nat_prop and significance
getNationalAveragePP <- function(hmc_draws, nat_prop){
  
  getPostProbG <- function(x, nat_prop){
    mean(x > nat_prop)
  }
  
  p <- apply(hmc_draws, 2, getPostProbG, nat_prop = nat_prop)
  sig <- as.factor(ifelse(p > 0.95 | p < 0.05, 1, 0))
  return(data.frame(post_prob_greater = p,
                    significant = sig,
                    ps_area = 1:ncol(hmc_draws)))
  
}

## -----------------------------------------------------------------------------
#' @param connected_nb a fully connected nb list
#' @param zero_policy logical; defaults to F
mungeCARdata4stan = function(connected_nb, zero.policy = F) {
  listw <- nb2listw(connected_nb, zero.policy = zero.policy)
  bgs <- listw2WB(listw)
  adjBUGS <- bgs$adj
  numBUGS <- bgs$num
  N = length(numBUGS);
  nn = numBUGS;
  N_edges = length(adjBUGS) / 2;
  node1 = vector(mode="numeric", length=N_edges);
  node2 = vector(mode="numeric", length=N_edges);
  iAdj = 0;
  iEdge = 0;
  for (i in 1:N) {
    for (j in 1:nn[i]) {
      iAdj = iAdj + 1;
      if (i < adjBUGS[iAdj]) {
        iEdge = iEdge + 1;
        node1[iEdge] = i;
        node2[iEdge] = adjBUGS[iAdj];
      }
    }
  }
  #return (list("N"=N,"nu_edges"=N_edges,"node1"=node1,"node2"=node2));
  return (list("nu_edges"=N_edges,"node1"=node1,"node2"=node2))
}

## ----------------------------------------------------------------------------
#' @param survey_data survey df
#' @param survey_area_var character specifying variable with which to join to aux_data
#' @param aux_data auxiliary df
#' @param aux_area_var character specifying variable with which to join to survey_data
#' @param var_lab character specifying the name of the variable to be added.
getSeq_ps <- function(survey_data, 
                      survey_area_var,
                      aux_data,
                      aux_area_var,
                      var_lab = "area"){
  
  # add "ps_" to new pseudo variable
  variable_label <- paste0("ps_", var_lab)
  
  # construct the list of unique values
  survey_areas <- unique(unlist(survey_data[,survey_area_var]))
  nonsurvey_areas <- unique(unlist(aux_data[,aux_area_var]))[!unique(unlist(aux_data[,aux_area_var])) %in% survey_areas]
  all_areas <- c(survey_areas, nonsurvey_areas)
  
  # create concordance file
  data.frame(area = all_areas,
             ps_ = 1:length(all_areas)) %>% 
    setNames(c(names(.)[1], variable_label))
  
}

## ----------------------------------------------------------------------------
#' @param stan_its matrix posterior draws from stan object (its by n_obs)
#' @param n_chains how many chains were used
getLOO_s2LN <- function(stan_its, n_chains){
  
  # Derive the different posterior draws matrices
  y <- stan_its$bar_theta_s1
  sd <- stan_its$sqrt_gamma
  
  # get objects
  n_obs <- ncol(y)
  n_its <- nrow(y)
  n_its_pc <- n_its/n_chains
  
  # get draws of nu
  # select the columns with data
  draws <- stan_its$nu[,1:n_obs]
  
  # create the correct matrices
  y_vec_mat <- matrix(apply(y, 2, median), 
                      byrow = T, 
                      nrow = n_its, 
                      ncol = n_obs)
  sd_vec_mat <- matrix(apply(sd, 2, median),
                       byrow = T, 
                       nrow = n_its, 
                       ncol = n_obs)
  
  # get the log_lik matrix
  log_lik <- dnorm(y_vec_mat, draws, sd_vec_mat, log = TRUE)
  
  message("Now running loo...")
  
  # derive the relative effective sample size
  r_eff <- loo::relative_eff(exp(log_lik), chain_id = sort(rep(1:n_chains,n_its_pc)))
  
  # return the loo
  return(loo::loo(log_lik, r_eff = r_eff, cores = 1))
}


## ----------------------------------------------------------------------------
#' @param survey_data survey df
#' @param survey_area_var character specifying variable with which to join to aux_data
#' @param aux_data auxiliary df
#' @param aux_area_var character specifying variable with which to join to survey_data
#' @param var_lab character specifying the name of the variable to be added. 
#' Function will automatically add "ps_" to argument. Defaults to "area"
#' @param map_data sf object
#' @param aux_area_var character specifying variable with which to join to map_data
#' @return list with two elements (survey_data, aux_data) where each have a column called 'variable_label' added

addPseudoAreaCodes <- function(survey_data, 
                               survey_area_var,
                               aux_data,
                               aux_area_var,
                               var_lab = "area",
                               map_data = NULL, 
                               map_area_var = NULL){
  
  # add "ps_" to new pseudo variable
  variable_label <- paste0("ps_", var_lab)
  
  # delete any ps_* variables already in data
  if( length(which(colnames(survey_data) == variable_label)) == 1 ){
    survey_data <- survey_data %>% dplyr::select(-all_of(variable_label))
	  aux_data <- aux_data %>% dplyr::select(-all_of(variable_label))
  }

  # get unique areas in survey_data
  survey_areas <- unique(unlist(survey_data[,survey_area_var]))
  nonsurvey_areas <- unique(unlist(aux_data[,aux_area_var]))[!unique(unlist(aux_data[,aux_area_var])) %in% survey_areas]
  #nonsurvey_areas <- unlist(aux_data[,aux_area_var])[!unlist(aux_data[,aux_area_var]) %in% survey_areas]
  all_areas <- c(survey_areas, nonsurvey_areas)
    
    # create concordance file
    c <- data.frame(area = all_areas,
                    ps_ = 1:length(all_areas)) %>% 
      setNames(c(names(.)[1], variable_label))
  
  # join aux_data and concordance
  fun <- paste0("left_join(aux_data, c, by = c('", aux_area_var, "' = 'area'))")
  aux <- eval(parse(text = fun)) %>% 
    relocate(variable_label) %>%
	arrange(.data[[variable_label]])
	#arrange(.dots = variable_label)
  
  # join survey_data and concordance
  fun <- paste0("left_join(survey_data, c, by = c('", survey_area_var, "' = 'area'))")
  survey <- eval(parse(text = fun)) %>% 
    relocate(variable_label) %>%
	arrange(.data[[variable_label]])
	#arrange(.dots = variable_label)
  
  # join map_data and concordance
  if(!is.null(map_data)){
    # delete any ps_* variables already in data
    if( length(which(colnames(map_data) == variable_label)) == 1 ){
      map_data <- map_data %>% dplyr::select(-all_of(variable_label))
    }
    
    fun <- paste0("left_join(map_data, c, by = c('", map_area_var, "' = 'area'))")
    map <- eval(parse(text = fun)) %>% 
      relocate(variable_label) %>%
	  arrange(.data[[variable_label]])
	  #arrange(.dots = variable_label)
    
    # output datasets
    return(list(survey = survey, aux = aux, map = map, concordance = c))
    
  }else{
    
    # output datasets
    return(list(survey = survey, aux = aux, concordance = c))
    
  }
  
}

## ----------------------------------------------------------------------------
#' @param W binary contiguity matrix (must be complete)
#' @return list of objects needed for efficient Leroux Stan function

mungeLEROUXdata4stan <- function(W){

	# create sparse matrices
	W <- Matrix::Matrix(W, sparse = TRUE)
	M <- nrow(W)
	Ni <- Matrix::rowSums(W)
	D <- Matrix::Diagonal( x = Ni )
	I <- Matrix::Diagonal( M )
	J <- I + W - D
	
	# Eigenvalues of J
	lambda <- eigen(J)$values
	
	# get the CRS representation of J
	crs <- rstan::extract_sparse_parts(J)
	nJ_w <- length(crs$w)
	
	# prepare output list
	return(
	list(lambda = lambda, 
		 nJ_w = nJ_w,
		 J_w = crs$w,
		 J_v = crs$v,
		 J_u = crs$u,
		 D_id_J_w = which(crs$w != 1),
		 offD_id_J_w = which(crs$w == 1))
	)

}

## ----------------------------------------------------------------------------
#' @param hmc_draws matrix of posterior draws for coefficients
#' @param baseline numeric value for which to evaluate the posterior probability. Defaults to zero. 
#' @param recommend logical defaulting to FALSE.
#' @param x design matrix passed to Stan. This is used to get covariate names. 
#' @return Setting recommend = F returns a vector of Bayesian p-values. Setting
# recommend = T returns a dataframe of p-values and recommendations
getpvalues <- function(hmc_draws,
                       baseline = 0,
                       recommend = F,
                       x = NULL){
  
  pps <- apply( hmc_draws, 2, function(x){mean(x > baseline)} )
  
  if(recommend == F){
    pps
  }else{
   	data.frame(covariate = colnames(x),
   	           p_value = pps,
   	           use = ifelse(pps < 0.1 | pps > 0.9, "Use", ".")) 
  }
}

## ----------------------------------------------------------------------------
#' @param fit stan fit object
getStanSummary <- function(fit){
	as.data.frame(summary(fit)$summary) %>% rownames_to_column("parameter")
}

## ----------------------------------------------------------------------------
#' @param draws posterior draws of size its x n_obs
#' @param truth vector of length n_obs
#' @return single value
getMRRMSE <- function(draws, truth, .mean = TRUE){
  out <- rep(NA, length(truth))
  for(i in 1:ncol(draws)){
    out[i] <- (sqrt(mean((truth[i]-draws[,i])^2)))/truth[i]
  }
  ifelse(.mean, return(mean(out)), return(out))
}

## ----------------------------------------------------------------------------
#' @param draws posterior draws of size its x n_obs
#' @param truth vector of length n_obs
#' @return single value
getMARB <- function(draws, truth, .mean = TRUE){
  out <- rep(NA, length(truth))
  for(i in 1:ncol(draws)){
    out[i] <- abs((mean(truth[i]-draws[,i]))/truth[i])
  }
  ifelse(.mean, return(mean(out)), return(out))
}

## ----------------------------------------------------------------------------
#' @param summ sumary object from stan (e.g. getStanSummary)
#' @param regex regex string used in str_starts (for multiple variables use 'mu\\[')
subsetSummary <- function(summ, regex){
	summ[str_starts(summ$parameter, regex),]
}

## ----------------------------------------------------------------------------
#' @param ll_ist list element (e.g. s2LN_list)
#' @description The function will automatically find all elements that start with summary
# and return various diagnostics measures for them. 
getConvergence <- function(ll_ist){

  # get summary objects
  ll <- ll_ist[which(str_detect(names(ll_ist), "summary"))]
  
  # define function
  sum_func <- function(x){
    c(Rhat_percgreater1.02 = 100*mean(x[x$sd != 0,]$Rhat > 1.02, na.rm = T),
      Rhat_max = max(x[x$sd != 0,]$Rhat, na.rm=T),
      ESS_median = median(x[x$sd != 0,]$n_eff, na.rm=T),
      ESS_min = min(x[x$sd != 0,]$n_eff, na.rm=T),
      ESS_percgreater1000 = 100*mean(x[x$sd != 0,]$n_eff > 1000, na.rm = T),
	  ESS_percgreater500 = 100*mean(x[x$sd != 0,]$n_eff > 500, na.rm = T))
  }
  
  # create dataset
  bind_rows(lapply(ll, sum_func)) %>% 
  mutate(spec = names(ll),
		 no_divs = length(which(rstan::get_divergent_iterations(ll_ist$fit)))) %>% 
  relocate(spec)
}

## ----------------------------------------------------------------------------
#' @param hmc_draws matrix of posterior draws
#' @param model_name character defining which model is in hmc_draws
#' @param prefix character vector to add before static names (defaults to "")
#' @param p_values logical defaulting to F
#' @param baseline numeric value for which to evaluate the posterior probability. Defaults to zero.
#' @param credMass numeric (defaults to 0.95) mass with the HDI
#' @return returns dataframe with mean, median, sd, lower and upper
getMCMCsummary <- function(hmc_draws,
						   model_name = NULL,
                           prefix = "",
                           p_values = F,
                           baseline = 0,
						   credMass = 0.95){
	
	# create summary function
	sum_func <- function(x){
		c(mean = mean(x), 
		  median = median(x), 
		  sd = sd(x),
		  lower = unname(HDInterval::hdi(x, credMass = credMass)[1]), #unname(quantile(x, 0.025, na.rm = T)),
		  upper = unname(HDInterval::hdi(x, credMass = credMass)[2]), #unname(quantile(x, 0.975, na.rm = T)))
		  hpd38 = unname(diff(HDInterval::hdi(x, credMass = 0.38))))
	}
	
	# calculate output  when we want p_values
	if(p_values == T){
		bind_rows(lapply(asplit(hmc_draws, 2), sum_func)) %>%
			mutate(CV = 100 * (sd/mean),
				   CV_b = 100 * (hpd38/median),
			       cisize = upper - lower,
			       pvalue = getpvalues(hmc_draws, baseline = baseline),
				   model = model_name) %>%
		relocate(model) %>%
	    setNames(c("model", paste0(prefix, names(.)[-1])))
			
	}else{
	# calculate output when we do not want p_values
		bind_rows(lapply(asplit(hmc_draws, 2), sum_func)) %>%
			mutate(CV = 100 * (sd/mean),
				   CV_b = 100 * (hpd38/median),
			       cisize = upper - lower,
				   model = model_name) %>%
		relocate(model) %>%
	    setNames(c("model", paste0(prefix, names(.)[-1])))
	}

}

## -----------------------------------------------------------------------------
#' @param x vector of point estimates
#' @param lower vector of lower bounds
#' @param upper vector of upper bounvectior ofs
#' @returns vector of logicals
between_vec <- function(x, lower, upper){
	out <- rep(NA, length(x))
	for(i in 1:length(x)){
	  out[i] <- between(x[i], lower[i], upper[i])
	}
	return(out)
}

## -----------------------------------------------------------------------------
#' @param mu_draws
#' @param null_value national wide prevalence
summary_fit <- function(mu_draws, null_value){

	  
	# difference in posterior probability
	list_prop_DPP <- getDPP(mu_draws, null_value = null_value)
	prop_DPP <- bind_cols(list_prop_DPP) %>% 
	  mutate(ps_area = 1:nrow(.))
	  
	# summary of prevalences
	prop_summ <- getMCMCsummary(mu_draws) %>% 
	  mutate(ps_area = 1:nrow(.)) %>%
	  left_join(., prop_DPP, by = "ps_area")

	# ors
	HT_odds <- null_value/(1-null_value)
	odds_draws <- getORs(mu_draws, baseline_odds = HT_odds)
	list_or_DPP <- getDPP(odds_draws, null_value = 1)
	or_DPP <- bind_cols(list_or_DPP)%>% 
	  mutate(ps_area = 1:nrow(.))
	or_summ <- getMCMCsummary(odds_draws) %>% 
	  mutate(ps_area = 1:nrow(.)) %>%
	  left_join(.,or_DPP, by = "ps_area")

	# return list
	return(list(prop_summ = prop_summ,
				or_summ = or_summ))
         
}

## -----------------------------------------------------------------------------
#' @param x vector of values between 0 and 1
#' @param mean numeric between 0 and 1. Mean of beta distribution
#' @param sd numeric, greater than 0. Standard deviation of beta distribution
dbetaMP <-function(x,
                   mean, 
                   sd){
  var <- sd^2
  phi <- ((mean * (1-mean))/var) - 1
  alpha <- mean * phi
  beta <- phi - alpha
  message(paste0("Alpha: ", alpha, ", Beta: ", beta))
  dbeta(x, alpha, beta)
}

## -----------------------------------------------------------------------------
#' @param W fully connected binary contiguity weight matrix
#' @return numeric scaling factor for BYM2 model
getBYM2scale <- function(adj_mat){
  
  # get necessary arguments for function
  adj_mat <-as(adj_mat, "sparseMatrix")
  M <- nrow(adj_mat)
  
  # ICAR precision matrix
  Q <- Matrix::Diagonal(M, Matrix::rowSums(adj_mat)) - adj_mat
  # Add a small jitter to the diagonal for numerical stability
  Q_pert <- Q + Matrix::Diagonal(M) * max(Matrix::diag(Q)) * sqrt(.Machine$double.eps)
  
  # Compute the diagonal elements of the covariance matrix subject to the
  # constraint that the entries of the ICAR sum to zero
  .Q_inv <- function(Q){
    Sigma <- Matrix::solve(Q)
    A <- matrix(1,1, NROW(Sigma))
    W <- Sigma %*% t(A)
    Sigma <- Sigma - W %*% solve(A %*% W) %*% Matrix::t(W)
    return(Sigma)
  }
  
  # get Q_inv
  Q_inv <- .Q_inv(Q_pert)
  
  # Compute the geometric mean of the variances (diagonal of Q_inv)
  exp(mean(log(Matrix::diag(Q_inv))))
  
  
}

## -----------------------------------------------------------------------------
#' @param design_mat design matrix directly from model.matrix() function
#' @return list with three elements. 'for_stan' is a list with the Q_ast, R_ast and R_ast_inverse matrices. 
# element x_c gives the centered and non-intercept version of design_mat.
# element q gives the number of columns of x_c. 
getStanQR <- function(design_mat, ...){
  # remove columns with zero counts
  if(length(which(colSums(design_mat) == 0)) != 0){
    design_mat <- design_mat[,-which(colSums(design_mat) == 0)]
  }
  x <- design_mat
  # get n
  n <- nrow(x)
  # center all columns
  x <- scale(x, scale = F, ...)
  # remove intercept
  x <- x[,-1]
  k <- ncol(x)
  # take the QR decomposition
  qrstr <- qr(x)
  Q <- qr.Q(qrstr)
  R <- qr.R(qrstr)
  R_ast <- (1/sqrt(n - 1)) * R
  if(det(R_ast) == 0){
    stop(paste0("Singular R_ast matrix. Please try reducing the number of covariates. ",
                "There are ", length(which(abs(eigen(R_ast)$values) < 0.0001)), 
                " eigenvalues less than 0.0001."))
  }
  R_ast_inverse <- solve(R_ast)
  return(list(for_stan = list(Q_ast = Q * sqrt(n - 1),
                              R_ast = R_ast,
                              R_ast_inverse = R_ast_inverse),
              x_c = x,
              q = k))
}

## -----------------------------------------------------------------------------
#' @description Function that adds the correctly weighted survey weights to 
# the NHS dataset. Best to be used within a pipe. 
addSurveyWeights <- function(.data){
  sumW <- sum(.data$NHIFINWT)
  sample_size <- nrow(.data)
  .data %>% 
    group_by(SA2) %>% 
    mutate(n_persons = n(),
           sumWsa2 = sum(NHIFINWT)) %>%
    ungroup() %>% 
    mutate(# original sample weights
      w_original = NHIFINWT, 
      # weights that sum to SA2 adult populations
      w_pop = w_original * (N_persons/sumWsa2),
      # weights that sum to SA2 sample size
      w_sample = w_original * (n_persons/sumWsa2),
      # weights that sum to overall sample size
      w_sample_ps = w_original * (sample_size/sumW))
}

## -----------------------------------------------------------------------------
#' @param stan_fit stan model object
#' @param direct_df dataframe with direct estimates
explorePlot <- function(stan_fit, 
                        direct_df){
  stan_fit %>% 
    spread_draws(mu[ps_area]) %>% 
    median_hdci() %>% 
    right_join(.,direct_df, by = "ps_area") %>% 
    ggplot(aes(y = mu, x = HT, 
               col = cut_number(n, n = 4),
               ymin = .lower, ymax = .upper)) + 
    geom_errorbar()+
    geom_point()+
    xlim(0,1)+
    ylim(0,1)+
    geom_abline(slope = 1, intercept = 0)
}

## ----------------------------------------------------------------------------
#' @param capital which capital city to subset to
#' @description should be used as a geom_* when using ggplot() to create maps
subset2Capital <- function(capital, ...){
  
  # Create initial list of capital cities
  # lims <- list(Brisbane = list(xlim = c(1832429.78, 1861917.08),
                             # ylim = c(-3236788.81, -3269337.58)),
             # Sydney = list(xlim = c(1554778.93, 1584318.50),
                           # ylim = c(-3898068.10, -3940799.69)),
             # Canberra = list(xlim = c(1359825.16, 1379812.67),
                             # ylim = c(-4041822.32,-4062727.25)),
             # Melbourne = list(xlim = c(939035.44, 1002491.44),
                              # ylim = c(-4278384.62, -4313003.22)),
             # Adelaide = list(xlim = c(408177.75,433544.42),
                             # ylim = c(-3919309.16,-3951790.73)),
             # Perth = list(xlim = c(-1702956.37,-1677884.5),
                          # ylim = c(-3704295.6,-3750182.8)))
  
  # Create initial list of capital cities
  lims <- list(Brisbane = list(xlim = c(152.696, 153.278),
                               ylim = c(-27.72, -27.31)),
               Sydney = list(xlim = c(151.070251, 151.2975),
  						       ylim = c(-33.965003, -33.772869)),
               Canberra = list(xlim = c(149.035378, 149.175453),
                               ylim = c(-35.342575, -35.237487)),
               Melbourne = list(xlim = c(145.056610, 144.845123),
								ylim = c(-37.756601, -37.875937)),
               Adelaide = list(xlim = c(408177.75,433544.42),
                               ylim = c(-3919309.16,-3951790.73)),
               Perth = list(xlim = c(-1702956.37,-1677884.5),
                            ylim = c(-3704295.6,-3750182.8)))
  
  # make selection
  selection <- lims[[which(names(lims) == capital)]]
  
  # provide the correct coord_sf() geom
  coord_sf(xlim = selection$xlim,
           ylim = selection$ylim)
}

## -----------------------------------------------------------------------------
# all elements should be the same length
getSR <- function(y, p, w, area){
  mu <- weighted.mean(y, w = w)
  
  data.frame(p = p, y = y, w = w, area = area) %>% 
    mutate(top = w*(y - p),
           bottom = w*(y-mu)) %>% 
    group_by(area) %>% 
    summarise(n = n(),
              top = abs(sum(top)/sum(w)),
              bottom = abs(sum(bottom)/sum(w))) %>% 
    summarise(SR = sum(top)/sum(bottom))
}

## -----------------------------------------------------------------------------
# all elements should be the same length
getWOLS <- function(y, p, w, area, vars){
  
  temp <- data.frame(p = p, y = y, w = w, area = area) %>% 
    group_by(area) %>% 
    summarise(n = n(),
			  d = weighted.mean(y, w),
			  pd = weighted.mean(p, w)) %>%
	filter(d > 0 & d < 1)
	
	# return WOLS
	unname(coef(lm(pd ~ d, data = temp, weights = n))[2])
}

## ----------------------------------------------------------------------------
# Series of functions from the manuscript
#' @param y vector
#' @param p vector
#' @param w vector
#' @param N_vec (eg. NHS$N_persons)
sae_met <- list()

sae_met$eq111 <- function(y,w,N_vec){
  
  n <- length(y)
  N <- unique(N_vec)
  
  # standardize weights
  w_ss <- n * (w / sum(w))
  
  # return direct estimate
  sum(y * w_ss)/sum(w_ss)
}

sae_met$eq112 <- function(y,w,N_vec){
  
  n <- length(y)
  N <- unique(N_vec)
  
  # standardize weights
  w_ss <- n * (w / sum(w))
    
  # direct estimate
  d <- sae_met$eq111(y,w,N_vec)
  
  # return sampling variance
  (1/n) * (1-(n/N)) * (1/(n-1)) * sum(w_ss^2 * (y - d)^2)
}

sae_met$eq212 <- function(y,p,w,N_vec){
  
  n <- length(y)
  N <- unique(N_vec)
  
  # standardize weights
  w_ss <- n * (w / sum(w))
  
  # return s1 estimate
  sum(p * w_ss)/sum(w_ss)
}

sae_met$eq214 <- function(y,p,w,N_vec){
  
  n <- length(y)
  N <- unique(N_vec)
  
  # standardize weights
  w_ss <- n * (w / sum(w))
  
  # return bias estimate
  sum((p-y) * w_ss)/sum(w_ss)
}

sae_met$eq213 <- function(y,p,w,N_vec){
  
  N <- unique(N_vec)
  
  # return s1 sampling variance estimate
  sae_met$eq112(y,w,N_vec) + sae_met$eq112(p-y,w,N_vec)
}

sae_met$getMETRICS <- function(y, p, w, N, area){
  
  data.frame(p = p, y = y, w = w, N = N, area = area) %>% 
		group_by(area) %>% 
		summarise(theta_D = sae_met$eq111(y,w,N = N),
				  psi_D = sae_met$eq112(y,w,N = N), 
				  theta_S1 = sae_met$eq212(y,p,w,N = N),
				  psi_S1 = sae_met$eq213(y,p,w,N = N))
				  
}

sae_met$getWOLS <- function(y, p, w, N, area){
  
  temp <- sae_met$getMETRICS(y, p, w, N, area) %>% 
	filter(theta_D > 0 & theta_D < 1)
	
	# return WOLS
	unname(coef(lm(theta_S1 ~ theta_D, data = temp, weights = 1/psi_D))[2])
}

## ----------------------------------------------------------------------------
#' @param x factor vector fed to `fac2NumBin()` function
#' @param df survey data (e.g. NHS)
#' @param census census dataset (e.g. aux)
getVarStability <- function(x, df, census){
  
  out <- list()
  
  # setup for function
  df$y <- fac2NumBin(x)
  HT_mu <- with(df, weighted.mean(y, w = w_sample))
  
  # Add correct SA2 pseudo-codes
  out_sa2 <- addPseudoAreaCodes(df, "SA2", census, "SA2", var_lab = "area")
  
  # Create SA2, SA3 concordance data
  area_sa_concor <- out_sa2$concordance %>% 
    rename(SA2 = area) %>% 
    mutate(SA3 = as.numeric(str_sub(SA2, 1, 5)),
           SA4 = as.numeric(str_sub(SA2, 1, 3)),
           state = as.character(str_sub(SA2, 1, 1))) %>% 
    left_join(.,getSeq_ps(out_sa2$survey, "SA3", out_sa2$aux, "SA3", var_lab = "sa3"), by = c("SA3" = "area")) %>% 
    left_join(.,getSeq_ps(out_sa2$survey, "SA4", out_sa2$aux, "SA4", var_lab = "sa4"), by = c("SA4" = "area")) %>% 
    addGroupID(ps_state, state)
  
  # join to survey data
  df <- out_sa2$survey %>% 
  left_join(., area_sa_concor, by = c("ps_area", "SA2")) %>% 
  droplevels() %>% 
  addSurveyWeights()
  
  # join to auxiliary data
  census <- out_sa2$aux %>% 
  left_join(., area_sa_concor, by = c("SA2", "ps_area"))
  
  # get the direct estimates
  out$sample_agg <- jdirect(outcome_name = "y",
                      area_name = "ps_area",
                      weight_name = "w_sample",
                      data = df, 
                      pop = census %>% dplyr::select(ps_area, N_persons))
  
  # get overall weighted prevalence
  out$HT_mu <- with(df, weighted.mean(y, w = w_sample))
  
  # number of unstable
  out$unstable <- prop.table(table(out$sample_agg$unstable))
  
  # summary of HT
  out$summ <- summary(out$sample_agg$HT)
  
  # return list
  return(out)
}

## ----------------------------------------------------------------------------
#' @param i which sa4 to plot (possible integers 1 -> number of sa4s)
#' @description Helps to compare the sa4 level prevalence estimates
# by the 4 models. This function is not generic and thus requires 
# that all necessary values are already loaded into the environment.

plot_comparesa4 <- function(i){
  temp <- bind_rows(list(
    sa4_agg %>% filter(ps_sa4 == i) %>% 
      dplyr::select(HT, HT_lower, HT_upper) %>% 
      mutate(model = "Direct") %>% 
      rename(median = HT,
             lower = HT_lower,
             upper = HT_upper),
    ELN_list$sa4_summ %>% 
      dplyr::select(median, lower, upper, model) %>% 
      mutate(model = "ELN") %>% 
      slice(i),
    s2LN_list$sa4_summ %>% 
      dplyr::select(median, lower, upper, model) %>% 
      mutate(model = "TSLN") %>% 
      slice(i),
    LOG_list$sa4_summ %>% 
      dplyr::select(median, lower, upper, model) %>% 
      mutate(model = "LOG") %>% 
      slice(i)
  ))
  
  temp %>% 
    ggplot(aes(y = median, ymin = lower, ymax = upper,
               x = model)) +
    geom_errorbar()+
    geom_point()
}

## ----------------------------------------------------------------------------
#' @description Returns a vector of overlap probabilites. 
# Will be equal to 1 if the first interval is 
# inside the second. 
# The goal is that the best fitting model gives the largest sum. 
#' @example `with(ELN_list$sa4_summ, overlap_v(lower, upper, HT_lower, HT_upper))`

overlap_v <- function(direct_l, direct_u, est_l, est_u){
  proportion_overlap <- function(interval1, interval2){
    overlap_start <- max(interval1[1],interval2[1])
    overlap_end <- min(interval1[2],interval2[2])
    
    overlap_length = max(0, overlap_end - overlap_start)
    
    if(interval1[1] >= interval2[1] & interval1[2] <= interval2[2]){
      return(1)
    }else{
      return(overlap_length/max((interval1[2] - interval1[1]),(interval2[2] - interval2[1])))
    }
    
  }
  
  out <- as.numeric()
  for(i in 1:length(direct_l)){
    out[i] <- proportion_overlap(c(direct_l[i], direct_u[i]), 
                                 c(est_l[i], est_u[i]))
  }
  
  return(out)
}
