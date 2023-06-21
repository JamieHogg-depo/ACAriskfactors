data {
	int<lower=0> n;  						// sample size
	int<lower=0> m; 						// number of sampled areas
	int<lower=0> M; 						// total number of areas
	vector[m] FPC; 							// finite population correction for sampling variance
	
	int<lower=0> q_u; 						// number of parameters
	real<lower=0> residual_scale;			// standard deviation for logit(p) ~ N(gmma, residual_scale)
	matrix[n, q_u] Q_ast;
	matrix[q_u, q_u] R_ast;
	matrix[q_u, q_u] R_ast_inverse;
	
	// Demographic interaction
	real bool_demo_re;
	int demo_re[n];							// indicators for demogaphic interaction
	int<lower=0> n_demo_re;					// total number of demogaphic categories
	
	// Risk factor interaction
	real bool_rf_re;
	int rf_re[n];							// indicators for risk factor interaction
	int<lower=0> n_rf_re;					// total number of risk factor interaction categories
	
	int area_loc_id[m]; 					// id for when we change to a new area
	int area_loc_size[m]; 	    			// number of sampled individuals for each area
	
	int y[n];       						// outcome - int
	vector[n] y2;       					// outcome - real
	vector[n] w_tilde;						// sample scaled weights
	int ps_area[n];							// area indicators
	vector[n] w;							// sample weights
	vector[n] w_ss2; 						// area sample scaled weights squared
	vector[m] sum_w;						// sum of sample weights per area
	real bot_SR; 							// bottom of smoothing ratio (SR)
	
	// boolan operators
	real bool_e;
	real bool_residual_scale;
	real bool_fe; 
	
	// priors
	real prior_fe_scale;
	real prior_sigma_norm_bool;
	real prior_sigma_exp_rate;
	//real bool_REprior_t;
	real prior_re_t_bool;
}
transformed data{
	// DECLARACTIONS
		vector[m] inv_sum_w2;					// sum of sample weights per area squared
		vector[n] w2; 							// sample weights squared

	// DEFINITIONS
		w2 = square(w);
		for(i in 1:m){
			inv_sum_w2[i] = pow(sum_w[i], -2.0);
		}
}
parameters {
	// Fixed effects
	vector[q_u] b_u;					// fixed parameters
	real intercept_u;					// intercept	
	vector[n] Z_r; 						// residual scale standard normal
	// SA2 RE
	vector[m] Z_e;
	real<lower=0> sigma_e;				// area-level error
	// Risk factor RE
	vector[n_rf_re] Z_rf_re;			// risk factor standard normal
	real<lower=0> sigma_rf_re;			// risk factor error
	// Demographic RE
	vector[n_demo_re] Z_demo_re;		// demographic standard normal
	real<lower=0> sigma_demo_re;		// demogaphic error
}
transformed parameters{
	vector[n] gmma;								// individual-level invlogit(p)
	for(i in 1:n){
		gmma[i] = intercept_u + 
		bool_fe * Q_ast[i,] * b_u + 
		bool_e * Z_e[ps_area[i]] * sigma_e + 
		bool_residual_scale * Z_r[i] * residual_scale + 
		bool_rf_re * Z_rf_re[rf_re[i]] * sigma_rf_re + 
		bool_demo_re * Z_demo_re[demo_re[i]] * sigma_demo_re;
	}
}
model{
	// PRIORS
		target += normal_lpdf( b_u | 0, prior_fe_scale );
		target += student_t_lpdf( intercept_u | 3, 0, prior_fe_scale );
		target += std_normal_lpdf( Z_r );
		
		// Priors for sigmas
		if(prior_sigma_norm_bool == 1){
			target += std_normal_lpdf( sigma_e );
			target += std_normal_lpdf( sigma_rf_re );
			target += std_normal_lpdf( sigma_demo_re );
		} else {
			target += exponential_lpdf( sigma_e | prior_sigma_exp_rate );
			target += exponential_lpdf( sigma_rf_re | prior_sigma_exp_rate );
			target += exponential_lpdf( sigma_demo_re | prior_sigma_exp_rate );
		}
		
		// Risk factor and demographic RE
		if(prior_re_t_bool == 1){
			target += student_t_lpdf( Z_e | 3, 0, 1 );
			target += student_t_lpdf( Z_rf_re | 3, 0, 1 );
			target += student_t_lpdf( Z_demo_re | 3, 0, 1 );
		} else {
			target += std_normal_lpdf( Z_e );
			target += std_normal_lpdf( Z_rf_re );
			target += std_normal_lpdf( Z_demo_re );
		}
			
	// MODEL
	for(i in 1:n)
		target += w_tilde[i] * bernoulli_logit_lpmf( y[i] | gmma[i] );
}
generated quantities{
	// DECLARACTIONS
		// vector<lower=0>[m] gamma_s1; 					// sampling variance -> empirical logit
		vector[m] gamma_s1; 							// sampling variance -> empirical logit
		// vector<lower=0>[m] var_mu_s1; 					// sampling variance
		vector[m] var_mu_s1; 							// sampling variance
		// vector<lower=0,upper=1>[M] mu_s1; 				// means 
		// vector<lower=0,upper=1>[m] mu_s1; 				// means 
		vector[m] mu_s1; 								// means
		vector[m] theta_s1; 							// means -> empirical logit
		real log_lik[n];								// LogLikelihood 
		real SR;										// smoothing ratio (SR)
		vector[q_u] beta_u; 							// correct regression coefficients
		
	// DEFINITIONS		
	{ // local scope 
			vector[n] p;								// p_ij
			vector[n] wp; 								// w_ij * p_ij				
			vector[n] pmy; 								// p_ij - y_ij
			vector[n] wpmy; 							// w_ij * (p_ij - y_ij)
			vector[n] wy;								// w_ij * y_ij
			vector[m] top_SR_per_area; 					// abs value of bias per area
			vector[m] bias_D; 							// weighted mean of bias
			vector[m] mu_d; 							// direct estimates
			
			// kernel of variance calculation
				vector[m] model_element;
				vector[m] data_element;
				vector[m] bias_element;
			p = inv_logit(gmma);
			
			// weights times probabilities
			wp = w .* p;
			pmy = p - y2;
			wpmy = w .* pmy;
			wy = w .* y2;
			for(j in 1:m){
			
				// weighted means
					mu_s1[j] = sum(segment(wp, area_loc_id[j], area_loc_size[j]))/sum_w[j];
					mu_d[j] = sum(segment(wy, area_loc_id[j], area_loc_size[j]))/sum_w[j];
					bias_D[j] = sum(segment(wpmy, area_loc_id[j], area_loc_size[j]))/sum_w[j];
				
				// kernel of variance calculation
					// sum( w_ij^2 * (y - Direct)^2 )
					data_element[j] = sum( segment(w_ss2, area_loc_id[j], area_loc_size[j]) .* square(segment(y2, area_loc_id[j], area_loc_size[j]) - mu_d[j]) );
					// sum( w_ij^2 * (p - mu_s1)^2 )
					model_element[j] = sum( segment(w_ss2, area_loc_id[j], area_loc_size[j]) .* square(segment(p, area_loc_id[j], area_loc_size[j]) - mu_s1[j]) );
					// sum( w_ij^2 * ((p-y) - bias_D)^2 )
					bias_element[j] = sum( segment(w_ss2, area_loc_id[j], area_loc_size[j]) .* square(segment(pmy, area_loc_id[j], area_loc_size[j]) - bias_D[j]) );

				// sampling variance for mu_s1
				var_mu_s1[j] = FPC[j] * ( data_element[j] + bias_element[j] );
				
				// sampling variance for theta_s1
				theta_s1[j] = logit(mu_s1[j]);
				gamma_s1[j] = var_mu_s1[j] / ( square( mu_s1[j] * (1 - mu_s1[j]) ) );

				// get top of smoothing ratio (SR)
				top_SR_per_area[j] = fabs( bias_D[j] );
			}
			SR = 1 - ( sum( top_SR_per_area ) / bot_SR );
			beta_u = R_ast_inverse * b_u;						// get correct regression coefficients
			for(i in 1:n)
				log_lik[i] = bernoulli_logit_lpmf( y[i] | gmma[i] );
	}  
}






