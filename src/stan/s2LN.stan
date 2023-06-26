functions {
real icar_normal_lpdf(vector phi, int N, int[] node1, int[] node2) {
  return -0.5 * dot_self(phi[node1] - phi[node2]) 
  + normal_lpdf(sum(phi) | 0, 0.001 * N);
  }
}
data {
	int<lower=0> m; 						// number of sampled areas
	int<lower=0> m_s;						// number of stable areas
	int<lower=0> M; 						// total number of areas
	int<lower=0> its;						// number of draws
	real<lower=0> me_w;						// 1/its as numeric
	int id_s[m_s]; 							// indexes for the stable areas
	int<lower=0> id_us[m-m_s]; 				// indexes for unstable areas
	vector<lower=0>[M] N_frac; 				// N_persons divided by total population
	
	int<lower=0> q_a; 						// number of parameters
	matrix[M, q_a] Q_ast; 					// covariates 
	
	// SHA estimates
	real bool_sha;
	vector[M] sha_est;
	vector[M] sha_se; 
	
	// PC
	int<lower=0> q_pc;
	array[M] row_vector[q_pc] Q_ast_pc;		// 
	int ps_ra[M];
	
	// GVF
	int<lower=0> q_gvf; 
	matrix[m, q_gvf] Q_ast_gvf; 			// design matrix for gamma model
	
	real<lower=0> rho_prior_beta_alpha;	    // alpha parameter for Beta prior on rho_sa2
	real<lower=0> rho_prior_beta_beta;		// beta parameter for Beta prior on rho_sa2
	
	// from LOGISTIC MODEL
	matrix[its, m] theta_s1_its;			// posterior draws of theta_o
	vector<lower=0>[m] theta_s1_sd;			// posterior sd of theta_o
	vector<lower=0>[m] sqrt_gamma_input;	// posterior mean of sqrt(gamma_s1)
	
	// for Bayesian Benchmarking
	real eps; 								// 0<eps<1: discrepancy parameter
	// majorstatebench
	int<lower=0> msb_K; 						// number of benchmarks
	matrix[M, msb_K] msb_multiplier; 			// indicators for benchmark assignments * sa2 populations
	vector[msb_K] msb_C_hat_D;					// benchmark estimates
	vector[msb_K] msb_v_C_hat_D; 				// sampling variance for benchmark direct estimates
	// StateBench
	int<lower=0> sb_K; 						// number of benchmarks
	matrix[M, sb_K] sb_multiplier; 			// indicators for benchmark assignments * sa2 populations
	vector[sb_K] sb_C_hat_D;				// benchmark estimates
	vector[sb_K] sb_v_C_hat_D; 				// sampling variance for benchmark direct estimates
	
	
	// SA2 - spatial components
	int<lower=0> rho_sa2_fixed;
	int<lower=0> nu_edges_sa2;
	int<lower=1, upper=M> node1_sa2[nu_edges_sa2]; 
	int<lower=1, upper=M> node2_sa2[nu_edges_sa2];
	real scale_factor_sa2;
	
	// SA3 - spatial components
	int<lower=0> rho_sa3_fixed;
	int ps_sa3[M];							
	int M_sa3; 								
	int<lower=0> nu_edges_sa3;
	int<lower=1, upper=M> node1_sa3[nu_edges_sa3]; 
	int<lower=1, upper=M> node2_sa3[nu_edges_sa3];
	real scale_factor_sa3;
	
	// boolan operators
	real bool_sa2; 			// zero omits sa2 random effect
	real bool_sa3; 			// zero omits sa3 random effect
	real bool_fe; 			// zero omits fixed effects
	real bool_pc;  			// zero omits remoteness specific fixed effects
	real benchmark;  		// zero does not benchmark, 1 uses majorstatebench, 2 uses statebench
	real consider_s1unc; 	// zero does not use s1 uncertainty 
	
	// priors
	real prior_fe_scale;
	real prior_sigma_norm_bool; // zero uses an exponential prior for all sigmas
	real prior_sigma_exp_rate;
}
parameters {
	// NORMAL MODEL
	vector[q_a] l_a;													// fixed parameters
	real B_sha;															// coefficient for SHA estimate
	vector[M] sha_mean;													// vector of 'true' SHA values
	real intercept_a;													// intercept
	vector[m] bar_theta_s1;												// column means of theta_s1_its

	// SA2 - random effect
	vector[M] Z_v_sa2;													// unstructured error
	vector[rho_sa2_fixed == 0 ? 0 : M] Z_s_sa2;							// CAR error
	real<lower=0> sigma_sa2;											// standard deviation of error
	real<lower=0,upper=1> rho_sa2;										// rho

	// SA3 - random effect
	vector[M_sa3] Z_v_sa3;												// unstructured error
	vector[rho_sa3_fixed == 0 ? 0 : M_sa3] Z_s_sa3;						// CAR error
	real<lower=0> sigma_sa3;											// standard deviation of error
	real<lower=0,upper=1> rho_sa3;										// rho
	
	// gamma MODEL
	vector[q_gvf] omega; 												// fixed parameters
	real omega0; 														// fixed parameters
	real<lower=0> sigma_gvf; 											// residual error
	
	// PC
	array[3] vector[q_pc] l_a_pcfe;										// random effects for PC

}
transformed parameters{
	vector[msb_K] msb_C_tilde;
	vector[sb_K] sb_C_tilde;
	vector[M] nu;
	vector[M] bym2_sa2;
	vector[M_sa3] bym2_sa3;
	
	// SA2 - BYM2
	if(rho_sa2_fixed == 1)
		bym2_sa2 = sigma_sa2 * Z_s_sa2;
	else if(rho_sa2_fixed == 0)
		bym2_sa2 = sigma_sa2 * Z_v_sa2;
	else
		bym2_sa2 = sigma_sa2 * ( sqrt(rho_sa2 / scale_factor_sa2) * Z_s_sa2 + sqrt(1-rho_sa2) * Z_v_sa2 );
	
	// SA3 - BYM2
	if(rho_sa3_fixed == 1)
		bym2_sa3 = sigma_sa3 * Z_s_sa3;
	else if(rho_sa3_fixed == 0)
		bym2_sa3 = sigma_sa3 * Z_v_sa3;
	else 
		bym2_sa3 = sigma_sa3 * ( sqrt(rho_sa3 / scale_factor_sa3) * Z_s_sa3 + sqrt(1-rho_sa3) * Z_v_sa3 );

	// Linear predictor
	for(i in 1:M){
		nu[i] = intercept_a + 
		bool_fe * Q_ast[i,] * l_a + 
		bool_pc * Q_ast_pc[i,] * l_a_pcfe[ps_ra[i]] +
		bool_sha * B_sha * sha_mean[i] + 
		bool_sa2 * bym2_sa2[i] + 
		bool_sa3 * bym2_sa3[ps_sa3[i]];
	}
	vector[M] mu = inv_logit(nu);
	
	// GVF for sqrt_gamma
		vector[m] sqrt_gamma_mu = omega0 + Q_ast_gvf * omega;
		vector[m] sqrt_gamma = sqrt_gamma_input;
		sqrt_gamma[id_us] = exp( sqrt_gamma_mu[id_us] + 0.5 * square(sigma_gvf) );
		
	// Bayesian Benchmarking
	for(k in 1:msb_K){
		msb_C_tilde[k] = (sum( msb_multiplier[,k] .* mu )) / (sum( msb_multiplier[,k] ));
	}
	for(k in 1:sb_K){
		sb_C_tilde[k] = (sum( sb_multiplier[,k] .* mu )) / (sum( sb_multiplier[,k] ));
	}
}
model{
	// PRIORS
		target += normal_lpdf( l_a | 0, prior_fe_scale );
		target += normal_lpdf( B_sha | 0, prior_fe_scale );
		target += normal_lpdf( sha_mean | 0, 2 );
		target += student_t_lpdf(intercept_a | 3, 0, prior_fe_scale );
		// SA2 - random effects
			if(rho_sa2_fixed != 0) target += icar_normal_lpdf( Z_s_sa2 | M, node1_sa2, node2_sa2);
			target += std_normal_lpdf( Z_v_sa2 );
			target += beta_lpdf( rho_sa2 | rho_prior_beta_alpha, rho_prior_beta_beta);
		// SA3 - random effects
			if(rho_sa3_fixed != 0) target += icar_normal_lpdf( Z_s_sa3 | M_sa3, node1_sa3, node2_sa3);
			target += std_normal_lpdf( Z_v_sa3 );
			target += beta_lpdf( rho_sa3 | rho_prior_beta_alpha, rho_prior_beta_beta);
		// Remoteness specific coefficients on PCs
			for(l in 1:3){
				target += normal_lpdf( l_a_pcfe[l] | 0, prior_fe_scale );
			}
		// Sigmas
		if(prior_sigma_norm_bool == 1){
			target += normal_lpdf( sigma_sa3 | 0, 2 );
			target += normal_lpdf( sigma_sa2 | 0, 2 );
			target += normal_lpdf( sigma_gvf | 0, 2 );
		} else {
			target += exponential_lpdf( sigma_sa3 | prior_sigma_exp_rate );
			target += exponential_lpdf( sigma_sa2 | prior_sigma_exp_rate );
			target += exponential_lpdf( sigma_gvf | prior_sigma_exp_rate );
		}

	// GVFs
		target += student_t_lpdf(omega0 | 3, 0, prior_fe_scale );
		target += normal_lpdf( omega | 0, prior_fe_scale );
		target += normal_lpdf( log(sqrt_gamma[id_s]) | sqrt_gamma_mu[id_s], sigma_gvf );
		
	// SHA PHN
		target += normal_lpdf( sha_est | sha_mean, sha_se );
			
	// MODEL
	for(i in 1:m){
		if(consider_s1unc == 1){
			target += me_w * normal_lpdf( theta_s1_its[,i] | bar_theta_s1[i], theta_s1_sd[i] );
			target += normal_lpdf( bar_theta_s1[i] | nu[i], sqrt_gamma[i] );
		}else{
			target += std_normal_lpdf( bar_theta_s1[i] );
			target += normal_lpdf( mean(theta_s1_its[,i]) | nu[i], sqrt_gamma[i] );
		}
	}
	
	// Bayesian Benchmarking
		// majorstatebench: value msb_K is the NT or very remote benchmark which should not be included	
		if(benchmark == 1){
			target += normal_lpdf( msb_C_tilde | msb_C_hat_D, eps * sqrt(msb_v_C_hat_D) );
		}
		// statebench: value sb_K is the NT or very remote benchmark which should not be included
		if(benchmark == 2){
			target += normal_lpdf( sb_C_tilde | sb_C_hat_D, eps * sqrt(sb_v_C_hat_D) );
		}
		// Both benchmarks
		if(benchmark == 3){
			target += normal_lpdf( msb_C_tilde | msb_C_hat_D, eps * sqrt(msb_v_C_hat_D) );
			target += normal_lpdf( sb_C_tilde | sb_C_hat_D, eps * sqrt(sb_v_C_hat_D) );
		}
}
generated quantities{
	vector[m] il_bar_theta_s1 = inv_logit(bar_theta_s1);
	real national_prevalence = sum( mu .* N_frac);
	// benchmark discrepancy
	vector[msb_K] msb_discrepancy = fabs(msb_C_tilde - msb_C_hat_D) ./ msb_C_hat_D; 	// Eq 40 in Fully Bayesian Benchmarking
	vector[sb_K] sb_discrepancy = fabs(sb_C_tilde - sb_C_hat_D) ./ sb_C_hat_D; 			// Eq 40 in Fully Bayesian Benchmarking	 
}






