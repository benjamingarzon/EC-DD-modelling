data {
  int<lower=1> M;                       // Number of observations
  int<lower=1> N;                       // Number of subjects
  vector<lower=0>[M] RT;                // reaction time  
  vector<lower=0>[N] minRT;             // minimum RT for each subject of the observed data 
  real RTbound;                         // lower bound or RT across all subjects (e.g., 0.1 second)
  vector[M] amount_later_centered;
  int<lower=0, upper=1> choice[M];      // 0 for instant reward, 1 for delayed reward
  int<lower=0> instance[M];             // instance number (group, subject...)
  
}

parameters {
  
  // Hyper(group)-parameters  
  vector[4] mu_p;  
  vector<lower=0>[4] sigma_p;
  
  // for slopes
  real mu_slope_p;  
  real<lower=0> sigma_slope_p;
  
  // Subject-level raw parameters (for Matt trick)
  vector[N] boundary_pr;
  vector[N] bias_pr;
  vector[N] drift_intercept_pr;
  vector[N] nondectime_pr;
  vector[N] drift_slope_pr;
}
transformed parameters {
  // Transform subject-level raw parameters
  vector<lower=0, upper=10>[M] boundary; // boundary separation
  vector<lower=0, upper=1>[M] bias;  // initial bias
  vector[M] drift; // drift rate
  vector<lower=RTbound, upper=max(minRT)>[M] nondectime; // nondecision time
  
  boundary = Phi_approx(mu_p[1] + sigma_p[1] * boundary_pr[instance])*10;   
  drift = mu_p[2] + sigma_p[2] * drift_intercept_pr[instance] + (mu_slope_p + sigma_slope_p * drift_slope_pr[instance]).* amount_later_centered;
  bias = Phi_approx( mu_p[3] + sigma_p[3] * bias_pr[instance]); 
  nondectime =  RTbound + (minRT[instance]-RTbound) .* Phi_approx( mu_p[4] + sigma_p[4] * nondectime_pr[instance]);

}

model {
  // Hyperparameters
  mu_p  ~ normal(0, 5);        
  sigma_p ~ cauchy(0, 5);
  mu_slope_p  ~ normal(0, 2);        
  sigma_slope_p ~ cauchy(0, 2);
  
  // Individual parameters for non-centered parameterization
  boundary_pr ~ normal(0, 1);
  bias_pr  ~ normal(0, 1);
  drift_intercept_pr ~ normal(0, 1);
  nondectime_pr ~ normal(0, 1);
  drift_slope_pr ~ normal(0, 1);

  // Loop across observations
  for (j in 1:M) {
    if (choice[j] > 0)  // if later choice
      RT[j] ~ wiener(boundary[j], nondectime[j], bias[j], drift[j]); 
      else 
      RT[j] ~ wiener(boundary[j], nondectime[j], 1 - bias[j], -drift[j]);
    }
}

generated quantities {
  
  vector[N] boundary_gen;
  vector[N] drift_intercept_gen;
  vector[N] drift_slope_gen;
  vector[N] bias_gen;
  vector[N] nondectime_gen;
  real log_lik[N];
  
  boundary_gen = Phi_approx(mu_p[1] + sigma_p[1] * boundary_pr)*10;
  drift_intercept_gen = mu_p[2] + sigma_p[2] * drift_intercept_pr;
  drift_slope_gen = (mu_slope_p + sigma_slope_p * drift_slope_pr);
  bias_gen = Phi_approx( mu_p[3] + sigma_p[3] * bias_pr); 
  nondectime_gen =  RTbound + (minRT-RTbound) .* Phi_approx( mu_p[4] + sigma_p[4] * nondectime_pr);
  
  // compute log-likelihood
  for (i in 1:N) log_lik[i] = 0;
  
  for (j in 1:M) {
    if (choice[j] > 0)  // if later choice
        log_lik[instance[j]]  += wiener_lpdf(RT[j]| boundary[j], nondectime[j], bias[j], drift[j]); 
      else 
        log_lik[instance[j]]  += wiener_lpdf(RT[j]| boundary[j], nondectime[j], 1 - bias[j], -drift[j]);  
    }
}
