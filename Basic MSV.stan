//Basic MSV

data {
  int<lower=0> N;  // Number of time points
  vector[N] y1;  // Observations (mean corrected response at time t) for time series 1
  vector[N] y2;  // Observations (mean corrected response at time t) for time series 2
}

parameters {
  real mu1; // Mean of log volatility of y1
  real mu2; // Mean of log volatility of y2
  real<lower=-1,upper=1> phi1; // Persistence of volatility of y1
  real<lower=-1,upper=1> phi2; // Persistence of volatility of y2
  real<lower=0> sigma1;  // Standard deviation of y1 (white noise)
  real<lower=0> sigma2;  // Standard deviation of y2 (white noise)
  vector[N] h1; //latent volatility vector for time series 1
  vector[N] h2; //latent volatility vector for time series 2
}

model {
  // Priors for mean parameters
  mu1 ~ normal(0, 25);
  mu2 ~ normal(0, 25);
  
  // Priors for standard deviation parameters
  sigma1 ~ normal(0,25);
  sigma2 ~ normal(0,25);
  
  // Priors for persistence parameters
  phi1 ~ uniform(-1,1); 
  phi2 ~ uniform(-1,1); 
  
  // Dynamics of the latent volatility
  h1[1] ~ normal(mu1, sigma1 / sqrt(1 - square(phi1)));
  for (t in 2:N) {
    h1[t] ~ normal(mu1 + phi1 * (h1[t - 1] - mu1), sigma1);
  }
  
  h2[1] ~ normal(mu2, sigma2 / sqrt(1 - square(phi2)));
  for (t in 2:N) {
    h2[t] ~ normal(mu2 + phi2 * (h2[t - 1] - mu2), sigma2);
  }
  
  for (t in 1:N) {
    //Dynamics for y1
    y1[t] ~ normal(0, exp(h1[t] / 2));
  }
  
  for (t in 1:N) {
    // Dynamics for y2 given y1, using the properties of the Bivariate Normal Distribution
    y2[t] ~ normal(mu2, exp(h2[t] / 2));
  }

}

generated quantities {
  vector[N] y_rep1;
  
  for (t in 1:N){
    y_rep1[t] = normal_rng(0, exp(h1[t]/2));
  }
  
  vector[N] y_rep2;
  
  for (t in 1:N){
    y_rep2[t] = normal_rng(mu2, exp(h2[t]/2));
  }
  
}
