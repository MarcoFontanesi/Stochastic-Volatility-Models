// Multiplicative factor-MSV

data {
  int<lower=0> N;  // Number of time points
  vector[N] y1;  // Observations (mean corrected response at time t) of time series 1
  vector[N] y2;  // Observations (mean corrected response at time t) of time series 2
}

parameters {
  real mu; // mean
  real<lower=0> sigma; // variance
  real<lower=-1,upper=1> phi; // persistence of volatility 
  vector[N] h; // latent volatility factor
  real<lower=-1,upper=1> rho; // correlation parameter
  real<lower=0> beta; // noise is the observations 
}

model {
  
  // Priors for mean parameter
  mu ~ normal(0, 25);
  
  // Priors for standard deviation parameter
  sigma ~ normal(0,25);
  
  // Priors for persistence parameter
  phi ~ uniform(-1,1); 
  
  // Prior correlation coefficient
  rho ~ uniform(-1,1);
  
  // Prior Noise in observations
  beta ~ normal(0,25);
  
  // Dynamics of the latent volatility
  h[1] ~ normal(mu, sigma / sqrt(1 - square(phi)));
  for (t in 2:N) {
    h[t] ~ normal(mu + phi * (h[t - 1] - mu), sigma);
  }
  
  for (t in 1:N) {
    //Dynamics for y1
    y1[t] ~ normal(0, exp(h[t]/2));
  }
  
    //Dynamics for y2
  for (t in 1:N){
    y2[t] ~ normal(mu + rho*beta*y1[t],  sqrt((1 - square(rho)))*exp(h[t]/2)*beta);
  }

}

generated quantities {
  vector[N] y_rep1;
  
  for (t in 1:N){
    y_rep1[t] = normal_rng(0, exp(h[t]/2));
  }
  
  vector[N] y_rep2;
  
  for (t in 1:N){
    y_rep2[t] = normal_rng(mu + rho*beta*y1[t],  sqrt((1 - square(rho)))*exp(h[t]/2)*beta);
  }
  
}
