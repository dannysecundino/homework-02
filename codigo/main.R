# =========================
# First Question
# =========================

# 0. Defining the group's probability
# students numbers
alexandre_sn <- 578345
bruno_sn <- 578342
danny_sn <- 579763
saulo_sn <- 579493

# highest student number
M <- max(c(alexandre_sn, bruno_sn, danny_sn, saulo_sn)) # 579763

# defining group's p and n
p <- 0.05 + (M %% 100)/200     # 0.365
n <- 50

# 1. Defining PMF
# support of X
support <- 0:n

# PMF of X (dbinom computes P(X = x) for a binomial distribution)
pmf <- dbinom(support, size = n, prob = p)

# sanity check: the PMF must sum to 1 over the whole support
sum_pmf <- sum(pmf)

# 2. Plotting the PMF and the CDF of X

# CDF of X (pbinom computes P(X <= x) for a binomial distribution)
cdf <- pbinom(support, size = n, prob = p)

# PMF: vertical lines with a point on top (appropriate for a discrete variable)
plot(support, pmf,
     type = "h",                    # vertical lines
     lwd = 2,
     col = "steelblue",
     main = "PMF of X ~ Bin(50, 0.365)",
     xlab = "x (number of requests requiring additional processing)",
     ylab = "P(X = x)")
points(support, pmf, pch = 19, col = "steelblue")

# CDF: step function (appropriate for a discrete variable)
plot(support, cdf,
     type = "s",                    # steps
     lwd = 2,
     col = "darkgreen",
     ylim = c(0, 1),
     main = "CDF of X ~ Bin(50, 0.365)",
     xlab = "x (number of requests requiring additional processing)",
     ylab = "P(X <= x)")
points(support, cdf, pch = 19, col = "darkgreen")

# 3. Computing the expected value, variance and standard deviation of X
# using the closed formulas of the binomial distribution
ex <- n * p                     # E(X) = n * p
vx <- n * p * (1 - p)           # Var(X) = n * p * (1 - p)
sdx <- sqrt(vx)                 # standard deviation

# checking with the definition (using the PMF)
ex_def <- sum(support * pmf)                    # E(X) = sum of x * P(X = x)
vx_def <- sum((support - ex_def)^2 * pmf)       # Var(X) = sum of (x - E(X))^2 * P(X = x)
sdx_def <- sqrt(vx_def)

# 4. Computing probabilities
# a) P(X >= 20) = 1 - P(X <= 19)
prob_a <- 1 - pbinom(19, size = n, prob = p)

# b) P(30 < X < 43) = P(31 <= X <= 42) = P(X <= 42) - P(X <= 30)
prob_b <- pbinom(42, size = n, prob = p) - pbinom(30, size = n, prob = p)

# c) P(X = 31)
prob_c <- dbinom(31, size = n, prob = p)

# checking with the PMF (sums over the support; remember that support starts at 0, so x is at position x + 1)
prob_a_check <- sum(pmf[support >= 20])
prob_b_check <- sum(pmf[support > 30 & support < 43])
prob_c_check <- pmf[support == 31]

# 5. Maximum likelihood estimation of p

# a) theory

# b) theory

# c) generating a sample of 50 i.i.d. Ber(p) observations
set.seed(M)                                   # define a seed
sample_bern <- rbinom(n, size = 1, prob = p)  # Ber(p) is a Bin(1, p)

# observed number of requests that required additional processing
k <- sum(sample_bern)

# maximum likelihood estimate (result of item b: p_hat = k / n)
p_hat <- k / n

# likelihood and log-likelihood as functions of q (candidate values of p)
lik <- function(q) dbinom(k, size = n, prob = q)
loglik <- function(q) dbinom(k, size = n, prob = q, log = TRUE)

# numerical check: maximizing the log-likelihood should give the same value as k / n
p_hat_num <- optimize(loglik, interval = c(0, 1), maximum = TRUE)$maximum

# plotting the likelihood, with p_hat and the theoretical p marked
p_grid <- seq(0.001, 0.999, length.out = 1000)
plot(p_grid, lik(p_grid),
     type = "l",
     lwd = 2,
     col = "steelblue",
     main = "Likelihood Function L(p | k)",
     xlab = "p",
     ylab = "L(p | k)")
abline(v = p_hat, col = "red", lty = 2, lwd = 2)
abline(v = p, col = "darkgreen", lty = 3, lwd = 2)
legend("topright",
       legend = c("MLE (p_hat)", "Theoretical p"),
       col = c("red", "darkgreen"),
       lty = c(2, 3),
       lwd = 2)

# d) supporting values for the comparison
abs_error <- abs(p_hat - p)                   # absolute difference between p_hat and p
se_theoretical <- sqrt(p * (1 - p) / n)       # standard error of p_hat: sqrt(p(1-p)/n)

# effect of the sample size (using the same p; each size generates a new sample)
sizes <- c(10, 50, 500, 5000, 50000)
p_hat_sizes <- sapply(sizes, function(m) mean(rbinom(m, size = 1, prob = p)))
se_sizes <- sqrt(p * (1 - p) / sizes)

# 6. Presenting our results
cat("=========================First Question=========================\n")
cat("0. Students Numbers of Group:\n")
cat("   Alexandre Grangeiro:", alexandre_sn, "\n")
cat("   Bruno Lage:", bruno_sn, "\n")
cat("   Danny Secundino:", danny_sn, "\n")
cat("   Saulo Gomes:", saulo_sn, "\n")
cat("   [Highest student number: M =", M, "]\n")
cat("   --> Group's probability: p =", p, "\n")
cat("   --> Group's (all groups use the same value) n: n =", n, "\n")
cat("\n")

cat("1. The probability distribution of X is a BINOMIAL distribution [justified on report].\n")
cat("In other words, X ~ Bin(n, p)\n")
cat("   --> Parameters: n = ", n, " p = ", p, "\n")
cat("   --> PMF: P(X = x) = C(n, x) * p^x * (1 - p)^(n - x)\n")
cat("   --> Support: x in {0, 1, ..., n}, that is, x = 0, 1, 2, ..., ", n, "\n")
cat("   --> Sum of the PMF over the support (should be 1): ", sum_pmf, "\n")
cat("\n")

cat("2. Plotting the PMF and the CDF of X:\n")
cat("   --> PMF: plotted as vertical lines (discrete variable).\n")
cat("   --> CDF: plotted as a step function (discrete variable).\n")
cat("   Both are at the generated pdf Rplots.pdf\n")
cat("\n")

cat("3. Expected value, variance and standard deviation of X:\n")
cat("a) Using the closed formulas of the binomial distribution:\n")
cat("   E(X) = n * p =", ex, "\n")
cat("   Var(X) = n * p * (1 - p) =", vx, "\n")
cat("   Standard deviation =", sdx, "\n")
cat("b) Checking with the definition (sums over the support):\n")
cat("   E(X) =", ex_def, "\n")
cat("   Var(X) =", vx_def, "\n")
cat("   Standard deviation =", sdx_def, "\n")
cat("\n")

cat("4. Computing probabilities:\n")
cat("a) P(X >= 20) = 1 - P(X <= 19) =", prob_a, "\n")
cat("b) P(30 < X < 43) = P(31 <= X <= 42) = P(X <= 42) - P(X <= 30) =", prob_b, "\n")
cat("c) P(X = 31) =", prob_c, "\n")
cat("[Same probabilities found by summing the PMF: a) ", prob_a_check, " b) ", prob_b_check, " c) ", prob_c_check, "]\n")
cat("\n")

cat("5. Maximum likelihood estimation of p:\n")
cat("a) Likelihood function: L(p | k) = C(n, k) * p^k * (1 - p)^(n - k) [derived on report].\n")
cat("b) Log-likelihood and MLE:\n")
cat("   Log-likelihood: l(p) = ln L(p | k) = ln C(n, k) + k * ln(p) + (n - k) * ln(1 - p)\n")
cat("   Derivative: l'(p) = k / p - (n - k) / (1 - p) = 0\n")
cat("   MLE: p_hat = k / n [full derivation on report].\n")
cat("c) Sample of", n, "i.i.d. Bernoulli observations with theoretical p =", p, ":\n")
cat("   Observed number of requests with additional processing: k =", k, "\n")
cat("   Maximum likelihood estimate: p_hat = k / n =", p_hat, "\n")
cat("   [Numerical check by optimizing the log-likelihood:", p_hat_num, "]\n")
cat("   (The plot of the likelihood is at the generated pdf Rplots.pdf)\n")
cat("d) Comparison with the theoretical probability:\n")
cat("   Theoretical p =", p, "\n")
cat("   Estimate p_hat =", p_hat, "\n")
cat("   Absolute difference =", abs_error, "\n")
cat("   Standard error of p_hat (for n = 50) =", se_theoretical, "\n")
cat("   Effect of the sample size:\n")
cat("       n:", sizes, "\n")
cat("       p_hat:", p_hat_sizes, "\n")
cat("       Standard error:", se_sizes, "\n")
cat("   The explanation is on the report.\n")
cat("\n")

cat("\n\n")

# =========================
# Second Question
# =========================

# 1. Computing N and p_alert (using the results of the first question)
# m = E[X] and c = SD(X) [we call it c_sd to not overwrite the function c() of R]
m <- ex
c_sd <- sdx

# number of requests per day and probability of a critical alert
N <- round(m * 10^6)
p_alert <- c_sd / N

# Y ~ Bin(N, p_alert); Poisson approximation with lambda = N * p_alert
lambda <- N * p_alert                  # equals c_sd

# PMF of the Poisson approximation (dpois computes P(Y = y) for a Poisson distribution)
y_vals <- 0:20                         # first values of the support (the Poisson support is 0, 1, 2, ...)
pmf_pois <- dpois(y_vals, lambda = lambda)

# checking the suitability of the approximation
# exact binomial PMF for the same values
pmf_binom_y <- dbinom(y_vals, size = N, prob = p_alert)

# largest absolute difference between the exact PMF and the approximation
max_abs_diff <- max(abs(pmf_binom_y - pmf_pois))

# variances of both distributions
var_binom_y <- N * p_alert * (1 - p_alert)
var_pois <- lambda

# 2. Computing E[Y] and Var(Y) with the exact distribution and with the Poisson approximation

# exact distribution: Y ~ Bin(N, p_alert)
ey_exact <- N * p_alert                       # E[Y] = N * p_alert
vy_exact <- N * p_alert * (1 - p_alert)       # Var(Y) = N * p_alert * (1 - p_alert)

# Poisson approximation: Y ~ Pois(lambda)
ey_pois <- lambda                             # E[Y] = lambda
vy_pois <- lambda                             # Var(Y) = lambda

# comparing the results
diff_e <- ey_pois - ey_exact                  # difference between the expected values
diff_v <- vy_pois - vy_exact                  # difference between the variances
rel_diff_v <- diff_v / vy_exact               # relative difference between the variances

# checking with the definition (sums over y = 0, ..., 100, since the probability beyond that is negligible)
y_check <- 0:100
ey_exact_check <- sum(y_check * dbinom(y_check, size = N, prob = p_alert))
vy_exact_check <- sum((y_check - ey_exact_check)^2 * dbinom(y_check, size = N, prob = p_alert))
ey_pois_check <- sum(y_check * dpois(y_check, lambda = lambda))
vy_pois_check <- sum((y_check - ey_pois_check)^2 * dpois(y_check, lambda = lambda))

# 3. Simulating 10,000 days of operation from the exact Binomial distribution

n_days <- 10000
set.seed(M)                                          # reproducible simulation (same seed as the first question)
y_sim <- rbinom(n_days, size = N, prob = p_alert)    # daily number of critical alerts

# relative frequencies of each value observed (levels from 0 to the maximum observed, so zeros are kept)
y_max <- max(y_sim)
y_range <- 0:y_max
freq_rel <- as.numeric(table(factor(y_sim, levels = y_range))) / n_days

# Poisson PMF for the same values
pmf_pois_sim <- dpois(y_range, lambda = lambda)

# plotting the observed relative frequencies (bars) and the Poisson PMF (points and line)
bp <- barplot(freq_rel,
              names.arg = y_range,
              col = "steelblue",
              ylim = c(0, max(freq_rel, pmf_pois_sim) * 1.2),
              main = "Simulated Daily Alerts vs Poisson PMF\n(10,000 days)",
              xlab = "y (number of critical alerts per day)",
              ylab = "Relative frequency / P(Y = y)")
points(bp, pmf_pois_sim, pch = 19, col = "red")
lines(bp, pmf_pois_sim, col = "red", lwd = 2)
legend("topright",
       legend = c("Observed relative frequency", "Poisson PMF"),
       col = c("steelblue", "red"),
       pch = c(15, 19),
       lwd = c(NA, 2))

# assessing the quality of the approximation
max_abs_diff_sim <- max(abs(freq_rel - pmf_pois_sim))     # largest difference between frequency and PMF
mean_sim <- mean(y_sim)                                   # sample mean (compare with lambda)
var_sim <- var(y_sim)                                     # sample variance (compare with lambda)

# expected size of the simulation noise: standard error of a relative frequency at the most probable value
pmf_max <- max(pmf_pois_sim)
se_freq <- sqrt(pmf_max * (1 - pmf_max) / n_days)

# 4. Presenting our results
cat("=========================Second Question=========================\n")
cat("1. Calculating N, p_alert and the Poisson approximation of Y:\n")
cat("   m = E[X] =", m, "\n")
cat("   c = SD(X) =", c_sd, "\n")
cat("   N = round(m * 10^6) =", N, "\n")
cat("   p_alert = c / N =", p_alert, "\n")
cat("   Y ~ Bin(N, p_alert), then Y is approximated by a Poisson distribution:\n")
cat("   --> Y ~ Pois(lambda), lambda = N * p_alert =", lambda, "\n")
cat("   --> PMF: P(Y = y) = exp(-lambda) * lambda^y / y!\n")
cat("   --> Support: y in {0, 1, 2, ...}\n")
cat("   Suitability of the approximation:\n")
cat("   --> N is large:", N, "\n")
cat("   --> p_alert is small:", p_alert, "\n")
cat("   --> lambda is moderate:", lambda, "\n")
cat("   --> Largest absolute difference between the PMFs (y = 0, ..., 20):", max_abs_diff, "\n")
cat("   --> Variance of the binomial:", var_binom_y, " Variance of the Poisson:", var_pois, "\n")
cat("   The discussion is on the report.\n")
cat("\n")

cat("2. Calculating E[Y] and Var(Y):\n")
cat("a) Using the exact distribution, Y ~ Bin(N, p_alert):\n")
cat("   E[Y] = N * p_alert =", ey_exact, "\n")
cat("   Var(Y) = N * p_alert * (1 - p_alert) =", vy_exact, "\n")
cat("b) Using the Poisson approximation, Y ~ Pois(lambda):\n")
cat("   E[Y] = lambda =", ey_pois, "\n")
cat("   Var(Y) = lambda =", vy_pois, "\n")
cat("c) Comparing the results:\n")
cat("   Difference between the expected values =", diff_e, "\n")
cat("   Difference between the variances =", diff_v, "\n")
cat("   Relative difference between the variances =", rel_diff_v, "\n")
cat("   [Checking with the definition (sums over y = 0, ..., 100):\n")
cat("    exact: E[Y] =", ey_exact_check, " Var(Y) =", vy_exact_check, "\n")
cat("    Poisson: E[Y] =", ey_pois_check, " Var(Y) =", vy_pois_check, "]\n")
cat("   The discussion is on the report.\n")
cat("\n")

cat("3. Simulating", n_days, "days of operation from the exact Binomial distribution:\n")
cat("   Values observed: from 0 to", y_max, "alerts per day\n")
cat("   (The plot comparing relative frequencies and the Poisson PMF is at the generated pdf Rplots.pdf)\n")
cat("   Sample mean =", mean_sim, " [lambda =", lambda, "]\n")
cat("   Sample variance =", var_sim, " [lambda =", lambda, "]\n")
cat("   Largest difference between relative frequency and Poisson PMF =", max_abs_diff_sim, "\n")
cat("   Standard error of a relative frequency at the most probable value =", se_freq, "\n")
cat("   The assessment is on the report.\n")
cat("\n")

cat("\n\n")

# =========================
# Third Question
# =========================

# 0. Defining the parameters of the question
# m = E[X] was calculated in the first question
nT <- round(100 * m)                   # equation (2): number of CPU temperatures to simulate

# parameters of the normal distribution of the CPU temperature
mu <- 62
sigma <- 3.5

# 1. Implementing the Box-Muller transformation
# function that returns a vector with n values of a normal distribution
# [with the default arguments (mu = 0, sigma = 1) the values are standard normal]
box_muller <- function(n, mu = 0, sigma = 1){
    n_pairs <- ceiling(n / 2)          # each pair (U1, U2) generates two normal values

    # a) two independent uniform random variables, U1, U2 ~ Unif(0, 1)
    # [runif never returns exactly 0 or 1, so log(U1) is always defined]
    u1 <- runif(n_pairs)
    u2 <- runif(n_pairs)

    # b) two independent standard normal values per pair
    r <- sqrt(-2 * log(u1))
    z1 <- r * cos(2 * pi * u2)
    z2 <- r * sin(2 * pi * u2)

    # combining them in one vector and discarding the extra value, if necessary
    z <- c(z1, z2)
    z <- z[1:n]

    # c) converting the standard normal values: T = mu + sigma * Z
    return(mu + sigma * z)
}

# 2. Generating the two sets of CPU temperatures
# the seed is defined only once, before generating both datasets
set.seed(M)

# first dataset: using our function (Box-Muller)
temp_bm <- box_muller(nT, mu, sigma)

# second dataset: using the built-in normal random number generator of R
temp_r <- rnorm(nT, mean = mu, sd = sigma)

# checking the datasets (length, sample mean and sample standard deviation)
len_bm <- length(temp_bm)              # should be equal to nT
len_r <- length(temp_r)                # should be equal to nT
mean_bm <- mean(temp_bm)               # should be close to mu
sd_bm <- sd(temp_bm)                   # should be close to sigma
mean_r <- mean(temp_r)                 # should be close to mu
sd_r <- sd(temp_r)                     # should be close to sigma

# 3. (to be done)

# 4. (to be done)

# X. Presenting our results
cat("=========================Third Question==========================\n")
cat("0. Parameters of the question:\n")
cat("   m = E[X] =", m, "\n")
cat("   nT = round(100 * m) =", nT, "\n")
cat("   T ~ N(mu, sigma), mu =", mu, " sigma =", sigma, "\n")
cat("\n")

cat("1. Implementing the Box-Muller transformation:\n")
cat("   The function box_muller(n, mu, sigma) returns a vector with n values of a normal distribution.\n")
cat("a) U1, U2 ~ Unif(0, 1) generated with runif.\n")
cat("b) Z1 = sqrt(-2 * ln(U1)) * cos(2 * pi * U2) and Z2 = sqrt(-2 * ln(U1)) * sin(2 * pi * U2).\n")
cat("   Each pair (U1, U2) generates two values, so ceiling(n / 2) pairs are used and the extra value is discarded when n is odd.\n")
cat("   For nT =", nT, ": pairs =", ceiling(nT / 2), "| values generated =", 2 * ceiling(nT / 2), "| extra values discarded =", 2 * ceiling(nT / 2) - nT, "\n")
cat("c) Conversion inside the function: T = mu + sigma * Z (with mu = 0 and sigma = 1 it returns the standard normal values).\n")
cat("\n")

cat("2. Generating the two sets of CPU temperatures:\n")
cat("   The seed was defined only once (set.seed(M)), and the datasets were generated sequentially.\n")
cat("a) Dataset 1 (function box_muller):\n")
cat("   Length =", len_bm, "[nT =", nT, "]\n")
cat("   Sample mean =", mean_bm, " Sample standard deviation =", sd_bm, "\n")
cat("b) Dataset 2 (built-in generator rnorm):\n")
cat("   Length =", len_r, "[nT =", nT, "]\n")
cat("   Sample mean =", mean_r, " Sample standard deviation =", sd_r, "\n")
cat("   [Theoretical values: mu =", mu, " sigma =", sigma, "]\n")
cat("\n")

cat("\n\n")