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


# X. Presenting our results
cat("=========================Second Question=========================\n")
