---
  output: rmarkdown::html_document
#csl: apa.csl  # optional, if you want citation styling
editor_options: 
  markdown: 
  wrap: 72
---
  
  ## Introduction
  
  The hypergeometric distribution models the probability of obtaining \( k \) successes in \( n \) draws, **without replacement**, from a population of size \( N \) containing \( m \) successes.

This is exactly what happens in lottery games like **6/49 lotto**.

---
  
  ## Lotto as a Hypergeometric Problem
  
  In a 6/49 lotto game:
  
  - There are \( N = 49 \) total numbers,
- You choose \( m = 6 \) of them (your numbers),
- The lottery also draws \( n = 6 \) numbers,
- Let \( X \) be the number of your numbers that match the drawn ones.

The number of matches \( X \) follows a **hypergeometric distribution**.

---
  
  ## Probability Mass Function
  
  The hypergeometric PMF is:
  
  $$
  P(X = k) = \frac{\binom{m}{k} \binom{N - m}{n - k}}{\binom{N}{n}}
$$
  
  For the 6/49 lotto case:
  
  $$
  P(X = k) = \frac{\binom{6}{k} \binom{43}{6 - k}}{\binom{49}{6}}, \quad k = 0, 1, \dots, 6
$$
  
  ---
  
  ## R Code: Visualising the Distribution
  
  ```{r, echo=TRUE, message=FALSE}
# Parameters
N <- 49  # Total numbers
m <- 6   # Your numbers
n <- 6   # Lotto draws
k <- 0:6 # Possible number of matches

# Hypergeometric PMF
probs <- dhyper(k, m, N - m, n)

# Plot
barplot(probs,
        names.arg = k,
        col = "lightblue",
        ylim = c(0, max(probs) * 1.1),
        ylab = "Probability",
        xlab = "Number of Matches",
        main = "Hypergeometric Distribution: Lotto 6/49")