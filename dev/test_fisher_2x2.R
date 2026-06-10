devtools::load_all(".")

# 2x2 Beispiel: Teetassen-Experiment (Fisher 1935)
# Kleine Zellhäufigkeiten -> Cochran verletzt -> exakter Fisher -> OR im Output
tea <- data.frame(
  poured  = as.factor(c(rep("milk_first", 8), rep("tea_first", 8))),
  correct = as.factor(c(rep("yes", 7), rep("no", 1), rep("yes", 1), rep("no", 7)))
)

result <- visstat(tea$poured, tea$correct)

cat("Method:  ", result$method, "\n")
cat("p-value: ", result$p.value, "\n")
cat("OR:      ", result$estimate, "\n")
cat("95% CI:  ", result$conf.int, "\n")

# Erwartet: OR >> 1 (milk_first deutlich haeufiger korrekt erkannt)
# Erwartet: exakter Fisher, KEIN simulate.p.value
stopifnot(grepl("Fisher", result$method))
stopifnot(!is.null(result$estimate))   # OR vorhanden
stopifnot(!is.null(result$conf.int))   # CI vorhanden
cat("\nAlle Tests bestanden.\n")
