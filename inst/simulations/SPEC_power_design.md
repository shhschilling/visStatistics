# Power design — read this before touching any power simulation or figure

Source: Brunner, Konietschke, Pauly & Puri (2017), *JRSS-B* **79**(5), 1463–1485,
power study on p. 1480, results in Table 5, p. 1479.

## The design

| | |
|---|---|
| distributions | \(X_{ik}\sim N(\mu_i,1)\) — **variance 1 in every group** |
| balance | **balanced only**, \(n_i\equiv n\) |
| sample size | **fixed**; he uses \(n\in\{15,20\}\) |
| x-axis | **\(\delta\)**, swept 0, 0.1, …, 1.6 |
| alternative (a) | \(\mu=(0,0,0,\delta)\) — one-point |
| alternative (b) | \(\mu=(\delta/4,\delta/2,3\delta/4,\delta)\) — increasing trend |

Our only departure: the five Fleishman panels in place of his normal,
double-exponential and log-normal.

## The three mistakes that keep being made

1. **δ is not a knob to choose.** It is the horizontal axis. There is no
   "which δ" question. Any answer that recommends a value of δ is wrong.
2. **The power study is homoscedastic.** The scaling vectors
   \((1,\sqrt2,2,\sqrt5)\) and \((\sqrt5,2,\sqrt2,1)\) belong to the **Type I**
   study, Table 2, p. 1477. They must not appear in a power design.
3. **The power study is balanced.** No unbalanced designs, no pairing contrast.
   Those are Type I as well.

## Why power is homoscedastic — the reason, not just the rule

Under heteroscedasticity a single \(\delta\) buys a different effect size in
every design: at \(\delta=1\) the one-point alternative gives
\(\omega^2 = 0.043\) balanced heteroscedastic, 0.057 positive pairing and 0.123
negative pairing. A power difference between those rows therefore measures the
effect size, not the test, and the mean shift is confounded with the variance
assignment. There is no comparison to be made and no effect to find, which is
why the heteroscedastic case is a **Type I** question — where all means are
equal and the variance structure is the only thing that varies — and never a
power question.

## Type I design, for contrast

Table 2, p. 1477: sample sizes \(n_1=(5,5,5,5)\) and \(n_2=(10,20,30,40)\) with
a constant \(m\in\{5,10,20,25\}\) added to every component; scaling vectors
\(\sigma\in\{(1,1,1,1),\,(1,\sqrt2,2,\sqrt5),\,(\sqrt5,2,\sqrt2,1)\}\). All
group means equal. This is where balance and heteroscedasticity are varied.
