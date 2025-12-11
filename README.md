# fickkk — A Lightweight R Package for 1D Diffusion Modeling (Fick’s Laws)

`fickkk` is an educational and computational R package designed to simulate, analyze, and visualize diffusion processes based on **Fick’s First and Second Laws**.  
It provides simple but powerful tools for modeling concentration gradients, diffusion flux, temporal evolution, and estimating diffusion coefficients in biomedical or physical contexts.

---

##  Features

- Compute diffusion flux using **Fick’s First Law**
- Simulate time evolution of concentration profiles (FTCS explicit scheme)
- Check numerical stability conditions
- Generate linear concentration profiles
- Run full diffusion scenarios with a single function
- Estimate the diffusion coefficient from experimental/simulated data
- Plot concentration curves and diffusion dynamics

---

##  Installation

Since the package is not yet on CRAN, install it from GitHub:

```r
remotes::install_github("your-username/fickkk")
