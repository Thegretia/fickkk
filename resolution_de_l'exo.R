library(fickkk)   # Ton package local contenant fick_flux()

# Données
C1 <- 3e-4       # mol/L
C2 <- 7e-5       # mol/L
S_mm2 <- 600     # mm^2
D_cm2_s <- 6.9e-7
dx_cm <- 2       # cm
dt_min <- 5
M_hemo <- 68000  # g/mol

# Conversion des unités
C1_mol_cm3 <- C1 / 1000      # mol/cm^3
C2_mol_cm3 <- C2 / 1000      # mol/cm^3
S_cm2 <- S_mm2 / 100         # mm^2 → cm^2
dt_s <- dt_min * 60          # min → s

# Calcul du gradient de concentration
dC <- C2_mol_cm3 - C1_mol_cm3

# Calcul du flux diffusif (mol/cm2/s)
J <- ficks_law_flux(D = D_cm2_s, dC = dC, dx = dx_cm)

# Quantité totale diffusée pendant Δt (mol)
n_mol <- J * S_cm2 * dt_s

# Masse transportée (g)
m_g <- n_mol * M_hemo

# Conversion en microgrammes
m_ug <- m_g * 1e6

cat("Masse d'hémoglobine transférée :", m_ug, "µg\n")

library(ggplot2)

# Données de l'exercice
C1 <- 3e-4      # mol/L
C2 <- 7e-5      # mol/L
dx_cm <- 2      # cm

# Conversion en mol/cm3
C1_mol_cm3 <- C1 / 1000
C2_mol_cm3 <- C2 / 1000

# Axe spatial (distance)
x <- seq(0, dx_cm, by = 0.1)

# Concentration linéaire selon Fick (approximation)
C <- C1_mol_cm3 + (C2_mol_cm3 - C1_mol_cm3) * (x / dx_cm)

# Dataframe pour ggplot
df <- data.frame(x = x, C = C)

# Graphique
ggplot(df, aes(x = x, y = C)) +
  geom_line(size = 1.5) +
  geom_point() +
  labs(
    title = "Gradient de concentration (loi de Fick)",
    x = "Distance (cm)",
    y = "Concentration (mol/cm³)"
  ) +
  theme_minimal()
