library(fickkk) #package actuellement en local
# paramètres
x <- seq(0, 1, length.out = 101)
res <- simulate_diffusion(C1 = 1, C2 = 0, x = x, D = 0.00000069, dt = 0.0001, steps = 300)
# tracer évolution (quelques courbes)
plot_concentration(res$evolution[c(1, 10, 50, 100, 200, 301), ], x = x, times = c(0,9,49,99,199,300),
                   main = "Évolution de la concentration")
# estimer D si tu as les temps
times <- seq(0, 300) * 0.0001
est <- diffusion_coefficient_estimate(res$evolution[c(1, seq(10,300,by=10)), ], x, times = times[c(1, seq(10,300,by=10))])
print(est$D_est)






library(fickkk)
library(ggplot2)
library(reshape2)

# Axe spatial : 0 → 2 cm
x <- seq(0, 2, length.out = 101)

# Conditions aux limites de l'exercice
C1 <- 3e-4        # concentration gauche
C2 <- 7e-5        # concentration droite

# Simulation avec TA fonction
res <- simulate_diffusion(
  C1 = C1,
  C2 = C2,
  x = x,
  D = 6.9e-7,
  dt = 0.01,
  steps = 300
)

# Sélection de temps à tracer
selected_times <- c(1, 50, 100, 200, 300)

# Mise en forme du dataframe pour ggplot
df_plot <- data.frame(res$evolution[selected_times, ])
df_plot$time <- selected_times
df_plot <- melt(df_plot, id.vars = "time")

# ajout de l'axe spatial
df_plot$x <- x[as.numeric(df_plot$variable)]

# Graphique
ggplot(df_plot, aes(x = x, y = value, color = factor(time))) +
  geom_line(size = 1.2) +
  labs(
    title = "Évolution de la concentration (diffusion)",
    x = "Distance (cm)",
    y = "Concentration (mol/L)",
    color = "Temps (itérations)"
  ) +
  theme_minimal()



library(fickkk)
library(ggplot2)
library(reshape2)

# Axe spatial
x <- seq(0, 2, length.out = 101)   # 0 → 2 cm

# Conditions de concentration
C_initial <- rep(0, length(x))
C_initial[1] <- 3e-4 / 1000         # C1
C_initial[length(x)] <- 7e-5 / 1000 # C2

# Simulation
res <- simulate_diffusion(
  C_initial = C_initial,
  D = 6.9e-7,
  dx = mean(diff(x)),
  dt = 0.01,
  n_steps = 300
)

# On prend quelques temps pour tracer
selected_times <- c(1, 50, 100, 200, 300)
df_plot <- data.frame(res$evolution[selected_times, ])
df_plot$time <- selected_times
df_plot <- melt(df_plot, id.vars = "time")
df_plot$x <- x[df_plot$variable]

# Graphique
ggplot(df_plot, aes(x = x, y = value, color = factor(time))) +
  geom_line(size = 1.2) +
  labs(
    title = "Évolution de la concentration (diffusion)",
    x = "Distance (cm)",
    y = "Concentration (mol/cm³)",
    color = "Temps"
  ) +
  theme_minimal()

library(fickkk)

x <- seq(0, 2, length.out = 101)  # distance de 2 cm
D <- 6.9e-7 / (100^2)              # conversion cm2/s → m2/s si besoin
steps <- 300                       # nombre d’itérations
dt <- 0.01                         # pas de temps (exemple)

res <- simulate_diffusion(
  C1 = 3e-4,
  C2 = 7e-5,
  x = x,
  D = 6.9e-7,
  dt = dt,
  n_steps = steps
)
times_plot <- c(1, 50, 100, 200, 300)

matplot(
  x,
  res$evolution[times_plot, ],
  type = "l",
  lwd = 2,
  xlab = "Distance (cm)",
  ylab = "Concentration",
  main = "Diffusion de l'hémoglobine dans le temps"
)
legend("topright", legend = paste("step", times_plot), col = 1:5, lwd = 2)





