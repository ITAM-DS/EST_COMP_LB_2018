##### EXAMEN PARCIAL #####

library(tidyverse)


##### Bootstrap

## Método Correcto: Especificando la semilla únicamente en la primera Muestra Aleatoria

library(bootstrap)

calcula_intervalos <- function(n = 20){
  x <- rexp(n)
  theta <- mean(x)   # theta_hat
  theta_b <- rerun(2000, sample(x, size = n, replace = TRUE)) %>% 
    map_dbl(~mean(.))
  bca <- bcanon(x, nboot = 2000, theta = function(y) mean(y), alpha = c(0.025, 0.975))$confpoints[,2]  #       intervalos BC_a
  intervalos <- data_frame(metodo = c("normal", "percent", "BC_a"), 
                           izq = c(theta + qnorm(0.025) * sd(theta_b), quantile(theta_b, probs = 0.025), bca[1]),
                           der = c(theta + qnorm(0.975) * sd(theta_b), quantile(theta_b, probs = 0.975), bca[2])
  )
}

calcula_intervalos()
set.seed(261285)
sims_intervalos_20 <- rerun(500, calcula_intervalos()) 

sims_intervalos_20 %>% 
  bind_rows() %>% 
  group_by(metodo) %>%
  summarise(
    falla_izq = 100 * sum(izq > 1) / 500, 
    falla_der = 100 * sum(der < 1) / 500,
    cobertura = 100 - falla_izq - falla_der
  )


### b. 

sims_intervalos <- sims_intervalos_20 %>% bind_rows()

ggplot(sims_intervalos, aes(x=data.frame(seq(1, 500, 5)))) +
  geom_line(aes(y = izq, colour = "green")) +
  geom_line(aes(y = der, colour = "blue")) +
  xlab("No. de Simulaciones") + 
  ylab("Intervalos de Confianza") +
  ggtitle("Gráfica de Páneles") + 
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(-1, 3)) + 
  facet_grid(~ metodo )



##### 5. Simulación de Variables Aleatorias 

### c.

rbinI <- function(n = 10, p = 0.3){
  U <- runif(1)
  c <- (p / (1 - p))
  i <- 0
  Pr <- (1 - p) ^ n
  FDA <- Pr
  while(U >= FDA){
    Pr <-  ((n - i) / (i + 1)) * c * Pr
    FDA <- FDA + Pr
    i <- i + 1
  }
  i
}
rbinI()


### d. 
set.seed(221285)
sims_bin <- rerun(10000, rbinI()) %>% flatten_dbl()
head(sims_bin, 5)
sims_bin[sims_bin==10]

### e.
ggplot() +
  geom_histogram(aes(x = sims_bin, y = ..density..), binwidth = 1)
