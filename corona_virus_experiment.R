library(patchwork)
library(ggplot2)
library(dplyr)
library(pwr)


n_canarias <- 2153000
n_madrid <- 6550000

cases_canarias <- 12
cases_madrid <- 109

# We are supposing the corona virus test is taked by all of the population
p_canarias <- rbeta(n_canarias, cases_canarias,
                    n_canarias - cases_canarias)

p_madrid <- rbeta(n_madrid, cases_madrid,
                  n_madrid - cases_madrid)

data <- data.frame(population=c(rep('Canarias', length(p_canarias)),
                                rep('Madrid', length(p_madrid))),
                   corona=c(p_canarias, p_madrid))

xlimit <- 2 * mean(data$corona)

density_plot <- data %>%
  sample_frac(.2) %>%
  filter(corona < xlimit) %>% 
  ggplot(aes(x = corona)) +
  geom_density(aes(fill = population)) +
  scale_x_continuous(limits = c(0, xlimit))

power_90 <- pwr.p.test(ES.h(p1 = mean(p_canarias),
                            p2 = mean(p_madrid)),
                       sig.level = 0.05,
                       power = 0.9)

power_plot <- plot(power_90)

density_plot + power_plot
