library(tidyverse)
library(knitr)
library(gapminder)

continent_lifeExp <- gapminder %>%
  group_by(continent, year) %>%
  summarise(lifeExp = mean(lifeExp, na.rm = TRUE), .groups = "drop") %>%
  split(.$continent) %>%
  lapply(function(df) select(df, year, lifeExp))

continent_lifeExp[1]

gapminder_asia <- gapminder %>% filter(continent == "Asia")
pop_mean_summary <- gapminder_asia %>% group_by(country) %>% summarise(mean_pop = mean(pop))

asia_pop_mean <- cbind(gapminder_asia, pop_mean = rep(pop_mean_summary$mean_pop, each = 12))

initial_year <- asia_pop_mean %>%
  filter(pop > pop_mean) %>%
  group_by(country) %>%
  summarise(year = min(year), .groups = "drop")

initial_year

gap_year_filtered <- gapminder %>%
  filter(year %in% c(1987, 1992, 1997, 2002, 2007))

summarized_data <- gap_year_filtered %>%
  group_by(year, continent) %>%
  summarise(
    lifeExp = mean(lifeExp, na.rm = TRUE),
    pop = mean(pop, na.rm = TRUE),
    gdpPercap = mean(gdpPercap, na.rm = TRUE),
    .groups = "drop"
  )

gapminder_array <- array(
  data = c(
    summarized_data$lifeExp,
    summarized_data$pop,
    summarized_data$gdpPercap
  ),
  dim = c(length(unique(summarized_data$year)),
          length(unique(summarized_data$continent)),
          3),
  dimnames = list(
    year = unique(summarized_data$year),
    continent = unique(summarized_data$continent),
    var = c("lifeExp", "pop", "gdpPercap")
  )
)

print(gapminder_array)

min_values <- apply(gapminder_array, c(1, 3), min) 
max_values <- apply(gapminder_array, c(1, 3), max)

for (var in dimnames(gapminder_array)$var) {
  cat(", , var =", var, "\n\n")
  
  cat("Min\n")
  print(min_values[, var, drop = FALSE])
  cat("\n")
  
  cat("Max\n")
  print(max_values[, var, drop = FALSE])
  cat("\n")
}

cat(", , year = 1987\n")
print(gapminder_array["1987", , ])

cat("\n, , year = 1992\n")
print(gapminder_array["1992", , ])

heart <- read_csv("C:\Users\Edward Lai\Documents\Portfolio\R\heart.csv")

sex_disease <- matrix(c(143, 267, 50, 458), 
                      nrow = 2, 
                      dimnames = list(
                        HeartDisease = c("0", "1"),
                        Sex = c("F", "M")
                      ))

print(sex_disease)

chi_square_test <- chisq.test(sex_disease)
chi_square_stat <- chi_square_test$statistic
total_n <- sum(sex_disease)
min_dim <- min(dim(sex_disease))

cramers_v <- sqrt(chi_square_stat / (total_n * (min_dim - 1)))
cramers_v

#Answer: Cramer’s V = 0.3028 → weak association between sex and heart disease.

angina_disease <- matrix(c(355, 55, 192, 316), 
                         nrow = 2, 
                         dimnames = list(
                           HeartDisease = c("0", "1"),
                           ExerciseAngina = c("N", "Y")
                         ))

print(angina_disease)

a <- angina_disease["1", "Y"]
b <- angina_disease["0", "Y"]
c <- angina_disease["1", "N"]
d <- angina_disease["0", "N"]

odds_ratio <- (a * d) / (b * c)
odds_ratio

#Answer: OR = 10.6231 → strong association between exercise-induced angina and heart disease.

correlation <- cor(heart$RestingBP, heart$MaxHR, method = "pearson", use = "complete.obs")
correlation
#Answer: Pearson’s r = -0.1121 → weak negative relationship between resting blood pressure and max heart rate.

