#heights <- c(173, 168, 171, 189, 179)
#weights <- c(65.4, 59.2, 63.6, 88.4, 68.7)
#heights_and_weights <- data.frame(heights, weights)

bmi_calculator  <- function(w, h) {
  h  <- h/100
  bmi  <- w/ h^2
  return(bmi)
}

my_weight  <- 70
my_height  <- 172
bmi_calculator(w = my_weight, h = my_height)

bmis  <- mapply(FUN= bmi_calculator, w = weights, h = heights)
heights_and_weights$bmis  <- bmis
View(heights_and_weights)

heights_and_weights$bmis = NULL

#use mutate
heights_and_weights %>%
  mutate(bmi = weights/ (heights/100)^2) %>%
  View()
