#Задание 3. Повторить любой другой тип графика из R graph gallery (кроме тех, что в задании 1-2) средствами базовой графики, используя built-in датасет. Если в примере из R graph gallery уже использованы средства базовой графики, можно просто повторить график, используя другие данные, не из примера.


data(iris)

numeric_vars <- names(iris)[sapply(iris, is.numeric)]

png("T3.png", width = 1000, height = 800, res = 300)
par(mfrow = c(2, 2), mar = c(3, 3, 2, 1))

for (var in numeric_vars) {
  formula <- as.formula(paste(var, "~ Species"))
  boxplot(formula, data = iris, main = var, col = "lightgreen")
}

dev.off()
system("xdg-open T3.png")
