#Задание 1 по базовой графике:
#Для датасета mtcars визуализировать:
#• распределение qsec в виде гладкой линии и гистограммы (hist, plot-density-polygon)
#• для гистограммы: подписать имена оси x, заголовок, подзаголовок, заполнить цветом гистограмму, задать другим цветом границы столбцов гистограммы. Увеличить размер шрифтов для осей в 2 раза (подписи и тики). 
#• точечный график, на котором будет отображаться зависимость mpg от disp (plot). Задать свой цвет, форму элементов для каждого значения cyl. Размер элементов должен быть равен 2. 
#• боксплоты+джиттерплоты mpg для каждого значения cyl(три боксплота на 1 графике, джиттерплоты наложены на боксплоты). Надписи по оси абсцисс развернуть под углом 90 градусов.
#Задание 2. Построить график c 4 панелями, использовав графики, сделанные ранее: распределение в виде гладкой линии, гистограмму, боксплоты и точечный график.
#Добавить буквы A B C В на соотв. панели(верхний левый угол.


#Запускаю в терминале, поэтому сохраняю выводы в файле и сразу же его открываю
#Сразу все вместе, чтоб было удобнее

data(mtcars)
qsec_data <- mtcars$qsec

png("T12.png", width = 300 * 10, height = 300 * 10, res = 300)

par(mfrow = c(2, 2))
# Гистограмма + гладкая линия
hist(
    qsec_data,
    col = "lightblue",
    border = "darkblue",
    main = "Гистограмма для qsec",
    sub = "qsec = quarter mile time",
    xlab = "qsec, с",
    ylab = "Частота",
    freq = FALSE,
    cex.axis = 2,
    cex.lab = 2
    )
mtext("A", side = 3, line = 1, adj = 0, cex = 1.5, font = 2)

plot(
    density(qsec_data), 
    col = "red", 
    lwd = 3,
    cex.axis = 2,
    cex.lab = 2
    )

mtext("B", side = 3, line = 1, adj = 0, cex = 1.5, font = 2)

# Точечный график
mpg_data <- mtcars$mpg
disp_data <- mtcars$disp
cyl_data <- mtcars$cyl

plot(
    disp_data, mpg_data,
    xlab = "disp", ylab = "mpg",
    cex.axis = 2, cex.lab = 2
    )

cols <- c("red", "green", "blue")
shapes <- c(17, 19, 8)

for (i in unique(cyl_data)) {
  subset_data <- subset(mtcars, cyl == i)
  points(subset_data$disp, subset_data$mpg,
         col = cols[i %in% unique(cyl_data)], 
         pch = shapes[i %in% unique(cyl_data)],
         cex = 2)
}

mtcars$cyl <- as.factor(mtcars$cyl)
mtext("C", side = 3, line = 1, adj = 0, cex = 1.5, font = 2)

# Боксплоты
boxplot(mpg ~ cyl,
        data = mtcars,
        xlab = "cyl",
        ylab = "mpg",
        col = "lightgray",
        las = 2,
        axes = FALSE
)


for (cyl_val in levels(mtcars$cyl)) {
  jitter_x <- jitter(rep(as.numeric(cyl_val), nrow(mtcars[mtcars$cyl == cyl_val, ])))
  jitter_y <- mtcars$mpg[mtcars$cyl == cyl_val]

  points(jitter_x, 
         jitter_y,
         col = "red", 
         pch = 19,
         cex = 0.8)
}

axis(1, at = 1:length(levels(mtcars$cyl)), labels = levels(mtcars$cyl), las = 2)
axis(2)
box()
mtext("D", side = 3, line = 1, adj = 0, cex = 1.5, font = 2)

dev.off()
system("xdg-open T12.png")
