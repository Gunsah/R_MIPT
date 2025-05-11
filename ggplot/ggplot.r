#Задание 2. Реализуйте средствами ggplot2.
#Основные графики
#Постройте точечный график (geom_point()) зависимости расхода топлива (mpg) от мощности (hp) в данных mtcars
#Добавьте цвет точек в зависимости от количества цилиндров (cyl)
#Измените размер точек в зависимости от веса машины (wt)
#Добавьте линию тренда (geom_smooth()) к точечному графику
#Линейные графики
#Постройте линейный график зависимости давления от температуры (pressure dataset)
#Настройте цвет и толщину линии
#Добавьте точки поверх линии
#Столбчатые диаграммы
#Постройте столбчатую диаграмму количества машин по количеству цилиндров
#Создайте диаграмму среднего расхода топлива на трассе (hwy) для каждого класса машин (mpg dataset)
#Настройка внешнего вида
#Для данных diamonds постройте график зависимости цены от карата, раскрасив точки по качеству огранки
#Добавьте прозрачность (alpha) для борьбы с перекрытием точек
#Используйте разные формы точек для разных уровней чистоты (clarity)
#Добавьте подписи осей и заголовок
#Примените одну из встроенных тем (например, theme_minimal())
#Графики с панелями
#Создайте точечный график с панелями по количеству цилиндров (facet_wrap)
#Сделайте разный (автоматически) масштаб осей Y в панелях
#Гистограммы и плотности
#Постройте гистограмму цен на алмазы, экспериментируя с binwidth
#Наложите кривую плотности распределения на гистограмму
#Поверните подписи тиков на оси X на 45 градусов
#Дополнительные графики
#Постройте boxplot расхода по топлива с разбивкой по классу и раскраской по типу привода
#Создайте скрипичный график + jitter-график для длины чашелистиков ирисов по видам
#Постройте тепловую карту корреляций переменных в mtcars (geom_tile, cor)
#Скомбинируйте несколько ранее построенных ggplot графиков в один с помощью пакета cowplot (функция plot_grid)
#Визуализируйте распределение возрастов в данных starwars
#Сравните средний рост персонажей starwars по полу и цвету глаз
#Постройте график зависимости мощности от расхода топлива с подписями моделей машин
library(ggplot2)
library(dplyr)
library(cowplot)

library(ggrepel)
# Точечный график зависимости mpg от hp, цвет по cyl, размер по wt
p1 <- ggplot(mtcars, aes(x = hp, y = mpg, color = factor(cyl), size = wt)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Зависимость расхода топлива от мощности",
       x = "Мощность (л.с.)", y = "Расход топлива (mpg)",
       color = "Цилиндры", size = "Вес") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

# Линейный график давления от температуры
p2 <- ggplot(pressure, aes(x = temperature, y = pressure)) +
  geom_line(color = "red", linewidth = 1) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Зависимость давления от температуры",
       x = "Температура", y = "Давление") +
  theme_classic()

# Столбчатая диаграмма количества машин по цилиндрам
p3 <- ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Количество машин по числу цилиндров",
       x = "Число цилиндров", y = "Количество") +
  theme_minimal()

# Средний hwy по классу из mpg
mpg_c <- mpg %>%
  group_by(class) %>%
  summarise(mean_hwy = mean(hwy))

p4 <- ggplot(mpg_c, aes(x = class, y = mean_hwy)) +
  geom_col(fill = "orange") +
  labs(title = "Средний расход топлива (hwy) по классу машин",
       x = "Класс", y = "Средний hwy") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# График цены от каратов, цвет по огранке, форма по чистоте, прозрачность
p5 <- ggplot(diamonds, aes(x = carat, y = price, color = cut, shape = clarity)) +
  geom_point(alpha = 0.5) +
  labs(title = "Зависимость цены от карата",
       x = "Карат", y = "Цена ($)",
       color = "Огранка", shape = "Чистота") +
  theme_minimal()

# Панельный точечный график по cyl, разные масштабы осей Y
p6 <- ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(aes(color = factor(cyl))) +
  facet_wrap(~ cyl, scales = "free_y") +
  labs(title = "Зависимость mpg от hp по числу цилиндров",
       x = "Мощность", y = "Расход топлива") +
  theme_bw()

# Гистограмма цен на алмазы с плотностью
p7 <- ggplot(diamonds, aes(x = price)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 500, fill = "steelblue", color = "black") +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Цены на алмазы", x = "Цена", y = "Плотность") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot расхода топлива по классам, раскраска по типу привода (drv)
p8 <- ggplot(mpg, aes(x = class, y = hwy, fill = drv)) +
  geom_boxplot() +
  labs(title = "Расход топлива по классу автомобилей",
       x = "Класс автомобиля", y = "Расход на трассе (hwy)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

# Скрипичный график + jitter для длины чашелистиков ирисов
p9 <- ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_violin(trim = FALSE, fill = "lightgray") +
  geom_jitter(width = 0.2, color = "blue", alpha = 0.6) +
  labs(title = "Длина чашелистиков ирисов по видам",
       x = "Вид", y = "Длина чашелистика") +
  theme_minimal()

# Тепловая карта корреляций переменных mtcars
cor_matrix <- cor(select(mtcars, where(is.numeric)))
cor_long <- as.data.frame(as.table(cor_matrix)) %>%
  rename(var1 = Var1, var2 = Var2, value = Freq)

p10 <- ggplot(cor_long, aes(x = var1, y = var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Тепловая карта корреляций")

# График мощности от расхода с подписями моделей
mtcars_rownames <- mtcars
mtcars_rownames$model <- rownames(mtcars)

p11 <- ggplot(mtcars_rownames, aes(x = mpg, y = hp)) +
  geom_point(color = "steelblue") +
  ggrepel::geom_text_repel(aes(label = model), size = 3) +
  labs(title = "Мощность от расхода топлива",
       x = "Расход топлива (mpg)", y = "Мощность (hp)") +
  theme_minimal()

# Распределение возрастов персонажей starwars
p12 <- ggplot(starwars, aes(x = birth_year)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  labs(title = "Распределение возрастов персонажей Star Wars",
       x = "Год рождения", y = "Частота") +
  theme_minimal()

# Сравнение среднего роста по полу и цвету глаз
p13 <- starwars %>%
  filter(!is.na(gender), !is.na(eye_color)) %>%
  group_by(gender, eye_color) %>%
  summarise(mean_height = mean(height, na.rm = TRUE)) %>%
  ggplot(aes(x = gender, y = mean_height, fill = eye_color)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Средний рост по полу и цвету глаз",
       x = "Пол", y = "Средний рост") +
  theme_minimal()

final_plot <- plot_grid(
  p1, p2, p3, p4, p5, p6,
  p7, p8, p9, p10, p11, p12, p13,
  nrow = 4, ncol = 4, labels = "AUTO"
)
ggsave("ggplot_combined.jpg", final_plot, width = 20, height = 15, dpi = 200)
system("xdg-open ggplot_combined.jpg")
