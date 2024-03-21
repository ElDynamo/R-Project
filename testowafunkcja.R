# Funkcja do wczytywania i czyszczenia danych
# Definiowanie funkcji ułatwia ponowne użycie kodu i utrzymanie przez izolowanie logiki.
clean_data <- function(file_path) {
  data <- read.csv(file_path, sep=",")
  data_cleaned <- data[c("<DATE>", "<CLOSE>")]
  colnames(data_cleaned) <- c("Date", "ClosingPrice")
  data_cleaned$Date <- as.Date(as.character(data_cleaned$Date), "%Y%m%d")
  data_cleaned <- data_cleaned[order(data_cleaned$Date),]
  return(data_cleaned)
}

# Funkcja do wykonania regresji liniowej i wygenerowania wykresu
# Oddzielenie logiki analizy danych i wizualizacji od wczytywania danych zwiększa modułowość i czytelność.
plot_linear_regression <- function(data) {
  model <- lm(ClosingPrice ~ Date, data=data)
  print(summary(model))
  ggplot(data, aes(x=Date, y=ClosingPrice)) +
    geom_point() +
    geom_smooth(method="lm", col="blue") +
    theme_minimal() +
    labs(title="Regresja liniowa ceny zamknięcia w czasie",
         x="Data",
         y="Cena zamknięcia")
}

# Struktura kodu jest czytelna i łatwa do śledzenia dzięki wykorzystaniu funkcji.
file_path <- "C:/Users/Adam/Download"
data_cleaned <- clean_data(file_path)
plot_linear_regression(data_cleaned)
