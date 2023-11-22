raw <- filter(raw, p == "N-experiment")

city = c("Melbourne", "Minsk", "Chengdu", "Copenhagen", "Bonn", "Athens", "Seoul", "Samara", "Zurich", "St. Gallen", "Istanbul", "Nottingham", "Dnipropetrovs'k", "Boston")
country_id = seq(1,length(city),1)
gini = c(34.3, 25.3, 38.5, 28.7, 31.9, 34.4, 31.6, 37.5,32.7,32.7,41.9,34.8,26.1,41.1)
indi = c(90, 25, 20, 74, 67, 35, 18, 25, 68, 68, 37, 89, 25, 91)
gdp = c(32.9, 8.9, 7.6, 36.5, 31.1, 26.0, 23.9, 12.1, 37.4, 37.4, 9.1, 35.1, 7.6, 43.4)
trust = c(.40, .42, .55, .67, .38, .24, .27, .24, .37, .37, .16, .29, .27, .36)
data <- data.frame(city, index, gini, indi, gdp, trust)

df <- merge(raw, data, on = "city")