# EDA--- (Exploratory Data Analysis) - hecho con inspectdf
# Horizontal bar plot for categorical column composition
x <- inspect_cat(dattrainOr)
show_plot(x)

# Correlation betwee numeric columns + confidence intervals
x <- inspect_cor(dattrainOr)
show_plot(x)

# Bar plot of most frequent category for each categorical column
x <- inspect_imb(dattrainOr)
show_plot(x)

# Bar plot showing memory usage for each column
x <- inspect_mem(dattrainOr)
show_plot(x)

# Occurence of NAs in each column ranked in descending order
x <- inspect_na(dattrainOr)
show_plot(x)

# Histograms for numeric columns
x <- inspect_num(dattrainOr)
show_plot(x)

# Barplot of column types
x <- inspect_types(dattrainOr)
show_plot(x)
