library("dplyr")

hepatitis_data<-read.csv("E:/Desktop/OneDrive/Documents/IDS/ds/Hepatitis data/hepatitis/hepatitis_data.csv")
numerical_vars <- hepatitis_data[, sapply(hepatitis_data, is.numeric)]
pairs(numerical_vars)

# Hexagonal binning plot between Age and ALB
plot(hexbin(hepatitis_data$Age, hepatitis_data$ALB), main = "", colramp = colorRampPalette(c("blue", "yellow")), legend = F)

# Compute the correlation matrix of numerical variables
correlation_matrix <- cor(hepatitis_data[, sapply(hepatitis_data, is.numeric)])

# Plot the correlation matrix using corrplot
corrplot(correlation_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, tl.cex = 0.7)

# Create a bubble chart with Age, ALB, and BIL
ggplot(hepatitis_data, aes(x = Age, y = ALB, size = BIL)) +
  geom_point(alpha = 0.6) +
  scale_size(range = c(3, 10)) +
  labs(title = "Bubble Chart", x = "Age", y = "ALB", size = "BIL") +
  theme_minimal()

# Create a mosaic plot to visualize the joint distribution of Category and Sex
mosaicplot(table(hepatitis_data$Category, hepatitis_data$Sex), col = c("blue", "red"), las = 2)

# Compute the correlation matrix of numerical variables for the heatmap
numeric_data <- hepatitis_data[, sapply(hepatitis_data, is.numeric)]
correlation_matrix <- cor(numeric_data)

# Plot the heatmap of the correlation matrix
heatmap(correlation_matrix, col = colorRampPalette(c("blue", "white", "red"))(100), symm = TRUE, main = "Correlation Heatmap")

# Create a box plot to summarize ALT levels across different categories
ggplot(hepatitis_data, aes(x = Category, y = ALT)) +
  geom_boxplot() +
  labs(title = "Distribution of ALT by Category", x = "Category", y = "ALT")

# Create a 3D scatter plot to visualize Age, ALT, and AST values
plot_ly(hepatitis_data, x = ~Age, y = ~ALT, z = ~AST, color = ~Category, colors = c('blue', 'red', 'orange', 'green', 'purple'), type = 'scatter3d', mode = 'markers') %>%
  layout(title = "3D Scatter Plot of Hepatitis Data",
         scene = list(xaxis = list(title = 'Age'),
                      yaxis = list(title = 'ALT'),
                      zaxis = list(title = 'AST')))

# Create a density plot to illustrate the distribution of cholesterol levels by sex
ggplot(hepatitis_data, aes(x = CHOL, fill = Sex)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Cholesterol by Sex", x = "Cholesterol", y = "Density") +
  scale_fill_manual(values = c("pink", "blue"))

# Create a ridge plot to visualize the distribution of albumin levels across categories
ridge_plot <- ggplot(hepatitis_data, aes(x = ALB, y = Category, fill = Category)) +
  geom_density_ridges() +
  labs(x = "ALB", y = "Category", title = "Albumin Distribution by Category") +
  theme_minimal()
print(ridge_plot)

# Create a Categorical Parallel Coordinates Plot (CPCP) for multivariate data
cpcp_plot <- ggparcoord(data = hepatitis_data, columns = c(1, 3, 7, 8, 11, 14), groupColumn = 2, scale = "globalminmax") +
  labs(title = "Categorical Parallel Coordinates Plot of Hepatitis Data", x = "Variables", y = "Values") +
  theme_minimal() +
  facet_wrap(~ Sex)
print(cpcp_plot)

# Create an alluvial plot to visualize the flow and distribution between Sex and Category
ggplot(hepatitis_data, aes(axis1 = Sex, axis2 = Category, y = ..count..)) +
  geom_alluvium(aes(fill = Category)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Sex", "Category")) +
  labs(title = "Alluvial Plot of Hepatitis Data", x = "Variables", y = "Count") +
  theme_minimal()

# Create a violin plot to visualize the distribution of ALT levels by sex
ggplot(hepatitis_data, aes(x = Sex, y = ALT, fill = Sex)) +
  geom_violin() +
  labs(x = "Sex", y = "ALT", title = "Violin Plot of ALT by Sex")

# Create a slope plot to visualize the relationship between ALP and CHOL levels
ggplot(hepatitis_data, aes(x = ALP, y = CHOL, color = CHOL)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "ALP", y = "CHOL", title = "Slope Plot of ALP by CHOL")

# Create a cross-sectional plot to visualize the relationship between ALT and AST levels
ggplot(hepatitis_data, aes(x = ALT, y = AST)) +
  geom_point(color = "purple") +
  labs(x = "ALT", y = "AST", title = "Cross-sectional Plot of ALT vs. AST")

# Create a contour plot to visualize the joint distribution of Bilirubin and Cholesterol levels
ggplot(hepatitis_data, aes(x = CHOL, y = BIL)) +
  geom_density_2d(aes(fill = ..level..)) +
  labs(x = "CHOL", y = "BIL", title = "Contour Plot of BIL vs CHOL")

# Create a pie chart to represent the proportion of each category
pie_chart <- ggplot(category_df, aes(x = "", y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart of Hepatitis Categories") +
  theme_void() +
  scale_fill_manual(values = rainbow(length(unique(hepatitis_data$Category))))
print(pie_chart)

# Create an ECDF plot to illustrate the cumulative distribution of the AST variable
ecdf_plot <- ggplot(hepatitis_data, aes(x = AST)) +
  stat_ecdf(geom = "step", color = "blue") +
  labs(title = "ECDF Plot of AST in Hepatitis Data", x = "AST", y = "Cumulative Probability") +
  theme_minimal()
print(ecdf_plot)
