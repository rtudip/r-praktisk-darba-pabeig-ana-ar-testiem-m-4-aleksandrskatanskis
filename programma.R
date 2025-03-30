# Load required libraries
library(dplyr)
library(ggplot2)

# Initialize output file
output_file <- "results.txt"
write("", file = output_file)  # Clear the file or create it if it doesn't exist

# Function to log messages to the output file
log_message <- function(message) {
  write(message, file = output_file, append = TRUE)
}

# Step 1: Load the data
if (!file.exists("variants2.txt")) {
  log_message("Error: File 'variants2.txt' not found. Please ensure the file exists in the working directory.")
  stop("File not found.")
}

kordat <- read.table("variants2.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE, strip.white = TRUE)
row.names(kordat) <- kordat[, 1]
kordat <- kordat[, -1]
log_message("Data loaded successfully.")

# Debugging: Print first few rows and column names
log_message("First few rows of the dataset:")
log_message(capture.output(head(kordat)))
log_message("Column names:")
log_message(paste(colnames(kordat), collapse = ", "))

# Step 2: Convert columns 9 and onward to factors
if (ncol(kordat) >= 9) {
  kordat[, 9:ncol(kordat)] <- lapply(kordat[, 9:ncol(kordat)], as.factor)
  log_message("Columns 9 and onward converted to factors.")
} else {
  log_message("Warning: Not enough columns to convert to factors.")
}

# Step 3: Fix non-numeric values in numeric columns
num_cols <- c("Slope", "Intercept", "adj.r.squared")
for (col in num_cols) {
  if (col %in% colnames(kordat)) {
    kordat[[col]] <- as.numeric(gsub(",", ".", kordat[[col]]))
    log_message(paste("Column", col, "converted to numeric."))
  } else {
    log_message(paste("Warning: Column", col, "not found."))
  }
}

# Debugging: Print summary of numeric columns
log_message("Summary of numeric columns after conversion:")
log_message(capture.output(summary(kordat[, num_cols])))

# Step 4: Remove rows with missing values in numeric columns
kordat <- kordat[complete.cases(kordat[, num_cols]), ]
log_message("Rows with missing values in numeric columns removed.")

# Step 5: Summarize factor levels
if (ncol(kordat) >= 9) {
  factor_summary <- sapply(kordat[, 9:ncol(kordat)], table)
  log_message("Factor Levels Summary:")
  log_message(capture.output(factor_summary))
} else {
  log_message("Warning: Not enough columns to summarize factor levels.")
}

# Step 6: Split "Slope" by 'b' factor levels
if ("Slope" %in% colnames(kordat) && "b" %in% colnames(kordat)) {
  sl_by_b <- split(kordat$Slope, kordat$b)
  log_message("Slope values split by 'b' factor levels:")
  log_message(capture.output(sl_by_b))
} else {
  log_message("Warning: Columns 'Slope' or 'b' not found. Skipping split operation.")
}

# Step 7: Create "Average" column
if (all(num_cols %in% colnames(kordat))) {
  kordat$Average <- rowMeans(kordat[, num_cols], na.rm = TRUE)
  log_message("Column 'Average' created successfully.")
} else {
  log_message("Warning: Required columns for 'Average' calculation not found.")
}

# Step 8: Calculate standard deviation by 'f' factor levels
if ("f" %in% colnames(kordat) && is.factor(kordat$f)) {
  std_dev_by_f <- aggregate(. ~ f, data = kordat[, c("f", num_cols)], FUN = function(x) sd(x, na.rm = TRUE))
  log_message("Standard Deviation by 'f' factor levels:")
  log_message(capture.output(std_dev_by_f))
} else {
  log_message("Warning: Column 'f' not found or is not a factor. Skipping standard deviation calculation.")
}

# Step 9: Filter rows for prockordat
if ("adj.r.squared" %in% colnames(kordat)) {
  prockordat <- kordat[!is.na(kordat$adj.r.squared) & kordat$adj.r.squared > 0.7, ]
  if ("Slope" %in% colnames(prockordat)) {
    prockordat$Slope <- 1 - (1 / prockordat$Slope)
  }
  log_message("Processed Kordat (prockordat):")
  log_message(capture.output(prockordat))
} else {
  log_message("Warning: Column 'adj.r.squared' not found. Skipping prockordat creation.")
}

# Step 10: Identify most frequent factor level in row names
if (exists("prockordat") && nrow(prockordat) > 0) {
  factor_levels <- unlist(strsplit(rownames(kordat), "[.]"))
  most_frequent_level <- names(sort(table(factor_levels), decreasing = TRUE))[1]
  filtered_prockordat <- prockordat[grep(most_frequent_level, rownames(prockordat)), ]
  log_message("Filtered Prockordat by most frequent factor level:")
  log_message(capture.output(filtered_prockordat))
} else {
  log_message("Warning: prockordat not found or is empty. Skipping filtering by factor level.")
}

# Step 11: Scatter plot
if ("MAD" %in% colnames(kordat) && "Average" %in% colnames(kordat)) {
  scatter_plot <- ggplot(kordat, aes(x = MAD, y = Average)) +
    geom_point() +
    labs(title = "Scatter Plot of MAD vs. Average", x = "MAD", y = "Average")
  ggsave("scatter.svg", plot = scatter_plot)
  log_message("Scatter plot saved as 'scatter.svg'.")
} else {
  log_message("Warning: Columns 'MAD' or 'Average' not found. Skipping scatter plot.")
}

# Step 12: Boxplot
if ("f" %in% colnames(kordat) && "Intercept" %in% colnames(kordat)) {
  box_plot <- ggplot(kordat, aes(x = f, y = Intercept, fill = f)) +
    geom_boxplot() +
    labs(title = "Boxplot of Intercept by f factor levels", x = "f Factor Levels", y = "Intercept")
  ggsave("boxplot.svg", plot = box_plot)
  log_message("Boxplot saved as 'boxplot.svg'.")
} else {
  log_message("Warning: Columns 'f' or 'Intercept' not found. Skipping boxplot.")
}

# Final message
log_message("Script execution completed.")