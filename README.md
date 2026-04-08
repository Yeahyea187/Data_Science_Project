# Supermarket Sales Data Analysis Project

## Dataset Description

This dataset contains 1000 supermarket sales records including customer details, product categories, and transaction information.

**Key Features:**

- **Customer Info:** Gender, Customer.type
- **Product Info:** Product.line, Unit.price, Quantity
- **Transaction Info:** Sales, gross.income, Payment

The dataset is processed to ensure data quality, consistency, and usability for analysis.

## Project Implementation Detail

### Task 1: Handling Missing Values

**Description of Task 1**  
Missing values can affect data analysis accuracy. This task ensures all missing values are properly handled.

**How I solved it**  
Converted empty values ("", " ") into NA  
Counted missing values  
Replaced:

- Numeric → Mean
- Categorical → Mode

**Code**

```r
data[data == ""] <- NA
data[data == " "] <- NA

sum(is.na(data))
colSums(is.na(data))
```

**Sample Output**

```
Total Missing Values: 37

Customer.type    10
Gender           10
Quantity         12
Payment           5
```

**Replacement Code**

```r
dataset_replaced <- data

get_mode <- function(v) {
  v <- v[!is.na(v)]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv))]
}

for (col in names(dataset_replaced)) {
  if (any(is.na(dataset_replaced[[col]]))) {
    if (is.numeric(dataset_replaced[[col]])) {
      dataset_replaced[[col]][is.na(dataset_replaced[[col]])] <-
        round(mean(dataset_replaced[[col]], na.rm = TRUE))
    } else {
      dataset_replaced[[col]][is.na(dataset_replaced[[col]])] <-
        get_mode(dataset_replaced[[col]])
    }
  }
}
```

**Output**

```
All missing values successfully replaced (0 remaining)
```

### Task 2: Missing Data Visualization

**Description of Task 2**  
Visualization helps identify missing data patterns.

**Code**

```r
gg_miss_var(data)
gg_miss_upset(data)
vis_miss(data)
```

**Output Description**  
Bar chart shows missing values in key columns  
Heatmap visualizes missing data positions  
UpSet plot shows missing combinations

### Task 3: Detect and Handle Outliers

**Description of Task 3**  
Outliers distort data analysis. The IQR method is used to detect and cap them.

**Code**

```r
dataset_outlier_handled$Unit.price <-
  handle_outliers_iqr(dataset_outlier_handled$Unit.price)
```

**Sample Output**

```
Outliers detected: 4
Lower Bound: -34.14
Upper Bound: 145.57
```

**Before Handling**

```
Max: 3041.00
Mean: 63.98
```

**After Handling**

```
Max: 145.57
Mean: 56.10
```

**Output Description**  
Extreme values were capped, improving data stability.

### Task 4: Numeric to Categorical (Binning)

**Description of Task 4**  
Continuous variables are grouped into categories for better interpretation.

**Code**

```r
dataset_converted$Unit_Price_Group <- cut(dataset_converted$Unit.price,
                                          breaks = c(0, 50, 100, 146),
                                          labels = c("Low", "Medium", "High"))
```

**Sample Output**

```
Unit.price | Group
74.69      Medium
15.28      Low
```

**Value Counts**

```
Unit_Price_Group:
Low     436
Medium  560
High      4

Quantity_Group:
Small   287
Medium  419
Large   294

Gross_Income_Group:
Low     589
Medium  322
High     89
```

**Output Description**  
Data is categorized into meaningful groups for analysis.

### Task 5: Normalization

**Description of Task 5**  
Normalization scales data between 0 and 1.

**Code**

```r
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

dataset_normalized$Unit.price_Normalized <-
  min_max_norm(dataset_normalized$Unit.price)
```

**Sample Output**

```
Min: 0.0000
Mean: 0.3396
Max: 1.0000
```

**Output Description**  
All values are scaled for fair comparison.

### Task 6: Handle Duplicate Rows

**Description of Task 6**  
Duplicate rows can bias results.

**Code**

```r
duplicate_count <- sum(duplicated(dataset_normalized))
dataset_final <- dataset_normalized[!duplicated(dataset_normalized), ]
```

**Sample Output**

```
Duplicate rows found: 0
Final dataset size: 1000
```

**Output Description**  
No duplicates found.

### Task 7: Data Filtering Methods

**Description of Task 7**  
Analyzing customer behavior using filtering.

**Code**

```r
gender_product <- dataset_final %>%
  group_by(Gender, Product.line) %>%
  summarise(count = n())
```

**Sample Output**

```
Female:
Fashion accessories 109

Male:
Electronic accessories 76
```

**Output Description**  
Shows product preference by gender.

### Task 8: Dataset Balancing

**Description of Task 8**  
Balances imbalanced classes.

**Sample Output**

```
Original:
Low     589
Medium  322
High     89

Under-sampling:
89 each

Over-sampling:
589 each
```

**Output Description**  
Balanced dataset improves model fairness.

### Task 9: Train-Test Split

**Description of Task 9**  
Split dataset for model validation.

**Sample Output**

```
Total: 1000
Train: 700
Test: 300
Overlap: 0
```

**Output Description**  
Proper dataset split achieved.

### Task 10: Descriptive Statistics

**Description of Task 10**  
Statistical summary of dataset.

**Sample Output**

```
Mean: 56.10
Median: 55.53
SD: 27.03
Variance: 730.53
```

**By Income Group**

```
Low     44.71
Medium  68.20
High    87.64
```

**Output Description**  
Higher income → higher product price.

### Task 11: Compare Average Values Across Categories

**Sample Output**

```
Male Sales:   299.34
Female Sales: 472.42
```

**Interpretation**  
Female customers spend more.

### Task 12: Compare Variability Across Categories

**Sample Output**

```
Most stable product line: Fashion accessories
Gender Stability
Male SD: 235.14 (more stable)
Female SD: higher variability
```

**Output Description**  
Identifies stability across categories.
