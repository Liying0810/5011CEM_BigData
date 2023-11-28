# Load the dataset and remove unnecessary columns
summer_df <- read.csv("C:/Users/User/Downloads/summer-products-with-rating-and-performance_2020-08.csv")

# Remove columns not required for analysis
summer_df$title_orig <- NULL
summer_df$currency_buyer <- NULL
summer_df$tags <- NULL
summer_df$product_color <- NULL
summer_df$product_variation_size_id <- NULL
summer_df$shipping_option_name <- NULL
summer_df$origin_country <- NULL
summer_df$merchant_title <- NULL
summer_df$merchant_name <- NULL
summer_df$merchant_info_subtitle <- NULL
summer_df$merchant_id <- NULL
# ... (remove other columns)

# Split data into training and test sets
m = nrow(summer_df)
set.seed(2903)
train_idx <- sample(m, 0.9 * m)
train_df <- summer_df[train_idx, ]
test_df <- summer_df[-train_idx, ]


# Model 1: Linear regression with 'units_sold' predicted by 'price'
fit.lm <- lm(formula = units_sold ~ price, data = train_df)
summary(fit.lm)

# Model 2: Linear regression with multiple predictors and polynomial terms
fit.lm2 <- lm(formula = units_sold ~ price + I(price^2) + rating + I(rating^3) +
                rating_count + I(rating_count^4) + rating_four_count + I(rating_four_count^5) +
                rating_three_count + I(rating_three_count^6) + product_variation_inventory +
                I(product_variation_inventory^7),
              data = train_df)
summary(fit.lm2)

# Model 3: Linear regression with a simplified set of predictors
fit.lm3 <- lm(formula = units_sold ~ price + rating_count + rating_five_count + rating_four_count + rating_three_count + merchant_rating_count, data = summer_df)
summary(fit.lm3)

# Model 4: Linear regression with interaction terms
fit.ireg <- lm(formula = units_sold ~ price + rating_count + rating_four_count + product_variation_inventory +
                 price:rating_count + price:rating_four_count + price:product_variation_inventory, 
               data = train_df)

summary(fit.ireg)

