packages <- c('caret', 'rpart', 'corrplot', 'rpart.plot', 'car',
              'VGAM', 'dplyr', 'caTools', 'class', 'ggplot2',
              'kernlab', 'glmnet', 'tidyr', 'reticulate', 'proxy')

invisible(lapply(packages, library, character.only = TRUE))

dir.create("output", showWarnings = FALSE)

### setwd('C:/Users/vomba/Desktop/Studijos/Data Mining/Group Project/Nutrient Project/Git')


# 1. Read the five files into one table ---------------------------------
file_names <- paste0("FOOD-DATA-GROUP", 1:5, ".csv")
all_files <- list()

for (name in file_names) {
  one_file <- read.csv(file.path("data", name), check.names = FALSE)
  bad_columns <- names(one_file) == "" | grepl("^Unnamed", names(one_file))
  one_file <- one_file[, !bad_columns]                         # drop index column
  one_file$group_file <- sub(".csv", "", name, fixed = TRUE)   # source file (for food categories later)
  all_files[[name]] <- one_file
}

food <- bind_rows(all_files)
food$id <- seq_len(nrow(food))   # unique food ID
food$`Nutrition Density` <- NULL   # not used
cat("Number of foods:", nrow(food), "\n")  

macros <- c("Fat", "Saturated Fats", "Monounsaturated Fats",
            "Polyunsaturated Fats", "Carbohydrates", "Sugars",
            "Protein", "Dietary Fiber", "Water")
minerals <- c("Calcium", "Copper", "Iron", "Magnesium", "Manganese",
              "Phosphorus", "Potassium", "Selenium", "Zinc")


# 2. Values per 100 g? (Figure 1) --------------------------------
# If yes, water + fat + carbohydrates + protein would be about 100 g
food$portion_g <- food$Water + food$Fat + food$Carbohydrates + food$Protein

cat("Share of foods with sum 90-110 g:",
    round(mean(food$portion_g >= 90 & food$portion_g <= 110), 3), "\n")   # ~0.04

ggplot(filter(food, portion_g > 0), aes(x = portion_g)) +
  geom_histogram(bins = 60, fill = "#4472C4") +
  geom_vline(xintercept = 100, linetype = "dashed", colour = "red") +
  scale_x_log10() +
  labs(x = "Water + fat + carbohydrates + protein (g, log scale)",
       y = "Number of foods",
       title = "Sum of main components per food (red line = 100 g)") +
  theme_minimal()
ggsave("output/fig1_portion_check.png", width = 8, height = 5, dpi = 300)


# 3. Convert to 100 g and clean (Table 1) --------------------------------
# Remove foods under 5 g (scaling up multiplies rounding errors)
step1 <- filter(food, portion_g >= 5)

# Convert: value / reference weight * 100
nutrient_columns <- setdiff(names(food)[sapply(food, is.numeric)], c("portion_g", "id"))
for (col in nutrient_columns) {
  step1[[col]] <- step1[[col]] / step1$portion_g * 100
}

# Remove impossible foods: a macronutrient above 100 g per 100 g
step2 <- step1[apply(step1[macros], 1, max) <= 100, ]

# Remove impossible foods: more sugar than carbohydrates (+0.1 for rounding)
step3 <- filter(step2, Sugars <= Carbohydrates + 0.1)

food100 <- step3

cleaning <- data.frame(
  step = c("Original dataset", "Reference weight below 5 g",
           "Nutrient above 100 g per 100 g", "Sugars exceed total carbohydrates"),
  removed = c(NA, nrow(food) - nrow(step1), nrow(step1) - nrow(step2),
              nrow(step2) - nrow(step3)),
  remaining = c(nrow(food), nrow(step1), nrow(step2), nrow(step3))
)
print(cleaning)
write.csv(cleaning, "output/table1_cleaning_steps.csv", row.names = FALSE)


# 4. Share of zeros (targets need < 25% zeros) ---------------------------
vitamins <- grep("^Vitamin", names(food100), value = TRUE)
share_zero <- sapply(food100[c(minerals, vitamins)], function(x) mean(x == 0))
print(round(sort(share_zero), 3))


# 5. Mineral distributions(Figure 2) -----------------
minerals_long <- pivot_longer(food100[minerals], cols = everything(),
                              names_to = "mineral", values_to = "value")

ggplot(minerals_long, aes(x = value)) +
  geom_histogram(bins = 40, fill = "#4472C4") +
  facet_wrap(~ mineral, scales = "free") +
  labs(x = "Value per 100 g", y = "Number of foods", title = "Minerals: raw scale") +
  theme_minimal()
ggsave("output/fig2_minerals_raw.png", width = 10, height = 7, dpi = 300)

# log1p(x) = log(1 + x)
ggplot(minerals_long, aes(x = log1p(value))) +
  geom_histogram(bins = 40, fill = "#4472C4") +
  facet_wrap(~ mineral, scales = "free") +
  labs(x = "log(1 + value per 100 g)", y = "Number of foods", title = "Minerals: log scale") +
  theme_minimal()
ggsave("output/fig2_minerals_log.png", width = 10, height = 7, dpi = 300)


# 6. Spearman correlations --------
cor_matrix <- cor(food100[macros], food100[minerals], method = "spearman")
cor_table <- as.data.frame(as.table(cor_matrix))
names(cor_table) <- c("macronutrient", "mineral", "rho")

ggplot(cor_table, aes(x = mineral, y = macronutrient, fill = rho)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(rho, 2)), size = 2.6) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       limits = c(-1, 1)) +
  labs(x = NULL, y = NULL, fill = "Spearman\nrho",
       title = "Correlation between macronutrients and minerals (per 100 g)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("output/fig3_correlations.png", width = 10, height = 6, dpi = 300)


# 7. Save the cleaned dataset - for modelling  --------------
write.csv(food100, "output/food_per100g_clean.csv", row.names = FALSE)
cat("Done:", nrow(food100), "foods saved\n")   # 2198

##########

df <- step3

### Target

targets <- minerals

### Test/Train splits 70:30

set.seed(321)

tr_df_filtered <- sample(1:nrow(df), size = 0.7 * nrow(df))
train_df_filtered <- df[tr_df_filtered, ]
test_df_filtered <- df[-tr_df_filtered, ]

names(train_df_filtered) <- make.names(names(train_df_filtered))
names(test_df_filtered) <- make.names(names(test_df_filtered))  

macros <- make.names(macros)

### kfolds value

k_folds <- 8


### Linear Model

lm_models <- list()
performance <- list()

for (target in targets){
  
  tr <- train_df_filtered
  te <- test_df_filtered
  tr[[target]] <- log1p(tr[[target]])
  te[[target]] <- log1p(te[[target]])
  
  predictors <- setdiff(macros, target)
  
  set.seed(321)
  fold <- createFolds(tr[[target]], k = k_folds, returnTrain = TRUE)
  ctr1 <- trainControl(method = 'cv', index = fold)
  
  lmCTRL <- train(reformulate(predictors, response = target), data = tr,method = 'lm',
    preProcess = c('zv', 'YeoJohnson'), trControl = ctr1)
  
  lm_models[[target]] <- lmCTRL
  
  ### Error metrics
  tr_perf <- getTrainPerf(lmCTRL)[, c('TrainRMSE', 'TrainRsquared', 'TrainMAE')]
  
  ### Log scale
  te_perf <- defaultSummary(data.frame(obs  = te[[target]],pred = predict(lmCTRL, te)))
  names(te_perf) <- paste0('Test', names(te_perf))
  
  ### Back to original scale
  te_orig <- defaultSummary(data.frame(obs  = expm1(te[[target]]), pred = expm1(predict(lmCTRL, te))))
  names(te_orig) <- paste0('TestOrig', names(te_orig))
  performance[[target]] <- cbind(tr_perf, t(te_perf), t(te_orig))
  }

performance_table <- do.call(rbind, performance)
round(performance_table,3)

### Lasso

lasso_models <- list()
performance_lasso <- list()

for (target in targets){
  
  tr <- train_df_filtered
  te <- test_df_filtered
  tr[[target]] <- log1p(tr[[target]])
  te[[target]] <- log1p(te[[target]])
  
  predictors <- setdiff(macros, target)
  
  set.seed(321)
  fold <- createFolds(tr[[target]], k = k_folds, returnTrain = TRUE)
  ctr1 <- trainControl(method = 'cv', index = fold)
  
  lassoGrid = expand.grid(alpha = 1, lambda = 10^seq(-3, 1, length = 30)) ### Check params later
  
  LglmnetCTRL <- train(reformulate(predictors, response = target), data = tr,
                       method = 'glmnet', preProcess = c('YeoJohnson','zv'),
                       trControl = ctr1, tuneGrid = lassoGrid)
  
  lasso_models[[target]] <- LglmnetCTRL
  
  ### Error metrics
  tr_perf <- getTrainPerf(LglmnetCTRL)[, c('TrainRMSE', 'TrainRsquared', 'TrainMAE')]
  
  ### Log scale
  te_perf <- defaultSummary(data.frame(obs  = te[[target]],pred = predict(LglmnetCTRL, te)))
  names(te_perf) <- paste0('Test', names(te_perf))
  
  ### Back to original scale
  te_orig <- defaultSummary(data.frame(obs  = expm1(te[[target]]), pred = expm1(predict(LglmnetCTRL, te))))
  names(te_orig) <- paste0('TestOrig', names(te_orig))
  
  performance_lasso[[target]] <- cbind(tr_perf, t(te_perf), t(te_orig))
  }

performance_table_lasso <- do.call(rbind, performance_lasso)
round(performance_table_lasso,3)

### Elastic Net

Enet_models <- list()
performance_enet <- list()

for (target in targets){
  
  tr <- train_df_filtered
  te <- test_df_filtered
  tr[[target]] <- log1p(tr[[target]])
  te[[target]] <- log1p(te[[target]])
  
  predictors <- setdiff(macros, target)
  
  set.seed(321)
  fold <- createFolds(tr[[target]], k = k_folds, returnTrain = TRUE)
  ctr1 <- trainControl(method = 'cv', index = fold)
  
  ENglmnetCTRL <- train(reformulate(predictors, response = target), data = tr, ### Add parameter grids
                        method = 'glmnet', preProcess = c('YeoJohnson','zv'),
                        trControl = ctr1)
  Enet_models[[target]] <- ENglmnetCTRL
  
  ### Error metrics
  tr_perf <- getTrainPerf(ENglmnetCTRL)[, c('TrainRMSE', 'TrainRsquared', 'TrainMAE')]
  
  ### Log scale
  te_perf <- defaultSummary(data.frame(obs  = te[[target]],pred = predict(ENglmnetCTRL, te)))
  names(te_perf) <- paste0('Test', names(te_perf))
  
  ### Back to original scale
  te_orig <- defaultSummary(data.frame(obs  = expm1(te[[target]]), pred = expm1(predict(ENglmnetCTRL, te))))
  names(te_orig) <- paste0('TestOrig', names(te_orig))
  
  performance_enet[[target]] <- cbind(tr_perf, t(te_perf), t(te_orig))
}

performance_table_enet <- do.call(rbind, performance_enet)
round(performance_table_enet,3)

### KNN

knnGrid <- data.frame(k = seq(3, 31, by = 2))

KNN_models <- list()
performance_KNN <- list()

for (target in targets){
  
  tr <- train_df_filtered
  te <- test_df_filtered
  tr[[target]] <- log1p(tr[[target]])
  te[[target]] <- log1p(te[[target]])
  
  predictors <- setdiff(macros, target)
  
  set.seed(321)
  fold <- createFolds(tr[[target]], k = k_folds, returnTrain = TRUE)
  ctr1 <- trainControl(method = 'cv', index = fold)
  
  knnTune <- train(reformulate(predictors, response = target), data = tr,
                   method = 'knn', tuneGrid = knnGrid,
                   preProcess = c('YeoJohnson', 'zv', 'center', 'scale'),
                   trControl = ctr1)
 
  KNN_models[[target]] <- knnTune  
  tr_perf <- getTrainPerf(knnTune)[, c('TrainRMSE', 'TrainRsquared', 'TrainMAE')]
  
  # Predict log scale
  
  pred_log <- predict(knnTune, te)
  te_perf <- defaultSummary(data.frame(obs = te[[target]], pred = pred_log))
  names(te_perf) <- paste0('Test', names(te_perf))
  
  # Predict orignal scale
  
  te_orig <- defaultSummary(data.frame(obs = expm1(te[[target]]), pred = expm1(pred_log)))
  names(te_orig) <- paste0('TestOrig', names(te_orig))
  
  performance_KNN[[target]] <- cbind(tr_perf, t(te_perf), t(te_orig),K = knnTune$bestTune$k)
  }
performance_table_KNN <- do.call(rbind, performance_KNN)
round(performance_table_KNN,3)

### Support vector machines linear

svmGrid <- expand.grid(C = 2^seq(-5, 5, by = 1))

SVMl_models <- list()
performance_SVMl <- list()

for (target in targets){
  
  tr <- train_df_filtered
  te <- test_df_filtered
  tr[[target]] <- log1p(tr[[target]])
  te[[target]] <- log1p(te[[target]])
  
  predictors <- setdiff(macros, target)
  
  set.seed(321)
  fold <- createFolds(tr[[target]], k = k_folds, returnTrain = TRUE)
  ctr1 <- trainControl(method = 'cv', index = fold)
  
  SVMLinear <- train(reformulate(predictors, response = target), data = tr,
                     method = 'svmLinear', tuneLength = 10,
                     preProcess = c('YeoJohnson','zv', 'center', 'scale'),
                     trControl = ctr1, grid = svmGrid)
  
  SVMl_models[[target]] <- SVMLinear  
  tr_perf <- getTrainPerf(SVMLinear)[, c('TrainRMSE', 'TrainRsquared', 'TrainMAE')]
  
  # Log predict
  
  pred_log <- predict(SVMLinear, te)
  te_perf <- defaultSummary(data.frame(obs = te[[target]], pred = pred_log))
  names(te_perf) <- paste0('Test', names(te_perf))
  
  # Original scale
  
  te_orig <- defaultSummary(data.frame(obs = expm1(te[[target]]), pred = expm1(pred_log)))
  names(te_orig) <- paste0('TestOrig', names(te_orig))
  
  performance_SVMl[[target]] <- cbind(tr_perf, t(te_perf), t(te_orig), C = SVMLinear$bestTune$C)
}

performance_table_SVMl <- do.call(rbind, performance_SVMl)
round(performance_table_SVMl,3)

### Random Forest

RF_models <- list()
performance_RF <- list()

for (target in targets){
  
  tr <- train_df_filtered
  te <- test_df_filtered
  tr[[target]] <- log1p(tr[[target]])
  te[[target]] <- log1p(te[[target]])
  
  predictors <- setdiff(macros, target)
  
  set.seed(321)
  fold <- createFolds(tr[[target]], k = k_folds, returnTrain = TRUE)
  ctr1 <- trainControl(method = 'cv', index = fold)
  
  p <- length(predictors)
  tunegrid <- expand.grid(mtry = sort(unique(c(2, floor(p / 2), p))))
  
  
  RF <- train(reformulate(predictors, response = target), data = tr,
                     method = 'rf', ntree = 50,
                     preProcess = c('zv'),
                     trControl = ctr1, tuneGrid = tunegrid)
  
  RF_models[[target]] <- RF  
  tr_perf <- getTrainPerf(RF)[, c('TrainRMSE', 'TrainRsquared', 'TrainMAE')] 
  
  # Log predict
  
  pred_log <- predict(RF, te)
  te_perf <- defaultSummary(data.frame(obs = te[[target]], pred = pred_log))
  names(te_perf) <- paste0('Test', names(te_perf))
  
  # Original scale
  
  te_orig <- defaultSummary(data.frame(obs = expm1(te[[target]]), pred = expm1(pred_log)))
  names(te_orig) <- paste0('TestOrig', names(te_orig))
  
  performance_RF[[target]] <- cbind(tr_perf, t(te_perf), t(te_orig), mtry = RF$bestTune$mtry)
    }

performance_table_RF <- do.call(rbind, performance_RF)
round(performance_table_RF,3)

### Category addition


py_install("sentence-transformers", pip = TRUE)

st <- import("sentence_transformers")
model <- st$SentenceTransformer('all-mpnet-base-v2')

embeddings <- model$encode(df$food)

dist_matrix <- proxy::dist(embeddings, method = "cosine")

hc <- hclust(dist_matrix, method = "average")

cluster_labels <- cutree(hc, h = 0.70)

df$group <- cluster_labels

df$group <- as.factor(df$group)
