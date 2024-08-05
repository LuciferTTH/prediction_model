#### Set the working directory path ####
rm(list = ls())
setwd('C:/Users/Tao/Desktop/KNN')

#### Load the packages ####
pkgs <- c('dplyr', 'ggplot2', 'class', 'caret', 'pROC', 'Hmisc')
# install.packages(pkgs)
for (i in pkgs) library(i, character.only = TRUE)

#### Load the data ####
load('.RData')
load(file = 'simulated_dataset.R')
head(dataset_final)
str(dataset_final)

#### Step 1: Clean and feature scaling ####
##### Clean the data #####
names(dataset_final) <- 
    c('ID', 'age', 'male', 'BMI', 'SBP', 'MI', 'HF', 'COPD', 
    'cancer', 'albuminuria', 'TC', 'LDLC', 'No_outpatient', 'No_inpatient', 
    'liver_disease', 'hypoglycemia', 'CKD_stage', 
    'AKI_time', 'AKI_status', 'AKI_binary', 'type')

dataset_final <- dataset_final %>%
        mutate(
        albuminuria = case_when( ## relabel of categorical variable
            albuminuria == 1 ~ "normal to mild",
            albuminuria == 2 ~ "moderate",
            albuminuria == 3 ~ "severe"),
        albuminuria = factor(albuminuria, levels = c("normal to mild", "moderate", "severe")),
        CKD_stage = case_when(
            CKD_stage == 1 ~ "G1-2",
            CKD_stage == 2 ~ "G3a",
            CKD_stage == 3 ~ "G3b",
            CKD_stage == 4 ~ "G4"),
        CKD_stage = factor(CKD_stage, levels = c("G1-2", "G3a", "G3b", "G4")))


data <- dataset_final %>% filter(type == "raw")
data <- data[, c('AKI_binary', 'age', 'male', 'BMI', 'SBP', 'MI', 'HF', 'COPD', 
                 'cancer', 'albuminuria', 'TC', 'LDLC', 'No_outpatient', 'No_inpatient', 
                 'liver_disease', 'hypoglycemia', 'CKD_stage')]
data$AKI_binary <- as.factor(data$AKI_binary)

data_external <- dataset_final %>% filter(type == "external")
data_external <- data_external[, c('AKI_binary', 'age', 'male', 'BMI', 'SBP', 'MI', 'HF', 'COPD', 
                          'cancer', 'albuminuria', 'TC', 'LDLC', 'No_outpatient', 'No_inpatient', 
                          'liver_disease', 'hypoglycemia', 'CKD_stage')]
data_external$AKI_binary <- as.factor(data_external$AKI_binary)

##### Feature scaling #####
data <- 
    data %>% 
    mutate(
        AKI_binary = as.factor(AKI_binary),
        male = as.numeric(male),
        MI = as.numeric(MI),
        HF = as.numeric(HF),
        COPD = as.numeric(COPD),
        cancer = as.numeric(cancer),
        albuminuria = as.numeric(albuminuria) - 1,
        liver_disease = as.numeric(liver_disease),
        hypoglycemia = as.numeric(hypoglycemia),
        CKD_stage = as.numeric(CKD_stage) - 1
    )

# scaling
data_scaled <- as.data.frame(scale(data[, -1]))
data_scaled <- cbind(AKI_binary = data$AKI_binary, data_scaled)
str(data_scaled)
levels(data_scaled$AKI_binary) <- c('no', 'yes')

# normalizing
normalized <- function(x){
    return((x - min(x)) / (max(x) - min(x)))
}
data_normalized <- as.data.frame(lapply(data[, -1], normalized))
data_normalized <- cbind(AKI_binary = data$AKI_binary, data_normalized)
str(data_normalized)
levels(data_normalized$AKI_binary) <- c('no', 'yes')

data_external <- 
    data_external %>% 
    mutate(
        AKI_binary = as.factor(AKI_binary),
        male = as.numeric(male),
        MI = as.numeric(MI),
        HF = as.numeric(HF),
        COPD = as.numeric(COPD),
        cancer = as.numeric(cancer),
        albuminuria = as.numeric(albuminuria) - 1,
        liver_disease = as.numeric(liver_disease),
        hypoglycemia = as.numeric(hypoglycemia),
        CKD_stage = as.numeric(CKD_stage) - 1
    )

# scaling
data_external_scaled <- as.data.frame(scale(data_external[, -1]))
data_external_scaled <- cbind(AKI_binary = data_external$AKI_binary, data_external_scaled)
levels(data_external_scaled$AKI_binary) <- c('no', 'yes')


# normalizing
normalized <- function(x){
    return((x - min(x)) / (max(x) - min(x)))
}
data_external_normalized <- as.data.frame(lapply(data_external[, -1], normalized))
data_external_normalized <- cbind(AKI_binary = data_external$AKI_binary, data_external_normalized)
str(data_external_normalized)
levels(data_external_normalized$AKI_binary) <- c('no', 'yes')



##### Randome split #####
# set.seed(123)
# id <- sample(seq_len(nrow(data_normalized)), size = 0.7 * nrow(data_normalized))
# train <- data_normalized[id, ]
# test <- data_normalized[-id, ]

set.seed(123)
id <- sample(seq_len(nrow(data_normalized)), size = 0.7 * nrow(data_normalized))
train <- data_normalized[id, ]
test <- data_normalized[-id, ]

#### Step 2: Model fitting ####
KNN_pred <- 
    knn(
        train = train[, -1], 
        test = test[, -1], 
        cl = train$AKI_binary, 
        k = ceiling(sqrt(nrow(train))), 
        prob = TRUE 
    )
pred_prob <- attr(KNN_pred, 'prob')
pred_prob <- ifelse(KNN_pred == 'yes', pred_prob, 1-pred_prob)

# confusion matrix
cm <- table(actual = test$AKI_binary, predicted = KNN_pred)
cm

# AUC 
roc <- roc(as.factor(as.numeric(test$AKI_binary) - 1), pred_prob)
AUC <- auc(roc)
AUC

plot(roc$specificities, 
     roc$sensitivities, 
     lwd = 2, col = "tomato", 
     xlim = c(1, 0), ylim = c(0, 1),
     xaxs = 'i', 
     yaxs = 'i',
     bty = ']')
lines(roc$specificities, 
      roc$sensitivities,
      col = "tomato")
abline(1, -1, lty = 2, col = 'blue')
legend("right", 
       legend = paste("AUC: ", round(AUC, 3)), 
       col = "tomato", 
       lwd = 2, 
       bty = "n")

#### Step 3: Model tuning ####
##### Random split #####
# using the for loop 
results <- data.frame()
for(k in 1:300) {
    KNN_fit <-
        knn(
            train = train[, -1],
            test = test[, -1],
            cl = train$AKI_binary,
            k = k,
            prob = TRUE
        )
    pred_prob <- attr(KNN_fit, 'prob')
    pred_prob <- ifelse(KNN_fit == 'yes', pred_prob, 1-pred_prob)

    # AUC 
    roc <- roc(as.factor(as.numeric(test$AKI_binary) - 1), pred_prob)
    AUC <- auc(roc)

    result <- data.frame(k = k, AUC = AUC)
    results <- rbind(results, result)
}

plot(results, 
    type = "b", 
    col = "dodgerblue", 
    cex = 1, 
    pch = 20, 
    xlab = "k, number of neighbors", 
    ylab = "AUC",
    main = "Tuning for k: AUC vs Neighbors")
# add line for max AUC seen
abline(h = max(results$AUC), col = "darkorange", lty = 3)
abline(v = results$k[results$AUC == max(results$AUC)], col = "darkorange", lty = 3)

##### Cross-validation #####
# Using caret package 
ctrl <- trainControl(
            method = "cv",
            classProbs = TRUE,
            summaryFunction = twoClassSummary
        )
KNN_tune <- train(
                AKI_binary ~ ., 
                data = data_normalized, 
                method = "knn", 
                metric = "ROC",
                trControl = ctrl, 
                tuneGrid = data.frame(k = c(1:300))
		    )

plot(KNN_tune$results[, c('k', 'ROC')], 
    type = "b", 
    col = "dodgerblue", 
    cex = 1, 
    pch = 20, 
    xlab = "k, number of neighbors", 
    ylab = "AUC",
    main = "Tuning for k: AUC vs Neighbors")
# add line for max AUC seen
abline(h = max(KNN_tune$results$ROC), col = "darkorange", lty = 3)
abline(v = KNN_tune$results$k[KNN_tune$results$ROC == max(KNN_tune$results[['ROC']])], col = "darkorange", lty = 3)

#### Step 4: Model refit ####
##### Random split ######
best_model_k_split <- results$k[results$AUC == max(results$AUC)]
best_model_k_split
best_model_split <-
        knn(
            train = data_normalized[, -1],
            test = data_normalized[, -1],
            cl = data_normalized$AKI_binary,
            k = best_model_k_split,
            prob = TRUE
        )
pred_prob_split <- attr(best_model_split, 'prob')
pred_prob_split <- 
    ifelse(
        best_model_split == 'yes', 
        pred_prob_split, 
        1 - pred_prob_split
    )

# AUC 
roc_split <- roc(as.factor(
                    as.numeric(data_normalized$AKI_binary)- 1), 
                pred_prob_split)
AUC_split <- auc(roc_split)
AUC_split

##### Cross-validation #####
best_model_k_cv <- KNN_tune$bestTune$k
best_model_cv <- 
    knn3(
        AKI_binary ~ .,
        data = data_normalized,
        k = best_model_k_cv
    )

predictions_cv <- predict(best_model_cv, data_normalized, type = "class")
pred_prob_cv <- predict(best_model_cv, data_normalized, type = "prob")
pred_prob_cv <- pred_prob_cv[,2]

# AUC 
roc_cv <- roc(as.factor(
                as.numeric(data_normalized$AKI_binary) - 1), 
              pred_prob_cv)
AUC_cv <- auc(roc_cv)
AUC_cv


#### Step 5: Model evaluation in external data ####
##### Random split #####
best_model_external <-
        knn(
            train = data_normalized[, -1],
            test = data_external_normalized[, -1],
            cl = data_normalized$AKI_binary,
            k = best_model_k_split,
            prob = TRUE
        )
pred_prob <- attr(best_model_external, 'prob')
pred_prob <- ifelse(best_model_external == 'yes', pred_prob, 1-pred_prob)

# confusion matrix
cm <- table(actual = data_external_normalized$AKI_binary, predicted = best_model_external)
cm

# AUC 
roc <- roc(data_external$AKI_binary, pred_prob)
AUC <- auc(roc)
AUC

plot(1 - roc$specificities, 
     roc$sensitivities,
     pch = 20,
     col = "tomato", 
     xlim = c(0, 1), ylim = c(0, 1),
     xlab = '1-Sepcificity',
     ylab = 'Sensitivity',
     xaxs = 'i', 
     yaxs = 'i')
lines(1- roc$specificities, 
      roc$sensitivities,
      col = "tomato")
abline(0, 1, lty = 2, col = 'blue')
legend("right", 
       legend = paste("AUC: ", round(AUC, 3)), 
       col = "tomato", 
       lwd = 2, 
       bty = "n")

png(filename = 'roc_split.png', width = 1600, height = 1600, unit = 'px', res = 300)
plot(1 - roc$specificities, 
     roc$sensitivities,
     pch = 20,
     col = "tomato", 
     xlim = c(0, 1), ylim = c(0, 1),
     xlab = '1-Sepcificity',
     ylab = 'Sensitivity',
     xaxs = 'i', 
     yaxs = 'i')
lines(1- roc$specificities, 
      roc$sensitivities,
      col = "tomato")
abline(0, 1, lty = 2, col = 'blue')
legend("right", 
       legend = paste("AUC: ", round(AUC, 3)), 
       col = "tomato", 
       lwd = 2, 
       bty = "n")
dev.off()

# calibration curve
n_group <- 10
cal_data <- data.frame(AKI_binary = as.numeric(data_external$AKI_binary) - 1, pred_prob)
cal_data <- cal_data[order(cal_data$pred_prob),]
cal_data <- cal_data %>% mutate(tiles = cut2(pred_prob, g = n_group))
cal_data <- cal_data %>% 
            group_by(tiles) %>% 
            summarise(
                n = n(),
                obs_mean = mean(AKI_binary),
                obs_sd = sd(AKI_binary),
                # obs_mean = sum(AKI_binary) / n(),
                # obs_sd = sqrt(obs_mean * (1 - obs_mean)),
                pred_mean = mean(pred_prob),
                obs_upper = obs_mean + 1.96*obs_sd/sqrt(n()),
                obs_lower = obs_mean - 1.96*obs_sd/sqrt(n())
            )

cal_plot <- 
    ggplot(
        cal_data, 
        aes(x = pred_mean, y = obs_mean)) +
    # geom_line() +
    geom_point(size = 5, colour = 'darkblue') +
    geom_errorbar(
        aes(ymin = obs_lower, ymax = obs_upper), 
        linewidth = 1,
        width = .005, 
        colour = 'darkblue'
    ) +
    geom_abline(linewidth = 1) +
    xlab("\nPredicted incidence") +
    ylab("Observed incidence\n") +
    scale_x_continuous(limits = c(0, 0.25), breaks = seq(0, 0.5, 0.05)) +
    scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, 0.5, 0.05)) +
    theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        # panel.grid.major.y = element_line(color = "gray"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0), 
        axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20, color = 'black'),
        axis.ticks.length.x = unit(0.3, 'cm'),
        axis.ticks.length.y = unit(0.3, 'cm'),
        axis.line = element_line(colour = "black", linewidth = 0.5)
    )
cal_plot

png(filename = 'calibration_curve_split.png', width = 3200, height = 3200, unit = 'px', res = 300)
cal_plot
dev.off()

##### Cross-validation #####
best_model_external <-
        knn(
            train = data_normalized[, -1],
            test = data_external_normalized[, -1],
            cl = data_normalized$AKI_binary,
            k = best_model_k_cv,
            prob = TRUE
        )
pred_prob <- attr(best_model_external, 'prob')
pred_prob <- ifelse(best_model_external == 'yes', pred_prob, 1-pred_prob)

# confusion matrix
cm <- table(actual = data_external$AKI_binary, predicted = best_model_external)
cm

# AUC 
roc <- roc(data_external$AKI_binary, pred_prob)
AUC <- auc(roc)
AUC

plot(1 - roc$specificities, 
     roc$sensitivities,
     pch = 20,
     col = "tomato", 
     xlim = c(0, 1), ylim = c(0, 1),
     xlab = '1-Sepcificity',
     ylab = 'Sensitivity',
     xaxs = 'i', 
     yaxs = 'i')
lines(1- roc$specificities, 
      roc$sensitivities,
      col = "tomato")
abline(0, 1, lty = 2, col = 'blue')
legend("right", 
       legend = paste("AUC: ", round(AUC, 3)), 
       col = "tomato", 
       lwd = 2, 
       bty = "n")

png(filename = 'roc_cv.png', width = 1600, height = 1600, unit = 'px', res = 300)
plot(1 - roc$specificities, 
     roc$sensitivities,
     pch = 20,
     col = "tomato", 
     xlim = c(0, 1), ylim = c(0, 1),
     xlab = '1-Sepcificity',
     ylab = 'Sensitivity',
     xaxs = 'i', 
     yaxs = 'i')
lines(1- roc$specificities, 
      roc$sensitivities,
      col = "tomato")
abline(0, 1, lty = 2, col = 'blue')
legend("right", 
       legend = paste("AUC: ", round(AUC, 3)), 
       col = "tomato", 
       lwd = 2, 
       bty = "n")
dev.off()

# calibration curve
n_group <- 10
cal_data <- data.frame(AKI_binary = as.numeric(data_external$AKI_binary) - 1, pred_prob)
cal_data <- cal_data[order(cal_data$pred_prob),]
cal_data <- cal_data %>% mutate(tiles = cut2(pred_prob, g = n_group))
cal_data <- cal_data %>% 
            group_by(tiles) %>% 
            summarise(
                n = n(),
                obs_mean = mean(AKI_binary),
                obs_sd = sd(AKI_binary),
                # obs_mean = sum(AKI_binary) / n(),
                # obs_sd = sqrt(obs_mean * (1 - obs_mean)),
                pred_mean = mean(pred_prob),
                obs_upper = obs_mean + 1.96*obs_sd/sqrt(n()),
                obs_lower = obs_mean - 1.96*obs_sd/sqrt(n())
            )

cal_plot <- 
    ggplot(
        cal_data, 
        aes(x = pred_mean, y = obs_mean)) +
    # geom_line() +
    geom_point(size = 5, colour = 'darkblue') +
    geom_errorbar(
        aes(ymin = obs_lower, ymax = obs_upper), 
        linewidth = 1,
        width = .005, 
        colour = 'darkblue'
    ) +
    geom_abline(linewidth = 1) +
    xlab("\nPredicted incidence") +
    ylab("Observed incidence\n") +
    scale_x_continuous(limits = c(0, 0.25), breaks = seq(0, 0.5, 0.05)) +
    scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, 0.5, 0.05)) +
    theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        # panel.grid.major.y = element_line(color = "gray"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0), 
        axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20, color = 'black'),
        axis.ticks.length.x = unit(0.3, 'cm'),
        axis.ticks.length.y = unit(0.3, 'cm'),
        axis.line = element_line(colour = "black", linewidth = 0.5)
    )
cal_plot

png(filename = 'calibration_curve_cv.png', width = 3200, height = 3200, unit = 'px', res = 300)
cal_plot
dev.off()
