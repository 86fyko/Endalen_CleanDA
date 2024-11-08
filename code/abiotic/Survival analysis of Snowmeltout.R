library(survival)

# Create a survival object
survival_data <- survfit(Surv(time_variable, event_variable) ~ treatment_variable, data = your_data)

# Plot survival curves
plot(survival_data, xlab = "Time", ylab = "Survival Probability", main = "Survival Curves")
