##First question
fair <- read.csv("D:/jupyter_code/R/fair5.csv", header = TRUE, sep = ",")
plot(
  fair$growth,
  fair$vote,
  xlab = "growth",
  ylab = "vote",
  pch = 19,
  col = "blue"
)

plot(
  fair$inflat,
  fair$vote,
  xlab = "inflat",
  ylab = "vote",
  pch= 19,
  col = "green"
)
fair_re = fair[, c("vote","growth","inflat")]
model1 = lm(data = fair_re, vote ~ growth + inflat)
print(summary(model1))

##Sencond and third question

data_demo = data.frame(
  growth = c(3),
  inflat = c(3)
) 

data_rep = data.frame(
  growth = c(-3),
  inflat = c(-3)
) 

prediction_demo = predict(model1, newdata = data_demo)
prediction_rep = predict(model1, newdata = data_rep)
print(c(prediction_demo, prediction_rep))

##Fourth question

fair_updated <- read.csv("D:/jupyter_code/R/fair2020.csv", header = TRUE, sep = ",")
fair_re2 = fair_updated[, c("vote","growth","inflat")]
tail(fair_re2)
model2 = lm(data = fair_re2, vote~growth+inflat)
print(summary(model2))

##Fifth question

data_five = data.frame(
  growth = c(3.24),
  inflat = c(4.85)
)
prediction_point = predict(model2, newdata = data_five)
prediction_interval5 = predict(model2,
                              newdata = data_five,
                              interval = "prediction",
                              level = 0.95)
prediction_interval1 = predict(model2,
                              newdata = data_five,
                              interval = "prediction",
                              level = 0.90)
print(prediction_point)
print(prediction_interval5)
print(prediction_interval1)