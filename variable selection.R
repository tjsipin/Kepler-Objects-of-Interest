x = model.matrix(koi_disposition ~ . - koi_pdisposition, koi)[,-1]
y = as.factor(koi$koi_disposition)

x.train = x[train,]
y.train = y[train]
x.test = x[-train,]
y.test = y[-train]

grid = 10^seq(10, -2, length = 100)
ridge_mod = glmnet(x, y, alpha = 0, lambda = grid, family = "multinomial")

cv.out.ridge = cv.glmnet(x.train, y.train, alpha=0, family = "multinomial")
plot(cv.out.ridge)
abline(v = log(cv.out.ridge$lambda.min), col = "red", lwd = 3, lty = 2)
bestlam = cv.out.ridge$lambda.min
bestlam


out = glmnet(x,y,alpha = 0, family = "multinomial")
predict(out, type = "coefficients", s = bestlam)[1:3]


# lasso

lasso.mod <- glmnet(x.train, y.train, alpha = 1, lambda = grid, family = "multinomial")
plot(lasso.mod,xvar = "lambda", label = TRUE)

cv.out.lasso = cv.glmnet(x.train, y.train, alpha = 1, family = "multinomial")
plot(cv.out.lasso)
abline(v = log(cv.out.lasso$lambda.min), col="red", lwd=3, lty=2)
bestlam = cv.out.lasso$lambda.min
lasso.pred = predict(lasso.mod, s = bestlam, newx = x[-train,])

out = glmnet(x, y, alpha = 1, lambda = grid, family = "multinomial")
lasso.coef = predict(out, type = "coefficients", s=bestlam)
lasso.coef

# HELP



# CV in e1071

set.seed(1)
tune.out = tune(svm, y~., data= koi, kernel="linear",
                ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))



# SVM
set.seed(1)
svmfit = svm(y~., data = koi, kernel = "linear", cost = 10, scale = TRUE)

plot(svmfit, symbolPalette = terrain.colors(2))

