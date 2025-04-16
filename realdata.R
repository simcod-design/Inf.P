
set.seed(96882)

library(GDINA)
library(dplyr)
library(RootsExtremaInflections)

dat=as.data.frame(srs[,3:22])
Qc=as.data.frame(qmatrix)

est=GDINA(dat=dat,Q=Qc,model="GDINA",sequential = TRUE,mono.constraint = TRUE,
                 att.dist="saturated",att.str = NULL,verbose=2,linkfunc = NULL,
                 control=list(conv.crit=0.00001))
modelfit(est, CI=0.90)
delt=coef(est,what="delta")
est.prob=round(personparm(est, what="mp"),4)
raw.scores=data.frame(matrix(0, nrow = nrow(dat), ncol =4))
colnames(raw.scores) <- c("A1.raw", "A2.raw", "A3.raw", "A4.raw")
raw.scores$A1.raw = rowSums(dat[, c(5, 9, 12, 15, 18)])  # Function
raw.scores$A2.raw = rowSums(dat[, c(1, 2, 8, 11, 17)])  # Pain
raw.scores$A3.raw = rowSums(dat[, c(4, 6, 10, 14, 19)])  # Self image
raw.scores$A4.raw = rowSums(dat[, c(3, 7, 13, 16, 20)])  # Mental Health
data=data.frame(srs[-193,2],raw.scores[-193,],est.prob[-193,])

inflection_points <- list()
variabless <- c(paste0("A", seq(4)))

for (var in variabless) {
  x <- data[[paste0(var, ".raw")]]
  y <- data[[var]]
  
  model <- lm(y ~ poly(x, 3, raw = TRUE))
  d <- inflexi(x, predict(model), 1, length(x), 5, 5, plots = TRUE)
  inflection_points[[var]] <- d$finfl[2]
  data[[paste0(var, ".raw.grp")]] <- ifelse(data[[paste0(var, ".raw")]] > inflection_points[[var]], 0,1)
  
  }
  

inf.point1 <- data.frame(
  inf.point = unlist(inflection_points)
)

print(inf.point1)


