
set.seed(123456) 


library(GDINA)
library(OptimalCutpoints)
library(dplyr)
library(RootsExtremaInflections)
path <- file.path(Sys.getenv("USERPROFILE"), "Desktop")
path <- gsub("\\\\", "/", path)

#### Generating response matrix 
r=1000 #repeat
result=matrix(0,4*r,16) 
resultss <- NULL

n.att<- c(3) 
n.item<- c(10,15,20,25,30) #5
n.sample <- c(30,50,100,150,200,250,300,350,400,450,500,1000) #12


for(a in 1: length(n.att)){
for (t in 1:length(n.item)){
    for (p in 1:length(n.sample)){
      
      file_namee <- paste0(path,"/condition_", n.att[a], "_", n.item[t], "_", n.sample[p])
      dir.create(file_namee)
      
      s<-1
      while (s<=r) {
        tryCatch ({
      cat("sim.step:", s, "\n")
    
      matrix_name <- vector("character", n.att[a])
      for (i in 1:n.att[a]){
        matrix_name[i]=paste0("mat",i)
        pattern <- rep(0,n.att[a])
        pattern[i]<-1
        assign(matrix_name[i], matrix(c(rep(pattern, times=(4*n.item[t]))),
                    nrow=(4*n.item [t]),ncol=n.att[a],byrow=TRUE))
        }
   
     Qc<- data.frame(item=rep(1:(n.item[t]*n.att[a]), each = 4),
                cat=rep(1:4, times = (n.item[t]*n.att[a])),
                A= do.call(rbind, mget(matrix_name)))

N0 <-  n.sample[p]  #group 0
d0<- list()
for (i in 1:(4*n.att[a]*n.item[t])) {  
  IDI.0 <- runif(1, 0.20, 0.40)  
  d0[[i]] <- c(0.10, IDI.0)  
}

N1 <-  n.sample[p]  #group 1
d1<- list()
for (i in 1:(4*n.att[a]*n.item[t])) {  
  IDI.1 <- runif(1, 0.60, 0.80)  
  d1[[i]] <- c(0.10, IDI.1)  
}


sim0<-  simGDINA(N0,Qc,gs.parm = NULL,delta.parm = d0,catprob.parm = NULL,
                 model = "GDINA",sequential = TRUE)

sim1 <- simGDINA(N1,Qc,gs.parm = NULL,delta.parm = d1,catprob.parm = NULL,
                 model = "GDINA",sequential = TRUE)

dat <- rbind(extract(sim0,what="dat"),extract(sim1,what="dat"))
grp <- rep(c(0,1),c(N0,N1))

#### raw scores 
raw.scores=data.frame(matrix(0, nrow = nrow(dat), ncol = n.att[a]))
for (i in 1:n.att[a]){
  raw.scores[, i]=rowSums(dat[,((i-1)*n.item[t]+1):(i*n.item[t])])
}

colnames(raw.scores) <- paste0("A", 1:n.att[a], ".raw")

#### GDINA model estimates
est=GDINA::GDINA(dat=dat,Q=Qc,model="GDINA",sequential = TRUE,mono.constraint = TRUE,
                 att.dist="saturated",att.str = NULL,verbose=2,linkfunc = NULL,
                 control=list(conv.crit=0.00001))

est.prob=round(personparm(est, what="mp"),4)

est.result = data.frame(est.prob)
all.results=data.frame(grp,raw.scores,est.result)


#### Youden cut-off
data=all.results
n.total=count(data)
n.0=count(data,group=grp)[1,2]
n.1=count(data,group=grp)[2,2]   
roc_results <- list()
variables <- c(paste0("A", seq(n.att[a])))
for (var in variables) {
  o.cut <- optimal.cutpoints(X = paste0(var, ".raw"), status = "grp", tag.healthy = 0, 
                             methods = "Youden", data = data, pop.prev = NULL, 
                             categorical.cov = NULL, 
                             control = control.cutpoints(), ci.fit = FALSE, 
                             conf.level = 0.95, trace = FALSE)
  
    roc_results[[var]]=c(auc=o.cut$Youden$Global$measures.acc$AUC[1],
                       auc.ll=o.cut$Youden$Global$measures.acc$AUC[2],
                       auc.ul=o.cut$Youden$Global$measures.acc$AUC[3],
                       cut.off=o.cut$Youden$Global$optimal.cutoff$cutoff[1],
                       sen=o.cut$Youden$Global$optimal.cutoff$Se[1],
                       sp=o.cut$Youden$Global$optimal.cutoff$Sp[1],
                       acc=(((n.1-o.cut$Youden$Global$optimal.cutoff$FN[1])+
                              (n.0-o.cut$Youden$Global$optimal.cutoff$FP[1]))/n.total))
}

roc.youden <- do.call(rbind, roc_results)


#### Proposed method cut-off -- Inf.P cut off
inflection_points <- list()
sen.inf<- list()
sp.inf<- list()
acc.inf<- list()
variabless <- c(paste0("A", seq(n.att[a])))

for (var in variabless) {
  x <- data[[paste0(var, ".raw")]]
  y <- data[[var]]
  
  model <- lm(y ~ poly(x, n.att[a], raw = TRUE))
  d <- inflexi(x, predict(model), 1, length(x), 5, 5, plots = FALSE)
  inflection_points[[var]] <- d$finfl[2]
  data[[paste0(var, ".raw.grp")]] <- ifelse(data[[paste0(var, ".raw")]] < inflection_points[[var]], 0, 1)
 
  conf_matrix <- table(data[[paste0(var, ".raw.grp")]], data$grp)
  if (nrow(conf_matrix) < 2 || ncol(conf_matrix) < 2) {
    complete_matrix <- matrix(1, nrow = 2, ncol = 2, dimnames = list(c("0", "1"), c("0", "1")))
    complete_matrix[rownames(conf_matrix), colnames(conf_matrix)] <- conf_matrix
    conf_matrix <- complete_matrix
  }
  
  sen.inf[[var]] <- conf_matrix[2, 2] / sum(conf_matrix[,2])
  sp.inf[[var]] <- conf_matrix[1, 1] / sum(conf_matrix[,1])
  acc.inf[[var]] <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
}

inf.point1 <- data.frame(
  inf.point = unlist(inflection_points),
  sen.inf = unlist(sen.inf),
  sp.inf = unlist(sp.inf),
  acc.inf = unlist(acc.inf)
)

inf.point=rbind(inf.point1)


varbs <- c(paste0("A", seq(n.att[a])))
results<-cbind(varbs, a, t, p, s, roc.youden,inf.point) 

for (i in varbs) {
  resultss <- as.data.frame(results[i,])
  row.names(resultss) <- NULL 
  if (!exists(paste0("results.", i,"_",n.att[a],"_",n.item[t],"_",n.sample[p]))) {  
    assign(paste0("results.", i,"_",n.att[a],"_",n.item[t],"_",n.sample[p]), resultss)  
  } else {
    assign(paste0("results.", i,"_",n.att[a],"_",n.item[t],"_",n.sample[p]), 
           rbind(get(paste0("results.", i,"_",n.att[a],"_",n.item[t],"_",n.sample[p])), resultss))
  }
  write.table(as.matrix(get(paste0("results.", i,"_",n.att[a],"_",n.item[t],"_",n.sample[p]))),
              file=paste0(file_namee,"/results_", i,"_",n.att[a],"_",n.item[t],"_",n.sample[p], ".txt"),sep=";",row.names=TRUE,col.names=TRUE)
  }
  
 s<-s+1
 
   },error=function (e){
   cat("error, repeat:",e$message,"\n")
       
   }) # tryCatch
 } # while

} # p, n.sample  
  
}  # t, n.item
  }  #a,  n.att


########### combined results ############3
n.att<- c(3)
n.item<- c(10,15,20,25,30) #5
n.sample <- c(30,50,100,150,200,250,300,350,400,450,500,1000) #12


all_results <- matrix(NA, nrow = length(n.att) * length(n.item) * length(n.sample), ncol = 16)
colnames(all_results) <- c("varbs","a","t","p","s","auc.AUC","auc.ll.ll","auc.ul.ul",
                           "cut.off","sen","sp","acc.n","inf.point","sen.inf","sp.inf","acc.inf")

combined_results <- list()
for(a in 1: length(n.att)){
  for (t in 1:length(n.item)){
    for (p in 1:length(n.sample)){
      file_path <- paste0(path,"/condition_", n.att[a], "_", n.item[t], "_", n.sample[p])
      
      varbs <- c(paste0("A", seq(n.att[a])))
      for (i in varbs) {
        
        temp_data <- read.table(paste0(file_path,"/results_", i,"_",n.att[a],"_",n.item[t],"_",n.sample[p], ".txt"), header = TRUE, sep = ";")
        
        if(is.null(combined_results[[i]])) {
          combined_results[[i]] <- temp_data
        } else {
          combined_results[[i]] <- rbind(combined_results[[i]], temp_data)
        }
        
      }
    }
  }
}

for(i in names(combined_results)) {
  output_file <- paste0(path,"/all_results_", i, ".txt")
  write.table(combined_results[[i]], output_file, row.names = FALSE, sep = ";")
}

       
      



