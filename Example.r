# ===============
# EXAMPLE
# ===============

rm(list=ls(all=TRUE))
source("Functions-RFIT.R")
source("Functions-IT.R")

set.seed(123)
p <- 5;
dat <- rdat(n=500, p=p, rho=0.5, Model="II", sigma=1, digit=2) # TRAINING SAMPLE
test <- rdat(n=1000, p=p, rho=0.5, Model="II", sigma=1, digit=2) # TEST SAMPLE
ITE.test <- test$delta # THE TRUE ITE IN THE TEST DATA
col.y <- 2; col.trt <- 3; cols.split.var <- 4:(p+3); cols.nominal <- NULL

# --------------------------
# SINGLE IT TREE ANALYSIS
# --------------------------

# GROWING IT AND BOOTSTRAP PRUNING
prn.boot <- bootstrap.grow.prune(B=30, data=dat, min.node.size=10, n0=3, max.depth=8,
	SSS=TRUE, a=10, q=0.01, n.intervals=1,
	col.y=col.y, col.trt=col.trt, cols.split.var=cols.split.var, cols.nominal=cols.nominal,
	mtry=p, LeBlanc=TRUE, min.boot.tree.size=1)  
prn.result <- bootstrap.size(prn.boot, n=NROW(dat), plot.it=FALSE)
prn.result$G.a 
# THE BEST IT TREE WITH a=2
btree <- prn.result$btree[[1]] 
plot.tree(btree, cols.nominal=7, textDepth=5, lines="rectangle")
# GENERATE LATEX CODES FOR TREE PLOT
plot.tree.latex(tree0, file="tree-code.tex", digits=5, cols.nominal=7)
# PREDICT ITE WITH THE TEST SAMPLE
Pred.IT <- send.down(dat.new=test, btree, cols.nominal=cols.nominal)

# --------------------------------------------
# RFIT - RANDOM FOREESTS OF INTERACTION TREES
# --------------------------------------------

ntree <- 2000
fit.rfit <- Build.RFIT(dat, min.node.size=10, n0=3, max.depth=10,
	SSS=TRUE, a=10, q=0.01, n.intervals=1,   # SSS OPTIONS
	col.y=col.y, col.trt=col.trt, cols.split.var=cols.split.var, cols.nominal=cols.nominal,
	ntree=ntree, mtry=3, avoid.nul.tree=TRUE)
pred <- predict.RFIT(fit.rfit, test, SE=TRUE)
Pred.RFIT <- pred$predicted.ite
SE.Corrected <- pred$se.ite.corrected

# ERROR BAR PLOT, SORTED BY ITE
se <- SE.Corrected; ite.RFIT <- Pred.RFIT
trt <- test$trt; n <- length(ite.RFIT)
par(mfrow=c(1, 1), mar=c(4, 4.5, 4, 4.5))
lb <- ite.RFIT - se; ub <- ite.RFIT + se;
y.min <- min(lb); y.max <- max(ub)
col.dot <- ifelse(trt==1, "blue", "darkorange3")
lb <- lb[order(ite.RFIT)]; ub <- ub[order(ite.RFIT)]
ite.RFIT <- ite.RFIT[order(ite.RFIT)] 
plot(1:n, ite.RFIT, ylim=c(y.min, y.max), pch=19, ylab=expression(hat(delta)),
	xlab="rank by ITE", cex=0.6, col=col.dot, cex.lab=1.2, las=1, # xaxt="n",
	main="Error Bar for Estimated ITE", cex.main=1.5)
col.bar <- ifelse(trt==1, "cyan4", "darkgoldenrod1") 
arrows(1:n, lb, 1:n, ub, length=0.03, angle=90, code=3, lwd=1, col=col.bar)
abline(h=0, lwd=1, col="black")
abline(h=mean(ite.RFIT), lwd=2, col="gray60")

# ---------------------------
# SR - SEPARATE REGRESSION
# ---------------------------

Pred.SR <- Predict.ITE.SR(y~x1+x2+x3+x4+x5|trt, dat=dat, test=test, 
	ntree=ntree, mtry=3)



# --------------
# RESULTS
# --------------

OUT <- data.frame(ITE.test, Pred.IT, Pred.RFIT, SE.Corrected, Pred.SR)
head(OUT)
# MSE
mean((Pred.IT-ITE.test)^2)
mean((Pred.RFIT-ITE.test)^2)
mean((Pred.SR-ITE.test)^2)

