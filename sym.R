source("funs.R")
# ============================================================
# simulations:
f_functions <- list(dayloop2, dayloop2_A, dayloop2_B, dayloop2_C, dayloop2_D)
f_names <- c("dayloop2","dayloop2_A","dayloop2_B","dayloop2_C","dayloop2_D")

params <- unique(rbind(
	expand.grid(method_id=1L:2L,i=(1L:10L)*1000L),
	expand.grid(method_id=2L:5L,i=(1L:10L)*10000L),
	expand.grid(method_id=3L:5L,i=50000+(1L:8L)*100000L)
))
params <- params[sample(seq_len(nrow(params))),]

n <- 850000
X <- as.data.frame(matrix(sample(1:10, n*9, TRUE), n, 9))

times <- apply(params, 1, function(x) {
	pX <- X[1:x[2],]
	pF <- f_functions[[x[1]]]
	system.time(pF(pX))[1]
})

results <- data.frame(method=f_names[params$method_id], params, time=times)
results <- results[with(results,order(method_id,i)),]
save(results, file="results.RData")
# ============================================================
# Plots
require(RColorBrewer) # display.brewer.all()
palette(brewer.pal(9,"Set1"))
w_axis <- function(n) axis(1, at=n*(0:10),
	labels=prettyNum(as.integer(n*(0L:10L)), big.mark=",", preserve.width="individual"),las=1,cex.axis=.8)
w_plot_init <- function(n, m, n0=1000) plot(0, 0,
	ylim = with(results, range(time[method_id %in% m & i<= n])),
	xlim = c(n0, n),
	type="n", ylab="user time (sec)", xlab="nrow(X)", las=2, xaxt="n", cex.axis=.8
	)

text_pos <- list(
	x1 = c(8000,8000),
	y1 = c(20,5),
	x2 = c(10000,80000,80000),
	y2 = c(28,40,3),
	x3 = c(5000,5000,700000,700000,700000),
	y3 = c(20,5,1.9,1.3,0.4)
)

# Plot 1
m <- 1L:2L
png("ver_1.png",540,360)
w_plot_init(10000, m)
w_axis(1000)
grid()
for (k in m) {
	indx <- with(results,method_id==k & i<=10000)
	with(results, lines(i[indx],time[indx],type="b",col=k))
	text(text_pos$x1[k],text_pos$y1[k],f_names[k],col=k)
}
title("dayloop2 vs dayloop2_A",cex.main=1)
dev.off()
shell.exec(file.path(getwd(),"ver_1.png"))

results$time[1:10]/results$time[11:20]

# Plot 2
m <- 1L:3L
png("ver_2.png",540,360)
w_plot_init(100000, m)
w_axis(10000)
grid()
for (k in m) {
	indx <- with(results,method_id==k & i<=100000)
	with(results, lines(i[indx],time[indx],type="b",col=k))
	text(text_pos$x2[k],text_pos$y2[k],f_names[k],col=k)
}
title("Vectorization FTW",cex.main=1)
dev.off()
shell.exec(file.path(getwd(),"ver_2.png"))

# Plot 3
m <- 3L:5L
png("ver_3.png",540,360)
w_plot_init(850000, 3:5, n0=100000)
w_axis(100000)
grid()
for (k in m) {
	indx <- with(results,method_id==k & i>=100000)
	with(results, lines(i[indx],time[indx],type="b",col=k))
	text(text_pos$x3[k],text_pos$y3[k],f_names[k],col=k)
}
title("More vectorization",cex.main=1)
dev.off()
shell.exec(file.path(getwd(),"ver_3.png"))

indx <- results$method=="dayloop2_A"
mdl <- lm(time~i, results, subset=indx)
est <- predict(mdl, data.frame(i=850000))
# 6m12s

# Tests
set.seed(2908822)
n <- 8500#00
temp <- as.data.frame(matrix(sample(1:10, n*9, TRUE), n, 9))
all.equal(dayloop2(temp)  , dayloop2_A(temp)) # TRUE
all.equal(dayloop2_A(temp), dayloop2_B(temp)) # TRUE
all.equal(dayloop2_B(temp), dayloop2_C(temp)) # TRUE
all.equal(dayloop2_C(temp), dayloop2_D(temp)) # TRUE

