source("funs.R")
# ============================================================
# Ostateczne symulacje do odpowiedzi:
funkcje <- list(dayloop2, dayloop2_A, dayloop2_B, dayloop2_C, dayloop2_D)
f_nazwy <- c("dayloop2","dayloop2_A","dayloop2_B","dayloop2_C","dayloop2_D")

params <- unique(rbind(
	expand.grid(metoda_id=1L:2L,i=(1L:10L)*1000L),
	expand.grid(metoda_id=2L:5L,i=(1L:10L)*10000L),
	expand.grid(metoda_id=3L:5L,i=50000+(1L:8L)*100000L)
))
params <- params[sample(seq_len(nrow(params))),]

n <- 850000
X <- as.data.frame(matrix(sample(1:10, n*9, TRUE), n, 9))

czasy <- apply(params, 1, function(x) {
	pX <- X[1:x[2],]
	pF <- funkcje[[x[1]]]
	system.time(pF(pX))[1]
})

wyniki <- data.frame(metoda=f_nazwy[params$metoda_id], params, czas=czasy)
wyniki <- wyniki[with(wyniki,order(metoda_id,i)),]
save(wyniki, file="symulacje.RData")
# ============================================================
# Wykresy
require(RColorBrewer) # display.brewer.all()
palette(brewer.pal(9,"Set1"))
w_axis <- function(n) axis(1, at=n*(0:10),
	labels=prettyNum(as.integer(n*(0L:10L)), big.mark=",", preserve.width="individual"),las=1,cex.axis=.8)
w_plot_init <- function(n, m, n0=1000) plot(0, 0,
	ylim = with(wyniki, range(czas[metoda_id %in% m & i<= n])),
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

# Wykres 1
m <- 1L:2L
png("wer_1.png",540,360)
w_plot_init(10000, m)
w_axis(1000)
grid()
for (k in m) {
	indx <- with(wyniki,metoda_id==k & i<=10000)
	with(wyniki, lines(i[indx],czas[indx],type="b",col=k))
	text(text_pos$x1[k],text_pos$y1[k],f_nazwy[k],col=k)
}
title("dayloop2 vs dayloop2_A",cex.main=1)
dev.off()
shell.exec(file.path(getwd(),"wer_1.png"))

wyniki$czas[1:10]/wyniki$czas[11:20]

# Wykres 2
m <- 1L:3L
png("wer_2.png",540,360)
w_plot_init(100000, m)
w_axis(10000)
grid()
for (k in m) {
	indx <- with(wyniki,metoda_id==k & i<=100000)
	with(wyniki, lines(i[indx],czas[indx],type="b",col=k))
	text(text_pos$x2[k],text_pos$y2[k],f_nazwy[k],col=k)
}
title("Vectorization FTW",cex.main=1)
dev.off()
shell.exec(file.path(getwd(),"wer_2.png"))

# Wykres 3
m <- 3L:5L
png("wer_3.png",540,360)
w_plot_init(850000, 3:5, n0=100000)
w_axis(100000)
grid()
for (k in m) {
	indx <- with(wyniki,metoda_id==k & i>=100000)
	with(wyniki, lines(i[indx],czas[indx],type="b",col=k))
	text(text_pos$x3[k],text_pos$y3[k],f_nazwy[k],col=k)
}
title("More vectorization",cex.main=1)
dev.off()
shell.exec(file.path(getwd(),"wer_3.png"))

indx <- wyniki$metoda=="dayloop2_A"
mdl <- lm(czas~i, wyniki, subset=indx)
est <- predict(mdl, data.frame(i=850000))
# 6m12s
   
# Sekcja TESTÓW
set.seed(2908822)
n <- 8500#00
temp <- as.data.frame(matrix(sample(1:10, n*9, TRUE), n, 9))
all.equal(dayloop2(temp)  , dayloop2_A(temp)) # TRUE
all.equal(dayloop2_A(temp), dayloop2_B(temp)) # TRUE
all.equal(dayloop2_B(temp), dayloop2_C(temp)) # TRUE
all.equal(dayloop2_C(temp), dayloop2_D(temp)) # TRUE

