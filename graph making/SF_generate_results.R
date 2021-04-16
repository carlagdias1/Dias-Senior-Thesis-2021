# This file reads results from multiple model runs, and produces the tables and figures in the article.

library(RColorBrewer)

# ---------------------
# load 2000 model runs
# =====================

# sA:sH = results with stochastic variation only. Each object is a list of 2 items; first item is key model results; second item is daily new cases
# mcA:mcH = key results with probabilistic variation in inputs
# mcA_cases:mcH_cases = daily new cases with probabilistic variation in input


sf <- function(x) {
  y <- apply(x, 2, quantile, probs = c(0.025, 0.5, 0.975))
  y <- formatC(round(y, 0), big.mark = ',', format = 'd')
  apply(y, 2, function(x) paste0(x[2], ' (', x[1], '-', x[3], ')'))
}

# ----------------------------------------
# results in main article (first 200 runs)
# ========================================

# table 3

table3SF <- rbind(sf(sASF[[1]][1:50,4:7]),
                  sf(sBSF[[1]][1:50,4:7]),
                  sf(sCSF[[1]][1:50,4:7]),
                  sf(sDSF[[1]][1:50,4:7]),
                  sf(sESF[[1]][1:50,4:7]),
                  sf(sFSF[[1]][1:50,4:7]),
                  sf(sGSF[[1]][1:50,4:7]),
                  sf(sHSF[[1]][1:50,4:7]))
write.csv(table3SF, 'table3SF.csv')

#  --------------------------------
#  plot of new cases - scenario A:H
#  ================================

nd <- ncol(sASF[[2]])
model_start <- as.Date('2020-03-01')

xl <- seq(model_start, model_start + 396, by = 'day')
xl <- xl[format(xl, '%d') == '01']
xl <- tail(head(xl, -1), -1)
xx <- as.numeric(xl - model_start)
xlab <- paste0('1 ', format(xl, format = '%b %Y'))

pf <- function(cases, ymax = 150, xoff = 0, yoff = 0, cols = brewer.pal(9, 'Oranges')[c(3, 8)]) {
  tot <- rowSums(cases)
  med <- cases[which.min(abs(tot - median(tot))),]
  xp <- seq_along(med)
  apply(replace(cases, cases > ymax, ymax), 1, function(y) lines(xp + xoff, y + yoff, lwd = 0.5, col = cols[1]))
  lines(xp + xoff, med + yoff, col = cols[2])
}

modelend <- as.numeric(as.Date('2021-03-31') - model_start)

# stochastic variation only
# -------------------------

pdf('fig3SF.pdf', height = 10, width = 15)

plot(1, type = 'n', xlim = c(0, nd * 4), ylim = c(0, 300), axes = F, xlab = NA, ylab = NA)

rect(0, 0, nd * 4, 150, col = 'grey98')

pf(sASF[[2]][1:50,], yoff = 75)
pf(sBSF[[2]][1:50,])
pf(sCSF[[2]][1:50,], yoff = 75, xoff = nd)
pf(sDSF[[2]][1:50,], xoff = nd)
pf(sESF[[2]][1:50,], yoff = 75, xoff = nd * 2)
pf(sFSF[[2]][1:50,], xoff = nd * 2)
pf(sGSF[[2]][1:50,], yoff = 75, xoff = nd * 3)
pf(sHSF[[2]][1:50,], xoff = nd * 3)

rect(0, 0, nd * 4, 75)
rect(nd, 0, nd*3, 150)
rect(nd, 0, nd*2, 150)

axis(1, xx, xlab, las = 2, pos = 0)
axis(1, xx + nd, xlab, las = 2, pos = 0)
axis(1, xx + nd * 2, xlab, las = 2, pos = 0)
axis(1, xx + nd * 3, xlab, las = 2, pos = 0)

axis(2, 0:2 * 25, las = 2, pos = 0)
axis(2, 0:3 * 25 + 75, 0:3 * 25, las = 2, pos = 0)

mtext('Number of new cases per day', 2, line = 3, at = 85)
text(rep(c(nd/2*0.05 + modelend/2, nd * 1 + modelend/2, nd * 2 + modelend/2, nd*3 + modelend/2), each = 2), c(147.5, 72.5, 147.5, 72.5, 147.5, 72.5),
     c('Scenario A\n(Base Case;\nday 61)',
       'Scenario B\n(No Hotel Housing)',
       'Scenario C\n(Hotel Housing Started;\nday 46)',
       'Scenario D\n(Hotel Housing Started;\nday 15)',
       'Scenario E\n(Hotel Housing Started;\nday 76)',
       'Scenario F\n(Hotel Housing Started;\nday 92)',
       'Scenario G\n(Hotel Housing Started;\nday 107)',
       'Scenario H\n(Hotel Housing Started;\nday 170)'),
     adj = c(0.5, 1))

#text(rep(c(modelend/2, nd + modelend/2, nd * 2 + modelend/2), each = 2), c(590, 290, 590, 290, 590, 290),
#     rep('Scenario A', 4),
#     adj = c(0.5, 1))

dev.off()