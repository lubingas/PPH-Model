
library(metafor)
library(RColorBrewer)

col <- brewer.pal(3,"Dark2")

setwd("C:/Users/slubinga/Dropbox/Babijo-Solo Shared Resources/PPH")
d <- read.csv("pph_incidence1.csv", header = T)
# subset these data by id==1
newd <- subset(d, id==1)
rand_incid <- rma(measure = "PR", xi=xi, ni=ni, data = newd, method = "DL")
incidencePPH <- c(rand_incid$b, rand_incid$ci.lb, rand_incid$ci.ub, rand_incid$se)

forest(rand_incid, 
       annotate = T,
       showweight = T,
       slab = paste(newd$author, newd$year, sep = ", "),
       ilab = newd[, c("xi", "ni")],
       ilab.xpos = c(-0.1, -0.05),
       xlab = "Incidence",
       order = "prec",
       refline = rand_incid$b,
       col=col[1],
       border=col[1],
       #points(rand_incid$yi, rand_incid$vi, pch=15, bg=col[3]),
       cex = 0.7)
text(-0.33, 5.5, "Author(s), year", cex = 0.7, pos = 4)
text(-0.12, 5.5, "n", cex = 0.7, pos = 4)
text(-0.07, 5.5, "N", cex = 0.7, pos = 4)
text(0.36, 5.5, "Weight", cex = 0.7, pos = 4)
text(0.44, 5.5, "Incidence (95% CI)", cex = 0.7, pos = 4)

d2 <- read.csv("trial_comp.csv", header = T)
# subset these data by id==1
newd2 <- subset(d2, id==1)
rand_rr <- rma(measure = "RR", ai=x2i, n1i=n2i, ci=x1i, n2i=n1i, data = newd2, method = "DL")
rand_rr
eff_rr <- c(exp(rand_rr$b), exp(rand_rr$se), exp(rand_rr$ci.lb), exp(rand_rr$ci.ub))

forest(rand_rr,
       transf = exp,
       annotate = T,
       showweight = T,
       slab = paste(newd2$author, newd2$year, sep = ", "),
       ilab = newd2[, c("x2i", "n2i", "x1i", "n1i")],
       ilab.xpos = c(0.12, 0.2, 0.38, 0.46),
       #xlab = "Incidence",
       order = "prec",
       refline = exp(rand_rr$b),
       col=col[1],
       border=col[1],
       cex = 0.7)

text(-0.3, 3.5, "Author(s), year", cex = 0.7, pos = 4)
text(0.03, 4, "Misoprostol", cex = 0.7, pos = 4)
text(0.06, 3.5, "n", cex = 0.6, pos = 4)
text(0.15, 3.5, "N", cex = 0.6, pos = 4)
text(0.31, 4, "Placebo", cex = 0.7, pos = 4)
text(0.32, 3.5, "n", cex = 0.6, pos = 4)
text(0.41, 3.5, "N", cex = 0.6, pos = 4)
text(1.42, 3.5, "Weight", cex = 0.7, pos = 4)
text(1.6, 3.5, "RR (95% CI)", cex = 0.7, pos = 4)
