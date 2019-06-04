# PhD Saskia Schirmer
# 15.05.2019
# continuous model
# plot normalized migratory connectivity
plotMnorm <- function(B,f_fit_norm,res_x,f_mean, f_sd, pdf = FALSE, all = FALSE){
  if(pdf) pdf("estimateM.pdf", width = 12, height = 12)
  if(all) B <- 1
  par(mfrow = c(1,1),mar = c(5,4,4,13)+0.1)
  plot(NA, ylim = c(0,max(sapply(f_fit_norm, max))), xlim = c(0,res_x),
       xlab = "wintering area w", ylab = "density", xaxt = "none")
  legend(105,1.5, lty = 1:2, legend = c("estimated distribution", "true distribution"),
         xpd = TRUE, col = 1)
  legend(105,1.0, lty = 1, legend = 1:10,
         xpd = TRUE, col = 1:10, title = "breeding area")
  for(b in 1:B){
    lines(1:(res_x-1),f_fit_norm[[b]], lty = 1, col = b)
    lines(1:(res_x-1),dtruncnorm(1:(res_x-1)/(res_x-1),0,1, mean = f_mean[b] ,sd = f_sd),
          col = b, lty = 2)

  }
  if(pdf) dev.off()
  par(mfrow = c(1,1),mar = c(5,4,4,2)+0.1)
}
