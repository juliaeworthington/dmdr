#' Plot Mode (number)
#'
#' Plots a specified DMD mode.
#' @import ggplot2
#' @import ggforce
#' @import patchwork
#' @import RColorBrewer
#' @param dmd.obj; list, output from dmd() function
#' @param x; vector, x values
#' @param mode.num; int, number of specified mode for plotting
#' @param complex.part; character,  desired mode for plotting, "real", "imaginary", "phase", or "magnitude"
#' @param save; boolean, saves plot if TRUE, prints plot if FALSE
#' @return p; plot
#' @export
plot.modes.num = function(dmd.obj, x, mode.num, complex.part = "real", save = FALSE) {
  # extracting data from dmd list
  lambda = dmd.obj$lambda; Phi = dmd.obj$Phi; b = dmd.obj$b

  # svd rank of the dmd is number of eigenvalues
  rank = length(lambda)

  # specified mode must be within range of rank of matrix
  if (mode.num > rank) { warning("Specified mode must be within range of available modes.")}

  # extract mode (number) from Phi * diag(b)
  mode.raw = (Phi  %*% diag(b))[,mode.num] # This is the full complex mode with amplitude

  # select mode values to be plotted
  if (complex.part == "imaginary") {mode.val = Im(mode.raw)}
  else if (complex.part == "phase") {mode.val = Arg(mode.raw)}
  else if (complex.part == "magnitude") {mode.val = Mod(mode.raw)}
  else {mode.val = Re(mode.raw)}

  # setting the color pallete for colorcoding with <=8 modes
  if (length(lambda)<=8) {set2 = brewer.pal(8, "Set2")}

  # plot title
  title = paste("Mode", mode.num, "-", complex.part)

  # creating data frame of data to be plotted (x and mode)
  df = data.frame(x, mode.val)

  # creating the plot
  p = ggplot(df, aes(x=x, y=mode.val))

  # colorcoding for plots with <= 8 plots
  if (length(lambda)<=8) {p = p + geom_line(color = set2[mode.num])}
  else {p = p + geom_line(color = "#E84A5FFF")}

  p = p + labs(x = "x", y = NULL, title = title)
  p = p + theme_bw()
  p = p + theme(plot.title = element_text(hjust = 0.5))

  # save plot as png or return plot
  if (save==TRUE) {png = paste("dmd mode", mode.num, ".png"); ggsave(png, plot = p, width = 6, height = 4, dpi = 150)}
  else {return(p)}
}
#' Plot Dynamics (number)
#'
#' Plots the underlying dynamics of a specified DMD mode.
#' @param dmd.obj; list, output from dmd() function
#' @param t; vector, time values
#' @param mode.num; int, number of specified mode for plotting
#' @param complex.part; character,  desired mode for plotting, "real", "imaginary", "phase", or "magnitude"
#' @param save; boolean, saves plot if TRUE, prints plot if FALSE
#' @return p; plot
#' @export

plot.dynamics.num = function(dmd.obj, t, mode.num, complex.part = "real", save = FALSE) {
  # extracting data from dmd list
  lambda = dmd.obj$lambda; Phi = dmd.obj$Phi; b = dmd.obj$b

  # svd rank of the dmd is number of eigenvalues
  rank = length(lambda)

  # specified mode must be within range of rank of matrix
  if (mode.num > rank) { warning("Selected mode  must be within range of available modes.")}

  # compute underlying dynamics of specified mode
  Lambda = outer(lambda, seq(1, length(t)), `^`); dynamics.raw = (diag(b) %*% Lambda)[mode.num,]

  # select mode dynamics values to be plotted
  if (complex.part == "imaginary") {dynamics.val = Im(dynamics.raw)}
  else if (complex.part == "phase") {dynamics.val = Arg(dynamics.raw)}
  else if (complex.part == "magnitude") {dynamics.val = Mod(dynamics.raw)}
  else {dynamics.val = Re(dynamics.raw)}

  # setting the color pallete for colorcoding with <=8 modes
  if (length(lambda)<=8) {set2 = brewer.pal(8, "Set2")}

  # plot title
  title = paste("Mode", mode.num, "Dynamics -", complex.part)

  # creating data frame of data to be plotted (x and mode)
  df = data.frame(t, dynamics.val)

  # creating the plot
  p = ggplot(df, aes(x=t, y=dynamics.val))

  # colorcoding for plots with <= 8 plots
  if (length(lambda)<=8) {p = p + geom_line(color = set2[mode.num])}
  else {p = p + geom_line()}

  p = p + labs(x = "t", y = NULL, title = title)
  p = p + theme_bw()
  p = p + theme(plot.title = element_text(hjust = 0.5))

  # save plot as png or return plot
  if (save==TRUE) {png = paste("dmd mode", mode.num, "dynamics.png"); ggsave(png, plot = p, width = 6, height = 4, dpi = 150)}
  else {return(p)}
}
#' Plot Eigenvalues (Discrete)
#'
#' Plots the DMD eigenvalues in discrete form as points on the complex plane.
#' @param dmd.obj; list, output from dmd() function
#' @param save; boolean, saves plot if TRUE, prints plot if FALSE
#' @return p; plot
#' @export
plot.eigs.discrete = function(dmd.obj, save = FALSE) {
  # extracting data from dmd list
  lambda = dmd.obj$lambda; Phi = dmd.obj$Phi; b = dmd.obj$b

  # plot title
  title = paste("Discrete-time Eigenvalues")

  # convert mode to factor and store in dataframe for plotting
  mode = as.factor(1:length(lambda)); df = data.frame(Re(lambda), Im(lambda), mode)

  # creating the plot
  p = ggplot(df, aes(x=Re(lambda), y=Im(lambda), color = mode))
  p = p + geom_circle(aes(x0 = 0, y0 = 0, r = 1), color = "grey", linetype = "dashed", linewidth = .8)
  if (length(lambda)<=8) {p = p + scale_color_brewer(palette = "Set2")}
  p = p + coord_fixed()
  p = p + guides()
  p = p + labs(x = "Re(λ)", y = "Im(λ)", title = title)
  p = p + theme_bw()
  p = p + theme(plot.title = element_text(hjust = 0.5))
  p = p + geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = .8, alpha = .5)
  p = p + geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = .8, alpha = .5)
  p = p + geom_point(size = 5)

  # save plot as png or return plot
  if (save==TRUE) {png = paste("dmd eigenvalues.png"); ggsave(png, plot = p, width = 6, height = 4, dpi = 150)}
  else {return(p)}
}
#' Plot Modes
#'
#' Plots all DMD modes.
#' @param dmd.obj; list, output from dmd() function
#' @param x; vector, x values
#' @param complex.part; character,  desired mode for plotting, "real", "imaginary", "phase", or "magnitude"
#' @param save; boolean, saves plot if TRUE, prints plot if FALSE
#' @return p; plot
#' @export
plot.modes = function(dmd.obj, x, complex.part = "real", save = FALSE) {
  # extracting data from dmd list
  lambda = dmd.obj$lambda; n = length(lambda)

  # plot all modes for dmd
  p = plot.modes.num(dmd.obj, x, 1, complex.part = complex.part, save = FALSE)
  for (i in 2:n) {p = p + plot.modes.num(dmd.obj, x, i, complex.part = complex.part, save = FALSE)}

  # save plot as png or return plot
  if (save==TRUE) {png = paste("dmd modes.png"); ggsave(png, plot = p, width = 6*n/2, height = 4, dpi = 150)}
  else {return(p)}
}
#' Plot Dynamics
#'
#' Plots underlying dynamics of all DMD modes.
#' @param dmd.obj; list, output from dmd() function
#' @param t; vector, time values
#' @param complex.part; character,  desired mode for plotting, "real", "imaginary", "phase", or "magnitude"
#' @param save; boolean, saves plot if TRUE, prints plot if FALSE
#' @return p; plot
#' @export
plot.dynamics = function(dmd.obj, t, complex.part = "real", save = FALSE) {
  # extracting data from dmd list
  lambda = dmd.obj$lambda; n = length(lambda)

  # plot all modes for dmd
  p = plot.dynamics.num(dmd.obj, t ,1, complex.part = complex.part, save = FALSE)
  for (i in 2:n) {p = p + plot.dynamics.num(dmd.obj, t, i, complex.part = complex.part, save = FALSE)}

  # save plot as png or return plot
  if (save==TRUE) {png = paste("dmd mode dynamics.png"); ggsave(png, plot = p, width = 6, height = 4, dpi = 150)}
  else {return(p)}
}
#' Plot Summary
#'
#' Plots all DMD modes, underlying dynamics, and eigenvalues.
#' @param dmd.obj; list, output from dmd() function
#' @param x; vector, x values
#' @param t; vector, time values
#' @param complex.part; character,  desired mode for plotting, "real", "imaginary", "phase", or "magnitude"
#' @param save; boolean, saves plot if TRUE, prints plot if FALSE
#' @return p; plot
#' @export
plot.sum = function(dmd.obj, x, t, complex.part = "real", save = FALSE) {
  p = plot.eigs.discrete(dmd.obj, save = FALSE) / plot.modes(dmd.obj, x, complex.part = complex.part, save = FALSE) / plot.dynamics(dmd.obj, t, complex.part = complex.part, save = FALSE)
  if (save==TRUE) {png = paste("dmd plot summary.png"); ggsave(png, plot = p, width = 3, height = 3, dpi = 300, scale = 5)}
  else {return(p)}
}
