
# Use base R to produce a ridgeline plot
# Stolen from https://stackoverflow.com/questions/65334695/using-base-r-how-to-create-a-joy-plot-aka-ridgeline-plots-with-many-distri
# Modified with ChatGPT

# This function takes three arguments
#   var1 = numeric variable
#   var2 = categorical variable
#   scale = a scale parameter that changes the overlap between ridges

new_ridgeline <- function(var1,var2,scale=4){
  
  # Split data into a list by Var2
  dat <- split(var1, var2)
  n_groups <- length(unique(var2))
  
  # Higher values of scale_parameter increase the overlap between densities
  scale_parameter <- scale
  scale_factor <- 1 + ((scale_parameter*n_groups)/length(dat))
  
  densities <- Map(function(x, g, i) {
    with(density(x), data.frame(x, y = (y*scale_factor) + (i - 1), g))
  }, dat, names(dat), seq_along(dat))
  
  xrange <- range(unlist(lapply(densities, function(d) range(d$x))))
  yrange <- range(unlist(lapply(densities, function(d) range(d$y))))
  
  colors <- colorRampPalette(c("red","yellow","green","blue"))(n_groups)  # Fixed color scale from red to blue
  colors <- rgb(t(col2rgb(colors) / 255), alpha = 0.5)  # Convert to RGB with alpha
  
  plot(0, 0, type = "n", xlim = xrange, ylim = yrange, ylab = "", xlab = "Value", axes=F)
  abline(h = seq_along(dat) - 1, col = "grey")
  for (i in seq_along(densities)) {
    with(densities[[i]], polygon(x, y, col = colors[i]))
  }
  axis(1)
  axis(2, at = seq_along(dat) - 1, labels = names(dat),las=1,col=rgb(0.5,0.5,0.5,0),hadj = 0.5)
  
}

# Example data.frame
example_groups <- 12
means <- rnorm(example_groups,sd=1.5)

df <- data.frame(
  Var1 = unlist(lapply(means,function(x) rnorm(n=100,mean=x,sd=1))),
  Var2 = rep(letters[1:example_groups], each = 100)
)

new_ridgeline(df$Var1,df$Var2,3)
