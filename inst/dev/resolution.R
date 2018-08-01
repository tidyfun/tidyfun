# constructors should use args as given
# resolution shoudl be computed from arg as given
# evaluate should: snap x-values to the nearest arg if below resolution, leave alone if not
# -> we just use rounding at the lowest possible level and else leave inputs and args the hell alone

arg <- seq(0,1, l =93)
evaluations <- 100*arg
new_arg <- seq(0,1, l = 150)
object <- tfd(evaluations, arg, resolution = 0.005)

plot(object)
