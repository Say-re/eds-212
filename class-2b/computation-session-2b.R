library(tidyverse)

# Define new function
equation = function(x){3*x^2 + 4}
equation_comp = function(x){5*x^2 + 4}

# Plot over specific range using 'stat_function' option:
ggplot(data.frame(x = c(1, 1000)), aes(x = x)) +
  stat_function(fun = equation) +
  geom_function(fun = equation_comp, colour = 'blue')

# geom_function can be useful for plotting a function over data sets to visualize fit

equation_2 = function(x){2.4 - 5*x^3 + 2.1*x^2}

# Plot over specified range using 'stat_function' option:
ggplot(data.frame(x = c(-50, 50)), aes(x = x)) +
  stat_function(fun = equation_2) +
  xlab('Scales (millions)') +
  ylab('Mercury ingested (mg)')


# Higher Order Derivatives Introduction

function_gt <- expression(2.2 + 3.1 * t - 5.6 * t ^ 4 + 4.3 / (t ^ 3))

# Take first order derivative
derivative_dg_dt <- D(function_gt, 't')

# Verify Output
derivative_dg_dt

# Find second order derivative
derivative_d2g_d2g <- D(derivative_dg_dt, 't')

# Verify Output
derivative_d2g_d2g

# Plotting Derivatives alongside functions

# Store function C(t)
ct <- function(t) {t ^ 3}

# Take Derivative of function C(t)
derivative_dc_dt <- D(expression(t ^ 3), 't')
derivative_dc_dt

# Store Derivative as a function
deriv_func_dc_dt <- function(t) {3 * t ^ 2}

# And Second derivative
deriv_func_d2c_d2t <- function(t) {6 * t}

# Plot function C(t)
ggplot(data.frame(t=c(-5, 5)), aes(x = t)) +
  stat_function(fun = ct) +
  stat_function(fun = deriv_func_dc_dt, colour = 'red') +
  stat_function(fun = deriv_func_d2c_d2t, colour = 'green')


###### Partial Derivatives #####
# Create function
function_xyz <- expression(2*x*y - 3*(x^2)*(x^3))

# Take partial with respect to x
derivative_df_dx <- D(function_xyz, 'x')
# Derivative of x
derivative_df_dx

# Take partial with respect to y
derivative_df_dy <- D(function_xyz, 'y')
# Derivative of y
derivative_df_dy

# Take partial with respect to z
derivative_df_dz <- D(function_xyz, 'z')
# Derivative of z
derivative_df_dz


