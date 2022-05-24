# 04 02 Music
# Simulation 


#simple sine wave

t <- seq(0, 4*pi, length.out=100) #sequence of points from 0 to 4pi
y1 <- sin(t)
plot(t,y1)
lines(t, y1, col=2)

# Harmonics and beat frequencies
yharm <- sin(t) + sin(2*t) + sin(3*t) + sin(t-0.1*t)
plot(t,yharm)
lines(t, yharm, col=3)

# Increasing amplitude
d <- 2
yamp <- yharm + d*t
plot(t,yamp)
lines(t, yamp, col=4)

# Add noise
n <- 100 # number of data points
c.norm <- rnorm(n)
namp <- 2.0

ynoise <- yamp + namp*c.norm
plot(t,ynoise)
lines(t, ynoise, col=4)