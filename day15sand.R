
# Copyright George Savva 2022
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>. 

library(data.table)
library(tuneR)

## Number of grains of sand.
N=1e5

## Distribute a pile of sand to start
points <- data.table(x=.2*sin(7*(1:N)/N)+rnorm(N,0.0,.1)+.5, y=.2*cos(7*(1:N)/N)+rnorm(N,0.0,.1)+.5)

## Sand grains have random weights
weights=runif(N)

## Hue of each grain.
h = (1:N)/N

## Set amplitudes of waves
a1=.5
a2=.2
a3=.3

## Function to define the sound wave
## f1, f2 and f3 are the frequencies of the notes we are playing.
## they will be set in the global environment during the main loop.
soundwave <- function(x){
  sin(f1*pi*x)*a1 + sin(f2*pi*x)*a2 + sin(f3*pi*x)*a3
}

## Function to define the standing wave
amp <- function(x,y){
 (sin(f1*pi*x)*sin(f1*pi*y)*a1+ 
  sin(f2*pi*x)*sin(f2*pi*y)*a2+
  sin(f3*pi*x)*sin(f3*pi*y)*a3)*
    rnorm(length(x))*.4*weights
}

## FPS
fps=30

## Indexer to count frames
i=0

## Baseline for 1 second of audio.

## Empty list will store each note as a wave.
sounds <- list()

## Each note = 1 second.
durations <- rep(1,12)

## Rate of note decay
decay=0.9

set.seed(15012022)

## These are the relative frequencies of the notes that can be played.
## They will be scaled so that the lowest note is middle C.
possiblenotes <- c(3,    4,    6,    8,    9,    10,   12,  15,  18)
notenames <-     c("C4", "F4", "C5", "F5", "G5", "A5", "C6","E6","G6")

## Sample the bass notes (must be from the fi)
notes <- sample(possiblenotes[1:3],size = 12, replace = TRUE)
## Now the other two can be anything
notes2 <- sample(possiblenotes,size = 12, replace = TRUE)
notes3 <- sample(possiblenotes,size = 12, replace = TRUE)

## Loop over the number of chords
for(j in 1:12){
  ## Set f1, f2, f3 to be the frequencies of the notes in the chord
  f1=notes[j]
  f2=notes2[j]
  f3=notes3[j]
  
  ## Make the times for the bits of the wave
  t <- seq(0, durations[j]*2, length = durations[j]*44100)
  ## Create the sound wave for this chord.
  sounds[[j]] <- round(32000 * soundwave(87 * t) * decay^(fps*t/(2*pi)))

## Now drawn each frame
for(k in 1:(fps*durations[j])){
  
  i = i + 1
 
  ## Get the aplitude vibration at the point x,k at this frame
  a=amp(points$x, points$y)*decay^k
  ## Choose a random direction for each point to go in
  theta=runif(N,0,2*pi)
  
  ## Move the points
  points[,x:= x + 2*sin(theta)*a*(1.5-weights)]
  points[,y:= y + 2*cos(theta)*a*(1.5-weights)]
  
  ## Start a png
  png(type="cairo",filename = sprintf("chladni%05d.png",i), width=600, height=600)
  
  ## Lay out the graph
  layout(matrix(c(3,4,1,2), ncol=2), widths=c(0.2,0.8), heights=c(0.8,0.2))
  
  par(bg="#050505", mar=3*c(0,0,1,1),fg="white")
  
  ## Plot sand
  plot(points, pch=".", col=hsv(h,.8,1,.5), axes=F, ann=F, xlim=c(0,1), ylim=c(0,1))
  box(col="white")
  
  # Plot bottom wave
  par(mar=3*c(1,0,0,1),fg="white")
  plot.function(function(x) soundwave(x)*0.90^k, xlim=c(0,1),ylim=abs(a1+a2+a3)*c(-1,1), axes=F, col="white",lwd=2)
  abline(0,0)
  box(col="white")
  grid(col="cyan")
  axis(1,col="white",labels = TRUE)
  
  # Plot side waves
  par(mar=3*c(0,1,1,0))
  plot(x=a1*sin(f1*pi*seq(0,1,.01)),y=seq(0,1,.01),xlim=c(-.5,.5), type="l",col="green",axes=FALSE)
  lines(x=a2*sin(f2*pi*seq(0,1,.01)),y=seq(0,1,.01), type="l",col="green")
  lines(x=a3*sin(f3*pi*seq(0,1,.01)),y=seq(0,1,.01), type="l",col="green")
  box(col="white")
  grid(col="cyan")
  axis(2,col="white",labels = TRUE)
  
  # Draw notes grid
  par(mar=3*c(1,1,0,0))
  plot(NA, xlim=c(0,3), ylim=c(0,3), axes=F, ann=F)
  text(rep(c(0.5,1.5,2.5),3),rep(c(0.5,1.5,2.5),each=3), 
       notenames, col=ifelse(possiblenotes %in% c(f1,f2,f3),"green", "#555555"),cex=1.3)
  box(col="white")
  
  ## Finished drawing frame.
  dev.off()
}
}

  ## Make sound wav
  channel <- Reduce(c,sounds)
  ## Needs to be stereo or ffmpeg doesn't like it.
  Wobj <- Wave(left = channel, right=channel)
  writeWave(Wobj, "sound.wav")

## Make video from the frames and the sound.
shell("ffmpeg -r 30 -y -i chladni%05d.png -i sound.wav -c:v libx264 -r 30 -pix_fmt yuv420p -c:a aac v1.mp4")

