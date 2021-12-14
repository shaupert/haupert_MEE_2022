###**********************************************************************###
# Functions to convert wav file into Sound Pressure Level (SPL) (=effective pressure) 
# or dB SPL (depending on the reference : 20µPa for air propagation)
#
# Author : Sylvain HAUPERT, MNHN, sylvain.haupert@mnhn.fr
# Date : 20/01/2020
# Licence : MIT
####**********************************************************************###

library(signal)

# Constant : minimum value possible
MIN = 2.2250738585072014e-308


dB2amplitude <- function (db)
{
  return (10**(db/20))
}

dB2power <- function (db)
{
  return (10**(db/10))
}

amplitude2dB <- function (s)
{
  return (20*log10(s))
}

power2dB <- function (s)
{
  return (10*log10(s))
}

####################

##### wav2volt
wav2volt <- function (wave, bit=16, Vadc=2) 
{ 
  # convert in Volt. Vadc is 2Vpp [peak to peak] for the SM4
  volt = (wave/(2^(bit))) * Vadc
  return (volt)
}

#####  volt2pressure
volt2pressure <- function (volt, gain, sensitivity=-35) 
{  
  #volt amplitude to instantaneous sound pressure (Pa)
  coef = 1/10^(sensitivity/20) 
  p = volt * coef / 10^(gain/20)

  return(p)
}

#####  wav2pressure
wav2pressure <- function (wave, gain, sensitivity=-35, bit=16, Vadc=2) 
{
  # Vadc => 2Vpp [peak to peak] for the SM4
  p = volt2pressure(wav2volt(wave, bit, Vadc), gain, sensitivity) 
  
  return (p)
}

###################################"

#####  pressure2dBSPL
pressure2dBSPL <- function(p, pRef=20e-6)
{
  # if p <=0 set to MIN
  p[p==0] = MIN
  # Take the log of the ratio pressure/pRef
  L = 20*log10(p/pRef) 
  return (L)
}

#####  dBSPL2pressure
dBSPL2pressure <- function(L, pRef=20e-6)
{
  # dB SPL to pressure
  p = 10**(L/20)*pRef
  return (p)
}                

#####  wav2dBSPL
wav2dBSPL <- function (wave, gain, sensitivity=-35, bit=16, Vadc=2, pRef=20e-6) 
{
  p = wav2pressure(wave, gain, sensitivity, bit, Vadc) 
  L = pressure2dBSPL(p, pRef)
  return (L)
}

#####  psd2dBSPL
psd2dBSPL <- function (P, gain, sensitivity=-35, bit=16, Vadc=2, pRef=20e-6) 
{
  # convert power (energy) to amplitude
  s = sqrt(P)
  # convert amplitude to dB sPL
  L = wav2dBSPL(s, gain, sensitivity, bit, Vadc, pRef)
  return (L)
}

########################

#####  wav2Leq
wav2leq <- function (wave, f, gain, dt=1, sensitivity=-35, bit=16, Vadc=2, pRef= 20e-6) 
{  
  # convert into pressure
  p = wav2pressure(wave, gain, sensitivity, bit, Vadc) 
  
  # p to Leq (Equivalent Continuous Sound level)
  Leq = pressure2leq(p, f, dt, pRef) 
  
  return(Leq)
}

#####  pressure2Leq
pressure2leq <- function (p, f, dt=1, pRef = 20e-6) 
{  
  # wav to RMS
  dN = dt*f # integration period in number of points
  N_RMS = floor(length(p)/dN)
  
  p_RMS = matrix(NA,nrow=1,ncol=N_RMS)
  
  for (ii in 1:N_RMS)
  {
    p_mean = mean(p[(1+(ii-1)*dN):(ii*dN)]^2)
    p_RMS[ii] = sqrt(p_mean)
  }
  
  # if p_RMS ==0 set to MIN
  p_RMS[p_RMS==0] = MIN
  
  # RMS to Leq (Equivalent Continuous Sound level)
  Leq = 20*log10(p_RMS/pRef)
  
  return(Leq)
}

#####  psdLeq
psd2leq <- function (psd, gain, sensitivity=-35, bit=16, Vadc=2, pRef=20e-6) 
{  
  # convert P (amplitude²) to pressure²
  P = wav2pressure (sqrt(psd), gain, sensitivity, bit, Vadc)**2

  # if P ==0 set to MIN
  P[P==0] = MIN
  
  # Energy (pressure^2) to Leq 
  # => Leq (Equivalent Continuous Sound level) if the sum is performed on the whole PSD
  leq = 10*log10(sum(P)/pRef**2)
  return (leq)
}
  

# # # # ########## TEST
# filename = "/home/haupert/DATA/mes_projets/02_GUYANE/dB@NOURAGUES/DATA_PROCESSING/Propagation_sonore/GUYANE/04_Propagation_100m_direction_sud_4dB_essai3/output/wn_1m.wav"
# NFFT = 512
# # read the a portion of the wave file in order to know the sampling frequency (fs_rec)
# wn = readWave(filename, units = 'seconds')
# # extract data
# wn.wav = wn@left
# f = wn@samp.rate
# bit = wn@bit
# # convert sounds (wave) into SPL and subtract DC offset
# wn.wav  = wn.wav  - mean(wn.wav)
# wn.wav.Pa = wav2pressure(wn.wav, wn@bit, gain=30, sensitivity=-35)
# # Leq (integration duration : 1s)
# wn.wav.Leq= pressure2leq(wn.wav.Pa, f=wn@samp.rate, dt=1)
# # Leq (integration duration : 1s)
# wn.wav.Leq2 = wav2leq(wn.wav, f=wn@samp.rate, gain=30, dt=1, sensitivity=-35, bit=wn@bit)
# # Power Density Spectrum : PSD
# wn.freq= meanspec(wn.wav, f=wn@samp.rate, wl=NFFT, wn="hamming", correction="amplitude", norm=FALSE, plot=FALSE)[,1]
# wn.spec= meanspec(wn.wav, f=wn@samp.rate, wl=NFFT, wn="hamming", correction="amplitude", norm=FALSE, plot=TRUE)[,2]
# wn.PSD = (wn.spec^2)/2
# # average Leq from PSD
# wn.PSD.Leq  = psd2leq(wn.PSD, gain=30, sensitivity=-35, bit=wn@bit)
# 
# # print both LEQ values. They should be very similar.
# print(wn.wav.Leq)
# print(wn.wav.Leq2)
# print(wn.PSD.Leq)

