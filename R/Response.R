Response = function(f){
  sinc = function(x){y=sin(pi*x)/(pi*x); y[x==0] = 1; y}
  t = (1:160000-40000)/40000 # -200 to get the jump up at t=0
  TD_RC = exp(-t * (t>=0)/(94e3*15e-9)) / (94e3*15e-9) * (t >= 0)
  TD_ADC = (t >= 0 & t <= 0.002105); TD_ADC = TD_ADC/sum(TD_ADC)
  TD_digitized = Re(fft(fft(TD_RC)*fft(TD_ADC), inverse = TRUE))[1:1600 * 100]/160000

  ### calculate FIR sinc coefficients with hamming window
  r=12; M=2*r; w = 0.54 - 0.46 * cos(2*pi*(0:M)/M) # hamming window
  s1 = sinc(-r:r/400 * 70)
  coef = s1*w/sum(s1*w)

  ### filter the digitized signal
  TD = filter(coef, 1, TD_digitized)

  ### calculate the spectrum
  FD = fft(TD)
  FD = FD/max(abs(FD)) # ugly ugly ugly
  freqs = (1:length(FD)-1)/(length(FD)/400)
  
  exp(approx(freqs, Re(log(FD)), f)$y + 1i * approx(freqs, Im(log(FD)), f)$y) # if not done in log space, output is super choppy
  
}
  
