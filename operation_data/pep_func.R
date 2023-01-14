# ****************************************************************************
# title: Steam Function for R
# author: J.H Ahn(T. 6569)
#
# revision    date              description
# --------    -----------       ----------------------------------------------
# 0           2022.08.10        Initial release
#
# ****************************************************************************

#
# 1. STEAM PROPERTIES based on IAPWS95
#

# loading steam properties library
# detail information refers to http://www.iapws.org/index.html
# https://web1.hszg.de/thermo_fpc/range_of_validity/range_of_validity_water.htm?choose_fluid=1&

# 주의!!! 해결책이 마련될 때까지 과열증기 영역에서만 사용할 것!!!

library(IAPWS95)

# It is valid from the triple point to the pressure of 10,000bar and temperature 1273K(999.85C)

# Get Enthalpy(kJ/kg) using Pressure(bar) and Entropy(kJkg/K)
stmPSH <- function(p,s) {
  p_MPa <- p / 10 # 10bara
  hps(p_MPa, s)
}

# Get Enthalpy(kJ/kg) using Pressure(bar) and Temperature(C)
stmPTH <- function(p,t) {
  p_MPa <- p / 10 # 10bara
  t_degK <- t + 273.15 # 273.15K
  hTp(t_degK,p_MPa)
}

# Get Temperature(C) using Pressure(bar) and Enthalpy(kJ/kg)
stmPHT <- function(p,h) {
  p_MPa <- p / 10 # 10bara
  t_degC <- Tph(p_MPa, h) - 273.15
  return(t_degC)
}

# Get Temperature(C) using Enthalpy(kJ/kg) and Entropy(kJkg/K)
stmHST <- function(h,s) {
  t_degC <- Ths(h,s) - 273.15
  return(t_degC)
}

# Get Entropy(kJkg/K) using Pressure(bar) and Entropy(kJkg/K)
stmPHS <- function(p,h) {
  p_MPa <- p / 10 # 10bara
  sph(p_MPa, h)
}

# Get Temperature(C) using Pressure(bar) and Entropy(kJkg/K)
stmPST <- function(p,s) {
  p_MPa <- p / 10 # 10bara
  t_degC <- Tps(p_MPa,s) - 273.15
  return(t_degC)
}

# Get saturation Temperature(C) using Pressure(bar)
stmPT <- function(p) {
  p_MPa <- p / 10 # 10bara
  t_degC <- TSatp(p_MPa) - 273.15
  return(t_degC)
}

# Get saturation Pressure(bar) using Temperature(C)
stmTP <- function(t) {
  t_degK <- t + 273.15 # 273.15K
  p_bar <- pSatT(t_degK) * 10
  return(p_bar)
}

# Get saturation Temperature(C) using Entropy(kJkg/K)
stmST <- function(s) {
  t_degC <- TSats(s) - 273.15
  return(t_degC)
}

# Get specific volume (m3/kg) using Temperature(C) and Pressure(bar)
stmPTV <- function(p,t) {
  p_MPa <- p / 10 # 10bara
  t_degK <- t + 273.15 # 273.15K
  vTp(t_degK, p_MPa)
}

# Get specific volume (m3/kg) using Pressure(bar) and Enthalpy(kJ/kg)
stmPHV <- function(p,h) {
  p_MPa <- p / 10 # 10bara
  t <- Tph(p_MPa, h) # 이렇게 온도를 구하면 Dry 상태의 온도만 구해짐짐
  vTp(t, p_MPa)
}


# Get Entropy(kJkg/K) using Temperature(C) and Pressure(bar)
stmPTS <- function(p,t) {
  p_MPa <- p / 10 # 10bara
  t_degK <- t + 273.15 # 273.15K
  sTp(t_degK, p_MPa)
}


#
# 2. Unit Conversion
#

# 2.1 Pressure Conversion

atm_to_bar <- function(Pressure) {Pressure * 1.01325}
atm_to_psia <- function(Pressure) {Pressure * 14.6959488}
atm_to_inHg <- function(Pressure) {Pressure * 29.92128}
atm_to_kgpcm2 <- function(Pressure) {Pressure * 1.03322745}
atm_to_ftH2O <- function(Pressure) {Pressure * 33.9570029}
atm_to_mmHg <- function(Pressure) {Pressure * 760.000512}
atm_to_kPa <- function(Pressure) {Pressure * 101.325}

bar_to_atm <- function(Pressure) {Pressure * 0.986923267}
bar_to_psia <- function(Pressure) {Pressure * 14.5037738}
bar_to_inHg <- function(Pressure) {Pressure * 29.5300074}
bar_to_ftH2O <- function(Pressure) {Pressure * 33.5129562}
bar_to_mmHg <- function(Pressure) {Pressure * 750.062188}
bar_to_kgpcm2 <- function(Pressure) {Pressure * 1.01971621}
bar_to_kPa <- function(Pressure) {Pressure * 100}

psia_to_atm <- function(Pressure) {Pressure * 0.0680459639}
psia_to_bar <- function(Pressure) {Pressure * 0.0689475729}
psia_to_inHg <- function(Pressure) {Pressure * 2.03602234}
psia_to_ftH2O <- function(Pressure) {Pressure * 2.31036399}
psia_to_mmHg <- function(Pressure) {Pressure * 51.7149674}
psia_to_kgpcm2 <- function(Pressure) {Pressure * 0.070306958}
psia_to_kPa <- function(Pressure) {Pressure * 6.89475729}
psia_to_kPa2 <- function(Pressure) {Pressure * 6.894757}

inHg_to_atm <- function(Pressure) {Pressure * 0.0334210301}
inHg_to_bar <- function(Pressure) {Pressure * 0.0338638588}
inHg_to_psia <- function(Pressure) {Pressure * 0.491153746}
inHg_to_ftH2O <- function(Pressure) {Pressure * 1.13487801}
inHg_to_mmHg <- function(Pressure) {Pressure * 25.4}
inHg_to_kgpcm2 <- function(Pressure) {Pressure * 0.0345315258}
inHg_to_kPa <- function(Pressure) {Pressure * 3.38638588}

mmH2O_to_bar <- function(Pressure) {Pressure * 0.098066496 / 1000}
mmH2O_to_psia <- function(Pressure) {Pressure * 0.098066496 / 1000 * 14.503774}
mmH2O_to_kgpcm2 <- function(Pressure) {Pressure * 0.098066496 / 1000 * 1.019716213}
mmH2O_to_kPa <- function(Pressure) {Pressure * 0.098066496 / 1000 * 100}
mmH2O_to_atm <- function(Pressure) {Pressure * 0.098066496 / 1.01325 / 1000}
mmH2O_to_mmHg <- function(Pressure) {Pressure * 0.0735559206}
mmH2O_to_inHg <- function(Pressure) {Pressure * 0.0735559206 / 25.4}

ftH2O_to_atm <- function(Pressure) {Pressure * 0.0294490066}
ftH2O_to_bar <- function(Pressure) {Pressure * 0.0298392059}
ftH2O_to_psia <- function(Pressure) {Pressure * 0.432781092}
ftH2O_to_inHg <- function(Pressure) {Pressure * 0.881151971}
ftH2O_to_mmHg <- function(Pressure) {Pressure * 22.3812601}
ftH2O_to_kgpcm2 <- function(Pressure) {Pressure * 0.030427522}
ftH2O_to_kPa <- function(Pressure) {Pressure * 2.98392059}

mmHg_to_atm <- function(Pressure) {Pressure * 0.00131578859}
mmHg_to_bar <- function(Pressure) {Pressure * 0.00133322279}
mmHg_to_psia <- function(Pressure) {Pressure * 0.0193367617}
mmHg_to_inHg <- function(Pressure) {Pressure * 0.0393700787}
mmHg_to_ftH2O <- function(Pressure) {Pressure * 0.0446802368}
mmHg_to_kgpcm2 <- function(Pressure) {Pressure * 0.00135950889}
mmHg_to_kPa <- function(Pressure) {Pressure * 0.133322279}

kgpcm2_to_atm <- function(Pressure) {Pressure * 0.967841105}
kgpcm2_to_bar <- function(Pressure) {Pressure * 0.980665}
kgpcm2_to_psia <- function(Pressure) {Pressure * 14.2233433}
kgpcm2_to_inHg <- function(Pressure) {Pressure * 28.9590447}
kgpcm2_to_ftH2O <- function(Pressure) {Pressure * 32.8649832}
kgpcm2_to_mmHg <- function(Pressure) {Pressure * 735.559736}
kgpcm2_to_kPa <- function(Pressure) {Pressure * 98.0665}

kPa_to_atm <- function(Pressure) {Pressure * 0.00986923267}
kPa_to_bar <- function(Pressure) {Pressure * 0.01}
kPa_to_psia <- function(Pressure) {Pressure * 0.145037738}
kPa_to_inHg <- function(Pressure) {Pressure * 0.295300074}
kPa_to_ftH2O <- function(Pressure) {Pressure * 0.335129562}
kPa_to_mmHg <- function(Pressure) {Pressure * 7.50062188}
kPa_to_kgpcm2 <- function(Pressure) {Pressure * 0.010197162}

# 2.2 Temperature Conversion

degF_to_degC <- function(Temperature) {(Temperature - 32) * 5 / 9}
degC_to_degF <- function(Temperature) {Temperature * 9 / 5 + 32}

# 2.3 Enthalpy Conversion

Btuplbm_to_ftlbfplbm <- function(Enthalpy) {Enthalpy * 778.169262}
Btuplbm_to_hphrplbm <- function(Enthalpy) {Enthalpy * 3.93014779 * 10 ^ (-4)}
Btuplbm_to_kgfmpkg <- function(Enthalpy) {Enthalpy * 237.185991}
Btuplbm_to_kcalpkg <- function(Enthalpy) {Enthalpy * 5.555556 * 10 ^ (-1)}
Btuplbm_to_kJpkg <- function(Enthalpy) {Enthalpy * 2.326}

ftlbfplbm_to_Btuplbm <- function (Enthalpy) {Enthalpy * 0.00128506746}
ftlbfplbm_to_hphrplbm <- function (Enthalpy) {Enthalpy * 5.05050505 * 10 ^ (-7)}
ftlbfplbm_to_kgfmpkg <- function (Enthalpy) {Enthalpy * 0.3048}
ftlbfplbm_to_kcalpkg <- function (Enthalpy) {Enthalpy * 7.13926369 * 10 ^ (-4)}
ftlbfplbm_to_kJpkg <- function (Enthalpy) {Enthalpy * 0.00298906692}

hphrplbm_to_Btuplbm <- function (Enthalpy) {Enthalpy * 2544.43358}
hphrplbm_to_ftlbfplbm <- function (Enthalpy) {Enthalpy * 1980000}
hphrplbm_to_kgfmpkg <- function (Enthalpy) {Enthalpy * 603504}
hphrplbm_to_kcalpkg <- function (Enthalpy) {Enthalpy * 1413.57421}
hphrplbm_to_kJpkg <- function (Enthalpy) {Enthalpy * 5918.3525}

kgfmpkg_to_Btuplbm <- function (Enthalpy) {Enthalpy * 0.042161006}
kgfmpkg_to_ftlbfplbm <- function (Enthalpy) {Enthalpy * 3.2808399}
kgfmpkg_to_hphrplbm <- function (Enthalpy) {Enthalpy * 0.000165698985}
kgfmpkg_to_kcalpkg <- function (Enthalpy) {Enthalpy * 0.00234227811}
kgfmpkg_to_kJpkg <- function (Enthalpy) {Enthalpy * 0.00980665}

kcalpkg_to_Btuplbm <- function (Enthalpy) {Enthalpy * 1.8}
kcalpkg_to_ftlbfplbm <- function (Enthalpy) {Enthalpy * 1400.70467}
kcalpkg_to_hphrplbm <- function (Enthalpy) {Enthalpy * 0.000707426602}
kcalpkg_to_kgfmpkg <- function (Enthalpy) {Enthalpy * 0.426934784}
kcalpkg_to_kJpkg <- function (Enthalpy) {Enthalpy * 4.1868}

kJpkg_to_Btuplbm <- function (Enthalpy) {Enthalpy * 0.429922614}
kJpkg_to_ftlbfplbm <- function (Enthalpy) {Enthalpy * 334.552563}
kJpkg_to_hphrplbm <- function (Enthalpy) {Enthalpy * 1.68965941 * 10 ^ (-4)}
kJpkg_to_kgfmpkg <- function (Enthalpy) {Enthalpy * 101.971621}
kJpkg_to_kcalpkg <- function (Enthalpy) {Enthalpy * 2.38845897 * 10 ^ (-1)}

# # # 2.4 Enthropy Conversion

BtuplbmR_to_ftlbfplbmR <- function(Entrophy) {Entrophy * 778.169262}
BtuplbmR_to_kwhrplbmR <- function(Entrophy) {Entrophy * 0.00029307107}
BtuplbmR_to_barcm3pgK <- function(Entrophy) {Entrophy * 41.868}
BtuplbmR_to_kcalpkgK <- function(Entrophy) {Entrophy * 1}
BtuplbmR_to_kgfmpkgK <- function(Entrophy) {Entrophy * 426.934784}
BtuplbmR_to_kJpkgK <- function(Entrophy) {Entrophy * 4.1868}

ftlbfplbmR_to_BtuplbmR <- function(Entrophy) {Entrophy * 0.00128506746}
ftlbfplbmR_to_kwhrplbmR <- function(Entrophy) {Entrophy * 3.76616097 * 10 ^ (-7)}
ftlbfplbmR_to_barcm3pgK <- function(Entrophy) {Entrophy * 0.0538032046}
ftlbfplbmR_to_kcalpkgK <- function(Entrophy) {Entrophy * 1.28506746 * 10 ^ (-3)}
ftlbfplbmR_to_kgfmpkgK <- function(Entrophy) {Entrophy * 0.54864}
ftlbfplbmR_to_kJpkgK <- function(Entrophy) {Entrophy * 0.00538032046}

kwhrplbmR_to_BtuplbmR <- function(Entrophy) {Entrophy * 3412.14163}
kwhrplbmR_to_ftlbfplbmR <- function(Entrophy) {Entrophy * 2655223.73}
kwhrplbmR_to_barcm3pgK <- function(Entrophy) {Entrophy * 142859.546}
kwhrplbmR_to_kcalpkgK <- function(Entrophy) {Entrophy * 3412.14163}
kwhrplbmR_to_kgfmpkgK <- function(Entrophy) {Entrophy * 1456.76195}
kwhrplbmR_to_kJpkgK <- function(Entrophy) {Entrophy * 14285.9546}

barcm3pgK_to_BtuplbmR <- function(Entrophy) {Entrophy * 0.0238845897}
barcm3pgK_to_ftlbfplbmR <- function(Entrophy) {Entrophy * 18.5862535}
barcm3pgK_to_kwhrplbmR <- function(Entrophy) {Entrophy * 6.99988225 * 10 ^ (-6)}
barcm3pgK_to_kcalpkgK <- function(Entrophy) {Entrophy * 2.38845897 * 10 ^ (-2)}
barcm3pgK_to_kgfmpkgK <- function(Entrophy) {Entrophy * 10.1971621}
barcm3pgK_to_kJpkgK <- function(Entrophy) {Entrophy * 0.1}

kcalpkgK_to_BtuplbmR <- function(Entrophy) {Entrophy * 1}
kcalpkgK_to_ftlbfplbmR <- function(Entrophy) {Entrophy * 778.169262}
kcalpkgK_to_kwhrplbmR <- function(Entrophy) {Entrophy * 0.00029307107}
kcalpkgK_to_barcm3pgK <- function(Entrophy) {Entrophy * 41.868}
kcalpkgK_to_kgfmpkgK <- function(Entrophy) {Entrophy * 426.934784}
kcalpkgK_to_kJpkgK <- function(Entrophy) {Entrophy * 4.1868}

kgfmpkgK_to_BtuplbmR <- function(Entrophy) {Entrophy * 0.00234227811}
kgfmpkgK_to_ftlbfplbmR <- function(Entrophy) {Entrophy * 1.82268883}
kgfmpkgK_to_kwhrplbmR <- function(Entrophy) {Entrophy * 0.000000686453953}
kgfmpkgK_to_barcm3pgK <- function(Entrophy) {Entrophy * 0.0980665}
kgfmpkgK_to_kcalpkgK <- function(Entrophy) {Entrophy * 0.00234227811}
kgfmpkgK_to_kJpkgK <- function(Entrophy) {Entrophy * 0.00980665}

kJpkgK_to_BtuplbmR <- function(Entrophy) {Entrophy * 0.238845897}
kJpkgK_to_ftlbfplbmR <- function(Entrophy) {Entrophy * 185.862535}
kJpkgK_to_kwhrplbmR <- function(Entrophy) {Entrophy * 6.99988225 * 10 ^ (-5)}
kJpkgK_to_barcm3pgK <- function(Entrophy) {Entrophy * 10}
kJpkgK_to_kcalpkgK <- function(Entrophy) {Entrophy * 0.238845897}
kJpkgK_to_kgfmpkgK <- function(Entrophy) {Entrophy * 101.971621}

# 2.5 Energy Conversion

Btu_to_ftlbf <- function(Energy) {Energy * 778.169262}
Btu_to_kcal <- function(Energy) {Energy / 3.96832072}
Btu_to_kJ <- function(Energy) {Energy / 0.94781712}
Btu_to_kwhr <- function(Energy) {Energy / 3412.141632}
Btu_to_hphr <- function(Energy) {Energy * 0.000393014779}

ftlbf_to_Btu <- function(Energy) {Energy * 0.00128506746}
ftlbf_to_kcal <- function(Energy) {(Energy * 3.96832072) * 778.169262}
ftlbf_to_kJ <- function(Energy) {(Energy * 0.94781712) * 778.169262}
ftlbf_to_kwhr <- function(Energy) {(Energy * 3412.1632) * 778.169262}
ftlbf_to_hphr <- function(Energy) {Energy * 5.05050505 * 10 ^ (-7)}

hphr_to_Btu <- function(Energy) {Energy * 2544.43358}
hphr_to_kcal <- function(Energy) {(Energy * 3.96832072) * 0.00039314779}
hphr_to_kJ <- function(Energy) {(Energy * 0.94781712) * 0.00039314779}
hphr_to_kwhr <- function(Energy) {(Energy * 3412.141632) * 0.00039314779}
hphr_to_ftlbf <- function(Energy) {Energy * 1980000}

kcal_to_Btu <- function(Energy) {Energy * 3.96832072}
kcal_to_hphr <- function(Energy) {(Energy * 2544.43358) / 3.96832072}
kcal_to_kJ <- function(Energy) {(Energy / 0.94781712) * 3.96832072}
kcal_to_kwhr <- function(Energy) {(Energy / 3412.141632) * 3.96832072}
kcal_to_ftlbf <- function(Energy) {(Energy * 0.00128506746) / 3.96832072}

kJ_to_Btu <- function(Energy) {Energy * 0.94781712}
kJ_to_hphr <- function(Energy) {(Energy * 2544.43358) / 0.94781712}
kJ_to_kcal <- function(Energy) {(Energy / 3.96832072) * 0.94781712}
kJ_to_kwhr <- function(Energy) {(Energy * 3412.141632) / 0.94781712}
kJ_to_ftlbf <- function(Energy) {(Energy * 0.00128506746) / 0.94781712}

kwhr_to_Btu <- function(Energy) {Energy * 3412.141632}
kwhr_to_hphr <- function(Energy) {(Energy * 2544.43358) / 3412.141632}
kwhr_to_kJ <- function(Energy) {(Energy * 0.94781712) / 3412.141632}
kwhr_to_kcal <- function(Energy) {(Energy / 3.96832072) * 3412.141632}
kwhr_to_ftlbf <- function(Energy) {(Energy * 0.00128506746) / 3412.141632}

# 2.6 Specific Volume Conversion

ft3plbm_to_in3plbm <- function(Specific_volume) {Specific_volume * 1728}
ft3plbm_to_USgalplbm <- function(Specific_volume) {Specific_volume * 7.48051948}
ft3plbm_to_literpkg <- function(Specific_volume) {Specific_volume * 62.4279606}
ft3plbm_to_m3pkg <- function(Specific_volume) {Specific_volume * 0.0624279606}

in3plbm_to_ft3plbm <- function(Specific_volume) {Specific_volume * 0.000578703704}
in3plbm_to_USgalplbm <- function(Specific_volume) {Specific_volume * 0.00432900433}
in3plbm_to_literpkg <- function(Specific_volume) {Specific_volume * 0.036127292}
in3plbm_to_m3pkg <- function(Specific_volume) {Specific_volume * 0.000036127292}

m3plbm_to_ft3plbm <- function(Specific_volume) {Specific_volume * 0.000578703704}
m3plbm_to_USgalplbm <- function(Specific_volume) {Specific_volume * 0.00432900433}
m3plbm_to_literpkg <- function(Specific_volume) {Specific_volume * 0.036127292}
m3plbm_to_m3pkg <- function(Specific_volume) {Specific_volume * 0.000036127292}

USgalplbm_to_ft3plbm <- function(Specific_volume) {Specific_volume * 0.133680556}
USgalplbm_to_in3plbm <- function(Specific_volume) {Specific_volume * 231}
USgalplbm_to_literpkg <- function(Specific_volume) {Specific_volume * 8.34540445}
USgalplbm_to_m3pkg <- function(Specific_volume) {Specific_volume * 0.00834540445}

literpkg_to_ft3plbm <- function(Specific_volume) {Specific_volume * 0.0160184634}
literpkg_to_in3plbm <- function(Specific_volume) {Specific_volume * 27.6799047}
literpkg_to_USgalplbm <- function(Specific_volume) {Specific_volume * 0.119826427}
literpkg_to_m3pkg <- function(Specific_volume) {Specific_volume * 0.001}

m3pkg_to_ft3plbm <- function(Specific_volume) {Specific_volume * 16.0184634}
m3pkg_to_in3plbm <- function(Specific_volume) {Specific_volume * 27679.9047}
m3pkg_to_USgalplbm <- function(Specific_volume) {Specific_volume * 119.826427}
m3pkg_to_literpkg <- function(Specific_volume) {Specific_volume * 1000}

# 2.7 Viscosity Conversion

pasec_to_lbfsecpft2 <- function(Viscosity) {Viscosity * 0.0208854342}
pasec_to_lbmpftsec <- function(Viscosity) {Viscosity * 0.671968975}
pasec_to_lbmphrft <- function(Viscosity) {Viscosity * 2419.08831}
pasec_to_gpcmsec <- function(Viscosity) {Viscosity * 10}
pasec_to_kgpmsec <- function(Viscosity) {Viscosity * 1}

lbfsecpft2_to_pasec <- function(Viscosity) {Viscosity * 47.880259}
lbfsecpft2_to_lbmpftsec <- function(Viscosity) {Viscosity * 32.1740486}
lbfsecpft2_to_lbmphrft <- function(Viscosity) {Viscosity * 115826.575}
lbfsecpft2_to_gpcmsec <- function(Viscosity) {Viscosity * 478.80259}
lbfsecpft2_to_kgpmsec <- function(Viscosity) {Viscosity * 47.880259}

lbmpftsec_to_pasec <- function(Viscosity) {Viscosity * 1.48816394}
lbmpftsec_to_lbfsecpft2 <- function(Viscosity) {Viscosity * 0.0310809502}
lbmpftsec_to_lbmphrft <- function(Viscosity) {Viscosity * 3600}
lbmpftsec_to_gpcmsec <- function(Viscosity) {Viscosity * 14.8816394}
lbmpftsec_to_kgpmsec <- function(Viscosity) {Viscosity * 1.48816394}

lbmphrft_to_pasec <- function(Viscosity) {Viscosity * 0.413378873 * 10 ^ (-3)}
lbmphrft_to_lbfsecpft2 <- function(Viscosity) {Viscosity * 8.63359727 * 10 ^ (-6)}
lbmphrft_to_lbmpftsec <- function(Viscosity) {Viscosity * 0.000277777778}
lbmphrft_to_gpcmsec <- function(Viscosity) {Viscosity * 0.00413378873}
lbmphrft_to_kgpmsec <- function(Viscosity) {Viscosity * 0.000413378873}

gpcmsec_to_pasec <- function(Viscosity) {Viscosity * 0.1}
gpcmsec_to_lbfsecpft2 <- function(Viscosity) {Viscosity * 0.00208854342}
gpcmsec_to_lbmpftsec <- function(Viscosity) {Viscosity * 0.0671968975}
gpcmsec_to_lbmphrft <- function(Viscosity) {Viscosity * 241.908831}
gpcmsec_to_kgpmsec <- function(Viscosity) {Viscosity * 0.1}

kgpmsec_to_pasec <- function(Viscosity) {Viscosity * 1}
kgpmsec_to_lbfsecpft2 <- function(Viscosity) {Viscosity * 0.0208854342}
kgpmsec_to_lbmpftsec <- function(Viscosity) {Viscosity * 0.671968975}
kgpmsec_to_lbmphrft <- function(Viscosity) {Viscosity * 2419.08831}
kgpmsec_to_gpcmsec <- function(Viscosity) {Viscosity * 10}

m2psec_to_ft2psec <- function(Kinematic_Viscosity) {Kinematic_Viscosity * 10.7639104}
m2psec_to_cm2psec <- function(Kinematic_Viscosity) {Kinematic_Viscosity * 10000}
m2psec_to_cm2phr <- function(Kinematic_Viscosity) {Kinematic_Viscosity * 36 * 10 ^ 6}
m2psec_to_m2phr <- function(Kinematic_Viscosity) {Kinematic_Viscosity * 3600}

ft2psec_to_m2psec <- function(Kinematic_Viscosity) {Kinematic_Viscosity * 0.09290304}
ft2psec_to_cm2psec <- function(Kinematic_Viscosity) {Kinematic_Viscosity * 929.0304}
ft2psec_to_cm2phr <- function(Kinematic_Viscosity) {Kinematic_Viscosity * 3344509.44}
ft2psec_to_m2phr <- function(Kinematic_Viscosity) {Kinematic_Viscosity * 334.450944}

cm2psec_to_m2psec <- function(Kinematic_Viscosity) {Kinematic_Viscosity * 10 ^ (-4)}
cm2psec_to_ft2psec <- function(Kinematic_Viscosity) {Kinematic_Viscosity * 0.00107639104}
cm2psec_to_cm2phr <- function(Kinematic_Viscosity) {Kinematic_Viscosity * 3600}
cm2psec_to_m2phr <- function(Kinematic_Viscosity) {Kinematic_Viscosity * 0.36}

cm2phr_to_m2psec <- function(Kinematic_Viscosity) {Kinematic_Viscosity * 27.7777778 * 10 ^ (-9)}
cm2phr_to_ft2psec <- function(Kinematic_Viscosity) {Kinematic_Viscosity * 0.298997512 * 10 ^ (-6)}
cm2phr_to_cm2psec <- function(Kinematic_Viscosity) {Kinematic_Viscosity * 0.000277777778}
cm2phr_to_m2phr <- function(Kinematic_Viscosity) {Kinematic_Viscosity * 0.0001}

m2phr_to_m2psec <- function(Kinematic_Viscosity) {Kinematic_Viscosity * 277.777778 * 10 ^ (-6)}
m2phr_to_ft2psec <- function(Kinematic_Viscosity) {Kinematic_Viscosity * 2.98997512 * 10 ^ (-3)}
m2phr_to_cm2psec <- function(Kinematic_Viscosity) {Kinematic_Viscosity * 2.77777778}
m2phr_to_cm2phr <- function(Kinematic_Viscosity) {Kinematic_Viscosity * 10000}

# 2.8 Conductivity Conversion

BtuphrftF_to_lbfphrftF <- function(Conductivity) {Conductivity * 778.169262}
BtuphrftF_to_WattpftF <- function(Conductivity) {Conductivity * 0.29307107}
BtuphrftF_to_WattpmC <- function(Conductivity) {Conductivity * 1.73073467}
BtuphrftF_to_kgfphrC <- function(Conductivity) {Conductivity * 635.348952}
BtuphrftF_to_calpseccmC <- function(Conductivity) {Conductivity * 0.00413378873}
BtuphrftF_to_kcalphrmC <- function(Conductivity) {Conductivity * 1.48816394}

lbfphrF_to_BtuphrftF <- function(Conductivity) {Conductivity * 0.00128506746}
lbfphrF_to_WattpftF <- function(Conductivity) {Conductivity * 0.000376616097}
lbfphrF_to_WattpmC <- function(Conductivity) {Conductivity * 0.00222411081}
lbfphrF_to_kgfphrC <- function(Conductivity) {Conductivity * 0.816466266}
lbfphrF_to_calpseccmC <- function(Conductivity) {Conductivity * 5.3121974 * 10 ^ (-6)}
lbfphrF_to_kcalphrmC <- function(Conductivity) {Conductivity * 0.00191239106}

WattpftF_to_BtuphrftF <- function(Conductivity) {Conductivity * 3.41214163}
WattpftF_to_lbfphrF <- function(Conductivity) {Conductivity * 2655.22374}
WattpftF_to_WattpmC <- function(Conductivity) {Conductivity * 5.90551181}
WattpftF_to_kgfphrC <- function(Conductivity) {Conductivity * 2167.90061}
WattpftF_to_calpseccmC <- function(Conductivity) {Conductivity * 0.0141050726}
WattpftF_to_kcalphrmC <- function(Conductivity) {Conductivity * 5.07782615}

WattpmC_to_BtuphrftF <- function(Conductivity) {Conductivity * 0.577789316}
WattpmC_to_lbfphrF <- function(Conductivity) {Conductivity * 449.617886}
WattpmC_to_WattpftF <- function(Conductivity) {Conductivity * 0.169333333}
WattpmC_to_kgfphrC <- function(Conductivity) {Conductivity * 367.097837}
WattpmC_to_calpseccmC <- function(Conductivity) {Conductivity * 0.00238845897}
WattpmC_to_kcalphrmC <- function(Conductivity) {Conductivity * 0.859845228}

kgfphrC_to_BtuphrftF <- function(Conductivity) {Conductivity * 0.00157393822}
kgfphrC_to_lbfphrF <- function(Conductivity) {Conductivity * 1.22479035}
kgfphrC_to_WattpftF <- function(Conductivity) {Conductivity * 0.000461275759}
kgfphrC_to_WattpmC <- function(Conductivity) {Conductivity * 0.00272406944}
kgfphrC_to_calpseccmC <- function(Conductivity) {Conductivity * 6.50632809 * 10 ^ (-6)}
kgfphrC_to_kcalphrmC <- function(Conductivity) {Conductivity * 0.00234227811}

calpseccmC_to_BtuphrftF <- function(Conductivity) {Conductivity * 241.908831}
calpseccmC_to_lbfphrF <- function(Conductivity) {Conductivity * 188246.017}
calpseccmC_to_WattpftF <- function(Conductivity) {Conductivity * 70.89648}
calpseccmC_to_WattpmC <- function(Conductivity) {Conductivity * 418.68}
calpseccmC_to_kgfphrC <- function(Conductivity) {Conductivity * 153696.522}
calpseccmC_to_kcalphrmC <- function(Conductivity) {Conductivity * 360}

kcalphrmC_to_BtuphrftF <- function(Conductivity) {Conductivity * 0.671968975}
kcalphrmC_to_lbfphrF <- function(Conductivity) {Conductivity * 522.905602}
kcalphrmC_to_WattpftF <- function(Conductivity) {Conductivity * 0.196934667}
kcalphrmC_to_WattpmC <- function(Conductivity) {Conductivity * 1.163}
kcalphrmC_to_kgfphrC <- function(Conductivity) {Conductivity * 426.934784}
kcalphrmC_to_calpseccmC <- function(Conductivity) {Conductivity * 0.00277777778}

# 2.9 Length Conversion

ft_to_m <- function(Length) {Length * 0.3048}
ft_to_cm <- function(Length) {Length * 30.48}
ft_to_mm <- function(Length) {Length * 304.8}
ft_to_in <- function(Length) {Length * 12}

m_to_ft <- function(Length) {Length / 0.3048}
m_to_in <- function(Length) {Length / 0.0254}
m_to_cm <- function(Length) {Length * 100}
m_to_mm <- function(Length) {Length * 1000}

cm_to_ft <- function(Length) {Length / 30.48}
cm_to_in <- function(Length) {Length / 2.54}
cm_to_m <- function(Length) {Length / 100}
cm_to_mm <- function(Length) {Length * 10}

mm_to_ft <- function(Length) {Length / 304.8}
mm_to_in <- function(Length) {Length / 25.4}
mm_to_m <- function(Length) {Length / 1000}
mm_to_cm <- function(Length) {Length / 10}

in_to_ft <- function(Length) {Length / 12}
in_to_m <- function(Length) {Length * 0.0254}
in_to_cm <- function(Length) {Length * 2.54}
in_to_mm <- function(Length) {Length * 25.4}

# 2.10 Area Conversion

ft2_to_m2 <- function(Area) {Area * 0.3048 ^ 2}
ft2_to_cm2 <- function(Area) {Area * 30.48 ^ 2}
ft2_to_mm2 <- function(Area) {Area * 304.8 ^ 2}
ft2_to_in2 <- function(Area) {Area * 12 ^ 2}

m2_to_ft2 <- function(Area) {Area / 0.3048 ^ 2}
m2_to_in2 <- function(Area) {Area / 0.0254 ^ 2}
m2_to_mm2 <- function(Area) {Area * 1000 ^ 2}
m2_to_cm2 <- function(Area) {Area * 100 ^ 2}

cm2_to_ft2 <- function(Area) {Area / 30.48 ^ 2}
cm2_to_in2 <- function(Area) {Area / 2.54 ^ 2}
cm2_to_m2 <- function(Area) {Area / 100 ^ 2}
cm2_to_mm2 <- function(Area) {Area * 10 ^ 2}

mm2_to_ft2 <- function(Area) {Area / 304.8 ^ 2}
mm2_to_in2 <- function(Area) {Area / 25.4 ^ 2}
mm2_to_m2 <- function(Area) {Area / 1000 ^ 2}
mm2_to_cm2 <- function(Area) {Area / 10 ^ 2}

in2_to_ft2 <- function(Area) {Area / 12 ^ 2}
in2_to_m2 <- function(Area) {Area * 0.0254 ^ 2}
in2_to_cm2 <- function(Area) {Area * 2.54 ^ 2}
in2_to_mm2 <- function(Area) {Area * 25.4 ^ 2}

# 2.11 Volume Conversion

ft3_to_m3 <- function(Volume) {Volume * 0.3048 ^ 3}
ft3_to_in3 <- function(Volume) {Volume * 12 ^ 3}
ft3_to_liter <- function(Volume) {(Volume * 0.3048) ^ 3 * 10 ^ 3}
ft3_to_USgal <- function(Volume) {Volume * 7.48051948}

m3_to_ft3 <- function(Volume) {Volume / 0.3048 ^ 3}
m3_to_in3 <- function(Volume) {Volume / 0.0254 ^ 3}
m3_to_USgal <- function(Volume) {(Volume / 0.3048) ^ 3 * 7.48051948}
m3_to_liter <- function(Volume) {Volume * 1000}

in3_to_ft3 <- function(Volume) {Volume / 12 ^ 3}
in3_to_m3 <- function(Volume) {Volume * 0.0254 ^ 3}
in3_to_USgal <- function(Volume) {Volume / 231}
in3_to_liter <- function(Volume) {(Volume * 0.0254) ^ 3 * 1000}

USgal_to_ft3 <- function(Volume) {Volume * 0.133680556}
USgal_to_m3 <- function(Volume) {1 / ((Volume / 0.3048) ^ 3 * 7.48051948)}
USgal_to_in3 <- function(Volume) {Volume * 231}
USgal_to_liter <- function(Volume) {1000 / ((Volume / 0.3048) ^ 3 * 7.48051948)}

liter_to_ft3 <- function(Volume) {(Volume / 0.3048) ^ 3 * 10 ^ (-3)}
liter_to_in3 <- function(Volume) {(Volume / 0.0254) ^ 3 * 10 ^ (-3)}
liter_to_USgal <- function(Volume) {(Volume / 0.3048) ^ 3 * 7.48051948 * 10 ^ (-3)}
liter_to_m3 <- function(Volume) {Volume * 0.001}

# 2.12 Mass Conversion

kg_to_lbm <- function(Mass) {Mass / 0.45359237}
kg_to_oz <- function(Mass) {Mass * 35.27}

lbm_to_kg <- function(Mass) {Mass * 0.45359237}
lbm_to_oz <- function(Mass) {(Mass / 35.27) / 0.45359237}

oz_to_kg <- function(Mass) {Mass / 35.27}
oz_to_lbm <- function(Mass) {(Mass * 0.45359237) * 35.27}

Kus_to_Kmetric <- function(K) {K * 0.45359237 / sqrt(0.070306958 / 0.0624279606)}

#
# 3. Flow Function
#

# Packing Coefficient
# *****************************************************************************

packing_K <- function(ttype, cp) {
#High-Low teeth
  if(ttype == 1) {
    if(cp <= 0.04) {(17000 * cp - 1640) * cp + 84}
    else if(cp < 0.08) {(3500 * cp - 535) * cp + 61.4}
    else{41}
  }
#Slant-Slant teeth
  else if(ttype == 2){
    if(cp <=0.05) {(-22212.12 * cp + 2149.72727) * cp + 29.25666667}
    else {81.21273}
  }
#Slant-Smooth teeth
  else if(ttype == 3){
    if(cp <= 0.05) {(31165.83 * cp - 2736.66833) * cp + 145.4688467}
    else if(cp < 0.085) {(-4476.19 * cp + 792.38095) * cp + 58.12142857}
    else {93.13333}
  }
#Straight-Smooth teeth
  else if(ttype == 4){
    if(cp <= 0.045) {(25071.43 * cp - 1765.8333) * cp + 145.7220238}
    else if(cp < 0.09) {(-6696.97 * cp + 1258.273) * cp + 73.968209}
    else {132.9673}
  }
# If type packing is greater than 10 set to packing coefficient
  else {ttype}
}


# Section Efficiency
# *****************************************************************************

section_eff <- function(var1, var2, var3, var4, calctype) {

# calc type: 1
# 일반적인 입출구 압력, 엔탈피로 효율 계산
# 엔탈피 대신 온도를 사용하고 싶다면 음수(Negative)로 입력

  if(calctype == 1){
    p1 <- var1

    if(var2 < 0){h1 <- stmPTH(p1, -var2)}
    else{h1 <- var2}

    p2 <- var3

    if(var4 < 0){h2 <- stmPTH(p2, -var4)}
    else{h2 <- var4}

    AE <- h1 - stmPSH(p2, stmPHS(p1, h1))
    UE <- h1 - h2

    return(UE/AE)
  }

# calc type: 2
# 입구 압력, 엔탈피, 출구 압력, 효율을 이용하여 출구 엔탈피를 찾음

  else if(calctype == 2){
    p1 <- var1

    if(var2 < 0){h1 <- stmPTH(p1, -var2)}
    else{h1 <- var2}

    p2 <- var3

    efficiency <- var4
    entropy <- stmPHS(p1, h1)

    h2 <- h1 - (h1 - stmPSH(p2, entropy)) * efficiency

    return(h2)
  }

# calc type: 3
# 입출구 압력, 출구 엔탈피(온도), 효율을 이용하여 입구 엔탈피를 찾음

  else if(calctype == 3){
    p1 <- var1
    p2 <- var2

    if(var3 < 0){h2 <- stmPTH(p2, -var3)}
    else{h2 <- var3}

    efficiency <- var4

    assumed_h1 <- h2 + 400
    deviation <- 1.0
    no_iteration <- 1

    while(abs(deviation) > 0.0001){
      s1 <- stmPHS(p1, assumed_h1)
      isen_h <- stmPSH(p2, s1)
      UE1 <- assumed_h1 - h2
      UE2 <- (assumed_h1 - isen_h) * efficiency
      deviation <- UE1 - UE2

      if(deviation > 0){assumed_h1 <- assumed_h1 - abs(deviation)/2}
      else{assumed_h1 <- assumed_h1 + abs(deviation)/2}

      if(no_iteration > 100){return("반복계산 횟수 초과")}
      else{no_iteration <- no_iteration + 1}
    }

    return(assumed_h1)
  }
  else {return("CalcType은 1,2,3 중 하나를 선택할 수 있습니다.")}
}

# Flow capacity
# *****************************************************************************

k_factor <- function(flow,p,h) {flow / sqrt((p/stmPHV(p,h)))} # flow[kg/hr]

# Packing flow
# *****************************************************************************

packing_flow <- function(k,p,h) {k * sqrt((p/stmPHV(p,h)))} # flow[kg/hr]

# Annulus Velocity
# 사용금지: 습분영역 비체적 계산 못함. Dry Specific Volume만 계산됨.

VAN <- function(flow, p, h, area) {
  v <- stmPHV(p,h)
  flow * v / (3600 * area) # 1hr = 3600sec, flow[kg/hr]
}































