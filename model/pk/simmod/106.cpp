// $GLOBAL
// #define CP (CENT/VC)
// #define CT (PERIPH/VP)


  
$PROB 
- Author: APerson
- Client: ACompany
- Date: `r Sys.Date()`
- NONMEM Run: 106
- Structure: two compartment, linear CL, IV.
- Implementation: ODE
- Error model: Proportional 
// - Covariates:
// -- CL - WT, CRCL, ALB,AGE.
// -- VC - WT
// -- Q - WT
// -- VP - WT
// - Random effects on: `CL`, `VC`, `KA`

[ PKMODEL ] cmt = "GUT CENT PERIPH", depot = TRUE

[ PARAM ]  
WT=70.9, AGE=33.8,CRCL = 88.7, ALB=4.3

$NMXML
project = ".."
root = "cppfile"
run = 106 //should this be changed since it is based on multiple nonmem ctl


// $ODE
// dxdt_GUT = -KA*GUT;
// dxdt_CENT = KA*GUT - (CL+Q)*CP  + Q*CT;
// dxdt_PERIPH = Q*CP - Q*CT;

//double
    

$MAIN

double V2WT  = log(WT/70);
double CLWT  = log(WT/70)*0.75;
double CLCR = log(CRCL/90)*THETA6;
double CLAGE = log(AGE/35)*THETA7;
double V3WT  = log(WT/70);
double QWT   = log(WT/70)*0.75;
double CLALB = log(ALB/4.5)*THETA8;

double  KA   = exp(THETA1+ETA(1));
double  V2   = exp(THETA2+V2WT+ETA(2));
double  CL   = exp(THETA3+CLWT+CLCR+CLAGE+CLALB+ETA(3));
double  V3   = exp(THETA4+V3WT);
double  Q    = exp(THETA5+QWT);  

double S2    = V2/1000; //; dose in mcg, conc in mcg/mL
  
  



// $INIT 
//   GUT = 0
//   CENT  = 0 
//   PERIPH  = 0  
  
$SET delta=0.5



[ TABLE ] 
double F = CENT/S2;
double Y=F*(1+EPS(1)); 
capture IPRED=F; 

[ CAPTURE ]
CL Q V2 V3 KA ETA(1) ETA(2) ETA(3) IPRED

// $TABLE
//
// capture DV = CP*(1 +EPS(1)) ;
// capture CLI = CL;
// capture CPI = CP;
// 
// 
// $CAPTURE @annotated 
// CP : Plasma concentration (mass/volume)
// VCI= VC: Central Volume