[ prob ]
106-104 + COV-effects(CRCL, AGE) on CL

[ pkmodel ] cmt = "GUT,CENT,PERIPH", depot = TRUE

[ param ] 
WT   = 70
EGFR = 90
ALB  = 4.5
AGE  = 35

[ nmxml ] 
path = "../../model/pk/106/106.xml"
root = "cppfile"

[ main ] 
double V2WT   = log(WT/70);
double CLWT   = log(WT/70)*0.75;
double CLEGFR = log(EGFR/90)*THETA6;
double CLAGE  = log(AGE/35)*THETA7;
double V3WT   = log(WT/70);
double QWT    = log(WT/70)*0.75;
double CLALB  = log(ALB/4.5)*THETA8;

double KA  = exp(THETA1+ETA(1));
capture V2  = exp(THETA2+V2WT+ETA(2));
capture CL  = exp(THETA3+CLWT+CLEGFR+CLAGE+CLALB+ETA(3));
double V3  = exp(THETA4+V3WT);
double Q   = exp(THETA5+QWT);

double S2 = V2/1000; //; dose in mcg, conc in mcg/mL

[ table ] 
capture IPRED = CENT/S2;
capture Y = IPRED * (1+EPS(1));
