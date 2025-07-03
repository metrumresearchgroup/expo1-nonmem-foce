[ prob ]
106-104 + COV-effects(CRCL, AGE) on CL

This model requires mrgsolve >= 1.0.3

[ plugin ] autodec nm-vars

[ pkmodel ] cmt = "GUT,CENT,PERIPH", depot = TRUE

[ input ] 
WT   = 70
EGFR = 90
ALB  = 4.5
AGE  = 35
DOSE = 25

[ nmxml ] 
path = "../../model/pk/106/106.xml"
root = "cppfile"

[ pk ] 
V2WT   = LOG(WT/70.0);
CLWT   = LOG(WT/70.0)*0.75;
CLEGFR = LOG(EGFR/90.0)*THETA(6);
CLAGE  = LOG(AGE/35.0)*THETA(7);
V3WT   = LOG(WT/70.0);
QWT    = LOG(WT/70.0)*0.75;
CLALB  = LOG(ALB/4.5)*THETA(8);

KA  = EXP(THETA(1) + ETA(1));
V2  = EXP(THETA(2) + V2WT + ETA(2));
CL  = EXP(THETA(3) + CLWT + CLEGFR + CLAGE + CLALB + ETA(3));
V3  = EXP(THETA(4) + V3WT);
Q   = EXP(THETA(5) + QWT);

S2 = V2/1000.0; //; dose in mcg, conc in mcg/mL

[ error ] 
IPRED = CENT/S2;
Y = IPRED * (1+EPS(1));
capture AUC = DOSE/CL;

[ capture ] CL V2 IPRED Y
