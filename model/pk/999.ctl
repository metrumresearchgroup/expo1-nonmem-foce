$PROBLEM 999 foo

$INPUT C NUM ID TIME SEQ CMT EVID AMT DV AGE WT HT EGFR ALB BMI SEX AAG
       SCR AST ALT CP TAFD TAD LDOS MDV BLQ PHASE

$DATA ../../../data/derived/analysis3.csv IGNORE=(C='C', BLQ=1)

$SUBROUTINE ADVAN4 TRANS4

$PK
 
;log transformed PK parms
 
V2WT = LOG(WT/70)
CLWT = LOG(WT/70)*0.75
CLEGFR = LOG(EGFR/90)*THETA(6)
CLAGE = LOG(AGE/35)*THETA(7)
V3WT = LOG(WT/70)
QWT  = LOG(WT/70)*0.75
CLALB = LOG(ALB/4.5)*THETA(8)


KA   = EXP(THETA(1)+ETA(1))
V2   = EXP(THETA(2)+V2WT+ETA(2))
CL   = EXP(THETA(3)+CLWT+CLEGFR+CLAGE+CLALB+ETA(3))
V3   = EXP(THETA(4)+V3WT)
Q    = EXP(THETA(5)+QWT) 

S2 = V2/1000 ; dose in mcg, conc in mcg/mL

$ERROR
IPRED = F 
Y=IPRED*(1+EPS(1))
REPI = IREP

$THETA  ; log values
(0.5)   ;  1 KA (1/hr) - 1.5
(3.5)   ;  2 V2 (L) - 60
(1)     ;  3 CL (L/hr) - 3.5
(4)     ;  4 V3 (L) - 70
(2)     ;  5 Q  (L/hr) - 4
(1)     ;  6 CLEGFR~CL ()
(1)     ;  7 AGE~CL ()
(0.5)   ;  8 ALB~CL ()

$OMEGA BLOCK(3)
0.2   ;ETA(KA)
0.01 0.2   ;ETA(V2)
0.01 0.01 0.2   ;ETA(CL)

$SIGMA
0.05     ; 1 pro error

$SIM (1223432) SUBPROBLEMS = 500 
$TABLE REPI IPRED CL V2 V3 KA FILE = 999.tab