$PROBLEM From bbr: see 101.yaml for details

$INPUT C NUM ID TIME SEQ CMT EVID AMT DV AGE WT HT EGFR ALB BMI SEX AAG
       SCR AST ALT CP TAFD TAD LDOS MDV BLQ PHASE

$DATA ../../../data/derived/analysis3.csv IGNORE=(C='C', BLQ=1)

$SUBROUTINE ADVAN4 TRANS4

$PK
 
;log transformed PK parms
 
KA   = EXP(THETA(1)+ETA(1))
V2   = EXP(THETA(2)+ETA(2))
CL   = EXP(THETA(3)+ETA(3))
V3   = EXP(THETA(4))
Q    = EXP(THETA(5))

S2 = V2/1000 ; dose in mcg, conc in mcg/mL

$ERROR
IPRED = F 
Y=IPRED*(1+EPS(1))

$THETA  ; log values
(0.5)   ;  1 KA (1/hr) - 1.5
(3.5)   ;  2 V2 (L) - 60
(1)     ;  3 CL (L/hr) - 3.5
(4)     ;  4 V3 (L) - 70
(2)     ;  5 Q  (L/hr) - 4


$OMEGA BLOCK(3)
0.2   ;ETA(KA)
0.01 0.2   ;ETA(V2)
0.01 0.01 0.2   ;ETA(CL)


$SIGMA
0.05     ; 1 pro error

$EST MAXEVAL=9999 METHOD=1 INTER SIGL=6 NSIG=3 PRINT=1 MSFO=./101.msf 
$COV PRINT=E
$TABLE NUM CL V2 Q V3 KA ETAS(1:LAST) IPRED NPDE CWRES NOPRINT ONEHEADER FILE=101.tab
$TABLE NUM CL V2 Q V3 KA ETAS(1:LAST) NOAPPEND NOPRINT ONEHEADER FILE=101par.tab