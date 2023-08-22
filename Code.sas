Code 
*Part 1;
libname thu '/home/u62275281';
filename in1 '/home/u62275281/baby_s2023.txt';
filename in2 '/home/u62275281/mom_s2023.txt';
filename in3 '/home/u62275281/dad_s2023.txt';
data thu.baby;
infile in1;
input ID Hc Leng Bwt Gage;
array  miss{4} Hc Leng Bwt Gage;
do i=1 to 4;
if miss{i}= -1 then miss{i}=.;
end;
drop i;
run;
data thu.mom;
infile in2;
input ID Matage Matsmk Matht Matwt;
if  Matsmk= -1 then Matsmk=.;
run;
data thu.dad;
infile in3;
input ID Patage Pated Patsmk Patht; 
if Patsmk = -1 then Patsmk=.;
run;

proc sort data=thu.baby; by id; run; 
proc sort data=thu.mom; by id; run; 
proc sort data=thu.dad; by id; run; 
data whole;      
merge thu.baby thu.mom thu.dad;    
  by id;    
  BWTG=Bwt*453.6 ;
 run; 
 proc means data=whole mean std; 
 var BWTG;
 output out=whole2 (drop=_type_ _freq_) mean=MEAN_BWTG std=SD_BWTG ;
 run;

data  thu.projdata1;
if _n_ eq 1 then set whole2;
set whole;

run;
proc print  data=thu.projdata1;
var ID Hc Leng Bwt Gage Matage Matsmk Matht Matwt Patage Pated Patsmk Patht MEAN_BWTG SD_BWTG;
run;

data  thu.projdata1;
set thu.projdata1;
BWTG=Bwt*453.6 ;
Z_BWTG=((BWTG-MEAN_BWTG)/SD_BWTG);
if Z_BWTG=. then SMALL_BWT=.;
if Z_BWTG<=-1.28 then SMALL_BWT=1;
else if  Z_BWTG>-1.28 then SMALL_BWT=0;


if matsmk=0 then MATSMK_CAT=1;
else if 1<=matsmk<=20 then MATSMK_CAT=2;
else if matsmk>20 then MATSMK_CAT=3;


if (0 <= matsmk <= 18) then smk1=matsmk; 
else if matsmk > 18 then smk1=18;
if (0 <= matsmk <= 18) then smk2=18;
else if matsmk > 18 then smk2=matsmk ;

if (0 <= matsmk <= 18) then smkgroup='s1';
 else if (matsmk > 18) then smkgroup='s2';
 

 
if Patsmk=0 then PATSMK_CAT=1;
else if 1<=Patsmk<=20 then PATSMK_CAT=2;
else if Patsmk>20 then PATSMK_CAT=3;
Matwtkg=matwt*0.4536;
mathtm=Matht*0.0254;
MATBMI=(Matwtkg/mathtm**2);

run;

proc print data=thu.projdata1;
run;
*part2;
proc means data=thu.projdata1 n mean median std min max maxdec=2;
var Bwt hc leng z_bwtg gage matage matbmi patage pated patht;
run;
proc univariate data=thu.projdata1 ;
var Bwt hc leng z_bwtg gage matage matbmi patage pated patht;
run;
proc freq data=thu.projdata1;
tables small_bwt matsmk_cat
patsmk_cat;
run;

*part3;
*Q1;


PROC glm DATA=thu.projdata1;
class matsmk_cat (ref='1');
MODEL Z_BWTG=matsmk_cat/ solution clparm;
run;

PROC glm DATA=thu.projdata1;
class matsmk_cat (ref='1');
MODEL Z_BWTG=matsmk_cat;
means matsmk_cat  ;
lsmeans matsmk_cat / tdiff adjust=tukey stderr ;
run;

*q2;
PROC glm DATA=thu.projdata1;
class matsmk_cat (ref='1') patsmk_cat (ref='1');
MODEL Z_BWTG=matsmk_cat patsmk_cat matsmk_cat*patsmk_cat/ solution;
means matsmk_cat*patsmk_cat;
lsmeans matsmk_cat*patsmk_cat/ stderr ;
run;

PROC glm DATA=thu.projdata1;
class matsmk_cat (ref='1') patsmk_cat (ref='1');
MODEL Z_BWTG=matsmk_cat patsmk_cat/ solution;
means matsmk_cat patsmk_cat ;
lsmeans matsmk_cat patsmk_cat / tdiff adjust=tukey stderr ;
run;

*Q3;
proc reg DATA=thu.projdata1;
model Z_BWTG=matsmk_cat matbmi;
run;
PROC glm DATA=thu.projdata1;
class matsmk_cat (ref='1');
MODEL Z_BWTG=matsmk_cat matbmi matsmk_cat*matbmi / solution;
run;


PROC glm DATA=thu.projdata1;
class matsmk_cat (ref='1');
MODEL Z_BWTG=matsmk_cat matbmi / solution ;
run;
proc corr DATA=thu.projdata1;
var matsmk_cat matbmi;
run; 

*q4;

proc reg DATA=thu.projdata1;
MODEL Z_BWTG= MATBMI ;
run;

proc plot data=thu.projdata1;
plot Z_BWTG*matbmi;
run;
proc sgplot data=thu.projdata1;
scatter x=matbmi y=Z_BWTG;
run;

proc rank groups=4 data=thu.projdata1 out=two;
var matbmi;
ranks rmatbmi;
run;
proc sort data=two;
by rmatbmi;
run;
proc univariate plot data=two;
by rmatbmi;
var Z_BWTG;
title1 'Boxplots of Z score by matbmi';
run;


proc rank groups=10 data=thu.projdata1 out=three;
var matbmi;
ranks rmatbmi;
run;
proc sort data=three;
by rmatbmi;
run;
proc means data=three noprint;
by rmatbmi;
var  Z_BWTG matbmi;
output out=str mean=zmean matbmimean;
run;
proc plot data=str ;
plot zmean*matbmimean='M';
title1 'Plot of Decile Means for BMI';
run;

*q5;
proc glm DATA=thu.projdata1;
class matsmk_cat (ref='1') patsmk_cat (ref='1');
MODEL Z_BWTG= gage matage matbmi patage pated patht  matsmk_cat patsmk_cat/ solution clparm ;
run;
proc reg DATA=thu.projdata1;
MODEL Z_BWTG= gage matage matbmi patage pated patht  matsmk_cat patsmk_cat/ cli ;
run;

 proc reg data=thu.projdata1;
model Z_BWTG= gage matage matbmi patage pated patht  matsmk_cat patsmk_cat / r;
id id;
output out=tw pred=p_zbwtg student=r_zbwtg cookd=cooksdistance;
title1 'Checking cooks distance';
run;


proc plot data=tw;
plot r_zbwtg*p_zbwtg;
title1 'Residual Plot';
run;
proc univariate plots normal data=tw;
id id;
var r_zbwtg;
title1 'Normality of Residuals';
run;

proc sgplot data=tw;
scatter x=p_zbwtg y=r_zbwtg;
xaxis label='Predicted z score';
yaxis label='Studentized residuals';
run;
DATA outliers;
  SET tw;
  WHERE r_zbwtg>= 2 or r_zbwtg<-2 ;
RUN;
proc print data=outliers;
var id r_zbwtg  ;
run;
DATA influencepoints;
  SET tw;
  WHERE cooksdistance> 0.006;
RUN;
proc print data=influencepoints;
var id  r_zbwtg cooksdistance;
run;


proc reg DATA=thu.projdata1;
MODEL Z_BWTG= gage matage matbmi patage pated patht  matsmk_cat patsmk_cat
/ tol vif collinoint;
run;


*q6;
proc means data=thu.projdata1;
 class smkgroup;
 var  Z_BWTG;
 run;
proc glm data=thu.projdata1;
class smkgroup;
model  Z_BWTG = smkgroup;
run;

proc reg data=thu.projdata1;
model Z_BWTG = smk1 smk2 / stb; 
output out=stats pred=yhat; 
test smk1= smk2;  
run;
proc sort data=stats; by matsmk; run;
symbol1 value=dot color=black; symbol2 line=1 interpol=join color=black;
proc gplot data=stats; plot Z_BWTG*matsmk yhat*matsmk / overlay; run;

proc glm data=thu.projdata1;
model Z_BWTG = gage matage matbmi patage pated patht patsmk_cat smk1 smk2 / solution; 
run;



