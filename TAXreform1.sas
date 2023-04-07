/**********************************************************************************/
/* Project Tax Reform ACT germany*/
/**********************************************************************************/
/* Data cleaning*/
/* Calculate marginal tax rate */
/* Sorting data */
/* Merge different data set*/ 
/* Ceate New variables */
/* Summary Statistics */
/* DiD regression */
/* Cross Tabulation*/ 


/********************************************************************************/
/********************************************************************************/




/* describe year taxableIncome taxBeforeAdjustments taxAfterAdjustments */

proc means data=TAXreform var Mean Sum;
  var year taxableIncome taxBeforeAdjustments taxAfterAdjustments;
run;

/* drop if taxableIncome==. */
data TAXreform;
  set TAXreform;
  if taxableIncome = . then delete;
run;

/* drop if taxableIncome==0 */
data TAXreform;
  set TAXreform;
  if taxableIncome = 0 then delete;
run;

/* order rank group5 MTaxR taxableIncome income0 income15 income43 income46 totaltax */
proc sort data=TAXreform;
  by MTaxR taxableIncome;
run;

/* generate income0=0
   replace income0= taxableIncome if taxableIncome<9000
   replace income0= 9000 if taxableIncome>=9000 */
data TAXreform;
  set TAXreform;
  if taxableIncome < 9000 then income0 = taxableIncome;
  else if taxableIncome >= 9000 then income0 = 9000;
run;

/* generate income15=0
   replace income15= (taxableIncome-9000) if (taxableIncome >9000 & taxableIncome  <54950)
   replace income15= (54950-9000) if taxableIncome>54950 */
data TAXreform;
  set TAXreform;
  if (9000 < taxableIncome < 54950) then income15 = taxableIncome - 9000;
  else if (taxableIncome >= 54950) then income15 = 45950;
run;

/* generate income43=0
   replace income43= taxableIncome-54949 if (taxableIncome >54949 & taxableIncome  <260533)
   replace income43= 205582 if taxableIncome >260533 */
data TAXreform;
  set TAXreform;
  if (54949 < taxableIncome < 260533) then income43 = taxableIncome - 54949;
  else if (taxableIncome >= 260533) then income43 = 205584;
run;

/* generate income46=0
   replace income46= taxableIncome-260532 if (taxableIncome >260532) */
data TAXreform;
  set TAXreform;
  if (taxableIncome > 260532) then income46 = taxableIncome - 260532;
run;

/* generate totaltax=income0*0+income15*.14+income43*.42+income46*.45 */
data TAXreform;
  set TAXreform;
  totaltax = income0*0 + income15*.14 + income43*.42 + income46*.45;
run;

/* generate MTaxR= (totaltax/taxableIncome)*100 */
data TAXreform;
  set TAXreform;
  MTaxR = (totaltax / taxableIncome) * 100;
run;

/* sort MTaxR */
proc sort data=TAXreform;
  by MTaxR;
run;

/* sort taxableIncome */
proc sort data=TAXreform;
  by taxableIncome;
run;
/* describe year taxableIncome taxBeforeAdjustments taxAfterAdjustments */
proc means data=TAXreform var Mean Sum;
  var year taxableIncome taxBeforeAdjustments taxAfterAdjustments;
run;

/* drop if taxableIncome==. */
data TAXreform;
  set TAXreform;
  if taxableIncome = . then delete;
run;

/* drop if taxableIncome==0 */
data TAXreform;
  set TAXreform;
  if taxableIncome = 0 then delete;
run;

/* order rank group5 MTaxR taxableIncome income0 income15 income43 income46 totaltax */
proc sort data=TAXreform;
  by MTaxR taxableIncome;
run;

/* generate income0=0
   replace income0= taxableIncome if taxableIncome<9000
   replace income0= 9000 if taxableIncome>=9000 */
data TAXreform;
  set TAXreform;
  if taxableIncome < 9000 then income0 = taxableIncome;
  else if taxableIncome >= 9000 then income0 = 9000;
run;

/* generate income15=0
   replace income15= (taxableIncome-9000) if (taxableIncome >9000 & taxableIncome  <54950)
   replace income15= (54950-9000) if taxableIncome>54950 */
data TAXreform;
  set TAXreform;
  if (9000 < taxableIncome < 54950) then income15 = taxableIncome - 9000;
  else if (taxableIncome >= 54950) then income15 = 45950;
run;

/* generate income43=0
   replace income43= taxableIncome-54949 if (taxableIncome >54949 & taxableIncome  <260533)
   replace income43= 205582 if taxableIncome >260533 */
data TAXreform;
  set TAXreform;
  if (54949 < taxableIncome < 260533) then income43 = taxableIncome - 54949;
  else if (taxableIncome >= 260533) then income43 = 205584;
run;

/* generate income46=0
   replace income46= taxableIncome-260532 if (taxableIncome >260532) */
data TAXreform;
  set TAXreform;
  if (taxableIncome > 260532) then income46 = taxableIncome - 260532;
run;

/* generate totaltax=income0*0+income15*.14+income43*.42+income46*.45 */
data TAXreform;
  set TAXreform;
  totaltax = income0*0 + income15*.14 + income43*.42 + income46*.45;
run;

/* generate MTaxR= (totaltax/taxableIncome)*100 */
data TAXreform;
  set TAXreform;
  MTaxR = (totaltax / taxableIncome) * 100;
run;

/* sort MTaxR */
proc sort data=TAXreform;
  by MTaxR;
run;

/* sort taxableIncome */
proc sort data=TAXreform;
  by taxableIncome;
run;

********************* /* generate variable */********************************;
proc rank data=TAXreform ties=mean out=TAXreform_rank groups=5;
  var MTaxR;
  ranks rank;
run;

data TAXreform;
  set TAXreform_rank;
  group5 = ceil(rank/5);
run;


********************* /* DID regression */*********************************;

data TAXreform;
  set TAXreform;
  if year=2001 then postperiod=1;
  else if year=1998 then postperiod=0;
  if group5>=4 then treatment=1;
  else if group5<=3 then treatment=0;
  did = group5*postperiod;
  did1 = treatment*postperiod;
run;

proc reg data=TAXreform;
  model MTaxR = postperiod#group5;
run;

proc reg data=TAXreform;
  model MTaxR = group5 postperiod did;
run;

proc reg data=TAXreform;
  model MTaxR = postperiod#MTRcat;
run;

proc reg data=TAXreform;
  model MTaxR = MTRcat postperiod did;
run;

proc reg data=TAXreform;
  model MTaxR = postperiod#treatment;
run;

proc reg data=TAXreform;
  model MTaxR = group5 postperiod did1;
run;


********************* /*Collapse */*********************************;

proc means data=TAXreform sum;
  var MTaxR taxableIncome;
  class group5;
  output out=TAXreform_group5_sum sum=;
run;

proc means data=TAXreform sum;
  var MTaxR taxableIncome;
  class year;
  output out=TAXreform_year_sum sum=;
run;

proc means data=TAXreform sum;
  var MTaxR taxableIncome;
  class year group5;
  output out=TAXreform_year_group5_sum sum=;
run;

proc means data=TAXreform sum;
  var MTaxR taxBeforeAdjustments taxAfterAdjustments;
  class year group5;
  output out=TAXreform_year_group5_tax_sum sum=;
run;

data TAXreform;
  set TAXreform;
  if MTaxR=0 then MTRcat=0;
  else if 0<MTaxR<=14 then MTRcat=14;
  else if 14<MTaxR<=30 then MTRcat=30;
  else if 30<MTaxR<=42 then MTRcat=42;
  else if MTaxR>42 then MTRcat=45;
run;

proc freq data=TAXreform;
  tables MTRcat postperiod / nocum nopercent nocol norow;
run;

proc means data=TAXreform sum;
  var taxBeforeAdjustments taxAfterAdjustments;
  class year group5 MTRcat1;
  output out=TAXreform_year_group5_MTRcat1_tax_sum sum=;
run;

proc means data=TAXreform sum;
  var taxableIncome;
  class year group5 MTRcat;
  output out=TAXreform_year_group5_MTRcat_income_sum sum=;
run;

proc means data=TAXreform sum;
  var MTaxR taxableIncome;
  class MTRcat year;
  output out=TAXreform_MTRcat_year_sum sum=;
run;



/* Drop Missing and zero taxable Income */
data TAXreform;
    set TAXreform;
    if taxableIncome=. then delete;
    if taxableIncome=0 then delete;
run;

/* Generate v98 and v2001 */
data TAXreform;
    set TAXreform;
    v98=ifn(year=1998,taxableIncome,.);
    v2001=ifn(year=2001,taxableIncome,.);
    TaxLi98=ifn(year=1998,taxAfterAdjustments,.);
    TaxLi2001=ifn(year=2001,taxAfterAdjustments,.);
run;

/* Generate v98N and v2001N */
data TAXreform;
    set TAXreform;
    v98N=v98;
    if v98=. then v98N=0;
    v2001N=v2001;
    if v2001=. then v2001N=0;
run;

/* Generate Marginal Tax Rate 1998 */
data TAXreform;
    set TAXreform;

    /* Calculate income12DM */
    income12DM=ifn(v98N<12365,v98N,ifn(v98N>=12365,12365,.));
    income58DM=ifn(v98N >12365 & v98N <58643,(v98N-12365),ifn(v98N>58643,(58643-12365),.));
    Y2E=sum(income12DM, income58DM);
    Y2E1=ifn(Y2E>12365,Y2E,.);
    INC=ifn(Y2E1>12365,(Y2E1-12365)/10000,.);
    inc1=ifn(INC>0,((91.19*INC+2590)*INC),.);
    income120DM=ifn(v98N >58643 & v98N  <120041,(v98N-58643),ifn(v98N>120041,61398,.));
    Y3E=sum(Y2E, income120DM);
    Y3E1=ifn(Y3E>58643,Y3E,.);
    INC1=ifn(Y3E1>58643,(Y3E1-58643)/10000,.);
    inc2=ifn(INC1>0,((151.96*INC1+3434)*INC1),.);
    income120above=ifn(v98N>120041,(v98N-120041),.);
    inc3=ifn(income120above>0,(0.53*income120above-22843),.);
    taxL=sum(inc1, inc2, inc3);
    taxL1=sum(inc1, inc2, inc3);
    inc3N=ifn(inc3>0,inc3,.);
    MTR98=ifn(v98N>0,(taxL/v98N)*100,.);
    MTR98N=ifn(MTR98>0,MTR98,.);
run;


/* Drop missing and zero taxable income */
data TAXreform;
    set TAXreform;
    if taxableIncome=. then delete;
    if taxableIncome=0 then delete;
run;

/* Generate marginal tax rate for 1998 */
data TAXreform;
    set TAXreform;
    if year=1998 then do;
        v98=taxableIncome;
        TaxLi98=taxAfterAdjustments;

        v98N=v98;
        if v98=. then v98N=0;

        income12DM=0;
        if v98N<12365 then income12DM=v98N;
        else if v98N>=12365 then income12DM=12365;
        else income12DM=0;

        income58DM=0;
        if v98N>12365 and v98N<58643 then income58DM=v98N-12365;
        else if v98N>=58643 then income58DM=58643-12365;
        else income58DM=0;

        Y2E=income12DM+income58DM;

        Y2E1=Y2E;
        if Y2E>12365 then Y2E1=Y2E;
        else Y2E1=0;

        INC=(Y2E1-12365)/10000;

        inc1=(91.19*INC+2590)*INC;

        income120DM=0;
        if v98N>58643 and v98N<120041 then income120DM=v98N-58643;
        else if v98N>=120041 then income120DM=61398;
        else income120DM=0;

        Y3E=Y2E+income120DM;

        Y3E1=Y3E;
        if Y3E>58643 then Y3E1=Y3E;
        else Y3E1=0;

        INC1=(Y3E1-58643)/10000;

        inc2=(151.96*INC1+3434)*INC1;

        income120above=0;
        if v98N>120041 then income120above=v98N-120041;
        else income120above=0;

        inc3=0.53*income120above-22843;

        taxL=inc1+inc2+inc3;

        MTR98=(taxL/v98N)*100;

        if MTR98=. then MTR98=0;

        MTR98N=MTR98;
        if MTR98=. then MTR98N=0;
    end;
run;

/* Generate marginal tax rate for 2001 */
data TAXreform;
    set TAXreform;
    if year=2001 then do;
        v2001=taxableIncome;
        TaxLi2001=taxAfterAdjustments;

        v2001N=v2001;
        if v2001=. then v2001N=0;

        income14DM=0;
        if v2001N<14093 then income14DM=v2001N;
        else if v2001N>=14093 then income14DM=14093;

        income18DM=0;
        if v2001N>14093 and v2001N<18089 then income18DM=v2001N-14093;
        else if v2001N>=18089 then income18DM=18089-14093;

        T2E=income14DM+income18DM;

        T2E1=T2E;
        if T2E>14093 then T


data TAXreform;
    set TAXreform;

    /* Drop missing and zero taxable income */
    if taxableIncome=. or taxableIncome=0 then delete;

    /* Generate variables for 1998 and 2001 */
    v98 = ifn(year=1998, taxableIncome, .);
    v2001 = ifn(year=2001, taxableIncome, .);
    TaxLi98 = ifn(year=1998, taxAfterAdjustments, .);
    TaxLi2001 = ifn(year=2001, taxAfterAdjustments, .);

    /* Replace missing values with 0 */
    v98N = ifn(missing(v98), 0, v98);
    v2001N = ifn(missing(v2001), 0, v2001);

    /* Generate marginal tax rate for 1998 */
    income12DM = ifn(v98N<12365, v98N, 12365);
    income12DM = ifn(v98N=. or income12DM=., 0, income12DM);
    income58DM = ifn(v98N>12365 & v98N<58643, v98N-12365, 0);
    income58DM = ifn(v98N>58643, 46278, income58DM);
    income58DM = ifn(v98N=. or income58DM=., 0, income58DM);
    Y2E = sum(income12DM, income58DM);
    Y2E1 = ifn(Y2E>12365, Y2E, 0);
    Y2E1 = ifn(Y2E=. or Y2E1=., 0, Y2E1);
    INC = ifn(Y2E1>12365, (Y2E1-12365)/10000, 0);
    inc1 = ifn(INC>0, (91.19*INC+2590)*INC, 0);
    income120DM = ifn(v98N>58643 & v98N<120041, v98N-58643, 0);
    income120DM = ifn(v98N>120041, 58698, income120DM);
    income120DM = ifn(v98N=. or income120DM=., 0, income120DM);
    Y3E = sum(Y2E, income120DM);
    Y3E = ifn(Y3E=., 0, Y3E);
    Y3E1 = ifn(Y3E>58643, Y3E, 0);
    INC1 = ifn(Y3E1>58643, (Y3E1-58643)/10000, 0);
    inc2 = ifn(INC1>0, (151.96*INC1+3434)*INC1, 0);
    income120above = ifn(v98N>120041, v98N-120041, 0);
    income120above = ifn(v98N=., 0, income120above);
    inc3 = ifn(0.53*income120above>=22843, 0.53*income120above-22843, 0);
    taxL = sum(inc1, inc2, inc3);
    taxL1 = sum(inc1, inc2, inc3);
    inc3N = ifn(inc3=., 0, inc3);
    MTR98 = ifn(v98N>0, (taxL/v98N)*100, 


/* Marginal Tax Rate 2001 */

/* Generate income14DM */
data TAXreform;
    set TAXreform;
    income14DM = 0;
    if v2001N < 14093 then income14DM = v2001N;
    else if v2001N >= 14093 then income14DM = 14093;
run;

/* Generate income18DM */
data TAXreform;
    set TAXreform;
    income18DM = 0;
    if v2001N > 14093 and v2001N < 18089 then income18DM = v2001 - 14093;
    else if v2001N >= 18089 then income18DM = 18089 - 14093;
run;

/* Generate T2E */
data TAXreform;
    set TAXreform;
    T2E = income14DM + income18DM;
run;

/* Generate T2E1 */
data TAXreform;
    set TAXreform;
    T2E1 = 0;
    if T2E > 14093 then T2E1 = T2E;
run;

/* Generate TINC and Tinc1 */
data TAXreform;
    set TAXreform;
    TINC = (T2E1 - 14093) / 10000;
    Tinc1 = (387.89 * TINC + 1990) * TINC;
run;

/* Generate income107DM */
data TAXreform;
    set TAXreform;
    income107DM = 0;
    if v2001N > 18089 and v2001N < 107567 then income107DM = v2001N - 18089;
    else if v2001N >= 107567 then income107DM = 89474;
run;

/* Generate T3E */
data TAXreform;
    set TAXreform;
    T3E = income107DM + T2E;
run;

/* Generate T3E1 */
data TAXreform;
    set TAXreform;
    T3E1 = 0;
    if T3E > 18089 then T3E1 = T3E;
run;

/* Generate TINC1 and Tinc2 */
data TAXreform;
    set TAXreform;
    TINC1 = (T3E1 - 18089) / 10000;
    Tinc2 = (142.49 * TINC1 + 2300) * TINC1 + 857;
run;

/* Generate income107above and Tinc3 */
data TAXreform;
    set TAXreform;
    income107above = 0;
    if v2001N > 107568 then income107above = v2001N - 107568;
    Tinc3 = 0.485 * income107above -

/* Summary Statistics */

proc means data=TAXreform sum;
var taxableIncome taxBeforeAdjustments;
run;

proc freq data=TAXreform;
tables children alter_a alter_b female year;
run;

proc means data=TAXreform sum;
class female year;
var taxAfterAdjustments taxBeforeAdjustments;
run;

/* Tax Before and After Adjustments */

proc means data=TAXreform sum;
var taxAfterAdjustments taxBeforeAdjustments;
by year;
run;

proc freq data=TAXreform;
tables MTRcat year;
summarize taxableIncome /table= MTRcat year;
run;

proc freq data=TAXreform;
tables MTRcat1 year;
summarize taxAfterAdjustments /table= MTRcat1 year;
summarize taxBeforeAdjustments /table= MTRcat1 year;
run;

/* AddMTR and Taxable Income */

proc freq data=TAXreform;
tables addMTR qaddMTR;
run;

proc means data=TAXreform sum;
var addMTR;
where qaddMTR=1;
run;

proc means data=TAXreform sum;
var addMTR;
where qaddMTR=2;
run;

proc means data=TAXreform sum;
var addMTR;
where qaddMTR=3;
run;

proc means data=TAXreform sum;
var addMTR;
where qaddMTR=4;
run;

proc means data=TAXreform sum;
var addMTR taxableIncome;
where qaddMTR=5 and year=2001;
run;



/***************** Summary Statistics *****************/
proc means data=TAXreform sum;
class children alter_a alter_b female year;
var taxableIncome taxAfterAdjustments taxBeforeAdjustments;
run;

proc freq data=TAXreform;
tables alter_a female;
tables MTRcat year / summarize taxAfterAdjustments taxBeforeAdjustments;
tables MTRcat1 year / summarize taxableIncome taxAfterAdjustments taxBeforeAdjustments;
tables children alter_a alter_b / summarize taxableIncome;
where qaddMTR = .;
run;

/**************** Tax Before and After Adjustments *****************/
proc sql;
select year, sum(taxAfterAdjustments) as sumTaxAfter, sum(taxBeforeAdjustments) as sumTaxBefore
from TAXreform
group by year;
quit;

proc freq data=TAXreform;
tables MTRcat year / summarize taxableIncome;
tables MTRcat1 year / summarize taxAfterAdjustments taxBeforeAdjustments;
where qaddMTR = .;
run;

proc freq data=TAXreform;
tables qaddMTR;
where qaddMTR > 0;
run;

proc sql;
select sum(addMTR) as sumAddMTR, sum(taxableIncome) as sumTaxableIncome
from TAXreform
where qaddMTR = 1;
quit;

proc sql;
select sum(addMTR) as sumAddMTR, sum(taxableIncome) as sumTaxableIncome
from TAXreform
where qaddMTR = 2;
quit;

proc sql;
select sum(addMTR) as sumAddMTR, sum(taxableIncome) as sumTaxableIncome
from TAXreform
where qaddMTR = 3;
quit;

proc sql;
select sum(addMTR) as sumAddMTR, sum(taxableIncome) as sumTaxableIncome
from TAXreform
where qaddMTR = 4;
quit;

proc sql;
select sum(addMTR) as sumAddMTR, sum(taxableIncome) as sumTaxableIncome
from TAXreform
where qaddMTR = 5 and year = 2001;
quit;


/* SummarY Statistics */
proc means data=TAXreform sum n;
class children alter_a;
var taxableIncome;
run;

proc freq data=TAXreform;
tables alter_a;
tables alter_b;
tables female;
run;

proc means data=TAXreform sum;
class female year;
var taxAfterAdjustments taxBeforeAdjustments;
run;

proc means data=TAXreform;
var taxableIncome taxBeforeAdjustments;
run;

proc freq data=TAXreform;
tables yearMTRcat;
tables yearMTRcat1;
run;

proc means data=TAXreform sum;
class year;
var taxAfterAdjustments taxBeforeAdjustments;
run;

proc means data=TAXreform;
class children alter_a alter_b;
var taxableIncome;
run;

proc freq data=TAXreform;
tables yearMTRcat;
tables yearMTRcat1;
tables year*MTRcat1 / summarize taxAfterAdjustments taxBeforeAdjustments;
run;

/* tax before and after Adjustments */
proc means data=TAXreform sum;
class year;
var taxAfterAdjustments taxBeforeAdjustments;
run;

proc freq data=TAXreform;
tables children alter_a alter_b;
tables year*MTRcat;
run;

proc freq data=TAXreform;
tables yearMTRcat1;
tables yearMTRcat1 / summarize taxAfterAdjustments taxBeforeAdjustments;
run;

proc freq data=TAXreform;
where qaddMTR=. or qaddMTR=1 or qaddMTR=2 or qaddMTR=3 or qaddMTR=4 or (qaddMTR=5 and year=2001);
tables addMTR qaddMTR year;
tables addMTR / summarize;
tables taxableIncome / summarize;
run;


proc sort data=TAXreform;
by children alter_a;
run;

proc tabulate data=TAXreform;
class alter_a;
var taxableIncome;
table alter_a, taxableIncome*(sum);
run;

proc tabulate data=TAXreform;
class alter_b;
var taxableIncome;
table alter_b, taxableIncome*(sum);
run;

proc sort data=TAXreform;
by female alter_a;
run;

proc tabulate data=TAXreform;
class female;
table female;
run;

proc sort data=TAXreform;
by female year;
run;

proc means data=TAXreform sum;
var taxAfterAdjustments taxBeforeAdjustments;
by female year;
run;

proc means data=TAXreform;
var taxableIncome taxBeforeAdjustments;
run;

proc tabulate data=TAXreform;
class MTRcat year;
var taxAfterAdjustments;
table MTRcat, year*(taxAfterAdjustments*(sum));
run;

proc tabulate data=TAXreform;
class MTRcat year;
var taxBeforeAdjustments;
table MTRcat, year*(taxBeforeAdjustments*(sum));
run;

proc sort data=TAXreform;
by year;
run;

proc means data=TAXreform sum;
var taxAfterAdjustments taxBeforeAdjustments;
by year;
run;

proc tabulate data=TAXreform;
class children alter_a alter_b;
var taxableIncome;
table children, alter_a, alter_b*(taxableIncome*(sum));
run;

proc tabulate data=TAXreform;
class MTRcat year;
var taxableIncome;
table MTRcat, year*(taxableIncome*(sum));
run;

proc tabulate data=TAXreform;
class MTRcat1 year;
var taxableIncome;
table MTRcat1, year*(taxableIncome*(sum));
run;

proc tabulate data=TAXreform;
class MTRcat1 year;
var taxAfterAdjustments;
table MTRcat1, year*(taxAfterAdjustments*(sum));
run;

proc tabulate data=TAXreform;
class MTRcat1 year;
var taxBeforeAdjustments;
table MTRcat1, year*(taxBeforeAdjustments*(sum));
run;

proc tabulate data=TAXreform;
where qaddMTR ^= .;
var addMTR;
table addMTR*qaddMTR;
run;

proc means data=TAXreform sum;
where qaddMTR=1;
var addMTR taxableIncome;
run;

proc means data=TAXreform sum;
where qaddMTR=2;
var addMTR taxableIncome;
run;

proc means data=TAXreform sum;
where qaddMTR=3;
var addMTR taxableIncome;
run;

proc means data=TAXreform sum;
where qaddMTR=4;
var addMTR taxableIncome;
run;

proc means data=TAXreform sum;
where qaddMTR=5 and year=2001;
var taxableIncome;
run;


/* Quintile Marginal Tax Rate */
proc rank data=TAXreform out=TAXreform
groups=5 ties=mean;
var addMTR;
ranks qaddMTR;
run;

proc rank data=TAXreform out=TAXreform
groups=5 ties=mean;
var ATR;
ranks qATR;
run;

proc rank data=TAXreform out=TAXreform
groups=100 ties=mean;
var addMTR;
ranks ptileMTR;
run;

proc rank data=TAXreform out=TAXreform
groups=50 ties=mean;
var addMTR;
ranks ptileMTR1;
run;

proc rank data=TAXreform out=TAXreform unique;
var addMTR;
ranks obs;
run;

/* Create Post Period Variable */
data TAXreform;
set TAXreform;
if year=2001 then postperiod=1;
else if year=1998 then postperiod=0;
run;

/* Experiments with Different Treatments /
/ I have used only quintile based on Marginal tax rate */

data TAXreform;
set TAXreform;
if taxableIncome >= 55000 then treatment=1;
else treatment=0;
run;

data TAXreform;
set TAXreform;
if taxableIncome >= 47432 then treatment1=1;
else if (taxableIncome<47432 & taxableIncome>=15799) then treatment1=2;
else treatment1=0;
run;

data TAXreform;
set TAXreform;
if taxableIncome >= 47432 then treatment2=1;
else treatment2=0;
run;

data TAXreform;
set TAXreform;
if qaddMTR=5 then treatment3=1;
else treatment3=0;
run;

* DID regression;

* Generate interaction terms;
data did_regress;
    set your_dataset;
    did5 = qaddMTR * postperiod;
    did = treatment * postperiod;
    did4 = treatment1 * postperiod;
    did6 = treatment1 * postperiod;
run;

* Sum by groups;
proc means data=did_regress sum;
    by qaddMTR postperiod;
    var taxableIncome addMTR;
    output out=summary_qaddMTR_postperiod;
run;

* Sum by groups with incomeSplitting == 2;
data did_regress_inc_split;
    set did_regress;
    where incomeSplitting = 2;
run;

proc means data=did_regress_inc_split sum;
    by qaddMTR postperiod;
    var taxableIncome addMTR;
    output out=summary_qaddMTR_postperiod_inc_split;
run;

* Regression models;
ods output ParameterEstimates=results1;
proc reg data=did_regress;
    model addMTR = qaddMTR postperiod did5;
run;
ods output ParameterEstimates=results2;
proc reg data=did_regress_inc_split;
    model addMTR = qaddMTR postperiod did5;
run;
ods output ParameterEstimates=results3;
proc reg data=did_regress;
    model addMTR = postperiod qaddMTR postperiod*qaddMTR;
run;

* Sum by groups for other treatment variables;
proc means data=did_regress sum;
    by treatment postperiod;
    var addMTR taxableIncome;
    output out=summary_treatment_postperiod;
run;

proc means data=did_regress sum;
    by treatment1 postperiod;
    var taxableIncome;
    output out=summary_treatment1_postperiod;
run;

proc means data=did_regress sum;
    by treatment2 postperiod;
    var taxableIncome;
    output out=summary_treatment2_postperiod;
run;

proc means data=did_regress sum;
    by treatment3 postperiod;
    var taxableIncome;
    output out=summary_treatment3_postperiod;
run;

* More regression models;
ods output ParameterEstimates=results4;
proc reg data=did_regress;
    model addMTR = postperiod treatment1 postperiod*treatment1;
run;
ods output ParameterEstimates=results5;
proc reg data=did_regress;
    model addMTR = postperiod treatment3 postperiod*treatment3;
run;
ods output ParameterEstimates=results6;
proc reg data=did_regress;
    model addMTR = postperiod treatment1 postperiod*treatment1;
run;
ods output ParameterEstimates=results7;
proc reg data=did_regress;
    model addMTR = postperiod treatment2 postperiod*treatment2;
run;

ods output ParameterEstimates=results8;
proc reg data=did_regress;
    model addMTR = qaddMTR postperiod did;
run;
ods output ParameterEstimates=results9;
proc reg data=did_regress;
    model addMTR = treatment3 postperiod did;
run;
ods output ParameterEstimates=results10;
proc reg data=did_regress;
    model addMTR = treatment1 postperiod did4;
run;
ods output ParameterEstimates=results11;
proc reg data=did_regress;
    model addMTR = treatment2 postperiod did;
run;



* Tabulate;
proc means data=TAXreform sum;
    by qaddMTR year;
    var addMTR;
    output out=tab_qaddMTR_year_addMTR;
run;

proc means data=TAXreform sum;
    by qaddMTR year;
    var taxableIncome;
    output out=tab_qaddMTR_year_taxableIncome;
run;

proc means data=TAXreform sum;
    by qaddMTR postperiod;
    var taxableIncome;
    output out=tab_qaddMTR_postperiod_taxableIncome;
run;

proc means data=TAXreform n;
    by qaddMTR year;
    var obs1;
    output out=tab_qaddMTR_year_obs1;
run;

proc means data=TAXreform sum;
    by qaddMTR postperiod;
    var taxableIncome;
    output out=tab_qaddMTR_postperiod_sum_taxableIncome;
run;

proc means data=TAXreform sum;
    by ptileMTR year;
    var addMTR;
    output out=tab_ptileMTR_year_addMTR;
run;

proc means data=TAXreform sum;
    by ptileMTR year;
    var taxableIncome;
    output out=tab_ptileMTR_year_taxableIncome;
run;

proc means data=TAXreform sum;
    by ptileMTR1 year;
    var addMTR;
    output out=tab_ptileMTR1_year_addMTR;
run;

proc means data=TAXreform sum;
    by ptileMTR1 year;
    var addMTR taxableIncome;
    output out=tab_ptileMTR1_year_addMTR_taxableIncome;
run;

* DID regression;
data did_regress;
    set TAXreform;
    if year == 2001 then postperiod = 1;
    if year == 1998 then postperiod = 0;
    did = group5 * postperiod;
    if group5 >= 4 then treatment = 1;
    if group5 <= 3 then treatment = 0;
    did1 = treatment * postperiod;
run;

* Regression models;
ods output ParameterEstimates=results1;
proc reg data=did_regress;
    model MTaxR = postperiod group5 postperiod*group5;
run;
ods output ParameterEstimates=results2;
proc reg data=did_regress;
    model MTaxR = group5 postperiod did;
run;
ods output ParameterEstimates=results3;
proc reg data=did_regress;
    model MTaxR = postperiod MTRcat postperiod*MTRcat;
run;
ods output ParameterEstimates=results4;
proc reg data=did_regress;
    model MTaxR = MTRcat postperiod did;
run;
ods output ParameterEstimates=results5;
proc reg data=did_regress;
    model MTaxR = postperiod treatment postperiod*treatment;
run;
ods output ParameterEstimates=results6;
proc reg data=did_regress;
    model MTaxR = group5 postperiod did1;
run;

* Collapse;
proc summary data=did_regress nway;
    class group5;
    var MTaxR taxableIncome rank;
    output out=collapsed_group5 sum=;
run;

proc summary data=did_regress nway;
    class year;
    var MTaxR taxableIncome rank;
    output out=collapsed_year sum=;
run;

proc summary data=did_regress nway;
    class year group5;
    var MTaxR taxableIncome rank;
    output out=collapsed_year_group5 sum=;
run;

proc summary data=did_regress nway;
    class year group5;
    var MTaxR taxBeforeAdjustments taxAfterAdjustments rank;
    output out=collapsed_year_group5_tax sum=;
run;

* Create MTRcat variable;
data did_regress;
    set did_regress;
    if MTaxR = 0 then MTRcat = 0;
    else if MTaxR > 0 and MTaxR <= 14 then MTRcat = 14;
    else if MTaxR > 14 and MTaxR <= 30 then MTRcat = 30;
    else if MTaxR > 30 and MTaxR <= 42 then MTRcat = 42;
    else if MTaxR > 42 then MTRcat = 45;
run;

* Tabulate MTRcat;
proc means data=did_regress sum;
    by MTRcat;
    var postperiod;
    output out=tab_MTRcat_postperiod;
run;

* Collapse with MTRcat;
proc summary data=did_regress nway;
    class year group5;
    var MTRcat taxBeforeAdjustments taxAfterAdjustments rank;
    output out=collapsed_year_group5_MTRcat sum=;
run;

proc summary data=did_regress nway;
    class year group5;
    var MTRcat taxableIncome rank;
    output out=collapsed_year_group5_MTRcat_taxableIncome sum=;
run;

proc summary data=did_regress nway;
    class MTRcat year;
    var taxableIncome rank;
    output out=collapsed_MTRcat_year_taxableIncome sum=;
run;



