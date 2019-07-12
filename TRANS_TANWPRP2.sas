/***********************************************************/
/* Program name:  TRANS_TANWPRP2.SAS                       */
/***********************************************************/
/** STNAME: State Name,                                  ***/
/** FY    : Fiscal Year,             format YYYY         ***/
/** CY    : Calendar Year,           format YYYY         ***/
/** YY    : 2 Digit CALENDAR year,   format YY           ***/
/** MONA  : First  Month of Quarter, format MM           ***/
/** MONB  : Second Month of Quarter, format MM           ***/
/** MONC  : Third  Month of Quarter, format MM           ***/
/** Q     : QUARTER DESIGNATION      format Q            ***/
/***********************************************************/
/* Change parameters at bottom of program                ***/
/***********************************************************/


%macro createm(STNAME,FY,CY,YY,MONA,MONB,MONC,Q) ;
LIBNAME  OLD   "C:\DOCUMENTS AND SETTINGS\PATRICK BRANNEN\MYDOCU~1\TRANSMISSION\FY&FY." ;

FILENAME OUT01 "C:\DOCUMENTS AND SETTINGS\PATRICK BRANNEN\MYDOCU~1\TRANSMISSION\FY&FY.\TWPR&FY&Q." ;
FILENAME OUT02 "C:\DOCUMENTS AND SETTINGS\PATRICK BRANNEN\MYDOCU~1\TRANSMISSION\FY&FY.\TANFQ&Q._STB.TXT" ;

%macro QUARTERIS1;
IF RPTM_MM = 10 THEN DO;
      M10AFWRK = AFNWORK;
      M10AFDEN = AF_DENUM;
      M10TPWRK = ATPWORK;
      M10TPDEN = TP_DENUM;
      M10AFWPR = AF_PRATE;
      M10TPWPR = TP_PRATE;
      M10AFLWK = AFLIMWK ;
      M10TPLWK = ATPLIMWK;
      M10AFLPR = AF_LRATE;
      M10TPLPR = TP_LRATE;
      M10AFAWK = AF_ADJN ;
      M10TPAWK = ATP_ADJN;
      M10AFAPR = AF_ARATE;
      M10TPAPR = TP_ARATE;
      END;
   ELSE DO;
      M10AFWRK = 0;
      M10AFDEN = 0;
      M10TPWRK = 0;
      M10TPDEN = 0;
      M10AFWPR = 0;
      M10TPWPR = 0;
      M10AFLWK = 0;
      M10TPLWK = 0;
      M10AFLPR = 0;
      M10TPLPR = 0;
      M10AFAWK = 0;
      M10TPAWK = 0;
      M10AFAPR = 0;
      M10TPAPR = 0;
      END;

   IF RPTM_MM = 11 THEN DO;
      M11AFWRK = AFNWORK;
      M11AFDEN = AF_DENUM;
      M11TPWRK = ATPWORK;
      M11TPDEN = TP_DENUM;
      M11AFWPR = AF_PRATE;
      M11TPWPR = TP_PRATE;
      M11AFLWK = AFLIMWK ;
      M11TPLWK = ATPLIMWK;
      M11AFLPR = AF_LRATE;
      M11TPLPR = TP_LRATE;
      M11AFAWK = AF_ADJN ;
      M11TPAWK = ATP_ADJN;
      M11AFAPR = AF_ARATE;
      M11TPAPR = TP_ARATE;
      END;
   ELSE DO;
      M11AFWRK = 0;
      M11AFDEN = 0;
      M11TPWRK = 0;
      M11TPDEN = 0;
      M11AFWPR = 0;
      M11TPWPR = 0;
      M11AFLWK = 0;
      M11TPLWK = 0;
      M11AFLPR = 0;
      M11TPLPR = 0;
      M11AFAWK = 0;
      M11TPAWK = 0;
      M11AFAPR = 0;
      M11TPAPR = 0;
      END;

   IF RPTM_MM = 12 THEN DO;
      M12AFWRK = AFNWORK;
      M12AFDEN = AF_DENUM;
      M12TPWRK = ATPWORK;
      M12TPDEN = TP_DENUM;
      M12AFWPR = AF_PRATE;
      M12TPWPR = TP_PRATE;
      M12AFLWK = AFLIMWK ;
      M12TPLWK = ATPLIMWK;
      M12AFLPR = AF_LRATE;
      M12TPLPR = TP_LRATE;
      M12AFAWK = AF_ADJN ;
      M12TPAWK = ATP_ADJN;
      M12AFAPR = AF_ARATE;
      M12TPAPR = TP_ARATE;
      END;
   ELSE DO;
      M12AFWRK = 0;
      M12AFDEN = 0;
      M12TPWRK = 0;
      M12TPDEN = 0;
      M12AFWPR = 0;
      M12TPWPR = 0;
      M12AFLWK = 0;
      M12TPLWK = 0;
      M12AFLPR = 0;
      M12TPLPR = 0;
      M12AFAWK = 0;
      M12TPAWK = 0;
      M12AFAPR = 0;
      M12TPAPR = 0;
      END;

run ;

PROC SORT;  BY FIPS; RUN ;

PROC SUMMARY;
   CLASS FIPS;
   VAR  M10AFWPR M11AFWPR M12AFWPR
        M10AFWRK M11AFWRK M12AFWRK
        M10AFDEN M11AFDEN M12AFDEN
        M10TPWPR M11TPWPR M12TPWPR
        M10TPWRK M11TPWRK M12TPWRK
        M10TPDEN M11TPDEN M12TPDEN
        M10AFLPR M11AFLPR M12AFLPR
        M10AFLWK M11AFLWK M12AFLWK
        M10TPLPR M11TPLPR M12TPLPR
        M10TPLWK M11TPLWK M12TPLWK 
        M10AFAWK M11AFAWK M12AFAWK 
        M10TPAWK M11TPAWK M12TPAWK 
        M10AFAPR M11AFAPR M12AFAPR 
        M10TPAPR M11TPAPR M12TPAPR ;
   OUTPUT OUT= STATS4
          SUM= AFM10WPR AFM11WPR AFM12WPR
               AFM10WRK AFM11WRK AFM12WRK
               AFM10DEN AFM11DEN AFM12DEN
               TPM10WPR TPM11WPR TPM12WPR
               TPM10WRK TPM11WRK TPM12WRK
               TPM10DEN TPM11DEN TPM12DEN
               AFM10LPR AFM11LPR AFM12LPR
               AFM10LWK AFM11LWK AFM12LWK
               TPM10LPR TPM11LPR TPM12LPR
               TPM10LWK TPM11LWK TPM12LWK 
               AFM10AWK AFM11AWK AFM12AWK 
               TPM10AWK TPM11AWK TPM12AWK 
               AFM10APR AFM11APR AFM12APR 
               TPM10APR TPM11APR TPM12APR ;

RUN ;

PROC SORT; BY FIPS ; RUN ;

DATA WPRATES;
   SET STATS4;
      BY FIPS;
   IF FIPS = . THEN DELETE;

   Q1AFMOS = 0;
   Q1TPMOS = 0;
      IF AFM10DEN > 0 THEN Q1AFMOS+1;
      IF AFM11DEN > 0 THEN Q1AFMOS+1;
      IF AFM12DEN > 0 THEN Q1AFMOS+1;
      IF TPM10DEN > 0 THEN Q1TPMOS+1;
      IF TPM11DEN > 0 THEN Q1TPMOS+1;
      IF TPM12DEN > 0 THEN Q1TPMOS+1;

   IF   Q1AFMOS = 0 THEN Q1AFWPR = 0;
   ELSE Q1AFWPR = (AFM10WPR + AFM11WPR + AFM12WPR) / Q1AFMOS;

   IF   Q1TPMOS = 0 THEN Q1TPWPR = 0;
   ELSE Q1TPWPR = (TPM10WPR + TPM11WPR + TPM12WPR) / Q1TPMOS;

   Q1AFLMOS = 0;
   Q1TPLMOS = 0;
      IF AFM10LPR > 0 THEN Q1AFLMOS+1;
      IF AFM11LPR > 0 THEN Q1AFLMOS+1;
      IF AFM12LPR > 0 THEN Q1AFLMOS+1;
      IF TPM10LPR > 0 THEN Q1TPLMOS+1;
      IF TPM11LPR > 0 THEN Q1TPLMOS+1;
      IF TPM12LPR > 0 THEN Q1TPLMOS+1;

   IF   Q1AFLMOS = 0 THEN Q1AFLPR = 0;
   ELSE Q1AFLPR = (AFM10LPR + AFM11LPR + AFM12LPR) / Q1AFLMOS;

   IF   Q1TPLMOS = 0 THEN Q1TPLPR = 0;
   ELSE Q1TPLPR = (TPM10LPR + TPM11LPR + TPM12LPR) / Q1TPLMOS;

   IF   Q1AFMOS = 0 THEN Q1AFAPR = 0;
   ELSE Q1AFAPR = (AFM10APR + AFM11APR + AFM12APR) / Q1AFMOS;

   IF   Q1TPMOS = 0 THEN Q1TPAPR = 0;
   ELSE Q1TPAPR = (TPM10APR + TPM11APR + TPM12APR) / Q1TPMOS;

PROC DELETE DATA= COMBO    STATS2   STSUMFL  
                  ALLSUMFL STATS4 ;

RUN ;

PROC SORT; BY FIPS; RUN ;

%mend;

%macro QUARTERIS2;
IF RPTM_MM = 01 THEN DO;
      M01AFWRK = AFNWORK;
      M01AFDEN = AF_DENUM;
      M01TPWRK = ATPWORK;
      M01TPDEN = TP_DENUM;
      M01AFWPR = AF_PRATE;
      M01TPWPR = TP_PRATE;
      M01AFLWK = AFLIMWK ;
      M01TPLWK = ATPLIMWK;
      M01AFLPR = AF_LRATE;
      M01TPLPR = TP_LRATE;
      M01AFAWK = AF_ADJN ;
      M01TPAWK = ATP_ADJN;
      M01AFAPR = AF_ARATE;
      M01TPAPR = TP_ARATE;
      END;
   ELSE DO;
      M01AFWRK = 0;
      M01AFDEN = 0;
      M01TPWRK = 0;
      M01TPDEN = 0;
      M01AFWPR = 0;
      M01TPWPR = 0;
      M01AFLWK = 0;
      M01TPLWK = 0;
      M01AFLPR = 0;
      M01TPLPR = 0;
      M01AFAWK = 0;
      M01TPAWK = 0;
      M01AFAPR = 0;
      M01TPAPR = 0;
      END;

   IF RPTM_MM = 02 THEN DO;
      M02AFWRK = AFNWORK;
      M02AFDEN = AF_DENUM;
      M02TPWRK = ATPWORK;
      M02TPDEN = TP_DENUM;
      M02AFWPR = AF_PRATE;
      M02TPWPR = TP_PRATE;
      M02AFLWK = AFLIMWK ;
      M02TPLWK = ATPLIMWK;
      M02AFLPR = AF_LRATE;
      M02TPLPR = TP_LRATE;
      M02AFAWK = AF_ADJN ;
      M02TPAWK = ATP_ADJN;
      M02AFAPR = AF_ARATE;
      M02TPAPR = TP_ARATE;
      END;
   ELSE DO;
      M02AFWRK = 0;
      M02AFDEN = 0;
      M02TPWRK = 0;
      M02TPDEN = 0;
      M02AFWPR = 0;
      M02TPWPR = 0;
      M02AFLWK = 0;
      M02TPLWK = 0;
      M02AFLPR = 0;
      M02TPLPR = 0;
      M02AFAWK = 0;
      M02TPAWK = 0;
      M02AFAPR = 0;
      M02TPAPR = 0;
      END;

   IF RPTM_MM = 03 THEN DO;
      M03AFWRK = AFNWORK;
      M03AFDEN = AF_DENUM;
      M03TPWRK = ATPWORK;
      M03TPDEN = TP_DENUM;
      M03AFWPR = AF_PRATE;
      M03TPWPR = TP_PRATE;
      M03AFLWK = AFLIMWK ;
      M03TPLWK = ATPLIMWK;
      M03AFLPR = AF_LRATE;
      M03TPLPR = TP_LRATE;
      M03AFAWK = AF_ADJN ;
      M03TPAWK = ATP_ADJN;
      M03AFAPR = AF_ARATE;
      M03TPAPR = TP_ARATE;
      END;
   ELSE DO;
      M03AFWRK = 0;
      M03AFDEN = 0;
      M03TPWRK = 0;
      M03TPDEN = 0;
      M03AFWPR = 0;
      M03TPWPR = 0;
      M03AFLWK = 0;
      M03TPLWK = 0;
      M03AFLPR = 0;
      M03TPLPR = 0;
      M03AFAWK = 0;
      M03TPAWK = 0;
      M03AFAPR = 0;
      M03TPAPR = 0;
      END;

PROC PRINT ;
   VAR FIPS     RPTM_MM  AF_PRATE AFNWORK  AF_DENUM
       AFSAMP   AFLIE    AFDCARD1 AFDCARD2 AFDCARD5
       TP_PRATE TP_DENUM ATPWORK  ATPSAMP  ATPDCRD2
       ATPDCRD5 ;
    TITLE 'SUMMED DATA FOR WPR CALCULATIONS' ;

RUN ;

PROC SORT;  BY FIPS; RUN ;

PROC SUMMARY;
   CLASS FIPS;
   VAR  M01AFWPR M02AFWPR M03AFWPR
        M01AFWRK M02AFWRK M03AFWRK
        M01AFDEN M02AFDEN M03AFDEN
        M01TPWPR M02TPWPR M03TPWPR
        M01TPWRK M02TPWRK M03TPWRK
        M01TPDEN M02TPDEN M03TPDEN
        M01AFLPR M02AFLPR M03AFLPR
        M01AFLWK M02AFLWK M03AFLWK
        M01TPLPR M02TPLPR M03TPLPR
        M01TPLWK M02TPLWK M03TPLWK 
        M01AFAWK M02AFAWK M03AFAWK 
        M01TPAWK M02TPAWK M03TPAWK 
        M01AFAPR M02AFAPR M03AFAPR 
        M01TPAPR M02TPAPR M03TPAPR ;
   OUTPUT OUT= STATS4
          SUM= AFM01WPR AFM02WPR AFM03WPR
               AFM01WRK AFM02WRK AFM03WRK
               AFM01DEN AFM02DEN AFM03DEN
               TPM01WPR TPM02WPR TPM03WPR
               TPM01WRK TPM02WRK TPM03WRK
               TPM01DEN TPM02DEN TPM03DEN
               AFM01LPR AFM02LPR AFM03LPR
               AFM01LWK AFM02LWK AFM03LWK
               TPM01LPR TPM02LPR TPM03LPR
               TPM01LWK TPM02LWK TPM03LWK 
               AFM01AWK AFM02AWK AFM03AWK 
               TPM01AWK TPM02AWK TPM03AWK 
               AFM01APR AFM02APR AFM03APR 
               TPM01APR TPM02APR TPM03APR ;

RUN ;

PROC SORT; BY FIPS ; RUN ;

DATA WPRATES;
   SET STATS4;
      BY FIPS;
   IF FIPS = . THEN DELETE;

   Q2AFMOS = 0;
   Q2TPMOS = 0;
      IF AFM01DEN > 0 THEN Q2AFMOS+1;
      IF AFM02DEN > 0 THEN Q2AFMOS+1;
      IF AFM03DEN > 0 THEN Q2AFMOS+1;
      IF TPM01DEN > 0 THEN Q2TPMOS+1;
      IF TPM02DEN > 0 THEN Q2TPMOS+1;
      IF TPM03DEN > 0 THEN Q2TPMOS+1;

   IF   Q2AFMOS = 0 THEN Q2AFWPR = 0;
   ELSE Q2AFWPR = (AFM01WPR + AFM02WPR + AFM03WPR) / Q2AFMOS;

   IF   Q2TPMOS = 0 THEN Q2TPWPR = 0;
   ELSE Q2TPWPR = (TPM01WPR + TPM02WPR + TPM03WPR) / Q2TPMOS;

   Q2AFLMOS = 0;
   Q2TPLMOS = 0;
      IF AFM01LPR > 0 THEN Q2AFLMOS+1;
      IF AFM02LPR > 0 THEN Q2AFLMOS+1;
      IF AFM03LPR > 0 THEN Q2AFLMOS+1;
      IF TPM01LPR > 0 THEN Q2TPLMOS+1;
      IF TPM02LPR > 0 THEN Q2TPLMOS+1;
      IF TPM03LPR > 0 THEN Q2TPLMOS+1;

   IF   Q2AFLMOS = 0 THEN Q2AFLPR = 0;
   ELSE Q2AFLPR = (AFM01LPR + AFM02LPR + AFM03LPR) / Q2AFLMOS;

   IF   Q2TPLMOS = 0 THEN Q2TPLPR = 0;
   ELSE Q2TPLPR = (TPM01LPR + TPM02LPR + TPM03LPR) / Q2TPLMOS;

   IF   Q2AFMOS = 0 THEN Q2AFAPR = 0;
   ELSE Q2AFAPR = (AFM01APR + AFM02APR + AFM03APR) / Q2AFMOS;

   IF   Q2TPMOS = 0 THEN Q2TPAPR = 0;
   ELSE Q2TPAPR = (TPM01APR + TPM02APR + TPM03APR) / Q2TPMOS;

PROC DELETE DATA= COMBO    STATS2   STSUMFL                   ALLSUMFL STATS4 ;

RUN ;

PROC SORT; BY FIPS; RUN ;

%mend;

%macro QUARTERIS3;
IF RPTM_MM = 04 THEN DO;
      M04AFWRK = AFNWORK;
      M04AFDEN = AF_DENUM;
      M04TPWRK = ATPWORK;
      M04TPDEN = TP_DENUM;
      M04AFWPR = AF_PRATE;
      M04TPWPR = TP_PRATE;
      M04AFLWK = AFLIMWK ;
      M04TPLWK = ATPLIMWK;
      M04AFLPR = AF_LRATE;
      M04TPLPR = TP_LRATE;
      M04AFAWK = AF_ADJN ;
      M04TPAWK = ATP_ADJN;
      M04AFAPR = AF_ARATE;
      M04TPAPR = TP_ARATE;
      END;
   ELSE DO;
      M04AFWRK = 0;
      M04AFDEN = 0;
      M04TPWRK = 0;
      M04TPDEN = 0;
      M04AFWPR = 0;
      M04TPWPR = 0;
      M04AFLWK = 0;
      M04TPLWK = 0;
      M04AFLPR = 0;
      M04TPLPR = 0;
      M04AFAWK = 0;
      M04TPAWK = 0;
      M04AFAPR = 0;
      M04TPAPR = 0;
      END;

   IF RPTM_MM = 05 THEN DO;
      M05AFWRK = AFNWORK;
      M05AFDEN = AF_DENUM;
      M05TPWRK = ATPWORK;
      M05TPDEN = TP_DENUM;
      M05AFWPR = AF_PRATE;
      M05TPWPR = TP_PRATE;
      M05AFLWK = AFLIMWK ;
      M05TPLWK = ATPLIMWK;
      M05AFLPR = AF_LRATE;
      M05TPLPR = TP_LRATE;
      M05AFAWK = AF_ADJN ;
      M05TPAWK = ATP_ADJN;
      M05AFAPR = AF_ARATE;
      M05TPAPR = TP_ARATE;
      END;
   ELSE DO;
      M05AFWRK = 0;
      M05AFDEN = 0;
      M05TPWRK = 0;
      M05TPDEN = 0;
      M05AFWPR = 0;
      M05TPWPR = 0;
      M05AFLWK = 0;
      M05TPLWK = 0;
      M05AFLPR = 0;
      M05TPLPR = 0;
      M05AFAWK = 0;
      M05TPAWK = 0;
      M05AFAPR = 0;
      M05TPAPR = 0;
      END;

   IF RPTM_MM = 06 THEN DO;
      M06AFWRK = AFNWORK;
      M06AFDEN = AF_DENUM;
      M06TPWRK = ATPWORK;
      M06TPDEN = TP_DENUM;
      M06AFWPR = AF_PRATE;
      M06TPWPR = TP_PRATE;
      M06AFLWK = AFLIMWK ;
      M06TPLWK = ATPLIMWK;
      M06AFLPR = AF_LRATE;
      M06TPLPR = TP_LRATE;
      M06AFAWK = AF_ADJN ;
      M06TPAWK = ATP_ADJN;
      M06AFAPR = AF_ARATE;
      M06TPAPR = TP_ARATE;
      END;
   ELSE DO;
      M06AFWRK = 0;
      M06AFDEN = 0;
      M06TPWRK = 0;
      M06TPDEN = 0;
      M06AFWPR = 0;
      M06TPWPR = 0;
      M06AFLWK = 0;
      M06TPLWK = 0;
      M06AFLPR = 0;
      M06TPLPR = 0;
      M06AFAWK = 0;
      M06TPAWK = 0;
      M06AFAPR = 0;
      M06TPAPR = 0;
      END;

PROC PRINT ;
   VAR FIPS     RPTM_MM  AF_PRATE AFNWORK  AF_DENUM
       AFSAMP   AFLIE    AFDCARD1 AFDCARD2 AFDCARD5
       TP_PRATE TP_DENUM ATPWORK  ATPSAMP  ATPDCRD2
       ATPDCRD5 ;
    TITLE 'SUMMED DATA FOR WPR CALCULATIONS' ;

RUN ;

PROC SORT;  BY FIPS; RUN ;

PROC SUMMARY;
   CLASS FIPS;
   VAR  M04AFWPR M05AFWPR M06AFWPR
        M04AFWRK M05AFWRK M06AFWRK
        M04AFDEN M05AFDEN M06AFDEN
        M04TPWPR M05TPWPR M06TPWPR
        M04TPWRK M05TPWRK M06TPWRK
        M04TPDEN M05TPDEN M06TPDEN
        M04AFLPR M05AFLPR M06AFLPR
        M04AFLWK M05AFLWK M06AFLWK
        M04TPLPR M05TPLPR M06TPLPR
        M04TPLWK M05TPLWK M06TPLWK 
        M04AFAWK M05AFAWK M06AFAWK 
        M04TPAWK M05TPAWK M06TPAWK 
        M04AFAPR M05AFAPR M06AFAPR 
        M04TPAPR M05TPAPR M06TPAPR ;
   OUTPUT OUT= STATS4
          SUM= AFM04WPR AFM05WPR AFM06WPR
               AFM04WRK AFM05WRK AFM06WRK
               AFM04DEN AFM05DEN AFM06DEN
               TPM04WPR TPM05WPR TPM06WPR
               TPM04WRK TPM05WRK TPM06WRK
               TPM04DEN TPM05DEN TPM06DEN
               AFM04LPR AFM05LPR AFM06LPR
               AFM04LWK AFM05LWK AFM06LWK
               TPM04LPR TPM05LPR TPM06LPR
               TPM04LWK TPM05LWK TPM06LWK 
               AFM04AWK AFM05AWK AFM06AWK 
               TPM04AWK TPM05AWK TPM06AWK 
               AFM04APR AFM05APR AFM06APR 
               TPM04APR TPM05APR TPM06APR ;

RUN ;

PROC SORT; BY FIPS ; RUN ;

DATA WPRATES;
   SET STATS4;
      BY FIPS;
   IF FIPS = . THEN DELETE;

   Q3AFMOS = 0;
   Q3TPMOS = 0;
      IF AFM04DEN > 0 THEN Q3AFMOS+1;
      IF AFM05DEN > 0 THEN Q3AFMOS+1;
      IF AFM06DEN > 0 THEN Q3AFMOS+1;
      IF TPM04DEN > 0 THEN Q3TPMOS+1;
      IF TPM05DEN > 0 THEN Q3TPMOS+1;
      IF TPM06DEN > 0 THEN Q3TPMOS+1;

   IF   Q3AFMOS = 0 THEN Q3AFWPR = 0;
   ELSE Q3AFWPR = (AFM04WPR + AFM05WPR + AFM06WPR) / Q3AFMOS;

   IF   Q3TPMOS = 0 THEN Q3TPWPR = 0;
   ELSE Q3TPWPR = (TPM04WPR + TPM05WPR + TPM06WPR) / Q3TPMOS;

   Q3AFLMOS = 0;
   Q3TPLMOS = 0;
      IF AFM04LPR > 0 THEN Q3AFLMOS+1;
      IF AFM05LPR > 0 THEN Q3AFLMOS+1;
      IF AFM06LPR > 0 THEN Q3AFLMOS+1;
      IF TPM04LPR > 0 THEN Q3TPLMOS+1;
      IF TPM05LPR > 0 THEN Q3TPLMOS+1;
      IF TPM06LPR > 0 THEN Q3TPLMOS+1;

   IF   Q3AFLMOS = 0 THEN Q3AFLPR = 0;
   ELSE Q3AFLPR = (AFM04LPR + AFM05LPR + AFM06LPR) / Q3AFLMOS;

   IF   Q3TPLMOS = 0 THEN Q3TPLPR = 0;
   ELSE Q3TPLPR = (TPM04LPR + TPM05LPR + TPM06LPR) / Q3TPLMOS;

   IF   Q3AFMOS = 0 THEN Q3AFAPR = 0;
   ELSE Q3AFAPR = (AFM04APR + AFM05APR + AFM06APR) / Q3AFMOS;

   IF   Q3TPMOS = 0 THEN Q3TPAPR = 0;
   ELSE Q3TPAPR = (TPM04APR + TPM05APR + TPM06APR) / Q3TPMOS;

PROC DELETE DATA= COMBO    STATS2   STSUMFL  
                  ALLSUMFL STATS4 ;

RUN ;

PROC SORT; BY FIPS; RUN ;
%mend;

%macro QUARTERIS4;
IF RPTM_MM = 07 THEN DO;
      M07AFWRK = AFNWORK;
      M07AFDEN = AF_DENUM;
      M07TPWRK = ATPWORK;
      M07TPDEN = TP_DENUM;
      M07AFWPR = AF_PRATE;
      M07TPWPR = TP_PRATE;
      M07AFLWK = AFLIMWK ;
      M07TPLWK = ATPLIMWK;
      M07AFLPR = AF_LRATE;
      M07TPLPR = TP_LRATE;
      M07AFAWK = AF_ADJN ;
      M07TPAWK = ATP_ADJN;
      M07AFAPR = AF_ARATE;
      M07TPAPR = TP_ARATE;
      END;
   ELSE DO;
      M07AFWRK = 0;
      M07AFDEN = 0;
      M07TPWRK = 0;
      M07TPDEN = 0;
      M07AFWPR = 0;
      M07TPWPR = 0;
      M07AFLWK = 0;
      M07TPLWK = 0;
      M07AFLPR = 0;
      M07TPLPR = 0;
      M07AFAWK = 0;
      M07TPAWK = 0;
      M07AFAPR = 0;
      M07TPAPR = 0;
      END;

   IF RPTM_MM = 08 THEN DO;
      M08AFWRK = AFNWORK;
      M08AFDEN = AF_DENUM;
      M08TPWRK = ATPWORK;
      M08TPDEN = TP_DENUM;
      M08AFWPR = AF_PRATE;
      M08TPWPR = TP_PRATE;
      M08AFLWK = AFLIMWK ;
      M08TPLWK = ATPLIMWK;
      M08AFLPR = AF_LRATE;
      M08TPLPR = TP_LRATE;
      M08AFAWK = AF_ADJN ;
      M08TPAWK = ATP_ADJN;
      M08AFAPR = AF_ARATE;
      M08TPAPR = TP_ARATE;
      END;
   ELSE DO;
      M08AFWRK = 0;
      M08AFDEN = 0;
      M08TPWRK = 0;
      M08TPDEN = 0;
      M08AFWPR = 0;
      M08TPWPR = 0;
      M08AFLWK = 0;
      M08TPLWK = 0;
      M08AFLPR = 0;
      M08TPLPR = 0;
      M08AFAWK = 0;
      M08TPAWK = 0;
      M08AFAPR = 0;
      M08TPAPR = 0;
      END;

   IF RPTM_MM = 09 THEN DO;
      M09AFWRK = AFNWORK;
      M09AFDEN = AF_DENUM;
      M09TPWRK = ATPWORK;
      M09TPDEN = TP_DENUM;
      M09AFWPR = AF_PRATE;
      M09TPWPR = TP_PRATE;
      M09AFLWK = AFLIMWK ;
      M09TPLWK = ATPLIMWK;
      M09AFLPR = AF_LRATE;
      M09TPLPR = TP_LRATE;
      M09AFAWK = AF_ADJN ;
      M09TPAWK = ATP_ADJN;
      M09AFAPR = AF_ARATE;
      M09TPAPR = TP_ARATE;
      END;
   ELSE DO;
      M09AFWRK = 0;
      M09AFDEN = 0;
      M09TPWRK = 0;
      M09TPDEN = 0;
      M09AFWPR = 0;
      M09TPWPR = 0;
      M09AFLWK = 0;
      M09TPLWK = 0;
      M09AFLPR = 0;
      M09TPLPR = 0;
      M09AFAWK = 0;
      M09TPAWK = 0;
      M09AFAPR = 0;
      M09TPAPR = 0;
      END;

PROC PRINT ;
   VAR FIPS     RPTM_MM  AF_PRATE AFNWORK  AF_DENUM
       AFSAMP   AFLIE    AFDCARD1 AFDCARD2 AFDCARD5
       TP_PRATE TP_DENUM ATPWORK  ATPSAMP  ATPDCRD2
       ATPDCRD5 ;
    TITLE 'SUMMED DATA FOR WPR CALCULATIONS' ;

RUN ;

PROC SORT;  BY FIPS; RUN ;

PROC SUMMARY;
   CLASS FIPS;
   VAR  M07AFWPR M08AFWPR M09AFWPR
        M07AFWRK M08AFWRK M09AFWRK
        M07AFDEN M08AFDEN M09AFDEN
        M07TPWPR M08TPWPR M09TPWPR
        M07TPWRK M08TPWRK M09TPWRK
        M07TPDEN M08TPDEN M09TPDEN
        M07AFLPR M08AFLPR M09AFLPR
        M07AFLWK M08AFLWK M09AFLWK
        M07TPLPR M08TPLPR M09TPLPR
        M07TPLWK M08TPLWK M09TPLWK 
        M07AFAWK M08AFAWK M09AFAWK 
        M07TPAWK M08TPAWK M09TPAWK 
        M07AFAPR M08AFAPR M09AFAPR 
        M07TPAPR M08TPAPR M09TPAPR ;
   OUTPUT OUT= STATS4
          SUM= AFM07WPR AFM08WPR AFM09WPR
               AFM07WRK AFM08WRK AFM09WRK
               AFM07DEN AFM08DEN AFM09DEN
               TPM07WPR TPM08WPR TPM09WPR
               TPM07WRK TPM08WRK TPM09WRK
               TPM07DEN TPM08DEN TPM09DEN
               AFM07LPR AFM08LPR AFM09LPR
               AFM07LWK AFM08LWK AFM09LWK
               TPM07LPR TPM08LPR TPM09LPR
               TPM07LWK TPM08LWK TPM09LWK 
               AFM07AWK AFM08AWK AFM09AWK 
               TPM07AWK TPM08AWK TPM09AWK 
               AFM07APR AFM08APR AFM09APR 
               TPM07APR TPM08APR TPM09APR ;

RUN ;

PROC SORT; BY FIPS ; RUN ;

DATA WPRATES;
   SET STATS4;
      BY FIPS;
   IF FIPS = . THEN DELETE;

   Q4AFMOS = 0;
   Q4TPMOS = 0;
      IF AFM07DEN > 0 THEN Q4AFMOS+1;
      IF AFM08DEN > 0 THEN Q4AFMOS+1;
      IF AFM09DEN > 0 THEN Q4AFMOS+1;
      IF TPM07DEN > 0 THEN Q4TPMOS+1;
      IF TPM08DEN > 0 THEN Q4TPMOS+1;
      IF TPM09DEN > 0 THEN Q4TPMOS+1;

   IF   Q4AFMOS = 0 THEN Q4AFWPR = 0;
   ELSE Q4AFWPR = (AFM07WPR + AFM08WPR + AFM09WPR) / Q4AFMOS;

   IF   Q4TPMOS = 0 THEN Q4TPWPR = 0;
   ELSE Q4TPWPR = (TPM07WPR + TPM08WPR + TPM09WPR) / Q4TPMOS;

   Q4AFLMOS = 0;
   Q4TPLMOS = 0;
      IF AFM07LPR > 0 THEN Q4AFLMOS+1;
      IF AFM08LPR > 0 THEN Q4AFLMOS+1;
      IF AFM09LPR > 0 THEN Q4AFLMOS+1;
      IF TPM07LPR > 0 THEN Q4TPLMOS+1;
      IF TPM08LPR > 0 THEN Q4TPLMOS+1;
      IF TPM09LPR > 0 THEN Q4TPLMOS+1;

   IF   Q4AFLMOS = 0 THEN Q4AFLPR = 0;
   ELSE Q4AFLPR = (AFM07LPR + AFM08LPR + AFM09LPR) / Q4AFLMOS;

   IF   Q4TPLMOS = 0 THEN Q4TPLPR = 0;
   ELSE Q4TPLPR = (TPM07LPR + TPM08LPR + TPM09LPR) / Q4TPLMOS;

   IF   Q4AFMOS = 0 THEN Q4AFAPR = 0;
   ELSE Q4AFAPR = (AFM07APR + AFM08APR + AFM09APR) / Q4AFMOS;

   IF   Q4TPMOS = 0 THEN Q4TPAPR = 0;
   ELSE Q4TPAPR = (TPM07APR + TPM08APR + TPM09APR) / Q4TPMOS;

PROC DELETE DATA= COMBO    STATS2   STSUMFL  
                  ALLSUMFL STATS4 ;

RUN ;

PROC SORT; BY FIPS; RUN ;

%mend;


DATA COMBO;
SET OLD.SF&YY&MONA
    OLD.SF&YY&MONB
    OLD.SF&YY&MONC ;

RUN ;

DATA COMBO2 ;
   SET COMBO  ;
      BY FIPS ;

   THISYEAR = &FY ;
   IF (RPTM_MM = 10 OR RPTM_MM = 11 OR RPTM_MM = 12) THEN 
      THISYEAR = &CY  ;

RUN ;

PROC SORT;
   BY FIPS THISYEAR RPTM_MM STRATUM ;

RUN ;

DATA SUMRPT;
   SET COMBO2;
      BY FIPS THISYEAR RPTM_MM STRATUM ;
FILE OUT02;
CURRENT = TODAY ( ) ;
STATE = &STNAME ; 
   IF RPTM_MM = . THEN DELETE;
   IF STRATUM = . THEN DELETE;
PUT @24 "SUMMARY OF WORK PARTICIPATION DATA" / /
    @32 "REPORTING" @46 "TOTAL    SAMPLE"  ;
PUT @01 "STATE"  @19 "STRATUM"  @32 "YEAR"
    @38 "MONTH"  @46 "CASES     SIZE"  ;
PUT @01 STATE  @21 STRATUM  @32 THISYEAR
    @40 RPTM_MM  @45 TCASES  @55 SSIZE / ;
PUT @1 "NUMBER OF FAMILIES"  @33 "ALL FAMILIES"
    @51 "TWO PARENT FAMILIES"  ;
PUT @1 "------------------"  @33 "------------"
    @51 "-------------------"  ;
PUT @1 "REPORTED                "  @35 SNSAMP    @55 STPSAMP  ;
PUT @1 "PARTICIPATING           "  @35 SNWORK    @55 STPWORK  ;
PUT @1 "FAMILIES WITH NO WEI    "  @35 SN_NOWEI               ;
PUT @1 "DISREGARDED FOR REASON 1"  @35 SNDCARD1               ;
PUT @1 "DISREGARDED FOR REASON 2"  @35 SNDCARD2  @55 STPDCRD2 ;
PUT @1 "DISREGARDED FOR REASON 5"  @35 SNDCARD5  @55 STPDCRD5 ;
PUT @1 "2P FAM W DISABLED PARENT"                @55 STPDABLE ;
PUT @1 "2P FAM W NCP EXCLUDED   "                @55 STPNCP   ;
PUT @1 "LISTED IN ERROR         "  @35 SNLIE              ;
PUT @1 "INCLUDED IN 30 PERCENT LIMIT" ;
PUT @1 "  DUE TO VOC. EDU       "  @35 SNLIM_VOC @55 STPLIM_V ;
PUT @1 "  DUE TO SAT.sCHOOL ATT."  @35 SNLIM_SSA @55 STPLIMIT ;
PUT @1 "  DUE TO ED. REL. TO EMP"  @35 SNLIM_EDE ;
PUT @1 "DV EXEMPTIONS           "  @35 SNDVEXEM  @55 STPDVEXM / / ; 
PUT @57 "TWO PARENT"  ;
PUT @31 "ALL FAMILY"  @46 "FIRST ADULT"  @65 "SECOND ADULT"
    @83 "ALL CASES"  ;
PUT @29 "NUMBER   TOTAL"  @47 "NUMBER   TOTAL" @65 "NUMBER   TOTAL"
    @83 "NUMBER    TOTAL" ;
PUT @01 "WORK ACTIVITIES"  @28 "OF CASES  HOURS"
    @46 "OF CASES  HOURS"  @64 "OF CASES  HOURS"
    @83 "OF ADULTS HOURS"  ;
PUT @01 "---------------"  @28 "-------  ------"
    @46 "--------- -----"  @64 "-------  ------"
    @83 "-------  ------"  ;

PUT @01 "UNSUB. EMPLOYMENT        " @28 SAFNWK1  @37 SAFWRK1
    @46 SP1NWK1  @55 SP1WRK1  @64 SP2NWK1  @73 SP2WRK1
    @83 SPERWK1  @92 SHRSWK1 ;
PUT @01 "PRIVATE SECTOR EMPLOYMENT" @28 SAFNWK2  @37 SAFWRK2
    @46 SP1NWK2  @55 SP1WRK2  @64 SP2NWK2  @73 SP2WRK2
    @83 SPERWK2  @92 SHRSWK2 ;
PUT @01 "PUBLIC  SECTOR EMPLOYMENT" @28 SAFNWK3  @37 SAFWRK3
    @46 SP1NWK3  @55 SP1WRK3  @64 SP2NWK3  @73 SP2WRK3
    @83 SPERWK3  @92 SHRSWK3 ;
PUT @01 "WORK EXPERIENCE          " @28 SAFNWK4  @37 SAFWRK4
    @46 SP1NWK4  @55 SP1WRK4  @64 SP2NWK4  @73 SP2WRK4
    @83 SPERWK4  @92 SHRSWK4 ;
PUT @01 "ON-THE-JOB TRAINING      " @28 SAFNWK5  @37 SAFWRK5
    @46 SP1NWK5  @55 SP1WRK5  @64 SP2NWK5  @73 SP2WRK5
    @83 SPERWK5  @92 SHRSWK5 ;
PUT @01 "JOB SEARCH               " @28 SAFNWK6  @37 SAFWRK6
    @46 SP1NWK6  @55 SP1WRK6  @64 SP2NWK6  @73 SP2WRK6
    @83 SPERWK6  @92 SHRSWK6 ;
PUT @01 "COMMUNITY SERVICE        " @28 SAFNWK7  @37 SAFWRK7
    @46 SP1NWK7  @55 SP1WRK7  @64 SP2NWK7  @73 SP2WRK7
    @83 SPERWK7  @92 SHRSWK7 ;
PUT @01 "VOCATIONAL EDUCATION     " @28 SAFNWK8  @37 SAFWRK8
    @46 SP1NWK8  @55 SP1WRK8  @64 SP2NWK8  @73 SP2WRK8
    @83 SPERWK8  @92 SHRSWK8 ;
PUT @01 "JOB SKILLS TRAINING      " @28 SAFNWK9  @37 SAFWRK9
    @46 SP1NWK9  @55 SP1WRK9  @64 SP2NWK9  @73 SP2WRK9
    @83 SPERWK9  @92 SHRSWK9 ;
PUT @01 "EDU. RELATED TO EMPLOY.  " @28 SAFNWK10 @37 SAFWRK10
    @46 SP1NWK10 @55 SP1WRK10 @64 SP2NWK10 @73 SP2WRK10
    @83 SPERWK10 @92 SHRSWK10 ;
PUT @01 "SATIF. SCHOOL ATTENDANCE " @28 SAFNWK11 @37 SAFWRK11
    @46 SP1NWK11 @55 SP1WRK11 @64 SP2NWK11 @73 SP2WRK11
    @83 SPERWK11 @92 SHRSWK11 ;
PUT @01 "PROVIDING CHILD CARE     " @28 SAFNWK12  @37 SAFWRK12
    @46 SP1NWK12 @55 SP1WRK12 @64 SP2NWK12 @73 SP2WRK12
    @83 SPERWK12 @92 SHRSWK12 ;
PUT @01 "OTHER WORK ACTIVITIES    " @28 SAFNWK13  @37 SAFWRK13
    @46 SP1NWK13 @55 SP1WRK13 @64 SP2NWK13 @73 SP2WRK13
    @83 SPERWK13 @92 SHRSWK13 ;
PUT @1 94 * "-" /
    @5 "ACF/OFA/DDCA"
    @80 CURRENT MMDDYY8. // ;
PUT _PAGE_ ;  

RUN ;

PROC SUMMARY DATA= COMBO;
   CLASS FIPS RPTM_MM ;
   VAR WNSAMP     WNWORK     WNLIE      WNDCARD1   WNDCARD2
       WNEXEMPT   WNDCARD5   WNLIM_VOC  WNLIM_SSA  WNLIM_EDE
       WNDEEM15   WNDEEM16   WNDEEM17   WN_NOWEI   WNDMCOR
	   WTPSAMP    WTPWORK    WTPEXEMP   WTPLIM_V   WTPLIMIT  
       WNDISABL   WTPDCRD2   WTPDCRD5   WNDVEXEM   WTPDVEXM 
       WTPNCP     WTPDABLE ;
   OUTPUT OUT = STATS2
          SUM =  AFSAMP     AFNWORK    AFLIE      AFDCARD1   AFDCARD2
                 AFEXEMPT   AFDCARD5   AFLIM_VOC  AFLIM_SSA  AFLIM_EDE 
                 AFDEEM15   AFDEEM16   AFDEEM17   AF_NOWEI   AFDMCOR  
                 ATPSAMP    ATPWORK    ATPEXEMP   ATPLIM_V   ATPLIMIT
                 ANDISABL   ATPDCRD2   ATPDCRD5   AFDVEXEM   ATPDVEXM 
                 ATPNCP     ATPDABLE ;

RUN ;

PROC SORT;  BY FIPS RPTM_MM ;  RUN ;

DATA STSUMFL;
   SET STATS2;
      BY FIPS RPTM_MM;
   IF FIPS    = . THEN DELETE ;
   IF RPTM_MM = . THEN DELETE ;

   P3AFWK = (0.3) * AFNWORK ;
   AFLIMIT = AFLIM_VOC + AFLIM_SSA + AFLIM_EDE;
   IF AFLIMIT > P3AFWK  THEN DO ;
      P7AFWK = AFNWORK - AFLIMIT ;
      AFLIMWK = P7AFWK / (0.7) ;
      END ;
   ELSE AFLIMWK = 0 ;

   P3TPWK = (0.3) * ATPWORK ;
   ATPTOTLIM = ATPLIM_V + ATPLIMIT; 
   IF ATPTOTLIM > P3TPWK  THEN DO ;
      P7TPWK = ATPWORK - ATPTOTLIM ;
      ATPLIMWK = P7TPWK / (0.7) ;
      END ;
    ELSE ATPLIMWK = 0 ;

   AF_DENUM = AFSAMP   - AFLIE  - AFDCARD1
            - AFDCARD2 - AFDCARD5 - AF_NOWEI ;
   TP_DENUM = ATPSAMP - ATPDCRD2 - ATPDCRD5
            - ATPNCP  - ATPDABLE ;

   AF_ADJD   = AF_DENUM - AFDVEXEM ; 
   ATP_ADJD  = TP_DENUM - ATPDVEXM ; 

   IF AFLIMWK > 0 THEN AF_ADJN = AFLIMWK;
   ELSE AF_ADJN = AFNWORK ;

   IF ATPLIMWK > 0 THEN ATP_ADJN = ATPLIMWK;
   ELSE ATP_ADJN = ATPWORK ;

PROC SORT;  BY FIPS  RPTM_MM; RUN ;

DATA ALLSUMFL;
   SET STSUMFL ;
      BY FIPS RPTM_MM;

   IF AF_DENUM = 0 THEN DO ;
      AF_PRATE = 0 ;
      AF_LRATE = 0 ;
      AF_ARATE = 0 ;
      END ;
   ELSE DO ;
      AF_PRATE = 100 * AFNWORK / AF_DENUM ;
      AF_LRATE = 100 * AFLIMWK / AF_DENUM ;
      AF_ARATE = 100 * AF_ADJN / AF_DENUM ;
      END ;
   IF TP_DENUM = 0 THEN DO ;
      TP_PRATE = 0 ;
      TP_LRATE = 0 ;
      TP_ARATE = 0 ;
      END ;
   ELSE DO ;
      TP_PRATE = 100 * ATPWORK  / TP_DENUM ;
      TP_LRATE = 100 * ATPLIMWK / TP_DENUM ;
      TP_ARATE = 100 * ATP_ADJN / TP_DENUM ;
      END ;

   IF AF_ADJD = 0 THEN AF_VRATE = 0 ;
   ELSE AF_VRATE = 100 * AF_ADJN / AF_ADJD ; 
   IF ATP_ADJD = 0 THEN TP_VRATE = 0 ;
   ELSE TP_VRATE = 100 * ATP_ADJN / ATP_ADJD ; 

  %QUARTERIS&Q.;  


RUN ;

DATA COMBO4;
   SET WPRATES  ;
      BY FIPS;
    IF FIPS = . THEN DELETE;




DATA QTRPRTFL ;
FILE OUT01 ;
   SET COMBO4 ;
STATEIS = &STNAME  ;
FQ = &FY&Q ;
PUT  @40 'TANF WORK PARTICIPATION RATES' ;
pUT @20 'STATE: ' @27 STATEIS @50 'QUARTER: ' @59 FQ / ; 
PUT @20'ALL FAMILY' @60 'TWO-PARENT' ;
PUT @20 'MONTH 1'  @30 'MONTH 2'  @40 'MONTH 3' @50 'AVERAGE' 
    @60 'MONTH 1'  @70 'MONTH 2'  @80 'MONTH 3' @90 'AVERAGE' ;
PUT @1 'TANF UNADJ. WPR'
      @20 AFM&MONA.WPR 5.1  @30 AFM&MONB.WPR 5.1  @40 AFM&MONC.WPR 5.1
      @50 Q&Q.AFWPR    5.1  @60 TPM&MONA.WPR 5.1  @70 TPM&MONB.WPR 5.1
      @80 TPM&MONC.WPR 5.1  @90 Q&Q.TPWPR  5.1 ;
PUT @1 'TANF UNADJUSTED NUMR'
      @20 AFM&MONA.WRK 8.1  @30 AFM&MONB.WRK 8.1  @40 AFM&MONC.WRK 8.1
      @60 TPM&MONA.WRK 8.1  @70 TPM&MONB.WRK 8.1  @80 TPM&MONC.WRK 8.1  ;
PUT @1 'TANF DENOMINATOR'
      @20 AFM&MONA.DEN 8.1  @30 AFM&MONB.DEN 8.1  @40 AFM&MONC.DEN 8.1
      @60 TPM&MONA.DEN 8.1  @70 TPM&MONB.DEN 8.1  @80 TPM&MONC.DEN 8.1  ;
PUT @1 'TANF ADJ. WPR'
      @20 AFM&MONA.APR 5.1  @30 AFM&MONB.APR 5.1  @40 AFM&MONC.APR 5.1
      @50 Q&Q.AFAPR    5.1  @60 TPM&MONA.APR 5.1  @70 TPM&MONB.APR 5.1
      @80 TPM&MONC.APR 5.1  @90 Q&Q.TPAPR  5.1 ;
PUT @1 'TANF ADJ. NUMERATOR'
      @20 AFM&MONA.AWK 8.1  @30 AFM&MONB.AWK 8.1  @40 AFM&MONC.AWK 8.1
      @60 TPM&MONA.AWK 8.1  @70 TPM&MONB.AWK 8.1  @80 TPM&MONC.AWK 8.1   ;

	  run ;

%mend createm;

/*** %macro createm(STNAME,FY,CY,YY,MONA,MONB,MONC,Q) ;  ***/
/** STNAME: State Name,                                  ***/
/** FY    : Fiscal Year,             format YYYY         ***/
/** CY    : Calendar Year,           format YYYY         ***/
/** YY    : 2 Digit CALENDAR year,   format YY           ***/
/** MONA  : First  Month of Quarter, format MM           ***/
/** MONB  : Second Month of Quarter, format MM           ***/
/** MONC  : Third  Month of Quarter, format MM           ***/
/** Q     : QUARTER DESIGNATION      format Q            ***/

%createm("SOUTH CAROLINA",2009,2008,08,10,11,12,1) ;
%createm("SOUTH CAROLINA",2009,2009,09,01,02,03,2) ;
%createm("SOUTH CAROLINA",2009,2009,09,04,05,06,3) ;
%createm("SOUTH CAROLINA",2009,2009,09,07,08,09,4) ;

QUIT;
