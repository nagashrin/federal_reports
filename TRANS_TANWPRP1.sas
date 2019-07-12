/***********************************************/
/* Program name:  TRANS_TANWPRP1.SAS           */
/***********************************************/
%macro createm(TRNFLE,FIP,FY,YYM,QTR,YQ,MON) ;
/************************************************************/
/* Parameters used in Program are as follows:              **/
/************************************************************/
/** TRNFLE: Name of the State Quarterly Transmission File ***/
/** FIP   : State FIPS Code                 format ##     ***/
/** FY    : 4 Dogot Fiscal Year             format YYYY   ***/
/** YYM   : Fiscal Year and Month           format YYYYMM ***/
/** QTR   : Fiscal Year and Quarter         format YYQ    ***/
/** YQ    : Fiscal Year and Quarter         format YYYYQ  ***/
/** MON   : 2 digit Year and month          format YYMM   ***/
/************************************************************/
/** Modify Parameters at Bottom of Program to Run for a   ***/
/** Different Fiscal Quarter or for a Different State.    ***/
/************************************************************/

LIBNAME  OLD   "C:\DOCUMENTS AND SETTINGS\PATRICK BRANNEN\MYDOCU~1\TRANSMISSION\FY&FY." ;

FILENAME  OUT  "C:\DOCUMENTS AND SETTINGS\PATRICK BRANNEN\MYDOCU~1\TRANSMISSION\FY&FY.\TN&YYM..P5B" ;
FILENAME  IN   "C:\DOCUMENTS AND SETTINGS\PATRICK BRANNEN\MYDOCU~1\TRANSMISSION\FY&FY.\&TRNFLE." ;
FILENAME WGT4  "C:\DOCUMENTS AND SETTINGS\PATRICK BRANNEN\MYDOCU~1\TRANSMISSION\FY&FY.\WGT&YQ._TAN.TXT" ;
FILENAME EFF   "C:\DOCUMENTS AND SETTINGS\PATRICK BRANNEN\MYDOCU~1\TRANSMISSION\FY&FY.\TANF&FIP._EF&MON..TXT" ;

%macro regionis;
 IF (FIPS_CODE = 09  OR
     FIPS_CODE = 23  OR
     FIPS_CODE = 25  OR 
     FIPS_CODE = 33  OR
     FIPS_CODE = 44  OR
     FIPS_CODE = 50) THEN REGION = 01 ;

 IF (FIPS_CODE = 34  OR
     FIPS_CODE = 36  OR
     FIPS_CODE = 72  OR 
     FIPS_CODE = 78) THEN REGION = 02 ;

 IF (FIPS_CODE = 10  OR
     FIPS_CODE = 11  OR
     FIPS_CODE = 24  OR 
     FIPS_CODE = 42  OR
     FIPS_CODE = 51  OR
     FIPS_CODE = 54) THEN REGION = 03 ;

 IF (FIPS_CODE = 01  OR
     FIPS_CODE = 12  OR
     FIPS_CODE = 13  OR
	 FIPS_CODE = 21  OR
     FIPS_CODE = 28  OR 
     FIPS_CODE = 37  OR
     FIPS_CODE = 45  OR
     FIPS_CODE = 47) THEN REGION = 04 ;

 IF (FIPS_CODE = 17  OR
     FIPS_CODE = 18  OR
     FIPS_CODE = 26  OR 
     FIPS_CODE = 39  OR
     FIPS_CODE = 55) THEN REGION = 05 ;

 IF (FIPS_CODE = 05  OR
     FIPS_CODE = 22  OR
     FIPS_CODE = 35  OR 
     FIPS_CODE = 40  OR
     FIPS_CODE = 48) THEN REGION = 06 ;

 IF (FIPS_CODE = 19  OR
     FIPS_CODE = 20  OR
     FIPS_CODE = 29  OR
     FIPS_CODE = 31) THEN REGION = 07 ;

 IF (FIPS_CODE = 08  OR
     FIPS_CODE = 30  OR
     FIPS_CODE = 38  OR 
     FIPS_CODE = 46  OR
     FIPS_CODE = 49  OR
     FIPS_CODE = 56) THEN REGION = 08 ;

 IF (FIPS_CODE = 04  OR
     FIPS_CODE = 06  OR
     FIPS_CODE = 15  OR 
     FIPS_CODE = 32  OR
     FIPS_CODE = 66) THEN REGION = 09 ;

 IF (FIPS_CODE = 02  OR
     FIPS_CODE = 16  OR
     FIPS_CODE = 41  OR
     FIPS_CODE = 53) THEN REGION = 10 ;

%mend;

/*-- macro for selecting fips and report month ----*/
%macro monthis;
 if rep_month_year=&YYM ;
%mend;


/*--- Macro var assign values for differenct adult --- */
%macro var(n);
   famaff_&n=FAMAFF_1     ;
   abirth_&n=ABIRTH_1     ;
   ncp_&n=NCP_1           ;
   AHEADH_&n=AHEADH_1     ;
   marital_&n=MARITAL_1   ;
   parent_&n=PARENT_1     ;
   wei_&n=wei_1           ;
   WPS_&n=WPS_1           ;
   citz_&n=citz_1         ;
   aidabd_&n=aidabd_1     ;
   recssi_&n=recssi_1     ;
   ssi_&n=ssi_1           ;
   ncorhrs_&n=ncorhrs_1   ;
   tpcorhrs_&n=tpcorhrs_1 ;
   A&n.work1=A1work1;
   A&n.work2=A1work2;
   A&n.work3=A1work3;
   A&n.work4=A1work4;
   A&n.work5=A1work5;
   A&n.work6=A1work6;
   A&n.work7=A1work7;
   A&n.work8=A1work8;
   A&n.work9=A1work9;
   A&n.work10=A1work10; 
   A&n.work11=A1work11;
   A&n.work12=A1work12;
   A&n.work13=A1work13;
   A&n.holi1=A1holi1;
   A&n.holi2=A1holi2;
   A&n.holi3=A1holi3;
   A&n.holi4=A1holi4; 
   A&n.holi5=A1holi5;
   A&n.holi6=A1holi6;
   A&n.holi7=A1holi7;
   A&n.holi8=A1holi8;
   A&n.exab1=A1exab1;
   A&n.exab2=A1exab2; 
   A&n.exab3=A1exab3;
   A&n.exab4=A1exab4;
   A&n.exab5=A1exab5;
   A&n.exab6=A1exab6;
   A&n.exab7=A1exab7;
   A&n.exab8=A1exab8;
%mend;

DATA HEADER   (KEEP=   CAL_QUARTER 
                       DATA_TYPE   
                       FIPS_CODE   
                       TRIBE_CODE  
                       PROGRAM_TYPE            
                       UPDATE_IND   
                       ENCRYP_INC) 

      FAMILY   (KEEP= REP_MONTH_YEAR              
                      CASE_NUMBER            
	                  COUNTY              
	                  STRATUM        
                      ZIP_CODE          
                      DISPOSITION             
	                  NEW_APPLICANT           
	                  FAMILY_TYPE             
	                  AMT_FOOD_STAMP_ASSISTANCE 
					  CHILD_SUPPORT_AMT
                      RECEIVES_SUB_CC           
                      CASH_AMOUNT               	     
                      CC_AMOUNT                
	                  TRANSP_AMOUNT          
                      TRANSITION_SERVICES_AMOUNT 
                      OTHER_AMOUNT             
	                 SANC_REDUCTION_AMT         
                     WORK_REQ_SANCTION    
                     fips_code
                     region)  

      ADULT (KEEP= REP_MONTH_YEAR          
                   CASE_NUMBER             
                   family_affiliation       
                   noncustodial_parent      
                   date_of_birth            
                   aid_aged_blind                 
                   receive_ssi               
                   marital_status           
                   relationship_hoh         
                   parent_minor_child       
	               citizenship_status     
                   work_eligible_indicator    
                   work_part_status           
                   unsub_employment           
                   sub_private_employment     
                   sub_public_employment      
                   work_experience_hop        
                   work_experience_ea         
                   work_experience_hol        
                   ojt                        
                   job_search_hop             
                    job_search_ea              
                    job_search_hol             
                    comm_services_hop          
                    comm_services_ea           
                    comm_services_hol          
                    vocational_ed_training_hop 
                    vocational_ed_training_ea  
                    vocational_ed_training_hol 
                    job_skills_training_hop    
                    job_skills_training_ea     
                    job_skills_training_hol    
                    ed_no_high_school_dipl_hop 
                    ed_no_high_school_dipl_ea  
                    ed_no_high_school_dipl_hol 
                    school_attendence_hop        
                    school_attendence_ea         
                    school_attendence_hol        
                    provide_cc_hop             
                    provide_cc_ea               
                    provide_cc_hol            
                    other_work_activities      
                    deemed_hours_overall       
                    deemed_hours_for_two_parent 
	                unearned_ssi   
                    fips_code) 

      CHILD  (KEEP= REP_MONTH_YEAR          
	                CASE_NUMBER             
                    date_of_birth 
                    fips_code)     

      TRAILER (KEEP= NO_REC ) ;
  INFILE IN LRECL= 156 PAD;
  INPUT REC_TYPE $ 1 - 2  BLK154 $ 3 - 156 ;
  IF REC_TYPE = "HE" THEN DO;
     CAL_QUARTER  = SUBSTR(BLK154,5,5)     ; 
     DATA_TYPE    = SUBSTR(BLK154,10,1)   ; 
     FIPS_CODE    = SUBSTR(BLK154,11,2)   ;
     TRIBE_CODE   = SUBSTR(BLK154,13,3)   ;
     PROGRAM_TYPE = SUBSTR(BLK154,16,3)   ;
     UPDATE_IND   = SUBSTR(BLK154,19,1)   ;
     ENCRYP_INC   = SUBSTR(BLK154,20,1)   ;
	 output header ;
	 RETURN;
  END;
  IF REC_TYPE = "T1" THEN DO;
    REP_MONTH_YEAR_C  = SUBSTR(BLK154,1,6)  ;
    REP_MONTH_YEAR    = INPUT(REP_MONTH_YEAR_C, 6.);
    CASE_NUMBER       = SUBSTR(BLK154,7,11)  ;     
    COUNTY_C          = SUBSTR(BLK154,18,3)  ;
    COUNTY            = INPUT (COUNTY_C, 3.) ;      
	STRATUM_C         = SUBSTR(BLK154,21,2)   ;
    STRATUM           = INPUT (STRATUM_C, 2.) ;  
    ZIP_CODE        = SUBSTR(BLK154,23,5)   ; 
    DISPOSITION_C   = SUBSTR(BLK154,29,1)   ;
    DISPOSITION     = INPUT (DISPOSITION_C, 1.) ;  
	NEW_APPLICANT_C = SUBSTR(BLK154,30,1)   ; 
    NEW_APPLICANT   = INPUT (NEW_APPLICANT_C, 1.) ; 
	FAMILY_TYPE_C   = SUBSTR(BLK154,33,1)   ;
    FAMILY_TYPE     = INPUT (FAMILY_TYPE_C, 1.) ; 
	AMT_FOOD_STAMP_ASSISTAN_C = SUBSTR(BLK154,37,4)   ; 
    AMT_FOOD_STAMP_ASSISTANCE = INPUT (AMT_FOOD_STAMP_ASSISTAN_C, 4.) ;
    CHILD_SUPPORT_AMT_C       = SUBSTR(BLK154,46,4) ;
    CHILD_SUPPORT_AMT         = INPUT (CHILD_SUPPORT_AMT_C, 4.) ; 
    RECEIVES_SUB_CC_C = SUBSTR(BLK154,41,1)   ; 
    RECEIVES_SUB_CC   = INPUT (RECEIVES_SUB_CC_C, 1.) ;      
    CASH_AMOUNT_C   = SUBSTR(BLK154,54,4)   ; 
    CASH_AMOUNT     = INPUT (CASH_AMOUNT_C, 4.) ; 
	CC_AMOUNT_C     = SUBSTR(BLK154,61,4)   ;
    CC_AMOUNT       = INPUT (CC_AMOUNT_C, 4.) ;
	TRANSP_AMOUNT_C = SUBSTR(BLK154,70,4)   ;
    TRANSP_AMOUNT   = INPUT (TRANSP_AMOUNT_C, 4.) ; 
    TRANSITION_SERVICES_AMNT_C = SUBSTR(BLK154,77,4)   ; 
    TRANSITION_SERVICES_AMOUNT = INPUT (TRANSITION_SERVICES_AMNT_C, 4.) ;
    OTHER_AMOUNT_C  = SUBSTR(BLK154,84,4)   ;
    OTHER_AMOUNT    = INPUT (OTHER_AMOUNT_C, 4.) ; 
	SANC_REDUCTION_A_C = SUBSTR(BLK154,91,4)   ;
    SANC_REDUCTION_AMT = INPUT (SANC_REDUCTION_A_C, 4.) ;   
    WORK_REQ_SANCTION  = SUBSTR(BLK154,95,1)   ;     
    fips_code = &fip;
	%regionis ;
    output family;
    RETURN;
END;
  IF REC_TYPE = "T2" THEN DO;
    REP_MONTH_YEAR_C   = SUBSTR(BLK154,1,6)   ;
    REP_MONTH_YEAR     = INPUT (REP_MONTH_YEAR_C, 6.) ; 
    CASE_NUMBER        = SUBSTR(BLK154,7,11)  ;
    family_affiliation_c = SUBSTR(BLK154,18,1)  ;
    FAMILY_AFFILIATION   = INPUT (FAMILY_AFFILIATION_C, 1.) ;
    noncustodial_parent_c = SUBSTR(BLK154,19,1) ;
    noncustodial_parent   = INPUT (noncustodial_parent_c, 1.) ;
    date_of_birth_c      = SUBSTR(BLK154,20,8)   ; 
    date_of_birth        = INPUT (DATE_OF_BIRTH_C, 8.) ; 
    aid_aged_blind       = SUBSTR(BLK154,47,1)   ;         
    receive_ssi          = SUBSTR(BLK154,48,1)   ;  
    marital_status       = SUBSTR(BLK154,49,1)   ; 
    relationship_hoh_C   = SUBSTR(BLK154,50,2)   ;
    relationship_hoh     = INPUT (relationship_hoh_c, 2.) ;
    parent_minor_child = SUBSTR(BLK154,52,1)   ;
	citizenship_status = SUBSTR(BLK154,56,1)   ;       
    work_eligible_indicator = SUBSTR(BLK154,65,2)   ;  
    work_part_status       = SUBSTR(BLK154,67,2)   ;  
    unsub_employment       = SUBSTR(BLK154,69,2)   ;   
    sub_private_employment = SUBSTR(BLK154,71,2)   ;   
    sub_public_employment  = SUBSTR(BLK154,73,2)   ; 
    work_experience_hop    = SUBSTR(BLK154,75,2)   ;   
    work_experience_ea     = SUBSTR(BLK154,77,2)   ;  
    work_experience_hol    = SUBSTR(BLK154,79,2)   ;   
    ojt                    = SUBSTR(BLK154,81,2)   ; 
    job_search_hop         = SUBSTR(BLK154,83,2)   ;  
    job_search_ea          = SUBSTR(BLK154,85,2)   ;  
    job_search_hol         = SUBSTR(BLK154,87,2)   ;  
    comm_services_hop      = SUBSTR(BLK154,89,2)   ; 
    comm_services_ea       = SUBSTR(BLK154,91,2)   ;  
    comm_services_hol      = SUBSTR(BLK154,93,2)   ; 
    vocational_ed_training_hop = SUBSTR(BLK154,95,2)   ; 
    vocational_ed_training_ea  = SUBSTR(BLK154,97,2)   ; 
    vocational_ed_training_hol = SUBSTR(BLK154,99,2)   ; 
    job_skills_training_hop = SUBSTR(BLK154,101,2)   ;  
    job_skills_training_ea  = SUBSTR(BLK154,103,2)   ;  
    job_skills_training_hol = SUBSTR(BLK154,105,2)   ;  
    ed_no_high_school_dipl_hop = SUBSTR(BLK154,107,2)   ; 
    ed_no_high_school_dipl_ea  = SUBSTR(BLK154,109,2)   ; 
    ed_no_high_school_dipl_hol = SUBSTR(BLK154,111,2)   ; 
    school_attendence_hop   = SUBSTR(BLK154,113,2)   ;   
    school_attendence_ea    = SUBSTR(BLK154,115,2)   ;    
    school_attendence_hol   = SUBSTR(BLK154,117,2)   ;    
    provide_cc_hop          = SUBSTR(BLK154,119,2)   ;  
    provide_cc_ea           = SUBSTR(BLK154,121,2)   ; 
    provide_cc_hol          = SUBSTR(BLK154,123,2)   ;  
    other_work_activities   = SUBSTR(BLK154,125,2)   ; 
    deemed_hours_overall    = SUBSTR(BLK154,127,2)   ;  
    deemed_hours_for_two_parent = SUBSTR(BLK154,129,2)   ;  
	unearned_ssi            = SUBSTR(BLK154,143,4)   ;  
    fips_code = &fip;
    output adult;
	RETURN;
  END;
  IF REC_TYPE = "T3" THEN DO;
    REP_MONTH_YEAR_c   = SUBSTR(BLK154,1,6)   ;
    REP_MONTH_YEAR     = INPUT (REP_MONTH_YEAR_C, 8.) ;
	CASE_NUMBER        = SUBSTR(BLK154,7,11)   ;  
    date_of_birth_1c   = SUBSTR(BLK154,19,8)   ;    
    date_of_birth_1    = INPUT (date_of_birth_1c, 8.);
    date_of_birth_2c   = SUBSTR(BLK154,60,8)   ; 
	date_of_birth_2    = INPUT (date_of_birth_2c, 8.) ;
    fips_code = &fip;
	if date_of_birth_2 > 0 then do;
       date_of_birth = date_of_birth_2;
       output child;
	end;

	date_of_birth = date_of_birth_1;
    output child;
	RETURN;
  END;
  IF REC_TYPE = "TR" THEN DO;
    NO_REC = SUBSTR(BLK154,6,7) ;
	output trailer;
	RETURN;
  END;

  RUN;

/*--------------------------------*/
/*-       child file             -*/
/*--------------------------------*/
data child;
   set child;
   %monthis;

rptm_yy=floor(rep_month_year/100);
rptm_mm=mod(rep_month_year,100);

rename date_of_birth=birth
       case_number=caseid;
run;

proc sort data=child;
by fips_code caseid;
run;

data child1 (keep=fips_code caseid birth1 rptm_yy rptm_mm)
     child2 (keep=fips_code caseid birth2 rptm_yy rptm_mm)
     child3 (keep=fips_code caseid birth3 rptm_yy rptm_mm)
     child4 (keep=fips_code caseid birth4 rptm_yy rptm_mm)
     child5 (keep=fips_code caseid birth5 rptm_yy rptm_mm)
     child6 (keep=fips_code caseid birth6 rptm_yy rptm_mm)
     child7 (keep=fips_code caseid birth7 rptm_yy rptm_mm)
     child8 (keep=fips_code caseid birth8 rptm_yy rptm_mm)
     child9 (keep=fips_code caseid birth9 rptm_yy rptm_mm)
     child10(keep=fips_code caseid birth10 rptm_yy rptm_mm);
set child;
by fips_code caseid;

if first.caseid then do;
total=1;
birth1=birth;
output child1;
end;

else total+1;

if total=2 then do; birth2=birth; output child2;end;
if total=3 then do; birth3=birth; output child3;end;
if total=4 then do; birth4=birth; output child4;end;
if total=5 then do; birth5=birth; output child5;end;
if total=6 then do; birth6=birth; output child6;end;
if total=7 then do; birth7=birth; output child7;end;
if total=8 then do; birth8=birth; output child8;end;
if total=9 then do; birth9=birth; output child9;end;
if total=10 then do; birth10=birth; output child10;end;
run;

/*--------------------------------*/
/*-       adult file             -*/
/*--------------------------------*/

data adult (keep=fips_code
        case_number
        unsub_employment
        sub_private_employment
        sub_public_employment
        work_experience_hop
        ojt
        job_search_hop
        comm_services_hop
        vocational_ed_training_hop
        job_skills_training_hop
        ed_no_high_school_dipl_hop
        school_attendence_hop
        provide_cc_hop
		work_experience_hol
        job_search_hol
        comm_services_hol
        vocational_ed_training_hol
        job_skills_training_hol
        ed_no_high_school_dipl_hol
        school_attendence_hol
        provide_cc_hol
		work_experience_ea
        job_search_ea
        comm_services_ea
        vocational_ed_training_ea
        job_skills_training_ea
        ed_no_high_school_dipl_ea
        school_attendence_ea
        provide_cc_ea
        other_work_activities

       family_affiliation
       noncustodial_parent
       date_of_birth
       marital_status
	   aid_aged_blind
	   receive_ssi
       relationship_hoh
       parent_minor_child
	   work_eligible_indicator
       work_part_status
       deemed_hours_for_overall
	   deemed_hours_for_two_parent
	   citizenship_status
	   unearned_ssi
       rep_month_year);
   set adult;
   %monthis;

     if (deemed_hours_for_overall    < ' 0'  or                                     
         deemed_hours_for_overall    > '99') then deemed_hours_for_overall = '00' ; 
	 if (deemed_hours_for_two_parent < ' 0'  or                                       
         deemed_hours_for_two_parent > '99') then deemed_hours_for_two_parent = '00' ;
    if (relationship_hoh <  1 or relationship_hoh > 10) then relationship_hoh = . ;

run;

data adult (keep=fips_code caseid
         famaff_1 abirth_1 ncp_1 aheadh_1
         parent_1 wei_1 wps_1 citz_1 ssi_1
		 aidabd_1 recssi_1 ncorhrs_1 tpcorhrs_1
         a1work1--a1work13 a1holi1--a1holi8
         a1exab1--a1exab8 rptm_yy rptm_mm);
set adult;

rptm_yy=floor(rep_month_year/100);
rptm_mm=mod(rep_month_year,100);

rename  case_number=caseid
        unsub_employment=a1work1
        sub_private_employment=a1work2
        sub_public_employment=a1work3
        work_experience_hop=a1work4
        ojt=a1work5
        job_search_hop=a1work6
        comm_services_hop=a1work7
        vocational_ed_training_hop=a1work8
        job_skills_training_hop=a1work9
        ed_no_high_school_dipl_hop=a1work10
        school_attendence_hop=a1work11
        provide_cc_hop=a1work12
 
        work_experience_hol=a1holi1
        job_search_hol=a1holi2
        comm_services_hol=a1holi3
        vocational_ed_training_hol=a1holi4
        job_skills_training_hol=a1holi5
        ed_no_high_school_dipl_hol=a1holi6
        school_attendence_hol=a1holi7
        provide_cc_hol=a1holi8
        work_experience_ea=a1exab1
        job_search_ea=a1exab2
        comm_services_ea=a1exab3
        vocational_ed_training_ea=a1exab4
        job_skills_training_ea=a1exab5
        ed_no_high_school_dipl_ea=a1exab6
        school_attendence_ea=a1exab7
        provide_cc_ea=a1exab8
        other_work_activities=a1work13

       family_affiliation=famaff_1
       noncustodial_parent=ncp_1
       date_of_birth=abirth_1
	   aid_aged_blind=aidabd_1
	   receive_ssi=recssi_1
       marital_status=marital_1
       relationship_hoh=aheadh_1
       parent_minor_child=parent_1
	   work_eligible_indicator=wei_1
       work_part_status=wps_1
       deemed_hours_for_overall=ncorhrs_1
	   deemed_hours_for_two_parent=tpcorhrs_1
       citizenship_status=citz_1
	   unearned_ssi=ssi_1;

run;

proc sort data=adult;
by fips_code caseid;
run;


/* rename var for each adult */

data adult1(keep=fips_code caseid rptm_yy rptm_mm famaff_1 abirth_1 ncp_1
                 aheadh_1 marital_1 parent_1 wei_1 wps_1 ncorhrs_1 tpcorhrs_1
                 recssi_1 citz_1 ssi_1 aidabd_1 
                 a1work1 - a1work13 a1holi1 - a1holi8 a1exab1-a1exab8)
     adult2(keep=fips_code caseid rptm_yy rptm_mm famaff_2 abirth_2 ncp_2
                 aheadh_2 marital_2 parent_2 wei_2 wps_2 ncorhrs_2 tpcorhrs_2
                 recssi_2 citz_2 ssi_2 aidabd_2
                 a2work1 - a2work13 a2holi1-a2holi8 a2exab1-a2exab8)
     adult3(keep=fips_code caseid rptm_yy rptm_mm famaff_3 abirth_3 ncp_3
                 aheadh_3 marital_3 parent_3 wei_3 wps_3 ncorhrs_3 tpcorhrs_3
                 recssi_3 citz_3 ssi_3 aidabd_3
                 a3work1 - a3work13 a3holi1-a3holi8 a3exab1-a3exab8)
     adult4(keep=fips_code caseid rptm_yy rptm_mm famaff_4 abirth_4 ncp_4
                 aheadh_4 marital_4 parent_4 wei_4 wps_4 ncorhrs_4 tpcorhrs_4
                 recssi_4 citz_4 ssi_4 aidabd_4
                 a4work1 - a4work13 a4holi1-a4holi8 a4exab1-a4exab8)
     adult5(keep=fips_code caseid rptm_yy rptm_mm famaff_5 abirth_5 ncp_5
                 aheadh_5 marital_5 parent_5 wei_5 wps_5 ncorhrs_5 tpcorhrs_5
                 recssi_5 citz_5 ssi_5 aidabd_5
                 a5work1 - a5work13 a5holi1-a5holi8 a5exab1-a5exab8)
     adult6(keep=fips_code caseid rptm_yy rptm_mm famaff_6 abirth_6 ncp_6
                 aheadh_6 marital_6 parent_6 wei_6 wps_6 ncorhrs_6 tpcorhrs_6
                 recssi_6 citz_6 ssi_6 aidabd_6
                 a6work1 - a6work13 a6holi1-a6holi8 a6exab1-a6exab8);
set adult;
by fips_code caseid;


if first.caseid then do;
   total=1;
   %var(1);
   output adult1;
end;

else total+1;
if total=2 then do; %var(2);output adult2;end;
if total=3 then do; %var(3); output adult3;end;
if total=4 then do; %var(4); output adult4;end;
if total=5 then do; %var(5); output adult5;end;
if total=6 then do; %var(6); output adult6;end;
run;

/*--------------------------------*/
/*-       family file             -*/
/*--------------------------------*/

data family (keep=fips_code
       case_number
	   region
	   county
	   zip_code
       stratum
       disposition
       family_type
       receives_sub_cc
	   amt_food_stamp_assistance
	   child_support_amt
	   cash_amount
	   cc_amount
	   other_amount
	   SANC_REDUCTION_AMT
	   transition_services_amount
	   transp_amount
   	   work_REQ_SANCTION
       rep_month_year);
   set family;
   %monthis;
run;

data family;
set family;
rptm_yy=floor(rep_month_year/100);
rptm_mm=mod(rep_month_year,100);

if rptm_mm >= 10 then year = rptm_yy + 1 ;
else year = rptm_yy ;

rename case_number=caseid
       disposition=disp
       family_type=type_fam
       receives_sub_cc=rec_cc
       amt_food_stamp_assistance=fs_amt
	   child_support_amt= cs_amt
       cash_amount=cash_amt
       cc_amount=cc_amt
       other_amount=oth_amt
	   SANC_REDUCTION_AMT=SANC_AMT
       transition_services_amount=trst_amt
       transp_amount=trnp_amt
       work_REQ_SANCTION=work_SANC ;
run;

proc sort data=family;
by fips_code caseid;
run;


/*----------------------------*/
/*- merging all temp files   -*/
/*----------------------------*/
data Pre_final;
merge family
      adult1 adult2 adult3 adult4 adult5 adult6

      child1 child2 child3 child4 child5 child6
      child7 child8 child9 child10;


by fips_code caseid ;
run;

/** proc contents; run; **/

PROC DELETE DATA =  family
      adult1 adult2 adult3 adult4 adult5 adult6
      child1 child2 child3 child4 child5 child6
      child7 child8 child9 child10;

run ;

/*----------------------------*/
/*- Numeric convertion       -*/
/*----------------------------*/
data numcon (drop= a1work1 - a1work13
a2work1 - a2work13
a3work1 - a3work13
a4work1 - a4work13
a5work1 - a5work13
a6work1 - a6work13
a1holi1 - a1holi8
a2holi1 - a2holi8
a3holi1 - a3holi8
a4holi1 - a4holi8
a5holi1 - a5holi8
a6holi1 - a6holi8
a1exab1 - a1exab8
a2exab1 - a2exab8
a3exab1 - a3exab8
a4exab1 - a4exab8
a5exab1 - a5exab8
a6exab1 - a6exab8
wei_1 wei_2 wei_3 wei_4 wei_5 wei_6
wps_1 wps_2 wps_3 wps_4 wps_5 wps_6
citz_1 citz_2 citz_3 citz_4 citz_5 citz_6
parent_1 parent_2 parent_3 parent_4 parent_5 parent_6
aidabd_1 aidabd_2 aidabd_3 aidabd_4 aidabd_5 aidabd_6
recssi_1 recssi_2 recssi_3 recssi_4 recssi_5 recssi_6 
ncorhrs_1 ncorhrs_2 ncorhrs_3 ncorhrs_4 ncorhrs_5 ncorhrs_6 
tpcorhrs_1 tpcorhrs_2 tpcorhrs_3 tpcorhrs_4 tpcorhrs_5 tpcorhrs_6);
set Pre_final;

Na1work1  = INPUT(a1work1 ,  2.) ;
Na1work2  = INPUT(a1work2 ,  2.) ;
Na1work3  = INPUT(a1work3 ,  2.) ;
Na1work4  = INPUT(a1work4 ,  2.) ;
Na1work5  = INPUT(a1work5 ,  2.) ;
Na1work6  = INPUT(a1work6 ,  2.) ;
Na1work7  = INPUT(a1work7 ,  2.) ;
Na1work8  = INPUT(a1work8 ,  2.) ;
Na1work9  = INPUT(a1work9 ,  2.) ;
Na1work10 = INPUT(a1work10,  2.) ;
Na1work11 = INPUT(a1work11,  2.) ;
Na1work12 = INPUT(a1work12,  2.) ;
Na1work13 = INPUT(a1work13,  2.) ;

Na2work1  = INPUT(a2work1 ,  2.) ;
Na2work2  = INPUT(a2work2 ,  2.) ;
Na2work3  = INPUT(a2work3 ,  2.) ;
Na2work4  = INPUT(a2work4 ,  2.) ;
Na2work5  = INPUT(a2work5 ,  2.) ;
Na2work6  = INPUT(a2work6 ,  2.) ;
Na2work7  = INPUT(a2work7 ,  2.) ;
Na2work8  = INPUT(a2work8 ,  2.) ;
Na2work9  = INPUT(a2work9 ,  2.) ;
Na2work10 = INPUT(a2work10,  2.) ;
Na2work11 = INPUT(a2work11,  2.) ;
Na2work12 = INPUT(a2work12,  2.) ;
Na2work13 = INPUT(a2work13,  2.) ;

Na3work1  = INPUT(a3work1 ,  2.) ;
Na3work2  = INPUT(a3work2 ,  2.) ;
Na3work3  = INPUT(a3work3 ,  2.) ;
Na3work4  = INPUT(a3work4 ,  2.) ;
Na3work5  = INPUT(a3work5 ,  2.) ;
Na3work6  = INPUT(a3work6 ,  2.) ;
Na3work7  = INPUT(a3work7 ,  2.) ;
Na3work8  = INPUT(a3work8 ,  2.) ;
Na3work9  = INPUT(a3work9 ,  2.) ;
Na3work10 = INPUT(a3work10,  2.) ;
Na3work11 = INPUT(a3work11,  2.) ;
Na3work12 = INPUT(a3work12,  2.) ;
Na3work13 = INPUT(a3work13,  2.) ;

Na4work1  = INPUT(a4work1 ,  2.) ;
Na4work2  = INPUT(a4work2 ,  2.) ;
Na4work3  = INPUT(a4work3 ,  2.) ;
Na4work4  = INPUT(a4work4 ,  2.) ;
Na4work5  = INPUT(a4work5 ,  2.) ;
Na4work6  = INPUT(a4work6 ,  2.) ;
Na4work7  = INPUT(a4work7 ,  2.) ;
Na4work8  = INPUT(a4work8 ,  2.) ;
Na4work9  = INPUT(a4work9 ,  2.) ;
Na4work10 = INPUT(a4work10,  2.) ;
Na4work11 = INPUT(a4work11,  2.) ;
Na4work12 = INPUT(a4work12,  2.) ;
Na4work13 = INPUT(a4work13,  2.) ;

Na5work1  = INPUT(a5work1 ,  2.) ;
Na5work2  = INPUT(a5work2 ,  2.) ;
Na5work3  = INPUT(a5work3 ,  2.) ;
Na5work4  = INPUT(a5work4 ,  2.) ;
Na5work5  = INPUT(a5work5 ,  2.) ;
Na5work6  = INPUT(a5work6 ,  2.) ;
Na5work7  = INPUT(a5work7 ,  2.) ;
Na5work8  = INPUT(a5work8 ,  2.) ;
Na5work9  = INPUT(a5work9 ,  2.) ;
Na5work10 = INPUT(a5work10,  2.) ;
Na5work11 = INPUT(a5work11,  2.) ;
Na5work12 = INPUT(a5work12,  2.) ;
Na5work13 = INPUT(a5work13,  2.) ;

Na6work1  = INPUT(a6work1 ,  2.) ;
Na6work2  = INPUT(a6work2 ,  2.) ;
Na6work3  = INPUT(a6work3 ,  2.) ;
Na6work4  = INPUT(a6work4 ,  2.) ;
Na6work5  = INPUT(a6work5 ,  2.) ;
Na6work6  = INPUT(a6work6 ,  2.) ;
Na6work7  = INPUT(a6work7 ,  2.) ;
Na6work8  = INPUT(a6work8 ,  2.) ;
Na6work9  = INPUT(a6work9 ,  2.) ;
Na6work10 = INPUT(a6work10,  2.) ;
Na6work11 = INPUT(a6work11,  2.) ;
Na6work12 = INPUT(a6work12,  2.) ;
Na6work13 = INPUT(a6work13,  2.) ;

Na1holi1  = INPUT(a1holi1 ,  2.) ;
Na1holi2  = INPUT(a1holi2 ,  2.) ;
Na1holi3  = INPUT(a1holi3 ,  2.) ;
Na1holi4  = INPUT(a1holi4 ,  2.) ;
Na1holi5  = INPUT(a1holi5 ,  2.) ;
Na1holi6  = INPUT(a1holi6 ,  2.) ;
Na1holi7  = INPUT(a1holi7 ,  2.) ;
Na1holi8  = INPUT(a1holi8 ,  2.) ;

Na2holi1  = INPUT(a2holi1 ,  2.) ;
Na2holi2  = INPUT(a2holi2 ,  2.) ;
Na2holi3  = INPUT(a2holi3 ,  2.) ;
Na2holi4  = INPUT(a2holi4 ,  2.) ;
Na2holi5  = INPUT(a2holi5 ,  2.) ;
Na2holi6  = INPUT(a2holi6 ,  2.) ;
Na2holi7  = INPUT(a2holi7 ,  2.) ;
Na2holi8  = INPUT(a2holi8 ,  2.) ;

Na3holi1  = INPUT(a3holi1 ,  2.) ;
Na3holi2  = INPUT(a3holi2 ,  2.) ;
Na3holi3  = INPUT(a3holi3 ,  2.) ;
Na3holi4  = INPUT(a3holi4 ,  2.) ;
Na3holi5  = INPUT(a3holi5 ,  2.) ;
Na3holi6  = INPUT(a3holi6 ,  2.) ;
Na3holi7  = INPUT(a3holi7 ,  2.) ;
Na3holi8  = INPUT(a3holi8 ,  2.) ;

Na4holi1  = INPUT(a4holi1 ,  2.) ;
Na4holi2  = INPUT(a4holi2 ,  2.) ;
Na4holi3  = INPUT(a4holi3 ,  2.) ;
Na4holi4  = INPUT(a4holi4 ,  2.) ;
Na4holi5  = INPUT(a4holi5 ,  2.) ;
Na4holi6  = INPUT(a4holi6 ,  2.) ;
Na4holi7  = INPUT(a4holi7 ,  2.) ;
Na4holi8  = INPUT(a4holi8 ,  2.) ;

Na5holi1  = INPUT(a5holi1 ,  2.) ;
Na5holi2  = INPUT(a5holi2 ,  2.) ;
Na5holi3  = INPUT(a5holi3 ,  2.) ;
Na5holi4  = INPUT(a5holi4 ,  2.) ;
Na5holi5  = INPUT(a5holi5 ,  2.) ;
Na5holi6  = INPUT(a5holi6 ,  2.) ;
Na5holi7  = INPUT(a5holi7 ,  2.) ;
Na5holi8  = INPUT(a5holi8 ,  2.) ;

Na6holi1  = INPUT(a6holi1 ,  2.) ;
Na6holi2  = INPUT(a6holi2 ,  2.) ;
Na6holi3  = INPUT(a6holi3 ,  2.) ;
Na6holi4  = INPUT(a6holi4 ,  2.) ;
Na6holi5  = INPUT(a6holi5 ,  2.) ;
Na6holi6  = INPUT(a6holi6 ,  2.) ;
Na6holi7  = INPUT(a6holi7 ,  2.) ;
Na6holi8  = INPUT(a6holi8 ,  2.) ;

Na1exab1  = INPUT(a1exab1 ,  2.) ;
Na1exab2  = INPUT(a1exab2 ,  2.) ;
Na1exab3  = INPUT(a1exab3 ,  2.) ;
Na1exab4  = INPUT(a1exab4 ,  2.) ;
Na1exab5  = INPUT(a1exab5 ,  2.) ;
Na1exab6  = INPUT(a1exab6 ,  2.) ;
Na1exab7  = INPUT(a1exab7 ,  2.) ;
Na1exab8  = INPUT(a1exab8 ,  2.) ;

Na2exab1  = INPUT(a2exab1 ,  2.) ;
Na2exab2  = INPUT(a2exab2 ,  2.) ;
Na2exab3  = INPUT(a2exab3 ,  2.) ;
Na2exab4  = INPUT(a2exab4 ,  2.) ;
Na2exab5  = INPUT(a2exab5 ,  2.) ;
Na2exab6  = INPUT(a2exab6 ,  2.) ;
Na2exab7  = INPUT(a2exab7 ,  2.) ;
Na2exab8  = INPUT(a2exab8 ,  2.) ;

Na3exab1  = INPUT(a3exab1 ,  2.) ;
Na3exab2  = INPUT(a3exab2 ,  2.) ;
Na3exab3  = INPUT(a3exab3 ,  2.) ;
Na3exab4  = INPUT(a3exab4 ,  2.) ;
Na3exab5  = INPUT(a3exab5 ,  2.) ;
Na3exab6  = INPUT(a3exab6 ,  2.) ;
Na3exab7  = INPUT(a3exab7 ,  2.) ;
Na3exab8  = INPUT(a3exab8 ,  2.) ;

Na4exab1  = INPUT(a4exab1 ,  2.) ;
Na4exab2  = INPUT(a4exab2 ,  2.) ;
Na4exab3  = INPUT(a4exab3 ,  2.) ;
Na4exab4  = INPUT(a4exab4 ,  2.) ;
Na4exab5  = INPUT(a4exab5 ,  2.) ;
Na4exab6  = INPUT(a4exab6 ,  2.) ;
Na4exab7  = INPUT(a4exab7 ,  2.) ;
Na4exab8  = INPUT(a4exab8 ,  2.) ;

Na5exab1  = INPUT(a5exab1 ,  2.) ;
Na5exab2  = INPUT(a5exab2 ,  2.) ;
Na5exab3  = INPUT(a5exab3 ,  2.) ;
Na5exab4  = INPUT(a5exab4 ,  2.) ;
Na5exab5  = INPUT(a5exab5 ,  2.) ;
Na5exab6  = INPUT(a5exab6 ,  2.) ;
Na5exab7  = INPUT(a5exab7 ,  2.) ;
Na5exab8  = INPUT(a5exab8 ,  2.) ;

Na6exab1  = INPUT(a6exab1 ,  2.) ;
Na6exab2  = INPUT(a6exab2 ,  2.) ;
Na6exab3  = INPUT(a6exab3 ,  2.) ;
Na6exab4  = INPUT(a6exab4 ,  2.) ;
Na6exab5  = INPUT(a6exab5 ,  2.) ;
Na6exab6  = INPUT(a6exab6 ,  2.) ;
Na6exab7  = INPUT(a6exab7 ,  2.) ;
Na6exab8  = INPUT(a6exab8 ,  2.) ;
 
Nwei_1 = INPUT(wei_1,  2.) ;
Nwei_2 = INPUT(wei_2,  2.) ;
Nwei_3 = INPUT(wei_3,  2.) ;
Nwei_4 = INPUT(wei_4,  2.) ;
Nwei_5 = INPUT(wei_5,  2.) ;
Nwei_6 = INPUT(wei_6,  2.) ;

Nwps_1 = INPUT(wps_1,  2.) ;
Nwps_2 = INPUT(wps_2,  2.) ;
Nwps_3 = INPUT(wps_3,  2.) ;
Nwps_4 = INPUT(wps_4,  2.) ;
Nwps_5 = INPUT(wps_5,  2.) ;
Nwps_6 = INPUT(wps_6,  2.) ;

Nparent_1 = INPUT(parent_1,  1.) ;
Nparent_2 = INPUT(parent_2,  1.) ;
Nparent_3 = INPUT(parent_3,  1.) ;
Nparent_4 = INPUT(parent_4,  1.) ;
Nparent_5 = INPUT(parent_5,  1.) ;
Nparent_6 = INPUT(parent_6,  1.) ;

Ncitz_1 = INPUT(citz_1,  1.) ;
Ncitz_2 = INPUT(citz_2,  1.) ;
Ncitz_3 = INPUT(citz_3,  1.) ;
Ncitz_4 = INPUT(citz_4,  1.) ;
Ncitz_5 = INPUT(citz_5,  1.) ;
Ncitz_6 = INPUT(citz_6,  1.) ;

Naidabd_1 = INPUT(aidabd_1,  1.) ;
Naidabd_2 = INPUT(aidabd_2,  1.) ;
Naidabd_3 = INPUT(aidabd_3,  1.) ;
Naidabd_4 = INPUT(aidabd_4,  1.) ;
Naidabd_5 = INPUT(aidabd_5,  1.) ;
Naidabd_6 = INPUT(aidabd_6,  1.) ;

Nrecssi_1 = INPUT(recssi_1,  1.) ;
Nrecssi_2 = INPUT(recssi_2,  1.) ;
Nrecssi_3 = INPUT(recssi_3,  1.) ;
Nrecssi_4 = INPUT(recssi_4,  1.) ;
Nrecssi_5 = INPUT(recssi_5,  1.) ;
Nrecssi_6 = INPUT(recssi_6,  1.) ;

Nncorhrs_1 = INPUT(ncorhrs_1,  2.) ;
Nncorhrs_2 = INPUT(ncorhrs_2,  2.) ;
Nncorhrs_3 = INPUT(ncorhrs_3,  2.) ;
Nncorhrs_4 = INPUT(ncorhrs_4,  2.) ;
Nncorhrs_5 = INPUT(ncorhrs_5,  2.) ;
Nncorhrs_6 = INPUT(ncorhrs_6,  2.) ;

Ntpcorhrs_1 = INPUT(tpcorhrs_1,  2.) ;
Ntpcorhrs_2 = INPUT(tpcorhrs_2,  2.) ;
Ntpcorhrs_3 = INPUT(tpcorhrs_3,  2.) ;
Ntpcorhrs_4 = INPUT(tpcorhrs_4,  2.) ;
Ntpcorhrs_5 = INPUT(tpcorhrs_5,  2.) ;
Ntpcorhrs_6 = INPUT(tpcorhrs_6,  2.) ;

run;

data final;
set numcon;

rename
Na1work1 - Na1work13=a1work1 - a1work13
Na2work1 - Na2work13=a2work1 - a2work13
Na3work1 - Na3work13=a3work1 - a3work13
Na4work1 - Na4work13=a4work1 - a4work13
Na5work1 - Na5work13=a5work1 - a5work13
Na6work1 - Na6work13=a6work1 - a6work13
Na1holi1 - Na1holi8=a1holi1 - a1holi8
Na2holi1 - Na2holi8=a2holi1 - a2holi8
Na3holi1 - Na3holi8=a3holi1 - a3holi8
Na4holi1 - Na4holi8=a4holi1 - a4holi8
Na5holi1 - Na5holi8=a5holi1 - a5holi8
Na6holi1 - Na6holi8=a6holi1 - a6holi8
Na1exab1 - Na1exab8=a1exab1 - a1exab8
Na2exab1 - Na2exab8=a2exab1 - a2exab8
Na3exab1 - Na3exab8=a3exab1 - a3exab8
Na4exab1 - Na4exab8=a4exab1 - a4exab8
Na5exab1 - Na5exab8=a5exab1 - a5exab8
Na6exab1 - Na6exab8=a6exab1 - a6exab8
Nwei_1=wei_1
Nwei_2=wei_2
Nwei_3=wei_3
Nwei_4=wei_4
Nwei_5=wei_5
Nwei_6=wei_6

Nwps_1=wps_1
Nwps_2=wps_2
Nwps_3=wps_3
Nwps_4=wps_4
Nwps_5=wps_5
Nwps_6=wps_6

Nparent_1=parent_1
Nparent_2=parent_2
Nparent_3=parent_3
Nparent_4=parent_4
Nparent_5=parent_5
Nparent_6=parent_6

Ncitz_1=citz_1
Ncitz_2=citz_2
Ncitz_3=citz_3
Ncitz_4=citz_4
Ncitz_5=citz_5
Ncitz_6=citz_6

Naidabd_1=aidabd_1
Naidabd_2=aidabd_2
Naidabd_3=aidabd_3
Naidabd_4=aidabd_4
Naidabd_5=aidabd_5
Naidabd_6=aidabd_6

Nrecssi_1=recssi_1
Nrecssi_2=recssi_2
Nrecssi_3=recssi_3
Nrecssi_4=recssi_4
Nrecssi_5=recssi_5
Nrecssi_6=recssi_6

Nncorhrs_1=ncorhrs_1
Nncorhrs_2=ncorhrs_2
Nncorhrs_3=ncorhrs_3
Nncorhrs_4=ncorhrs_4
Nncorhrs_5=ncorhrs_5
Nncorhrs_6=ncorhrs_6

Ntpcorhrs_1=tpcorhrs_1
Ntpcorhrs_2=tpcorhrs_2
Ntpcorhrs_3=tpcorhrs_3
Ntpcorhrs_4=tpcorhrs_4
Ntpcorhrs_5=tpcorhrs_5
Ntpcorhrs_6=tpcorhrs_6

fips_code=fips;


run;

PROC DELETE DATA= Pre_Final  numcon;
RUN ;

****************************************************************
*  Main Program Version 2.
****************************************************************;

/*-------------------------------------------------*/
/*-   Initialize counters for all family work     -*/
/*-         participation rates                   -*/
/*-------------------------------------------------*/
%macro INIT_AF;
      NEXEMPT  = 0 ;
      NDCARD1  = 0 ;
      NDCARD2  = 0 ;
      NDCARD5  = 0 ;
      NDEEM15  = 0 ;
      NDEEM16  = 0 ;
      NDEEM17  = 0 ;
	  NDEEMCOR = 0 ;
      Nwork    = 0 ;
      NLIM_VOC = 0 ;
	  NLIM_EDE = 0 ;
	  NLIM_SSA = 0 ;
      NLIE     = 0 ;
      N_NOWEI  = 0 ;
      NDISABLE = 0 ;
      NDVEXEM  = 0 ;
	  NDCRD_PART = 0;
      HRS_WRK  = 0 ;
      OHRS_WRK = 0 ;
      THRS_WRK = 0 ;
      AF_WPS   = 0 ;
      AF_WEI   = 0 ;
	  AF_HEADH = 0 ;
	  AF_NCORHRS = 0 ;
      AFERRFG  = '  ';
      AF_DMHRS = 0 ;

      HWK_VOC  = 0;
      TWK_VOC  = 0;
      HRS_DEEM = 0;
      
	  TOTASST  = 0 ;
	  ZPAYCASE = 0 ;
	  CORDMVOC = 0 ;
	  DHRS_WRK = 0 ;
	  DTHS_WRK = 0 ;
	  VDTHS_WRK = 0 ;
	  DCOR_PART = 0 ;
%mend;


/*-------------------------------------------------*/
/*-   Initialize counters for two-parent family   -*/
/*-         work participation rates              -*/
/*-------------------------------------------------*/
%macro INIT_TP;

      TPSAMP   = 0;
      TPEXEMPT = 0;
      TPwork   = 0;
      TPLIM_VOC = 0;
	  TPLIMIT   = 0;
	  TPDCARD2 = 0;
      TPDCARD5 = 0;
      TPDEEM15 = 0;
      TPDEEM16 = 0;
	  TPDEEMCOR = 0;
      TPDVEXEM = 0;
      TPNCP    = 0;
      TPDABLE  = 0;
      TPERRFG  = ' ';
	  TPDCRD_PAR = 0 ;
      TPDEEMCOR = 0 ;
	  TPCORDMHRS = 0 ;
      P1HRS    = 0;
      OP1HRS   = 0;
      TP1HRS   = 0;
      P1_WPS   = 0;
	  P1_WEI   = 0;
      P2HRS    = 0;
      OP2HRS   = 0;
      TP2HRS   = 0;
      P2_WPS   = 0;
	  P2_WEI   = 0;
      TPHRS    = 0;
      TPOHRS   = 0;
      TPTHRS   = 0;

      TPHRS_V  = 0;
      TPTHRS_V = 0;
      TP_DMHRS = 0;

	  P1_SANC = 0 ;
	  P2_SANC = 0 ;
%mend;


/*-------------------------------------------------*/
/*-   Macro to set up variable for all family rate-*/
/*-------------------------------------------------*/
%macro CHECK_4_WEI;
   ISWEI = 0 ;
   IF (B_WEI >= 1 AND B_WEI < 6) THEN ISWEI = 1;
   IF (B_WEI = 6 AND B_FAMAFF < 3) THEN DO ;
       ISWEI = 1;
       AFERRFG = '10';
	   END ;
   IF (B_WEI = 7 AND B_CITZ = 1)   THEN DO ;
       ISWEI = 1 ;
       AFERRFG = '11' ;
	   END ;
   IF (B_WEI = 8 AND FIPS < 66 AND (B_RECSSI = 2 OR B_SSI = 0))    THEN DO ;
       ISWEI = 1 ;
       AFERRFG = '12' ;
	   END ;
      IF (B_WEI = 8 AND FIPS > 65 AND B_aidabd = 2)    THEN DO ;
       ISWEI = 1 ;
       AFERRFG = '12' ;
	   END ;
   IF (B_WEI = 11 AND (B_NCP = 2 OR B_FAMAFF = 1)) THEN DO;
       ISWEI = 1;
       AFERRFG = '14' ;
	END;    
%mend;


/*-------------------------------------------------*/
/*-   Macro to set up variable for all family rate-*/
/*-------------------------------------------------*/
%macro SETAF;
   NOW = (RPTM_YY * 10000) + (RPTM_MM * 100) + 01; 
   IF AF = 1 THEN DO;
      AF_FAMAFF = FAMAFF_1 ;
      AF_NCP   = NCP_1 ;
      AF_BIRTH = ABIRTH_1;
      AF_MARITAL = MARITAL_1 ;
      AF_HEADH = AHEADH_1;
      AF_PARENT = PARENT_1;
      AF_WPS   = WPS_1;
      AF_WEI   = WEI_1;
      AF_CITZ  = CITZ_1;
	  AF_AIDABD = AIDABD_1 ;
      AF_RECSSI = RECSSI_1 ;
	  AF_NCORHRS = NCORHRS_1 ;
	  AF_TPCORHRS = TPCORHRS_1;
      AF_SSI   = SSI_1;
      DO I = 1 TO 13;
         AFwork (I) = A1work (I);
	  END;
	  DO I = 1 TO 8;
         AFholi (I) = A1holi (I);
         AFexab (I) = A1exab (I);
      END;
   END;
   ELSE DO;
      IF AF = 2 THEN DO;
        AF_FAMAFF = FAMAFF_2 ;
        AF_NCP   = NCP_2 ;
        AF_BIRTH = ABIRTH_2;
        AF_MARITAL = MARITAL_2 ;
        AF_HEADH = AHEADH_2;
        AF_PARENT = PARENT_2;
        AF_WPS   = WPS_2;
		AF_WEI   = WEI_2;
        AF_CITZ  = CITZ_2;
		AF_AIDABD = AIDABD_2 ;
        AF_RECSSI = RECSSI_2 ;
        AF_NCORHRS = NCORHRS_2 ;
		AF_TPCORHRS = TPCORHRS_2;
        AF_SSI   = SSI_2;
        DO  I = 1 TO 13;
          AFwork (I) = A2work (I);
		END;
	    DO I = 1 TO 8;
          AFholi (I) = A2holi (I);
          AFexab (I) = A2exab (I);
        END;
      END;
      ELSE DO;
         IF AF = 3 THEN DO;
            AF_FAMAFF = FAMAFF_3 ;
            AF_NCP   = NCP_3 ;
            AF_BIRTH = ABIRTH_3;
            AF_MARITAL = MARITAL_3 ;
            AF_HEADH = AHEADH_3;
            AF_PARENT = PARENT_3;
            AF_WPS   = WPS_3;
			AF_WEI   = WEI_3;
            AF_CITZ  = CITZ_3;
			AF_AIDABD = AIDABD_3 ;
			AF_RECSSI = RECSSI_3 ;
            AF_NCORHRS = NCORHRS_3 ;
			AF_TPCORHRS = TPCORHRS_3;
            AF_SSI   = SSI_3;
            DO I = 1 TO 13;
               AFwork (I) = A3work (I);
			END;
	        DO I = 1 TO 8;
               AFholi (I) = A3holi (I);
               AFexab (I) = A3exab (I);
            END;
         END;
         ELSE DO;
            IF AF = 4 THEN DO;
               AF_FAMAFF = FAMAFF_4 ;
               AF_NCP   = NCP_4 ;
               AF_BIRTH = ABIRTH_4;
               AF_MARITAL = MARITAL_4 ;
               AF_HEADH = AHEADH_4;
               AF_PARENT = PARENT_4;
               AF_WPS   = WPS_4;
			   AF_WEI   = WEI_4;
               AF_CITZ  = CITZ_4;
			   AF_AIDABD = AIDABD_4 ;
			   AF_RECSSI = RECSSI_4 ;
               AF_NCORHRS = NCORHRS_4 ;
			   AF_TPCORHRS = TPCORHRS_4;
               AF_SSI   = SSI_4;
               DO I = 1 TO 13;
                 AFwork (I) = A4work (I);
			   END;
	           DO I = 1 TO 8;
				 AFholi (I) = A4holi (I);
                 AFexab (I) = A4exab (I);
               END;
            END;
            ELSE DO;
               IF AF = 5 THEN DO;
                  AF_FAMAFF = FAMAFF_5 ;
                  AF_NCP   = NCP_5 ;
                  AF_BIRTH = ABIRTH_5;
                  AF_MARITAL = MARITAL_5 ;
                  AF_HEADH = AHEADH_5;
                  AF_PARENT = PARENT_5;
                  AF_WPS   = WPS_5;
				  AF_WEI   = WEI_5;
                  AF_CITZ  = CITZ_5;
				  AF_AIDABD = AIDABD_5 ;
				  AF_RECSSI = RECSSI_5 ;
                  AF_NCORHRS = NCORHRS_5;
	 	          AF_TPCORHRS = TPCORHRS_5;
                  AF_SSI   = SSI_5;
                  DO I = 1 TO 13;
                     AFwork (I) = A5work (I);
				  END;
	              DO I = 1 TO 8;
					 AFholi (I) = A2holi (I);
                     AFexab (I) = A2exab (I);
                  END;
               END;
               ELSE DO;
                  IF AF = 6 THEN DO;
                     AF_FAMAFF = FAMAFF_6 ;
                     AF_NCP   = NCP_6 ;
                     AF_BIRTH = ABIRTH_6;
                     AF_MARITAL = MARITAL_6 ;
                     AF_HEADH = AHEADH_6;
                     AF_PARENT = PARENT_6;
                     AF_WPS   = WPS_6;
					 AF_WEI   = WEI_6;
                     AF_CITZ  = CITZ_6;
					 AF_AIDABD = AIDABD_6 ;
					 AF_RECSSI = RECSSI_6 ;
                     AF_NCORHRS = NCORHRS_6 ;
					 AF_TPCORHRS = TPCORHRS_6;
                     AF_SSI   = SSI_6;
                     DO I = 1 TO 13;
                        AFwork (I) = A6work (I);
	    			 END;
	                 DO I = 1 TO 8;
						AFholi (I) = A6holi (I);
                        AFexab (I) = A6exab (I);
                     END;
                  END;
               END;
            END;
         END;
      END;
   END;
%mend setaf;



%macro CHECK_4_FLSA_STATE;
   FLSA_STATE = 0 ;
   NOW = (RPTM_YY * 10000) + (RPTM_MM * 100) + 01; 

   IF (FIPS = 01 AND NOW >= 20060830) THEN FLSA_STATE = 1 ;
   IF (FIPS = 02 AND NOW >= 90061001) THEN FLSA_STATE = 1 ;
   IF (FIPS = 04 AND NOW >= 20081001) THEN FLSA_STATE = 1 ;
   IF (FIPS = 05 AND NOW >= 90080229) THEN FLSA_STATE = 1 ;
   IF (FIPS = 06 AND NOW >= 20000501) THEN FLSA_STATE = 1 ;
   IF (FIPS = 08 AND NOW >= 20060926) THEN FLSA_STATE = 1 ;
   IF (FIPS = 09 AND NOW >= 90061001) THEN FLSA_STATE = 1 ;
   IF (FIPS = 10 AND NOW >= 19971126) THEN FLSA_STATE = 1 ;
   IF (FIPS = 11 AND NOW >= 20060830) THEN FLSA_STATE = 1 ;
   IF (FIPS = 12 AND NOW >= 20060911) THEN FLSA_STATE = 1 ;
   IF (FIPS = 13 AND NOW >= 20070117) THEN FLSA_STATE = 1 ;
   IF (FIPS = 15 AND NOW >= 20060918) THEN FLSA_STATE = 1 ;
   IF (FIPS = 16 AND NOW >= 19971014) THEN FLSA_STATE = 1 ;
   IF (FIPS = 17 AND NOW >= 20070227) THEN FLSA_STATE = 1 ;
   IF (FIPS = 18 AND NOW >= 20081001) THEN FLSA_STATE = 1 ;
   IF (FIPS = 19 AND NOW >= 20060901) THEN FLSA_STATE = 1 ;
   IF (FIPS = 20 AND NOW >= 20060919) THEN FLSA_STATE = 1 ;
   IF (FIPS = 21 AND NOW >= 20060905) THEN FLSA_STATE = 1 ;
   IF (FIPS = 22 AND NOW >= 20060921) THEN FLSA_STATE = 1 ;
   IF (FIPS = 23 AND NOW >= 19980505) THEN FLSA_STATE = 1 ;
   IF (FIPS = 24 AND NOW >= 20060830) THEN FLSA_STATE = 1 ;
   IF (FIPS = 25 AND NOW >= 20060918) THEN FLSA_STATE = 1 ;
   IF (FIPS = 26 AND NOW >= 90060911) THEN FLSA_STATE = 1 ;
   IF (FIPS = 27 AND NOW >= 20060901) THEN FLSA_STATE = 1 ;
   IF (FIPS = 28 AND NOW >= 20060727) THEN FLSA_STATE = 1 ;
   IF (FIPS = 29 AND NOW >= 20060926) THEN FLSA_STATE = 1 ;
   IF (FIPS = 30 AND NOW >= 90061001) THEN FLSA_STATE = 1 ;
   IF (FIPS = 31 AND NOW >= 20060914) THEN FLSA_STATE = 1 ;
   IF (FIPS = 32 AND NOW >= 20060807) THEN FLSA_STATE = 1 ;
   IF (FIPS = 33 AND NOW >= 20060919) THEN FLSA_STATE = 1 ;
   IF (FIPS = 34 AND NOW >= 19971008) THEN FLSA_STATE = 1 ;
   IF (FIPS = 35 AND NOW >= 20060913) THEN FLSA_STATE = 1 ;
   IF (FIPS = 36 AND NOW >= 19971027) THEN FLSA_STATE = 1 ;
   IF (FIPS = 37 AND NOW >= 20060801) THEN FLSA_STATE = 1 ;
   IF (FIPS = 38 AND NOW >= 20060929) THEN FLSA_STATE = 1 ;
   IF (FIPS = 39 AND NOW >= 20060913) THEN FLSA_STATE = 1 ;
   IF (FIPS = 40 AND NOW >= 90061001) THEN FLSA_STATE = 1 ;
   IF (FIPS = 41 AND NOW >= 20060801) THEN FLSA_STATE = 1 ;
   IF (FIPS = 42 AND NOW >= 20060927) THEN FLSA_STATE = 1 ;
   IF (FIPS = 44 AND NOW >= 20070401) THEN FLSA_STATE = 1 ;
   IF (FIPS = 45 AND NOW >= 20060911) THEN FLSA_STATE = 1 ;
   IF (FIPS = 46 AND NOW >= 90061001) THEN FLSA_STATE = 1 ;
   IF (FIPS = 47 AND NOW >= 20060913) THEN FLSA_STATE = 1 ;
   IF (FIPS = 48 AND NOW >= 20061025) THEN FLSA_STATE = 1 ;
   IF (FIPS = 49 AND NOW >= 20070401) THEN FLSA_STATE = 1 ;
   IF (FIPS = 50 AND NOW >= 20081001) THEN FLSA_STATE = 1 ;
   IF (FIPS = 51 AND NOW >= 20081001) THEN FLSA_STATE = 1 ;
   IF (FIPS = 53 AND NOW >= 20061019) THEN FLSA_STATE = 1 ;
   IF (FIPS = 54 AND NOW >= 20060906) THEN FLSA_STATE = 1 ;
   IF (FIPS = 55 AND NOW >= 90061001) THEN FLSA_STATE = 1 ;
   IF (FIPS = 56 AND NOW >= 90061001) THEN FLSA_STATE = 1 ;
   IF (FIPS = 66 AND NOW >= 90061001) THEN FLSA_STATE = 1 ;
   IF (FIPS = 72 AND NOW >= 90061001) THEN FLSA_STATE = 1 ;
   IF (FIPS = 78 AND NOW >= 20070401) THEN FLSA_STATE = 1 ;
   /** IF NOT A FLSA STATE, THEN COMMENT OUT  **/
   /**  CHANGE DATE FOR APPROVAL DATE FPR EACH FLSA_STATE  **/
%mend CHECK_4_FLSA_STATE;


%macro get_min_wage_rate;
 if fips = 01 then min_wages = 7.25  ;
 if fips = 02 then min_wages = 7.25  ;
 if fips = 04 then min_wages = 7.25  ;
 if fips = 05 then min_wages = 7.25  ;
 if fips = 06 then min_wages = 8.00  ;
 if fips = 08 then min_wages = 7.28  ;
 if fips = 09 then min_wages = 8.00  ;
 if fips = 10 then min_wages = 7.25  ;
 if fips = 11 then min_wages = 7.55  ;
 if fips = 12 then min_wages = 7.25  ;
 if fips = 13 then min_wages = 7.25  ;
 if fips = 15 then min_wages = 7.25  ;
 if fips = 16 then min_wages = 7.25  ;
 if fips = 17 then min_wages = 8.00  ;
 if fips = 18 then min_wages = 7.25  ;
 if fips = 19 then min_wages = 7.25  ;
 if fips = 20 then min_wages = 7.25  ;
 if fips = 21 then min_wages = 7.25  ;
 if fips = 22 then min_wages = 7.25  ;
 if fips = 23 then min_wages = 7.50  ;
 if fips = 24 then min_wages = 7.25  ;
 if fips = 25 then min_wages = 8.00  ;
 if fips = 26 then min_wages = 7.40  ;
 if fips = 27 then min_wages = 7.25  ;
 if fips = 28 then min_wages = 7.25  ;
 if fips = 29 then min_wages = 7.25  ;
 if fips = 30 then min_wages = 7.25  ;
 if fips = 31 then min_wages = 7.25  ;
 if fips = 32 then min_wages = 7.25  ;
 if fips = 33 then min_wages = 7.25  ;
 if fips = 34 then min_wages = 7.25  ;
 if fips = 35 then min_wages = 7.25  ;
 if fips = 36 then min_wages = 7.25  ;
 if fips = 37 then min_wages = 7.25  ;
 if fips = 38 then min_wages = 7.25  ;
 if fips = 39 then min_wages = 7.25  ;
 if fips = 40 then min_wages = 7.25  ;
 if fips = 41 then min_wages = 8.40  ;
 if fips = 42 then min_wages = 7.25  ;
 if fips = 44 then min_wages = 7.40  ;
 if fips = 45 then min_wages = 7.25  ;
 if fips = 46 then min_wages = 7.25  ;
 if fips = 47 then min_wages = 7.25  ;
 if fips = 48 then min_wages = 7.25  ;
 if fips = 49 then min_wages = 7.25  ;
 if fips = 50 then min_wages = 8.25  ;
 if fips = 51 then min_wages = 7.25  ;
 if fips = 53 then min_wages = 7.25  ;
 if fips = 54 then min_wages = 7.25  ;
 if fips = 55 then min_wages = 7.25  ;
 if fips = 56 then min_wages = 7.25  ;
 if fips = 66 then min_wages = 7.25  ;
 if fips = 72 then min_wages = 7.25  ;
 if fips = 78 then min_wages = 7.25  ;

%mend;


%macro check_4_enough_cswehrs;
  enough_cswehrs = 0 ;
  %get_min_wage_rate;
  flsa_amt = TOTASST + fs_amt - cs_amt;
  Tot_flsa_hrs = flsa_amt / min_wages ;
  avg_flsa_hrs = tot_flsa_hrs / 5 ;
  avg_flsa_hrs = INT (avg_flsa_hrs) ;
  if cswe_hrs >= avg_flsa_hrs then enough_cswehrs = 1 ; 

%mend;

/*-------------------------------------------------*/
/*-   Macro to set counters for all family rates  -*/
/*-------------------------------------------------*/
%macro AFCOUNTS;
   IF AF_WPS = 1 THEN DO;
      /* CHECK FOR CHILD UNDER 12 MOS */
      UNDER12M = 0;
      NOW = (RPTM_YY * 10000) + (RPTM_MM * 100) + 01;
      IF RPTM_MM = 12 THEN DO;
         YY = RPTM_YY + 1;
         MM = 01;
      END;
      ELSE DO;
         YY = RPTM_YY;
         MM = RPTM_MM + 1;
      END;
      NEXTMN  = (YY * 10000) + (MM * 100) + 01;
      PAST12M = NOW - 10000;
      IF ((BIRTH1  <= NEXTMN AND BIRTH1  > PAST12M) OR
          (BIRTH2  <= NEXTMN AND BIRTH2  > PAST12M) OR
          (BIRTH3  <= NEXTMN AND BIRTH3  > PAST12M) OR
          (BIRTH4  <= NEXTMN AND BIRTH4  > PAST12M) OR
          (BIRTH5  <= NEXTMN AND BIRTH5  > PAST12M) OR
          (BIRTH6  <= NEXTMN AND BIRTH6  > PAST12M) OR
          (BIRTH7  <= NEXTMN AND BIRTH7  > PAST12M) OR
          (BIRTH8  <= NEXTMN AND BIRTH8  > PAST12M) OR
          (BIRTH9  <= NEXTMN AND BIRTH9  > PAST12M) OR
          (BIRTH10 <= NEXTMN AND BIRTH10 > PAST12M)) THEN DO;
         UNDER12M = 1;
         NDCARD1 = 1;
      END;
      IF (AF_WPS = 1 AND UNDER12M = 0) THEN DO ;
         /* NO CHILD UNDER 1 YEAR IN AGE */

         AFERRFG = '03' ;
      END;
   END;

IF AF_WPS = 2 THEN DO;
     IF ((AF_FAMAFF = 1 AND work_SANC = '1' AND SANC_AMT > 0) OR 
         (AF_FAMAFF = 2 AND work_SANC = '1' AND SANC_AMT > 0 AND 
          AF_WEI = 2))  THEN NDCARD2 = 1 ;
     ELSE DO;
        AFERRFG = '13' ;
     END;
END;

IF AF_WPS = 5 THEN DO;
   NDCARD5 = 1;
END;

ISPARTI = 0;

HRS_WRK  = 0;
THRS_WRK = 0;
OHRS_WRK = 0;
DO I = 1 TO 8;
   HRS_WRK = HRS_WRK + AFwork (I);
END;
DO I = 1 TO 4;
   HRS_WRK = HRS_WRK + AFholi (I) + AFexab (I);
END;
HRS_WRK  = HRS_WRK + AFwork (12) + AFholi (8) + AFexab (8);
OHRS_WRK = AFwork (9) + AFwork (10) + AFwork (11) +
           AFholi (5) + AFholi (6)  + AFholi (7) +
           AFexab (5) + AFexab (6)  + AFexab (7) ;

THRS_WRK = HRS_WRK + OHRS_WRK;

 CSWE_HRS = AFwork (4) + AFwork (7) +
            AFholi (1) + AFholi (3) +
            AFexab (1) + AFexab (3) ;

REQ_HRS = 30;


IF (HRS_WRK >= 20 AND THRS_WRK >= REQ_HRS) THEN DO;
   ISPARTI = 1 ;
   Nwork = 1 ;
   NDCRD_PART = 0 ;
   IF (NDCARD1 = 1 OR NDCARD2 = 1 OR NDCARD5 = 1) THEN 
       NDCRD_PART = 1;
	   END;

IF ISPARTI = 1 THEN DO;
   HWK_VOC  = 0;
   TWK_VOC  = 0;

   HWK_VOC = HRS_WRK  - AFwork (8) - AFholi (4) - AFexab (4);
   TWK_VOC = THRS_WRK - AFwork (8) - AFholi (4) - AFexab (4);

   IF (HWK_VOC < 20) OR (TWK_VOC < REQ_HRS) THEN NLIM_VOC = 1;
END;

IF ISPARTI = 0 THEN DO;
   %CHECK_4_FLSA_STATE;
   IF (AF_NCORHRS > 0 AND FLSA_STATE = 0) THEN AFERRFG = '15' ;
   IF ((CSWE_HRS = 0 AND AF_NCORHRS > 0) OR
       (AF_NCORHRS >= 20) OR
       (AF_NCORHRS > 0 AND(CSWE_HRS + AF_NCORHRS > 20)))THEN AFERRFG = '16' ;
   %check_4_enough_cswehrs ;
   if (AF_NCORHRS > 0 AND FLSA_STATE = 1 AND enough_cswehrs = 0) then aferrfg = '17' ;
   IF ((AF_NCORHRS > 0 AND AF_NCORHRS < 20) AND (CSWE_HRS + AF_NCORHRS = 20) AND 
        enough_cswehrs = 1 AND FLSA_STATE = 1) THEN DO;
      NDEEMCOR = 1 ;
      DCOR_PART = 0;
      DHRS_WRK = CSWE_HRS + AF_NCORHRS ;
	  DTHS_WRK = THRS_WRK + AF_NCORHRS ;
      IF (DHRS_WRK >= 20 AND DTHS_WRK >= REQ_HRS) THEN DO;
         ISPARTI = 1 ;
		 Nwork = 1 ;
		 DCOR_PAR = 1;
		 NDCRD_PART = 0 ;
		 IF (NDCARD1 = 1 OR NDCARD2 = 1 OR NDCARD5 = 1) THEN NDCRD_PART = 1;
		 END ;
   END;

   iF DCOR_PART = 1 THEN DO;
     DTWH_VOC = 0 ;
     DTWK_VOC = DTHS_WRK - AFwork (8)- AFholi (4) - AFexab (4) ;
    IF DTWK_VOC < REQ_HRS THEN NLIM_VOC = 1;
   END;
   END;

   IF NLIM_VOC = 1 THEN DO;
     IF NDCARD1 = 1THEN DO;
	    Nwork = 0;
		NDCARD2 = 0;
		NDCARD5 = 0;
/**     NLIE     = 0;  **/
        N_NOWEI = 0;
        NDVEXEM  = 0;
	 END;
     IF NDCARD2 = 1THEN DO;
	    Nwork = 0;
		NDCARD1 = 0;
		NDCARD5 = 0;
/**     NLIE     = 0;  **/
        N_NOWEI = 0;
        NDVEXEM  = 0;
	 END;
     IF NDCARD5 = 1THEN DO;
	    Nwork = 0;
		NDCARD1 = 0;
		NDCARD2 = 0;
/**     NLIE     = 0;  **/
        N_NOWEI = 0;
        NDVEXEM  = 0;
	 END;
   END;
   IF (NLIM_VOC = 1 AND NDCARD1 NE 1 AND NDCARD2 NE 1 AND NDCARD5 NE 1)THEN DO;
     Nwork    = 1;
     NDCARD1  = 0;
     NDCARD2  = 0;
     NDCARD5  = 0;
/**  NLIE     = 0;  **/
     N_NOWEI = 0;
     NDVEXEM  = 0;
   END;

   IF (Nwork = 1 AND NLIM_VOC = 0) THEN DO;	
      NDCARD1 = 0;
	  NDCARD2 = 0;
	  NDCARD5 = 0;
	  END;

/***  IF ISPARTI = 0 THEN DO; ***/
   IF (AF_WPS >= 6 AND AF_WPS <= 11) THEN NEXEMPT = 1;


   IF (AF_WPS = 15 OR AF_WPS = 16) THEN DO;
      ISTEEN = 0;
	  SATSCH = 0;
	  EDUDIR = 0;
      NOW     = (RPTM_YY * 10000) + (RPTM_MM * 100) + 01;
      PAST20Y = NOW - 200000;
      IF (AF_BIRTH < NOW AND AF_BIRTH > PAST20Y) THEN DO;
         ISTEEN = 1;

         IF AF_WPS = 15 THEN DO ;
			SATSCH = AFwork (11) + AFholi (7) + AFexab (7) ;
            IF (ISTEEN = 1 AND (AF_HEADH = 01 OR AF_HEADH = 02) AND
                SATSCH > 0) THEN DO;
                  Nwork = 1;
                  NDEEM15 = 1;
                  NLIM_SSA = 1;
            END;
            ELSE DO;
               /* ERROR: WPS = 15      */
               AFERRFG = '04';
            END;
         END ;

         IF AF_WPS = 16 THEN DO ;
		    EDUDIR = AFwork (10) + AFholi (6) + AFexab (6) ;
            IF (ISTEEN = 1 AND (AF_HEADH = 01 OR AF_HEADH = 02) AND
                EDUDIR >=  20)
                THEN DO;
                  Nwork = 1;
                  NDEEM16 = 1;
                  NLIM_EDE = 1;
                END;
            ELSE DO;
               /* ERROR: WPS = 16 - BUT NOT 20 HOURS OF PARTICIPATION */
               AFERRFG = '05';
            END;
         END ;

    END;
   END;

   IF AF_WPS = 17 THEN DO;
      /* CALCULATE AGE OF YOUNGEST CHILD TO DETERMINE IF UNDER 6 */
      UNDERSIX = 0;
	  NLIM_VOC = 0 ;
      NOW = (RPTM_YY * 10000) + (RPTM_MM * 100) + 01;
      SIXPAST = NOW - 60000;
      IF RPTM_MM = 12 THEN DO;
         YY = RPTM_YY + 1;
         MM = 01;
      END;
      IF RPTM_MM NE 12 THEN DO;
         YY = RPTM_YY;
         MM = RPTM_MM + 1;
      END;
      NEXTMN  = (YY * 10000) + (MM * 100) + 01;
      IF ((BIRTH1  <= NEXTMN AND BIRTH1  > SIXPAST) OR
          (BIRTH2  <= NEXTMN AND BIRTH2  > SIXPAST) OR
          (BIRTH3  <= NEXTMN AND BIRTH3  > SIXPAST) OR
          (BIRTH4  <= NEXTMN AND BIRTH4  > SIXPAST) OR
          (BIRTH5  <= NEXTMN AND BIRTH5  > SIXPAST) OR
          (BIRTH6  <= NEXTMN AND BIRTH6  > SIXPAST) OR
          (BIRTH7  <= NEXTMN AND BIRTH7  > SIXPAST) OR
          (BIRTH8  <= NEXTMN AND BIRTH8  > SIXPAST) OR
          (BIRTH9  <= NEXTMN AND BIRTH9  > SIXPAST) OR
          (BIRTH10 <= NEXTMN AND BIRTH10 > SIXPAST)) THEN DO ;
             UNDERSIX = 1;
      END ;
      IF UNDERSIX NE 1 THEN DO;
         /* ERROR: NO CHILD UNDER SIX */
         AFERRFG = '06' ;
      END;

      IF UNDERSIX = 1 THEN DO;
        IF HRS_WRK >= 20 THEN DO;
	      ISPARTI = 1;
		  NDEEM17 = 1;
          Nwork = 1;
          HWK_VOC = HRS_WRK - AFwork (8) - AFholi (4) - AFexab (4) ;
          IF HWK_VOC < 20 THEN NLIM_VOC = 1;
	    END;
          
        IF HRS_WRK < 20 THEN DO;
          IF (NDEEMCOR = 1 and enough_cswehrs = 1) THEN DO;
           IF DHRS_WRK >= 20 THEN DO;
              ISPARTI = 1;
			  Nwork = 1 ;
			  DCOR_PART = 1;
			  IF ISPARTI = 1 THEN DO;
			    DHRS_VOC = DHRS_WRK - AFwork (8) - AFholi (4) - AFexab (4) ;
			    IF DHRS_VOC < 20 THEN NLIM_VOC = 1 ;
			  END;
			END;
        END;
	    ELSE DO;
		    IF (AFERRFG = '  ' OR AFERRFG = '00') THEN AFERRFG = '07' ;
            /* CHILD UNDER SIX - BUT NOT 20 HOURS OF PARTICIPATION */
        END;
	  END;
	END;
   END ;
   IF (AF_WPS = 9 AND Nwork = 0) THEN NDVEXEM = 1 ;
   IF (AF_WPS = 99 AND (AF_NCP = 2 AND AF_WEI < 6)) THEN AFERRFG = '08';
/*** END;  ***/

%mend afcounts;


/*-------------------------------------------------*/
/*-   Macro to deterime if two-parent family      -*/
/*-------------------------------------------------*/
%macro DETFI2P;
/* TO DETERMINE IF THE FAMILY IS 2-PARENT FOR work RATES */
NUM_PAR = 0 ;
NUM_NCP = 0 ;
IF ((FAMAFF_1 = 1 OR FAMAFF_1 = 2) AND PARENT_1 = 1) THEN DO;
  IF (((WEI_1 >= 1 AND WEI_1 < 6) AND NCP_1 = 1 AND FAMAFF_1 = 1) OR
      ((WEI_1 >= 1 AND WEI_1 < 6) AND NCP_1 = 2)) THEN DO; 
    NUM_PAR+1;
    ISPARNT(NUM_PAR) = 1 ;
    /* IDENTIFY NCPs THAT ARE NOT USED IN TWO-PARENT RATE */
    IF (NCP_1 = 1 AND WPS_1 = 99) THEN DO ;
      NUM_NCP+1;
      ISNCP(NUM_NCP) = 1 ;
    END;
  END;
END;

IF ((FAMAFF_2 = 1 OR FAMAFF_2 = 2) AND PARENT_2 = 1)  THEN DO ;
  IF (((WEI_2 >= 1 AND WEI_2 < 6) AND NCP_2 = 1 AND FAMAFF_2 = 1) OR
      ((WEI_2 >= 1 AND WEI_2 < 6) AND NCP_2 = 2)) THEN DO; 
    NUM_PAR+1;
    ISPARNT(NUM_PAR) = 2 ;
    IF (NCP_2 = 1 AND WPS_2 = 99) THEN DO ;
      NUM_NCP+1;
      ISNCP(NUM_NCP) = 2 ;
    END;
  END;
END;

IF ((FAMAFF_3 = 1 OR FAMAFF_3 = 2) AND PARENT_3 = 1)  THEN DO ;
  IF (((WEI_3 >= 1 AND WEI_3 < 6) AND NCP_3 = 1 AND FAMAFF_3 = 1) OR
      ((WEI_3 >= 1 AND WEI_3 < 6) AND NCP_3 = 2)) THEN DO; 
    NUM_PAR+1;
    ISPARNT(NUM_PAR) = 3 ;
    IF (NCP_3 = 1 AND WPS_3 = 99) THEN DO ;
      NUM_NCP+1;
      ISNCP(NUM_NCP) = 3 ;
    END;
  END;
END;

IF ((FAMAFF_4 = 1 OR FAMAFF_4 = 2) AND PARENT_4 = 1)  THEN DO ;
  IF (((WEI_4 >= 1 AND WEI_4 < 6) AND NCP_4 = 1 AND FAMAFF_4 = 1) OR
      ((WEI_4 >= 1 AND WEI_4 < 6) AND NCP_4 = 2)) THEN DO; 
    NUM_PAR+1;
    ISPARNT(NUM_PAR) = 4 ;
    IF (NCP_4 = 1 AND WPS_4 = 99) THEN DO ;
      NUM_NCP+1;
      ISNCP(NUM_NCP) = 4 ;
    END;
  END;
END;

IF ((FAMAFF_5 = 1 OR FAMAFF_5 = 2) AND PARENT_5 = 1)  THEN DO ;
  IF (((WEI_5 >= 1 AND WEI_5 < 6) AND NCP_5 = 1 AND FAMAFF_5 = 1) OR
      ((WEI_5 >= 1 AND WEI_5 < 6) AND NCP_5 = 2)) THEN DO; 
    NUM_PAR+1;
    ISPARNT(NUM_PAR) = 5 ;
    IF (NCP_5 = 1 AND WPS_5 = 99) THEN DO ;
      NUM_NCP+1;
      ISNCP(NUM_NCP) = 5 ;
    END;
  END;
END;

IF ((FAMAFF_6 = 1 OR FAMAFF_6 = 2) AND PARENT_6 = 1)  THEN DO ;
  IF (((WEI_6 >= 1 AND WEI_6 < 6) AND NCP_6 = 1 AND FAMAFF_6 = 1) OR
      ((WEI_6 >= 1 AND WEI_6 < 6) AND NCP_6 = 2)) THEN DO; 
    NUM_PAR+1;
    ISPARNT(NUM_PAR) = 6 ;
    IF (NCP_6 = 1 AND WPS_6 = 99) THEN DO ;
      NUM_NCP+1;
      ISNCP(NUM_NCP) = 6 ;
    END;
  END;
END;

IF NUM_PAR >= 2 THEN DO;
   IS2P = 1 ;
   P1 = 0;
   P2 = 0;

   IF NUM_PAR = 2 THEN DO;
      P1 = ISPARNT1 ;
      P2 = ISPARNT2 ;
      END ;

   IF (NUM_PAR = 3 AND NUM_NCP = 1) THEN DO;
      IF ISNCP1 = 1 THEN DO;
         P1 = ISPARNT2 ;
         P2 = ISPARNT3 ;
      END ;

      IF ISNCP1 = 2 THEN DO;
         P1 = ISPARNT1 ;
         P2 = ISPARNT3 ;
      END ;

      IF ISNCP1 = 3 THEN DO;
         P1 = ISPARNT1 ;
         P2 = ISPARNT2 ;
      END ;
   END ;

   IF (NUM_PAR >  2 AND NUM_NCP NE 1) THEN DO;
         P1 = ISPARNT1 ;
         P2 = ISPARNT2 ;
         TPERRFG = '3' ;
   END ;

   END;

   IF NUM_PAR <  2  THEN DO;
         TPERRFG = '5' ;
   END ;

%mend detfi2p;


/*-------------------------------------------------*/
/*-   Macro ;to set up two parents variable       -*/
/*-------------------------------------------------*/
%macro SET2P;
DO2P = 1;
NOW = (RPTM_YY * 10000) + (RPTM_MM * 100) + 01; 

IF P1 = 1 THEN DO;
   P1_FAMAFF = FAMAFF_1 ;
   P1_NCP   = NCP_1 ;
   P1_BIRTH = ABIRTH_1;
   P1_MARITAL = MARITAL_1 ;
   P1_HEADH = AHEADH_1;
   P1_PARNT = PARENT_1;
   P1_WEI   = WEI_1;
   P1_CITZ  = CITZ_1;
   P1_SSI   = SSI_1;
   P1_RECSSI = RECSSI_1 ; 
   P1_WPS   = WPS_1;
   DO I = 1 TO 13;
      P1work (I) = A1work (I);
   END;
   DO I = 1 TO 8;
	  P1holi (I) = A1holi (I);
	  P1exab (I) = A1exab (I);
   END;
  P1_CORHRS = TPCORHRS_1 ;
END;
ELSE DO;
   IF P1 = 2 THEN DO;
      P1_FAMAFF = FAMAFF_2 ;
      P1_NCP   = NCP_2 ;
      P1_BIRTH = ABIRTH_2;
      P1_MARITAL = MARITAL_2 ;
      P1_HEADH = AHEADH_2;
      P1_PARNT = PARENT_2;
      P1_WEI   = WEI_2;
      P1_CITZ  = CITZ_2;
      P1_SSI   = SSI_2;
      P1_RECSSI = RECSSI_2 ; 
      P1_WPS   = WPS_2;
      DO I = 1 TO 13;
         P1work (I) = A2work (I);
	  END;
      DO I = 1 TO 8;
		 P1holi (I) = A2holi (I);
	     P1exab (I) = A2exab (I);
      END;
      P1_CORHRS = TPCORHRS_2 ;
   END;
   ELSE DO;
      IF P1 = 3 THEN DO;
         P1_FAMAFF = FAMAFF_3 ;
         P1_NCP   = NCP_3 ;
         P1_BIRTH = ABIRTH_3;
         P1_MARITAL = MARITAL_3 ;
         P1_HEADH = AHEADH_3;
         P1_PARNT = PARENT_3;
         P1_WEI   = WEI_3;
         P1_CITZ  = CITZ_3;
         P1_SSI   = SSI_3;
         P1_RECSSI = RECSSI_3 ; 
          P1_WPS   = WPS_3;
         DO I = 1 TO 13;
            P1work (I) = A3work (I);
		 END;
         DO I = 1 TO 8;
			P1holi (I) = A3holi (I);
	        P1exab (I) = A3exab (I);
         END;
         P1_CORHRS = TPCORHRS_3 ;
      END;
      ELSE DO;
         IF P1 = 4 THEN DO;
            P1_FAMAFF = FAMAFF_4 ;
            P1_NCP   = NCP_4 ;
            P1_BIRTH = ABIRTH_4;
            P1_MARITAL = MARITAL_4 ;
            P1_HEADH = AHEADH_4;
            P1_PARNT = PARENT_4;
            P1_WEI   = WEI_4;
            P1_CITZ  = CITZ_4;
            P1_SSI   = SSI_4;
            P1_RECSSI = RECSSI_4 ; 
            P1_WPS   = WPS_4;
            DO I = 1 TO 13;
               P1work (I) = A4work (I);
			END;
            DO I = 1 TO 8;
			   P1holi (I) = A4holi (I);
	           P1exab (I) = A4exab (I);
            END;
			P1_CORHRS = TPCORHRS_4 ;
         END;
         ELSE DO;
            IF P1 = 5 THEN DO;
               P1_FAMAFF = FAMAFF_5 ;
               P1_NCP   = NCP_5 ;
               P1_BIRTH = ABIRTH_5;
               P1_MARITAL = MARITAL_5 ;
               P1_HEADH = AHEADH_5;
               P1_PARNT = PARENT_5;
               P1_WEI   = WEI_5;
               P1_CITZ  = CITZ_5;
               P1_SSI   = SSI_5;
               P1_RECSSI = RECSSI_5 ; 
               P1_WPS   = WPS_5;
               DO I = 1 TO 13;
                  P1work (I) = A5work (I);
			   END;
               DO I = 1 TO 8;
				  P1holi (I) = A5holi (I);
	              P1exab (I) = A5exab (I);
               END;
               P1_CORHRS = TPCORHRS_5 ;
            END;
            ELSE DO;
               IF P1 = 6 THEN DO;
                  P1_FAMAFF = FAMAFF_6 ;
                  P1_NCP   = NCP_6 ;
                  P1_BIRTH = ABIRTH_6;
                  P1_MARITAL = MARITAL_6 ;
                  P1_HEADH = AHEADH_6;
                  P1_PARNT = PARENT_6;
                 P1_WEI   = WEI_6;
                 P1_CITZ  = CITZ_6;
                 P1_SSI   = SSI_6;
                 P1_RECSSI = RECSSI_6;  
                 P1_WPS   = WPS_6;
                 DO I = 1 TO 13;
                    P1work (I) = A6work (I);
 				 END;
				 DO I = 1 TO 8 ;
					P1holi (I) = A6holi (I);
	                P1exab (I) = A6exab (I);
                 END;
                 P1_CORHRS = TPCORHRS_6 ;
                END;
            END;
         END;
      END;
   END;
END;


IF P2 = 1 THEN DO;
   P2_FAMAFF = FAMAFF_1 ;
   P2_NCP   = NCP_1 ;
   P2_BIRTH = ABIRTH_1;
   P2_MARITAL = MARITAL_1 ;
   P2_HEADH = AHEADH_1;
   P2_PARNT = PARENT_1;
   P2_WEI   = WEI_1;
   P2_CITZ  = CITZ_1;
   P2_SSI   = SSI_1;
   P2_RECSSI = RECSSI_1;  
   P2_WPS   = WPS_1;
   DO I = 1 TO 13;
      P2work (I) = A1work (I);
   END;
   DO I = 1 TO 8;
	  P2holi (I) = A1holi (I);
	  P2exab (I) = A1exab (I);
   END;
  P2_CORHRS = TPCORHRS_1 ;
END;
ELSE DO;
   IF P2 = 2 THEN DO;
      P2_FAMAFF = FAMAFF_2 ;
      P2_NCP   = NCP_2 ;
      P2_BIRTH = ABIRTH_2;
      P2_MARITAL = MARITAL_2 ;
      P2_HEADH = AHEADH_2;
      P2_PARNT = PARENT_2;
      P2_WEI   = WEI_2;
      P2_CITZ  = CITZ_2;
      P2_SSI   = SSI_2;
      P2_RECSSI = RECSSI_2; 
      P2_WPS   = WPS_2;
      DO I = 1 TO 13;
         P2work (I) = A2work (I);
	  END;
      DO I = 1 TO 8;
		 P2holi (I) = A2holi (I);
	     P2exab (I) = A2exab (I);
      END;
	  P2_CORHRS = TPCORHRS_2 ;
   END;
   ELSE DO;
      IF P2 = 3 THEN DO;
         P2_FAMAFF = FAMAFF_3 ;
         P2_NCP   = NCP_3 ;
         P2_BIRTH = ABIRTH_3;
         P2_MARITAL = MARITAL_3 ;
         P2_HEADH = AHEADH_3;
         P2_PARNT = PARENT_3;
         P2_WEI   = WEI_3;
         P2_CITZ  = CITZ_3;
         P2_SSI   = SSI_3;
         P2_RECSSI = RECSSI_3;  
         P2_WPS   = WPS_3;
         DO I = 1 TO 13;
            P2work (I) = A3work (I);
		 END;
         DO I = 1 TO 8;
			P2holi (I) = A3holi (I);
	        P2exab (I) = A3exab (I);
         END;
		 P2_CORHRS = TPCORHRS_3 ;
      END;
      ELSE DO;
         IF P2 = 4 THEN DO;
            P2_FAMAFF = FAMAFF_4 ;
            P2_NCP   = NCP_4 ;
            P2_BIRTH = ABIRTH_4;
            P2_MARITAL = MARITAL_4 ;
            P2_HEADH = AHEADH_4;
            P2_PARNT = PARENT_4;
            P2_WEI   = WEI_4;
            P2_CITZ  = CITZ_4;
            P2_SSI   = SSI_4;
            P2_RECSSI = RECSSI_4;  
            P2_WPS   = WPS_4;
            DO I = 1 TO 13;
               P2work (I) = A4work (I);
			END;
            DO I = 1 TO 8;
			   P2holi (I) = A4holi (I);
	           P2exab (I) = A4exab (I);
            END;
			P2_CORHRS = TPCORHRS_4 ;
         END;
         ELSE DO;
            IF P2 = 5 THEN DO;
               P2_FAMAFF = FAMAFF_5 ;
               P2_NCP   = NCP_5 ;
               P2_BIRTH = ABIRTH_5;
               P2_MARITAL = MARITAL_5 ;
               P2_HEADH = AHEADH_5;
               P2_PARNT = PARENT_5;
               P2_WEI   = WEI_5;
               P2_CITZ  = CITZ_5;
               P2_SSI   = SSI_5;
               P2_RECSSI = RECSSI_5;  
               P2_WPS   = WPS_5;
               DO I = 1 TO 13;
                  P2work (I) = A5work (I);
			   END;
               DO I = 1 TO 8;
				  P2holi (I) = A5holi (I);
	              P2exab (I) = A5exab (I);
               END;
			   P2_CORHRS = TPCORHRS_5 ;
            END;
            ELSE DO;
               IF P2 = 6 THEN DO;
                  P2_FAMAFF = FAMAFF_6 ;
                  P2_NCP   = NCP_6 ;
                  P2_BIRTH = ABIRTH_6;
                  P2_MARITAL = MARITAL_6 ;
                  P2_HEADH = AHEADH_6;
                  P2_PARNT = PARENT_6;
                  P2_WEI   = WEI_6;
                  P2_CITZ  = CITZ_6;
                  P2_SSI   = SSI_6; 
				  P2_RECSSI = RECSSI_6;  
                  P2_WPS   = WPS_6;
                  DO I = 1 TO 13;
                     P2work (I) = A6work (I);
				  END;
                  DO I = 1 TO 8;
					 P2holi (I) = A6holi (I);
	                 P2exab (I) = A6exab (I);
                  END;
  			      P2_CORHRS = TPCORHRS_6 ;
               END;
            END;
         END;
      END;
   END;
END;

IF (P1_NCP = 1 AND P1_WPS = 99) OR
   (P2_NCP = 1 AND P2_WPS = 99) THEN DO ;
      DO2P = 0 ;
      /* FAMILY NOT USED IN TWO-PARENT CALCULATIONS */
      TPNCP = 1 ;
      END ;

IF (TPNCP NE 1 AND (P1_WPS =  7 OR  P2_WPS =  7)) THEN DO ;
      DO2P = 0 ;
      TPDABLE = 1;
   END ;

IF (TPNCP NE 1 AND (P1_WPS NE 7 AND P2_WPS NE 7)) THEN DO2P = 1;

%mend SET2P;


/*-------------------------------------------------*/
/*-   Macro to calculate hours for two parents    -*/
/*-------------------------------------------------*/
%macro CHECKHRS;
TPISPART  = 0;
REQ_HRS = 30;

P1HRS  = 0;
TP1HRS = 0;
OP1HRS = 0;
P1_CSWEHRS = 0 ;
 
DO I = 1 TO 8;
   P1HRS = P1HRS + P1work (I) ;
END;
DO I = 1 TO 4;
   P1HRS = P1HRS + P1holi (I) + P1exab (I) ;
END;
P1HRS = P1HRS + P1work (12) + P1holi (8) + P1exab (8);
OP1HRS = P1work (9) + P1work (10) + P1work (11) +
         P1holi (5) + P1holi (6)  + P1holi (7) +
         P1exab (5) + P1exab (6)  + P1exab (7) ;
TP1HRS = P1HRS + OP1HRS;
P1_CSWEHRS = P1work (4) + P1work (7) +
             P1holi (1) + P1holi (3) +
             P1exab (1) + P1exab (3) ;

P2HRS  = 0;
TP2HRS = 0;
OP2HRS = 0;
P2_CSWEHRS = 0 ;

DO I = 1 TO 8;
   P2HRS = P2HRS + P2work (I);
END;
DO I = 1 TO 4;
   P2HRS = P2HRS + P2holi (I) + P2exab (I);
END;
P2HRS = P2HRS + P2work (12) + P2holi (8) + P2exab (8);
OP2HRS = P2work (9) + P2work (10) + P2work (11) +
         P2holi (5) + P2holi (6)  + P2holi (7) +
         P2exab (5) + P2exab (6)  + P2exab (7) ;
TP2HRS = P2HRS + OP2HRS;
P2_CSWEHRS = P2work (4) + P2work (7) +
             P2holi (1) + P2holi (3) +
             P2exab (1) + P2exab (3) ;
%mend;


%macro check_4_enough_tpcswehrs;
  enough_tpcswehrs = 0 ;
  %get_min_wage_rate;
  flsa_amt = TOTASST + fs_amt - cs_amt;
  Tot_flsa_hrs = flsa_amt / min_wages ;
  avg_flsa_hrs = tot_flsa_hrs / 5 ;
  avg_flsa_hrs = INT (avg_flsa_hrs) ;
  if tpcswehrs >= avg_flsa_hrs then enough_tpcswehrs = 1 ; 

%mend;

%macro CHECK_TP_DEEMED_COREHOURS;
   TPCORDMHRS = 0 ;
   TPREQCORE = 0 ;
   IF REC_CC NE 1 THEN TPREQCORE = 30 ;
   IF (REC_CC = 1 AND (P1_WPS = 08 OR P2_WPS = 08))
      THEN TPREQCORE = 30 ;
   IF (REC_CC = 1 AND (P1_WPS NE 08 AND P2_WPS NE 08))
      THEN TPREQCORE = 50 ;
   %CHECK_4_FLSA_STATE;
   IF FLSA_STATE = 1 THEN DO;
     TPCORDMHRS = P1_CORHRS + p2_CORHRS ;
	 IF ((TPCSWEHRS = 0 AND TPCORDMHRS > 0) OR TPCORDMHRS > TPREQCORE OR 
        (TPCORDMHRS > 0 AND (TPCSWEHRS + TPCORDMHRS) > TPREQCORE)) THEN TPERRFG = '7' ;
	 %check_4_enough_tpcswehrs;
	 if (TPDORDMHRS > 0 AND enough_tpcswehrs = 0) then tperrfg = '8' ;
	 IF (enough_tpcswehrs = 1 and (TPCORDMHRS > 0 AND TPCORDMHRS < TPREQCORE))
	    THEN TPDEEMCOR = 1 ;
   END;

%mend;


/*-------------------------------------------------*/
/*-   Macro to create counters for two parents    -*/
/*-------------------------------------------------*/
%macro TPCOUNTS;
TPHRS_V  = 0;
TPTHRS_V = 0;
IF (P1_WPS = 1 OR P2_WPS = 1) THEN DO;
    /* TWO-PARENT FAMILY, NOT SINGLE CUSTODIAL PARENT FAMILY - ERROR */
    TPERRFG = '4';
END;

IF  (P1_WPS = 2 OR P2_WPS = 2) THEN DO;

   P1_SANC = 0 ;
   P2_SANC = 0 ;

   IF ((P1_FAMAFF = 1 AND work_SANC = '1' AND SANC_AMT > 0) OR
       (P1_FAMAFF = 2 AND work_SANC = '1' AND SANC_AMT > 0 AND P1_WEI = 2))
       THEN P1_SANC = 1;

   IF ((P2_FAMAFF = 1 AND work_SANC = '1' AND SANC_AMT > 0) OR
       (P2_FAMAFF = 2 AND work_SANC = '1' AND SANC_AMT > 0 AND P2_WEI = 2))
       THEN P2_SANC = 1;

   IF (P1_SANC = 1 OR P2_SANC = 1) THEN TPDCARD2 = 1 ;
   ELSE DO;
      TPERRFG = '6' ;

END;
END;


IF (P1_WPS = 5 OR P2_WPS = 5) THEN DO ;
   TPDCARD5 = 1;
   IF TPDCARD2 = 1 THEN TPDCARD2 = 0;
   END ;

TPISPART = 0;
TPHRS = P1HRS + P2HRS;
TPOHRS = OP1HRS + OP2HRS;
TPTHRS = TP1HRS + TP2HRS;

IF (REC_CC NE 1 AND TPHRS >= 30 AND TPTHRS >= 35) THEN TPISPART = 1;

IF (REC_CC = 1 AND (P1_WPS = 08 OR P2_WPS = 08) AND
   TPHRS >= 30 AND TPTHRS >= 35) THEN TPISPART = 1;

IF (REC_CC = 1 AND (P1_WPS NE 08 AND P2_WPS NE 08) AND
   TPHRS >= 50 AND TPTHRS >= 55) THEN TPISPART = 1;

IF TPISPART = 1 THEN DO;
   TPwork   = 1;
   /** TPDCARD2 = 0; **/
   /** TPDCARD5 = 0; **/
   TPDCRD_PAR = 0 ;
   IF (TPDCARD2 = 1 OR TPDCARD5 = 1) THEN
      TPDCRD_PAR = 1;

   TPHRS_V  = TPHRS  - P1work (8) - P2work (8) - p1holi (4) - p2holi (4) - p1EXAB (4) - p2EXAB (4) ;
   TPTHRS_V = TPTHRS - P1work (8) - P2work (8) - p1holi (4) - p2holi (4) - p1EXAB (4) - p2EXAB (4) ;

   IF (REC_CC NE 1 AND (TPHRS_V < 30 OR TPTHRS_V < 35))
       THEN TPLIM_VOC = 1;
   IF (REC_CC = 1 AND (P1_WPS = 08 OR P2_WPS = 08) AND
       (TPHRS_V < 30 OR TPTHRS_V < 35))
       THEN TPLIM_VOC = 1;
   IF (REC_CC = 1 AND (P1_WPS NE 08 AND P2_WPS NE 08) AND
       (TPHRS_V < 50 OR TPTHRS_V < 55))
       THEN TPLIM_VOC = 1;

END;

/** new section for deemed core **/

IF TPISPART NE 1 THEN DO;
TPCSWEHRS = P1_CSWEHRS + P2_CSWEHRS ;
%CHECK_TP_DEEMED_COREHOURS;
   IF TPDEEMCOR = 1 THEN DO;
     TPDEEMCOR = 1;

     DTPHRS  = TPCSWEHRS + TPCORDMHRS ;
	 DTPTHRS = TPTHRS + TPCORDMHRS;
     
     IF (REC_CC NE 1 AND DTPHRS >= 30 AND DTPTHRS >= 35) THEN TPISPART = 1;

     IF (REC_CC = 1 AND (P1_WPS = 08 OR P2_WPS = 08) AND
          DTPHRS >= 30 AND DTPTHRS >= 35) THEN TPISPART = 1;

     IF (REC_CC = 1 AND (P1_WPS NE 08 AND P2_WPS NE 08) AND
         DTPHRS >= 50 AND DTPTHRS >= 55) THEN TPISPART = 1;

     IF TPISPART = 1 THEN DO;
         TPwork   = 1;
		 TPDCRD_PAR = 0;
		 IF (TPDCARD2 = 1 OR TPDCARD5 = 1) THEN
		     TPDCRD_PAR = 1;
 
         DTPHRS_V  = DTPHRS  - P1work (8) - P2work (8)- p1holi (4) -p2holi (4) - p1EXAB (4) - p2EXAB (4);
         DTPTHRS_V = DTPTHRS - P1work (8) - P2work (8)- p1holi (4) -p2holi (4) - p1EXAB (4) - p2EXAB (4);

         IF (REC_CC NE 1 AND (DTPHRS_V < 30 OR DTPTHRS_V < 35))
             THEN TPLIM_VOC = 1;

         IF (REC_CC = 1 AND (P1_WPS = 08 OR P2_WPS = 08) AND
           (DTPHRS_V < 30 OR DTPTHRS_V < 35))
            THEN TPLIM_VOC = 1;

         IF (REC_CC = 1 AND (P1_WPS NE 08 AND P2_WPS NE 08) AND
            (DTPHRS_V < 50 OR DTPTHRS_V < 55))
             THEN TPLIM_VOC = 1;
	  END;
   END;
END;

IF TPLIM_VOC = 1 THEN DO;
   IF TPDCARD2 = 1 THEN DO;
     TPwork = 0;
  	 TPDCARD5 = 0;
	 END;

   IF TPDCARD5 = 1 THEN DO;
	 TPwork = 0;
	 TPDCARD2 = 0;
	 END;

   IF (TPDCARD2 = 0 AND TPDCARD5 = 0) THEN DO;
      TPwork = 1;
      TPDCRD_PAR = 0;
	  END;
END;

if (TPwork = 1 AND TPLIM_VOC = 0) THEN DO;
   TPDCARD2 = 0;
   TPDCARD5 = 0;
   TPDCRD_PAR = 0;
   END; 


/* DEEMED FOR TWO PARENT work RATE */
 IF (P1_WPS = 15 OR P1_WPS = 16  OR
     P2_WPS = 15 OR P2_WPS = 16) THEN DO;
      P1_DEEM = 0 ;
      P2_DEEM = 0;
      P1_DMHRS = 0;
      P2_DMHRS = 0;
	  P1_SATSCH = 0;
	  P2_SATSCH = 0;
	  P1_EDUDIR = 0;
	  P2_EDUDIR = 0;

      P1_TEENP = 0;
      P2_TEENP = 0;
      NOW = (RPTM_YY * 10000) + (RPTM_MM * 100) + 01;
      PAST20Y = NOW - 200000;
      IF ((P1_BIRTH < NOW AND P1_BIRTH > PAST20Y)
           AND P1_PARNT = 1)  THEN P1_TEENP = 1;

      IF ((P2_BIRTH < NOW AND P2_BIRTH > PAST20Y)
           AND P2_PARNT = 1)  THEN P2_TEENP = 1;

	  P1_SATSCH = P1work (11) + P1holi (7) + P1exab (7) ;
	  P2_SATSCH = P2work (11) + P2holi (7) + P2exab (7) ;
	  P1_EDUDIR = P1work (10) + P1holi (6) + P1exab (6) ;
	  P2_EDUDIR = P2work (10) + P2holi (6) + P2exab (6) ;

      IF (P1_WPS = 15 AND P1_TEENP = 1 AND
           (P1_HEADH = 01 OR P1_HEADH = 02) AND
            P1_SATSCH > 0) THEN DO ;
               P1_DEEM = 1;
               P1_DMHRS = P1_SATSCH ;
               IF P1_DMHRS < 20 THEN P1_DMHRS = 20 ;
               P1HRS = P1HRS + P1_DMHRS ;
               OP1HRS = OP1HRS - P1_SATSCH;
               TP1HRS = P1HRS + OP1HRS ;
               END;

      IF (P1_WPS = 16 AND P1_TEENP = 1 AND
           (P1_HEADH = 01 OR P1_HEADH = 02) AND
            P1_EDUDIR >= 20) THEN DO ;
               P1_DEEM = 1;
               P1_DMHRS = P1_EDUDIR ;
               P1HRS = P1HRS + P1_DMHRS ;
               OP1HRS = OP1HRS - P1_EDUDIR ;
               TP1HRS = P1HRS + OP1HRS ;
               END ;


      IF (P2_WPS = 15 AND P2_TEENP = 1 AND
           (P2_HEADH = 01 OR P2_HEADH = 02) AND
            P2_SATSCH > 0) THEN DO ;
               P2_DEEM = 1;
               P2_DMHRS = P2_SATSCH ;
               IF P2_DMHRS < 20 THEN P2_DMHRS = 20 ;
               P2HRS = P2HRS + P2_DMHRS ;
               OP2HRS = OP2HRS - P2_SATSCH ;
               TP2HRS = P2HRS + OP2HRS ;
               END;

      IF (P2_WPS = 16 AND P2_TEENP = 1 AND
           (P2_HEADH = 01 OR P2_HEADH = 02) AND
            P2_EDUDIR >= 20) THEN DO ;
               P2_DEEM = 1;
               P2_DMHRS = P2_EDUDIR ;
               P2HRS = P2HRS + P2_DMHRS ;
               OP2HRS = OP2HRS - P2_EDUDIR ;
               TP2HRS = P2HRS + OP2HRS ;
               END ;

      TPHRS  = P1HRS  + P2HRS;
      TPOHRS = OP1HRS + OP2HRS;
      TPTHRS = TP1HRS + TP2HRS;


    IF (P1_DEEM = 1 OR P2_DEEM = 1) THEN DO;

       IF P1_DEEM = 1 AND P2_DEEM = 1 THEN DO;
          TPwork   = 1 ;
          TPDEEM   = 1 ;
          TPLIMIT  = 1 ;
          END;

       ELSE DO ;
          IF (REC_CC NE 1 AND
              TPHRS >= 30 AND TPTHRS >= 35) THEN DO;
              TPISPART = 1 ;
              TPwork   = 1 ;
              TPDEEM   = 1 ;
              TPLIMIT  = 1 ;
              END;

          IF (REC_CC = 1 AND (P1_WPS = 08 OR P2_WPS = 08) AND
              TPHRS >= 30 AND TPTHRS >= 35) THEN DO;
              TPISPART = 1 ;
              TPwork   = 1 ;
              TPDEEM   = 1 ;
              TPLIMIT  = 1 ;
              END;

          IF (REC_CC = 1 AND
              (P1_WPS NE 08 AND P2_WPS NE 08) AND
              TPHRS >= 50 AND TPTHRS >= 55) THEN DO;
              TPISPART = 1 ;
              TPwork   = 1 ;
              TPDEEM   = 1 ;
              TPLIMIT  = 1 ;
              END;
			  IF (TPwork = 1 AND (TPDCARD2 = 1 OR TPDCARD5 = 1)) THEN
			     TPDCRD_PAR = 1 ;
			  IF TPDCRD_PAR = 1 THEN DO;
                 IF (TPLIM_VOC = 1 OR TPLIMIT = 1) THEN DO;
                    IF TPDCARD2 = 1 THEN DO;
					   TPwork   = 0;
					   TPDCARD5 = 0;
					   END;
                    IF TPDCARD5 = 1 THEN DO;
					   TPwork   = 0;
					   TPDCARD2 = 0;
					   END;
					END;
				ELSE DO;
				   TPwork = 1;
                   TPDCARD2 = 0;
                   TPDCARD5 = 0;
                   END;

             END ;
           END;
         END;
   IF ((TPDCARD2 = 1 OR TPDCARD5 = 1) AND TPLIMIT = 1) THEN DO;
      TPwork = 0;
      TPDCRD_PAR = 1; 
   END;


END;
		 /* END OF DEEMED FOR TWO PARENT work RATE CODING */


		 /* DV EXEMPTION FOR TWO-PARENT FAMILY  */
 IF (TPwork NE 1 AND (P1_WPS = 09 OR P2_WPS = 09)) THEN TPDVEXEM = 1 ;

%mend;


/*-------------------------------------------------*/
/*-   Macro to create all families counters for   -*/
/*-         family with more than one adult       -*/
/*-------------------------------------------------*/
%macro TP_AFR;

     DO J = 1 TO N_WEI ;
        AF = IPER (J);
        %init_af ;
        %SETAF;
        %AFCOUNTS;
        ISwork (J)  = Nwork ;
        ISLIMVOC (J) = NLIM_VOC ;
        ISLIMSSA (J) = NLIM_SSA ;
        ISLIMEDE (J) = NLIM_EDE ;
        ISDV (J)    = NDVEXEM ;
        IF NDCARD1 = 1  THEN ISDCARD (J) = 1;
        IF NDCARD2 = 1  THEN ISDCARD (J) = 2;
        IF NDCARD5 = 1  THEN ISDCARD (J) = 5;
     END ;

     NGAGED = 0 ;
     DO J = 1 TO N_WEI ;
        IF ISwork (J) = 1 THEN NGAGED+1 ;	
     END ;


     DO J = 1 TO N_WEI ;
        HOLDER (J) =  ( (1000 * ISwork (J)) + ( 100 * ISDCARD (J)) -
                        ( 10 * ISLIMVOC (J)) + ISDV (J)) ;
        USEPER (J) = IPER (J) ;
     END ;

/* SORT HOLDER TO IDENTIFY PERSON TO USE FOR AF RATE  */;
IF N_WEI = 2 THEN DO;
   IF HOLDER1 >= HOLDER2 THEN AF = IPER1;
   IF HOLDER2 >  HOLDER1 THEN AF = IPER2;

   %init_af ;
   %setaf;
   %afcounts;

END ;

IF N_WEI > 2 THEN DO ;

   DO J = 1 TO 2;
      DO K = J+1 TO N_WEI;
         IF HOLDER (K) > HOLDER (J) THEN DO;
            HLDTOT = HOLDER (J);
            HLDPER = USEPER (J);

            HOLDER (J) = HOLDER (K);
            USEPER (J) = USEPER (K);

            HOLDER (K) = HLDTOT;
            USEPER (K) = HLDPER;
            END;
         END;
      END;

   AF = usePER1;
   %init_af ;
   %setaf;
   %afcounts;

END;

%mend;

******************* 2. The data step part **********************;

DATA FILE1  ;
   set final;
FILE OUT LRECL= 524  ;

   ARRAY A1work  {13}   A1work1-A1work13;
   ARRAY A2work  {13}   A2work1-A2work13;
   ARRAY A3work  {13}   A3work1-A3work13;
   ARRAY A4work  {13}   A4work1-A4work13;
   ARRAY A5work  {13}   A5work1-A5work13;
   ARRAY A6work  {13}   A6work1-A6work13;
   ARRAY AFwork  {13}   AFwork1-AFwork13;
   ARRAY A1holi  {8}   A1holi1-A1holi8;
   ARRAY A2holi  {8}   A2holi1-A2holi8;
   ARRAY A3holi  {8}   A3holi1-A3holi8;
   ARRAY A4holi  {8}   A4holi1-A4holi8;
   ARRAY A5holi  {8}   A5holi1-A5holi8;
   ARRAY A6holi  {8}   A6holi1-A6holi8;
   ARRAY AFholi  {8}   AFholi1-AFholi8;
   ARRAY A1exab  {8}   A1exab1-A1exab8;
   ARRAY A2exab  {8}   A2exab1-A2exab8;
   ARRAY A3exab  {8}   A3exab1-A3exab8;
   ARRAY A4exab  {8}   A4exab1-A4exab8;
   ARRAY A5exab  {8}   A5exab1-A5exab8;
   ARRAY A6exab  {8}   A6exab1-A6exab8;
   ARRAY AFexab  {8}   AFexab1-AFexab8;
   ARRAY IPER    {6}    IPER1-IPER6;
   ARRAY P1work  {13}   P1work1-P1work13;
   ARRAY P2work  {13}   P2work1-P2work13;
   ARRAY P1holi  {8}    P1holi1-P1holi8;
   ARRAY P2holi  {8}    P2holi1-P2holi8;
   ARRAY P1exab  {8}    P1exab1-P1exab8;
   ARRAY P2exab  {8}    P2exab1-P2exab8;
   ARRAY BIRTH   {10}   BIRTH1-BIRTH10;
   ARRAY TPERWK  {13}   TPERWK1-TPERWK13;
   ARRAY THRSWK  {13}   THRSWK1-THRSWK13;
   ARRAY TPERHL  {8}   TPERHL1-TPERHL8;
   ARRAY THRSHL  {8}   THRSHL1-THRSHL8;
   ARRAY TPEREA  {8}   TPEREA1-TPEREA8;
   ARRAY THRSEA  {8}   THRSEA1-THRSEA8;
   ARRAY ISwork  {6}    ISwork1-ISwork6;
   ARRAY ISDCARD {6}    ISDCARD1-ISDCARD6;
   ARRAY ISLIMVOC {6}   ISLIMVOC1-ISLIMVOC6;
   ARRAY ISLIMSSA {6}   ISLIMSSA1-ISLIMSSA6;
   ARRAY ISLIMEDE {6}   ISLIMEDE1-ISLIMEDE6;
   ARRAY ISDV    {6}    ISDV1-ISDV6;
   ARRAY ISNCP   {6}    ISNCP1-ISNCP6;
   ARRAY USEPER  {6}    USEPER1-USEPER6;
   ARRAY HOLDER  {6}    HOLDER1-HOLDER6;
   ARRAY ISPARNT {6}    ISPARNT1-ISPARNT6;


      /* INITIALIZING COUNTERS */
      NSAMP    = 1 ;
  
      %INIT_AF;
      %INIT_TP;
      NGAGED = 0 ;

 DO I = 1 TO 6;
         IPER (I)    = 0 ;
         ISwork (I)  = 0 ;
         ISDCARD (I) = 0 ;
         ISLIMVOC (I) = 0;
		 ISLIMSSA (I) = 0;
		 ISLIMEDE (I) = 0;
         ISDV (I)    = 0 ;
         ISNCP (I)   = 0 ;
         HOLDER (I)  = 0 ;
         USEPER (I)  = 0 ;
      END;

      DO I = 1 TO 13;
         AFwork (I) = 0;
         P1work (I) = 0;
         P2work (I) = 0;
         TPERWK (I) = 0;
         THRSWK (I) = 0;
      END;

      DO I = 1 TO 8 ;
		 AFholi (I) = 0;
         AFexab (I) = 0;
         P1holi (I) = 0;
         P1exab (I) = 0;
		 P2holi (i) = 0;
		 P2exab (I) = 0;
		 TPERHL (I) = 0;
         THRSHL (I) = 0;
         TPEREA (I) = 0;
         THRSEA (I) = 0;
	  END;


DO I = 1 TO 13;
         IF A1work (I) = . THEN A1work (I) = 0;
         IF A2work (I) = . THEN A2work (I) = 0;
         IF A3work (I) = . THEN A3work (I) = 0;
         IF A4work (I) = . THEN A4work (I) = 0;
         IF A5work (I) = . THEN A5work (I) = 0;
         IF A6work (I) = . THEN A6work (I) = 0;
      END;

	  DO I = 1 TO 8 ;
         IF A1holi (I) = . THEN A1holi (I) = 0;
         IF A2holi (I) = . THEN A2holi (I) = 0;
         IF A3holi (I) = . THEN A3holi (I) = 0;
         IF A4holi (I) = . THEN A4holi (I) = 0;
         IF A5holi (I) = . THEN A5holi (I) = 0;
         IF A6holi (I) = . THEN A6holi (I) = 0;
         IF A1exab (I) = . THEN A1exab (I) = 0;
         IF A2exab (I) = . THEN A2exab (I) = 0;
         IF A3exab (I) = . THEN A3exab (I) = 0;
         IF A4exab (I) = . THEN A4exab (I) = 0;
         IF A5exab (I) = . THEN A5exab (I) = 0;
         IF A6exab (I) = . THEN A6exab (I) = 0;


	  END;

      IF REC_CC   = .  THEN REC_CC = 0;
      IF FS_AMT   = .  THEN FS_AMT = 0;
	  IF CS_AMT   = .  THEN CS_AMT = 0;
      IF CASH_AMT = .  THEN CASH_ANT = 0;
      IF CC_AMT   = .  THEN CC_AMT = 0;
      IF TRST_AMT = .  THEN TRST_AMT = 0;
      IF TRNP_AMT = .  THEN TRNP_AMT = 0;
      IF OTH_AMT  = .  THEN OTH_AMT = 0;
	  IF NCORHRS  = . THEN NCORHRS  = 0; 
      IF TPCORHRS = . THEN TPCORHRS = 0;
 
      /*----------------------------------------------------------*/
      /* CHECK FOR NUMBER OF WEI IN FAMILY                        */
      /* TABULATE THE NUMBER OF PERSON AND HOURS OF PARTICIPATION */
      /*    FOR EACH work ACTIVITY.                               */
      /*----------------------------------------------------------*/
      N_WEI    = 0;
      WRK_WEI  = 0;
	  HOL_WEI  = 0;
	  EXA_WEI  = 0;
      NUM_PAR  = 0;
	  wkhlea_wei = 0 ;
      B_WEI = 0 ;

      IF (FAMAFF_1 = 1 OR FAMAFF_1 = 2) THEN DO;
         B_WEI    = WEI_1;
		 B_FAMAFF = FAMAFF_1 ;
		 B_CITZ   = CITZ_1 ;
		 B_AIDABD = AIDABD_1 ;
		 B_SSI    = SSI_1 ;
		 B_RECSSI = RECSSI_1 ;
		 B_WPS    = WPS_1 ;
		 B_NCP    = NCP_1 ;
         %CHECK_4_WEI;
         IF ISWEI = 1 THEN DO;
         N_WEI+1;
         IPER(N_WEI) = 1;
         SUMHRS = 0;
         DO I = 1 TO 13;
            IF A1work (I) > 0 THEN DO;
               TPERWK (I) = TPERWK (I) + 1;
               THRSWK (I) = THRSWK (I) + A1work (I) ;
               SUMHRS = SUMHRS + A1work (I) ;
            END;
         END;
		 SUMHLHRS = 0 ; 
         DO I = 1 TO 8;
            IF A1holi (I) > 0 THEN DO;
               TPERHL (I) = TPERHL (I) + 1;
               THRSHL (I) = THRSHL (I) + A1holi (I) ;
               SUMHLHRS = SUMHLHRS + A1holi (I) ;
            END;
         END;
         SUMEAHRS = 0 ; 
         DO I = 1 TO 8;
            IF A1exab (I) > 0 THEN DO;
               TPEREA (I) = TPEREA (I) + 1;
               THRSEA (I) = THRSEA (I) + A1exab (I) ;
               SUMEAHRS = SUMHLHRS + A1exab (I) ;
            END;
         END;
        IF SUMHRS > 0 THEN WRK_WEI+1 ;
		IF SUMHLHRS > 0 THEN HOL_WEI+1 ;
        IF SUMEAHRS > 0 THEN EXA_WEI+1 ;
        IF (SUMHRS > 0 OR SUMHLHRS > 0 OR SUMEAHRS > 0) THEN WKHLEA_WEI+1;
      END;
	  END;

      IF (FAMAFF_2 = 1 OR FAMAFF_2 = 2) THEN DO;
         B_WEI    = WEI_2;
		 B_FAMAFF = FAMAFF_2 ;
		 B_CITZ   = CITZ_2 ;
		 B_AIDABD = AIDABD_2 ;
		 B_SSI    = SSI_2 ;
		 B_RECSSI = RECSSI_2 ;
		 B_WPS    = WPS_2 ;
		 B_NCP    = NCP_2 ;
         %CHECK_4_WEI;
         IF ISWEI = 1 THEN DO;
         N_WEI+1;
         IPER(N_WEI) = 2;
         SUMHRS = 0;
         DO I = 1 TO 13;
            IF A2work (I) > 0 THEN DO;
               TPERWK (I) = TPERWK (I) + 1;
               THRSWK (I) = THRSWK (I) + A2work (I);
               SUMHRS = SUMHRS + A2work (I) ;
            END;
         END;
		 SUMHLHRS = 0 ; 
         DO I = 1 TO 8;
            IF A2holi (I) > 0 THEN DO;
               TPERHL (I) = TPERHL (I) + 1;
               THRSHL (I) = THRSHL (I) + A2holi (I) ;
               SUMHLHRS = SUMHLHRS + A2holi (I) ;
            END;
         END;
         SUMEAHRS = 0 ; 
         DO I = 1 TO 8;
            IF A2exab (I) > 0 THEN DO;
               TPEREA (I) = TPEREA (I) + 1;
               THRSEA (I) = THRSEA (I) + A2exab (I) ;
               SUMEAHRS = SUMEAHRS + A2exab (I) ;
            END;
         END;
         IF SUMHRS > 0 THEN WRK_WEI+1 ;
	     IF SUMHLHRS > 0 THEN HOL_WEI+1 ;
         IF SUMEAHRS > 0 THEN EXA_WEI+1 ;
		 IF (SUMHRS > 0 OR SUMHLHRS > 0 OR SUMEAHRS > 0) THEN WKHLEA_WEI+1;

      END;
	  END;

      IF (FAMAFF_3 = 1 OR FAMAFF_3 = 2) THEN DO;
	     B_WEI    = WEI_3;
		 B_FAMAFF = FAMAFF_3 ;
		 B_CITZ   = CITZ_3 ;
		 B_AIDABD = AIDABD_3 ;
		 B_SSI    = SSI_3 ;
		 B_RECSSI = RECSSI_3 ; 
		 B_WPS    = WPS_3 ;
		 B_NCP    = NCP_3 ;
         %CHECK_4_WEI;
         IF ISWEI = 1 THEN DO;
          N_WEI+1;
         IPER(N_WEI) = 3;
         SUMHRS = 0;
         DO I = 1 TO 13;
            IF A3work (I) > 0 THEN DO;
               TPERWK (I) = TPERWK (I) + 1;
               THRSWK (I) = THRSWK (I) + A3work (I);
               SUMHRS = SUMHRS + A3work (I) ;
            END;
         END;
	     SUMHLHRS = 0 ; 
         DO I = 1 TO 8;
            IF A3holi (I) > 0 THEN DO;
               TPERHL (I) = TPERHL (I) + 1;
               THRSHL (I) = THRSHL (I) + A3holi (I) ;
               SUMHLHRS = SUMHLHRS + A3holi (I) ;
            END;
         END;
         SUMEAHRS = 0 ; 
         DO I = 1 TO 8;
            IF A3exab (I) > 0 THEN DO;
               TPEREA (I) = TPEREA (I) + 1;
               THRSEA (I) = THRSEA (I) + A3exab (I) ;
               SUMEAHRS = SUMEAHRS + A3exab (I) ;
            END;
         END;
         IF SUMHRS > 0 THEN WRK_WEI+1 ;
		 IF SUMHLHRS > 0 THEN HOL_WEI+1 ;
         IF SUMEAHRS > 0 THEN EXA_WEI+1 ;
         IF (SUMHRS > 0 OR SUMHLHRS > 0 OR SUMEAHRS > 0) THEN WKHLEA_WEI+1;

      END;
	  END;

      IF (FAMAFF_4 = 1 OR FAMAFF_4 = 2) THEN DO;
	     B_WEI    = WEI_4;
		 B_FAMAFF = FAMAFF_4 ;
		 B_CITZ   = CITZ_4 ;
		 B_AIDABD = AIDABD_4 ;
		 B_SSI    = SSI_4 ;
		 B_RECSSI = RECSSI_4 ;
		 B_WPS    = WPS_4 ;
		 B_NCP    = NCP_4 ;
         %CHECK_4_WEI;
         IF ISWEI = 1 THEN DO;
          N_WEI+1;
         IPER(N_WEI) = 4;
         SUMHRS = 0;
         DO I = 1 TO 13;
            IF A4work (I) > 0 THEN DO;
               TPERWK (I) = TPERWK (I) + 1;
               THRSWK (I) = THRSWK (I) + A4work (I);
               SUMHRS = SUMHRS + A4work (I) ;
            END;
         END;
		 SUMHLHRS = 0 ; 
         DO I = 1 TO 8;
            IF A4holi (I) > 0 THEN DO;
               TPERHL (I) = TPERHL (I) + 1;
               THRSHL (I) = THRSHL (I) + A4holi (I) ;
               SUMHLHRS = SUMHLHRS + A4holi (I) ;
            END;
         END;
         SUMEAHRS = 0 ; 
         DO I = 1 TO 8;
            IF A4exab (I) > 0 THEN DO;
               TPEREA (I) = TPEREA (I) + 1;
               THRSEA (I) = THRSEA (I) + A4exab (I) ;
               SUMEAHRS = SUMEAHRS + A4exab (I) ;
            END;
         END;
         IF SUMHRS > 0 THEN WRK_WEI+1 ;
		 IF SUMHLHRS > 0 THEN HOL_WEI+1 ;
         IF SUMEAHRS > 0 THEN EXA_WEI+1 ;
		 IF (SUMHRS > 0 OR SUMHLHRS > 0 OR SUMEAHRS > 0) THEN WKHLEA_WEI+1;

      END;
	  END;

      IF (FAMAFF_5 = 1 OR FAMAFF_5 = 2) THEN DO;
	     B_WEI    = WEI_5;
		 B_FAMAFF = FAMAFF_5 ;
		 B_CITZ   = CITZ_5 ;
		 B_AIDABD = AIDABD_5 ;
		 B_SSI    = SSI_5 ;
		 B_RECSSI = RECSSI_5 ;
		 B_WPS    = WPS_5 ;
		 B_NCP    = NCP_5 ;
         %CHECK_4_WEI;
         IF ISWEI = 1 THEN DO;
          N_WEI+1;
         IPER(N_WEI) = 5;
         SUMHRS = 0;
         DO I = 1 TO 13;
            IF A5work (I) > 0 THEN DO;
               TPERWK (I) = TPERWK (I) + 1;
               THRSWK (I) = THRSWK (I) + A5work (I);
               SUMHRS = SUMHRS + A5work (I) ;
            END;
         END;
		 SUMHLHRS = 0 ; 
         DO I = 1 TO 8;
            IF A5holi (I) > 0 THEN DO;
               TPERHL (I) = TPERHL (I) + 1;
               THRSHL (I) = THRSHL (I) + A5holi (I) ;
               SUMHLHRS = SUMHLHRS + A5holi (I) ;
            END;
         END;
         SUMEAHRS = 0 ; 
         DO I = 1 TO 8;
            IF A5exab (I) > 0 THEN DO;
               TPEREA (I) = TPEREA (I) + 1;
               THRSEA (I) = THRSEA (I) + A5exab (I) ;
               SUMEAHRS = SUMEAHRS + A5exab (I) ;
            END;
         END;
         IF SUMHRS > 0 THEN WRK_WEI+1 ;
		 IF SUMHLHRS > 0 THEN HOL_WEI+1 ;
         IF SUMEAHRS > 0 THEN EXA_WEI+1 ;
		 IF (SUMHRS > 0 OR SUMHLHRS > 0 OR SUMEAHRS > 0) THEN WKHLEA_WEI+1;
      END;
	  END;

      IF (FAMAFF_6 = 1 OR FAMAFF_6 = 2) THEN DO;
         B_WEI    = WEI_6;
		 B_FAMAFF = FAMAFF_6 ;
		 B_CITZ   = CITZ_6 ;
		 B_AIDABD = AIDABD_6 ;
		 B_SSI    = SSI_6 ;
		 B_RECSSI = RECSSI_6 ;
		 B_WPS    = WPS_6 ;
		 B_NCP    = NCP_6 ;
         %CHECK_4_WEI;
         IF ISWEI = 1 THEN DO;
          N_WEI+1;
         IPER(N_WEI) = 6;
         SUMHRS = 0;
         DO I = 1 TO 13;
            IF A6work (I) > 0 THEN DO;
               TPERWK (I) = TPERWK (I) + 1;
               THRSWK (I) = THRSWK (I) + A6work (I);
               SUMHRS = SUMHRS + A6work (I) ;
            END;
         END;
		 SUMHLHRS = 0 ; 
         DO I = 1 TO 8;
            IF A6holi (I) > 0 THEN DO;
               TPERHL (I) = TPERHL (I) + 1;
               THRSHL (I) = THRSHL (I) + A6holi (I) ;
               SUMHLHRS = SUMHLHRS + A6holi (I) ;
            END;
         END;
         SUMEAHRS = 0 ; 
         DO I = 1 TO 8;
            IF A6exab (I) > 0 THEN DO;
               TPEREA (I) = TPEREA (I) + 1;
               THRSEA (I) = THRSEA (I) + A6exab (I) ;
               SUMEAHRS = SUMEAHRS + A6exab (I) ;
            END;
         END;
         IF SUMHRS > 0 THEN WRK_WEI+1 ;
	     IF SUMHLHRS > 0 THEN HOL_WEI+1 ;
         IF SUMEAHRS > 0 THEN EXA_WEI+1 ;
		 IF (SUMHRS > 0 OR SUMHLHRS > 0 OR SUMEAHRS > 0) THEN WKHLEA_WEI+1;
      END;
      END;	

    /* LISTED IN ERROR CASE  */
      IF DISP = 2 THEN DO;
         NLIE = 1;
         GOTO OUTREC;
      END;

      /* VALID TYPE_FAM CODES ARE 1, 2, & 3.   */
      IF (TYPE_FAM < 1 OR TYPE_FAM > 3) THEN DO;
         /* WRITE ERR MSG: MISSING TYPE_FAM CODE ? */
         AFERRFG = '01' ;
         IF N_WEI = 0 THEN DO;
            NLIE = 1;
            GOTO OUTREC;
         END;

      TOTASST = CASH_AMT + CC_AMT + 
	            OTH_AMT + TRST_AMT + TRNP_AMT ;
      IF TOTASST = 0 THEN DO;
	     ZPAYCASE = 1 ;
		 GOTO OUTREC;   
		 END;

         IF N_WEI = 1 THEN DO;
            /* IS PARTI FOR AF RATE? */
            AF = IPER1;
            %setaf;
            %afcounts;
            IF Nwork = 1 THEN NGAGED = 1 ;
            GOTO OUTREC;
         END;

         IF N_WEI >= 2 THEN DO;
            IS2P = 0;
            %DETFI2P;

            IF IS2P = 1 THEN DO;
               TPSAMP = 1;
               %SET2P;
               IF DO2P = 1 THEN DO ;  
                  %CHECKHRS;
                  %TPCOUNTS;
               END ;
               %TP_AFR;
               GOTO OUTREC;
            END;
            IF IS2P = 0 THEN DO;
               /* IF NO,  IS PARTICIPATING FOR AF RATE? */
               %TP_AFR;
               GOTO OUTREC;
            END;
         END;

      END;


      /* NO PARENT FAMILIES */
      IF TYPE_FAM = 3 THEN DO;
         IF N_WEI = 0 THEN N_NOWEI = 1;
         ELSE IF AFERRFG = '  ' THEN AFERRFG = '09' ;
         GOTO OUTREC;
      END;

      /* 1-PARENT FAMILY */
      IF TYPE_FAM = 1 THEN DO;
         IF N_WEI = 0 THEN DO;
            /* WRITE RECORD TO ERROR FILE REG. MISSING DATA */
            AFERRFG = '02';
            /* WE ASSUME THE FAMILY DID NOT PATICIPATE */
            GOTO OUTREC;
         END;

         IF N_WEI = 1 THEN DO;
            AF = IPER1;
            %setaf;
            %afcounts;
            IF Nwork = 1 THEN NGAGED = 1 ;
            GOTO OUTREC;
         END;

         IF N_WEI >= 2 THEN DO;
            %TP_AFR;
            GOTO OUTREC;
         END;
      END;

      IF TYPE_FAM = 2  THEN DO;
         TPSAMP = 1;
         IF N_WEI = 0 THEN DO;
            /* WRITE RECORD TO ERROR FILE REG. MISSING DATA */
            TPERRFG = '1';
            /* WE ASSUME THE FAMILY DID NOT PATICIPATE */
            GOTO OUTREC;
         END;

         IF N_WEI = 1 THEN DO;
            /* WE ASSUME STATE FAILED TO MEET TWO PARENT RATE */
            TPERRFG = '2';
            /* CHECK FOR MEETING ALL FAMILY RATE */
            AF = IPER1;
            %setaf;
            %afcounts;
            IF Nwork = 1 THEN NGAGED = 1 ;
            GOTO OUTREC;
         END;

         IF N_WEI >= 2 THEN DO;
            /* DETERMINE IF TWO-PARENT FAMILY */
            IS2P = 0;
            %DETFI2P;

            /* IF YES, IS PARTICIPATING FOR BOTH AF AND TP RATES  */
            IF IS2P = 1 THEN DO;
               %SET2P;
               IF DO2P = 1 THEN DO ;
                  %CHECKHRS;
                  %TPCOUNTS;
               END ; 
               %TP_AFR;
               GOTO OUTREC;
            END;
            IF IS2P = 0 THEN DO;
               /* IF NO,  IS PARTICIPATING FOR AF RATE?  */
               %TP_AFR;
               GOTO OUTREC;
            END;
         END;

      END;

   OUTREC: NCOUNTY = COUNTY + 0 ;
      ENDPOS = '9';
      PUT    @1   YEAR       4.  @5   REGION     2.  @7   FIPS       2.
             @9   RPTM_MM    2.  @11  STRATUM    2.  @13  CASEID   $11.
             @24  TYPE_FAM   1.  @25  N_WEI      1.  @26  WRK_WEI    1.
             @27  HOL_WEI    1.  @28  EXA_WEI    1.
             @29  NSAMP      1.  @30  Nwork      1.  @31  N_NOWEI    1.
             @32  NDCARD1    1.  @33  NDCARD2    1.  @34  NDCARD5    1.
             @35  NLIE       1.  @36  NLIM_VOC   1.  @37  NLIM_SSA   1.
		     @38  NLIM_EDE   1.  @39  NEXEMPT    1.  @40  NDISABLE   1.
             @41  NDEEM15    1.  @42  NDEEM16    1.  @43  NDEEM17    1.  
             @44  NDEEMCOR   1.  @45  AF_NCORHRS 2.  @47  CSWE_HRS   3.
             @50  FLSA_STATE 1.  @51  DCOR_PART  1.  @52  DHRS_WRK   3.  
             @55  DTHS_WRK   3.  @58  NDVEXEM    1.  @59  AFERRFG   $2. 
             @61  NDCRD_PART 1.
             @62  AF_FAMAFF  1.  @63 AF_NCP      1.  @64  AF_BIRTH   8.
			 @72  AF_MARITAL 1.  @73  AF_HEADH   2.  @75  AF_PARENT  1.
			 @76  AF_CITZ    1.  @77  AF_RECSSI  1.  @77  AF_WEI     2.
             @80  AF_SSI     8.
             @88  AF_WPS     2.  @90 AFwork1    2.  @92  AFwork2    2.
             @94  AFwork3    2.  @96  AFwork4   2.  @98  AFwork5    2.
             @100 AFwork6    2.  @102 AFwork7    2.  @104 AFwork8    2.
             @106 AFwork9    2.  @108 AFwork10   2.  @110 AFwork11   2.
             @112 AFwork12   2.  @114 AFwork13   2.
             @116 AFholi1    2.  @118 AFholi2    2.  @120 AFholi3    2.
             @122 AFholi4    2.  @124 AFholi5    2.  @126 AFholi6    2.
             @128 AFholi7    2.  @130 AFholi8    2.  
             @132 AFexab1    2.  @134 AFexab2    2.  @136 AFexab3    2.
             @138 AFexab4    2.  @140 AFexab5    2.  @142 AFexab6    2.
             @144 AFexab7    2.  @146 AFexab8    2.  
             @148 TPSAMP     1.  @149 TPwork     1.  @150 NUM_PAR    1.
             @151 TPDCARD2   1.  @152 TPDCARD5   1.  @153 TPLIM_VOC  1.
		     @153 TPLIMIT    1.  @155 TPEXEMPT   1.  @156 TPDEEM     1.  
             @156 TPDVEXEM   1.  @158 TPNCP      1.  @159 TPDABLE    1.
             @160 TPERRFG   $1.  @161 TPDEEMCOR  1.  @162 TPCORDMHRS 3.
             @165 TPCSWEHRS  3.  @168 DTPHRS     3.  @171 DTPTHRS    3. 
             @174 DTPHRS_V   3.  @177 DTPTHRS_V  3.  @180 TPDCRD_PAR 1. 
             @181 HRS_WRK    3.  @184 OHRS_WRK   3.  @187 THRS_WRK   3.  
             @190 TPHRS      3.  @193 TPOHRS     3.  @196 TPTHRS     3.
             @199 P1HRS      3.  @202 OP1HRS     3.  @205 TP1HRS     3.
             @208 P2HRS      3.  @211 OP2HRS     3.  @214 TP2HRS     3.
             @217 HWK_VOC    3.  @220 TWK_VOC    3.  @223 HRS_DEEM   3.
             @226 TPHRS_V    3.  @229 TPTHRS_V   3. 
             @232 P1_WPS     2.  @234 P1work1    2.  @236 P1work2    2.
             @238 P1work3    2.  @240 P1work4    2.  @242 P1work5    2.
             @244 P1work6    2.  @246 P1work7    2.  @248 P1work8    2.
             @250 P1work9    2.  @252 P1work10   2.  @254 P1work11   2.
             @256 P1work12   2.  @258 P1work13   2.  
             @260 P1holi1    2.  @262 P1holi2    2.  @264 P1holi3    2.
             @266 P1holi4    2.  @268 P1holi5    2.  @270 P1holi6    2.
             @272 P1holi7    2.  @274 P1holi8    2.   
             @276 P1exab1    2.  @278 P1exab2    2.  @280 P1exab3    2.
             @282 P1exab4    2.  @284 P1exab5    2.  @286 P1exab6    2.
             @288 P1exab7    2.  @290 P1exab8    2.  
             @292 P1_NCP     1.  @293 P1_PARNT   1.  @294 P1_WEI     2.
             @296 P2_WPS     2.  @298 P2work1    2.  @300 P2work2    2.
             @302 P2work3    2.  @304 P2work4    2.  @306 P2work5    2.
             @308 P2work6    2.  @310 P2work7    2.  @312 P2work8    2.
             @314 P2work9    2.  @316 P2work10   2.  @318 P2work11   2.
             @320 P2work12   2.  @322 P2work13   2.  
             @324 P2holi1    2.  @326 P2holi2    2.  @328 P2holi3    2.
             @330 P2holi4    2.  @332 P2holi5    2.  @334 P2holi6    2.
             @336 P2holi7    2.  @338 P2holi8 2.   
             @340 P2exab1    2.  @342 P2exab2    2.  @344 P2exab3    2.
             @346 P2exab4    2.  @348 P2exab5    2.  @350 P2exab6    2.
             @352 P2exab7    2.  @354 P2exab8    2.  


             @356 P2_NCP     1.  @357 P2_PARNT   1.  @358 p2_WEI     2.
             @360 TPERWK1    2.  @362 TPERWK2    2.  @354 TPERWK3    2.
             @366 TPERWK4    2.  @368 TPERWK5    2.  @370 TPERWK6    2.
             @372 TPERWK7    2.  @374 TPERWK8    2.  @376 TPERWK9    2.
             @378 TPERWK10   2.  @380 TPERWK11   2.  @382 TPERWK12   2.
             @384 TPERWK13   2. 
             @386 THRSWK1    3.  @389 THRSWK2    3.  @392 THRSWK3    3.
             @395 THRSWK4    3.  @398 THRSWK5    3.  @401 THRSWK6    3.
             @404 THRSWK7    3.  @407 THRSWK8    3.  @410 THRSWK9    3.
             @413 THRSWK10   3.  @416 THRSWK11   3.  @419 THRSWK12   3.
             @422 THRSWK13   3. 
             @425 TPERHL1    2.  @427 TPERHL2    2.  @429 TPERHL3    2.
             @431 TPERHL4    2.  @433 TPERHL5    2.  @435 TPERHL6    2.
             @437 TPERHL7    2.  @439 TPERHL8    2.  
             @441 THRSHL1    3.  @444 THRSHL2    3.  @447 THRSHL3    3.
             @450 THRSHL4    3.  @453 THRSHL5    3.  @456 THRSHL6    3.
             @459 THRSHL7    3.  @462 THRSHL8    3.
             @465 TPEREA1    2.  @467 TPEREA2    2.  @469 TPEREA3    2.
             @471 TPEREA4    2.  @473 TPEREA5    2.  @475 TPEREA6    2.
             @477 TPEREA7    2.  @479 TPEREA8    2.  
             @481 THRSEA1    3.  @484 THRSEA2    3.  @487 THRSEA3    3.
             @490 THRSEA4    3.  @493 THRSEA5    3.  @496 THRSEA6    3.
             @499 THRSEA7    3.  @502 THRSEA8    3. 
             @505 NCOUNTY    3.  @508 NGAGED     1.  @509 ZIP_CODE   5.
             @514 SANC_AMT   8.  @522 work_SANC $1.  @523 WKHLEA_WEI 1.
             @524 ENDPOS    $1.;   

run;

DATA ERRFLAG;
   SET FILE1 ;
FILE EFF LRECL= 87   PAD;
IF AFERRFG > '  ' OR TPERRFG > ' ' ;
IF AFERRFG = '  ' THEN AFERRFG = '00' ;
IF TPERRFG = ' '  THEN TPERRFG = '0'  ;
IF AF_NCORHRS  = . THEN AF_NCORHRS  = 0 ;
IF AF_TPCORHRS = . THEN AF_TPCORHRS = 0 ;
PUT @01 FIPS        @04 STRATUM    @07 RPTM_MM  @10 CASEID     @22 TYPE_FAM
    @24 N_WEI       @26 NSAMP      @28 NWORK    @30 N_NOWEI    @32 NDCARD1
    @34 NDCARD2     @36 NDCARD5    @38 NLIE     @40 NLIM_VOC   @42 NLIM_SSA 
    @44 NLIM_EDE    @46 nDEEMCOR   @48 AF_NCORHRS @52 NDCRD_PART 
    @54 NDVEXEM     @56 AFERRFG    @59 NUM_PAR  @61 TPSAMP     @63 TPWORK    
    @65 TPDCARD2    @67 TPDCARD5   @69 TPNCP    @71 TPDABLE    @73 TPLIM_VOC 
    @75 TPLIMIT     @77 TPDCRD_PAR @79 TPDVEXEM
    @81 TPDEEMCOR   @83 AF_TPCORHRS @87 TPERRFG ;

RUN;













DATA EPRPART1;
   set FILE1;

IF (FIPS = 06 AND NLIE = 1) THEN DELETE ;

AFNWRK1  = 0 ;
AFNWRK2  = 0 ;
AFNWRK3  = 0 ;
AFNWRK4  = 0 ;
AFNWRK5  = 0 ;
AFNWRK6  = 0 ;
AFNWRK7  = 0 ;
AFNWRK8  = 0 ;
AFNWRK9  = 0 ;
AFNWRK10 = 0 ;
AFNWRK11 = 0 ;
AFNWRK12 = 0 ;
AFNWRK13 = 0 ;

P1NWRK1  = 0 ;
P1NWRK2  = 0 ;
P1NWRK3  = 0 ;
P1NWRK4  = 0 ;
P1NWRK5  = 0 ;
P1NWRK6  = 0 ;
P1NWRK7  = 0 ;
P1NWRK8  = 0 ;
P1NWRK9  = 0 ;
P1NWRK10 = 0 ;
P1NWRK11 = 0 ;
P1NWRK12 = 0 ;
P1NWRK13 = 0 ;

P2NWRK1  = 0 ;
P2NWRK2  = 0 ;
P2NWRK3  = 0 ;
P2NWRK4  = 0 ;
P2NWRK5  = 0 ;
P2NWRK6  = 0 ;
P2NWRK7  = 0 ;
P2NWRK8  = 0 ;
P2NWRK9  = 0 ;
P2NWRK10 = 0 ;
P2NWRK11 = 0 ;
P2NWRK12 = 0 ;
P2NWRK13 = 0 ;

IF AFWORK1  > 0  THEN AFNWRK1  = 1;
IF AFWORK2  > 0  THEN AFNWRK2  = 1;
IF AFWORK3  > 0  THEN AFNWRK3  = 1;
IF AFWORK4  > 0  THEN AFNWRK4  = 1;
IF AFWORK5  > 0  THEN AFNWRK5  = 1;
IF AFWORK6  > 0  THEN AFNWRK6  = 1;
IF AFWORK7  > 0  THEN AFNWRK7  = 1;
IF AFWORK8  > 0  THEN AFNWRK8  = 1;
IF AFWORK9  > 0  THEN AFNWRK9  = 1;
IF AFWORK10 > 0  THEN AFNWRK10 = 1;
IF AFWORK11 > 0  THEN AFNWRK11 = 1;
IF AFWORK12 > 0  THEN AFNWRK12 = 1;
IF AFWORK13 > 0  THEN AFNWRK13 = 1;

IF P1WORK1  > 0  THEN P1NWRK1  = 1;
IF P1WORK2  > 0  THEN P1NWRK2  = 1;
IF P1WORK3  > 0  THEN P1NWRK3  = 1;
IF P1WORK4  > 0  THEN P1NWRK4  = 1;
IF P1WORK5  > 0  THEN P1NWRK5  = 1;
IF P1WORK6  > 0  THEN P1NWRK6  = 1;
IF P1WORK7  > 0  THEN P1NWRK7  = 1;
IF P1WORK8  > 0  THEN P1NWRK8  = 1;
IF P1WORK9  > 0  THEN P1NWRK9  = 1;
IF P1WORK10 > 0  THEN P1NWRK10 = 1;
IF P1WORK11 > 0  THEN P1NWRK11 = 1;
IF P1WORK12 > 0  THEN P1NWRK12 = 1;
IF P1WORK13 > 0  THEN P1NWRK13 = 1;

IF P2WORK1  > 0  THEN P2NWRK1  = 1;
IF P2WORK2  > 0  THEN P2NWRK2  = 1;
IF P2WORK3  > 0  THEN P2NWRK3  = 1;
IF P2WORK4  > 0  THEN P2NWRK4  = 1;
IF P2WORK5  > 0  THEN P2NWRK5  = 1;
IF P2WORK6  > 0  THEN P2NWRK6  = 1;
IF P2WORK7  > 0  THEN P2NWRK7  = 1;
IF P2WORK8  > 0  THEN P2NWRK8  = 1;
IF P2WORK9  > 0  THEN P2NWRK9  = 1;
IF P2WORK10 > 0  THEN P2NWRK10 = 1;
IF P2WORK11 > 0  THEN P2NWRK11 = 1;
IF P2WORK12 > 0  THEN P2NWRK12 = 1;
IF P2WORK13 > 0  THEN P2NWRK13 = 1;

PROC SUMMARY DATA= EPRPART1;
   CLASS FIPS RPTM_MM STRATUM;
   VAR NSAMP      NWORK      NLIE       NDCARD1    NDCARD2
       NEXEMPT    NDCARD5    NLIM_VOC   NLIM_SSA   NLIM_EDE
       NDEEM15    NDEEM16    NDEEM17    N_NOWEI    NDISABLE
       NDVEXEM    N_WEI      WRK_WEI    HOL_WEI
	   EXA_WEI    WKHLEA_WEI NGAGED     NDEEMCOR   
       TPSAMP     TPWORK     TPEXEMPT   TPLIM_VOC  TPLIMIT 
       TPDCARD2   TPDCARD5   TPDEEM     TPDVEXEM   TPNCP
       TPDABLE    TPDEEMCOR  
       AFWORK1    AFWORK2    AFWORK3    AFWORK4    AFWORK5
       AFWORK6    AFWORK7    AFWORK8    AFWORK9    AFWORK10
       AFWORK11   AFWORK12   AFWORK13   
       P1WORK1    P1WORK2    P1WORK3    P1WORK4    P1WORK5
       P1WORK6    P1WORK7    P1WORK8    P1WORK9    P1WORK10
       P1WORK11   P1WORK12   P1WORK13   
       P2WORK1    P2WORK2    P2WORK3    P2WORK4    P2WORK5
       P2WORK6    P2WORK7    P2WORK8    P2WORK9    P2WORK10
       P2WORK11   P2WORK12   P2WORK13   
       AFNWRK1    AFNWRK2    AFNWRK3    AFNWRK4    AFNWRK5
       AFNWRK6    AFNWRK7    AFNWRK8    AFNWRK9    AFNWRK10
       AFNWRK11   AFNWRK12   AFNWRK13   
       P1NWRK1    P1NWRK2    P1NWRK3    P1NWRK4    P1NWRK5
       P1NWRK6    P1NWRK7    P1NWRK8    P1NWRK9    P1NWRK10
       P1NWRK11   P1NWRK12   P1NWRK13   
       P2NWRK1    P2NWRK2    P2NWRK3    P2NWRK4    P2NWRK5
       P2NWRK6    P2NWRK7    P2NWRK8    P2NWRK9    P2NWRK10
       P2NWRK11   P2NWRK12   P2NWRK13   
       TPERWK1    TPERWK2    TPERWK3    TPERWK4    TPERWK5
       TPERWK6    TPERWK7    TPERWK8    TPERWK9    TPERWK10
       TPERWK11   TPERWK12   TPERWK13   
       THRSWK1    THRSWK2    THRSWK3    THRSWK4    THRSWK5
       THRSWK6    THRSWK7    THRSWK8    THRSWK9    THRSWK10
       THRSWK11   THRSWK12   THRSWK13 
       AFholi1    AFholi2    AFholi3    AFholi4    AFholi5
       AFholi6    AFholi7    AFholi8    
       P1holi1    P1holi2    P1holi3    P1holi4    P1holi5
       P1holi6    P1holi7    P1holi8    
       P2holi1    P2holi2    P2holi3    P2holi4    P2holi5
       P2holi6    P2holi7    P2holi8 
       TPERHL1    TPERHL2    TPERHL3    TPERHL4    TPERHL5
       TPERHL6    TPERHL7    TPERHL8   
       THRSHL1    THRSHL2    THRSHL3    THRSHL4    THRSHL5
       THRSHL6    THRSHL7    THRSHL8   
       AFexab1    AFexab2    AFexab3    AFexab4    AFexab5
       AFexab6    AFexab7    AFexab8    
       P1exab1    P1exab2    P1exab3    P1exab4    P1exab5
       P1exab6    P1exab7    P1exab8    
       P2exab1    P2exab2    P2exab3    P2exab4    P2exab5
       P2exab6    P2exab7    P2exab8 
       TPEREA1    TPEREA2    TPEREA3    TPEREA4    TPEREA5
       TPEREA6    TPEREA7    TPEREA8   
       THRSEA1    THRSEA2    THRSEA3    THRSEA4    THRSEA5
       THRSEA6    THRSEA7    THRSEA8   ;
   OUTPUT OUT = STATP1
          SUM =  SNSAMP     SNWORK      SNLIE     SNDCARD1  SNDCARD2
                 SNEXEMPT   SNDCARD5    SNLIM_VOC SNLIM_SSA SNLIM_EDE
                 SNDEEM15   SNDEEM16    SNDEEM17  SN_NOWEI  SNDISABL
                 SNDVEXEM   SN_WEI      SWRK_WEI  SHOL_WEI
                 SEXA_WEI   SWKHLEA_WEI SNGAGED     SNDMCOR    
                 STPSAMP    STPWORK     STPEXEMP   STPLIM_V   STPLIMIT 
                 STPDCRD2   STPDCRD5    STPDEEM    STPDVEXM   STPNCP
                 STPDABLE   STPDEEMCOR      
                 SAFWRK1    SAFWRK2     SAFWRK3    SAFWRK4    SAFWRK5
                 SAFWRK6    SAFWRK7     SAFWRK8    SAFWRK9    SAFWRK10
                 SAFWRK11   SAFWRK12    SAFWRK13   
                 SP1WRK1    SP1WRK2     SP1WRK3    SP1WRK4    SP1WRK5
                 SP1WRK6    SP1WRK7     SP1WRK8    SP1WRK9    SP1WRK10
                 SP1WRK11   SP1WRK12    SP1WRK13   
                 SP2WRK1    SP2WRK2     SP2WRK3    SP2WRK4    SP2WRK5
                 SP2WRK6    SP2WRK7     SP2WRK8    SP2WRK9    SP2WRK10
                 SP2WRK11   SP2WRK12    SP2WRK13   
                 SAFNWK1    SAFNWK2     SAFNWK3    SAFNWK4    SAFNWK5
                 SAFNWK6    SAFNWK7     SAFNWK8    SAFNWK9    SAFNWK10
                 SAFNWK11   SAFNWK12    SAFNWK13   
                 SP1NWK1    SP1NWK2     SP1NWK3    SP1NWK4    SP1NWK5
                 SP1NWK6    SP1NWK7     SP1NWK8    SP1NWK9    SP1NWK10
                 SP1NWK11   SP1NWK12    SP1NWK13   
                 SP2NWK1    SP2NWK2     SP2NWK3    SP2NWK4    SP2NWK5
                 SP2NWK6    SP2NWK7     SP2NWK8    SP2NWK9    SP2NWK10
                 SP2NWK11   SP2NWK12    SP2NWK13   
                 SPERWK1    SPERWK2     SPERWK3    SPERWK4    SPERWK5
                 SPERWK6    SPERWK7     SPERWK8    SPERWK9    SPERWK10
                 SPERWK11   SPERWK12    SPERWK13   
                 SHRSWK1    SHRSWK2     SHRSWK3    SHRSWK4    SHRSWK5
                 SHRSWK6    SHRSWK7     SHRSWK8    SHRSWK9    SHRSWK10
                 SHRSWK11   SHRSWK12    SHRSWK13  
                 safhol1    safhol2    safhol3    safhol4    safhol5
                 safhol6    safhol7    safhol8    
                 sp1hol1    sp1hol2    sp1hol3    sp1hol4    sp1hol5
                 sp1hol6    sp1hol7    sp1hol8    
                 sp2hol1    sp2hol2    sp2hol3    sp2hol4    sp2hol5
                 sp2hol6    sp2hol7    sp2hol8 
                 SPERHL1    SPERHL2    SPERHL3    SPERHL4    SPERHL5
                 SPERHL6    SPERHL7    SPERHL8   
                 SHRSHL1    SHRSHL2    SHRSHL3    SHRSHL4    SHRSHL5
                 SHRSHL6    SHRSHL7    SHRSHL8   
                 safexa1    safexa2    safexa3    safexa4    safexa5
                 safexa6    safexa7    safexa8    
                 sp1exa1    sp1exa2    sp1exa3    sp1exa4    sp1exa5
                 sp1exa6    sp1exa7    sp1exa8    
                 sp2exa1    sp2exa2    sp2exa3    sp2exa4    sp2exa5
                 sp2exa6    sp2exa7    sp2exa8 
                 SPEREA1    SPEREA2    SPEREA3    SPEREA4    SPEREA5
                 SPEREA6    SPEREA7    SPEREA8   
                 SHRSEA1    SHRSEA2    SHRSEA3    SHRSEA4    SHRSEA5
                 SHRSEA6    SHRSEA7    SHRSEA8   ;

RUN ;

DATA SUMMFLE;
   SET STATP1  ;
   IF FIPS    = . THEN DELETE ;
   IF STRATUM = . THEN DELETE ;
   IF RPTM_MM = . THEN DELETE ;
   SSIZE = SNSAMP ;

PROC SORT;
   BY FIPS STRATUM;

RUN ;

DATA WGTFLE;
   INFILE WGT4 LRECL = 40 PAD ;
   INPUT FIPS      $ 1 -  2 +2
         STRATUM     5 -  6 +2
         M01CASES    9 - 17 +2
         M02CASES   20 - 28 +2
         M03CASES   31 - 39
         VALIDINC $ 40        ;

PROC SORT;
   BY FIPS STRATUM;

RUN ;


DATA COMBO;
   MERGE SUMMFLE WGTFLE ;
      BY FIPS STRATUM;
   IF FIPS    = . THEN DELETE;
   IF RPTM_MM = . THEN DELETE;
   IF STRATUM = . THEN DELETE;

   IF (RPTM_MM = 01 OR RPTM_MM = 04 OR RPTM_MM = 07 OR
       RPTM_MM = 10)  THEN TCASES = M01CASES ;
   IF (RPTM_MM = 02 OR RPTM_MM = 05 OR RPTM_MM = 08 OR
       RPTM_MM = 11)  THEN TCASES = M02CASES ;
   IF (RPTM_MM = 03 OR RPTM_MM = 06 OR RPTM_MM = 09 OR
       RPTM_MM = 12)  THEN TCASES = M03CASES ;

   IF TCASES = . THEN TCASES = 1 ;
   IF SSIZE GE TCASES THEN TCASES = SSIZE;
   IF SSIZE = 0 THEN DO;
      WNSAMP    = SNSAMP     ;
      WNWORK    = SNWORK     ;
      WNDCARD1  = SNDCARD1   ;
      WNDCARD2  = SNDCARD2   ;
      WNEXEMPT  = SNEXEMPT   ;
      WNDCARD5  = SNDCARD5   ;
      WNLIM_VOC = SNLIM_VOC ;
	  WNLIM_SSA = SNLIM_SSA ;
      WNLIM_EDE = SNLIM_EDE ;
      WNDEEM15  = SNDEEM15  ;
	  WNDEEM16  = SNDEEM16  ;
      WNDEEM17  = SNDEEM17  ;
      WN_NOWEI  = SN_NOWEI   ;
      WN_WEI    = SN_WEI     ;
      WNLIE     = SNLIE      ;
      WNDVEXEM  = SNDVEXEM   ;
      WNDISABL  = SNDISABL   ;
      WNDMCOR   = SNDMCOR   ;
      WTPSAMP   = STPSAMP    ;
      WTPWORK   = STPWORK    ;
      WTPEXEMP  = STPEXEMP   ;
      WTPLIM_V  = STPLIM_V   ;
	  WTPLIMIT  = STPLIMIT   ;
      WTPDCRD2  = STPDCRD2   ;
      WTPDCRD5  =  STPDCRD5   ;
      WTPDEEM   = STPDEEM    ;
      WTPDVEXM  = STPDVEXM   ;
      WTPNCP    = STPNCP     ;
      WTPDABLE  = STPDABLE   ;
      WN_NOWEI  = SN_NOWEI   ;
      WWRK_WEI  = SWRK_WEI   ;
	  WHOL_WEI  = SHOL_WEI   ;
      WEXA_WEI  = SEXA_WEI   ;
	  WWKHLEA_WEI  = SWKHLEA_WEI   ;
      WNGAGED   = SNGAGED    ;
      WTPDEEMCOR = STPDEEMCOR   ;
      END ;
   ELSE DO ;
      WNSAMP     = TCASES * SNSAMP    / SSIZE ;
      WNWORK     = TCASES * SNWORK    / SSIZE ;
      WNDCARD1   = TCASES * SNDCARD1  / SSIZE ;
      WNDCARD2   = TCASES * SNDCARD2  / SSIZE ;
      WNEXEMPT   = TCASES * SNEXEMPT  / SSIZE ;
      WNDCARD5   = TCASES * SNDCARD5  / SSIZE ;
      WNLIM_VOC  = TCASES * SNLIM_VOC   / SSIZE ;
	  WNLIM_SSA  = TCASES * SNLIM_SSA   / SSIZE ;
      WNLIM_EDE  = TCASES * SNLIM_EDE   / SSIZE ;
      WNDEEM15   = TCASES * SNDEEM15    / SSIZE ;
	  WNDEEM16   = TCASES * SNDEEM16    / SSIZE ;
      WNDEEM17   = TCASES * SNDEEM17    / SSIZE ;
      WN_NOWEI   = TCASES * SN_NOWEI  / SSIZE ;
      WN_WEI     = TCASES * SN_WEI    / SSIZE ;
      WNLIE      = TCASES * SNLIE     / SSIZE ;
      WNDVEXEM   = TCASES * SNDVEXEM  / SSIZE ;
      WNDISABL   = TCASES * SNDISABL  / SSIZE ;
      WTPSAMP    = TCASES *  STPSAMP  / SSIZE ;
	  WNDMCOR    = TCASES * SNDMCOR   / SSIZE ;
      WTPWORK    = TCASES *  STPWORK  / SSIZE ;
      WTPEXEMP   = TCASES *  STPEXEMP / SSIZE ;
      WTPLIM_V   = TCASES *  STPLIM_V / SSIZE ;
	  WTPLIMIT   = TCASES *  STPLIMIT / SSIZE ;
      WTPDCRD2   = TCASES *  STPDCRD2 / SSIZE ;
      WTPDCRD5   = TCASES *  STPDCRD5 / SSIZE ;
      WTPDEEM    = TCASES *  STPDEEM  / SSIZE ;
      WTPDVEXM   = TCASES *  STPDVEXM / SSIZE ;
      WTPNCP     = TCASES *  STPNCP   / SSIZE ;
      WTPDABLE   = TCASES *  STPDABLE / SSIZE ;
      WN_NOWEI   = TCASES *  SN_NOWEI / SSIZE ;
      WWRK_WEI   = TCASES *  SWRK_WEI / SSIZE ;
	  WHOL_WEI   = TCASES *  SHOL_WEI / SSIZE ;
      WEXA_WEI   = TCASES *  SEXA_WEI / SSIZE ;
	  WWHHLEA_WEI = TCASES *  SWKHLEA_WEI / SSIZE ;
      WNGAGED    = TCASES *  SNGAGED  / SSIZE ;
      WTPDMCOR   = TCASES *  STPDEEMCOR / SSIZE ;
      END ;

PROC PRINT ;
   BY FIPS ;
   VAR FIPS       STRATUM    RPTM_MM  TCASES   SSIZE
       SNWORK     SNSAMP     SNDCARD1 SNDCARD2 SNDCARD5
       SNLIE      SN_NOWEI   SNDMCOR  STPSAMP  
       STPWORK    STPDCRD2   STPDCRD5 STPNCP   STPDABLE 
       STPDEEMCOR SNGAGED ;
   TITLE 'BASE SUMMED DATA FOR WPR CALCULATIONS' ;

RUN ;

PROC SORT DATA= COMBO ;
   BY FIPS RPTM_MM STRATUM  ;

RUN ;

DATA OLD.SF&MON ;
   SET COMBO ;

RUN ;

PROC DELETE DATA= EPRPART1 STATP1 SUMMFLE COMBO ;


PROC DELETE DATA= FINAL FILE1;

RUN ;

%mend createm;

/*** %macro createm(FY,YYM,QTR) ;     ***/
/** TRNFLE: Name of the State Quarterly Transmission File ***/
/** FIP   : State FIPS Code                 format ##     ***/
/** FY    : 4 Dogot Fiscal Year             format YYYY   ***/
/** YYM   : Fiscal Year and Month           format YYYYMM ***/
/** QTR   : Fiscal Year and Quarter         format YYQ    ***/
/** YQ    : Fiscal Year and Quarter         format YYYYQ  ***/
/** MON   : 2 digit Year and month          format YYMM   ***/

%createm(FTP1.ts33.txt,33,2009,200810,091,20091,0810) ;
%createm(FTP1.ts33.txt,33,2009,200811,091,20091,0811) ;
%createm(FTP1.ts33.txt,33,2009,200812,091,20091,0812) ;



QUIT;

