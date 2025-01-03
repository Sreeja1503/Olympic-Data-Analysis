/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Wednesday, December 11, 2024     TIME: 12:03:13 PM
PROJECT: DA6223_Final_project_V1
PROJECT PATH: Z:\Desktop\MSDA\DA6223_Final_project_V1.egp
---------------------------------------- */

/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend _sas_pushchartsize;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend _sas_popchartsize;


/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide


%*--------------------------------------------------------------*
 * Tests the current version against a required version. A      *
 * negative result means that the SAS server version is less    *
 * than the version required.  A positive result means that     *
 * the SAS server version is greater than the version required. *
 * A result of zero indicates that the SAS server is exactly    *
 * the version required.                                        *
 *                                                              *
 * NOTE: The parameter maint is optional.                       *
 *--------------------------------------------------------------*;
%macro _SAS_VERCOMP(major, minor, maint);
    %_SAS_VERCOMP_FV(&major, &minor, &maint, &major, &minor, &maint)
%mend _SAS_VERCOMP;

%*--------------------------------------------------------------*
 * Tests the current version against either the required        *
 * foundation or Viya required version depending on whether the *
 * SYSVLONG version is a foundation or Viya one. A negative     *
 * result means that the SAS server version is less than the    *
 * version required.  A positive result means that the SAS      *
 * server version is greater than the version required. A       *
 * result of zero indicates that the SAS server is exactly the  *
 * version required.                                            *
 *                                                              *
 * NOTE: The *maint parameters are optional.                    *
 *--------------------------------------------------------------*;
%macro _SAS_VERCOMP_FV(fmajor, fminor, fmaint, vmajor, vminor, vmaint);
    %local major;
    %local minor;
    %local maint;
    %local CurMaj;
    %local CurMin;
    %local CurMnt;

    %* Pull the current version string apart.;
    %let CurMaj = %scan(&sysvlong, 1, %str(.));

    %* The Viya version number has a V on the front which means
       we need to adjust the Maint SCAN funtion index and also
       get the appropriate parameters for the major, minor, and
       maint values we need to check against (foundation or Viya);
    %if %eval(&CurMaj EQ V) %then
        %do;
		   %*   MM mm t           MM = Major version , mm = Minor version , t = Maint version ;
		   %* V.03.04M2P07112018 ;

            %let major = &vmajor;
            %let minor = &vminor;
            %let maint = &vmaint;
			%let CurMaj = %scan(&sysvlong, 2, %str(.));
			%* Index is purposely 2 because V is now one of the scan delimiters ;
			%let CurMin = %scan(&sysvlong, 2, %str(.ABCDEFGHIKLMNOPQRSTUVWXYZ));
			%let CurMnt = %scan(&sysvlong, 3, %str(.ABCDEFGHIKLMNOPQRSTUVWXYZ));
        %end;
    %else
        %do;
		    %* M mm    t           M = Major version , mm = Minor version , t = Maint version ;  
		    %* 9.01.02M0P11212005 ;

            %let major = &fmajor;
            %let minor = &fminor;
            %let maint = &fmaint;
			%let CurMin = %scan(&sysvlong, 2, %str(.));
			%let CurMnt = %scan(&sysvlong, 4, %str(.ABCDEFGHIKLMNOPQRSTUVWXYZ));
        %end;

    %* Now perform the version comparison.;
    %if %eval(&major NE &CurMaj) %then
        %eval(&CurMaj - &major);
    %else
        %if %eval(&minor NE &CurMin) %then
            %eval(&CurMin - &minor);
        %else
            %if "&maint" = "" %then
                %str(0);
            %else
                %eval(&CurMnt - &maint);
%mend _SAS_VERCOMP_FV;

%*--------------------------------------------------------------*
 * This macro calls _SAS_VERCONDCODE_FV() with the passed       *
 * version. If the current server version matches or is newer,  *
 * then the true code (tcode) is executed, else the false code  *
 * (fcode) is executed.                                         *
 * Example:                                                     *
 *  %let isV92 =                                                *
 *     %_SAS_VERCONDCODE(9,2,0,                                 *
 *         tcode=%nrstr(Yes),                                   *
 *         fcode=%nrstr(No))                                    *
 *--------------------------------------------------------------*;
%macro _SAS_VERCONDCODE( major, minor, maint, tcode=, fcode= );
    %_SAS_VERCONDCODE_FV( &major, &minor, &maint, &major, &minor, &maint, &tcode, fcode )
%mend _SAS_VERCONDCODE;

%*--------------------------------------------------------------*
 * This macro calls _SAS_VERCOMP_FV() with the passed versions. *
 * If the current server version matches or is newer, then the  *
 * true code (tcode) is executed, else the false code (fcode)   *
 * is executed.                                                 *
 * Example:                                                     *
 *  %let isV92 =                                                *
 *     %_SAS_VERCONDCODE_FV(9,2,0, 3,5,0                        *
 *         tcode=%nrstr(Yes),                                   *
 *         fcode=%nrstr(No))                                    *
 *--------------------------------------------------------------*;
%macro _SAS_VERCONDCODE_FV( fmajor, fminor, fmaint, vmajor, vminor, vmaint, tcode=, fcode= );
    %if %_SAS_VERCOMP_FV(&fmajor, &fminor, &fmaint, &vmajor, &vminor, &vmaint) >= 0 %then
        %do;
        &tcode
        %end;
    %else
        %do;
        &fcode
        %end;
%mend _SAS_VERCONDCODE_FV;

%*--------------------------------------------------------------*
 * Tests the current version to see if it is a Viya version     *
 * number.                                                      *
 * A result of 1 indicates that the SAS server is a Viya        *
 * server.                                                      *
 * A zero result indicates that the server version is not       *
 * that of a Viya server.                                       *
 *--------------------------------------------------------------*;
%macro _SAS_ISVIYA;
    %local Major;

    %* Get the major component of the current version string.;
    %let Major = %scan(&sysvlong, 1, %str(.));

    %* Check if it it V for Viya.;
    %if %eval(&Major EQ V) %then
        %str(1);
    %else
        %str(0);
%mend _SAS_ISVIYA;


ODS PROCTITLE;
OPTIONS DEV=SVG;
GOPTIONS XPIXELS=0 YPIXELS=0;
%macro HTML5AccessibleGraphSupported;
    %if %_SAS_VERCOMP_FV(9,4,4, 0,0,0) >= 0 %then ACCESSIBLE_GRAPH;
%mend;
FILENAME EGHTMLX TEMP;
ODS HTML5(ID=EGHTMLX) FILE=EGHTMLX
    OPTIONS(BITMAP_MODE='INLINE')
    %HTML5AccessibleGraphSupported
    ENCODING='utf-8'
    STYLE=HtmlBlue
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
;

/*   START OF NODE: Data_Merging   */
%LET _CLIENTTASKLABEL='Data_Merging';
%LET _CLIENTPROCESSFLOWNAME='Data Cleaning';
%LET _CLIENTPROJECTPATH='Z:\Desktop\MSDA\DA6223_Final_project_V1.egp';
%LET _CLIENTPROJECTPATHHOST='HORRDS13';
%LET _CLIENTPROJECTNAME='DA6223_Final_project_V1.egp';
%LET _SASPROGRAMFILE='';
%LET _SASPROGRAMFILEHOST='';

proc sql;
create table Olympics_results_1 as
select country_3_letter_code, country_name,discipline_title,
event_title,medal_type,participant_type,athlete_full_name, athlete_url, rank_position,slug_game
from work.olympic_results;
quit;

proc sql;
    create table joined_table as
    select r.*, 
           h.game_location, h.game_name, h.game_season, h.game_year
    from work.Olympics_results_1 as r
    left join work.olympic_hosts as h
    on strip(r.slug_game) = strip(h.game_slug);
quit;

proc sql;
create table master_joined_table as 
select joined_table.*, 
olympic_athletes.games_participations,olympic_athletes.first_game,	
olympic_athletes.athlete_year_birth,	
olympic_athletes.athlete_medals
from work.joined_table as joined_table
left join work.olympic_athletes as olympic_athletes
on joined_table.athlete_url=olympic_athletes.athlete_url;
quit;


data filtered_table;
    set work.master_joined_table;
    if 2000 <= game_year <= 2020 and game_season = 'Summer';
run;

data master_table_updated;
    set work.filtered_table;
    if country_name = 'Democratic Republic of Congo' then country_name = 'Congo';
	if country_name = 'German Democratic Republic(Germany)' then country_name = 'Germany';
	if country_name = 'Federal Republic of Germany' then country_name = 'Germany';
	if country_name = 'ROC' then country_name = 'Russia';
    if country_name = 'Olympic Athletes from Russia' then country_name = 'Russia';
	if country_name = 'Russian Federation' then country_name = 'Russia';
	if country_name = 'People''s Republic of China' then country_name = 'China';
	if country_name = 'Republic of Korea' then country_name = 'Korea';
	if country_name = 'Korea Team' then country_name = 'Korea';
	if country_name = 'Democratic People''s Republic of Korea' then country_name = 'Korea';
run;


proc sql;
    create table updated_master_table as
    select *,
        case
            when lowcase(event_title) like '%women%' or 
                 lowcase(event_title) like '%woman%' or 
                 lowcase(event_title) like '%ladies%' then 'Female'
            when lowcase(event_title) like '%men%' or 
                 lowcase(event_title) like '%man%' then 'Male'
            else 'Mixed Team' 
        end as gender
    from master_table_updated; 
quit;

proc sql;
    alter table work.updated_master_table
    add medal_count num;

    update work.updated_master_table
    set medal_count = case 
                          when medal_type is not null then 1
                          else 0
                      end;
quit;

proc sql;
    alter table work.updated_master_table
    add medal_points num;

    update work.updated_master_table
    set medal_points = case 
                          when medal_type = 'GOLD' then 3
                          when medal_type = 'SILVER' then 2
                          when medal_type = 'BRONZE' then 1
                          else 0
                      end;
quit;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;
%LET _SASPROGRAMFILEHOST=;


/*   START OF NODE: Summary Statistics   */
%LET _CLIENTTASKLABEL='Summary Statistics';
%LET _CLIENTPROCESSFLOWNAME='Data Cleaning';
%LET _CLIENTPROJECTPATH='Z:\Desktop\MSDA\DA6223_Final_project_V1.egp';
%LET _CLIENTPROJECTPATHHOST='HORRDS13';
%LET _CLIENTPROJECTNAME='DA6223_Final_project_V1.egp';

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, December 11, 2024 at 12:02:34 PM
   By task: Summary Statistics

   Input Data: Local:WORK.UPDATED_MASTER_TABLE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set WORK.UPDATED_MASTER_TABLE
   ------------------------------------------------------------------- */
PROC SORT
	DATA=WORK.UPDATED_MASTER_TABLE(KEEP=athlete_year_birth game_year gender)
	OUT=WORK.SORTTempTableSorted
	;
	BY gender;
RUN;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by SAS (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	NWAY
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		N 
		NMISS	;
	VAR athlete_year_birth;
	CLASS game_year /	ORDER=UNFORMATTED ASCENDING;
	BY gender;

RUN;
/* -------------------------------------------------------------------
   End of task code
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Bar Chart   */
%LET _CLIENTTASKLABEL='Bar Chart';
%LET _CLIENTPROCESSFLOWNAME='Data Cleaning';
%LET _CLIENTPROJECTPATH='Z:\Desktop\MSDA\DA6223_Final_project_V1.egp';
%LET _CLIENTPROJECTPATHHOST='HORRDS13';
%LET _CLIENTPROJECTNAME='DA6223_Final_project_V1.egp';

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, December 11, 2024 at 12:02:34 PM
   By task: Bar Chart

   Input Data: Local:WORK.UPDATED_MASTER_TABLE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.UPDATED_MASTER_TABLE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.gender, T.game_year
	FROM WORK.UPDATED_MASTER_TABLE as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR=NONE


;
Axis2
	STYLE=1
	WIDTH=1


;




;
TITLE;
TITLE1 "Male/Female Participation from 2000-2020";
FOOTNOTE;
FOOTNOTE1 "Generated by SAS (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 gender
 /
	GROUP=game_year
	CLIPREF
		SPACE=0
FRAME	TYPE=FREQ
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
PATTERNID=MIDPOINT
;
/* -------------------------------------------------------------------
   End of task code
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Bar Chart 1   */
%LET _CLIENTTASKLABEL='Bar Chart 1';
%LET _CLIENTPROCESSFLOWNAME='Data Cleaning';
%LET _CLIENTPROJECTPATH='Z:\Desktop\MSDA\DA6223_Final_project_V1.egp';
%LET _CLIENTPROJECTPATHHOST='HORRDS13';
%LET _CLIENTPROJECTNAME='DA6223_Final_project_V1.egp';

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, December 11, 2024 at 12:02:34 PM
   By task: Bar Chart 1

   Input Data: Local:WORK.UPDATED_MASTER_TABLE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.UPDATED_MASTER_TABLE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.gender, T.game_year, T.medal_count
	FROM WORK.UPDATED_MASTER_TABLE as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)


;
Axis2
	STYLE=1
	WIDTH=1


;




;
TITLE;
TITLE1 "Bar Chart";
FOOTNOTE;
FOOTNOTE1 "Generated by SAS (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 gender
 /
	SUMVAR=medal_count
	GROUP=game_year
	CLIPREF
FRAME	TYPE=SUM
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
PATTERNID=MIDPOINT
;
/* -------------------------------------------------------------------
   End of task code
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Bar Chart 2   */
%LET _CLIENTTASKLABEL='Bar Chart 2';
%LET _CLIENTPROCESSFLOWNAME='Data Cleaning';
%LET _CLIENTPROJECTPATH='Z:\Desktop\MSDA\DA6223_Final_project_V1.egp';
%LET _CLIENTPROJECTPATHHOST='HORRDS13';
%LET _CLIENTPROJECTNAME='DA6223_Final_project_V1.egp';

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, December 11, 2024 at 12:02:34 PM
   By task: Bar Chart 2

   Input Data: Local:WORK.UPDATED_MASTER_TABLE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.UPDATED_MASTER_TABLE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.gender, T.game_year, T.medal_points
	FROM WORK.UPDATED_MASTER_TABLE as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)


;
Axis2
	STYLE=1
	WIDTH=1


;




;
TITLE;
TITLE1 "Bar Chart";
FOOTNOTE;
FOOTNOTE1 "Generated by SAS (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR 
	 gender
 /
	SUMVAR=medal_points
	GROUP=game_year
	CLIPREF
FRAME	TYPE=SUM
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
PATTERNID=MIDPOINT
;
/* -------------------------------------------------------------------
   End of task code
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Standardization   */
%LET _CLIENTTASKLABEL='Data Standardization';
%LET _CLIENTPROCESSFLOWNAME='Data Cleaning';
%LET _CLIENTPROJECTPATH='Z:\Desktop\MSDA\DA6223_Final_project_V1.egp';
%LET _CLIENTPROJECTPATHHOST='HORRDS13';
%LET _CLIENTPROJECTNAME='DA6223_Final_project_V1.egp';
%LET _SASPROGRAMFILE='';
%LET _SASPROGRAMFILEHOST='';

proc format;
   value $country_fmt (default = 50)
      'TÜRKIYE' = 'TURKEY'
      'TURKIYE' = 'TURKEY'   
      'TÃ¼RKIYE' = 'TURKEY'  
      'ROC' = 'RUSSIAN FEDERATION'
      'IR IRAN' = 'ISLAMIC REPUBLIC OF IRAN'
      'DPR KOREA' = 'DEMOCRATIC PEOPLE''S REPUBLIC OF KOREA'
      'GREAT BRITAIN' = 'UNITED KINGDOM'
      'PEOPLE''S REPUBLIC OF CHINA' = 'CHINA'
      'CHINESE TAIPEI' = 'TAIWAN'
      'CZECH REPUBLIC' = 'CZECHIA'
      'UNITED STATES' = 'UNITED STATES OF AMERICA'
      'AIN' = 'INDEPENDENT OLYMPIC ATHLETES'
      'EOR' = 'INDEPENDENT OLYMPIC ATHLETES'
      'SERBIA AND MONTENEGRO' = 'SERBIA'
      'REFUGEE OLYMPIC TEAM' = 'INDEPENDENT OLYMPIC ATHLETES'
      other = [upcase.]   
   ;
run;

proc sql;
   alter table work.medals
   modify country_long char(50);
   update work.medals
   set country_long = put(country_long, $country_fmt.);
quit;

proc sql;
   alter table work.athletes
   modify country_long char(50);
   update work.athletes
   set country_long = put(country_long, $country_fmt.);
quit;

proc sql;
   alter table work.olympic_results
   modify country_name char(50); 
   update work.olympic_results
   set country_name = put(country_name, $country_fmt.);
quit;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;
%LET _SASPROGRAMFILEHOST=;


/*   START OF NODE: Feature Creation   */
%LET _CLIENTTASKLABEL='Feature Creation';
%LET _CLIENTPROCESSFLOWNAME='Data Cleaning';
%LET _CLIENTPROJECTPATH='Z:\Desktop\MSDA\DA6223_Final_project_V1.egp';
%LET _CLIENTPROJECTPATHHOST='HORRDS13';
%LET _CLIENTPROJECTNAME='DA6223_Final_project_V1.egp';
%LET _SASPROGRAMFILE='';
%LET _SASPROGRAMFILEHOST='';


/* Combine historical and Paris 2024 medal data */
proc sql;
   create table medals_data_by_country_year as
   select distinct 
          a.country_name length=50, 
          game_year,
          a.discipline_title as discipline length=50,
          a.event_title as event length=100,
          medal_type length=20
   from work.olympic_results as a
   left join work.olympic_hosts as c
      on a.slug_game = c.game_slug  
   where  a.medal_type is not null and c.game_season = 'Summer' and c.game_year >= 2000
   union
   select distinct 
          b.country_long as country_name length=50, 
          2024 as game_year,
          discipline length=50,
          event length=100,
          medal_type length=20
   from work.medals as b
   where b.medal_type is not null
   group by country_name, game_year, discipline
   order by country_name, game_year, discipline;
quit;


proc sql;
    create table hist_country_disc_with_gender as
    select distinct
        r.country_name as country_name length=50,
        h.game_year as game_year,
        r.discipline_title as discipline_title length=75,
        sum(case
            when (upcase(event_title) like '%WOMEN%' or upcase(event_title) like '%WOMAN%' 
                or upcase(event_title) like '%LADIES%' or upcase(discipline_title) like "%RHYTHMIC GYMNASTICS%") 
                then 1 
            else 0 end) as female_count,
        sum(case
            when (upcase(event_title) like '%MEN%' or upcase(event_title) like '%MAN%' 
                or upcase(event_title) like '%BASEBALL%') 
                and not (upcase(event_title) like '%WOMEN%' or upcase(event_title) like '%LADIES%')
                then 1 
            else 0 end) as male_count,
        sum(case
            when (upcase(event_title) like '%MIXED%' or upcase(discipline_title) like '%EQUESTRIAN%' 
                or upcase(event_title) like '%TEAM%' or upcase(event_title) like '%DUET%' 
                or upcase(event_title) like '%SOFTBALL%') 
                then 1 
            else 0 end) as mixed_count
    from 
        work.olympic_results r
    inner join 
        work.olympic_hosts h
    on 
        r.slug_game = h.game_slug
    where 
        h.game_year >= 2000 and h.game_season = "Summer"
    group by 
        r.country_name, h.game_year, r.discipline_title
    
    union all
    
    select  distinct
        upcase(country_long) as country_name length=50,
        2024 as game_year,
        disciplines as discipline_title length=75,
        sum(case
            when (upcase(events) like '%WOMEN%' or upcase(events) like '%WOMAN%' 
                or upcase(events) like '%LADIES%' or upcase(disciplines) like "%RHYTHMIC GYMNASTICS%") 
                then 1 
            else 0 end) as female_count,
        sum(case
            when (upcase(events) like '%MEN%' or upcase(events) like '%MAN%' 
                or upcase(events) like '%BASEBALL%') 
                and not (upcase(events) like '%WOMEN%' or upcase(events) like '%LADIES%')
                then 1 
            else 0 end) as male_count,
        sum(case
            when (upcase(events) like '%MIXED%' or upcase(disciplines) like '%EQUESTRIAN%' 
                or upcase(events) like '%TEAM%' or upcase(events) like '%DUET%' 
                or upcase(events) like '%SOFTBALL%') 
                then 1 
            else 0 end) as mixed_count
    from 
        work.athletes ath
    group by 
        upcase(country_long), disciplines;
quit;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;
%LET _SASPROGRAMFILEHOST=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
