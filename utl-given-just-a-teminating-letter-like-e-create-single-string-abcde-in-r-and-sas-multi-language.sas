%let pgm=utl-given-just-a-teminating-letter-like-e-create-single-string-abcde-in-r-and-sas-multi-language;

%stop_submission;

Given just a teminating letter like e create a single string abcde in r and sas multi language

github
https://tinyurl.com/ynuzyr9x
https://github.com/rogerjdeangelis/utl-given-just-a-teminating-letter-like-e-create-single-string-abcde-in-r-and-sas-multi-language

stackoverflow r
https://tinyurl.com/ysuds4y6
https://stackoverflow.com/questions/79193995/r-how-to-parse-a-vector-of-values-with-three-rows-then-the-parsed-output-shoul


   SOLUTIONS (may need to adjust for cjanges in collating sequences in other languages)

      1 SAS datastep

      2 r for loops

      3 r not run
        input_string <-
          c("S0027A-E", "S0028A-D", "S0029A-C")

        seqChar <- function(a, b) { lett <- LETTERS
          lett[which(lett == a):which(lett == b)] }

        prefix <- sub("(.*)\\D-\\D$", "\\1", input_string)
        library(data.table)

        data.table(input_string)[, .(parsed_strings =
          lapply(strsplit(
           sub(".*(\\D)-","\\1",input_string),""),\(x)
            paste0(prefix, seqChar(x[1], x[2]))))
            ,by = input_string]


/*               _     _
 _ __  _ __ ___ | |__ | | ___ _ __ ___
| `_ \| `__/ _ \| `_ \| |/ _ \ `_ ` _ \
| |_) | | | (_) | |_) | |  __/ | | | | |
| .__/|_|  \___/|_.__/|_|\___|_| |_| |_|
|_|
*/

/**************************************************************************************************************************/
/*                         |                                                |                                             */
/*         INPUT           |         PROCESS                                |           OUTPUT                            */
/*                         |                                                |                                             */
/*                         |                                                |                                             */
/*  SD1.HAVE total obs=3   | Covert letters on the end of                   |                                             */
/*                         | the have LST column to the                     |                                             */
/*  Obs      LST           | position in windows collating                  |                                             */
/*                         | sequence ie S0027A-E, E=69 and                 |                                             */
/*  1     S0027A-E         | A=64. Loop from 64 to 69                       |                                             */
/*  2     S0028A-D         | Converting the positions back                  |                                             */
/*  3     S0029A-C         | letters and concatenate                        |                                             */
/*                         |                                                |                                             */
/*  options                |                                                |                                             */
/*   validvarname=upcase;  | ---------------------------------------        |                                             */
/*  libname sd1 "d:/sd1";  |                                                |                                             */
/*  data sd1.have;         | SAS DATASTEP  (CAN BE SIPLIFIED)               |                                             */
/*    input lst$;          | ================================               |                                             */
/*  cards4;                |                                                |                                             */
/*  S0027A-E               | data want;                                     |  LIST                                       */
/*  S0028A-D               |   length list $200;                            |                                             */
/*  S0029A-C               |   set have;                                    |  S0027E,S0027D,S0027C,S0027B,S0027A         */
/*  ;;;;                   |   to=rank(substr(lst,8,1)) - rank('A');        |  S0028D,S0028C,S0028B,S0028A                */
/*  run;quit;              |   do i =0 to to;                               |  S0029C,S0029B,S0029A                       */
/*                         |     str=cats(substr(lst,1,5)                   |                                             */
/*                         |      ,byte(i+rank('A')));                      |                                             */
/*                         |     list=catx(',',str,list);                   |                                             */
/*                         |   end;                                         |                                             */
/*                         |   put list;                                    |                                             */
/*                         | run;quit;                                      |                                             */
/*                         |                                                |                                             */
/*                         | -------------------------------------------    |                                             */
/*                         |                                                |                                             */
/*                         |  BASE R FOR LOOP (basically the same as SAS    |  R                                          */
/*                         |                                                |                                    LIS      */
/*                         |                                                |  1  S0027A S0027B S0027C S0027D S0027E      */
/*                         |  want<-data.frame(lis=character())             |  2         S0028A S0028B S0028C S0028D      */
/*                         |   lis<-""                                      |  3                S0029A S0029B S0029C      */
/*                         |   for ( r in 1:nrow(have) ) {                  |                                             */
/*                         |     to<-utf8ToInt(                             |  SAS                                        */
/*                         |       substring(have[r,1],8,8))-utf8ToInt('A') |                                             */
/*                         |     for ( i in 0:to ) {                        |  ROWNAMES LIS                               */
/*                         |      str=paste0(substring(have[r,1],1,5)       |                                             */
/*                         |        ,intToUtf8(i+utf8ToInt('A')));          |      1  S0027A S0027B S0027C S0027D S0027E  */
/*                         |      lis<-paste(lis,str,sep=" ")               |      2  S0028A S0028B S0028C S0028D         */
/*                         |     }                                          |      3  S0029A S0029B S0029C                */
/*                         |     print(lis)                                 |                                             */
/*                         |     want<-rbind(want,lis)                      |                                             */
/*                         |     lis=""                                     |                                             */
/*                         |   }                                            |                                             */
/*                         |                                                |                                             */
/*                         |      R                   SAS                   |                                             */
/*                         |      =                   ===                   |                                             */
/*                         |                                                |                                             */
/*                         | utf8ToInt "A" to 64       RANK                 |                                             */
/*                         | intToUtf8 y4 to positiom  BYTE                 |                                             */
/*                         |                                                |                                             */
/*                         | substring start stop      substr( start length |                                             */
/*                         |                                                |                                             */
/*                         | IN SQL SQLLITE substr is same as SAS substr    |                                             */
/*                         |                                                |                                             */
/*                         |                                                |                                             */
/**************************************************************************************************************************/

/*                   _
(_)_ __  _ __  _   _| |_
| | `_ \| `_ \| | | | __|
| | | | | |_) | |_| | |_
|_|_| |_| .__/ \__,_|\__|
        |_|
*/

 validvarname=upcase;
libname sd1 "d:/sd1";
data sd1.have;
  input lst$;
cards4;
S0027A-E
S0028A-D
S0029A-C
;;;;
run;quit;

/**************************************************************************************************************************/
/*                                                                                                                        */
/* bs      LST                                                                                                            */
/*                                                                                                                        */
/* 1     S0027A-E                                                                                                         */
/* 2     S0028A-D                                                                                                         */
/* 3     S0029A-C                                                                                                         */
/*                                                                                                                        */
/**************************************************************************************************************************/

/*                       _       _            _
/ |  ___  __ _ ___    __| | __ _| |_ __ _ ___| |_ ___ _ __
| | / __|/ _` / __|  / _` |/ _` | __/ _` / __| __/ _ \ `_ \
| | \__ \ (_| \__ \ | (_| | (_| | || (_| \__ \ ||  __/ |_) |
|_| |___/\__,_|___/  \__,_|\__,_|\__\__,_|___/\__\___| .__/
                                                     |_|
*/

data want;
  length list $200;
  set have;
  to=rank(substr(lst,8,1)) - rank('A');
  do i =0 to to;
    str=cats(substr(lst,1,5),byte(i+rank('A')));
    list=catx(',',str,list);
  end;
  put list;
run;quit;

/**************************************************************************************************************************/
/*                                                                                                                        */
/*  WANT total obs=3                                                                                                      */
/*                                                                                                                        */
/*   LIST                                    LST                                                                          */
/*                                                                                                                        */
/*   S0027E,S0027D,S0027C,S0027B,S0027A    S0027A-E                                                                       */
/*   S0028D,S0028C,S0028B,S0028A           S0028A-D                                                                       */
/*   S0029C,S0029B,S0029A                  S0029A-C                                                                       */
/*                                                                                                                        */
/**************************************************************************************************************************/

/*___    _
|___ \  | |__   __ _ ___  ___   _ __
  __) | | `_ \ / _` / __|/ _ \ | `__|
 / __/  | |_) | (_| \__ \  __/ | |
|_____| |_.__/ \__,_|___/\___| |_|

*/

proc datasets lib=sd1 nolist nodetails;
 delete want;
run;quit;


%utl_rbeginx;
parmcards4;
library(haven)
source("c:/oto/fn_tosas9x.R")
have<-read_sas("d:/sd1/have.sas7bdat")
want<-data.frame(lis=character())
 lis<-""
 for ( r in 1:nrow(have) ) {
   to<-utf8ToInt(
     substring(have[r,1],8,8))-utf8ToInt('A')
   for ( i in 0:to ) {
    str=paste0(substring(have[r,1],1,5)
      ,intToUtf8(i+utf8ToInt('A')));
    lis<-paste(lis,str,sep=" ")
   }
   print(lis)
   want<-rbind(want,lis)
   lis=""
 }
colnames(want)<-"LIS"
want
fn_tosas9x(
      inp    = want
     ,outlib ="d:/sd1/"
     ,outdsn ="want"
     )
;;;;
%utl_rendx;

proc print data=sd1.want;
run;quit;

/**************************************************************************************************************************/
/*                                         |                                                                              */
/*  R                                      |   SAS                                                                        */
/*                                         |                                                                              */
/*                                    LIS  |   ROWNAMES    LIS                                                            */
/*                                         |                                                                              */
/*  1  S0027A S0027B S0027C S0027D S0027E  |       1       S0027A S0027B S0027C S0027D S0027E                             */
/*  2         S0028A S0028B S0028C S0028D  |       2       S0028A S0028B S0028C S0028D                                    */
/*  3                S0029A S0029B S0029C  |       3       S0029A S0029B S0029C                                           */
/*                                         |                                                                              */
/*                                         |                                                                              */
/**************************************************************************************************************************/

/*              _
  ___ _ __   __| |
 / _ \ `_ \ / _` |
|  __/ | | | (_| |
 \___|_| |_|\__,_|

*/
