REM PRGM from KOEFOED:  Geosounding Principals,1. P98-99.Elsevier,1979.
REM WENNER.OK for Otto KOEFOED.  8 Equal log spaces per decade.
REM WENNER filter coef 4/decade Ghosh,1971 and table 6.5.3,p95 of KOEFOED.
REM PRGM checked with Orellana and Mooney master tables and curves.
REM ...Interciencia Madrid 1966.Agreement Excellent...  CSC11/20/81
REM Converted to Microsoft QuickBasic 4/1994 William A. Richter - Edinboro University of PA
REM Graphics printer drivers and other auto functions 7/1994 William A. Richter.
REM Program debugged and input/output features modified 9/1994 /Ben R. Bullock

CLS 'clear screen for action
DEFDBL S
DEFINT P

DIM r(10), d(10), T(35), ar(40), x(40)
DIM res(40), cres(40), xter(40), rter(40)
DIM xs(40), cxs(40), xv(40)
DIM pbit(8), prnname$(10)

CONST BADFILENAME = 53

REM Ask User to specify path for wenner files
PRINT : PRINT
PRINT "Enter path for Wenner Files"
PRINT "For example  c:\wenner\ or a:\, etc. Be sure path ends with '\'"
PRINT
PRINT "Enter file path here:";
INPUT p$
IF p$ <> "" THEN path$ = p$

REM defaults follow
ON ERROR GOTO ErrorHandler:
GOSUB loaddef: 'load program definitions from disk.
GOSUB prninit: 'initialize printer codes for default printer.
pp = 0
cc = -1
sv = 1
indicat = 1
numlayers = 3
d(1) = 1
d(2) = 10
d(3) = 999999
r(1) = 150
r(2) = 50
r(3) = 600
lth = 1
a1 = .5
REM a1 is minimum electrode spacing in meters.
nspace = 18 'number of values program calculates based on model. Can be changed.
nterm = 18 'number of values to be entered at terminal. Can be changed by user.
f = EXP(LOG(10) / 8)

OPEN "LPT1:" FOR RANDOM AS #2
WIDTH #2, 255
GOSUB menu:
CLS
CLOSE #2
END

ErrorHandler:
   IF ERR = BADFILENAME THEN
      ' get another file name
      PRINT "File "; UCASE$(filename$); " not found."
      filename$ = ""
      WHILE filename$ = ""
         INPUT "Enter the file to load: ", filename$
      WEND
      RESUME
   ELSE
      ' some other error, so print message and abort
      PRINT "Unrecoverable error--"; ERR
      CLOSE #2
      GOSUB pause:
      SCREEN 0
      CLS
      END
   END IF

REM main menu for program
menu:
   fin = 1
   WHILE (fin)
      GOSUB menutxt:

      REM ***** Interactive Input *****
      PRINT
      PRINT "Input";
      INPUT z$
      z$ = UCASE$(z$)
      SELECT CASE z$
         CASE "NL"
            GOSUB layer:
         CASE "N"
            GOSUB nspace:
         CASE "A"
            GOSUB mspace:
         CASE "D"
            GOSUB sumsqr:
         CASE "L"
            GOSUB llayer:
         CASE "C"
            GOSUB aresist:
         CASE "P"
            GOSUB prnlayer:
         CASE "G"
            GOSUB graph:
         CASE "LD"
            GOSUB loaddata:
         CASE "SV"
            GOSUB savedata:
         CASE "PR"
            GOSUB prndata:
         CASE "PRTO"
            GOSUB prto:
         CASE "RD"
            GOSUB rdata:
         CASE "PD"
            GOSUB programdef:
         CASE "E" ' quit the program
            IF sv < 1 THEN
               INPUT "Do you want to save data entered at terminal (Y/N): ", a$
               IF UCASE$(a$) = "Y" THEN GOSUB savedata:
            END IF
            fin = 0
      END SELECT
   WEND
RETURN

REM menu text for main screen.
menutxt:
   CLS
   PRINT "   Apparent Resistivity for Multi-Earth Model:  Program WENNER20."
   PRINT "                 ******** MAIN MENU ********"
   PRINT
   PRINT "   Type 'NL' to specify # of layers in model. Current#=", numlayers
   PRINT "   Type 'L' to input data for L'th layer in model."
   PRINT "   Type 'P' to see a listing of current model parameters."
   PRINT "   Type 'C' to calculate resistivities using model parameters."
   PRINT "   Type 'A' to enter different min. spacing.  Current#=", a1
   PRINT "   Type 'N' for # of values (at 8/decade) that you want the"
   PRINT "   program to calculate using model param's.  Current#=", nspace
   PRINT
   PRINT "                       ***CONTROLS***"
   PRINT "   To enter field data from the terminal, type 'RD'"
   PRINT "   To print field data to the screen, type 'PRTO'"
   PRINT "   To print hardcopy of data and layer parameters, type 'PR'"
   PRINT "   To graph, type 'G'"
   PRINT "   To calculate sum of difference^2, type 'D'"
   PRINT "   To load data and layer parameters, type 'LD'"
   PRINT "   To save data and layer parameters, type 'SV'"
   PRINT "   To change program defaults, type 'PD'"
   PRINT "   ******To end program, type 'E'"
RETURN


REM ***** Input layer parameters
layer:
   ntemp = numlayers
   numlayers = 11
   WHILE numlayers > 10
      PRINT "= (max. number of layers = 10)";
      INPUT numlayers
      IF numlayers <= 0 THEN
         numlayers = ntemp
         RETURN
      END IF
   WEND
RETURN

REM find the number of values for spacing
nspace:
   PRINT "Number of values = ";
   INPUT nspace$
   IF nspace$ <> "" AND VAL(nspace$) > 0 THEN
      nspace = VAL(nspace$)
   END IF
RETURN

REM input the l'th layer
llayer:
   ltemp = lth
   lth = numlayers + 1
   CLS
   PRINT "These are current resistivity(R) and depth(D) values"
   PRINT "for each layer in the model."
   PRINT
   FOR m = 1 TO numlayers
      PRINT "R(" + LTRIM$(STR$(m)) + ") ="; r(m), "D(" + LTRIM$(STR$(m)) + ") ="; d(m)
   NEXT m
   WHILE (lth > numlayers)
      PRINT
      INPUT "Layer number you wish to change (1,2 etc.)", lth$
      IF lth$ = "" THEN
         lth = ltemp
         RETURN
      END IF
      lth = VAL(lth$)
      IF lth > numlayers THEN
         PRINT "***Layer number you have selected is out of range.***"
      END IF
      IF lth < 1 THEN
         lth = numlayers + 1
         PRINT "***Layer number you have selected is out of range.***"
      END IF
   WEND

   PRINT "Enter value for resisistivity R(L) = ";
   INPUT a$
   IF a$ <> "" THEN r(lth) = VAL(a$)
      IF r(lth) <= 0 THEN
         WHILE (r(lth) <= 0)
         PRINT "Value of R must be > 0"
         INPUT "Resistivity value ="; a$
         r(lth) = VAL(a$)
         WEND
      END IF
   PRINT
   PRINT "Enter value for depth D(L) = ";
   INPUT a$
   IF a$ <> "" THEN d(lth) = VAL(a$)
      IF d(lth) <= 0 THEN
         WHILE (d(lth) <= 0)
         PRINT "Value of D must be > 0"
         INPUT "Depth value ="; a$
         d(lth) = VAL(a$)
         WEND
      END IF
RETURN

REM input minimum spacing
mspace:
   PRINT " Minimum = ";
   INPUT a1$
   IF a1$ <> "" THEN a1 = VAL(a1$)
RETURN

REM print the layer parameters
prnlayer:
   REM Print layer parameters
   CLS
   PRINT
   PRINT "Here are the current layer parameters for your"; numlayers; "layer model."
   PRINT
   PRINT "Resistivity    Thickness"
   REM d(numlayers) = 999999
  
   REM first abcissa=6.7128. 822.87=exp(6.7128). 1/y is lambda in eq 3.4.3.
   REM where initial y is in stmt 1420.
   FOR i = 1 TO numlayers
      PRINT r(i), d(i)
   NEXT i
   PRINT
   PRINT "Minimum electrode spacing ="; a1; "meters."
   PRINT "Number of resistivity values program will calculate ="; nspace
   PRINT
   INPUT "Do you want a hardcopy of the layer parameters? (Y/N)  "; x$
   x$ = UCASE$(x$)
   IF x$ = "N" THEN RETURN
   LPRINT : LPRINT DATE$
   LPRINT
   LPRINT "Current layer parameters in model"
   LPRINT : LPRINT
   LPRINT "Resistivity    Thickness"
   LPRINT "  (ohm-m)         (m)"
   FOR i = 1 TO numlayers
      LPRINT r(i), d(i)
   NEXT i
   LPRINT : LPRINT
   LPRINT "Minimum electrode spacing="; a1
   LPRINT "Number of resistivity values program will calculate="; nspace
   LPRINT CHR$(12)
RETURN

REM apparent restivity prgm
aresist:
   PRINT
   PRINT
   cc = 1
   PRINT "Results of calculations using your"; numlayers; "- layer model."
   PRINT "3 - layer default model used if you have not entered new values."
   PRINT
   PRINT "   Spacing(m)", "    Resistivity (ohm-m)"
   ltemp = lth
   lth = 1 / LOG(10)
   a9 = INT(10 ^ (INT(lth * LOG(100 / a1))) + .0001)
   REM a9 gives significant figures for print.
  
   y = a1 / 61.504
   FOR j = 1 TO 26
      GOSUB tcalc:
      T(j) = b
      y = y * f
   NEXT j
   FOR m = 1 TO nspace
      GOSUB tcalc:
      T(27) = b
      y = y * f
      s = 6 * T(1) - 42 * T(3) + 193 * T(5) - 1029 * T(7)
      s = s + 5670 * T(9) - 17514 * T(11) + 12348 * T(13)
      s = s + 8415 * T(15) + 1380 * T(17) + 589 * T(19)
      s = s - 86 * T(21) + 91 * T(23) - 31 * T(25) + 8 * T(27)
      s = s / 10000
      ar(m) = s
      x(m) = a1 * f ^ (m - 1)
      FOR j = 1 TO 26
         T(j) = T(j + 1)
      NEXT j
      x(m) = INT(x(m) * a9) / a9
      ar(m) = INT(s * 10 + .5) / 10
      PRINT TAB(5); x(m); TAB(20); ar(m)
   NEXT m
   lth = ltemp
   GOSUB pause:
RETURN

REM file defaults menu
programdef:
   CLS
   PRINT "                         Current defaults"
   PRINT
   PRINT TAB(20); "(P)ath: "; path$
   PRINT TAB(20); "(C)aculate on data load: ";
   IF ldcalc THEN
      PRINT "ON"
   ELSE
      PRINT "OFF"
   END IF

   PRINT TAB(20); "(A)utoscaling: ";
   IF pascale THEN
      PRINT "ON"
   ELSE
      PRINT "OFF"
   END IF
   PRINT TAB(20); "(PR)inter type: "; prnname$(prntype)
   PRINT TAB(20); "(S)ave defaults"
   PRINT
   PRINT TAB(20);
   INPUT d$
   d$ = UCASE$(d$)
   SELECT CASE d$
      CASE "P"
         PRINT
         PRINT TAB(20); "Enter new file path:";
         INPUT p$
         IF p$ <> "" THEN path$ = p$
      CASE "C" ' toggle autocalculation on load on/off
         IF ldcalc = 0 THEN
            ldcalc = 1
         ELSE
            ldcalc = 0
         END IF

      CASE "A" ' toggle autoscaling on/off
         IF pascale = 0 THEN
            pascale = 1
         ELSE
            pascale = 0
         END IF
      CASE "PR"
         PRINT
         PRINT TAB(20); "select printer type:"
         FOR i = 1 TO prnmax
            PRINT TAB(25); STR$(i) + ". "; prnname$(i)
         NEXT i
         PRINT TAB(20);
         INPUT ptmp
         IF ptmp >= 1 AND ptmp <= prnmax THEN
            prntype = ptmp
            GOSUB prninit: 'reset printer codes to new type.
         END IF
      CASE "S"
         GOSUB savedef:
   END SELECT
RETURN


REM Graphics section
graph:
   Rmin = 9999999
   Rmax = 0
   pick = 1
   WHILE (pick)
      WHILE (Rmin >= Rmax)
         ind = 1
         CLS
         PRINT "Graph values entered from:"
         PRINT "   Terminal"
         PRINT "   Calculated by program"
         PRINT "   Both data sets"
         PRINT
         lpick = 1
         WHILE lpick
            INPUT "        Enter one of the following:  (term,calc,both)"; d$
            d$ = UCASE$(d$)
            SELECT CASE d$
              
               CASE "TERM"
                  lpick = 0
                  IF pp < 1 THEN
                     GOSUB valuewarn:
                     RETURN
                  END IF
              
               CASE "CALC"
                  lpick = 0
                  IF cc < 1 THEN
                     GOSUB calcwarn:
                     RETURN
                  END IF

               CASE "BOTH"
                 lpick = 0
                 IF pp < 1 THEN
                    GOSUB valuewarn:
                    RETURN
                 ELSE
                    IF cc < 1 THEN
                       GOSUB calcwarn:
                       RETURN
                    END IF
                 END IF
           
            END SELECT
         WEND
         Rmin = 0
         GOSUB autoscale:

      WEND
      SELECT CASE d$
         CASE "CALC"
            GOSUB graphcalc:
            pick = 0
         CASE "TERM"
            GOSUB graphterm:
            pick = 0
         CASE "BOTH"
            GOSUB graphboth:
            pick = 0
      END SELECT

   WEND
RETURN


REM create a screen graph of calculated data
makegraph:
   IF nboth < 1 THEN
      SCREEN 12 ' set screen up in 640 x 480 graphics mode
      WINDOW SCREEN (-105, 0)-(640, 720) 'offset screen for information
   END IF
   GOSUB plotgraph:
RETURN

plotgraph:
   hcolor = 15

REM Calculate scale factors for x and y axes.  sx and sy represent the
REM number of pixels/decade on the x and y axes, respectively.
   sx = 639 / (Xmax - Xmin)
   sy = 479 / (Rmax - Rmin)
   decx = INT(Xmax - Xmin)' number of full decades on x-axis.
   decy = INT(Rmax - Rmin)' number of full decades on y-axis.

REM Draw and label vertical grid lines.
IF lgraph THEN
   FOR j = 0 TO decx
      x = INT(sx * j) + 1
      LINE (x, 479)-(x, 1), hcolor
      LOCATE 21, INT((100 + x) * .1081 + .5)
      PRINT 10 ^ Xmin * 10 ^ j;
   NEXT j

REM Draw and label horizontal grid lines.
   FOR j = 0 TO decy
      y = 479 - INT(sy * j)
      LINE (1, y)-(639, y), hcolor
      LOCATE INT(y * .0417) + 1, 12 - LEN(STR$(10 ^ Rmin * 10 ^ decy))
      PRINT STR$(10 ^ Rmin * 10 ^ j);
   NEXT j
END IF

REM Draw resistivity curve(s).
DEF FNLB (b) = LOG(b) / LOG(10)
FOR m = 1 TO k - 1
   x1 = INT((FNLB(xv(m)) - Xmin) * sx)
   x2 = INT((FNLB(xv(m + 1)) - Xmin) * sx)
   y1 = 479 - INT((FNLB(res(m)) - Rmin) * sy)
   y2 = 479 - INT((FNLB(res(m + 1)) - Rmin) * sy)
   LINE (x1, y1)-(x2, y2), gcolor
NEXT m


REM Add hash marks to the x-axis.
   paloop = 1
   bxp = Xmin
   j = 2
WHILE paloop
   xhash = 10 ^ bxp * j
   x1 = INT((FNLB(xhash) - Xmin) * sx)
   j = j + 1
   IF x1 < 639 THEN
      LINE (x1, 479)-(x1, 469), hcolor
      ELSE paloop = 0
   END IF
   IF j > 9 THEN
      j = 2
      bxp = bxp + 1
   END IF
WEND


REM Add hash marks to the y-axis.
   paloop = 1
   bxp = Rmin
   j = 2
WHILE paloop
   yhash = 10 ^ bxp * j
   y1 = 479 - INT((FNLB(yhash) - Rmin) * sy)
   j = j + 1
   IF y1 > 0 THEN
      LINE (1, y1)-(10, y1), hcolor
      ELSE paloop = 0
   END IF
   IF j > 9 THEN
      j = 2
      bxp = bxp + 1
   END IF
WEND

   REM Print a legend for the graph.
   LOCATE legnpr, 10
   LINE (30, legnln)-(70, legnln), gcolor
   PRINT legend$
   IF nboth <> 0 THEN
      WHILE INKEY$ = "": WEND
      GOSUB hardcopy:
      SCREEN 0
   END IF
RETURN

hardcopy:
   LOCATE legnpr + 2, 10
   PRINT "Do you want a hard copy of the graph? (Y/N)";
   INPUT z$
   z$ = UCASE$(z$)
   IF z$ = "Y" THEN
      LOCATE legnpr + 2, 10
      PRINT "                                                   ";
      WINDOW SCREEN (0, 0)-(639, 479) ' set window back to default for printing
      PRINT
      PRINT TAB(10); "R minimum = "; Rmin; "    R maximum = "; Rmax
      PRINT TAB(10); "minimum electrode spacing = "; a1
      IF prntype < prnlaser THEN
         GOSUB prnhard:
      ELSE
         GOSUB prnlaser:
      END IF
   END IF
RETURN


REM Subroutine for hardcopy of data.
prndata:
   hfin = 1
   PRINT
   PRINT
   PRINT "Do you want a printout of data entered at the terminal or calculated"
   PRINT "by the program? (TERM,CALC,BOTH)  ";
   WHILE (hfin)
      INPUT d$
      IF d$ = "" THEN RETURN
      d$ = UCASE$(d$)
      SELECT CASE d$
         CASE "TERM"
            GOSUB getterm:
            IF rter(1) = 0 THEN
               GOSUB valuewarn:
               RETURN
            END IF
            hfin = 0
         CASE "CALC"
            GOSUB getcalc:
            IF ar(1) = 0 THEN
               GOSUB calcwarn:
               RETURN
            END IF
            hfin = 0
         CASE "BOTH"
            GOSUB getterm:
            IF rter(1) = 0 THEN
               GOSUB valuewarn:
               RETURN
            END IF
            GOSUB getcalc:
            IF ar(1) = 0 THEN
               GOSUB calcwarn:
               RETURN
            END IF
            hfin = 0
      END SELECT
   WEND
   LPRINT
   LPRINT
   LPRINT
   IF d$ = "BOTH" THEN
      LPRINT DATE$
      LPRINT
      LPRINT "Data entered from terminal and calculated by program."
      LPRINT
   ELSE
      IF ind = 1 THEN
         LPRINT DATE$
         LPRINT
         LPRINT "Data entered from terminal."
         LPRINT
      ELSE
         LPRINT DATE$
         LPRINT
         LPRINT "Data calculated by program."
         LPRINT
      END IF
   END IF
   IF d$ = "BOTH" THEN
      LPRINT "Electrode spacing    Resistivity     Resistivity"
      LPRINT "  (Meters)           (Ohm-Meters)    (Ohm-Meters)"
      LPRINT "                     (Terminal)      (Calculated)"
      LPRINT
      FOR m = 1 TO k
         LPRINT TAB(4); xs(m); TAB(24); res(m); TAB(40); cres(m)
      NEXT m
   ELSE
      LPRINT "Electrode spacing    Resistivity"
      LPRINT "  (Meters)           (Ohm-Meters)"
      LPRINT
      FOR m = 1 TO k
         LPRINT TAB(4); xs(m); TAB(24); res(m)
      NEXT m
   END IF
  
   REM Print Layer Parameters
   LPRINT : LPRINT : LPRINT : LPRINT
   LPRINT "Current Layer Parameters in Model"
   LPRINT
   LPRINT "Resistivity    Thickness"
   FOR i = 1 TO numlayers
      LPRINT r(i), d(i)
   NEXT i
   LPRINT : LPRINT
   LPRINT "Minimum electrode spacing="; a1
   LPRINT "Number of resistivity values program will calculate="; nspace
   LPRINT CHR$(12)
RETURN

REM Terminal data.
getterm:
   FOR m = 1 TO nterm
      res(m) = rter(m)
      xs(m) = xter(m)
   NEXT m
   ind = 1
   k = nterm
RETURN

REM Calculated values.
getcalc:
   FOR m = 1 TO nspace
      IF d$ = "CALC" THEN
         res(m) = ar(m)
         xs(m) = x(m)
      ELSE
         cres(m) = ar(m)
         cxs(m) = x(m)
      END IF
   NEXT m
   ind = 2
   k = nspace
RETURN

REM Change previously entered data.
rdata:
   CLS
   IF pp = 0 THEN
      GOSUB chgdata:
   ELSE
      PRINT
      PRINT "You have previously entered data."
      INPUT "Do you want to re-enter data? (Y/N)"; x$
      x$ = UCASE$(x$)
      IF x$ = "Y" THEN GOSUB chgdata:
   END IF
RETURN

REM Input of data values from the terminal.
chgdata:
   PRINT
   PRINT "If you plan to plot both data entered at terminal and that calculated"
   PRINT "by the program, the number of data points you enter at the terminal"
   PRINT "***MUST BE ="; nspace
   PRINT
   PRINT
   INPUT "Number of data points?  "; nterm
   PRINT "Give the electrode spacing, followed by the observed apparent resistivity."
   PRINT "The first data point should be the minimum electrode spacing, and"
   PRINT "the last point should be the maximum electrode spacing."
   PRINT
   PRINT "Data Point          Spacing(m)"
   REM Getting ready to calculate spacing values for current value of (a).
   cx = 1 / LOG(10)
   a9 = INT(10 ^ (INT(cx * LOG(100 / a1))) + .0001)
   FOR m = 1 TO nterm
      REM Calculate spacing value for current min. spacing value (a).
      xter(m) = a1 * f ^ (m - 1)
      xter(m) = INT(xter(m) * a9) / a9
      PRINT "  #"; m; TAB(23); xter(m); TAB(36); "   resistivity";
      INPUT rter(m)
   NEXT m
   pp = 1
   indicat = -1
   PRINT
   PRINT
   T$ = "Y"
   WHILE (T$ = "Y")
      PRINT "Do you want to make corrections for any data points (Y/N)";
      INPUT T$
      T$ = UCASE$(T$)
      IF T$ = "Y" THEN GOSUB corrdata:
   WEND
   sv = -1
RETURN

corrdata:
   change = 1
   WHILE change
      PRINT
      m = nterm + 1
      WHILE (m > nterm)
         INPUT "Number of the data point ="; a$
         IF a$ = "" THEN RETURN
         m = VAL(a$)
         IF m > nterm THEN PRINT "Value must be <="; STR$(nterm) + "."
         IF m <= 0 THEN
            PRINT "Value must be >0"
            m = nterm + 1
         END IF
      WEND
      PRINT "Enter new value of resistivity for data point #"; m;
      INPUT rter(m)
      PRINT
      INPUT "Any more corrections? (Y/N)"; T$
      T$ = UCASE$(T$)
      IF T$ = "N" THEN change = 0
      PRINT
   WEND
   sv = -1
RETURN

prto:
   REM Prints data entered at terminal for user's review.
   CLS
   PRINT "Here is the data you entered at the terminal."
   IF indicat = 1 THEN
     GOSUB valuewarn:
     RETURN
   END IF
   PRINT
   PRINT "Data Point        Spacing          Resistivity"
   FOR m = 1 TO nterm
   PRINT "    #"; m; TAB(20); xter(m); TAB(38); rter(m)
   NEXT m
   T$ = "Y"
   WHILE (T$ = "Y")
      PRINT "Do you want to make corrections for any data points (Y/N)";
      INPUT T$
      T$ = UCASE$(T$)
      IF T$ = "Y" THEN GOSUB corrdata:
   WEND
RETURN

prnlaser: 'Laser Driver.
   REM create bitmap of screen image
   PRINT #2, prnreset$; 'reset printer
   PRINT #2, prnLland$; ' set printer to landscape mode
   pclr = 0
   FOR i = 0 TO 479
      pline = -1
      pword$ = ""
      pchar = 1
      FOR j = 0 TO 639 STEP 8
         pbit(7) = POINT(j, i)
         pbit(6) = POINT(j + 1, i)
         pbit(5) = POINT(j + 2, i)
         pbit(4) = POINT(j + 3, i)
         pbit(3) = POINT(j + 4, i)
         pbit(2) = POINT(j + 5, i)
         pbit(1) = POINT(j + 6, i)
         pbit(0) = POINT(j + 7, i)
         GOSUB makebyte:
      NEXT j
     
      GOSUB setlgraph:
   NEXT i
   PRINT #2, prnFF$; ' formfeed page out
   PRINT #2, prnreset$; 'reset printer
RETURN

prnhard: 'Subroutine for hardcopy of the graph on a dot matrix printer.
   REM Create bitmap of screen image.
   LPRINT
   LPRINT
   LPRINT
   LPRINT
   pclr = 0
   FOR i = 48 TO 639 STEP 8
      pline = -1
      pword$ = ""
      pchar = 1
      FOR j = 479 TO 0 STEP -1
         pbit(7) = POINT(i, j)
         pbit(6) = POINT(i + 1, j)
         pbit(5) = POINT(i + 2, j)
         pbit(4) = POINT(i + 3, j)
         pbit(3) = POINT(i + 4, j)
         pbit(2) = POINT(i + 5, j)
         pbit(1) = POINT(i + 6, j)
         pbit(0) = POINT(i + 7, j)
         GOSUB makebyte:
      NEXT j
      GOSUB setpgraph:
   NEXT i
   PRINT #2, prnFF$ ' formfeed page out
   PRINT #2, prnreset$ 'reset printer
RETURN

makebyte:
   pbyte = 0
   FOR k = 0 TO 7
      IF pbit(k) < 1 THEN
         pbit(k) = 0
      ELSE
         IF pbit(k) = 5 THEN
            IF prntype < prnlaser THEN ' don't need this for a laser printer
               IF pline = -1 THEN pline = i
               IF pline = i THEN pclr = pclr + 1
            ELSE
               pclr = pclr + 1
            END IF
            IF pclr > 2 THEN
               pbit(k) = 0
               IF pclr > 3 AND pline = i AND prntype < prnlaser THEN
                  pclr = 0
               ELSE
                  IF pclr > 3 THEN pclr = 0
               END IF
            ELSE
               pbit(k) = 1
            END IF
         ELSE
            pbit(k) = 1
         END IF
      END IF
      pbyte = pbyte + 2 ^ k * pbit(k) 'create a character'
   NEXT k
   IF k1 = 1 THEN plin = 1
   pword$ = pword$ + CHR$(pbyte)
RETURN

setlgraph: 'Laser printer output
   GOSUB prninit: 'update parameters before print
   PRINT #2, prnLres$; ' set 75 DPI resolution
   PRINT #2, prnLsetxy$; 'set x,y location on page
   PRINT #2, prnLori$; ' print graphic in current page orientation
   PRINT #2, prnLstartG$; 'start raster graphics at current x location
   PRINT #2, prnLunenc$; ' tell printer to accept unencoded raster format
   PRINT #2, prnLgraph$ + pword$; 'transfer a row of raster graphics to printer
   PRINT #2, prnLendg$; 'end raster graphics mode
   pword$ = ""
RETURN

setpgraph: 'Dot matrix printer output.
   REM Set printer 128 graphics characters and set line spacing.
   PRINT #2, prnuni$;
   PRINT #2, prngraph$;
   
   PRINT #2, pword$;
   PRINT #2, prnline$;
   PRINT #2, prnreturn$;
   pword$ = ""
RETURN

prninit: 'Load printer codes.
   REM Set printer names.
   prnname$(1) = "IBM Proprinter II"
   prnname$(2) = "Epson FX"
   prnname$(3) = "Epson MX"
   prnname$(4) = "HP Laserjet"

   prnmax = 4 ' maximum printer type
   prnlaser = 4 ' first non-dot matrix printer

   SELECT CASE prntype
      CASE 1 'Printer codes for IBM Proprinter.
         prnuni$ = CHR$(27) + "U" + CHR$(1) 'unidirectional mode
         prngraph$ = CHR$(27) + "K" + CHR$(224) + CHR$(1) 'print a 640 character graphics line
         prnline$ = CHR$(27) + CHR$(74) + CHR$(24) 'set linefeed to 8 points.
         prnreturn$ = CHR$(13) 'return print head to right left side.
         prnreset$ = CHR$(27) + "@" 'reinitialize printer.
         prnFF$ = CHR$(12) 'form feed printer'

      CASE 2 'Printer codes for Epson FX printer.
         prnuni$ = CHR$(27) + "U" + CHR$(1) 'unidirectional mode.
         prngraph$ = CHR$(27) + "K" + CHR$(224) + CHR$(1) 'print a 640 character graphics line
         prnline$ = CHR$(27) + CHR$(74) + CHR$(24) 'set linefeed to 8 points.
         prnreturn$ = CHR$(13) 'return print head to right left side.
         prnreset$ = CHR$(27) + "@" + CHR$(27) + CHR$(74) + CHR$(18) + CHR$(27) + "<"'reinitialize printer.
         prnFF$ = CHR$(12) 'form feed printer'
     
      CASE 3 'Printer codes for Epson MX printer.
         prnuni$ = "" 'unidirectional mode not available.
         prngraph$ = CHR$(27) + "K" + CHR$(224) + CHR$(1) 'print a 640 character graphics line
         prnline$ = CHR$(27) + "3" + CHR$(18) + CHR$(10) 'set linefeed to 8 points.
         prnreturn$ = CHR$(27) + "@" 'return print head to right left side.
         prnreset$ = CHR$(27) + "@" 'reinitialize printer.
         prnFF$ = CHR$(12) 'form feed printer'
     
      CASE 4 'Printer codes for HP laserjet
         prnLland$ = CHR$(27) + CHR$(38) + CHR$(108) + CHR$(49) + CHR$(79) ' set printer to landscape mode
         prnLres$ = CHR$(27) + "*t75R" ' set 75 DPI resolution
         prnLsetxy$ = CHR$(27) + "*p0x" + STR$(i * 4) + "Y" 'set x,y location on page
         prnLori$ = CHR$(27) + "*r0F" ' print graphic in current page orientation
         prnLstartG$ = CHR$(27) + "*r1A" 'start raster graphics at current x location
         prnLunenc$ = CHR$(27) + "*b0M" ' tell printer to accept unencoded raster format
         prnLgraph$ = CHR$(27) + "*b" + STR$(LEN(pword$)) + "W" 'transfer a row of raster graphics to printer
         prnLendg$ = CHR$(27) + "*rB" 'end raster graphics mode
         prnreset$ = CHR$(27) + "E" 'reinitialize printer.
         prnFF$ = CHR$(12) 'form feed printer'
   END SELECT
RETURN

graphcalc: 'Calculate graphics.
      nboth = -1
      FOR m = 1 TO nspace
         res(m) = ar(m)
         xv(m) = x(m)
      NEXT m
      ind = ind + 1
      k = nspace
      gcolor = 5
      legnpr = 22
      legnln = 515
      legend$ = "CALC"
      panum = nspace
      lgraph = 1
      GOSUB makegraph:
RETURN

graphterm:
      nboth = -1
      FOR m = 1 TO nterm
         res(m) = rter(m)
         xv(m) = xter(m)
      NEXT m
      k = nterm
      gcolor = 10
      legnpr = 22
      legnln = 515
      legend$ = "TERM"
      lgraph = 1
      panum = nterm
      GOSUB makegraph:
RETURN

graphboth:
   IF x(1) <> xter(1) THEN
      PRINT "      Electrode spacing values in the two data sets do not agree."
      PRINT
         GOSUB pause:
         RETURN
   END IF
 
   IF nterm <> nspace THEN
      PRINT
      PRINT "WARNING!!"
      PRINT "Number of data points calculated by the program is not the"
      PRINT "same as the number entered earlier at the terminal. Cannot"
      PRINT "plot a graph of both data sets unless these numbers are same."
      PRINT
      PRINT "Type 'N' to specify number of resistivity values to be calculated."
      PRINT "Type 'C' to have program calculate N resistivity values."
      PRINT "Type 'RD' to enter field data at the terminal."
      PRINT
         GOSUB pause:
         RETURN
      END IF
     
      REM Continue plotting once all checks on terminal&calculated data are done.
      nboth = 0
      FOR m = 1 TO nspace
         res(m) = ar(m)
         xv(m) = x(m)
      NEXT m
      k = nterm
      ind = ind + 1
      gcolor = 5
      legnpr = 22
      legnln = 515
      legend$ = "CALC"
      lgraph = 1
      GOSUB makegraph:
        
      nboth = 1
      FOR m = 1 TO nterm
         res(m) = rter(m)
         xv(m) = xter(m)
      NEXT m
      k = nterm
      gcolor = 10
      legnpr = 23
      legnln = 540
      legend$ = "TERM"
      lgraph = 0
      GOSUB makegraph:
RETURN

REM This routine will autoscale the axes of the graph logarithmically.
autoscale:
   Rmin = 1000000
   Rmax = 0
   SELECT CASE d$ 'find the smallest and largest values in the data set(s)
      CASE "TERM"
         FOR m = 1 TO nterm
            IF rter(m) < Rmin THEN Rmin = rter(m)
            IF rter(m) > Rmax THEN Rmax = rter(m)
         NEXT m
         Xmin = xter(1): Xmax = xter(nterm)
          
      CASE "CALC"
         FOR m = 1 TO nspace
            IF ar(m) < Rmin THEN Rmin = ar(m)
            IF ar(m) > Rmax THEN Rmax = ar(m)
         NEXT m
         Xmin = x(1): Xmax = x(nspace)
     
      CASE "BOTH"
         FOR m = 1 TO nterm
            IF rter(m) < Rmin THEN Rmin = rter(m)
            IF rter(m) > Rmax THEN Rmax = rter(m)
         NEXT m
         FOR m = 1 TO nspace
            IF ar(m) < Rmin THEN Rmin = ar(m)
            IF ar(m) > Rmax THEN Rmax = ar(m)
         NEXT m
            Xmin = xter(1): Xmax = xter(nterm)
   END SELECT
   
   REM This section finds lowest/highest log values for graph axes.
   paloop = 1
   pa = -1
   WHILE (paloop) 'find lowest log value on y-axis.
      IF INT(10 ^ pa) > INT(Rmin) THEN
         Rmin = pa - 1
         paloop = 0
      END IF
      pa = pa + 1
   WEND
   Rmax = LOG(Rmax) / LOG(10) 'highest log value on y-axis.

   paloop = 1
   pa = -1
   WHILE (paloop) 'find lowest log value on x-axis.
      IF INT(10 ^ pa) > INT(Xmin) THEN
         Xmin = pa - 1
         paloop = 0
      END IF
      pa = pa + 1
   WEND
   Xmax = LOG(Xmax) / LOG(10) 'highest log value on x-axis.
RETURN

REM Warn user that you can't do this yet.
calcwarn:
   CLS
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT "        You are attempting to graph or print resistivity values"
   PRINT "                 that the program has not calculated."
   PRINT
   PRINT "             Type 'C' and then graph or print the values"
   PRINT
   PRINT
   GOSUB pause:
RETURN

valuewarn: 'Warn user about lack of data.
   CLS
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT "          You have not entered observed values of resistivity!"
   PRINT
   PRINT "                       Input 'RD' to enter data."
   PRINT
   GOSUB pause:
RETURN


REM ** Sum of squares of differences **
REM ** Observed vs. calculated data **
sumsqr:
   IF indicat > 0 THEN
      GOSUB valuewarn:
      RETURN
   END IF
   IF nterm <> nspace THEN
      CLS
      PRINT
      PRINT "            Number of calculated data points is not the same"
      PRINT "             as the number entered earlier at the terminal."
      PRINT
         GOSUB pause:
         RETURN
   END IF
  
   IF x(1) <> xter(1) THEN
      CLS
      PRINT
      PRINT "             Electrode spacing values in terminal data set do"
      PRINT "              not match those in the calculated data set."
      PRINT
         GOSUB pause:
         RETURN
   END IF
  
   IF cc = 1 THEN
      CLS
      GOSUB calcsum:
   ELSE
      GOSUB calcwarn:
   END IF
   GOSUB pause:
RETURN

calcsum:
   sum = 0
   FOR i = 1 TO nspace
      sum = sum + (ar(i) - rter(i)) ^ 2
   NEXT i
   PRINT
   PRINT
   PRINT "            Sum of squares of differences"
   PRINT "               (observed-calculated)^2"
   PRINT
   PRINT "                    Sum = "; INT(sum)
   PRINT
RETURN

REM Subroutine for t(j) calc.
tcalc:
   b = r(numlayers)
   FOR k = 1 TO numlayers - 1
      i = numlayers - k
      u = d(i) / y
      IF (5 - u) > 0 THEN
         e1 = EXP(u)
         e2 = (e1 - 1 / e1) / (e1 + 1 / e1)
         b = (b + e2 * r(i)) / (1 + e2 * b / r(i))
      ELSE
         b = r(i)
      END IF
   NEXT k
RETURN


savedata: 'Save data that has been entered at keyboard by user.
   IF pp < 1 THEN
      GOSUB valuewarn:
   ELSE
      INPUT "Enter file name (do NOT specify .3-letter extension): "; filename$
      OPEN path$ + filename$ + ".wen" FOR OUTPUT AS #1
      WRITE #1, nterm ' number of data points from terminal
      FOR m = 1 TO nterm 'data points
         WRITE #1, xter(m), rter(m)
      NEXT m

      WRITE #1, numlayers 'number of layers
      WRITE #1, lth
      FOR m = 1 TO numlayers 'layer data
         WRITE #1, r(m), d(m)
      NEXT m
      WRITE #1, a1 'minumum spacing
      WRITE #1, nspace  'spacing
      CLOSE #1
      sv = 1
   END IF
RETURN


REM Load field data that was previously input from keyboard by user.
loaddata:
   CLS
   IF sv < 1 THEN
      PRINT "You have previously entered field data at the terminal."
      INPUT "Do you want to save this data before loading a new set? (Y/N)", a$
      IF UCASE$(a$) = "Y" THEN GOSUB savedata:
   END IF
   SHELL "dir " + path$ + "*.wen/w"
   filename$ = ""
   WHILE filename$ = ""
      INPUT "Enter complete file name with .extension: ", filename$
   WEND
   OPEN path$ + filename$ FOR INPUT AS #1
   INPUT #1, nterm
   FOR m = 1 TO nterm 'number of data points
      INPUT #1, xter(m), rter(m) 'data points
   NEXT m

   INPUT #1, numlayers 'number of layers
   INPUT #1, lth  'current layer
   FOR m = 1 TO numlayers
      INPUT #1, r(m), d(m)'layer data
   NEXT m
   INPUT #1, a1   'minimum spacing
   INPUT #1, nspace    'spacing

   CLOSE #1
   sv = 1
   pp = 1
   indicat = -1
      CLS
      PRINT
      PRINT "You have just loaded a new data set of observed resistivity values"
      PRINT "and an associated"; numlayers; "-layer model."
      PRINT
      PRINT "Type 'P' at the main screen prompt to see model parameters."
      PRINT
      INPUT "Meanwhile, do you want to see this data set? (Y/N)", a$
         IF UCASE$(a$) = "Y" THEN GOSUB prto:
RETURN

loaddef: 'Load file defaults.
   OPEN path$ + "wenner.ini" FOR INPUT AS #1
   INPUT #1, pascale 'autoscaling function
   INPUT #1, ldcalc  'autocalculate on load
   INPUT #1, prntype 'default printer type
   CLOSE #1
   RETURN

savedef: 'Save file defaults
   OPEN path$ + "wenner.ini" FOR OUTPUT AS #1
   WRITE #1, pascale 'autoscaling funtion
   WRITE #1, ldcalc 'autocaculate on load
   WRITE #1, prntype 'default print type
   CLOSE #1
   RETURN

pause: 'Pause until a key is pressed.
   PRINT "                       Press any key to continue."
   DO
   LOOP WHILE INKEY$ = ""
RETURN
REM End of Program WENNER20
REM **********************************************************************




