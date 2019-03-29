REM PRGM from KOEFOED:Geosounding Principals,1. P98-99.Elsevier,1979
REM WENNER.OK for Otto KOEFOED.8 Equal log spaces per decade
REM WENNER filter coef 4/decade Ghosh,1971 and table 6.5.3,p95 of KOEFOED
REM PRGM checked with Orellana and Mooney master tables and curves
REM ...Interciencia Madrid 1966.Agreement Excellent.CSC11/20/81
REM Converted to Microsoft QuickBasic 4/1994 William A. Richter - Edinboro University of PA
REM Graphics printer drivers and other auto functions 7/1994 William A. Richter.

DEFDBL S
DEFINT P

DIM r(10), d(9), t(35), ar(40), x(40)
DIM res(40), cres(40), xter(40), rter(40)
DIM xs(40), cxs(40)
DIM pbit(8), prnname$(10)

CONST BADFILENAME = 53

REM defaults follow
ON ERROR GOTO ErrorHandler:
GOSUB loaddef: 'load program definitions from disk.
GOSUB prninit: 'initialize printer codes for default printer.
pp = 0
cc = -1
sv = 1
numlayers = 3
d(1) = 10
d(2) = 20
d(3) = 10
r(1) = 100
r(2) = 2.5
r(3) = 10
nspace = 25
lth = 1
a1 = .5

REM a1 is minimum spacing

f = EXP(LOG(10) / 8)
nspace = 18
nterm = 18

FOR i = 1 TO 18
   READ xter(i)
NEXT i

FOR i = 1 TO 18
   READ rter(i)
NEXT i
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

      REM ***** Interactive input
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
            GOSUB graph
         CASE "LD"
            GOSUB loaddata:
         CASE "SV"
            GOSUB savedata:
         CASE "PR"
            GOSUB prndata:
         CASE "RD"
            GOSUB rdata:
         CASE "PD"
            GOSUB programdef:
         CASE "E" ' quit the program
            IF sv < 1 THEN
               INPUT "Do you want to save your data (Y/N): ", a$
               IF UCASE$(a$) = "Y" THEN GOSUB savedata:
            END IF
            fin = 0
      END SELECT
   WEND
RETURN

REM menu text for main screen.
menutxt:
   CLS
   PRINT "   Apparent resistivity for multi Earth. Program Wenner."
   PRINT
   PRINT "   Type 'NL' to input number of layers.", numlayers
   PRINT "   type 'L' to input L'th layer.", lth
   PRINT
   PRINT "   Program asks for RHO and thickness"
   PRINT "   Type 'P' for layer parameters."
   PRINT "   Type 'C' to calculate."
   PRINT "   Type 'A' for minimum spacing.", a1
   PRINT "   Type 'N' for number of values"
   PRINT "   for spacing at 8/decade.     ", nspace
   PRINT
   PRINT "Controls:"
   PRINT "   To read resistivity and spacing values from the terminal, type 'RD'."
   PRINT "   To print, type 'PR'."
   PRINT "   To graph, type 'G'"
   PRINT "   To calculate sum of difference^2, type 'D'."
   PRINT "   To load data, type 'LD"
   PRINT "   To save data, type 'SV'"
   PRINT "   To change program defaults 'PD'"
   PRINT "   To end program, type 'E'"
RETURN


REM ***** Input layer parameters
layer:
   ntemp = numlayers
   numlayers = 11
   WHILE numlayers > 10
      PRINT "= (max=10)";
      INPUT numlayers$
      IF numlayer$ = "" THEN
         numlayers = ntemp
         RETURN
      END IF
   WEND
   sv = -1
RETURN

REM find the number of values for spacing
nspace:
   PRINT "=";
   INPUT nspace$
   IF nspace$ <> "" AND VAL(nspace$) > 0 THEN
      nspace = VAL(nspace$)
      sv = -1
   END IF
RETURN

REM input the l'th layer
llayer:
   ltemp = lth
   lth = numlayers + 1
   CLS
   PRINT "change which layer:"
   PRINT
   FOR m = 1 TO numlayers
      PRINT "R(" + LTRIM$(STR$(m)) + ") ="; r(m), "D(" + LTRIM$(STR$(m)) + ") ="; d(m)
   NEXT m
   WHILE (lth > numlayers)
      PRINT
      INPUT lth$
      IF lth$ = "" THEN
         lth = ltemp
         RETURN
      END IF
      lth = VAL(lth$)
      IF lth < 1 THEN lth = numlayers + 1
   WEND

   PRINT "R(L) = (" + LTRIM$(STR$(r(lth))) + ")";
   INPUT a$
   IF a$ <> "" THEN r(lth) = VAL(a$)
   PRINT "D(L) = (" + LTRIM$(STR$(d(lth))) + ")";
   INPUT a$
   IF a$ <> "" THEN d(lth) = VAL(a$)
   sv = -1
RETURN

REM input minimum spacing
mspace:
   PRINT " Minimum = ";
   INPUT a1$
   IF a1$ <> "" THEN a1 = VAL(a1$)
   sv = -1
RETURN

REM print the layer parameters
prnlayer:
   REM ***** print layer parameters
   PRINT
   PRINT "Resistivity    Thickness"
   REM d(numlayers) = 999999
  
   REM first abcissa=6.7128. 822.87=exp(6.7128). 1/y is lambda in eq 3.4.3.
   REM where initial y is in stmt 1420.
   FOR i = 1 TO numlayers
      PRINT r(i), d(i)
   NEXT i
   PRINT "    A="; a1; "  N="; nspace
   PRINT
   INPUT "Do you want a hardcopy of the layer parameters? (Y/N)  "; x$
   x$ = UCASE$(x$)
   IF x$ = "N" THEN RETURN
   LPRINT "Resistivity    Thickness"
   FOR i = 1 TO numlayers
      LPRINT r(i), d(i)
   NEXT i
   LPRINT "    A="; a1; "  N="; nspace
   LPRINT CHR$(12)
RETURN

REM apparent restivity prgm
aresist:
   PRINT
   PRINT
   cc = 1
   PRINT "Electrode"
   PRINT "spacing(M)", "resistivity; "
   ltemp = lth
   lth = 1 / LOG(10)
   a9 = INT(10 ^ (INT(lth * LOG(100 / a1))) + .0001)
   REM a9 gives significant figures for print.
  
   y = a1 / 61.504
   FOR j = 1 TO 26
      GOSUB tcalc:
      t(j) = b
      y = y * f
   NEXT j
   FOR m = 1 TO nspace
      GOSUB tcalc:
      t(27) = b
      y = y * f
      s = 6 * t(1) - 42 * t(3) + 193 * t(5) - 1029 * t(7)
      s = s + 5670 * t(9) - 17514 * t(11) + 12348 * t(13)
      s = s + 8415 * t(15) + 1380 * t(17) + 589 * t(19)
      s = s - 86 * t(21) + 91 * t(23) - 31 * t(25) + 8 * t(27)
      s = s / 10000
      ar(m) = s
      x(m) = a1 * f ^ (m - 1)
      FOR j = 1 TO 26
         t(j) = t(j + 1)
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
            INPUT "      (term,calc,both)"; d$
            d$ = UCASE$(d$)
            SELECT CASE d$
               CASE "TERM"
                  lpick = 0
               CASE "CALC"
                  lpick = 0
               CASE "BOTH"
                  lpick = 0
            END SELECT
         WEND
         PRINT
         PRINT
         PRINT
         Rmin = 0
         
         IF pascale THEN
            IF pascale THEN GOSUB autoscale:
         ELSE
            WHILE (Rmin < 1)
               PRINT "Input R minimum,R maximum=";
               INPUT Rmin, Rmax
            WEND
         END IF
      WEND
      PRINT "a minimum="; a1; " R minimum="; Rmin
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
   ltemp = lth
   lth = 1 / LOG(10)

   REM scale x and ar
   sx = 639 / k
   m9 = INT(k / 8)
   l1 = INT(LOG(Rmin) * lth)
   l2 = INT(LOG(Rmax) * lth + 1)
   m8 = ABS(l2 - l1)
   m8 = m8 - 1
   sy = 479 / m8
   yo = INT(sy * l1)
  
   IF lgraph THEN
      FOR j = 0 TO m9
         x = INT(sx * j * 8) + 1
         LINE (x, 479)-(x, 1), hcolor
         LOCATE 21, INT((100 + x) * .1081 + .5)
         REM LOCATE 21, INT(x * .125+1)
         PRINT 10 ^ j;
      NEXT j
  
      FOR j = 0 TO m8
         y = 479 - INT(j * sy)
         LINE (1, y)-(639, y), hcolor
         LOCATE INT(y * .0417) + 1, 12 - LEN(STR$(Rmax))
         REM LOCATE 21, INT(x * .125 + 1)
         PRINT STR$(Rmin * 10 ^ j);
      NEXT j
   END IF
   FOR m = 1 TO k - 1
      x = INT(sx * (m - 1)) + 1
      x1 = INT(sx * m) + 1
      yl = lth * LOG(res(m))
      y = 479 - INT(sy * yl) + yo
      IF y < 0 THEN y = 0
      IF y > 479 THEN y = 479
      yl = lth * LOG(res(m + 1))
      y1 = 479 - INT(sy * yl) + yo
      IF y1 < 0 THEN y1 = 0
      IF y1 > 479 THEN y1 = 479
      LINE (x, y)-(x1, y1), gcolor
   NEXT m

   REM print a legend for the graph
   LOCATE legnpr, 10
   LINE (30, legnln)-(70, legnln), gcolor
   PRINT legend$
   IF nboth <> 0 THEN
      WHILE INKEY$ = "": WEND
      GOSUB hardcopy:
      SCREEN 0
   END IF
   lth = ltemp
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


REM subroutine for hardcopy of data
prndata:
   hfin = 1
   PRINT
   PRINT
   PRINT "Do you want a printout of data entered at the terminal or calculated"
   PRINT "by the program? (TERM,CALC,BOTH)  ";
   WHILE (hfin)
      INPUT d$
      d$ = UCASE$(d$)
      SELECT CASE d$
         CASE "TERM"
            GOSUB getterm:
            hfin = 0
         CASE "CALC"
            GOSUB getcalc:
            hfin = 0
         CASE "BOTH"
            GOSUB getterm:
            GOSUB getcalc:
            hfin = 0
      END SELECT
   WEND
   LPRINT
   LPRINT
   LPRINT
   IF d$ = "BOTH" THEN
      LPRINT "Data entered from terminal and calculated by program"
      LPRINT
   ELSE
      IF ind = 1 THEN
         LPRINT "Data entered from terminal"
         LPRINT
      ELSE
         LPRINT "Data calculated by program"
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
   LPRINT CHR$(12)
RETURN

REM terminal data
getterm:
   FOR m = 1 TO nterm
      res(m) = rter(m)
      xs(m) = xter(m)
   NEXT m
   ind = 1
   k = nterm
RETURN

REM calculated values
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

REM change previously entered data
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

REM input of data values from the terminal
chgdata:
   PRINT "If you plan to plot both data entered at terminal and that calculated"
   PRINT "by the program, the number of data points you enter at the terminal"
   PRINT "should be = N (" + STR$(nspace) + ")."
   PRINT
   PRINT
   INPUT "Number of data points?  "; nterm
   PRINT "Give the electrode spacing, followed by the observed apparent resistivity."
   PRINT "The first data point should be the minimum electrode spacing, and"
   PRINT "the last point should be the maximum electrode spacing."
   FOR m = 1 TO nterm
      PRINT "x(" + STR$(m) + "),AR(" + STR$(m) + ") = ";
      INPUT xter(m), rter(m)
   NEXT m
   pp = 1
   PRINT
   PRINT
   t$ = "Y"
   WHILE (t$ = "Y")
      PRINT "Do you want to make corrections for any data points";
      INPUT t$
      t$ = UCASE$(t$)
      IF t$ = "Y" THEN GOSUB corrdata:
   WEND
   sv = -1
RETURN

corrdata:
   change = 1
   WHILE change
      PRINT
      PRINT
      m = nspace + 1
      WHILE (m > nspace)
         INPUT "Number of the data point ="; m
         IF m > nspace THEN PRINT "Value must be <="; STR$(nspace) + "."
      WEND
      PRINT
      PRINT
      PRINT "Enter new value for x("; m; ") and apparent; resistivity("; m; ")"
      INPUT "X,AR = "; xter(m), rter(m)
      PRINT
      PRINT
      INPUT "Any more corrections? (Y/N)"; t$
      IF t$ = "N" THEN change = 0
      PRINT
   WEND
RETURN

prnlaser: 'laser driver
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

REM subroutine for hardcopy of the graph on a dot matrix printer
prnhard:
   REM create bitmap of screen image
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

setlgraph: ' laser printer output
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

setpgraph: ' dot matrix printer output
   REM set printer 128 graphics characters and set line spacing
   PRINT #2, prnuni$;
   PRINT #2, prngraph$;
   
   PRINT #2, pword$;
   PRINT #2, prnline$;
   PRINT #2, prnreturn$;
   pword$ = ""
RETURN

REM load printer codes
prninit:
   REM set printer names.
   prnname$(1) = "IBM Proprinter II"
   prnname$(2) = "Epson FX"
   prnname$(3) = "Epson MX"
   prnname$(4) = "HP Laserjet"

   prnmax = 4 ' maximum printer type
   prnlaser = 4 ' first non-dot matrix printer

   SELECT CASE prntype
      CASE 1 'printer codes for IBM Proprinter.
         prnuni$ = CHR$(27) + "U" + CHR$(1) 'unidirectional mode
         prngraph$ = CHR$(27) + "K" + CHR$(224) + CHR$(1) 'print a 640 character graphics line
         prnline$ = CHR$(27) + CHR$(74) + CHR$(24) 'set linefeed to 8 points.
         prnreturn$ = CHR$(13) 'return print head to right left side.
         prnreset$ = CHR$(27) + "@" 'reinitialize printer.
         prnFF$ = CHR$(12) 'form feed printer'

      CASE 2 'printer codes for Epson FX printer.
         prnuni$ = CHR$(27) + "U" + CHR$(1) 'unidirectional mode.
         prngraph$ = CHR$(27) + "K" + CHR$(224) + CHR$(1) 'print a 640 character graphics line
         prnline$ = CHR$(27) + CHR$(74) + CHR$(24) 'set linefeed to 8 points.
         prnreturn$ = CHR$(13) 'return print head to right left side.
         prnreset$ = CHR$(27) + "@" + CHR$(27) + CHR$(74) + CHR$(18) + CHR$(27) + "<"'reinitialize printer.
         prnFF$ = CHR$(12) 'form feed printer'
     
      CASE 3 'printer codes for Epson MX printer.
         prnuni$ = "" 'unidirectional mode not available.
         prngraph$ = CHR$(27) + "K" + CHR$(224) + CHR$(1) 'print a 640 character graphics line
         prnline$ = CHR$(27) + "3" + CHR$(18) + CHR$(10) 'set linefeed to 8 points.
         prnreturn$ = CHR$(27) + "@" 'return print head to right left side.
         prnreset$ = CHR$(27) + "@" 'reinitialize printer.
         prnFF$ = CHR$(12) 'form feed printer'
     
      CASE 4 ' printer codes for HP laserjet
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

REM calculate graphics
graphcalc:
   IF cc < 1 THEN
      GOSUB calcwarn:
   ELSE
      nboth = -1
      FOR m = 1 TO nspace
         res(m) = ar(m)
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
   END IF
RETURN

graphterm:
   IF pp < 1 THEN
      GOSUB valuewarn:
   ELSE
      nboth = -1
      FOR m = 1 TO nterm
         res(m) = rter(m)
      NEXT m
      k = nterm
      gcolor = 10
      legnpr = 22
      legnln = 515
      legend$ = "TERM"
      lgraph = 1
      panum = nterm
      
      GOSUB makegraph:
   END IF
RETURN

graphboth:
   IF pp < 1 THEN
      GOSUB valuewarn:
   ELSE
      IF cc < 1 THEN
          GOSUB calcwarn:
      ELSE
         nboth = 0
         FOR m = 1 TO nspace
            res(m) = ar(m)
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
         NEXT m
         k = nterm
         gcolor = 10
         legnpr = 23
         legnln = 540
         legend$ = "TERM"
         lgraph = 0
         GOSUB makegraph:
     
      END IF
   END IF
RETURN


REM this routine will autoscale the horizontal axis of the graph logrithmically
autoscale:
   Rmin = 1000000
   Rmax = 0
   SELECT CASE d$ 'find the smallest and largest values in the data set(s)
      CASE "TERM"
         FOR m = 1 TO nterm
            IF rter(m) < Rmin THEN Rmin = rter(m)
            IF rter(m) > Rmax THEN Rmax = rter(m)
         NEXT m
      CASE "CALC"
         FOR m = 1 TO nspace
            IF ar(m) < Rmin THEN Rmin = ar(m)
            IF ar(m) > Rmax THEN Rmax = ar(m)
         NEXT m
      CASE "BOTH"
         FOR m = 1 TO nterm
            IF rter(m) < Rmin THEN Rmin = rter(m)
            IF rter(m) > Rmax THEN Rmax = rter(m)
         NEXT m
         FOR m = 1 TO nspace
            IF ar(m) < Rmin THEN Rmin = ar(m)
            IF ar(m) > Rmax THEN Rmax = ar(m)
         NEXT m
   END SELECT

   paloop = 1
   pa = 1
   WHILE (paloop) 'find lowest log scale value
      IF INT(10 ^ pa) > INT(Rmin) THEN
         Rmin = pa - 1
         paloop = 0
      END IF
      pa = pa + 1
   WEND
   paloop = 1
   pa = 1
   WHILE (paloop) 'find highest log scale value
      IF INT(Rmax) <= INT(10 ^ pa) + 1 THEN
         Rmax = pa
         paloop = 0
      END IF
      pa = pa + 1
   WEND
   IF Rmin = Rmax THEN Rmax = Rmax + 1 'if all numbers fall within the same scale
   Rmin = 10 ^ Rmin                'then increment Rmax (max) by 1
   Rmax = 10 ^ Rmax
RETURN

REM warn user that you can't do this yet
calcwarn:
   CLS
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT "you are attempting to graph resistivity values that the program"
   PRINT "has not calculated."
   PRINT
   PRINT
   PRINT "Input 'C' and then graph values"
   PRINT
   PRINT
   GOSUB pause:
RETURN

REM warn user about lack of data
valuewarn:
   CLS
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT
   PRINT "You have not entered observed values of electrode values and spacing."
   PRINT
   PRINT "Input 'RD' to enter data"
   PRINT
   GOSUB pause:
RETURN


REM ** Sum of squares of differences **
REM ** Observed vs. calculated data **
sumsqr:
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
   PRINT "Sum of squares of differences"
   PRINT "(observed-calculated)^2"
   PRINT
   PRINT "Sum = "; sum
RETURN

REM sub for t(j) calc
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

REM save data that has been input from keyboard by user.
savedata:
   IF pp < 1 THEN
      GOSUB valuewarn:
   ELSE
      INPUT "Enter file name: "; filename$
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


REM load data that was previously input from keyboard by user.
loaddata:
   CLS
   IF sv < 1 THEN
      INPUT "Do you want to save your data (Y/N): ", a$
      IF UCASE$(a$) = "Y" THEN GOSUB savedata:
   END IF
   SHELL "dir " + path$ + "*.wen/w"
   filename$ = ""
   WHILE filename$ = ""
      INPUT "Enter file name: ", filename$
   WEND
   OPEN path$ + filename$ + ".wen" FOR INPUT AS #1
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
   IF ldcalc THEN GOSUB aresist:
RETURN

REM load file defaults
loaddef:
   OPEN "wenner.ini" FOR INPUT AS #1
   INPUT #1, path$ 'current file path
   INPUT #1, pascale 'autoscaling function
   INPUT #1, ldcalc  'autocalculate on load
   INPUT #1, prntype 'default printer type
   CLOSE #1
   RETURN

REM save file defaults
savedef:
   OPEN "wenner.ini" FOR OUTPUT AS #1
   WRITE #1, path$ 'current file path
   WRITE #1, pascale 'autoscaling funtion
   WRITE #1, ldcalc 'autocaculate on load
   WRITE #1, prntype 'default print type
   CLOSE #1
   RETURN

REM pause until a key is pressed
pause:
   PRINT
   PRINT "    Press any key to continue."
   DO
   LOOP WHILE INKEY$ = ""
RETURN

REM **Electrode spacing data**
DATA .5,.66,.88,1.18,1.58,2.1,2.81,3.74,5,6.66,8.89,11.85
DATA 15.81,21.08,28.11,37.49,50,66.67

REM **Appararent resistivity data**
DATA 390,440,510,570,640,700,745,760,750,680,570,450
DATA 340,250,190,160,160,180


