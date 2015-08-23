'QBASIC GORILLAS 2.2
'Version 1.0 (c)1990 Microsoft Corp and/or IBM Corp
'Version 2.2 (c)1997-2007 Daniel Beardsmore
'See http://telcontar.net/Misc/Gorillas/ for more information

'Set default data type to integer for faster game play
DEFINT A-Z

'Sub Declarations
DECLARE SUB RestReal (t#)
DECLARE SUB AlertSnd ()
DECLARE SUB LoadSettings ()
DECLARE SUB Center (Row, Text$)
DECLARE SUB DoBeep ()
DECLARE SUB DoExplosion (x#, y#)
DECLARE SUB DoSun (Mouth)
DECLARE SUB DrawBan (xc#, yc#, r, bc)
DECLARE SUB DrawGorilla (x, y, arms)
DECLARE SUB ExplodeGorilla (x#, y#, PlayerHit)
DECLARE SUB Extro ()
DECLARE SUB GetInputs (Player$(), NumGames, P)
DECLARE SUB GorillaIntro (Player$(), cIntro)
DECLARE SUB Intro ()
DECLARE SUB MakeCityScape (BCoor() AS ANY)
DECLARE SUB PlaceGorillas (BCoor() AS ANY)
DECLARE SUB Rest (t#)
DECLARE SUB SetScreen ()
DECLARE SUB ShowPrompts (fieldNum AS INTEGER)
DECLARE SUB Slidy ()
DECLARE SUB SparklePause (opt AS INTEGER)
DECLARE SUB Stats (Wins(), name$(), Ban!(), P, abortYN)
DECLARE SUB VictoryDance (Player)

DECLARE FUNCTION CalcDelay# ()
DECLARE FUNCTION DoShot (Player$(), PlayerNum, x, y, turn, othX, othY)
DECLARE FUNCTION Get$ (Row, Col, Prev$, Typ, Max, Esc)
DECLARE FUNCTION PlayGame (Player$(), NumGames, P)
DECLARE FUNCTION PlotShot (StartX, StartY, angle#, velocity, PlayerNum, othX, othY)
DECLARE FUNCTION Scl (N!)
DECLARE FUNCTION WhereX (num)
DECLARE FUNCTION WhereY (num)

'Make all arrays Dynamic
'$DYNAMIC

' User-Defined TYPEs

TYPE settings
  useSound AS INTEGER
  useOldExplosions AS INTEGER
  newExplosionRadius AS INTEGER
  useSlidingText AS INTEGER
  defaultGravity AS INTEGER
  defaultRoundQty AS INTEGER
  showIntro AS INTEGER
  forceCGA AS INTEGER
END TYPE

TYPE XYPoint
  XCoor AS INTEGER
  YCoor AS INTEGER
END TYPE

TYPE PlayerData
  PNam AS STRING * 17
  Rounds AS INTEGER
  Won AS INTEGER
  Accu AS SINGLE
END TYPE

' Constants
CONST NPLAYERS = 20
CONST TRUE = -1
CONST FALSE = NOT TRUE
CONST HITSELF = 1
CONST BACKATTR = 0
CONST OBJECTCOLOR = 1
CONST WINDOWCOLOR = 14
CONST SUNHAPPY = FALSE
CONST SUNSHOCK = TRUE
CONST RIGHTUP = 1
CONST LEFTUP = 2
CONST ARMSDOWN = 3

' Global Variables
DIM SHARED GSettings AS settings

DIM SHARED lastErrCode

DIM SHARED SLIDECONST AS LONG

DIM SHARED GorillaX(1 TO 2)  'Location of the two gorillas
DIM SHARED GorillaY(1 TO 2)
DIM SHARED LastBuilding

DIM SHARED pi#
DIM SHARED LBan&(x), RBan&(x), UBan&(x), DBan&(x) 'Graphical picture of banana
DIM SHARED GorD&(120)        'Graphical picture of Gorilla arms down
DIM SHARED GorL&(120)        'Gorilla left arm raised
DIM SHARED GorR&(120)        'Gorilla right arm raised

DIM SHARED Gravity
DIM SHARED Wind
DIM SHARED GLeftAngle#
DIM SHARED GRightAngle#
DIM SHARED GLeftVeloc
DIM SHARED GRightVeloc

'Screen Mode Variables
DIM SHARED ScrHeight
DIM SHARED ScrWidth
DIM SHARED Mode
DIM SHARED MaxCol

' Screen Color Variables
DIM SHARED ExplosionColor
DIM SHARED SUNATTR
DIM SHARED BackColor

DIM SHARED SunHt
DIM SHARED GHeight
DIM SHARED MachSpeed AS DOUBLE

DIM SHARED PDefs(1 TO 2)
DIM Player$(1 TO 2)
DIM SHARED PDat(1 TO NPLAYERS) AS PlayerData
DIM SHARED GamePlayedYN

DIM SHARED DoesFileExist

DIM NumGames

 ' Load settings before initVars so we can look for forceCGA
 LoadSettings

 ' Check for league table file, and load table entries

 DoesFileExist = 1
 ON ERROR GOTO IsThereNoFile
 OPEN "Gorillas.lge" FOR INPUT AS #1
 ON ERROR GOTO CorruptFile
 IF DoesFileExist = 1 THEN
  INPUT #1, count
  FOR l = 1 TO count
   INPUT #1, PDat(l).PNam, PDat(l).Rounds, PDat(l).Won, PDat(l).Accu
  NEXT
  CLOSE #1
  ON ERROR GOTO 0
 ELSE
  count = 0
 END IF

 DEF FNRan (x) = INT(RND(1) * x) + 1
 DEF SEG = 0                         ' Set NumLock to ON
 KeyFlags = PEEK(1047)
 IF (KeyFlags AND 32) = 0 THEN
  POKE 1047, KeyFlags OR 32
 END IF
 DEF SEG

 ' Initialisation and sliding text speed calculation

 GOSUB InitVars
 MachSpeed = CalcDelay
 IF MachSpeed < 1000 THEN
  SLIDECONST = (4 * MachSpeed) - 1250
  IF SLIDECONST < 0 THEN SLIDECONST = 0
 ELSE
  SLIDECONST = 2.929 * MachSpeed
 END IF

 ' Program outline
 Gravity = GSettings.defaultGravity
 NumGames = GSettings.defaultRoundQty
 IF Mode = 1 THEN
  REM CGA needs a half-size explosion radius
  GSettings.newExplosionRadius = GSettings.newExplosionRadius \ 2
 END IF

 ' Init screen
 SCREEN 0
 WIDTH 80, 25
 MaxCol = 80
 COLOR 15, 0
 CLS

 GamePlayed = 0
 IF GSettings.showIntro THEN Intro
 more = 1: DO
  GetInputs Player$(), NumGames, count
  GorillaIntro Player$(), DoesFileExist
  more = PlayGame(Player$(), NumGames, count)
 LOOP UNTIL more = 0
 Extro

 COLOR 7: CLS ' Else QBasic crashes here! lol

 DEF SEG = 0                         ' Restore NumLock state
 POKE 1047, KeyFlags
 DEF SEG
 SYSTEM

' Banana sprite definitions

CGABanana:
 'BananaLeft
 DATA 327686, -252645316, 60
 'BananaDown
 DATA 196618, -1057030081, 49344
 'BananaUp
 DATA 196618, -1056980800, 63
 'BananaRight
 DATA 327686,  1010580720, 240

EGABanana:
 'BananaLeft
 DATA 458758,202116096,471604224,943208448,943208448,943208448,471604224,202116096,0
 'BananaDown
 DATA 262153, -2134835200, -2134802239, -2130771968, -2130738945,8323072, 8323199, 4063232, 4063294
 'BananaUp
 DATA 262153, 4063232, 4063294, 8323072, 8323199, -2130771968, -2130738945, -2134835200,-2134802239
 'BananaRight
 DATA 458758, -1061109760, -522133504, 1886416896, 1886416896, 1886416896,-522133504,-1061109760,0

' Initialise graphics mode and sprites

InitVars:
 pi# = 4 * ATN(1#)

 IF GSettings.forceCGA THEN
  Mode = 1
 ELSE
  ' Select best graphics mode
  ON ERROR GOTO ScreenModeError
  Mode = 9
  SCREEN Mode
  ON ERROR GOTO PaletteError
  IF Mode = 9 THEN PALETTE 4, 0   'Check for 64K EGA
 END IF

 IF Mode = 9 THEN
  ScrWidth = 640
  ScrHeight = 350
  GHeight = 25
  SUNATTR = 3
  RESTORE EGABanana
  REDIM LBan&(8), RBan&(8), UBan&(8), DBan&(8)

  FOR i = 0 TO 8
   READ LBan&(i)
  NEXT i
  FOR i = 0 TO 8
   READ DBan&(i)
  NEXT i
  FOR i = 0 TO 8
   READ UBan&(i)
  NEXT i
  FOR i = 0 TO 8
   READ RBan&(i)
  NEXT i

  SunHt = 43
 ELSE
  ScrWidth = 320
  ScrHeight = 200
  GHeight = 12
  SUNATTR = 3
  RESTORE CGABanana
  REDIM LBan&(2), RBan&(2), UBan&(2), DBan&(2)
  REDIM GorL&(20), GorD&(20), GorR&(20)

  FOR i = 0 TO 2
   READ LBan&(i)
  NEXT i
  FOR i = 0 TO 2
   READ DBan&(i)
  NEXT i
  FOR i = 0 TO 2
   READ UBan&(i)
  NEXT i
  FOR i = 0 TO 2
   READ RBan&(i)
  NEXT i

  MachSpeed = MachSpeed * 1.3
  SunHt = 20
 END IF
RETURN

FuckOff:
 lastErrCode = ERR
 RESUME NEXT

ScreenModeError:
  IF Mode = 1 THEN
    CLS
    LOCATE 10, 5
    PRINT "Sorry, you must have CGA, EGA color or VGA graphics to play Gorillas"
    PRINT
    SYSTEM
  ELSE
    Mode = 1
    RESUME
  END IF

PaletteError:
  Mode = 1            '64K EGA cards will run in CGA mode.
  RESUME NEXT

IsThereNoFile:
 DoesFileExist = 0
RESUME NEXT

NoSaveStats:
 COLOR 7: CLS
 COLOR 12: PRINT "An error occurred trying to save the stats file GORILLAS.LGE"
 PRINT "The statistics have not been saved.": COLOR 7: PRINT
 CLOSE
SYSTEM

CorruptFile:
 PRINT
 BEEP
 COLOR 12: PRINT "An error occurred while attempting to read data from the league"
 PRINT "table file, GORILLAS.LGE. Fix it, get it fixed, or delete it. Simple."
 COLOR 7: PRINT
SYSTEM

' Sliding text data store

SlidyText:
DATA 5
DATA "      Q B a s i c  G O R I L L A S  v2.2",15,1,4
DATA "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ",7,-1,5
DATA "DELUXE EDITION",15,1,6
DATA "Original program (c)1990 Microsoft Corporation",3,1,10
DATA "Gorillas Deluxe (c)1997-2007 Daniel Beardsmore",2,-1,12
DATA 10
DATA "INSTRUCTIONS",9,1,8
DATA "Your mission is to hit your opponent with an exploding",11,1,10
DATA "banana by varying the angle and power of your throw, taking",11,-1,11
DATA "into account wind speed, gravity, and the city skyline.",11,1,12
DATA "The wind speed is shown by a directional arrow at the bottom",11,-1,14
DATA "of the playing field, its length relative to its strength.",11,1,15
DATA "Zero degrees is horizontal, towards your opponent, with 90 degrees",11,-1,16
DATA "being vertically upwards, and so on. Angles can be from 0 to",11,1,17
DATA "360 degrees and velocity can range from 1 to 200.",11,-1,18
DATA "Press any key to continue...",15,1,20
PartingMessage:
DATA 1
DATA "Thank you for playing Gorillas!",11,1,8
'Next number is the number of final phrases
DATA 5
DATA 1,"May the Schwarz be with you!",14,-1,14
DATA 1,"Live long and prosper.",14,-1,14
DATA 1,"Goodbye!",14,-1,14
DATA 1,"So long!",14,-1,14
DATA 1,"Adios!",14,-1,14
Ready:
DATA 1,"Prepare for battle!",12,1,1
Setup:
DATA 1,"Game Setup",14,-1,1
GameOver:
DATA 1,"Game Over!",14,-1,3
Aborted:
DATA 1,"Game aborted",12,-1,3
NowWhat:
DATA 1,"Now What?",14,1,1

VectorData:
DATA 39
DATA 0.582,0.988, 0.608,0.850, 0.663,0.788, 0.738,0.800
DATA 0.863,0.838, 0.813,0.713, 0.819,0.650, 0.875,0.588
DATA 1.000,0.563, 0.850,0.450, 0.825,0.400, 0.830,0.340
DATA 0.925,0.238, 0.775,0.243, 0.694,0.225, 0.650,0.188, 0.630,0.105
DATA 0.625,0.025, 0.535,0.150, 0.475,0.175, 0.425,0.150
DATA 0.325,0.044, 0.325,0.150, 0.315,0.208, 0.288,0.250, 0.225,0.275
DATA 0.053,0.288, 0.150,0.392, 0.175,0.463, 0.144,0.525
DATA 0.025,0.638, 0.163,0.650, 0.225,0.693, 0.250,0.775
DATA 0.225,0.905, 0.360,0.825, 0.450,0.823, 0.525,0.863
DATA 0.582,0.988

REM $STATIC
SUB AlertSnd
 IF GSettings.useSound THEN PLAY ">>B10<<"
END SUB

'CalcDelay:
'  Checks speed of the machine.
FUNCTION CalcDelay#

  s# = TIMER
  DO
    i# = i# + 1
  LOOP UNTIL TIMER - s# >= .5
  CalcDelay# = i#

END FUNCTION

' Center:
'   Centers and prints a text string on a given row
' Parameters:
'   Row - screen row number
'   Text$ - text to be printed
'
SUB Center (Row, Text$)

 Col = MaxCol \ 2
 LOCATE Row, Col - (LEN(Text$) / 2) + 1
 PRINT Text$;

END SUB

SUB DoBeep
  IF GSettings.useSound THEN PLAY "O2A24"
END SUB

' DoExplosion:
'   Produces explosion when a shot is fired
' Parameters:
'   x#, y# - location of explosion
'
SUB DoExplosion (x#, y#)
 DIM radii(1 TO 4, 1 TO 2), colors(1 TO 4)
 
 IF GSettings.useOldExplosions THEN
  IF GSettings.useSound THEN PLAY "MBO0L32EFGEFDC"
  Radius = ScrHeight / 50
  IF Mode = 9 THEN Inc# = .5 ELSE Inc# = .41
  FOR c# = 0 TO Radius STEP Inc#
    CIRCLE (x#, y#), c#, ExplosionColor
  NEXT c#
  FOR c# = Radius TO 0 STEP (-1 * Inc#)
    CIRCLE (x#, y#), c#, BACKATTR
    FOR i = 1 TO 100
    NEXT i
    Rest .005
  NEXT c#

 ELSE
  radii(1, 1) = GSettings.newExplosionRadius
  radii(2, 1) = .9 * radii(1, 1)
  radii(3, 1) = .6 * radii(1, 1)
  radii(4, 1) = .45 * radii(1, 1)
  FOR i = 1 TO 4
   radii(i, 2) = .825 * radii(i, 1)
  NEXT
  colors(1) = 4: colors(2) = 2
  colors(3) = 3: colors(4) = 9
  
  IF GSettings.useSound THEN PLAY "MBO0L32EFGEFDC"
  
  'þ Draw grey smoke, EGA/VGA only
  IF Mode = 9 THEN
   CIRCLE (x#, y#), 1.175 * radii(1, 1), 10
   PAINT (x#, y#), 10, 10
  ELSE
   CIRCLE (x#, y#), 1.175 * radii(1, 1), 1
   PAINT (x#, y#), 0, 1
   CIRCLE (x#, y#), 1.175 * radii(1, 1), 0
  END IF
  
  '? Draw vector explosion graphics
  FOR i = 1 TO 4
   Iwidth = 2 * radii(i, 1): Iheight = 2 * radii(i, 2)
   locX = x# - radii(i, 1): locY = y# - radii(i, 2)
   imageCol = colors(i)

   IF MachSpeed > 700 THEN
    GOSUB DrawShape
    Delay = .5
   ELSE
    CIRCLE (x#, y#), radii(i, 1), imageCol: PAINT (x#, y#), imageCol, imageCol
    Delay = .9
   END IF
  NEXT

  timeStay! = TIMER: DO: LOOP UNTIL TIMER > timeStay! + .1

  CIRCLE (x#, y#), 1.175 * radii(1, 1), 0
  PAINT (x#, y#), 0, 0
 END IF

EXIT SUB

DrawShape:
 RESTORE VectorData
 READ noOfPoints, initX!, initY!
 initX! = (initX! * Iwidth) + locX
 initY! = (initY! * Iheight) + locY
 FOR lVar = 1 TO noOfPoints - 1
  READ toX!, toY!
  toX! = (toX! * Iwidth) + locX
  toY! = (toY! * Iheight) + locY
  IF lVar = 1 THEN
   LINE (initX!, initY!)-(toX!, toY!), imageCol
  ELSE
   LINE -(toX!, toY!), imageCol
  END IF
 NEXT
 PAINT (locX + (Iwidth / 2), locY + (Iwidth / 2)), imageCol, imageCol
RETURN

END SUB

' DoShot:
'   Controls banana shots by accepting player input and plotting
'   shot angle
' Parameters:
'   PlayerNum - Player
'   x, y - Player's gorilla position
'   turn - do not show zeroes at input prompts on first turn
'
FUNCTION DoShot (Player$(), PlayerNum, x, y, turn, othX, othY)

  'Input shot
  IF PlayerNum = 1 THEN
    LocateCol = 2
  ELSE
    IF Mode = 9 THEN
      LocateCol = 67
    ELSE
      LocateCol = 26
    END IF
  END IF

  IF PlayerNum = 1 THEN
    PrevA# = GLeftAngle#: PrevV# = GLeftVeloc
  ELSE
    IF PlayerNum = 2 THEN
      PrevA# = GRightAngle#: PrevV# = GRightVeloc
    END IF
  END IF

  GAng$ = "": Velo$ = ""
 
  LOCATE 2, LocateCol + 3: PRINT "Angle:";
  LOCATE 3, LocateCol: PRINT "Velocity:";
  IF turn > 2 THEN
    PRINT PrevV#
    Pa$ = LTRIM$(STR$(PrevA#))
    Pv$ = LTRIM$(STR$(PrevV#))
  ELSE
    Pa$ = "": Pv$ = ""
  END IF
 
  WHILE INKEY$ <> "": WEND
  DO: pass = 1
   DO
    GAng$ = Get$(2, LocateCol + 10, Pa$, 0, 360, 1)
    IF GAng$ = "" THEN GOSUB AbortGame
   LOOP UNTIL GAng$ <> ""
   IF LEFT$(GAng$, 1) = "*" THEN GAng$ = RIGHT$(GAng$, LEN(GAng$) - 1)
   angle# = VAL(GAng$)
   
   DO
    Velo$ = Get$(3, LocateCol + 10, Pv$, 1, -200, 1)
    IF Velo$ = "" THEN GOSUB AbortGame
   LOOP UNTIL Velo$ <> ""
   IF LEFT$(Velo$, 1) = "*" THEN
    pass = 0: Velo$ = RIGHT$(Velo$, LEN(Velo$) - 1)
    PrevA# = angle#
    PrevV# = CINT(VAL(Velo$))
    Pa$ = GAng$
    Pv$ = Velo$
   END IF
   velocity = CINT(VAL(Velo$))
  LOOP UNTIL pass = 1

  IF PlayerNum = 1 THEN
   GLeftAngle# = angle#: GLeftVeloc = velocity
  ELSE
   IF PlayerNum = 2 THEN
    GRightAngle# = angle#: GRightVeloc = velocity
   END IF
  END IF
 
  IF PlayerNum = 2 THEN
   angle# = 180 - angle#
  END IF

  'Erase input
  FOR i = 1 TO 3 ' Was 4
   'LOCATE i, 1
   'PRINT SPACE$(30 \ (80 \ MaxCol));
   'LOCATE i, (50 \ (80 \ MaxCol))
   'PRINT SPACE$(30 \ (80 \ MaxCol));

   LOCATE i, 2: PRINT SPACE$(17)
   LOCATE i, MaxCol - 17: PRINT SPACE$(17)
  NEXT

  PlayerHit = PlotShot(x, y, angle#, velocity, PlayerNum, othX, othY)
  IF PlayerHit = 0 THEN
   DoShot = FALSE
  ELSE
   DoShot = TRUE
   IF PlayerHit <> PlayerNum AND turn < 3 THEN
    'þ Killed opponent in one shot message
    tpause! = TIMER: DO: LOOP UNTIL TIMER > tpause! + .8
    IF GSettings.useSound THEN PLAY "MFO2L24A+>DFA+FD<A+>DFA+FD<A+>DFA+FD<A+4MB"
    COLOR 12
    FOR msg = 1 TO 3
     Center 1, "IN ONE THROW!": tpause! = TIMER: DO: LOOP UNTIL TIMER > tpause! + .25
     Center 1, SPACE$(14): GOSUB DSRestoreSun: tpause! = TIMER: DO: LOOP UNTIL TIMER > tpause! + .25
    NEXT
   ELSE tpause! = TIMER: DO: LOOP UNTIL TIMER > tpause! + .9
   END IF
   IF PlayerHit = PlayerNum THEN PlayerNum = 3 - PlayerNum
   VictoryDance PlayerNum
  END IF

EXIT FUNCTION

AbortGame:
 cont = FALSE: cval = 1: tpause! = TIMER - 2
 IF Mode = 9 THEN COLOR 14
 DO
  IF TIMER > tpause! + .5 THEN
   IF cval = 1 THEN
    Center 1, " Abort game? [Y/N] "
   ELSE
    Center 1, SPACE$(19)
    GOSUB DSRestoreSun
   END IF
  
   cval = 2 / cval
   tpause! = TIMER
  END IF
  resp$ = UCASE$(INKEY$)
  IF resp$ = "Y" THEN cont = 1
  IF resp$ = "N" THEN cont = 2
 LOOP UNTIL NOT (cont = FALSE)
 IF cont = 1 THEN
  DoShot = 1: EXIT FUNCTION
 ELSE
  IF cval = 2 THEN Center 1, SPACE$(19): DoSun SUNHAPPY
  IF Mode = 1 THEN GOSUB CGARestNames
  IF Mode = 9 THEN COLOR 15
  RETURN
 END IF
EXIT FUNCTION

DSRestoreSun:
 sunX = ScrWidth \ 2: sunY = Scl(25)
 LINE (sunX, sunY - Scl(15))-(sunX, sunY), SUNATTR
 LINE (sunX - Scl(8), sunY - Scl(13))-(sunX, sunY), SUNATTR
 LINE (sunX, sunY)-(sunX + Scl(8), sunY - Scl(13)), SUNATTR
RETURN

CGARestNames:
 REM Under CGA, the Abort Game prompt can overwrite player names
 LOCATE 1, 2: PRINT Player$(1)
 LOCATE 1, MaxCol - LEN(Player$(2)): PRINT Player$(2)
RETURN

END FUNCTION

' DoSun:
'   Draws the sun at the top of the screen.
' Parameters:
'   Mouth - If TRUE draws "O" mouth else draws a smile mouth.
'
SUB DoSun (Mouth)

  'set position of sun
  x = ScrWidth \ 2: y = Scl(25)

  'clear old sun
  LINE (x - Scl(22), y - Scl(18))-(x + Scl(22), y + Scl(18)), BACKATTR, BF

  'draw new sun:
  'body
  CIRCLE (x, y), Scl(12), SUNATTR
  PAINT (x, y), SUNATTR

  'rays
  LINE (x - Scl(20), y)-(x + Scl(20), y), SUNATTR
  LINE (x, y - Scl(15))-(x, y + Scl(15)), SUNATTR

  LINE (x - Scl(15), y - Scl(10))-(x + Scl(15), y + Scl(10)), SUNATTR
  LINE (x - Scl(15), y + Scl(10))-(x + Scl(15), y - Scl(10)), SUNATTR

  LINE (x - Scl(8), y - Scl(13))-(x + Scl(8), y + Scl(13)), SUNATTR
  LINE (x - Scl(8), y + Scl(13))-(x + Scl(8), y - Scl(13)), SUNATTR

  LINE (x - Scl(18), y - Scl(5))-(x + Scl(18), y + Scl(5)), SUNATTR
  LINE (x - Scl(18), y + Scl(5))-(x + Scl(18), y - Scl(5)), SUNATTR

  'mouth
  IF Mouth THEN  'draw "o" mouth
    CIRCLE (x, y + Scl(5)), Scl(2.9), 0
    PAINT (x, y + Scl(5)), 0, 0
  ELSE           'draw smile
    CIRCLE (x, y), Scl(8), 0, (210 * pi# / 180), (330 * pi# / 180)
  END IF

  'eyes
  CIRCLE (x - 3, y - 2), 1, 0
  CIRCLE (x + 3, y - 2), 1, 0
  PSET (x - 3, y - 2), 0
  PSET (x + 3, y - 2), 0

END SUB

'DrawBan:
'  Draws the banana
'Parameters:
'  xc# - Horizontal Coordinate
'  yc# - Vertical Coordinate
'  r - rotation position (0-3). (  \_/  ) /-\
'  bc - if TRUE then DrawBan draws the banana ELSE it erases the banana
SUB DrawBan (xc#, yc#, r, bc)

SELECT CASE r
  CASE 0
    IF bc THEN PUT (xc#, yc#), LBan&, PSET ELSE PUT (xc#, yc#), LBan&, XOR
  CASE 1
    IF bc THEN PUT (xc#, yc#), UBan&, PSET ELSE PUT (xc#, yc#), UBan&, XOR
  CASE 2
    IF bc THEN PUT (xc#, yc#), DBan&, PSET ELSE PUT (xc#, yc#), DBan&, XOR
  CASE 3
    IF bc THEN PUT (xc#, yc#), RBan&, PSET ELSE PUT (xc#, yc#), RBan&, XOR
END SELECT

END SUB

'DrawGorilla:
'  Draws the Gorilla in either CGA or EGA mode
'  and saves the graphics data in an array.
'Parameters:
'  x - x coordinate of gorilla
'  y - y coordinate of the gorilla
'  arms - either Left up, Right up, or both down
SUB DrawGorilla (x, y, arms)
  DIM i AS SINGLE   ' Local index must be single precision

  'draw head
  LINE (x - Scl(4), y)-(x + Scl(2.9), y + Scl(6)), OBJECTCOLOR, BF
  LINE (x - Scl(5), y + Scl(2))-(x + Scl(4), y + Scl(4)), OBJECTCOLOR, BF

  'draw eyes/brow
  LINE (x - Scl(3), y + Scl(2))-(x + Scl(2), y + Scl(2)), 0

  'draw nose if ega
  IF Mode = 9 THEN
    FOR i = -2 TO -1
      PSET (x + i, y + 4), 0
      PSET (x + i + 3, y + 4), 0
    NEXT i
  END IF

  'neck
  LINE (x - Scl(3), y + Scl(7))-(x + Scl(2), y + Scl(7)), OBJECTCOLOR

  'body
  LINE (x - Scl(8), y + Scl(8))-(x + Scl(6.9), y + Scl(14)), OBJECTCOLOR, BF
  LINE (x - Scl(6), y + Scl(15))-(x + Scl(4.9), y + Scl(20)), OBJECTCOLOR, BF

  'legs
  FOR i = 0 TO 4
    CIRCLE (x + Scl(i), y + Scl(25)), Scl(10), OBJECTCOLOR, 3 * pi# / 4, 9 * pi# / 8
    CIRCLE (x + Scl(-6) + Scl(i - .1), y + Scl(25)), Scl(10), OBJECTCOLOR, 15 * pi# / 8, pi# / 4
  NEXT

  'chest
  CIRCLE (x - Scl(4.9), y + Scl(10)), Scl(4.9), 0, 3 * pi# / 2, 0
  CIRCLE (x + Scl(4.9), y + Scl(10)), Scl(4.9), 0, pi#, 3 * pi# / 2

  FOR i = -5 TO -1
    SELECT CASE arms
      CASE 1
        'Right arm up
        CIRCLE (x + Scl(i - .1), y + Scl(14)), Scl(9), OBJECTCOLOR, 3 * pi# / 4, 5 * pi# / 4
        CIRCLE (x + Scl(4.9) + Scl(i), y + Scl(4)), Scl(9), OBJECTCOLOR, 7 * pi# / 4, pi# / 4
        GET (x - Scl(15), y - Scl(1))-(x + Scl(14), y + Scl(28)), GorR&
      CASE 2
        'Left arm up
        CIRCLE (x + Scl(i - .1), y + Scl(4)), Scl(9), OBJECTCOLOR, 3 * pi# / 4, 5 * pi# / 4
        CIRCLE (x + Scl(4.9) + Scl(i), y + Scl(14)), Scl(9), OBJECTCOLOR, 7 * pi# / 4, pi# / 4
        GET (x - Scl(15), y - Scl(1))-(x + Scl(14), y + Scl(28)), GorL&
      CASE 3
        'Both arms down
        CIRCLE (x + Scl(i - .1), y + Scl(14)), Scl(9), OBJECTCOLOR, 3 * pi# / 4, 5 * pi# / 4
        CIRCLE (x + Scl(4.9) + Scl(i), y + Scl(14)), Scl(9), OBJECTCOLOR, 7 * pi# / 4, pi# / 4
        GET (x - Scl(15), y - Scl(1))-(x + Scl(14), y + Scl(28)), GorD&
    END SELECT
  NEXT i
END SUB

'ExplodeGorilla:
'  Causes gorilla explosion when a direct hit occurs
'Parameters:
'  X#, Y# - shot location
SUB ExplodeGorilla (x#, y#, PlayerHit)
  YAdj = Scl(12)
  XAdj = Scl(5)
  SclX# = ScrWidth / 320
  SclY# = ScrHeight / 200
 
  IF GSettings.useSound THEN PLAY "MBO0L16EFGEFDC"

  FOR i = 1 TO 16 * SclX#
   CIRCLE (GorillaX(PlayerHit) + 3.5 * SclX# + XAdj, GorillaY(PlayerHit) + YAdj), i, i MOD 2 + 1, , , -1.57
  NEXT i

  timeStay! = TIMER: DO: LOOP UNTIL TIMER > timeStay! + .05

  FOR i = 24 * SclX# TO 1 STEP -1
    CIRCLE (GorillaX(PlayerHit) + 3.5 * SclX# + XAdj, GorillaY(PlayerHit) + YAdj), i, BACKATTR, , , -1.57
    FOR count = 1 TO 200
    NEXT
  NEXT i
 
END SUB

SUB Extro

 COLOR 7: CLS

 RESTORE PartingMessage
 Slidy
 READ num
 num = CINT(RND * (num - 1))
 IF num > 0 THEN FOR l = 1 TO num: READ pnum, pmsg$, pnum, pnum, pnum: NEXT
 Slidy

 t! = TIMER: DO: LOOP UNTIL TIMER > t! + 3.8 OR INKEY$ <> ""

END SUB

FUNCTION Get$ (Row, Col, Prev$, Typ, Max, Esc)

 ' Row,Col : position
 ' Prev$ : the previous value of the number or string.
 ' Typ : the type of input required: TRUE for string, FALSE for numeric
 '       and 1 for numerical, tabbable while empty
 ' Max : the maximum number of characters for string or the maximum
 ' value for numeric. For numeric, a negative maximum means that the minimum
 ' value is to be one not zero and the maximum value is the absolute value
 ' of Max.
 ' Esc : TRUE if Escape key permitted, FALSE if not permitted, 1 if Escape
 ' clears input rather then undoes

 SpecTab = 0: IF Typ = 1 THEN Typ = FALSE: SpecTab = 1
 IF NOT Typ THEN
   IF Max < 0 THEN Zero = 0 ELSE Zero = -1
   Max = ABS(Max)
 END IF

 Hold$ = Prev$
 cont = 0: Lett$ = "": Curs = 0: Timo! = 0
 Valid$ = "1234567890" + CHR$(8) + CHR$(9) + CHR$(13) + CHR$(27)
 IF Typ THEN Valid$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ- .'!" + Valid$
 LOCATE Row, Col:
 IF Typ THEN
   Bck = Max - LEN(Hold$) + 1
 ELSE
   Bck = LEN(STR$(Max)) - LEN(Hold$)
 END IF
 PRINT Hold$; SPC(Bck);

 DO
   DO
     Timo! = TIMER: Curs = 0: LOCATE Row, Col + LEN(Hold$): IF LEN(Hold$) = Max THEN PRINT "Û" ELSE PRINT "_"
     DO: LOOP UNTIL INKEY$ = ""
     DO
       IF TIMER > Timo! + .5 THEN
         LOCATE Row, Col + LEN(Hold$)
         IF Curs <> 1 THEN
          PRINT " "
         ELSE
          IF Typ AND LEN(Hold$) = Max THEN PRINT "Û" ELSE PRINT "_"
         END IF
         Curs = 1 - Curs
         Timo! = TIMER
       END IF
       Lett$ = INKEY$
     LOOP UNTIL Lett$ <> ""
     LOCATE Row, Col + LEN(Hold$): PRINT " "
     Intra = INSTR(Valid$, UCASE$(Lett$))
     IF Lett$ = CHR$(0) + CHR$(83) THEN Intra = 50 ' DEL key
     IF Intra = 0 THEN DoBeep: DO: LOOP UNTIL INKEY$ = ""
   LOOP UNTIL Intra > 0

   SELECT CASE Intra

     CASE 50
     'þ DELETE key
       LOCATE Row, Col: PRINT STRING$(LEN(Hold$), " ");
       Hold$ = ""

     CASE 1 TO LEN(Valid$) - 4 'þ Letter, number or symbol
     'þ Numeric field
      IF NOT Typ THEN 'þ Number
       IF NOT ((Lett$ = "0" AND (NOT Zero AND Hold$ = "")) OR Hold$ = "0") THEN
        IF VAL(Hold$ + Lett$) <= Max THEN
         Hold$ = Hold$ + Lett$
         LOCATE Row, Col: PRINT Hold$
        ELSE DoBeep
        END IF
       ELSE DoBeep
       END IF
      ELSE 'þ Text field
       IF LEN(Hold$) < Max THEN
        Hold$ = Hold$ + Lett$
        LOCATE Row, Col: PRINT Hold$
       ELSE DoBeep
       END IF
      END IF

     CASE LEN(Valid$) - 3
     'þ BACKSPACE key
      IF LEN(Hold$) > 0 THEN
       Hold$ = LEFT$(Hold$, LEN(Hold$) - 1)
       LOCATE Row, Col: PRINT Hold$; " ";
      ELSE DoBeep
      END IF
                        
     CASE LEN(Valid$) - 2
     'þ TAB key
       IF (LEN(Hold$) > 0 AND NOT Typ) OR SpecTab = 1 THEN Hold$ = "*" + Hold$: cont = 1 ELSE DoBeep

     CASE LEN(Valid$) - 1
     'þ RETURN key
       IF LEN(Hold$) > 0 THEN cont = 1 ELSE DoBeep

     CASE LEN(Valid$)
     'þ ESCAPE key
       IF Esc = TRUE THEN Hold$ = Prev$: cont = 1
       IF Esc = 1 THEN Hold$ = "": cont = 1

   END SELECT
   DO: LOOP UNTIL INKEY$ = ""

 LOOP UNTIL cont = 1

Get$ = Hold$
END FUNCTION

'GetInputs:
'  Gets competing players and game configuration play at beginning of game
'  and manages players list
'Parameters:
'  Player$() - player names
'  NumGames - number of games to play
'  P - number of stored players
SUB GetInputs (Player$(), NumGames, P)

' Lay out screen

 CLS : RESTORE Setup: Slidy: COLOR 2: LOCATE 2, 1: PRINT STRING$(80, "Í") 'þ Show screen title
 active = 0: FOR fld = 1 TO 4: GOSUB SetupFields: NEXT 'þ Display fields
 fld = 0: GOSUB SetupFields 'þ Display player names

' Fill in players box

 cStat = 0: FOR N = 1 TO P: GOSUB Curs: NEXT
 'þ Must highlight opponent player (normally done after [ENTER] or [TAB]
 IF PDefs(2) > 0 THEN N = PDefs(2): cStat = 2: GOSUB Curs

' Process fields loop
  ' complete: ready to start the game
  ' fld: which field is being processed
  ' numG$: text field to hold number of games
  ' grav$: text field to hold gravity

 complete = 0: fld = 1: numG$ = LTRIM$(STR$(NumGames)): grav$ = LTRIM$(STR$(Gravity))
 DO
  'þ Highlight current field if there are enough players. Player field not
  '  highlighted until there is a player which can be assigned to it, and the
  '  last two fields are unselectable unless there are enough players
  active = 1: IF P >= 2 THEN GOSUB SetupFields

  SELECT CASE fld
   CASE 1 TO 2
    GOSUB ManagePlayers
   CASE IS = 3
    GOSUB Rounds
   CASE IS = 4
    GOSUB Gravity
  END SELECT
  active = 0: GOSUB SetupFields 'þ Unhighlight current field
  IF NOT complete THEN fld = fld + 1: IF fld = 5 THEN fld = 1
  IF complete AND (PDefs(1) = 0 OR PDefs(2) = 0) THEN fld = 1: complete = 0
 LOOP UNTIL complete

Player$(1) = RTRIM$(PDat(PDefs(1)).PNam)
Player$(2) = RTRIM$(PDat(PDefs(2)).PNam)
NumGames = VAL(numG$)
Gravity = VAL(grav$)

'þ Clear most of the screen
COLOR , 0: FOR l = 3 TO 24: LOCATE l, 1: PRINT STRING$(80, " "); : NEXT

EXIT SUB

'þþþþþþþþþþþþþþþþþþþþ
'þ FIELDS SUBROUTINES

ManagePlayers:
 cre = 0
 WHILE P < 2 'þ Ensure enough players for the game (only used before league table created)
  cre = 1: GOSUB CreatePlayer
  IF P = 2 THEN GOSUB SetupFields 'þ Finally ready to highlight Player field
 WEND
 cre = 0

 'þ OK. Assuming that there are enough players to select.
 opp = 2 / fld 'þ PDefs array number of opposite player
 ShowPrompts fld
 IF PDefs(fld) > 0 THEN 'þ Put cursor bar on currently selected player
  x = ((PDefs(fld) - 1) MOD 4) + 1: y = INT((PDefs(fld) - 1) / 4) + 1
 ELSE                   'þ Otherwise choose free player
  IF PDefs(opp) <> 1 THEN
   x = 1: y = 1
  ELSE
   'IF PDefs(opp) = 1 AND P > 1 THEN
   x = 2: y = 1
  END IF
 END IF

 finished = 0: mov = 0: IF P > 1 THEN mov = 1
 DO
 defSwap = 0 'þ Flag for player definition swapping
  N = (y - 1) * 4 + x 'þ Convert cursor bar position into player number
  LOCATE 8 + (fld * 2 - 2), 22
  'þ Do not display player name if it is taken and swap is not permitted
  IF NOT ((PDefs(fld) = 0 OR PDefs(opp) = 0) AND PDefs(opp) = N) THEN
   COLOR 2, 0: PRINT PDat(N).PNam;
  ELSE
   COLOR 12, 0: PRINT "Can't have.      "
  END IF
  IF (PDefs(opp)) = N AND PDefs(fld) > 0 THEN
   LOCATE 8 + (opp * 2 - 2), 23 + LEN(RTRIM$(PDat(PDefs(opp)).PNam))
   COLOR 2, 0: PRINT "("; CHR$(26); " "; RTRIM$(PDat(PDefs(fld)).PNam); ")";
  COLOR 2: LOCATE 7, 3: PRINT "": COLOR 9: LOCATE 7, 5
   PRINT "Pressing [ENTER] now will switch the players over."
   defSwap = 1
  END IF
  IF mov = 1 THEN cur = 1: GOSUB Move
  DO
   key$ = INKEY$
  LOOP UNTIL key$ <> ""
  COLOR 1, 0
  IF defSwap = 1 THEN
   LOCATE 8 + (opp * 2 - 2), 23 + LEN(RTRIM$(PDat(PDefs(opp)).PNam))
   PRINT STRING$(21, " ")
   LOCATE 7, 3: PRINT STRING$(52, " ")
  END IF

  'þ Move cursor bar, manipulate players, and select a player to compete
  SELECT CASE UCASE$(key$)
   CASE CHR$(0) + CHR$(72)
    IF y > 1 THEN cur = 0: GOSUB Move: y = y - 1: mov = 1 ELSE AlertSnd
   CASE CHR$(0) + CHR$(80)
    IF (y * 4 + x) <= P THEN cur = 0: GOSUB Move: y = y + 1: mov = 1 ELSE AlertSnd
   CASE CHR$(0) + CHR$(75)
    IF x > 1 THEN
     cur = 0: GOSUB Move: x = x - 1: mov = 1
    ELSE
     IF y > 1 THEN
      cur = 0: GOSUB Move: mov = 1: x = 4: y = y - 1
     ELSE
      AlertSnd
     END IF
    END IF
   CASE CHR$(0) + CHR$(77)
    IF x < 4 AND ((y - 1) * 4 + (x + 1)) <= P THEN
     cur = 0: GOSUB Move: x = x + 1: mov = 1
    ELSE
     IF (y * 4 + 1) <= P THEN
      cur = 0: GOSUB Move: mov = 1: x = 1: y = y + 1
     ELSE
      AlertSnd
     END IF
    END IF
   CASE CHR$(9), CHR$(13)
    IF key$ = CHR$(13) THEN 'þ Only update player defs if ENTER pressed
     IF PDefs(opp) = N AND PDefs(fld) > 0 THEN
      'þ Swap player definitions
      SWAP PDefs(1), PDefs(2): COLOR , 0: finished = 1
      cStat = 2: GOSUB Curs
      IF fld = 2 THEN N = PDefs(opp): GOSUB Curs
     ELSEIF PDefs(opp) <> N THEN
      'þ Define player
      IF PDefs(fld) <> N THEN 'þ Remove green highlight and define PDefs
       IF PDefs(fld) > 0 THEN Nt = N: N = PDefs(fld): cStat = 0: GOSUB Curs: N = Nt
       PDefs(fld) = N
      END IF
      finished = 1
      cStat = 2: GOSUB Curs
     ELSE
      AlertSnd
     END IF
    ELSE
     IF PDefs(fld) > 0 THEN
      'þ Abort change to definition, and move to next field
      finished = 1
      cur = 0: GOSUB Move 'þ Remove cursor bar
      N = PDefs(fld): cStat = 2: GOSUB Curs 'þ Red highlight
     ELSEIF PDefs(fld) = 0 AND PDefs(opp) <> N THEN
      'þ Player undefined, so define it
      PDefs(fld) = N: finished = 1
      cStat = 2: GOSUB Curs
     ELSE
      AlertSnd
     END IF
    END IF
    IF finished = 1 THEN
     LOCATE 8 + (fld * 2 - 2), 22: COLOR 10, 0: PRINT PDat(PDefs(fld)).PNam;
     IF defSwap = 1 THEN LOCATE 8 + (opp * 2 - 2), 22: PRINT PDat(PDefs(opp)).PNam;
    END IF
   CASE "N"
    GOSUB CreatePlayer
   CASE "R"
    GOSUB RenamePlayer
   CASE CHR$(0) + CHR$(83)
    GOSUB DeletePlayer
   CASE ELSE
    'þ Incorrect key pressed
    AlertSnd
  END SELECT

 'Player chosen
 LOOP UNTIL finished

RETURN
       
CreatePlayer:
 IF P < NPLAYERS THEN
  IF cre = 1 THEN ShowPrompts -12 ELSE ShowPrompts 12
  nx = WhereX(P + 1): ny = WhereY(P + 1)
  cStat = 0: GOSUB Curs: COLOR 10, 1
  PDat(P + 1).PNam = " "
  DO: cont = 1
   IF P < 2 THEN Esc = FALSE ELSE Esc = TRUE 'þ Prevent ESCAPE key when players not yet created
   PDat(P + 1).PNam = RTRIM$(Get$(ny, nx, RTRIM$(PDat(P + 1).PNam), -1, 17, Esc))
  
   IF LTRIM$(PDat(P + 1).PNam) = "" THEN
    cont = 2
   ELSE
    FOR inl = 1 TO P
     IF PDat(inl).PNam = PDat(P + 1).PNam THEN AlertSnd: cont = 0
    NEXT
   END IF
  LOOP UNTIL cont > 0
  IF cont = 1 THEN
   P = P + 1: DoBeep: x = ((P - 1) MOD 4) + 1: y = INT((P - 1) / 4) + 1
   IF P > 1 THEN N = P - 1: cur = 0: GOSUB Move
   N = (y - 1) * 4 + x: cStat = 0: GOSUB Curs
  ELSEIF cont = 2 THEN
   Nt = N: N = P + 1: cStat = 0: GOSUB Curs
   N = Nt: GOSUB Move
  END IF
  ShowPrompts fld
 ELSE
  AlertSnd
 END IF
RETURN

RenamePlayer:
 ShowPrompts 13
 nx = WhereX(P + 1): ny = WhereY(P + 1)
 cStat = 0: GOSUB Curs: COLOR 10, 1
 DO: cont = 1: count = 0
  PDat(N).PNam = Get$(WhereY(N), WhereX(N), RTRIM$(PDat(N).PNam), -1, 17, TRUE)
  IF LEFT$(PDat(N).PNam, 1) = "*" THEN PDat(N).PNam = RIGHT$(PDat(N).PNam, LEN(PDat(N).PNam) - 1)
  FOR inl = 1 TO P
   IF PDat(inl).PNam = PDat(N).PNam THEN count = count + 1
  NEXT: IF count > 1 THEN AlertSnd: cont = 0
 LOOP UNTIL cont = 1: DoBeep
 cStat = 1: GOSUB Curs: ShowPrompts fld: upd = 0
 IF PDefs(1) = N THEN
  upd = 1
 ELSEIF PDefs(2) = N THEN
  upd = 2
 END IF
 IF upd > 0 THEN
  COLOR 10, 0: LOCATE 8 + (upd * 2 - 2), 22
  PRINT PDat(PDefs(upd)).PNam;
 END IF
RETURN

DeletePlayer:
 'þ What to do after the delete
 nextAction = 0
 IF N = PDefs(opp) THEN 'þ Opposite player redefined
  IF NOT (fld = 1 AND P > 2) THEN 'þ But not in this situation
   nextAction = 1
  END IF
 END IF

 IF PDefs(fld) > 0 THEN COLOR 10, 0: LOCATE 8 + (fld * 2 - 2), 22: PRINT PDat(PDefs(fld)).PNam;
 COLOR 0, 0
 FOR l = 3 TO 7: LOCATE l, 1: PRINT STRING$(80, " "); : NEXT
 ShowPrompts 11
 LOCATE 3, 3: COLOR 4
 PRINT "Do you want to delete the player `" + RTRIM$(PDat(N).PNam) + "'?"
 BEEP: DO: DO
  i$ = INKEY$
 LOOP UNTIL i$ <> "": i$ = UCASE$(i$): LOOP UNTIL i$ = "Y" OR i$ = "N"
 COLOR 0, 0: LOCATE 3: PRINT STRING$(80, " ")
 IF i$ = "Y" THEN
  'þ Corrects PDefs (selected players) values and display
  IF fld = 2 AND PDefs(fld) = 0 AND N = PDefs(opp) THEN
   COLOR 8, 0: LOCATE 10, 22
   PRINT "<undefined>      ";
  END IF

  FOR upd = 1 TO 2
   IF PDefs(upd) = N THEN
    COLOR 8, 0: LOCATE 8 + (upd * 2 - 2), 22
    PRINT "<undefined>      ";
    PDefs(upd) = 0
   ELSEIF PDefs(upd) > N THEN
    PDefs(upd) = PDefs(upd) - 1
   END IF
  NEXT
 
  IF P = 2 AND PDefs(fld) = 0 AND PDefs(opp) > 0 THEN
   COLOR 8, 0: LOCATE 8 + (fld * 2 - 2), 22
   PRINT "<undefined>      ";
  END IF
 

  'þ Tidies up PDat (array of players)
  Pt = P: P = P - 1: Nt = N
  IF N < Pt THEN
   FOR N = N TO P
    PDat(N).PNam = PDat(N + 1).PNam
    PDat(N).Rounds = PDat(N + 1).Rounds
    PDat(N).Won = PDat(N + 1).Won
    PDat(N).Accu = PDat(N + 1).Accu
    IF PDefs(2 * (1 / fld)) = N THEN cStat = 2 ELSE cStat = 0
    GOSUB Curs
   NEXT
  END IF

  'þ This wipes all trace of the deleted player
  PDat(Pt).Won = 0
  PDat(Pt).PNam = "": PDat(Pt).Accu = 0: PDat(Pt).Rounds = 0
     
  N = Pt: cStat = 0: GOSUB Curs
  N = Nt
  IF N > P THEN
   N = N - 1: x = x - 1: IF x = 0 THEN x = 1: y = y - 1: IF y = 0 THEN y = 1
  END IF
  IF P > 0 THEN ShowPrompts fld
 
  IF nextAction > 0 THEN
   cStat = 0: GOSUB Curs 'þ Remove cursor bar
  END IF

  IF PDefs(fld) > 0 THEN
   x = ((PDefs(fld) - 1) MOD 4) + 1: y = INT((PDefs(fld) - 1) / 4) + 1
  ELSEIF N = PDefs(opp) THEN
   IF N > 1 THEN
    x = x - 1: IF x = 0 THEN y = y - 1: x = 4 'þ Back one player
   ELSEIF N < P THEN
    x = x + 1: IF x = 5 THEN y = y + 1: x = 1 'þ Fwd one player
   END IF
  END IF

  IF nextAction = 1 THEN
   active = 0: GOSUB SetupFields
   SWAP fld, opp: active = 1: GOSUB SetupFields
  END IF
 
  'þ Ensure always 2 players minimum
  IF P = 1 THEN cre = 1: GOSUB CreatePlayer: cre = 0

 ELSE
  ShowPrompts fld
 END IF

RETURN

Rounds:
 ShowPrompts 3
 COLOR 15, 9: numG$ = Get$(20, 51, numG$, 0, -99, FALSE): COLOR 15, 0
 IF LEFT$(numG$, 1) = "*" THEN numG$ = RIGHT$(numG$, LEN(numG$) - 1)
 LOCATE 20, 51: PRINT numG$; SPC(3 - LEN(numG$));
RETURN

Gravity:
 ShowPrompts 4
 COLOR 15, 9: grav$ = Get$(22, 51, grav$, 0, -99, FALSE): COLOR 15, 0
 IF LEFT$(grav$, 1) = "*" THEN grav$ = RIGHT$(grav$, LEN(grav$) - 1) ELSE complete = 1
 LOCATE 22, 51: PRINT grav$; SPC(4 - LEN(grav$));
RETURN

'þþþþþþþþþþþþþþþþþþþþþ
'þ SUPPORT SUBROUTINES

' field display

SetupFields:
 IF fld = 1 AND active THEN GOSUB DrawBox
 IF fld = 2 AND NOT active THEN GOSUB DrawBox
 IF active THEN COLOR 15 ELSE COLOR 8
 SELECT CASE fld
  CASE IS = 0
   FOR upd = 1 TO 2
    LOCATE 8 + (upd * 2 - 2), 22
    IF PDefs(upd) > 0 THEN
     COLOR 10, 0: PRINT PDat(PDefs(upd)).PNam;
    ELSE
     COLOR 8, 0: PRINT "<undefined>"
    END IF
   NEXT
  CASE IS = 1
   LOCATE 8, 11: PRINT "Player 1 ="
  CASE IS = 2
   LOCATE 10, 11: PRINT "Player 2 ="
  CASE IS = 3
   tStr$ = "Maximum rounds? (1 - 99, Default =" + STR$(GSettings.defaultRoundQty) + "):"
   LOCATE 20, 50 - LEN(tStr$): PRINT tStr$
  CASE IS = 4
   LOCATE 22, 13: PRINT "Gravity in m/sý (1 - 99, Earth = 10):"
 END SELECT
RETURN

DrawBox:
 COLOR 2, 0
 IF active THEN
  LOCATE 12, 1: PRINT "É"; STRING$(78, "Í"); "»";
  LOCATE 18, 1: PRINT "È"; STRING$(78, "Í"); "¼";
  FOR l = 13 TO 17: LOCATE l, 1: PRINT "º"; : LOCATE l, 80: PRINT "º"; : NEXT
 ELSE
  LOCATE 12, 1: PRINT "Ú"; STRING$(78, "Ä"); "¿";
  LOCATE 18, 1: PRINT "À"; STRING$(78, "Ä"); "Ù";
  FOR l = 13 TO 17: LOCATE l, 1: PRINT "³"; : LOCATE l, 80: PRINT "³"; : NEXT
 END IF
RETURN

' cursor display

Move: 'þ Displays or removes cursor bar, calculating highlight colour
 which = 1
 IF PDefs(2 / fld) = N THEN which = 0
 IF PDefs(fld) = N THEN which = 2
 SELECT CASE cur 'þ Blue (1) or black (0) background
  CASE 1
   IF which = 1 THEN
    cStat = 1: GOSUB Curs
   ELSEIF which = 0 THEN
    cStat = 3: GOSUB Curs
   ELSE
    cStat = 5: GOSUB Curs
   END IF
  CASE 0
   IF which = 1 THEN
    cStat = 0: GOSUB Curs
   ELSEIF which = 0 THEN
    cStat = 2: GOSUB Curs
   ELSE
    cStat = 4: GOSUB Curs
   END IF
 END SELECT
RETURN

Curs: 'þ Displays or removes cursor bar, being told the highlight colour
 SELECT CASE cStat
  CASE 0
   COLOR 15, 0
  CASE 1
   COLOR 11, 1
  CASE 2
   COLOR 4, 0
  CASE 3
   COLOR 4, 1
  CASE 4
   COLOR 2, 0
  CASE 5
   COLOR 2, 1
 END SELECT
 LOCATE WhereY(N), WhereX(N): PRINT RTRIM$(PDat(N).PNam);
 IF N < P THEN
  PRINT ",";
 ELSEIF N = P THEN
  PRINT ".";
 ELSE
  PRINT " ";
 END IF
 PRINT SPC(17 - LEN(RTRIM$(PDat(N).PNam)));
RETURN

END SUB

'GorillaIntro:
'  Displays gorillas on screen for the first time
'  allows the graphical data to be put into an array
'Parameters:
'  Player$() - The names of the players
'  cIntro - Is introduction compulsory? (Yes for first ever game)
'
SUB GorillaIntro (Player$(), cIntro)
  IF cIntro = 1 THEN 'þ cIntro = 0 means introduction compulsory
   CLS
   LOCATE 1, 36: PRINT STRING$(10, " ")
   RESTORE Ready: Slidy
   COLOR 2: LOCATE 15, 31: PRINT "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ"
   COLOR 9: LOCATE 17, 34: PRINT "= View Intro"
   LOCATE 18, 34: PRINT "= Play Game"
   LOCATE 19, 34: PRINT "= Quit Gorillas"
   LOCATE 21, 34: PRINT "Your Choice?"
   COLOR 12: LOCATE 17, 32: PRINT "V": LOCATE 18, 32: PRINT "P"
   LOCATE 19, 32: PRINT "Q"
   DO
    Char$ = UCASE$(INKEY$)
   LOOP UNTIL Char$ <> "" AND INSTR("QVP", Char$)
   IF Char$ = "V" THEN cIntro = 0
   IF Char$ = "Q" THEN
    IF GamePlayedYN = 1 THEN Extro
    COLOR 7: CLS : SYSTEM
   END IF
  END IF

  IF Mode = 1 THEN
    x = 125
    y = 100
  ELSE
    x = 286
    y = 175
  END IF

  SCREEN Mode
  SetScreen
  IF Mode = 1 THEN
   MaxCol = 40
   Center 5, "Please wait while gorillas are drawn."
  END IF

  VIEW PRINT 9 TO 24

  IF Mode = 9 THEN PALETTE OBJECTCOLOR, BackColor
 
  DrawGorilla x, y, ARMSDOWN
  CLS 2
  DrawGorilla x, y, LEFTUP
  CLS 2
  DrawGorilla x, y, RIGHTUP
  CLS 2
  
  IF Mode = 1 THEN CLS ' For some reason, the above CLS 2s don't work in CGA
 
  VIEW PRINT 1 TO 25
  IF Mode = 9 THEN PALETTE OBJECTCOLOR, 46
 
  IF cIntro = 0 THEN
    IF Mode = 9 THEN
      Rad! = 100: yStep! = 1: DO
        CIRCLE (319, 190), Rad!, 8, , , .5
        CIRCLE (319, 187), Rad!, 11, , , .5
        Rad! = Rad! + yStep!: yStep! = yStep! * 1.1
      LOOP UNTIL 320 + Rad! >= 640
      PAINT (0, 0), 8, 11
      LINE (142, 20)-(491, 20), 3
      LINE (491, 20)-(491, 95), 3
      LINE (491, 95)-(317, 156), 3
      LINE (317, 156)-(142, 95), 3
      LINE (142, 95)-(142, 20), 3
      PAINT (317, 40), 0, 3
    END IF

    IF Mode = 9 THEN COLOR 11
    Center 2, " QBasic G O R I L L A S "
    IF Mode = 9 THEN COLOR 9
    Center 4, "STARRING:"
    P$ = player$(1) + " AND " + player$(2)
    IF Mode = 9 THEN COLOR 3
    Center 5, STRING$(LEN(P$), "Ä")
    IF Mode = 9 THEN COLOR 2
    Center 6, P$
    IF Mode = 9 THEN COLOR 9

    PUT (x - 13, y), GorD&, PSET
    PUT (x + 47, y), GorD&, PSET
    Rest 1

    IF INKEY$ <> "" GOTO GetThisOverWith

    PUT (x - 13, y), GorL&, PSET
    PUT (x + 47, y), GorR&, PSET
    IF GSettings.useSound THEN PLAY "t120o1l16b9n0baan0bn0bn0baaan0b9n0baan0b" ELSE RestReal .18
    Rest .3

    IF INKEY$ <> "" GOTO GetThisOverWith

    PUT (x - 13, y), GorR&, PSET
    PUT (x + 47, y), GorL&, PSET
    IF GSettings.useSound THEN PLAY "o2l16e-9n0e-d-d-n0e-n0e-n0e-d-d-d-n0e-9n0e-d-d-n0e-" ELSE RestReal .18
    Rest .3

    IF INKEY$ <> "" GOTO GetThisOverWith

    PUT (x - 13, y), GorL&, PSET
    PUT (x + 47, y), GorR&, PSET
    IF GSettings.useSound THEN PLAY "o2l16g-9n0g-een0g-n0g-n0g-eeen0g-9n0g-een0g-" ELSE RestReal .18
    Rest .3

    IF INKEY$ <> "" GOTO GetThisOverWith

    PUT (x - 13, y), GorR&, PSET
    PUT (x + 47, y), GorL&, PSET
    IF GSettings.useSound THEN PLAY "o2l16b9n0baan0g-n0g-n0g-eeen0o1b9n0baan0b" ELSE RestReal .18
    Rest .3

    IF INKEY$ <> "" GOTO GetThisOverWith

    FOR i = 1 TO 4
      PUT (x - 13, y), GorL&, PSET
      PUT (x + 47, y), GorR&, PSET
      IF GSettings.useSound THEN PLAY "T160O0L32EFGEFDC" ELSE RestReal .18
      Rest .1
      PUT (x - 13, y), GorR&, PSET
      PUT (x + 47, y), GorL&, PSET
      IF GSettings.useSound THEN PLAY "T160O0L32EFGEFDC" ELSE RestReal .18
      Rest .1

      IF INKEY$ <> "" GOTO GetThisOverWith
    NEXT

    Rest 1
  END IF

GetThisOverWith:
 ' Finally, the intro can be aborted
END SUB

'Intro:
'  Displays game introduction
SUB Intro

  IF GSettings.useSound THEN PLAY "MBT160O2" ' Initialise sound
  WHILE INKEY$ <> "": WEND 'Clear keyboard buffer
  RESTORE SlidyText
  Slidy
  SparklePause (5)
  t$ = STRING$(80, " ")
  FOR s = 5 TO 8: LOCATE s * 2, 1: PRINT t$; : NEXT
  LOCATE 1, 1: PRINT t$: LOCATE 22, 1: PRINT t$
  FOR s = 1 TO 22: LOCATE s, 1: PRINT " "; : LOCATE s, 80: PRINT " "; : NEXT
  Slidy
  SparklePause (0)
END SUB

SUB LoadSettings
DIM currLine$, eqPos, key$, value$, nBool

 'þ set default settings
 GSettings.useSound = 1
 GSettings.useOldExplosions = 0
 GSettings.newExplosionRadius = 40
 GSettings.useSlidingText = 0 '1
 GSettings.defaultGravity = 17
 GSettings.defaultRoundQty = 4
 GSettings.showIntro = 1
 GSettings.forceCGA = 0
 

 lastErrCode = 0
 ON ERROR GOTO FuckOff
 OPEN "Gorillas.ini" FOR INPUT AS #1
 IF lastErrCode > 0 THEN EXIT SUB
 WHILE NOT EOF(1)
  LINE INPUT #1, currLine$
  IF lastErrCode > 0 THEN CLOSE #1: EXIT SUB
  GOSUB processLine
 WEND
 CLOSE #1
 ON ERROR GOTO 0

EXIT SUB

processLine:
 eqPos = INSTR(currLine$, "=")
 IF eqPos = 0 THEN
  RETURN
 END IF
 key$ = RTRIM$(LTRIM$(MID$(currLine$, 1, eqPos - 1)))
 value$ = RTRIM$(LTRIM$(RIGHT$(currLine$, LEN(currLine$) - eqPos)))

 SELECT CASE UCASE$(key$)
 CASE "USESOUND"
  GOSUB getBool
  IF nBool > -1 THEN GSettings.useSound = nBool
 CASE "USEOLDEXPLOSIONS"
  GOSUB getBool
  IF nBool > -1 THEN GSettings.useOldExplosions = nBool
 CASE "NEWEXPLOSIONRADIUS"
  GSettings.newExplosionRadius = VAL(value$)
 CASE "USESLIDINGTEXT"
  GOSUB getBool
  IF nBool > -1 THEN GSettings.useSlidingText = nBool
 CASE "DEFAULTGRAVITY"
  tVal = VAL(value$)
  IF tVal > 0 AND tVal < 100 THEN GSettings.defaultGravity = tVal
 CASE "DEFAULTROUNDQTY"
  tVal = VAL(value$)
  IF tVal > 0 AND tVal < 100 THEN GSettings.defaultRoundQty = tVal
 CASE "SHOWINTRO"
  GOSUB getBool
  IF nBool > -1 THEN GSettings.showIntro = nBool
 CASE "FORCECGA"
  GOSUB getBool
  IF nBool > -1 THEN GSettings.forceCGA = nBool
 END SELECT
 RETURN

getBool:
 IF UCASE$(value$) = "YES" OR value$ = "1" OR UCASE$(value$) = "TRUE" THEN
  nBool = 1
 ELSEIF UCASE$(value$) = "NO" OR value$ = "0" OR UCASE$(value$) = "FALSE" THEN
  nBool = 0
 ELSE
  nBool = -1
 END IF
 RETURN

leave:

END SUB

'MakeCityScape:
'  Creates random skyline for game
'Parameters:
'  BCoor() - a user-defined type array which stores the coordinates of
'  the upper left corner of each building.
SUB MakeCityScape (BCoor() AS XYPoint)

  x = 2

  'Set the sloping trend of the city scape. NewHt is new building height
  Slope = FNRan(6)
  SELECT CASE Slope
    CASE 1: NewHt = 15                 'Upward slope
    CASE 2: NewHt = 130                'Downward slope
    CASE 3 TO 5: NewHt = 15            '"V" slope - most common
    CASE 6: NewHt = 130                'Inverted "V" slope
  END SELECT

  IF Mode = 9 THEN
    BottomLine = 335                   'Bottom of building
    HtInc = 10                         'Increase value for new height
    DefBWidth = 37                     'Default building height
    RandomHeight = 120                 'Random height difference
    WWidth = 3                         'Window width
    WHeight = 6                        'Window height
    WDifV = 15                         'Counter for window spacing - vertical
    WDifh = 10                         'Counter for window spacing - horizontal
  ELSE
    BottomLine = 190
    HtInc = 6
    NewHt = NewHt * 20 \ 35            'Adjust for CGA
    DefBWidth = 18
    RandomHeight = 54
    WWidth = 1
    WHeight = 2
    WDifV = 5
    WDifh = 4
  END IF

  CurBuilding = 1
  DO

    SELECT CASE Slope
      CASE 1
        NewHt = NewHt + HtInc
      CASE 2
        NewHt = NewHt - HtInc
      CASE 3 TO 5
        IF x > ScrWidth \ 2 THEN
          NewHt = NewHt - 2 * HtInc
        ELSE
          NewHt = NewHt + 2 * HtInc
        END IF
      CASE 4
        IF x > ScrWidth \ 2 THEN
          NewHt = NewHt + 2 * HtInc
        ELSE
          NewHt = NewHt - 2 * HtInc
        END IF
    END SELECT

    'Set width of building and check to see if it would go off the screen
    BWidth = FNRan(DefBWidth) + DefBWidth
    IF x + BWidth > ScrWidth THEN BWidth = ScrWidth - x - 2

    'Set height of building and check to see if it goes below screen
    BHeight = FNRan(RandomHeight) + NewHt
    IF BHeight < HtInc THEN BHeight = HtInc

    'Check to see if Building is too high
    IF BottomLine - BHeight <= MaxHeight + GHeight THEN BHeight = MaxHeight + GHeight - 5

    'Set the coordinates of the building into the array
    BCoor(CurBuilding).XCoor = x
    BCoor(CurBuilding).YCoor = BottomLine - BHeight

    IF Mode = 9 THEN BuildingColor = FNRan(3) + 4 ELSE BuildingColor = 2

    'Draw the building, outline first, then filled
    LINE (x - 1, BottomLine + 1)-(x + BWidth + 1, BottomLine - BHeight - 1), BACKGROUND, B
    LINE (x, BottomLine)-(x + BWidth, BottomLine - BHeight), BuildingColor, BF

    'Draw the windows
    c = x + 3
    DO
      FOR i = BHeight - 3 TO 7 STEP -WDifV
        IF Mode <> 9 THEN
          WinColr = (FNRan(2) - 2) * -3
        ELSEIF FNRan(4) = 1 THEN
          WinColr = 8
        ELSE
          WinColr = WINDOWCOLOR
        END IF
        LINE (c, BottomLine - i)-(c + WWidth, BottomLine - i + WHeight), WinColr, BF
      NEXT
      c = c + WDifh
    LOOP UNTIL c >= x + BWidth - 3

    x = x + BWidth + 2

    CurBuilding = CurBuilding + 1

  LOOP UNTIL x > ScrWidth - HtInc

  LastBuilding = CurBuilding - 1

  'Set Wind speed
  Wind = FNRan(10) - 5
  IF FNRan(3) = 1 THEN
    IF Wind > 0 THEN
      Wind = Wind + FNRan(10)
    ELSE
      Wind = Wind - FNRan(10)
    END IF
  END IF

  'Draw Wind speed arrow
  IF Wind <> 0 THEN
    WindLine = Wind * 3 * (ScrWidth \ 320)
    LINE (ScrWidth \ 2, ScrHeight - 5)-(ScrWidth \ 2 + WindLine, ScrHeight - 5), ExplosionColor
    IF Wind > 0 THEN ArrowDir = -2 ELSE ArrowDir = 2
    LINE (ScrWidth / 2 + WindLine, ScrHeight - 5)-(ScrWidth / 2 + WindLine + ArrowDir, ScrHeight - 5 - 2), ExplosionColor
    LINE (ScrWidth / 2 + WindLine, ScrHeight - 5)-(ScrWidth / 2 + WindLine + ArrowDir, ScrHeight - 5 + 2), ExplosionColor
  END IF
END SUB

'PlaceGorillas:
'  PUTs the Gorillas on top of the buildings.  Must have drawn
'  Gorillas first.
'Parameters:
'  BCoor() - user-defined TYPE array which stores upper left coordinates
'  of each building.
SUB PlaceGorillas (BCoor() AS XYPoint)
    
  IF Mode = 9 THEN
    XAdj = 14
    YAdj = 30
  ELSE
    XAdj = 7
    YAdj = 16
  END IF
  SclX# = ScrWidth / 320
  SclY# = ScrHeight / 200
    
  'Place gorillas on second or third building from edge
  FOR i = 1 TO 2
    IF i = 1 THEN BNum = FNRan(2) + 1 ELSE BNum = LastBuilding - FNRan(2)

    BWidth = BCoor(BNum + 1).XCoor - BCoor(BNum).XCoor
    GorillaX(i) = BCoor(BNum).XCoor + BWidth / 2 - XAdj
    GorillaY(i) = BCoor(BNum).YCoor - YAdj
    PUT (GorillaX(i), GorillaY(i)), GorD&, PSET
  NEXT i

END SUB

'PlayGame:
'  Main game play routine
'Parameters:
'  Player$() - player names
'  NumGames - number of games to play
FUNCTION PlayGame (Player$(), NumGames, P)
 
 DIM BCoor(0 TO 30) AS XYPoint
 DIM minRounds
 DIM totalWins(1 TO 2)
 DIM avBan!(1 TO 2)                 ' mean accuracy
 DIM Throw(1 TO 2)                  ' throw counter
 DIM numHits(1 TO 2, 1 TO NumGames) ' number of throws needed to kill
                                     '  opponent per win for each player
 J = 1
 abortYN = FALSE
 minRounds = FIX(NumGames / 2) + 1

 i = 1
 DO
  CLS
  RANDOMIZE (TIMER)
  CALL MakeCityScape(BCoor())
  CALL PlaceGorillas(BCoor())
  DoSun SUNHAPPY
  GLeftAngle# = 0: GRightAngle# = 0
  GLeftVeloc = 0: GRightVeloc = 0
  Hit = FALSE: IF GSettings.useSound THEN PLAY "MBT160O1L8<G>CDEDCDL4ECC"
  go = 1
  DO WHILE Hit = FALSE
   J = 1 - J
   LOCATE 1, 2
   IF Mode = 9 THEN COLOR 12
   PRINT Player$(1);
   LOCATE 1, (MaxCol - LEN(Player$(2)))
   PRINT Player$(2);
   IF Mode = 9 THEN COLOR 9
   Center 23, STR$(totalWins(1)) + " > Score < " + LTRIM$(STR$(totalWins(2)) + " ")
   Tosser = J + 1: Tossee = 2 - J
  
   'Plot the shot.  Hit is true if Gorilla gets hit.
   Hit = DoShot(Player$(), Tosser, GorillaX(Tosser), GorillaY(Tosser), go, GorillaX(Tossee), GorillaY(Tossee))
   IF Hit = 1 THEN abortYN = TRUE: EXIT DO
   'If the throw was fatal, Tosser now contains the player who WON
  
   'If not hit self then increase number of hits
   IF (J + 1) = Tosser THEN Throw(Tosser) = Throw(Tosser) + 1
  
   IF Hit = TRUE THEN
    'Update scores
    totalWins(Tosser) = totalWins(Tosser) + 1
    IF (J + 1) = Tosser THEN numHits(Tosser, totalWins(Tosser)) = Throw(Tosser)
   END IF
  
   go = go + 1
 
  LOOP
 
  IF abortYN THEN EXIT DO
 
  Throw(1) = 0: Throw(2) = 0
  SLEEP 1

 i = i + 1
 LOOP UNTIL i > NumGames OR totalWins(1) >= minRounds OR totalWins(2) >= minRounds

 'þ If game played out then go through end game sequence
 IF NOT abortYN THEN
  GamePlayedYN = 1
  FOR l = 1 TO 2: Kills = 0
  IF totalWins(l) > 0 THEN
    FOR m = 1 TO totalWins(l)
     IF numHits(l, m) > 0 THEN
      avBan!(l) = avBan!(l) + numHits(l, m): Kills = Kills + 1
     END IF
    NEXT
    IF avBan!(l) > 0 THEN avBan!(l) = avBan!(l) / Kills
   END IF
  NEXT
 END IF
 
 SCREEN 0
 WIDTH 80, 25
 COLOR 7, 0
 MaxCol = 80
 CLS
 Stats totalWins(), Player$(), avBan!(), P, abortYN
 CLS : RESTORE NowWhat: Slidy
 LOCATE 2, 1: COLOR 2: PRINT STRING$(80, "Í")
 LOCATE 4, 4: PRINT "Another game? [Y/N]";
 DO
  in$ = UCASE$(INKEY$)
 LOOP UNTIL in$ = "Y" OR in$ = "N"
 IF in$ = "Y" THEN PlayGame = 1 ELSE PlayGame = 0

END FUNCTION

'PlayGame:
'  Plots banana shot across the screen
'Parameters:
'  StartX, StartY - starting shot location
'  Angle - shot angle
'  Velocity - shot velocity
'  PlayerNum - the banana thrower
FUNCTION PlotShot (StartX, StartY, angle#, velocity, PlayerNum, othX, othY)
  
 angleChk = angle#: IF PlayerNum = 2 THEN angleChk = 180 - angleChk
 
 angle# = angle# / 180 * pi#  'Convert degree angle to radians
 InitXVel# = COS(angle#) * velocity
 InitYVel# = SIN(angle#) * velocity
 oldx# = StartX
 oldy# = StartY

 ' draw gorilla toss
 IF PlayerNum = 1 THEN
  PUT (StartX, StartY), GorL&, PSET
 ELSE
  PUT (StartX, StartY), GorR&, PSET
 END IF
 ' throw sound
 IF GSettings.useSound THEN PLAY "MBO0L32A-L64CL16BL64A+"
 Rest .1
 ' redraw gorilla
 PUT (StartX, StartY), GorD&, PSET

 adjust = Scl(4)                   'For scaling CGA

 xedge = Scl(9) * (2 - PlayerNum)  'Find leading edge of banana for check

 Impact = FALSE
 SunHit = FALSE
 ShotInSun = FALSE
 OnScreen = TRUE 'þ FALSE if the banana is off side
 PlayerHit = 0
 NeedErase = FALSE
 Bounced = FALSE
 'þ Set up banana sound effect
 DoooMinVeloc = 40
 pitch! = 9800
 pitchDec! = 100
 pitchDecDec! = (((InitYVel# - DoooMinVeloc) / (200 - DoooMinVeloc)) * 1.2) - .5
 
 t2b# = 9999 'þ Used to store the time when the banana is to stop moving
             '  when continuing off screen. 9999 means unused.

 StartXPos = StartX
 StartYPos = StartY - adjust - 3

 IF PlayerNum = 2 THEN
  StartXPos = StartXPos + Scl(25)
  Direction = Scl(4)
 ELSE
  Direction = Scl(-4)
 END IF

 IF velocity < 2 THEN              'Shot too slow - hit self
  x# = StartX
  y# = StartY
  pointval = OBJECTCOLOR
 END IF
 
 'þ Obtain predicted x-coordinate when banana reaches bottom of screen
 GOSUB PredictBottomOfScreen
 'þ See if banana will overshoot (direction is +ve for left & -ve for right)
 'þ MissedDist# is -ve for miss, and +ve for hit
 IF Direction > 0 THEN
  MissedDist# = XPredicted#
 ELSE
  MissedDist# = ScrWidth - XPredicted#
 END IF
 'þ If shot is going backwards, then turns it into a miss
 IF SGN(Direction) = SGN(InitXVel#) THEN MissedDist# = 0 - MissedDist#

 DO WHILE (NOT Impact) AND OnScreen
  Rest .02

  'Erase old banana, if necessary
  IF NeedErase THEN
    NeedErase = FALSE
    CALL DrawBan(oldx#, oldy#, oldrot, FALSE)
  END IF

  x# = StartXPos + (InitXVel# * t#) + (.5 * (Wind / 5) * t# ^ 2)
  y# = StartYPos + ((-1 * (InitYVel# * t#)) + (.5 * Gravity * t# ^ 2)) * (ScrHeight / 350)
 
  IF y# > oldy# AND InitYVel# > DoooMinVeloc AND NOT Bounced AND MissedDist# > -175 THEN
   'þ Play banana sound effect
   IF GSettings.useSound THEN SOUND pitch!, 1
   'þ Decrement banana sound effect pitch
   IF (pitch! - pitchDec! >= 37) THEN
    pitch! = pitch! - pitchDec!: pitchDec! = pitchDec! - pitchDecDec!
   END IF
  END IF
  
  IF y# >= ScrHeight - 7 THEN
   'þ If velocity is still high enough to bounce, and banana is on screen
   IF InitYVel# > 2 AND t2b# = 9999 THEN
    Bounced = TRUE
    IF GSettings.useSound THEN PLAY "O4A64"
    InitYVel# = SQR(InitYVel# ^ 2 - (2 * Gravity * (StartYPos - (ScrHeight - 7)))) * .4
    StartXPos = x#
    y# = ScrHeight - 7: StartYPos = y#
    t# = 0
   ELSE
    'þ Terminate banana motion
    OnScreen = FALSE
    DoSun SUNHAPPY
    IF t2b# = 9999 THEN 'þ Because its velocity ran out
     IF GSettings.useSound THEN PLAY "O0A4"
    ELSE 'þ Or because it bounced when off screen
     IF SGN(Direction) <> SGN(InitXVel#) THEN GOSUB FailureMessage
    END IF
   END IF
  END IF
 
  'þ If banana leaves the screen
  IF (x# >= ScrWidth - Scl(10)) OR (x# <= 3) THEN
   'þ And banana will not return to the screen
   IF (XPredicted# >= ScrWidth - Scl(10)) OR (XPredicted# <= 3) THEN
    IF t# > t2b# THEN
     OnScreen = FALSE
     'þ Redraw sun as soon as poss
     '  Ignore SunHit: bananas can still take pieces out of the sun unnoticed
     DoSun SUNHAPPY
     IF SGN(Direction) <> SGN(InitXVel#) THEN
      GOSUB FailureMessage
     END IF
    ELSEIF t2b# = 9999 THEN
     IF y# <= 0 THEN t2b# = t# + 1.5 ELSE t2b# = t# + 4
    END IF
   END IF
  END IF

  IF OnScreen AND y# > 0 AND (x# > 3 AND x# < (ScrWidth - Scl(10))) THEN
   'check it
   LookY = 0
   LookX = Scl(8 * (2 - PlayerNum))
   
   DO
    pointval = POINT(x# + LookX, y# + LookY)
    IF pointval = 0 THEN
     Impact = FALSE
     IF ShotInSun = TRUE THEN
      IF ABS(ScrWidth \ 2 - x#) > Scl(20) OR y# > SunHt THEN ShotInSun = FALSE
     END IF
    ELSEIF pointval = SUNATTR AND y# < SunHt THEN
     IF NOT SunHit THEN DoSun SUNSHOCK
     SunHit = TRUE
     ShotInSun = TRUE
    ELSE
     Impact = TRUE
     DoSun SUNHAPPY
    END IF
    LookX = LookX + Direction
    LookY = LookY + Scl(6)
   LOOP UNTIL Impact OR LookX <> Scl(4)
   
   IF NOT ShotInSun AND NOT Impact THEN
    'plot it
    rot = (t# * 10) MOD 4
    CALL DrawBan(x#, y#, rot, TRUE)
    NeedErase = TRUE
   END IF
           
   oldrot = rot

  END IF
 
  oldx# = x#
  oldy# = y#
  t# = t# + .1
 LOOP

 IF pointval = OBJECTCOLOR THEN
  IF x# < ScrWidth / 2 THEN PlayerHit = 1 ELSE PlayerHit = 2
  IF PlayerHit = PlayerNum THEN
   IF Mode = 9 THEN COLOR 2
   DoSun SUNSHOCK ' hehehe
   Center 1, "Now that was pretty dumb."
  END IF
  ExplodeGorilla x#, y#, PlayerHit
  IF PlayerHit = PlayerNum THEN
   tpause! = TIMER: DO: LOOP UNTIL TIMER > tpause! + .75
   ' Center 1, SPACE$(25): DoSun SUNHAPPY
  END IF
 ELSEIF pointval <> OBJECTCOLOR AND Impact THEN
  CALL DoExplosion(x# + adjust, y# + adjust)
  'þ Reset values for shot's initial stage (before any bouncing)
  InitXVel# = COS(angle#) * velocity
  InitYVel# = SIN(angle#) * velocity
  StartXPos = StartX: IF PlayerNum = 2 THEN StartXPos = StartXPos + Scl(25)
  StartYPos = StartY - adjust - 3
  GOSUB PredictReturnToHeight
  'þ If shot went the right direction...
  IF SGN(Direction) <> SGN(InitXVel#) THEN
   'þ ...and if shot was too low powered:
   IF (ABS(XPredicted# - StartX) < ABS((othX - StartX) / 3) AND angleChk > 60) OR ABS(XPredicted# - StartX) < ABS((othX - StartX) / 6) THEN
    SELECT CASE FNRan(3)
     CASE 1:
      IF Mode = 9 THEN
       Message$ = "Aren't your little muscles strong enough?"
      ELSE
       Message$ = "Your little muscles not strong enough?"
      END IF
     CASE 2: Message$ = "Now that was feeble."
     CASE 3: Message$ = "You can do better than that!"
    END SELECT
    IF GSettings.useSound THEN PLAY "MBO2L24BAGFEDCO1C2"
    GOSUB DoMessage
    GOSUB RestoreSun
   END IF
  END IF
 END IF

 'redraw gorillas
 IF PlayerHit = 0 THEN
  PUT (StartX, StartY), GorD&, PSET
  PUT (othX, othY), GorD&, PSET
 END IF

 'þ Message for backwards-tossed shot
 IF SGN(Direction) = SGN(InitXVel#) AND PlayerHit <> PlayerNum THEN
  IF GSettings.useSound THEN PLAY "MBO1L24BAGFEDCO0C2"
  IF Mode = 9 THEN
   Message$ = "You're not supposed to throw it that way."
  ELSE
   Message$ = "Don't throw it that way!"
  END IF
  GOSUB DoMessage
  GOSUB RestoreSun
 END IF

 PlotShot = PlayerHit

EXIT FUNCTION

' When doing position calculation, don't forget -Gravity and Wind/5

PredictReturnToHeight:
 ' Prediction of the banana's x-coordinate when it has come down to a level
 ' horizontally equal with the gorilla that fired it.
 t2# = (2 * InitYVel#) / Gravity
 XPredicted# = (InitXVel# * t2#) + (.5 * (Wind / 5) * t2# ^ 2) + StartXPos
 IF PlayerNum = 2 THEN XPredictedRet# = XPredictedRet# + Scl(25)
RETURN

PredictBottomOfScreen:
 ' Prediction of the x-coordinate of the shot when it reaches the bottom of
 ' the screen
 fallDist = StartYPos - (ScrHeight - 7)
 t2# = (-InitYVel# - SQR((InitYVel# ^ 2) + (2 * (-Gravity) * fallDist))) / (-Gravity)
 XPredicted# = (InitXVel# * t2#) + ((t2# ^ 2 * Wind) / 10) + StartXPos
 IF PlayerNum = 2 THEN XPredicted# = XPredicted# + Scl(25)
RETURN

FailureMessage:
 'þ Select message based on distance beyond screen edge
 'þ NOT calibrated for CGA

 GiveDist = 0 'þ Flag to indicate whether to show distance travelled

 'þ If the player saw the banana leave the screen
 MissedDist# = ABS(MissedDist#)
 IF y# > 0 THEN
  SELECT CASE MissedDist#
   CASE 1 TO 155
    SELECT CASE FNRan(2)
     CASE 1: Message$ = "That went a wee bit far, didn't it?"
     CASE 2: Message$ = "It seems you overdid that a little."
    END SELECT
   CASE 156 TO 640
    SELECT CASE FNRan(4)
     CASE 1: Message$ = "I think you need glasses."
     CASE 2 TO 4: Message$ = "Hmmm...that wasn't good."
    END SELECT
   CASE 641 TO 1500: Message$ = "WHAT? That went MILES OFF!"
   CASE IS > 1500
    SELECT CASE FNRan(2)
     CASE 1: Message$ = "WHAT ARE YOU PLAYING AT?"
     CASE 2: Message$ = "Temper temper"
    END SELECT
  END SELECT
 ELSE
  SELECT CASE MissedDist#
   CASE 1 TO 155:
    IF Mode = 9 THEN
     Message$ = "A little nearer and you might stand a chance"
    ELSE
     Message$ = "A little nearer and you might make it."
    END IF
   CASE 156 TO 640:
    SELECT CASE FNRan(2)
     CASE 1: Message$ = "Nope. That was too far off."
     CASE 2: Message$ = CHR$(34) + "Hello? I'm over here!" + CHR$(34)
    END SELECT
   CASE 640 TO 1500
    SELECT CASE FNRan(2)
     CASE 1: Message$ = "Whoa! Go easy with it!"
     CASE 2: Message$ = "You must be JOKING!"
    END SELECT
   CASE IS > 1500:
    IF Mode = 9 THEN
     Message$ = "You weren't supposed to put it into orbit."
    ELSE
     Message$ = "Don't put it into orbit!"
    END IF
  END SELECT
 END IF

 IF GSettings.useSound THEN PLAY "MBO1L24BAGFEDCO0C2"
 GOSUB DoMessage
 GOSUB RestoreSun
RETURN

DoMessage:
 IF Mode = 9 THEN COLOR 2
 Center 1, Message$
 tpause! = TIMER: DO: LOOP UNTIL TIMER > tpause! + 2
 Center 1, SPACE$(LEN(Message$))
RETURN

RestoreSun:
 sunX = ScrWidth \ 2: sunY = Scl(25)
 LINE (sunX, sunY - Scl(15))-(sunX, sunY), SUNATTR
 LINE (sunX - Scl(8), sunY - Scl(13))-(sunX, sunY), SUNATTR
 LINE (sunX, sunY)-(sunX + Scl(8), sunY - Scl(13)), SUNATTR
RETURN

END FUNCTION

'Rest:
'  pauses the program
SUB Rest (t#)
  s# = TIMER
  t2# = 0
  ' t2# = MachSpeed * t#' / SPEEDCONST
  'þ Speed calibration disabled
  DO
  LOOP UNTIL TIMER - s# > t2#
END SUB

SUB RestReal (t#)
  s# = TIMER
  DO
  LOOP UNTIL TIMER - s# > t#
END SUB

'Scl:
'  Pass the number in to scaling for cga.  If the number is a decimal, then we
'  want to scale down for cga or scale up for ega.  This allows a full range
'  of numbers to be generated for scaling.
'  (i.e. for 3 to get scaled to 1, pass in 2.9)
FUNCTION Scl (N!)

  IF N! <> INT(N!) THEN
      IF Mode = 1 THEN N! = N! - 1
  END IF
  IF Mode = 1 THEN
      Scl = CINT(N! / 2 + .1)
  ELSE
      Scl = CINT(N!)
  END IF

END FUNCTION

'SetScreen:
'  Sets the appropriate color statements
SUB SetScreen

  IF Mode = 9 THEN
    ExplosionColor = 2
    BackColor = 1
    PALETTE 0, 1
    PALETTE 1, 46
    PALETTE 2, 44
    PALETTE 3, 54
    PALETTE 5, 7
    PALETTE 6, 4
    PALETTE 7, 3
    PALETTE 9, 63       'Display Color
    PALETTE 10, 24
    PALETTE 14, 55
  ELSE
    ExplosionColor = 2
    BackColor = 0
    COLOR BackColor, 2
  END IF

END SUB

SUB ShowPrompts (fieldNum)

 SELECT CASE fieldNum
  CASE 1 TO 2
   GOSUB pPlayers ' player list manipulation
    CASE 11
     GOSUB pDeletePlayer
    CASE 12, -12
     GOSUB pCreatePlayer
    CASE 13
     GOSUB pRenamePlayer
  CASE 3
   GOSUB pRounds
  CASE 4
   GOSUB pGravity
 END SELECT

EXIT SUB

pPlayers:
 COLOR , 0
 FOR l = 3 TO 6: LOCATE l, 1: PRINT STRING$(80, " "); : NEXT
 noOfDiams = 4: GOSUB Diamonds
 IF fieldNum = 1 THEN LOR$ = "LEFT" ELSE IF fieldNum = 2 THEN LOR$ = "RIGHT"
 COLOR 9: LOCATE 3, 5
 PRINT "Use arrow keys to choose " + LOR$ + " HAND player and press [ENTER] to confirm."
 LOCATE 4, 5: PRINT "Type [N] to create a new player (up to 20 players)."
 LOCATE 5, 5: PRINT "Type [R] to rename a player."
 LOCATE 6, 5: PRINT "Type [DELETE] to delete a player."
RETURN

pDeletePlayer:
 noOfDiams = 3: GOSUB Diamonds
 LOCATE 4, 5: COLOR 9: PRINT "Press [Y] to delete the player, OR"
 LOCATE 5, 5: PRINT "Press [N] to cancel"
RETURN

pCreatePlayer:
 COLOR , 0
 FOR l = 3 TO 6: LOCATE l, 1: PRINT STRING$(80, " "): NEXT
 noOfDiams = 1: GOSUB Diamonds
 'þ Used if the ESCAPE prompt is to be given
IF fieldNum = 12 THEN LOCATE 6, 3: PRINT ""
 COLOR 9
 LOCATE 3, 5: PRINT "Enter name of new player and press [ENTER] when done. You may as well"
 LOCATE 4, 5: PRINT "specify the player's full name as you only ever have to enter it"
 LOCATE 5, 5: PRINT "once."
 'þ Signals whether ESCAPE can be pressed
 IF fieldNum = 12 THEN LOCATE 6, 5: PRINT "Or press [ESC] to cancel."
RETURN

pRenamePlayer:
 COLOR , 0
 FOR l = 3 TO 6
  LOCATE l, 1: PRINT STRING$(80, " ")
 NEXT
 noOfDiams = 3: GOSUB Diamonds: COLOR 9
 LOCATE 3, 5: PRINT "Edit name of player and press [ENTER] when done."
 LOCATE 4, 5: PRINT "Pressing [DELETE] will clear the name field."
 LOCATE 5, 5: PRINT "Press [ESC] if you want to undo the changes."
RETURN

'

pRounds:
 COLOR , 0
 FOR l = 3 TO 6: LOCATE l, 1: PRINT STRING$(80, " "); : NEXT
 noOfDiams = 1: GOSUB Diamonds
 COLOR 9
 LOCATE 3, 5: PRINT "Enter input and press [ENTER] for the next field."
RETURN

pGravity:
 noOfDiams = 2: GOSUB Diamonds
 COLOR 9
 LOCATE 3, 5: PRINT "Enter input and press [ENTER] to finish and play the game."
 LOCATE 4, 5: PRINT "Or press [TAB] to return to the first entry."
 LOCATE 6, 5: PRINT "Competition gravity is 17 m/sý."
RETURN

'

Diamonds:
 COLOR 2
 FOR l = 3 TO (3 + (noOfDiams - 1))
LOCATE l, 3: PRINT ""
 NEXT
RETURN

END SUB

SUB Slidy

 DIM q AS LONG

 READ N

 DIM t$(1 TO N): DIM i(1 TO N, 1 TO 3)
 FOR l = 1 TO N
   READ P$: x = 40 - LEN(P$) / 2
   P$ = STRING$(x, " ") + P$ + STRING$(x, " ")
   READ i(l, 1), i(l, 2), i(l, 3)
   t$(l) = P$
 NEXT

 IF GSettings.useSlidingText THEN
  FOR la = 1 TO 80
   FOR lb = 1 TO N
    IF i(lb, 2) < 0 THEN
      P$ = LEFT$(t$(lb), la): x = 81 - la
    ELSE
      P$ = RIGHT$(t$(lb), la): x = 1
    END IF
    LOCATE i(lb, 3), x: COLOR i(lb, 1): PRINT P$;
   NEXT
   FOR q = 1 TO SLIDECONST: NEXT
  NEXT

 ELSE
  FOR lb = 1 TO N
   LOCATE i(lb, 3), 1: COLOR i(lb, 1): PRINT t$(lb)
  NEXT
 END IF

END SUB

'SparklePause:
'  Creates flashing border for intro and statistics screens
SUB SparklePause (opt AS INTEGER)
      
 DO: LOOP UNTIL INKEY$ = "" 'þ Clear keyboard buffer
 COLOR 12, 0
 a$ = "*    *    *    *    *    *    *    *    *    *    *    *    *    *    *    *    *    "
 t! = TIMER
 DO
   FOR a = 1 TO 5
     t1! = TIMER: DO: LOOP UNTIL TIMER > t1! + .001
     LOCATE 1, 1                             'print horizontal sparkles
     PRINT MID$(a$, a, 80);
     LOCATE 22, 1
     PRINT MID$(a$, 6 - a, 80);

     FOR b = 2 TO 21                         'Print Vertical sparkles
       c = (a + b) MOD 5
       IF c = 1 THEN
         LOCATE b, 80
         PRINT "*";
         LOCATE 23 - b, 1
         PRINT "*";
       ELSE
         LOCATE b, 80
         PRINT " ";
         LOCATE 23 - b, 1
         PRINT " ";
       END IF
     NEXT b
   NEXT a
 LOOP UNTIL INKEY$ <> "" OR (opt > 0 AND TIMER > t! + opt)

END SUB

SUB Stats (Wins(), nam$(), Ban!(), P, abortYN)

 IF abortYN THEN
  RESTORE Aborted: Slidy
  LOCATE 4, 3: COLOR 2: PRINT STRING$(76, "Í")
 ELSE
  'þ Update and sort the league table
  RESTORE GameOver: Slidy
  LOCATE 4, 3: COLOR 2: PRINT STRING$(76, "Í")
  FOR l = 1 TO 2
   PDat(PDefs(l)).Rounds = PDat(PDefs(l)).Rounds + Wins(1) + Wins(2)
   PDat(PDefs(l)).Won = PDat(PDefs(l)).Won + Wins(l)
   IF Ban!(l) > 0 THEN
    IF PDat(PDefs(l)).Accu > 0 THEN
     PDat(PDefs(l)).Accu = CINT(((PDat(PDefs(l)).Accu + Ban!(l)) / 2) * 10) / 10
    ELSE
     PDat(PDefs(l)).Accu = CINT(Ban!(l) * 10) / 10
    END IF
   END IF
  NEXT
  
  'þ routine to sort the player list
  DO
   complete = 1: tempW1 = 0: tempW2 = 0
   FOR l = 1 TO P - 1
    IF PDat(l).Rounds > 0 THEN tempW1 = (PDat(l).Won / PDat(l).Rounds * 100)
    IF PDat(l + 1).Rounds > 0 THEN tempW2 = (PDat(l + 1).Won / PDat(l + 1).Rounds * 100)
    IF (tempW1 < tempW2) OR (tempW1 = tempW2 AND PDat(l).Accu > PDat(l + 1).Accu) THEN
     SWAP PDat(l).PNam, PDat(l + 1).PNam
     SWAP PDat(l).Rounds, PDat(l + 1).Rounds
     SWAP PDat(l).Won, PDat(l + 1).Won
     SWAP PDat(l).Accu, PDat(l + 1).Accu
     FOR PDl = 1 TO 2
      IF PDefs(PDl) = l THEN
       PDefs(PDl) = PDefs(PDl) + 1
      ELSEIF PDefs(PDl) = l + 1 THEN
       PDefs(PDl) = PDefs(PDl) - 1
      END IF
     NEXT
    
     complete = 0
    END IF
   NEXT
  LOOP UNTIL complete
 
  FOR l = 1 TO 2
   IF Wins(1) <> Wins(2) THEN
    D = (Wins(l) >= Wins(2 / l))
    COLOR (D + 2) * 2: LOCATE 6 + D, 7
   ELSE
    COLOR 9: LOCATE 4 + l, 7
   END IF
   PRINT nam$(l); " "; STRING$(20 - LEN(nam$(l)), "Ä"); ""; Wins(l);
   IF (Wins(1) <> Wins(2)) THEN
     IF D = -1 THEN PRINT CHR$(27); "ÄÄÄÄ Winnar!";
   ELSEIF l = 1 THEN
    PRINT "   (The game was a draw)";
   END IF
   posn = 0: DO: posn = posn + 1: LOOP UNTIL nam$(l) = RTRIM$(PDat(posn).PNam)
   IF posn > 10 THEN PRINT TAB(54); "(position"; RTRIM$(STR$(posn)); "th)"
  NEXT
 END IF
 
 'þ Show league table no matter what
 LOCATE 8, 20: COLOR 9: PRINT "STATISTICS";
 LOCATE 9, 3: COLOR 2: PRINT "Ú"; STRING$(74, "Ä"); "¿";
 FOR l = 10 TO 20: LOCATE l, 3: PRINT "³"; TAB(78); "³"; : NEXT
 LOCATE 21, 3: PRINT "À"; STRING$(74, "Ä"); "Ù";
 COLOR 3
 LOCATE 9, 5:  PRINT "Place";
 LOCATE 9, 12: PRINT "Player";
 LOCATE 9, 32: PRINT "Rounds";
 LOCATE 9, 54: PRINT "Mean Accuracy";
 LOCATE 9, 40: PRINT "Won";

 COLOR 5: IF P > 9 THEN lim = 10 ELSE lim = P
 FOR l = 1 TO lim
  LOCATE l + 10, 6:
  IF (PDefs(1) = l OR PDefs(2) = l) AND NOT abortYN THEN COLOR 11 ELSE COLOR 5
  IF l < 10 THEN PRINT "0";
  PRINT LTRIM$(STR$(l)); " ÄÄ "; TAB(12); PDat(l).PNam
  COLOR 5: LOCATE l + 10, 31: PRINT PDat(l).Rounds; TAB(39); PDat(l).Won; TAB(45);
  IF PDat(l).Rounds = 0 THEN
   PRINT "-"; TAB(53);
  ELSE
   IF (PDefs(1) = l OR PDefs(2) = l) AND NOT abortYN THEN COLOR 11 ELSE COLOR 13
   PRINT ; "("; LTRIM$(RTRIM$(STR$(CINT(PDat(l).Won / PDat(l).Rounds * 100)))); "%)"; TAB(53);
  END IF
  COLOR 5
  IF PDat(l).Accu = 0 THEN
   PRINT ; " -"
  ELSE
   PRINT ; PDat(l).Accu;
   IF PDat(l).Accu > 1! THEN PRINT "bananas" ELSE PRINT "banana"
  END IF
 NEXT

 'þ Only save stats if they have changed or if file absent
 IF NOT abortYN OR DoesFileExist = 0 THEN
  COLOR 5: LOCATE 24, 3: PRINT "Saving stats...";

  ON ERROR GOTO NoSaveStats
  IF DoesFileExist = 1 THEN KILL "Gorillas.lge"
  OPEN "Gorillas.lge" FOR OUTPUT AS #1
  PRINT #1, P
  FOR l = 1 TO P
   PRINT #1, PDat(l).PNam
   PRINT #1, PDat(l).Rounds, PDat(l).Won, PDat(l).Accu
  NEXT
  CLOSE #1
  DoesFileExist = 1
  ON ERROR GOTO 0
 END IF

 COLOR 15: LOCATE 24, 3: PRINT "Press a key... ";
 SparklePause (0)

END SUB

'VictoryDance:
'  gorilla dances after he has eliminated his opponent
'Parameters:
'  Player - which gorilla is dancing
SUB VictoryDance (Player)
  FOR i# = 1 TO 4
    PUT (GorillaX(Player), GorillaY(Player)), GorL&, PSET
    IF GSettings.useSound THEN PLAY "MFO0L32EFGEFDC" ELSE RestReal .2
    Rest .2
    PUT (GorillaX(Player), GorillaY(Player)), GorR&, PSET
    IF GSettings.useSound THEN PLAY "MFO0L32EFGEFDC" ELSE RestReal .2
    Rest .2
  NEXT
END SUB

FUNCTION WhereX (num)

 WhereX = ((num - 1) MOD 4) * 19 + 3

END FUNCTION

FUNCTION WhereY (num)

 WhereY = INT((num - 1) / 4) + 13

END FUNCTION

