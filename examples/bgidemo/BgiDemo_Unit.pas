unit BgiDemo_Unit;

interface

// Borland demos
procedure Initialize;
procedure ReportStatus;                 // 0
procedure TextPlay;                     // 1
procedure BarPlay;                      // 2
procedure Bar3DPlay;                    // 3
procedure RandBarPlay;                  // 4
procedure CirclePlay(Buffered:boolean); // 5,6
procedure TextDump(font:integer);       // 7..11
procedure LineToPlay;                   // 12
procedure LineRelPlay;                  // 13
procedure LineStylePlay;                // 14
procedure ColorPlay;                    // 15
procedure PolyPlay(Buffered:boolean);   // 16,17
procedure FillStylePlay;                // 18
procedure FillPatternPlay;              // 19
procedure PutPixelPlay;                 // 20
procedure ArcPlay;                      // 21
procedure FillEllipsePlay;              // 22
procedure SectorPlay;                   // 23
procedure WriteModePlay;                // 24
procedure PiePlay;                      // 25

// Additional demos
procedure FloodFillPlay;                // 0
procedure FillBitmapPlay;               // 1
procedure GradientDemo;                 // 2
procedure UserFontDemo;                 // 3

procedure Delay(msec:word);

var
  UseOEMCharset: Boolean = false;


implementation

uses
  SysUtils, Graphics, LConvEncoding, lazBGI;

var
  MaxX, MaxY : integer;
  MaxColor : word;

const
  { The ten fonts available }
  Fonts : array[0..10] of string[17] =
  ('DefaultFont', 'TriplexFont', 'SmallFont', 'SansSerifFont', 'GothicFont',
   'ScriptFont', 'SimplexFont', 'TriplexScriptFont', 'ComplexFont',
   'EuropeanFont', 'BoldFont');

  { The five predefined line styles supported }
  LineStyles : array[0..4] of string[9] =
  ('SolidLn', 'DottedLn', 'CenterLn', 'DashedLn', 'UserBitLn');

  { The twelve predefined fill styles supported }
  FillStyles : array[0..11] of string[14] =
  ('EmptyFill', 'SolidFill', 'LineFill', 'LtSlashFill', 'SlashFill',
   'BkSlashFill', 'LtBkSlashFill', 'HatchFill', 'XHatchFill',
   'InterleaveFill', 'WideDotFill', 'CloseDotFill');

  { The two text directions available }
  TextDirect : array[0..1] of string[8] = ('HorizDir', 'VertDir');

  { The Horizontal text justifications available }
  HorizJust  : array[0..2] of string[10] = ('LeftText', 'CenterText', 'RightText');

  { The vertical text justifications available }
  VertJust   : array[0..2] of string[10] = ('BottomText', 'CenterText', 'TopText');

//------------------------------------------------------------------------------

{ Initialize graphics and report any errors that may occur }
procedure Initialize;
(*
var
  InGraphicsMode : boolean; { Flags initialization of graphics mode }
  PathToDriver   : string;  { Stores the DOS path to *.BGI & *.CHR }
*)  // in WinGraph unnötig
begin
(*
  { when using Crt and graphics, turn off Crt's memory-mapped writes }
  DirectVideo := False;
  OldExitProc := ExitProc;                { save previous exit proc }
  ExitProc := @MyExitProc;                { insert our exit proc in chain }
  PathToDriver := '';
  repeat


    VESA16 := InstallUserDriver('VESA16', @DetectVESA16);

{$IFDEF Use8514}                          { check for Use8514 $DEFINE }
    GraphDriver := IBM8514;
    GraphMode := IBM8514Hi;
{$ELSE}
    {GraphDriver := Detect;                { use autodetection }
    graphdriver := vga;
    graphmode := 2;
{$ENDIF}

    InitGraph(GraphDriver, GraphMode, PathToDriver);
    ErrorCode := GraphResult;             { preserve error return }
    if ErrorCode <> grOK then             { error? }
    begin
      Writeln('Graphics error: ', GraphErrorMsg(ErrorCode));
      if ErrorCode = grFileNotFound then  { Can't find driver file }
      begin
        Writeln('Enter full path to BGI driver or type <Ctrl-Break> to quit:');
        Readln(PathToDriver);
        Writeln;
      end
      else
        Halt(1);                          { Some other error: terminate }
    end;
  until ErrorCode = grOK;
*)

// The commented code above is not needed by lazGraph.

  Randomize;                { init random number generator }
  MaxColor := GetMaxColor;  { Get the maximum allowable drawing color }
  MaxX := GetMaxX;          { Get screen resolution values }
  MaxY := GetMaxY;
end; { Initialize }

{ Converts an integer to a string for use with OutText, OutTextXY }
function Int2Str(L : LongInt) : string;
var
  S : string;
begin
  Str(L, S);
  Int2Str := S;
end; { Int2Str }

{ Returns a Random non-zero color value that is within the legal
  color range for the selected device driver and graphics mode.
  MaxColor is set to GetMaxColor by Initialize }
function RandColor : word;
begin
  RandColor := Random(MaxColor)+1;
end; { RandColor }

{ Select the maximum color in the Palette for the drawing color }
procedure DefaultColors;
begin
  SetColor(MaxColor);
end; { DefaultColors }

{ Draw a border around the current view port }
procedure DrawBorder;
var
  ViewPort : ViewPortType;
begin
  DefaultColors;
  SetLineStyle(SolidLn, 0, NormWidth);
  GetViewSettings(ViewPort);
  with ViewPort do
    Rectangle(0, 0, x2-x1, y2-y1);
end; { DrawBorder }

{ Set the view port to the entire screen }
procedure FullPort;
begin
  SetViewPort(0, 0, MaxX, MaxY, ClipOn);
end; { FullPort }

{ Make a default window and view port for demos }
procedure MainWindow(Header : string);
begin
  DefaultColors;                           { Reset the colors }
  ClearDevice;                             { Clear the screen }
  SetTextStyle(DefaultFont, HorizDir, 1);  { Default text font }
  SetTextJustify(CenterText, TopText);     { Left justify text }
  FullPort;                                { Full screen view port }
  OutTextXY(MaxX div 2, 2, Header);        { Draw the header }
  { Draw main window }
  SetViewPort(0, TextHeight('M')+4, MaxX, MaxY-(TextHeight('M')+4), ClipOn);
  DrawBorder;                              { Put a border around it }
  { Move the edges in 1 pixel on all sides so border isn't in the view port }
  SetViewPort(1, TextHeight('M')+5, MaxX-1, MaxY-(TextHeight('M')+5), ClipOn);
end; { MainWindow }

{ Display a status line at the bottom of the screen }
procedure StatusLine(Msg : string);
begin
  FullPort;
  DefaultColors;
  SetTextStyle(DefaultFont, HorizDir, 1);
  SetTextJustify(CenterText, TopText);
  SetLineStyle(SolidLn, 0, NormWidth);
  SetFillStyle(SolidFill, 0);  // wp: war: EmptyFill ?!
  Bar(0, MaxY-(TextHeight('M')+4), MaxX, MaxY);      { Erase old status line }
  Rectangle(0, MaxY-(TextHeight('M')+4), MaxX, MaxY);
  OutTextXY(MaxX div 2, MaxY-(TextHeight('M')+2), Msg);
  { Go back to the main window }
  SetViewPort(1, TextHeight('M')+5, MaxX-1, MaxY-(TextHeight('M')+5), ClipOn);
end; { StatusLine }

{ Wait for the user to abort the program or continue }
procedure WaitToGo;
{
const
  Esc = #27;
var
  Ch : char;
}
begin
  StatusLine('Select the next demo from the list box...');
//  StatusLine('Esc aborts or press a key...');

// LCL: Cannot query key presses this way.
(*
  repeat until KeyPressed;
  Ch := ReadKey;
  if ch = #0 then ch := readkey;      { trap function keys }
  if Ch = Esc then
    Halt(0)                           { terminate program }
  else
    ClearDevice;                      { clear screen, go on with demo }
*)
end; { WaitToGo }

{ Return strings describing the current device driver and graphics mode
  for display of status report }
procedure GetDriverAndMode(out DriveStr, ModeStr: string);
begin
  DriveStr := GetDriverName;
  ModeStr := GetModeName(GetGraphMode);
end; { GetDriverAndMode }

{ Display the status of all query functions after InitGraph }
procedure ReportStatus;
const
  X = 10;
var
  ViewInfo   : ViewPortType;     { Parameters for inquiry procedures }
  LineInfo   : LineSettingsType;
  FillInfo   : FillSettingsType;
  TextInfo   : TextSettingsType;
//  Palette    : PaletteType;
  DriverStr  : string;           { Driver and mode strings }
  ModeStr    : string;
  Y          : word;

  procedure WriteOut(S : string);
  { Write out a string and increment to next line }
  begin
    OutTextXY(X, Y, S);
    Inc(Y, TextHeight('M')+2);
  end; { WriteOut }

begin { ReportStatus }
  GetDriverAndMode(DriverStr, ModeStr);   { Get current settings }
  GetViewSettings(ViewInfo);
  GetLineSettings(LineInfo);
  GetFillSettings(FillInfo);
  GetTextSettings(TextInfo);
//  GetPalette(Palette);

  Y := 4;
  MainWindow('Status report after InitGraph');
  SetTextJustify(LeftText, TopText);
  WriteOut('Graphics device    : '+DriverStr);
  WriteOut('Graphics mode      : '+ModeStr);
  WriteOut('Screen resolution  : (0, 0, '+Int2Str(GetMaxX)+', '+Int2Str(GetMaxY)+')');
  with ViewInfo do
  begin
    WriteOut('Current view port  : ('+Int2Str(x1)+', '+Int2Str(y1)+', '+Int2Str(x2)+', '+Int2Str(y2)+')');
    if Clip then
      WriteOut('Clipping           : ON')
    else
      WriteOut('Clipping           : OFF');
  end;
  WriteOut('Current position   : ('+Int2Str(GetX)+', '+Int2Str(GetY)+')');
//  WriteOut('Palette entries    : '+Int2Str(Palette.Size));
  WriteOut('GetMaxColor        : '+Int2Str(GetMaxColor));
  WriteOut('Current color      : '+Int2Str(GetColor));
  with LineInfo do
  begin
    WriteOut('Line style         : '+LineStyles[LineStyle]);
    WriteOut('Line thickness     : '+Int2Str(Thickness));
  end;
  with FillInfo do
  begin
    WriteOut('Current fill style : '+FillStyles[Pattern]);
    WriteOut('Current fill color : '+Int2Str(Color));
  end;
  with TextInfo do
  begin
    WriteOut('Current font       : '+Fonts[Font]);
    WriteOut('Text direction     : '+TextDirect[Direction]);
    WriteOut('Character size     : '+Int2Str(CharSize));
    WriteOut('Horizontal justify : '+HorizJust[Horiz]);
    WriteOut('Vertical justify   : '+VertJust[Vert]);
  end;
  WaitToGo;
end; { ReportStatus }

{ Random filled ellipse demonstration }
procedure FillEllipsePlay;
const
  MaxFillStyles = 12; { patterns 0..11 }
var
  MaxRadius : word;
  FillColor : integer;
  i: integer; // LCL;
begin
  MainWindow('FillEllipse demonstration');
//  StatusLine('Esc aborts or press a key');
  MaxRadius := MaxY div 10;
  SetLineStyle(SolidLn, 0, NormWidth);
//  repeat             // LCL: No input allowed in OnPaint.
  for i:=1 to 100 do begin
    FillColor := RandColor;
    SetColor(FillColor);
    SetFillStyle(Random(MaxFillStyles), FillColor);
    FillEllipse(Random(MaxX), Random(MaxY),
                Random(MaxRadius), Random(MaxRadius));
//  until KeyPressed; // LCL: No input allowed in OnPaint.
  end;
  WaitToGo;
end; { FillEllipsePlay }

{ Draw random sectors on the screen }
procedure SectorPlay;
const
  MaxFillStyles = 12; { patterns 0..11 }
var
  MaxRadius : word;
  FillColor : integer;
  EndAngle  : integer;
  i : integer; // Delphi
begin
  MainWindow('Sector demonstration');
//  StatusLine('Esc aborts or press a key');
  MaxRadius := MaxY div 10;
  SetLineStyle(SolidLn, 0, NormWidth);
  for i:=1 to 100 do begin  { LCL: No input allowed in OnPaint.
  repeat }
    FillColor := RandColor;
    SetColor(FillColor);
    SetFillStyle(Random(MaxFillStyles), FillColor);
    EndAngle := Random(360);
    Sector(Random(MaxX), Random(MaxY), Random(EndAngle), EndAngle,
           Random(MaxRadius), Random(MaxRadius)); 
  end; { LCL: No input allowed in OnPaint.
  until KeyPressed; }
  WaitToGo;
end; { SectorPlay }

{ Demonstrate the SetWriteMode procedure for XOR lines }
procedure WriteModePlay;
//const   // LCL: not needed
//  DelayValue = 50;  { milliseconds to delay }
var
  ViewInfo      : ViewPortType;
//  Color         : word;
  Left, Top     : integer;
  Right, Bottom : integer;
  Step          : integer; { step for rectangle shrinking }
  i, j : integer; // LCL
begin
  // LCL: In contrast to the original routine the graph is painted only once.
  // With the next click it will be XOR-painted again, i.e. will disappear.
  // It is important here that no random numbers are used any more.
  // Routine changed massively.

  MainWindow('SetWriteMode demonstration');
  GetViewSettings(ViewInfo);
  SetColor(GetMaxColor);
  SetTextStyle(DefaultFont, HorizDir, 1);
  SetTextJustify(CenterText, CenterText);
  SetWriteMode(XORPut);
  for j:=1 to 10  do begin
    Left := 0;
    Top := 0;
    with ViewInfo do
    begin
      Right := x2-x1;
      Bottom := y2-y1;
    end;
    Step := Bottom div 20;
    for i:=1 to 50 do begin
      Line(Left, Top, Right, Bottom);
      Line(Left, Bottom, Right, Top);
      Rectangle(Left, Top, Right, Bottom);
      inc(Left, step);
      inc(Top, step);
      dec(Right, step);
      dec(Bottom, step);
    end;
    Delay(100);
  end;

  SetWriteMode(CopyPut);                   { back to overwrite mode }
  WaitToGo;
end; { WriteModePlay }

{ Demonstrate text justifications and text sizing }
procedure TextPlay;
var
  Size : word;
  H, X, Y : word;
  ViewInfo : ViewPortType;
begin
  MainWindow('SetTextJustify / SetUserCharSize demo');
  GetViewSettings(ViewInfo);
  with ViewInfo do
  begin
    SetTextStyle(TriplexFont, VertDir, 4);
    Y := (y2-y1) - 2;
    SetTextJustify(CenterText, BottomText);
    OutTextXY(2*TextWidth('M'), Y, 'Vertical');
    SetTextStyle(TriplexFont, HorizDir, 4);
    SetTextJustify(LeftText, TopText);
    OutTextXY(2*TextWidth('M'), 2, 'Horizontal');
    SetTextJustify(CenterText, CenterText);
    X := (x2-x1) div 2;
    Y := TextHeight('H');
    for Size := 1 to 4 do
    begin
      SetTextStyle(TriplexFont, HorizDir, Size);
      H := TextHeight('M');
      Inc(Y, H);
      OutTextXY(X, Y, 'Size '+Int2Str(Size));
    end;
    Inc(Y, H div 2);
    SetTextJustify(CenterText, TopText);
//    SetUserCharSize(5, 6, 3, 2);
    SetTextStyle(TriplexFont, HorizDir, UserCharSize);
    OutTextXY((x2-x1) div 2, Y, 'User defined size! --- not implemented');
  end;
  WaitToGo;
end; { TextPlay }

{ Demonstrate Bar command }
procedure BarPlay;
const
  NumBars   = 5;
  BarHeight : array[1..NumBars] of byte = (1, 3, 5, 2, 4);
  Styles    : array[1..NumBars] of byte = (1, 3, 10, 5, 9);
var
  ViewInfo  : ViewPortType;
  H         : word;
  XStep     : real;
  YStep     : real;
  I, J      : integer;
  Color     : word;
begin
  MainWindow('Bar / Rectangle demonstration');
  H := 2*TextHeight('M');       // LCL: factor 2 instead of 3, looks better
  GetViewSettings(ViewInfo);
  SetTextJustify(CenterText, TopText);
  SetTextStyle(TriplexFont, HorizDir, 4);
  OutTextXY(MaxX div 2, 6, 'These are 2D bars !');
  SetTextStyle(DefaultFont, HorizDir, 1);
  with ViewInfo do
    SetViewPort(x1+50, y1+30, x2-50, y2-10, ClipOff);  // LCL: ClipOff instead of ClipOn
  GetViewSettings(ViewInfo);
  with ViewInfo do
  begin
    Line(H, H, H, (y2-y1)-H);
    Line(H, (y2-y1)-H, (x2-x1)-H, (y2-y1)-H);
    YStep := ((y2-y1)-(2*H)) / NumBars;
    XStep := ((x2-x1)-(2*H)) / NumBars;
    J := (y2-y1)-H;
    SetTextJustify(LeftText, CenterText);   // LCL: LeftText instead of CenterText

    { Draw Y axis with tick marks }
    for I := 0 to NumBars do
    begin
      Line(H div 2, J, H, J);
      OutTextXY(0, J, Int2Str(i));
      J := Round(J-Ystep);
    end;

    { Draw X axis, bars, and tick marks }
    J := H;
    SetTextJustify(CenterText, TopText);
    for I := 1 to Succ(NumBars) do
    begin
      SetColor(MaxColor);
      Line(J, (y2-y1)-H, J, (y2-y1-3)-(H div 2));
      OutTextXY(J, (y2-y1)-(H div 2), Int2Str(I));
      if I <> Succ(NumBars) then
      begin
        Color := RandColor;
        SetFillStyle(Styles[I], Color);
        SetColor(Color);
        Bar(J, round((y2-y1-H)-(BarHeight[I] * Ystep)), round(J+Xstep), (y2-y1)-H-1);
        Rectangle(J, round((y2-y1-H)-(BarHeight[I] * Ystep)), round(J+Xstep), (y2-y1)-H-1);
      end;
      J := Round(J+Xstep);
    end;

  end;
  WaitToGo;
end; { BarPlay }

{ Demonstrate Bar3D command }
procedure Bar3DPlay;
const
  NumBars   = 7;  { The number of bars drawn }
  BarHeight : array[1..NumBars] of byte = (1, 3, 2, 5, 4, 2, 1);
  YTicks    = 5;  { The number of tick marks on the Y axis }
var
  ViewInfo : ViewPortType;
  H        : word;
  XStep    : real;
  YStep    : real;
  I, J     : integer;
  Depth    : word;
  Color    : word;
begin
  MainWindow('Bar3D / Rectangle demonstration');
  H := 2*TextHeight('M');   // LCL: Factor 2 instead of 3, looks better...
  GetViewSettings(ViewInfo);
  SetTextJustify(CenterText, TopText);
  SetTextStyle(TriplexFont, HorizDir, 4);
  OutTextXY(MaxX div 2, 6, 'These are 3D bars !');
  SetTextStyle(DefaultFont, HorizDir, 1);
  with ViewInfo do
    SetViewPort(x1+50, y1+40, x2-50, y2-10, ClipOff);  // LCL: ClipOff instead of ClipOn
  GetViewSettings(ViewInfo);
  with ViewInfo do
  begin
    Line(H, H, H, (y2-y1)-H);
    Line(H, (y2-y1)-H, (x2-x1)-H, (y2-y1)-H);
    YStep := ((y2-y1)-(2*H)) / YTicks;
    XStep := ((x2-x1)-(2*H)) / NumBars;
    J := (y2-y1)-H;
    SetTextJustify(LeftText, CenterText);   // LCL: LeftText instead of CenterText

    { Draw the Y axis and ticks marks }
    for I := 0 to Yticks do
    begin
      Line(H div 2, J, H, J);
      OutTextXY(0, J, Int2Str(I));
      J := Round(J-Ystep);
    end;

    Depth := trunc(0.25 * XStep);    { Calculate depth of bar }

    { Draw X axis, bars, and tick marks }
    SetTextJustify(CenterText, TopText);
    J := H;
    for I := 1 to Succ(NumBars) do
    begin
      SetColor(MaxColor);
      Line(J, (y2-y1)-H, J, (y2-y1-3)-(H div 2));
      OutTextXY(J, (y2-y1)-(H div 2), Int2Str(I-1));
      if I <> Succ(NumBars) then
      begin
        Color := RandColor;
        SetFillStyle(I, Color);
        SetColor(Color);
        Bar3D(J, round((y2-y1-H)-(BarHeight[I] * Ystep)),
                 round(J+Xstep-Depth), round((y2-y1)-H-1), Depth, TopOn);
        J := Round(J+Xstep);
      end;
    end;

  end;
  WaitToGo;
end; { Bar3DPlay }

{ Draw random bars on the screen }
procedure RandBarPlay;
var
  MaxWidth  : integer;
  MaxHeight : integer;
  ViewInfo  : ViewPortType;
  Color     : word;
  n : integer;  // Delphi
begin
  MainWindow('Random Bars');
//  StatusLine('Esc aborts or press a key');  // LCL: No input allowed in OnPaint.
  GetViewSettings(ViewInfo);
  with ViewInfo do
  begin
    MaxWidth := x2-x1;
    MaxHeight := y2-y1;
  end;
  for n:=1 to 100 do begin
    Color := RandColor;
    SetColor(Color);
    SetFillStyle(Random(CloseDotFill)+1, Color);

    Bar(Random(MaxWidth), Random(MaxHeight), Random(MaxWidth), Random(MaxHeight));
    
  //    Bar3D(Random(MaxWidth), Random(MaxHeight),
  //          Random(MaxWidth), Random(MaxHeight), 0, TopOff);
  end;
  WaitToGo;
end; { RandBarPlay }

{ Draw random circles on the screen
  Buffered: Test for buffered painting. }
procedure CirclePlay(Buffered: boolean);
var
  MaxRadius: word;
  i: Integer;
begin
  if Buffered then DrawToBuffer;
  // LCL: Initialize the random number generator to get reproducible so that
  // the graph does not change with every repaint.
  RandSeed := 0;

  MainWindow('Circle demonstration');
//  StatusLine('Esc aborts or press a key'); // LCL: not input
  MaxRadius := MaxY div 10;
  SetLineStyle(SolidLn, 0, NormWidth);
  for i:=1 to 1000 do begin
    SetColor(RandColor);
    Circle(Random(MaxX), Random(MaxY), Random(MaxRadius));
  end;
  WaitToGo;

  // LCL: Buffered = Test for buffered drawing
  if Buffered then ShowBuffer;
end; { CirclePlay }

{ Dump the complete character sets to the screen }
procedure TextDump(font:integer);
const
  CGASizes  : array[0..10] of word = (1, 3, 7, 3, 3, 3, 3, 3, 3, 1, 1);
  NormSizes : array[0..10] of word = (1, 4, 7, 4, 4, 4, 4, 4, 4, 2, 2);
var
  ViewInfo : ViewPortType;
  Ch : char;
begin
// LCL: no input allowed in OnPaint
//  for Font := 0 to 10 do
//  begin
    MainWindow(Fonts[Font]+' character set');
    GetViewSettings(ViewInfo);
    with ViewInfo do
    begin
      SetTextJustify(LeftText, TopText);
      MoveTo(2, 3);
      if Font = DefaultFont then
        begin
          SetTextStyle(Font, HorizDir, 1);
          Ch := #0;
          repeat
            // LCL: Take care of character encoding
            if UseOEMCharset then
              OutText(CP437ToUTF8(ch))
            else
              OutText(CP1252ToUTF8(ch));
            if (GetX + TextWidth('M')) > (x2-x1) then
              MoveTo(2, GetY + TextHeight('M')+3);
            Ch := Succ(Ch);
          until (Ch >= #255);
        end
      else
        begin
          if MaxY < 200 then
            SetTextStyle(Font, HorizDir, CGASizes[Font])
          else
            SetTextStyle(Font, HorizDir, NormSizes[Font]);
          Ch := '!';
          repeat
            // LCL: take care of character encoding
            if UseOEMCharset then
              OutText(CP437ToUTF8(ch))
            else
              OutText(CP1252ToUTF8(ch));
            if (GetX + TextWidth('M')) > (x2-x1) then
              MoveTo(2, GetY + TextHeight('M')+3);
            Ch := Succ(Ch);
          until (Ch >= #255);
        end;
    end; { with }
    WaitToGo;
//  end; { for loop }
end; { TextDump }

{ Demonstrate MoveTo and LineTo commands }
procedure LineToPlay;
const
  MaxPoints = 15;
var
  Points     : array[0..MaxPoints] of PointType;
  ViewInfo   : ViewPortType;
  I, J       : integer;
  CenterX    : integer;   { The center point of the circle }
  CenterY    : integer;
  Radius     : word;
  StepAngle  : word;
  Xasp, Yasp : word;
  Radians    : real;

function AdjAsp(Value : integer) : integer;
{ Adjust a value for the aspect ratio of the device }
begin
  AdjAsp := (LongInt(Value) * Xasp) div Yasp;
end; { AdjAsp }

begin
  MainWindow('MoveTo, LineTo demonstration');
  GetAspectRatio(Xasp, Yasp);
  GetViewSettings(ViewInfo);
  with ViewInfo do
  begin
    CenterX := (x2-x1) div 2;
    CenterY := (y2-y1) div 2;
    Radius := CenterY;
    while (CenterY+AdjAsp(Radius)) < (y2-y1)-20 do
      Inc(Radius);
  end;
  StepAngle := 360 div MaxPoints;
  for I := 0 to MaxPoints - 1 do
  begin
    Radians := (StepAngle * I) * Pi / 180;
    Points[I].X := CenterX + round(Cos(Radians) * Radius);
    Points[I].Y := CenterY - AdjAsp(round(Sin(Radians) * Radius));
  end;
  Circle(CenterX, CenterY, Radius);
  for I := 0 to MaxPoints - 1 do
  begin
    for J := I to MaxPoints - 1 do
    begin
      MoveTo(Points[I].X, Points[I].Y);
      LineTo(Points[J].X, Points[J].Y);
    end;
  end;
  WaitToGo;
end; { LineToPlay }

{ Demonstrate MoveRel and LineRel commands }
procedure LineRelPlay;
const
  MaxPoints = 12;
var
  Poly     : array[1..MaxPoints] of PointType; { Stores a polygon for filling }
  CurrPort : ViewPortType;

{ Draw a Tesseract on the screen with relative move and
  line drawing commands, also create a polygon for filling }
procedure DrawTesseract;
const
  CheckerBoard : FillPatternType = (0, $10, $28, $44, $28, $10, 0, 0);
var
  X, Y, W, H   : integer;

begin
  GetViewSettings(CurrPort);
  with CurrPort do
  begin
    W := (x2-x1) div 9;
    H := (y2-y1) div 8;
    X := ((x2-x1) div 2) - round(2.5 * W);
    Y := ((y2-y1) div 2) - (3 * H);

    { Border around viewport is outer part of polygon }
    Poly[1].X := 0;     Poly[1].Y := 0;
    Poly[2].X := x2-x1; Poly[2].Y := 0;
    Poly[3].X := x2-x1; Poly[3].Y := y2-y1;
    Poly[4].X := 0;     Poly[4].Y := y2-y1;
    Poly[5].X := 0;     Poly[5].Y := 0;
    MoveTo(X, Y);

    { Grab the whole in the polygon as we draw }
    MoveRel(0, H);      Poly[6].X := GetX;  Poly[6].Y := GetY;
    MoveRel(W, -H);     Poly[7].X := GetX;  Poly[7].Y := GetY;
    MoveRel(4*W, 0);    Poly[8].X := GetX;  Poly[8].Y := GetY;
    MoveRel(0, 5*H);    Poly[9].X := GetX;  Poly[9].Y := GetY;
    MoveRel(-W, H);     Poly[10].X := GetX; Poly[10].Y := GetY;
    MoveRel(-4*W, 0);   Poly[11].X := GetX; Poly[11].Y := GetY;
    MoveRel(0, -5*H);   Poly[12].X := GetX; Poly[12].Y := GetY;

    { Fill the polygon with a user defined fill pattern }
    SetFillPattern(CheckerBoard, MaxColor);
    FillPoly(12, Poly);
    //DrawPoly(12, Poly);

    MoveRel(W, -H);
    LineRel(0, 5*H);   LineRel(2*W, 0);    LineRel(0, -3*H);
    LineRel(W, -H);    LineRel(0, 5*H);    MoveRel(0, -5*H);
    LineRel(-2*W, 0);  LineRel(0, 3*H);    LineRel(-W, H);
    MoveRel(W, -H);    LineRel(W, 0);      MoveRel(0, -2*H);
    LineRel(-W, 0);

    { Flood fill the center }
    FloodFill((x2-x1) div 2, (y2-y1) div 2, MaxColor);
  end;
end; { DrawTesseract }

begin
  MainWindow('LineRel / MoveRel demonstration');
  GetViewSettings(CurrPort);
  with CurrPort do
    { Move the viewport out 1 pixel from each end }
    SetViewPort(x1-1, y1-1, x2+1, y2+1, ClipOn);
  DrawTesseract;
  WaitToGo;
end; { LineRelPlay }

{ Demonstrate the predefined line styles available }
procedure LineStylePlay;
var
  Style    : word;
  Step     : word;
  X, Y     : word;
  ViewInfo : ViewPortType;
  H : integer; // LCL: Text height
begin
  ClearDevice;
  DefaultColors;
  MainWindow('Pre-defined line styles');
  GetViewSettings(ViewInfo);
  with ViewInfo do
  begin
    X := 35;
    Y := 10;
    Step := (x2-x1) div 15;    // LCL: 15 rather than 11 due to more line types
    SetTextJustify(LeftText, TopText);
    H := TextHeight('M');      // LCL
    OutTextXY(X, Y, 'NormWidth');
    SetTextJustify(CenterText, TopText);
    for Style := 0 to 5 do     // LCL: extended from 3 to 5
    begin
      SetLineStyle(Style, 0, NormWidth);
      { LCL: the following original code assumes a fixed character height:
        Line(X, Y+20, X, Y2-40);
        OutTextXY(X, Y2-30, Int2Str(Style));
      Better: }
      Line(X, Y+2*H, X, Y2-4*H);
      OutTextXY(X, Y2-3*H, Int2Str(Style));

      Inc(X, Step);
    end;
    Inc(X, 2*Step);
    SetTextJustify(LeftText, TopText);
    OutTextXY(X, Y, 'ThickWidth');
    SetTextJustify(CenterText, TopText);
    for Style := 0 to 5 do     // LCL: extended from 3 to 5
    begin
      SetLineStyle(Style, 0, ThickWidth);
      { LCL: see above...
        Line(X, Y+20, X, Y2-40);
        OutTextXY(X, Y2-30, Int2Str(Style)); }
      Line(X, Y+2*H, X, Y2-4*H);
      OutTextXY(X, Y2-3*H, Int2Str(Style));
      Inc(X, Step);
    end;
  end;
  SetTextJustify(LeftText, TopText);
  WaitToGo;
end; { LineStylePlay }

{ Display all of the colors available for the current driver and mode }
procedure ColorPlay;
var
  Color    : word;
  Width    : word;
  Height   : word;
  X, Y     : word;
  I, J     : word;
  ViewInfo : ViewPortType;

procedure DrawBox(X, Y : word);
begin
  SetFillStyle(SolidFill, Color);
  SetColor(Color);
  with ViewInfo do
    Bar(X, Y, X+Width, Y+Height);
  Rectangle(X, Y, X+Width, Y+Height);
  Color := GetColor;
  if Color = 0 then
  begin
    SetColor(MaxColor);
    Rectangle(X, Y, X+Width, Y+Height);
  end;
  OutTextXY(X+(Width div 2), Y+Height+4, Int2Str(Color));
  Color := Succ(Color) mod (MaxColor + 1);
end; { DrawBox }

begin
  MainWindow('Color demonstration');
  Color := 1;
  GetViewSettings(ViewInfo);
  with ViewInfo do
  begin
    Width := 2 * ((x2+1) div 16);
    Height := 2 * ((y2-10) div 10);
  end;
  X := Width div 2;
  Y := Height div 2;
  for J := 1 to 3 do
  begin
    for I := 1 to 5 do
    begin
      DrawBox(X, Y);
      Inc(X, (Width div 2) * 3);
    end;
    X := Width div 2;
    Inc(Y, (Height div 2) * 3);
  end;
  WaitToGo;
end; { ColorPlay }

{ Draw random polygons with random fill styles on the screen
  Buffered: demonstrate buffered painting }
procedure PolyPlay(Buffered:Boolean);
const
  MaxPts = 5;
type
  PolygonType = array[1..MaxPts] of PointType;
var
  Poly : PolygonType;
  I, Color : word;
  n : integer; // LCL
begin
  if Buffered then DrawToBuffer;
  RandSeed := 0;
  // LCL: Initialize the random number generator so that all shapes look the same
  // with each redraw.

  MainWindow('FillPoly demonstration');
//  StatusLine('Esc aborts or press a key...');  // LCL: No input allowed
  for n:= 1 to 100 do begin
//  repeat
    Color := RandColor;
    SetFillStyle(Random(11)+1, Color);
    SetColor(Color);
    for I := 1 to MaxPts do
      with Poly[I] do
      begin
        X := Random(MaxX);
        Y := Random(MaxY);
      end;
    FillPoly(MaxPts, Poly);
//  until KeyPressed;
// LCL: No input allowed in OnPaint event!
  end;
  WaitToGo;

  if Buffered then ShowBuffer;  // LCL: Demonstrate buffered painting
end; { PolyPlay }

{ Display all of the predefined fill styles available }
procedure FillStylePlay;
var
  Style    : word;
  Width    : word;
  Height   : word;
  X, Y     : word;
  I, J     : word;
  ViewInfo : ViewPortType;

procedure DrawBox(X, Y : word);
begin
  SetFillStyle(Style, MaxColor);
  with ViewInfo do
    Bar(X, Y, X+Width, Y+Height);
  Rectangle(X, Y, X+Width, Y+Height);
  OutTextXY(X+(Width div 2), Y+Height+4, Int2Str(Style));
  Inc(Style);
end; { DrawBox }

begin
  MainWindow('Pre-defined fill styles');
  GetViewSettings(ViewInfo);
  with ViewInfo do
  begin
    Width := 2 * ((x2+1) div 13);
    Height := 2 * ((y2-10) div 10);
  end;
  X := Width div 2;
  Y := Height div 2;
  Style := 0;
  for J := 1 to 3 do
  begin
    for I := 1 to 4 do
    begin
      DrawBox(X, Y);
      Inc(X, (Width div 2) * 3);
    end;
    X := Width div 2;
    Inc(Y, (Height div 2) * 3);
  end;
  SetTextJustify(LeftText, TopText);
  WaitToGo;
end; { FillStylePlay }

{ Display some user defined fill patterns }
procedure FillPatternPlay;
const
  Patterns : array[0..11] of FillPatternType = (
  ($AA, $55, $AA, $55, $AA, $55, $AA, $55),
  ($33, $33, $CC, $CC, $33, $33, $CC, $CC),
  ($F0, $F0, $F0, $F0, $F, $F, $F, $F),
  (0, $10, $28, $44, $28, $10, 0, 0),
  (0, $70, $20, $27, $25, $27, $4, $4),
  (0, 0, 0, $18, $18, 0, 0, 0),
  (0, 0, $3C, $3C, $3C, $3C, 0, 0),
  (0, $7E, $7E, $7E, $7E, $7E, $7E, 0),
  (0, 0, $22, $8, 0, $22, $1C, 0),
  ($FF, $7E, $3C, $18, $18, $3C, $7E, $FF),
  (0, $10, $10, $7C, $10, $10, 0, 0),
  (0, $42, $24, $18, $18, $24, $42, 0));
var
  Style    : word;
  Width    : word;
  Height   : word;
  X, Y     : word;
  I, J     : word;
  ViewInfo : ViewPortType;

procedure DrawBox(X, Y : word);
begin
  SetFillPattern(Patterns[Style], MaxColor);
  with ViewInfo do
    Bar(X, Y, X+Width, Y+Height);
  Rectangle(X, Y, X+Width, Y+Height);
  Inc(Style);
end; { DrawBox }

begin
  MainWindow('User defined fill styles');
  GetViewSettings(ViewInfo);
  with ViewInfo do
  begin
    Width := 2 * ((x2+1) div 13);
    Height := 2 * ((y2-10) div 10);
  end;
  X := Width div 2;
  Y := Height div 2;
  Style := 0;
  for J := 1 to 3 do
  begin
    for I := 1 to 4 do
    begin
      DrawBox(X, Y);
      Inc(X, (Width div 2) * 3);
    end;
    X := Width div 2;
    Inc(Y, (Height div 2) * 3);
  end;
  SetTextJustify(LeftText, TopText);
  WaitToGo;
end; { FillPatternPlay }

{ Demonstrate the PutPixel and GetPixel commands }
procedure PutPixelPlay;
const
//  Seed   = 1962; { A seed for the random number generator }
  NumPts = 2000; { The number of pixels plotted }
//  Esc    = #27;
var
  I : word;
  X, Y, Color : word;
  XMax, YMax  : integer;
  ViewInfo    : ViewPortType;
  n : integer;
  seed : integer;  // Delphi
begin
  MainWindow('PutPixel / GetPixel demonstration');
//  StatusLine('Esc aborts or press a key...');   // LCL: No input allowed

  GetViewSettings(ViewInfo);
  with ViewInfo do
  begin
    XMax := (x2-x1-1);
    YMax := (y2-y1-1);
  end;

  seed := 1962;
//  while not KeyPressed do
  for n:= 1 to 10 do   // LCL: No input allowed in OnPaint
  begin
    { Plot random pixels }
    RandSeed := Seed;
    I := 0;
    while {(not KeyPressed) and} (I < NumPts) do
    begin
      Inc(I);
      PutPixel(Random(XMax)+1, Random(YMax)+1, RandColor);
    end;

    { Erase pixels }
    RandSeed := Seed;
    I := 0;
    while {(not KeyPressed) and} (I < NumPts) do
    begin
      Inc(I);
      X := Random(XMax)+1;
      Y := Random(YMax)+1;
      Color := GetPixel(X, Y);
      if Color = RandColor then PutPixel(X, Y, 0);
    end;
    inc(seed); // LCL
  end;
  WaitToGo;
end; { PutPixelPlay }

{ Draw random arcs on the screen }
procedure ArcPlay;
var
  MaxRadius : word;
  EndAngle : word;
  ArcInfo : ArcCoordsType;
  n : integer; // Delphi
begin
  MainWindow('Arc / GetArcCoords demonstration');
//  StatusLine('Esc aborts or press a key');  //LCL: No input allowed
  MaxRadius := MaxY div 10;
  for n:=1 to 100 do begin  // LCL: No input allowed in OnPaint!
//  repeat
    SetColor(RandColor);
    EndAngle := Random(360);
    SetLineStyle(SolidLn, 0, NormWidth);
    Arc(Random(MaxX), Random(MaxY), Random(EndAngle), EndAngle, Random(MaxRadius));
    GetArcCoords(ArcInfo);
    with ArcInfo do
    begin
      Line(X, Y, XStart, YStart);
      Line(X, Y, Xend, Yend);
    end;
//  until KeyPressed;
  end;  // LCL
  WaitToGo;
end; { ArcPlay }

{ Demonstrate  PieSlice and GetAspectRatio commands }
procedure PiePlay;
var
  ViewInfo   : ViewPortType;
  CenterX    : integer;
  CenterY    : integer;
  Radius     : word;
  Xasp, Yasp : word;
  X, Y       : integer;

function AdjAsp(Value : integer) : integer;
{ Adjust a value for the aspect ratio of the device }
begin
  AdjAsp := (LongInt(Value) * Xasp) div Yasp;
end; { AdjAsp }

procedure GetTextCoords(AngleInDegrees, Radius : word; out X, Y : integer);
{ Get the coordinates of text for pie slice labels }
var
  Radians : real;
begin
  Radians := AngleInDegrees * Pi / 180;
  X := round(Cos(Radians) * Radius);
  Y := round(Sin(Radians) * Radius);
end; { GetTextCoords }

begin
  MainWindow('PieSlice / GetAspectRatio demonstration');
  GetAspectRatio(Xasp, Yasp);
  GetViewSettings(ViewInfo);
  with ViewInfo do
  begin
    CenterX := (x2-x1) div 2;
    CenterY := ((y2-y1) div 2) + 20;
    Radius := (y2-y1) div 3;
    while AdjAsp(Radius) < round((y2-y1) / 3.6) do
      Inc(Radius);
  end;
  SetTextStyle(TriplexFont, HorizDir, 4);
  SetTextJustify(CenterText, TopText);
  OutTextXY(CenterX, 0, 'This is a pie chart!');

  SetTextStyle(TriplexFont, HorizDir, 3);

  SetFillStyle(SolidFill, RandColor);
  PieSlice(CenterX+10, CenterY-AdjAsp(10), 0, 90, Radius);
  GetTextCoords(45, Radius, X, Y);
  SetTextJustify(LeftText, BottomText);
  OutTextXY(CenterX+10+X+TextWidth('H'), CenterY-AdjAsp(10+Y), '25 %');

  SetFillStyle(HatchFill, RandColor);
  PieSlice(CenterX, CenterY, 225, 360, Radius);
  GetTextCoords(293, Radius, X, Y);
  SetTextJustify(LeftText, TopText);
  OutTextXY(CenterX+X+TextWidth('H'), CenterY-AdjAsp(Y), '37.5 %');

  SetFillStyle(InterleaveFill, RandColor);
  PieSlice(CenterX-10, CenterY, 135, 225, Radius);
  GetTextCoords(180, Radius, X, Y);
  SetTextJustify(RightText, CenterText);
  OutTextXY(CenterX-10+X-TextWidth('H'), CenterY-AdjAsp(Y), '25 %');

  SetFillStyle(WideDotFill, RandColor);
  PieSlice(CenterX, CenterY, 90, 135, Radius);
  GetTextCoords(112, Radius, X, Y);
  SetTextJustify(RightText, BottomText);
  OutTextXY(CenterX+X-TextWidth('H'), CenterY-AdjAsp(Y), '12.5 %');

  WaitToGo;
end; { PiePlay }


{===============================================================================
                      Additional demo routines
===============================================================================}

procedure FloodFillPlay;
var
  r, x, y : integer;
  ViewInfo : ViewPortType;
begin
  MainWindow('FloodFill demonstration');
  GetViewSettings(ViewInfo);
  with ViewInfo do begin
    if (x2-x1) > (y2-y1) then r := (y2-y1)
      else r := x2-x1;
    r := r * 9 div 20;
    x := (x2-x1) div 2;
    y := (y2-y1) div 2;
    SetColor(yellow);
    Circle(x, y, r);
    Circle(x, y, r div 2);
    SetFillStyle(XHatchFill, LightRed);
    FloodFill(x+r *3 div 4, y, yellow);
  end;
  WaitToGo;
end; { FloodFillPlay }

procedure FillBitmapPlay;
var
  viewInfo : ViewPortType;
  img : TCustomBitmap;
begin
  MainWindow('FillBitmap Demonstration');
  GetViewSettings(ViewInfo);
  img := TPortableNetworkGraphic.Create;
  try
    img.LoadFromFile('paw.png');
    SetFillBitmap(img);
    Bar(10, 10, GetmaxX-10, GetMaxY-10);
    WaitToGo;
  finally;
    img.Free;
  end;
end;

procedure GradientDemo;
const
  d = 2;
var
  Viewinfo : ViewportType;
  w : integer;
  x : integer;
  y : integer;
begin
  MainWindow('GradientDemo');
  GetViewSettings(ViewInfo);
  with ViewInfo do begin
    x := x2 - x1;
    y := y2 - y1;
    w := x div 2;
    GradientRect(d, d, w-d div 2, y-d, clRed, clBlue, VertDir);
    GradientRect(w+d div 2, d, x-d, y-d, clMaroon, clYellow, HorizDir);
  end;
  WaitToGo;
end;

procedure UserFontDemo;
var
  MyFont : integer;
  ViewInfo : ViewportType;
  x,y : integer;
begin
  MainWindow('Install User Font');
  GetViewSettings(ViewInfo);
  MyFont := InstallUserFont('Symbol',
    [8, 10, 12, 14, 16, 18, 20, 24, 28, 36], []);
  SetTextStyle(MyFont, HorizDir, 6);
  SetTextJustify(CenterText, CenterText);
  with ViewInfo do begin
    x := (x2 - x1) div 2;
    y := (y2 - y1) div 2;
    OutTextXY(x, y, 'Ein Text in Symbol...');
  end;
  WaitToGo;
end;

//==============================================================================

procedure Delay(msec: word);
var
  t0 : TDateTime;
  delta : TDateTime;
begin
  t0 := now;
  delta := msec / (24*60*60*1000);
  repeat until (now-t0) >= delta;
end;

end.


