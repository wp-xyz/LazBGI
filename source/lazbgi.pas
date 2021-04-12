(*******************************************************************************
                                   lazBGI
--------------------------------------------------------------------------------
Emulates Turbo Pascal's BGI commands for Lazarus

Introduction:

- Add a descendant of TGraphicControl (TPaintbox) or TCustomControl (TPanel) to
  the form. A paintbox is recommended, but it is also possible to pain
  directly on a form.
  The BGI painting then is executed on the rectangle defined by this control.
  The top/left corner of this control has the BGI coordinates (0, 0), and the
  bottom/right corner has (GetMaxX, GetMaxY).

- BGI commands must begin with "InitGraph". In contrast to BGI, the routine does
  not provide the graphics driver, but the name of the control onto which the
  output is painted, e.g. "Paintbox1" or "Form1".

- Then the rest of the BGI commands follow like in an original ancient DOS
  program, such as "Line", "OutText", "Circle" etc.

- "CloseGraph" at the end of the graphic output is no longer absolutely required
  any more since the system cannot be switched back to text mode.
  CloseGraph, however, cleans up memory usage - it will be called automatically
  at the end of the program anyway.

- The graphics commands must be called from a routine of the painting cycle of
  the container control. In case of TPaintBox, TPanel or TForm, this is the
  OnPaint event.

- Note that the OnPaint handler must be able to paint the entire control.

- In DOS, often the same function was painted over the same graphic with
  different parameters - this was possible because of the persistent screen
  memory. This is not possible any more because the OS can reqest a complete
  repaint at any time and thus erase the previous drawing.

- Also be prepared of surprises when random numbers are used for some drawing
  parameters, such as in Borland's BGIDEMO.

- The BGI painting routine, by no means, must be allowed to wait for user input
  like in the original BGIDEMO. User input must be handled by the usual
  LCL OnKey* and OnMouse* events outside the painting routine.

- If nevertheless several curves are to be painted above each other, or if
  a flicker-free animation is supposed to be shown then the BGI graphic can
  be buffered:

  - When the graph is supposed to be drawn upon a button click the drawing
    commands must be put into the OnClick event handler of the button; the
    last command must be "DrawToBuffer" which copies the graphic from the
    screen into a buffer bitmap.

    procedure TForm1.Button1Click(Sender: TObject);
    begin
      InitGraph(Paintbox1);
      {... BGI graphics commands ... }
      DrawToBuffer;
    end;

    It is also possible to call DrawToBuffer immediately after InitGraph which
    requires to trigger repainting by calling Paintbox1.Invalidate.

  - In the OnPaint handler of the Paintbox (Panel, Form, ...) the graphic must
    be copied to the screen by calling ShowBuffer

    procedure TForm1.Paintbox1Paint(Sender: TObject);
    begin
      ShowBuffer;
    end;


Differences to BGI:

- Palette function are not emulated. Colors are rendered by the modern OS in a
  much more general way then by the BGI. Access to colors defined by constants
  "Black", "Red", "LightGray" etc is possible. More than that, colors can also
  be defined by "SetRGBColor" and "SetRGBBkColor" (for line/text and background
  fill, respectively). Unlike in the OS, texts are not painted in their own
  color, but by the line color.

- Page switching by SetActivePage/SetVisualPage is not supported at the moment.

- Line types are displayed correctly only with line thickness 1 (NormWidth),
  otherwise only solid lines are shown. Unlike in the BGI, line thicknesses
  other than 1 (NormWidth) or 3 (ThickWidth) can be selected.
  New line type DashDotDotLn.
  User-defined line patterns are not supported at the moment.

- The LCL provides less fill patterns as the BGI. Therefore, all non-empty and
  non-solid fill patterns are emulated by means of bitmaps, in the same way
  as user-defined fill patterns. Moreover, a rectangle can also be filled by
  a linear color gradient ("GradientRect").

- Text output uses the same fonts as the LCL, Borlands CHR vector fonts are not
  supported. Using SetDefaultFont, SetTriplexFont, etc. you can define which
  fonts of the OS are supposed to be used instead of the BGI fonts (DefaultFont,
  Triplex, Small, SansSerif, Gothic), and which font sizes (in points) will be
  selected for the 10 BGI font sizes. Be prepared for differences to the
  original BGI fonts. "RegisterUserFont" is not supported. "InstallUserFont" is
  supported, however, but with different calling parameters (now allowing
  OS font names).
  Similarly every font available in the system can be used for text output by
  using the function SetTextFont.

- DrawPoly and FillPoly draw also the connection between outer and inner polygons
*******************************************************************************)

unit lazBGI;

{ Missing:
- Palettes
- SetActivePage

Problems:
- PutImage using different WriteModes
}

interface

uses
  LCLIntf,
  Classes, SysUtils, Graphics, Controls;


{ BGI declarations }

const
  // Bar3D constants
  TopOn = true;
  TopOff = false;

  // BitBlt constants
  NormalPut = 0;
  CopyPut = 0;
  XORPut = 1;
  OrPut = 2;
  AndPut = 3;
  NotPut = 4;

  // Clipping constants
  ClipOff = false;
  ClipOn = true;

  // color constants
  MaxColors = 15;

  Black = 0;
  Blue = 1;
  Green = 2;
  cyan = 3;
  Red = 4;
  Magenta = 5;
  Brown = 6;
  LightGray = 7;
  DarkGray = 8;
  LightBlue = 9;
  LightGreen = 10;
  LightCyan = 11;
  LightRed = 12;
  LightMagenta = 13;
  Yellow = 14;
  White = 15;

  /// error constants, for reasons of compatiblity only
  grOK = 0;
  grNoInitGraph = -1;
  grNotDetected = -2;
  grFileNotFound = -3;
  grInvalidDriver = -4;
  grNoLoadMem = -5;
  grNoScanMem = -6;
  grNoFloodMem = -7;
  grFontNotFound = -8;
  grNoFontMem = -9;
  grInvalidMode = -10;
  grError = -11;
  grIOError = -12;
  grInvalidFont = -13;
  grInvalidFontNum = -14;


  // Fill pattern constants
  EmptyFill = 0;
  SolidFill = 1;
  LineFill = 2;       // ---
  LtSlashFill = 3;    // ///
  SlashFill = 4;      // /// with thick lines
  BkSlashFill = 5;    // \\\ with thick lines
  LtBkSlashFill = 6;  // \\\
  HatchFill = 7;      // ++++
  XHatchFill = 8;     // xxxx
  InterleaveFill = 9; // dashed lines
  WideDotFill = 10;   // dots with large spacing
  CloseDotFill = 11;  // dots with small spacing
  UserFill = 12;      // user-defined pattern

  // Text alignment in SetTextJustify
  LeftText = 0;
  CenterText = 1;
  RightText = 2;
  BottomText = 0;
  TopText = 2;

  // Line style types
  SolidLn = 0;
  DottedLn = 1;
  CenterLn = 2;
  DashedLn = 3;
  UserBitLn = 4;
  DashDotLn = CenterLn;           // new
  DashDotDotLn = 5;               // new

  // Line widths
  NormWidth = 1;
  ThickWidth = 3;

  // Font types, text display
  DefaultFont = 0;
  TriplexFont = 1;
  SmallFont = 2;
  SansserifFont = 3;
  GothicFont = 4;

  HorizDir = 0;
  VertDir = 1;
  UserCharSize = 0;

type
  ArcCoordsType = record
    X, Y,
    XStart, YStart,
    XEnd, YEnd : integer;
  end;

  FillPatternType = array[1..8] of byte;

  FillSettingsType = record
    Pattern   : word;
    Color     : word;
  end;

  LineSettingsType = record
    LineStyle : word;
    Pattern   : word;
    Thickness : word;
  end;

  PaletteType = record
    Size: byte;
    Colors: Array[0..maxColors] of ShortInt;
  end;

  PointType = record
    X, Y: integer;
  end;

  TextSettingsType = record
    Font: word;
    Direction: word;
    CharSize: word;
    Horiz: word;
    Vert: word;
  end;

  ViewportType = record
    x1, y1, x2, y2: integer;
    Clip: boolean;
  end;


{ BGI routines }

procedure Arc(x, y: integer; StAngle, EndAngle, Radius: word);
procedure Bar(x1, y1, x2, y2: integer);
procedure Bar3D(x1, y1, x2, y2: integer; ADepth: word; ATop: boolean);
procedure Circle(x, y, r: integer);
procedure ClearDevice;
procedure CloseGraph;
procedure DrawPoly(NumPoints: integer; var PolyPoints);
procedure Ellipse(x, y: integer; StAngle, EndAngle, xRadius, yRadius: word);
procedure FillEllipse(x, y: integer; xRadius, yRadius: word);
procedure FillPoly(NumPoints: integer; var PolyPoints);
procedure FloodFill(x, y: integer; ABorder: word);
procedure GetArcCoords(out ArcCoords: ArcCoordsType);
procedure GetAspectRatio(out xasp, yasp: word);
function GetBkColor: word;
function GetColor: word;
function GetDriverName: string;
procedure GetFillSettings(out FillInfo: FillSettingsType);
function GetGraphMode: integer;
procedure GetImage(x1, y1, x2, y2: integer; ABitmap: TBitmap);
procedure GetLineSettings(out LineInfo: LineSettingsType);
function GetMaxColor: word;
function GetMaxX: integer;
function GetMaxY: integer;
function GetModeName({%H-}AMode: integer): string;
function GetPixel(x, y: integer): word;
procedure GetTextSettings(out TextInfo: TextSettingsType);
function GetX: integer;
function GetY: integer;
procedure GraphDefaults;
function GraphErrorMsg({%H-}AErrorCode: integer): string;
function GraphResult: integer;
procedure GetViewSettings(out ViewPort: ViewportType);
function ImageSize({%H-}x1, {%H-}y1, {%H-}x2, {%H-}y2: integer): word;
procedure InitGraph(AControl: TControl);  // NOTE: Parameters changed!
function InstallUserDriver({%H-}AName: string; {%H-}AutoDetectPtr: Pointer): integer;
function InstallUserFont(AFontName: string; ASizes: array of integer; AStyle: TFontStyles): integer;  // NOTE: Parameters changed!
procedure Line(x1, y1, x2, y2: Integer);
procedure LineRel(dx, dy: integer);
procedure LineTo(x, y: integer);
procedure MoveRel(dx, dy: integer);
procedure MoveTo(x, y: integer);
procedure OutText(s: string);
procedure OutTextXY(x, y: integer; s: string);
procedure PieSlice(x, y: integer; StAngle, EndAngle, Radius: word);
procedure PutImage(x, y: integer; ABitmap: TBitmap; AMode: word);
procedure PutPixel(x, y: integer; AColor:word);
procedure Rectangle(x1, y1, x2, y2: integer);
function RegisterBGIdriver({%H-}ADriver: Pointer): integer;
function RegisterBGIFont({%H-}AFont: Pointer): integer;
procedure Sector(x, y: integer; StAngle, EndAngle, xRadius, yRadius: word);
procedure SetBkColor(AColorNum: word);
procedure SetFgColor(AColor: word);
procedure SetColor(AColor: word);  // DANGER to call inherited TControl.Color!
procedure SetFillPattern(APattern: FillPatternType; AColor: word);
procedure SetFillStyle(APattern, AColor: word);
procedure SetLineStyle(ALinestyle, APattern, AThickness: word);
procedure SetTextJustify(Horiz, Vert: word);
procedure SetTextStyle(AFont, ADirection, ACharsize: word);
procedure SetUserCharSize({%H-}MultX, {%H-}DivX, {%H-}MultY, {%H-}DivY: word);
procedure SetViewPort(x1, y1, x2, y2: integer; AClip: boolean);
procedure SetWriteMode(AMode: integer);
function  TextHeight(s: string): integer;
function  TextWidth(s: string): integer;

{ Additional declarations and routines }

type
  EGraphError = Exception;

// Buffering
procedure DrawToBuffer;
procedure DrawToScreen;
procedure ShowBuffer;

// Colors
function GetRGBBkColor : TColor;
function GetRGBColor : TColor;
procedure SetRGBBkColor(AColor:TColor);
procedure SetRGBColor(AColor:TColor);

// Fonts
procedure GetTextFont(out AFontname: TFontName; out ASize: integer; out AStyle: TFontStyles);
procedure SetSansSerifFont(AFontname: TFontName; ASizes: array of integer; AStyle: TFontStyles);
procedure SetDefaultFont(AFontname: TFontName; ASizes: array of integer; AStyle: TFontStyles);
procedure SetGothicFont(AFontname: TFontName; ASizes: array of integer; Astyle: TFontStyles);
procedure SetSmallFont(AFontname: TFontName; ASizes: array of integer; AStyle: TFontStyles);
procedure SetTriplexFont(AFontname: TFontName; ASizes: array of integer; AStyle: TFontStyles);
procedure SetTextFont(AFontname: TFontName; ASize: integer; AStyle: TFontStyles);

// Fill
procedure GradientRect(x1, y1, x2, y2: Integer; StartColor, EndColor:TColor; ADirection: integer);
procedure SetFillBitmap(ABitmap: TCustomBitmap);

// Misc
function GetCanvas: TCanvas;
procedure SetBgiControl(AControl: TControl);


implementation

uses
  Math,
  Forms;
  
{ Interfacing to LCL controls

  Not every TGraphicControl has a public Canvas property. In these cases,
  type-casting to TBgiXXXControl gives access to the Canvas.}
type
  // for TPaintbox
  TBgiGraphicControl = class(TGraphicControl)
  end;

  // for TPanel
  TBgiCustomControl = class(TCustomControl)
  end;

var
  _Canvas : TCanvas;           // Canvas on which painting occurs
  _ActiveCanvas : TCanvas;     // Current canvas, control or buffer bitmap
  _BgiBuffer : TBitmap;        // Buffer bitmap
  _IsBuffered : boolean;       // Painting is buffered
  _CanvasRect : TRect;         // Size of canvas on the BGI control
  _ClipOrigin : TPoint;        // Origin of the canvas on the BGI control
  _ArcCoords : ArcCoordsType;  // Coordinates for "Arc"
  _Viewport : ViewportType;    // Current viewport


{ Font management }

type
  TFontSizeArray = array[1..10] of integer;
  TFontRec = record
    Name : TFontName;
    Sizes : TFontSizeArray;
    Style : TFontStyles;
  end;
  TFontArray = array[DefaultFont..10] of TFontRec;

var
  _Fonts : TFontArray;         // Array with max 10(+1) BGI fonts
  _NumFonts : integer;         // Count of available fonts in array _Fonts
  _FontName : string;          // Currently used font
  _FontSize : integer;         // Point size of the currently used font
  _FontStyle : TFontStyles;    // Style attributes of the currently used font
  _BgiFont : integer;          // Current font as BGI number
  _BgiSize : integer;          // BGI size of the current font
  _TextAlignHor : integer;     // Current alignment: LeftText...
  _TextAlignVert : integer;    // ... TopText...
  _TextDir : integer;          // Current writing direction (HorizDir...)


{ Area fills
  There are no LCL brushes which exactly correspond to the BGI fills. Therefore,
  the fill patterns are created manually. }
const
  Empty_Fill     : FillpatternType = ($00,$00,$00,$00,$00,$00,$00,$00);
  Solid_Fill     : FillPatternType = ($FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF);
  Line_Fill      : FillpatternType = ($FF,$FF,$00,$00,$FF,$FF,$00,$00);
  Lt_Slash_Fill  : FillPatternType = ($01,$02,$04,$08,$10,$20,$40,$80);
  Slash_Fill     : FillPatternType = ($E0,$C1,$83,$07,$0E,$1C,$38,$70);
  Backslash_Fill : FillPatternType = ($F0,$78,$3C,$1E,$0F,$87,$C3,$E1);
  Lt_Bkslash_Fill: FillPatternType = ($80,$40,$20,$10,$08,$04,$02,$01);
  Hatch_Fill     : FillPatternType = ($FF,$88,$88,$88,$FF,$88,$88,$88);
  XHatch_Fill    : FillPatternType = ($81,$42,$24,$18,$18,$24,$42,$81);
  Interleave_Fill: FillPatternType = ($F0,$00,$00,$00,$0F,$00,$00,$00);
  Wide_Dot_Fill  : FillPatternType = ($80,$00,$08,$00,$80,$00,$08,$00);
  Close_Dot_Fill : FillPatternType = ($88,$00,$22,$00,$88,$00,$22,$00);

var
  User_Fill      : FillPatternType = ($00,$00,$00,$00,$00,$00,$00,$00);

const
  _FillPatterns : Array[EmptyFill..UserFill] of ^FillPatternType =
    (@Empty_Fill, @Solid_Fill, @Line_Fill, @Lt_Slash_Fill, @Slash_Fill,
     @BackSlash_Fill, @Lt_BkSlash_Fill, @Hatch_Fill, @XHatch_Fill,
     @InterLeave_Fill, @Wide_Dot_Fill, @Close_Dot_Fill, @User_Fill);

var
  _FillSettings : FillSettingsType;  // Current fill settings
  _FillBitmap : TBitmap;             // Bitmap for area fills


{ Color management
  The VGA color of the BGI do not correspond exactly to LCL TColors. The
  following colors are extracted from a BGI program and extended from range
  0..63 to 0..255. }
const
  _Black = clBlack;             // R=0 G=0 B=0
  _Blue = $00800000;            // R=0 G=0 B=170
  _Green = $00008000;           // R=0 G=170 B=0
  _Cyan = $00808000;            // R=0 G=170 B=170
  _Red = $00000080;             // R=170 G=0 B=0
  _Magenta = $00800080;         // R=170 G=0 B=170
  _Brown = $00004080;           // R=170 G=85 B=0
  _LightGray = $00808080;       // R=170 G=170 B=170
  _DarkGray = $00404040;        // R=85 G=85 B=85
  _LightBlue = $00FF4040;       // R=85 G=85 B=255
  _LightGreen = $0040FF40;      // R=85 G=255 B=85
  _LightCyan = $00FFFF40;       // R=85 G=255 B=255
  _LightRed = $004040FF;        // R=255 G=85 B=85
  _LightMagenta = $00FF40FF;    // R=255 G=85 B=255
  _Yellow = clYellow;           // R=255 G=255 B=85
  _White = clWhite;             // R=255 G=255 B=255

  MaxVGAColor = 15;

  // These are the 16 VGA colors
  VGAColors : array[0..maxVGAColor] of TColor = (_Black, _Blue, _Green, _Cyan,
    _Red, _Magenta, _Brown, _LightGray, _DarkGray, _LightBlue, _LightGreen,
    _LightCyan, _LightRed, _LightMagenta, _Yellow, _White);

var
  _RGB_Color : TColor;           // Painting forground color as RGB
  _RGB_BkColor : TColor;         // Background color as RGB


{ Strings for error messages }

resourcestring
  SNoCanvas = 'Canvas was not assigned.';
  SNoBitmap = 'No bitmap created.';
  SNotImplemented = 'Function not implemented.';
  SInvalidFont = 'Invalid font.';
  SInvalidPattern = 'Invalid fill pattern.';
  SInvalidAncestor = 'Invalid component (has no Canvas).';

{===============================================================================
                              Local routines
===============================================================================}

{ Checks whether "SegBgiControl has been called as first procedure and
  raises an exception otherwise. }
procedure CheckCanvas;
begin
  if not Assigned(_Canvas)
    then raise EGraphError.Create(SNoCanvas);
  if _IsBuffered
    then _ActiveCanvas := _BgiBuffer.Canvas
    else _ActiveCanvas := _Canvas;
end;

{ Checks whether the provided bitmap has been created and raises an
  exception otherwise. }
procedure CheckBitmap(ABitmap: TBitmap);
begin
  if not Assigned(ABitmap) then
    raise EGraphError.Create(SNoBitmap);
end;

{ Raises an exception for important BGI functions which are not implemented. }
procedure NotImplementedError;
begin
  raise EGraphError.Create(SNotImplemented);
end;

{ Raises an exception when the font to be used is not valid. }
procedure InvalidFontError;
begin
  raise EGraphError.Create(SInvalidFont);
end;

{ Raises an exception when then fill pattern to be used is not valid }
procedure InvalidPatternError;
begin
  raise EGraphError.Create(SInvalidPattern);
end;

{ Transforms the global point (x,y) (global with respect to GetMaxX and GetMaxY)
  to the local coordinate system defined by the currenly set viewport. }
procedure LocalPoint(var x,y: Integer);
begin
  x := x - _ViewPort.x1;
  y := y - _ViewPort.y1;
end;

{ Transforms the point (x, y) from the local viewport coordinate system to
  "global" coordinates of the painting area (GetMaxX, GetMaxY). }
procedure GlobalPoint(var x,y: Integer);
begin
  x := _ViewPort.x1 + x;
  y := _ViewPort.y1 + y;
end;

{ Exchanges the integers x and y }
procedure Swap(var x,y: Integer);
var
  tmp : integer;
begin
  tmp := x;
  x := y;
  y := tmp;
end;

{ Makes sure that x1 < x2 and y1 < y2 }
procedure CheckCorners(var x1,y1,x2,y2:integer);
begin
  if x2 < x1 then Swap(x1, x2);
  if y2 < y1 then Swap(y1, y2);
end;

{ Converts an LCL RGB-Color to a BGI color (0..15) }
function ColorToPalette(AColor: TColor): word;
var
  i: integer;
begin
  for i := 0 to MaxVGAColor do
    if AColor = VGAColors[i] then
    begin
      Result := i;
      exit;
    end;
  Result := 0;
end;

procedure UseAsBkColor(AColor: TColor);
begin
  _RGB_BkColor := AColor;
  _ActiveCanvas.Brush.Color := AColor;
end;

procedure UseAsColor(AColor: TColor);
begin
  with _ActiveCanvas do begin
    Pen.Color := AColor;
    Font.Color := AColor;
  end;
end;

{ Converts a BGI color (0...15) to an LCL RGB TColor }
function PaletteToColor(APalette: word): TColor;
begin
  if APalette <= GetMaxColor then
    Result := VGAColors[APalette]
  else
    Result := 0;
end;

{ Converts a BGI pen style to an LCL TPenStyle. }
function PenStyle(AStyle: integer): TPenStyle;
begin
  case AStyle of
    SolidLn      : Result := psSolid;
    DottedLn     : Result := psDot;
    DashedLn     : Result := psDash;
    CenterLn     : Result := psDashDot;
    DashDotDotLn : Result := psDashDotDot;
    else           Result := psSolid;
  end;
end;

{ Converts an LCL TPenStyle to a BGI pen style }
function BgiLinestyle(AStyle: TPenStyle): word;
begin
  case AStyle of
    psSolid      : Result := SolidLn;
    psDot        : Result := DottedLn;
    psDash       : Result := DashedLn;
    psDashDot    : Result := CenterLn;
    psDashDotDot : Result := DashDotDotLn;
    else           Result := SolidLn;
  end;
end;

{ Stores the given parameters in the _Fonts array at index id. }
procedure SetFontParams(id: integer; AName: string; ASizes: TFontSizeArray;
  AStyle: TFontStyles);
begin
  _Fonts[id].Name := AName;
  Move(ASizes, _Fonts[id].Sizes, SizeOf(ASizes));
  _Fonts[id].Style := AStyle;
end;

{ Initializes the _Fonts array }
procedure InitFonts;
var
  i: integer;
begin
  SetDefaultFont('Courier New', [10, 12, 14, 16, 20, 24, 28, 32, 38, 48, 72], []);
  SetGothicFont('Allegro BT', [10, 12, 14, 16, 20, 24, 28, 32, 38, 48, 72], []);
  SetSansserifFont('Arial', [10, 12, 14, 16, 20, 24, 28, 32, 38, 48, 72], []);
  SetSmallFont('Arial Narrow', [6, 7, 8, 10, 12, 14, 16, 20, 24, 32], []);
  SetTriplexFont('Times New Roman', [10, 12, 14, 16, 20, 24, 28, 32, 38, 48, 72], []);
  _NumFonts := 4;
  for i:=_NumFonts+1 to 10 do begin
    with _Fonts[i] do begin
      Name := '';
      Style := [];
    end;
  end;
end;


{===============================================================================
                               BGI routines
===============================================================================}

// like BGI.
procedure Arc(x,y:integer; StAngle,EndAngle,Radius:word);
var
  x1,y1, x2,y2, x3,y3, x4,y4 : integer;
  phi : double;
begin
  CheckCanvas;
  _ArcCoords.x := x;
  _ArcCoords.y := y;
  x1 := x - Radius;
  y1 := y - Radius;
  GlobalPoint(x1,y1);

  x2 := x + Radius;
  y2 := y + Radius;
  GlobalPoint(x2,y2);

  phi := degToRad(StAngle);
  x3 := x + Round(Radius*cos(phi));
  y3 := y - round(Radius*sin(phi));
  _ArcCoords.XStart := x3;
  _ArcCoords.YStart := y3;
  GlobalPoint(x3,y3);

  phi := DegToRad(EndAngle);
  x4 := x + round(Radius*cos(phi));
  y4 := y - round(Radius*sin(phi));
  _ArcCoords.XEnd := x4;
  _ArcCoords.YEnd := y4;
  GlobalPoint(x4, y4);

  _ActiveCanvas.Arc(x1,y1, x2,y2, x3,y3, x4,y4);
end;

procedure Bar(x1,y1,x2,y2:integer);
var
  penstyle : TPenStyle;
begin
  CheckCanvas;
  inc(x2);  // LCL ignores the right/bottom corner
  inc(y2);
  GlobalPoint(x1,y1);
  GlobalPoint(x2,y2);
  CheckCorners(x1,y1, x2,y2);
  with _ActiveCanvas do begin
    penstyle := Pen.Style;
    Pen.Style := psClear;
    Rectangle(x1, y1, x2, y2);
    Pen.Style := penstyle;
  end;
end;

{ Paints a 3D bar like the BGI. The front face extends between the corner points
  (x1, y1) and (x2, y2) and is filled with the current fill color and the current
  fill pattern (SetFillBitmap is ignored!).
  Depth is the depth of the bar in pixels. The side face is drawn to the right
  of the front face and filled with the background color.
  When Top=true a top face of the bar is painted, again filled with the
  background color.
  The border outlines are painted in the current forground pen color (SetColor). }
procedure Bar3D(x1, y1, x2, y2:integer; ADepth: word; ATop: boolean);
var
  fs: FillSettingsType;
begin
  CheckCanvas;
  inc(x2);  // LCL ignores the right/bottom edge
  inc(y2);
  GlobalPoint(x1,y1);
  GlobalPoint(x2,y2);
  CheckCorners(x1,y1,x2,y2);
  GetFillSettings(fs);
  with _FillSettings do
    SetFillStyle(Pattern, Color);
  with _ActiveCanvas do
  begin
    Rectangle(x1,y1,x2,y2);
    Brush.Style := bsSolid;
    Brush.Color := _Rgb_BkColor;
    Polygon([
      Point(x2, y1),
      Point(x2 + ADepth, y1 - ADepth),
      Point(x2 + ADepth, y2 - ADepth),
      Point(x2, y2),
      Point(x2, y1)
      ]);
    if ATop then Polygon([
      Point(x1, y1),
      Point(x1 + ADepth, y1 - ADepth),
      Point(x2 + ADepth, y1 - ADepth),
      Point(x2, y1),
      Point(x1, y1)
      ]);
  end;
  SetFillStyle(fs.Pattern, fs.Color);
end;

procedure Circle(x, y, r: integer);
var
  brushstyle: TBrushStyle;
begin
  CheckCanvas;
  with _ActiveCanvas do begin
    brushStyle := Brush.Style;
    try
      GlobalPoint(x,y);
      Brush.Style := bsClear;
      Ellipse(x-r, y-r, x+r, y+r);
    finally
      Brush.Style := brushStyle;
    end;
  end;
end;

procedure ClearDevice;
var
  brushstyle: TBrushStyle;
begin
  CheckCanvas;
  with _ActiveCanvas do begin
    brushstyle := Brush.Style;
    try
      Brush.Style := bsSolid;
      Brush.Color := _RGB_BkColor;
      FillRect(_CanvasRect);
    finally
      Brush.Style := brushstyle;
    end;
  end;
end;

{ Cleans up memory }
procedure CloseGraph;
begin
  FreeAndNil(_FillBitmap);
  FreeAndNil(_BgiBuffer);
end;

{ Warning in case of self-enclosing polygons: In contrast to the BGI, the LCL
  paints also the connecting line from outer to inner polygon (see BgiDemo -
  LineRelPlay. }
procedure DrawPoly(NumPoints: integer; var PolyPoints);
type
  PPoint = ^PointType;
var
  i: integer;
  P: PPoint;
begin
  CheckCanvas;
  P := PPoint(@PolyPoints);
  i := NumPoints;
  while i > 0 do
  begin
    GlobalPoint(P^.x, P^.y);
    inc(P);
//    PtrInt(P) := PtrInt(P) + SizeOf(PointType);
    dec(i);
  end;
  PolyLine(_ActiveCanvas.Handle, @PPoint(PolyPoints), NumPoints);
end;

procedure Ellipse(x, y: integer; StAngle, EndAngle, xRadius, yRadius:word);
var
  x1,y1, x2,y2, x3,y3, x4,y4: integer;
  cosphi, sinphi: double;
  brushstyle: TBrushStyle;
begin
  CheckCanvas;
  if (StAngle = 0) and (EndAngle = 360) then
  begin
    brushStyle := _ActiveCanvas.Brush.Style;
    _ActiveCanvas.Brush.Style := bsClear;
    GlobalPoint(x, y);
    _ActiveCanvas.Ellipse(x-xRadius, y-yRadius, x+xRadius, y+yRadius);
    _ActiveCanvas.Brush.Style := brushStyle;
  end else
  begin
    x1 := x - xRadius;
    y1 := y - yRadius;
    GlobalPoint(x1, y1);

    x2 := x + xRadius;
    y2 := y + yRadius;
    GlobalPoint(x2, y2);

    SinCos(DegToRad(StAngle), sinPhi, cosPhi);
    x3 := x + Round(xRadius * cosPhi);
    y3 := y - round(yRadius * sinPhi);
    GlobalPoint(x3, y3);

    SinCos(DegToRad(EndAngle), sinPhi, cosPhi);
    x4 := x + round(xRadius * cosPhi);
    y4 := y - round(yRadius * sinPhi);
    GlobalPoint(x4, y4);

    _ActiveCanvas.Arc(x1,y1, x2,y2, x3,y3, x4, y4);
  end;
end;

procedure FillEllipse(x, y: integer; xRadius, yRadius: word);
begin
  CheckCanvas;
  GlobalPoint(x, y);
  _ActiveCanvas.Ellipse(x - xRadius, y - yRadius, x + xRadius, y + yRadius);
end;

{ Warning in case of self-enclosing polygons: In contrast to the BGI, the LCL
  paints also the connecting line from outer to inner polygon (see BgiDemo -
  LineRelPlay. }
procedure FillPoly(NumPoints: integer; var PolyPoints);
type
  PPoint = ^PointType;
var
  i: integer;
  P: PPoint;
begin
  CheckCanvas;
  P := PPoint(@PolyPoints);
  i := NumPoints;
  while i > 0 do begin
    GlobalPoint(P^.x, P^.y);
    inc(P);
    dec(i);
  end;
  _ActiveCanvas.Polygon(@PolyPoints, NumPoints);
end;

{ Like BGI: Starting at (x, y), fills all pixels until color "ABorder" is found. }
procedure FloodFill(x, y: integer; ABorder: word);
begin
  CheckCanvas;
  GlobalPoint(x, y);
  _ActiveCanvas.FloodFill(x, y, PaletteToColor(ABorder), fsBorder);
  // Remark:
  // Using "fsSurface" it would be possible to fill all pixels of the specific color.
end;

procedure GetArcCoords(out ArcCoords: ArcCoordsType);
begin
  ArcCoords := _ArcCoords;
end;

{ Usually the pixels should be square. }
procedure GetAspectRatio(out xasp, yasp: word);
begin
  xasp := 1;
  yasp := 1;
end;

function GetBkColor: word;
begin
  CheckCanvas;
  Result := ColorToPalette(_ActiveCanvas.Brush.Color);
end;

function GetColor: word;
begin
  CheckCanvas;
  Result := ColorToPalette(_ActiveCanvas.Pen.Color);
end;

{ Just for compatibility. There is no BGI driver any more... }
function GetDriverName: string;
begin
  result := '';
end;

procedure GetFillSettings(out FillInfo: FillSettingsType);
begin
  CheckCanvas;
  FillInfo := _FillSettings;
end;

{ Just for compatibility. There is not graphics mode any more... }
function GetGraphMode: integer;
begin
  Result := 0;
end;

{ Ignores user pattern }
procedure GetLineSettings(out LineInfo: LineSettingsType);
begin
  CheckCanvas;
  LineInfo.LineStyle := BgiLinestyle(_ActiveCanvas.Pen.Style);
  LineInfo.Pattern := 0;
  LineInfo.Thickness := _ActiveCanvas.Pen.Width;
end;

{ Only 16 VGA colors! More colors can be created by SetRGBColor }
function GetMaxColor: word;
begin
  result := MaxVGAColor;
end;

{ Similar to BGI, but now the size of the drawing area is not defined by the
  monitor but by the size of the GraphicControl defined by SetBGIControl. }
function GetMaxX: integer;
begin
  with _CanvasRect do Result := Right - Left;
end;

{ See also GetMaxX }
function GetMaxY: integer;
begin
  with _CanvasRect do result := Bottom - Top;
end;

{ Just for compatibility - there is no graph mode any more... }
function GetModeName(AMode: integer) : string;
begin
  Result := '';
end;

function GetPixel(x,y: integer): word;
begin
  CheckCanvas;
  GlobalPoint(x, y);
  result := ColorToPalette(_ActiveCanvas.Pixels[x,y]);
end;

procedure GetTextSettings(out TextInfo: TextSettingsType);
begin
  CheckCanvas;
  TextInfo.Font := _BgiFont;
  TextInfo.Direction := _TextDir;
  TextInfo.CharSize := _BgiSize;
  TextInfo.Horiz := _TextAlignHor;
  TextInfo.Vert := _TextAlignVert;
end;

procedure GraphDefaults;
begin
  CheckCanvas;
  SetViewPort(0, 0, GetMaxX, GetMaxY, ClipOn);
  UseAsBkColor(_RGB_BkColor);
  UseAsColor(_RGB_Color);
  SetTextStyle(DefaultFont, HorizDir, 1);
  SetTextJustify(LeftText, TopText);
  SetWriteMode(CopyPut);
  SetFillStyle(EmptyFill, White);
  SetLineStyle(SolidLn, 0, NormWidth);
  ClearDevice;
end;

{ Is ignored. }
function GraphErrorMsg(AErrorCode: integer): string;
begin
  result := '';
end;

{ Unlike BGI, the destination of GetImage is not a pointer any more but a
  TBitmap instance which must have been created before calling GetImage
    ABitmap := TBitmap.Create;
  and which must be destroyed when no longer needed.
    ABitmap.Free; }
procedure GetImage(x1, y1, x2, y2:integer; ABitmap: TBitmap);
var
  Rs, Rd : TRect;
begin
  CheckCanvas;
  CheckBitmap(ABitmap);
  CheckCorners(x1, y1, x2, y2);
  GlobalPoint(x1, y1);
  GlobalPoint(x2, y2);
  Rs := Rect(x1, y1, x2, y2);
  Rd := Rect(0, 0, x2-x1, y2-y1);
  with ABitmap do begin
    Width := x2-x1;
    Height := y2-y1;
    Canvas.CopyRect(Rd, _ActiveCanvas, Rs);
  end;
end;

{ Is ignored. The typical BGI errors cannot happen here or raise exceptions. }
function GraphResult: integer;
begin
  Result := grOK;
end;

procedure GetViewSettings(out ViewPort: ViewportType);
begin
  ViewPort := _ViewPort;
end;

function GetX: integer;
begin
  CheckCanvas;
  GetX := _ActiveCanvas.PenPos.X - _ViewPort.x1;
end;

function GetY: integer;
begin
  CheckCanvas;
  GetY := _ActiveCanvas.PenPos.Y - _ViewPort.y1;
end;

{ Because GetImage/PutImage use TBitmap objects here rather than pointer
  strutures, this function has no effect. Because of this fundamental difference
  relative to the BGI, an exception is raised. }
function {%H-}ImageSize(x1,y1,x2,y2:integer) : word;
begin
  NotImplementedError;
end;

{ Changed calling parameters!
  Defines the LCL control which will paint the BGI output on its canvas.
  The following components are good examples:
  - TPaintbox
  - TPanel
  - TForm.
  Does all initializations by calling GraphDefaults.
  InitGraph MUST BE CALLED BEFORE ANY OTHER CALLS of lazGraph. }
procedure InitGraph(AControl:TControl);
begin
  SetBgiControl(AControl);
  if Assigned(_Canvas) then
    GraphDefaults;
  _IsBuffered := false;
end;

{ Not implemented. Calling this raises an exception because of the fundamental
  difference to the BGI. }
function {%H-}InstallUserDriver(AName:string; AutoDetectPtr:Pointer) : integer;
begin
  NotImplementedError;
end;

{ AFontName must be the name of a font of the system, e.g. 'Times New Roman'
  When this font is not found it is silently replace by a similar one. }
function InstallUserFont(AFontName: string; ASizes: array of integer;
  AStyle: TFontStyles): integer;
begin
  inc(_NumFonts);
  _Fonts[_NumFonts].Name := AFontName;
  Move(_Fonts[_NumFonts-1].Sizes, _Fonts[_NumFonts].Sizes, SizeOf(TFontSizeArray));
  _Fonts[_NumFonts].Style := AStyle;
  _Fonts[_NumFonts].Sizes := ASizes;
  Result := _NumFonts;
end;

procedure Line(x1, y1, x2, y2: integer);
begin
  CheckCanvas;
  GlobalPoint(x1, y1);
  _ActiveCanvas.MoveTo(x1, y1);
  GlobalPoint(x2, y2);
  _ActiveCanvas.LineTo(x2, y2);
end;

procedure LineRel(dx, dy: integer);
begin
  CheckCanvas;
  with _ActiveCanvas do LineTo(PenPos.X+dx, PenPos.Y+dy);
end;

procedure LineTo(x, y: integer);
begin
  CheckCanvas;
  _ActiveCanvas.LineTo(x+_Viewport.x1, y+_Viewport.y1);
end;

procedure MoveTo(x, y: integer);
begin
  CheckCanvas;
  _ActiveCanvas.MoveTo(x+_Viewport.x1, y+_Viewport.y1);
end;

procedure MoveRel(dx, dy: integer);
begin
  CheckCanvas;
  with _ActiveCanvas do MoveTo(PenPos.X+dx, PenPos.Y+dy);
end;

procedure OutText(s: string);
var
  w : integer;
begin
  CheckCanvas;
  OutTextXY(_ActiveCanvas.PenPos.X, _ActiveCanvas.PenPos.Y, s);
  w := TextWidth(s);
  case _TextDir of
    HorizDir :
      with _ActiveCanvas do MoveTo(PenPos.X+w, PenPos.Y);
    VertDir :
      with _ActiveCanvas do MoveTo(PenPos.X, PenPos.Y-w);
  end;
end;

procedure OutTextXY(x, y: integer; s: string);
var
  R: TRect;
  oldbrush: TBrushStyle;
  w, h: integer;
begin
  CheckCanvas;
  oldBrush := _ActiveCanvas.Brush.Style;
  try
    _ActiveCanvas.Brush.Style := bsClear;
    w := TextWidth(s);
    h := TextHeight(s);
    case _TextDir of
      HorizDir :
        begin
          case _TextAlignHor of
            LeftText   : ;
            CenterText : x := x - w div 2;
            RightText  : x := x - w;
          end;
          case _TextAlignVert of
            TopText    : ;
            CenterText : y := y - h div 2;
            BottomText : y := y - h;
          end;
          with _Viewport do begin
            if Clip=ClipOn then begin
              R := Rect(x1, y1, x2, y2);
              _ActiveCanvas.TextRect(R, x+x1, y+y1, s);
            end else
              _ActiveCanvas.TextOut(x+x1, y+y1, s);
          end;
        end;  // HorizDir
      VertDir :
        begin
          case _TextAlignHor of
            LeftText   : ;
            CenterText : x := x - h div 2;
            RightText  : x := x - h;
          end;
          case _TextAlignVert of
            TopText    : y := y + w;
            CenterText : y := y + w div 2;
            BottomText : ;
          end;
          _ActiveCanvas.Font.Orientation := 900;
          try
            with _ViewPort do
            begin
              if Clip = ClipOn then
              begin
                R := Rect(x1, y1, x2, y2);
                _ActiveCanvas.TextRect(R, x+x1, y+y1, s);
              end else
                _ActiveCanvas.TextOut(x+x1, y+y1, s);
            end;
          finally
            _ActiveCanvas.Font.Orientation := 0;
          end;
                    (*
          // wp: Is this cross-platform???????? Why not font.Orientation = 900???

          // Erzeugung des LogFonts lt. PC Magazin 11/98, Font-Handling
          // lt Windows SDK-Hilfe "Rotating Lines of Text"
          GetObject(_ActiveCanvas.Font.Handle, SizeOf(TLogFont), @LogFont);
          LogFont.lfEscapement := 900;   // in Zehntel Grad!
          hFnt := CreateFontIndirect(LogFont);
          hFntPrev := SelectObject(_ActiveCanvas.Handle, hFnt);
          with _Viewport do begin
            if Clip=ClipOn then begin
              R := Rect(x1, y1, x2, y2);
              ExtTextOut(_ActiveCanvas.Handle, x+x1, y+y1, ETO_CLIPPED, @R,
                PChar(s), Length(s), nil);
            end else
              TextOut(_ActiveCanvas.Handle, x+x1, y+y1, PChar(s), Length(s));
          end;
          SelectObject(_ActiveCanvas.Handle, hFntPrev);
          DeleteObject(hFnt);
          *)
        end;  // VertDir
    end;
  finally
    _ActiveCanvas.Brush.Style := oldBrush;
  end;
end;

procedure PieSlice(x, y: integer; StAngle, EndAngle, Radius: word);
begin
  Sector(x, y, StAngle, EndAngle, Radius, Radius);
end;

{ Difference to BGI: The data of PutImage originate from a TBitmap instance
  which must have been created before:
    ABitmap := TBitmap.Create
  and destroyed afterwards
    ABitmap.Free; }
procedure PutImage(x, y: integer; ABitmap: TBitmap; AMode: word);
var
  Rsrc, Rdest: TRect;
begin
  CheckCanvas;
  CheckBitmap(ABitmap);
  GlobalPoint(x, y);
  Rsrc := Rect(0, 0, ABitmap.Width, ABitmap.Height);
  Rdest := Rect(x, y, x + ABitmap.Width, y + ABitmap.Height);
  case AMode of
    NormalPut : _ActiveCanvas.CopyMode := cmSrcCopy;
    XorPut    : _ActiveCanvas.CopyMode := cmPatInvert;
    OrPut     : _ActiveCanvas.CopyMode := cmSrcPaint;
    AndPut    : _ActiveCanvas.CopyMode := cmSrcAnd;
    NotPut    : _ActiveCanvas.CopyMode := cmMergeCopy;
  end;
  { The LCL canvas has many options for CopyMode (TCanvas.CopyMode).
    Possibly this is not 100% correct here... }
  _ActiveCanvas.CopyRect(Rdest, ABitmap.Canvas, Rsrc);
end;

procedure PutPixel(x, y: integer; AColor: word);
begin
  CheckCanvas;
  GlobalPoint(x, y);
  _ActiveCanvas.Pixels[x,y] := VGAColors[AColor];
end;

procedure Rectangle(x1, y1, x2, y2: integer);
var
  brushstyle: TBrushStyle;
begin
  CheckCanvas;
  inc(x2);    // LCL ignores the lower right edge.
  inc(y2);
  GlobalPoint(x1, y1);
  GlobalPoint(x2, y2);
  CheckCorners(x1,y1, x2,y2);
  with _ActiveCanvas do begin
    brushstyle := Brush.Style;
    Brush.Style := bsClear;
    Rectangle(x1, y1, x2, y2);
    Brush.Style := brushstyle;
  end;
end;

{ Not implemented. Raises an exception because of this fundamental difference
  to BGI }
function {%H-}RegisterBGIdriver(ADriver: Pointer): integer;
begin
  NotImplementedError;
end;

{ Not implemented. Raises an exception because of this fundamental difference
  to BGI }
function {%H-}RegisterBGIFont(AFont: Pointer): integer;
begin
  NotImplementedError;
end;

procedure Sector(x, y: integer; StAngle, EndAngle, xRadius, yRadius: word);
var
  x1,y1, x2,y2, x3,y3, x4,y4 : integer;
  phi : double;
begin
  CheckCanvas;
  x1 := x - xRadius;
  y1 := y - yRadius;
  GlobalPoint(x1,y1);

  x2 := x + xRadius;
  y2 := y + yRadius;
  GlobalPoint(x2,y2);

  phi := degToRad(StAngle);
  x3 := x + Round(xRadius*cos(phi));
  y3 := y - round(yRadius*sin(phi));
  GlobalPoint(x3,y3);

  phi := DegToRad(EndAngle);
  x4 := x + round(xRadius*cos(phi));
  y4 := y - round(yRadius*sin(phi));
  GlobalPoint(x4, y4);

  _ActiveCanvas.Pie(x1,y1, x2,y2, x3,y3, x4,y4);
end;

procedure SetBkColor(AColorNum: word);
begin
  CheckCanvas;
  UseAsBkColor(PaletteToColor(AColorNum));
end;

{ Sets the line and text color.
  BUT: When calling SetColor from a TControl event handler does not call THIS
  routine but the inherited SetColor of the control. It is recommended to
  use SetFgColor as a replacement. }
procedure SetColor(AColor: word);
begin
  CheckCanvas;
  UseAsColor(PaletteToColor(AColor));
end;

{ Replacement for SetColor which cannot be called in the context of a TControl }
procedure SetFgColor(AColor: word);
begin
  CheckCanvas;
  UseAsColor(PaletteToColor(AColor));
end;

procedure SetFillPattern(APattern: FillPatternType; AColor: word);
begin
  CheckCanvas;
  User_Fill := APattern;
//  Move(APattern, User_Fill, SizeOf(APattern));
  SetFillStyle(UserFill, AColor);
end;

procedure SetFillStyle(APattern, AColor: word);
const
  Bits : array[0..7] of byte = ($80,$40,$20,$10,$08,$04,$02,$01);
var
  patt: FillPatternType;
  col: TColor;
  i, j: integer;
  x, y: integer;
begin
  CheckCanvas;
  if (APattern > UserFill) then
    InvalidPatternError;

  _FillBitmap.Free;
  case APattern of
    EmptyFill :
      with _ActiveCanvas do begin
        _FillBitmap := nil;
        Brush.Bitmap := nil;
        Brush.Style := bsClear;
      end;
    SolidFill :
      with _ActiveCanvas do begin
        _FillBitmap := nil;
        Brush.BitMap := nil;
        Brush.Style := bsSolid;
        Brush.Color := PaletteToColor(AColor);
      end;
    else begin
      patt := _FillPatterns[APattern]^;
      _FillBitmap := TBitmap.Create;
      col := PaletteToColor(AColor);
      _FillBitmap.Width := 8;
      _FillBitmap.Height := 8;
      for i := 1 to 8 do begin
        y := i-1;
        for j:=0 to 7 do begin
          x := j;
          if patt[i] and Bits[j] <> 0
            then _FillBitmap.Canvas.Pixels[x,y] := col
            else _FillBitmap.Canvas.Pixels[x,y] := _RGB_BkColor;
        end;
      end;
    end;
    _FillSettings.Pattern := APattern;
    _FillSettings.Color := AColor;
    _ActiveCanvas.Brush.Bitmap := _FillBitmap;
  end;
end;

{ Pattern has not effect.
  Line widths different from 1 or 3 are working, too, but line types are working
  only for line width 1. }
procedure SetLineStyle(ALineStyle, APattern, AThickness: word);
begin
  CheckCanvas;
  with _ActiveCanvas.Pen do begin
    Style := PenStyle(ALineStyle);
    Width := AThickness;
  end;
end;

procedure SetTextJustify(Horiz, Vert: word);
var
  ts: TTextStyle;
begin
  CheckCanvas;
  _TextAlignHor := Horiz;
  _TextAlignVert := Vert;
  ts := _ActiveCanvas.TextStyle;
  ts.Alignment := taLeftJustify;
  ts.Layout := tlTop;
  _ActiveCanvas.TextStyle := ts;
//  SetTextAlign(_ActiveCanvas.Handle, TA_LEFT+TA_TOP);
end;

{ There are more possibilities with using "SetTextFont" }
procedure SetTextStyle(AFont, ADirection, ACharSize: word);
var
  nam: TFontName;
  siz: integer;
  sty: TFontStyles;
begin
  CheckCanvas;

  if ACharsize > 10 then
    ACharsize := 10;

  if (AFont > _NumFonts) then
    InvalidFontError;

  _BgiFont := AFont;
  _BgiSize := ACharSize;
  _TextDir := ADirection;
  nam := _Fonts[AFont].Name;
  siz := _Fonts[AFont].Sizes[ACharsize];
  sty := _Fonts[AFont].Style;
  SetTextFont(nam, siz, sty);
end;

procedure SetUserCharSize(MultX, DivX, MultY, DivY: word);
begin
  NotImplementedError;
end;

procedure SetViewPort(x1, y1, x2, y2: integer; AClip: boolean);
begin
  CheckCanvas;
  CheckCorners(x1,y1, x2,y2);
  _viewport.x1 := x1;
  _viewport.y1 := y1;
  _viewport.x2 := x2;
  _viewport.y2 := y2;
  _viewport.Clip := AClip;
  _ActiveCanvas.Clipping := AClip;
  if AClip then
    _ActiveCanvas.ClipRect := Rect(x1, y1, x2, y2);
end;

procedure SetWriteMode(AMode: integer);
begin
  CheckCanvas;
  case AMode of
    CopyPut : _ActiveCanvas.Pen.Mode := pmCopy;     // also: NormalPut
    XorPut  : _ActiveCanvas.Pen.Mode := pmXor;
  end;
end;

function TextHeight(s: string): integer;
begin
  CheckCanvas;
  Result := _ActiveCanvas.TextHeight(s);
end;

function TextWidth(s: string): integer;
begin
  CheckCanvas;
  Result := _ActiveCanvas.TextWidth(s);
end;


//==============================================================================
//                        Additional routines
//==============================================================================

{ All subsequent BGI graphics commands are buffered by means of a bitmap in
  memory. An already visible graphic is also copied to the buffer. }
procedure DrawToBuffer;
var
  R : TRect;
begin
  CheckCanvas;
  R := _CanvasRect;
  _BgiBuffer.Free;
  _BgiBuffer := TBitmap.Create;
  with _BgiBuffer do begin
    Width := R.Right - R.Left;
    Height := R.Bottom - R.Top;
  end;
  _BgiBuffer.Canvas.CopyRect(R, _Canvas, R);
  _IsBuffered := true;
end;

{ All subsequent BGI graphics commands are displayed on the screen.
  Undos a previously called "DrawToBuffer". }
procedure DrawToScreen;
begin
  CheckCanvas;
  _IsBuffered := false;
end;

{ Displays on the screen the internal bitmap buffer which had been used for
  painting since the last call of "DrawToBuffer" }
procedure ShowBuffer;
var
  Rsrc, Rdest: TRect;
begin
  CheckCanvas;
  CheckBitmap(_BgiBuffer);
  Rsrc := Rect(0, 0, _BgiBuffer.Width, _BgiBuffer.Height);
  Rdest := _CanvasRect;
  _Canvas.CopyRect(Rdest, _BgiBuffer.Canvas, Rsrc);
end;

function GetCanvas: TCanvas;
begin
  result := _ActiveCanvas;
end;

{ Returns the current background color as RGB value.
  See also GetBkColor. }
function GetRGBBkColor: TColor;
begin
  CheckCanvas;
  Result := _ActiveCanvas.Brush.Color;
end;

{ Returns the current foreground painting color as RGB value.
  See also GetColor. }
function GetRGBColor: TColor;
begin
  CheckCanvas;
  Result := _ActiveCanvas.Pen.Color;
end;

{ See SetTextFont. }
procedure GetTextFont(out AFontName: TFontName; out ASize: integer;
  out AStyle: TFontStyles);
begin
  AFontName := _FontName;
  ASize := _FontSize;
  AStyle := _FontStyle;
end;

procedure GradientRect(x1, y1, x2, y2:integer; StartColor, EndColor:TColor;
  ADirection: integer);
var
  dir: TGradientDirection;
begin
  case ADirection of
    HorizDir: dir := gdHorizontal;
    VertDir: dir := gdVertical;
  end;
  _ActiveCanvas.GradientFill(Rect(x1, y1, x2, y2), StartColor, EndColor, dir);
end;
{
  function CalcColor(mincol,maxcol:byte; value,maxvalue:integer) : integer;
  begin
    result := integer(mincol) + round(value*(maxcol-mincol)/maxvalue);
  end;

var
  i,n,m : integer;
  r,g,b : integer;
  color : TColor;
  oldCol : TColor;
begin
  CheckCanvas;
  CheckCorners(x1,y1,x2,y2);
  GlobalPoint(x1, y1);
  GlobalPoint(x2, y2);
  case direction of
    HorizDir : begin n := x2-x1; m := y2-y1; end;
    VertDir  : begin n := y2-y1; m := x2-x1; end;
  end;
  oldcol := _ActiveCanvas.Pen.Color;
  try
    for i:= 0 to n-1 do begin
      r := CalcColor(StartColor, EndColor, i, n);
      g := CalcColor(StartColor shr 8, EndColor shr 8, i, n);
      b := CalcColor(StartColor shr 16, EndColor shr 16, i, n);
      Color := r + g shl 8 + b shl 16;
      with _ActiveCanvas do begin
        Pen.Color := color;
        case direction of
          HorizDir :
            begin
              MoveTo(x1+i, y1);
              LineTo(x1+i, y2);
            end;
          VertDir :
            begin
              MoveTo(x1, y1+i);
              LineTo(x2, y1+i);
            end;
        end;
      end;
    end;
  finally
    _ActiveCanvas.Pen.Color := oldCol;
  end;
end;
}

//------------------------------------------------------------------------------

procedure SetBgiControl(AControl: TControl);
begin                    
  if AControl is TGraphicControl then
    _Canvas := TBgiGraphicControl(AControl).Canvas
  else
  if AControl is TCustomControl then
    _Canvas := TBgiCustomControl(AControl).Canvas
  else if AControl is TCustomForm then
    _Canvas := TCustomForm(AControl).Canvas
  else
    raise EGraphError.Create(SInvalidAncestor);

  if Assigned(_Canvas) then begin
    _CanvasRect := AControl.ClientRect;            // Größe des Zeichenbreichs
    if AControl is TCustomForm then
      _ClipOrigin := Point(0, 0)
    else
    _ClipOrigin := Point(AControl.Left, AControl.Top);
  end;
end;

{ Set background color as RGB value.
  See also: SetBkColor
  Can be called BEFORE InitGraph. }
procedure SetRGBBkColor(AColor: TColor);
begin
  _RGB_BkColor := AColor;
end;

{ Set line and text color as RGB value.
  See also: SetColor
  Can be called BEFORE InitGraph. }
procedure SetRGBColor(AColor:TColor);
begin
  _RGB_Color := AColor;
end;

{ Defines the default font for text output:
  - AFontName: name of the font. e.g. 'MS Sans Serif'
  - ASizes: the 10 character sizes in points (e.g. 8) for the 10 CharSizes
  - AStyle: font attributes as set: fsBold, fsItalic, fsUnderLine, fsStrikeOut
    e.g. [fsBold, fsItalic] of bold and italic text }
procedure SetDefaultFont(AFontName: TFontName; ASizes: array of integer;
  AStyle: TFontStyles);
begin
  SetFontParams(DefaultFont, AFontName, ASizes, AStyle);
end;

{ Defines the GothicFont for text output }
procedure SetGothicFont(AFontName: TFontName; ASizes: array of integer;
  AStyle: TFontStyles);
begin
  SetFontParams(GothicFont, AFontName, ASizes, AStyle);
end;

{ The provided bitmap instance is used in the Bar(), FillPoly() etc calls for
  filling the area.
  In contrast to BGI, any bitmap can be used here. But it must have the size of
  8x8 pixels (or be clipped to this size).
  The bitmap must have been created by ABitmap := TBitmap.Create, and can have
  been loaded from a bmp file by ABitmap.LoadFromFile(<filename>).
  After usage the bitmap must be destroyed by ABitmap.Free. }
procedure SetFillBitmap(ABitmap: TCustomBitmap);
begin
  CheckCanvas;
  _ActiveCanvas.Brush.Bitmap := ABitmap;
end;

{ Defines the SansSerifFont for text output }
procedure SetSansSerifFont(AFontName: TFontName; ASizes: array of integer;
  AStyle: TFontStyles);
begin
  SetFontParams(SansSerifFont, AFontName, ASizes, AStyle);
end;

{ Defines the SmallFont for text output }
procedure SetSmallFont(AFontName: TFontName; ASizes: array of integer;
  AStyle: TFontStyles);
begin
  SetFontParams(SmallFont, AFontName, ASizes, AStyle);
end;

{ Defines the font for text output. Overwrites the settings made by "SetTextStyle".
  - AFontName: name of the font, e.g. 'Times New Roman'
  - ASize: font size in points
  - AStyle: Set of style attributes, fsBold, fsItalic, fsUnderLine, fsStrikeOut,
    normally empty []
  When the font doe not exist it is replaced by a similar one. No error message. }
procedure SetTextFont(AFontName: TFontName; ASize: integer; AStyle: TFontStyles);
begin
  _FontName := AFontName;
  _FontSize := ASize;
  _FontStyle := AStyle;
  if Assigned(_ActiveCanvas) then
    with _ActiveCanvas.Font do begin
      Name := _FontName;
      Size := _FontSize;
      Style := _FontStyle;
    end;
end;

{ Defines the TriplexFont for text output }
procedure SetTriplexFont(AFontName: TFontName; ASizes: array of integer;
  AStyle: TFontStyles);
begin
  SetFontParams(TriplexFont, AFontName, ASizes, AStyle);
end;


initialization
  _Canvas := nil;
  _ClipOrigin := Point(0, 0);
  _RGB_Color := clWhite;
  _RGB_BkColor := clBlack;
  _FillBitmap := nil;
  _BgiBuffer := nil;
  _IsBuffered := false;
  InitFonts;

finalization
  CloseGraph;

end.
