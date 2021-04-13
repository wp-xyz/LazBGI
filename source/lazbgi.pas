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
  not provide the graphics driver, but the canvas onto which the output is
  painted, e.g. "Paintbox1" or "Form1". The other two parameters define the
  with and height of the drawing area:

    InitGraph(Paintbox1.Canvas, Paintbox1.Width, Paintbox1.Height);

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
  repaint at any time and thus erase the previous drawing. An exception is
  painting into a temporary buffer bitmap - see below.

- Also be prepared of surprises when random numbers are used for some drawing
  parameters, such as in Borland's BGIDEMO.

- The BGI painting routine, by no means, must be allowed to wait for user input
  like in the original BGIDEMO. User input must be handled by the usual
  LCL OnKey* and OnMouse* events outside the painting routine.

- If nevertheless several curves are to be painted above each other, or if
  a flicker-free animation is supposed to be shown then the BGI graphic can
  be buffered:

  - When the graph is supposed to be drawn upon a button click the drawing
    commands must be put into the OnClick event handler of the button. The
    canvas for painting must be the canvas of a temporary bitmap, and the
    drawing routine must trigger a repaint of the control on which the BGI
    graphic is supposed to appear (Paintbox1.Invalidate). In the OnPaint
    handler of the control (Paintbox, Panel, Form, ...) the BGI graphic must
    be copied from the bitmap buffer to the control canvas:

      var
        Buffer: TBitmap;

      procedure TForm1.Button1Click(Sender: TObject);
      begin
        FreeAndNil(Buffer);
        Buffer := TBitmap.Create
        InitGraph(Buffer.Canvas, Paintbox1.Width, Paintbox1.Height);
        {... BGI graphics commands ... }
        CloseGraph;
        Paintbox1.Invalidate;
      end;

      procedure TForm1.Paintbox1Paint(Sender: TObject);
      begin
        Paintbox1.Canvas.Draw(0, 0, Buffer);
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
function GetFgColor: word;
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
procedure InitGraph(ACanvas: TCanvas; AWidth, AHeight: Integer);  // NOTE: Parameters changed!
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
  EBGIGraphError = Exception;

// Colors
function GetBkColorRGB : TColor;
function GetColorRGB : TColor;
procedure SetBkColorRGB(AColor: TColor);
procedure SetColorRGB(AColor: TColor);

function GetDefaultBkColorRGB: TColor;
procedure SetDefaultBkColorRGB(AColor: TColor);

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


implementation

uses
  Math,
  Forms;
  
var
  FCanvas: TCanvas;           // Canvas as BGI screen replacement
  FCanvasWidth: Integer;      // Width of the area reserved on the canvas for BGI painting
  FCanvasHeight: Integer;     // Height of the area reserved on the canvas for BGI painting
  FActiveCanvas: TCanvas;     // Canvas used for painting

  FViewPort: ViewportType;    // Current viewport in BGI units
  FArcCoords: ArcCoordsType;  // Coordinates needed by "Arc()"

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
  FFonts : TFontArray;         // Array with max 10(+1) BGI fonts
  FNumFonts : integer;         // Count of available fonts in array _Fonts
  FFontName : string;          // Currently used font
  FFontSize : integer;         // Point size of the currently used font
  FFontStyle : TFontStyles;    // Style attributes of the currently used font
  FBgiFont : integer;          // Current font as BGI number
  FBgiSize : integer;          // BGI size of the current font
  FTextAlignHor : integer;     // Current alignment: LeftText...
  FTextAlignVert : integer;    // ... TopText...
  FTextDir : integer;          // Current writing direction (HorizDir...)

{ Area fills
  There are no LCL brushes which exactly correspond to the BGI fills.
  Therefore, the fill patterns are created manually. }
const
  Empty_Fill     : FillPatternType = ($00, $00, $00, $00, $00, $00, $00, $00);
  Solid_Fill     : FillPatternType = ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
  Line_Fill      : FillPatternType = ($FF, $FF, $00, $00, $FF, $FF, $00, $00);
  Lt_Slash_Fill  : FillPatternType = ($01, $02, $04, $08, $10, $20, $40, $80);
  Slash_Fill     : FillPatternType = ($E0, $C1, $83, $07, $0E, $1C, $38, $70);
  Backslash_Fill : FillPatternType = ($F0, $78, $3C, $1E, $0F, $87, $C3, $E1);
  Lt_Bkslash_Fill: FillPatternType = ($80, $40, $20, $10, $08, $04, $02, $01);
  Hatch_Fill     : FillPatternType = ($FF, $88, $88, $88, $FF, $88, $88, $88);
  XHatch_Fill    : FillPatternType = ($81, $42, $24, $18, $18, $24, $42, $81);
  Interleave_Fill: FillPatternType = ($F0, $00, $00, $00, $0F, $00, $00, $00);
  Wide_Dot_Fill  : FillPatternType = ($80, $00, $08, $00, $80, $00, $08, $00);
  Close_Dot_Fill : FillPatternType = ($88, $00, $22, $00, $88, $00, $22, $00);

var
  User_Fill      : FillPatternType = ($00, $00, $00, $00, $00, $00, $00, $00);

const
  FILL_PATTERNS: Array[EmptyFill..UserFill] of ^FillPatternType =
    (@Empty_Fill, @Solid_Fill, @Line_Fill, @Lt_Slash_Fill, @Slash_Fill,
     @BackSlash_Fill, @Lt_BkSlash_Fill, @Hatch_Fill, @XHatch_Fill,
     @InterLeave_Fill, @Wide_Dot_Fill, @Close_Dot_Fill, @User_Fill);

var
  FFillSettings: FillSettingsType;  // Current fill settings
  FFillBitmap: TBitmap;             // Bitmap for area fills

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
  FBkColorRGB: TColor;        // Background color as RGB
  FFgColorRGB: TColor;        // Text and line color as TGB
  FDefaultBkColorRGB: TColor; // Default background color, used by GraphDefaults
  FDefaultColorRGB: TColor;   // Default text/line color, used by GraphDefaults

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

{ Checks whether a canvas has been assigned for painting and
  raises an exception otherwise. }
procedure CheckCanvas;
begin
  if not Assigned(FCanvas) then
    raise EBGIGraphError.Create(SNoCanvas);

  if not Assigned(FActiveCanvas) then
    raise EBGIGraphError.Create(SNoCanvas);
end;

{ Checks whether the provided bitmap has been created and raises an
  exception otherwise. }
procedure CheckBitmap(ABitmap: TCustomBitmap);
begin
  if not Assigned(ABitmap) then
    raise EBGIGraphError.Create(SNoBitmap);
end;

{ Raises an exception for important BGI functions which are not implemented. }
procedure NotImplementedError;
begin
  raise EBGIGraphError.Create(SNotImplemented);
end;

{ Raises an exception when the font to be used is not valid. }
procedure InvalidFontError;
begin
  raise EBGIGraphError.Create(SInvalidFont);
end;

{ Raises an exception when then fill pattern to be used is not valid }
procedure InvalidPatternError;
begin
  raise EBGIGraphError.Create(SInvalidPattern);
end;

{ Return the canvas used for painting }
function GetActiveCanvas: TCanvas;
begin
  Result := FCanvas;
end;

{ Transforms the global point (x,y) (global with respect to GetMaxX and GetMaxY)
  to the local coordinate system defined by the currenly set viewport. }
procedure LocalPoint(var x,y: Integer);
begin
  x := x - FViewPort.x1;
  y := y - FViewPort.y1;
end;

{ Transforms the point (x, y) from the local viewport coordinate system to
  "global" coordinates of the painting area (GetMaxX, GetMaxY). }
procedure GlobalPoint(var x, y: Integer);
begin
  x := FViewPort.x1 + x;
  y := FViewPort.y1 + y;
end;

{ Exchanges the integers x and y }
procedure Swap(var x, y: Integer);
var
  tmp : integer;
begin
  tmp := x;
  x := y;
  y := tmp;
end;

{ Makes sure that x1 < x2 and y1 < y2 }
procedure CheckCorners(var x1, y1, x2, y2: integer);
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
  FBkColorRGB := AColor;
  if Assigned(FActiveCanvas) then
    FActiveCanvas.Brush.Color := AColor;
end;

procedure UseAsFgColor(AColor: TColor);
begin
  FFgColorRGB := AColor;
  if Assigned(FActiveCanvas) then
  begin
    FActiveCanvas.Pen.Color := AColor;
    FActiveCanvas.Font.Color := AColor;
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
  FFonts[id].Name := AName;
  Move(ASizes, FFonts[id].Sizes, SizeOf(ASizes));
  FFonts[id].Style := AStyle;
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
  FNumFonts := 4;
  for i := FNumFonts+1 to 10 do begin
    with FFonts[i] do begin
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
  sinPhi, cosPhi: Double;
begin
  CheckCanvas;

  FArcCoords.x := x;
  FArcCoords.y := y;

  x1 := x - Radius;
  y1 := y - Radius;
  GlobalPoint(x1,y1);

  x2 := x + Radius;
  y2 := y + Radius;
  GlobalPoint(x2,y2);

  SinCos(DegToRad(StAngle), sinPhi, cosPhi);
  x3 := x + Round(Radius * cosphi);
  y3 := y - round(Radius * sinphi);
  FArcCoords.XStart := x3;
  FArcCoords.YStart := y3;
  GlobalPoint(x3,y3);

  SinCos(DegToRad(EndAngle), sinPhi, cosPhi);
  x4 := x + round(Radius * cosphi);
  y4 := y - round(Radius * sinphi);
  FArcCoords.XEnd := x4;
  FArcCoords.YEnd := y4;
  GlobalPoint(x4, y4);

  FActiveCanvas.Arc(x1,y1, x2,y2, x3,y3, x4,y4);
end;

procedure Bar(x1,y1,x2,y2:integer);
var
  savedPenStyle : TPenStyle;
begin
  CheckCanvas;
  inc(x2);  // LCL ignores the right/bottom corner
  inc(y2);
  GlobalPoint(x1,y1);
  GlobalPoint(x2,y2);
  CheckCorners(x1,y1, x2,y2);
  with FActiveCanvas do begin
    savedPenStyle := Pen.Style;
    try
      Pen.Style := psClear;
      Rectangle(x1, y1, x2, y2);
    finally
      Pen.Style := savedPenStyle;
    end;
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
  savedFillSettings: FillSettingsType;
begin
  CheckCanvas;
  inc(x2);  // LCL ignores the right/bottom edge
  inc(y2);
  GlobalPoint(x1,y1);
  GlobalPoint(x2,y2);
  CheckCorners(x1,y1,x2,y2);

  GetFillSettings(savedFillSettings);
  try
    with FFillSettings do
      SetFillStyle(Pattern, Color);
    with FActiveCanvas do
    begin
      Rectangle(x1,y1,x2,y2);
      Brush.Style := bsSolid;
      Brush.Color := FBkColorRGB;
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
  finally
    SetFillStyle(savedFillSettings.Pattern, savedFillSettings.Color);
  end;
end;

procedure Circle(x, y, r: integer);
var
  savedBrushStyle: TBrushStyle;
begin
  CheckCanvas;
  with FActiveCanvas do begin
    savedBrushStyle := Brush.Style;
    try
      GlobalPoint(x,y);
      Brush.Style := bsClear;
      Ellipse(x-r, y-r, x+r, y+r);
    finally
      Brush.Style := savedBrushStyle;
    end;
  end;
end;

procedure ClearDevice;
var
  savedBrushStyle: TBrushStyle;
begin
  CheckCanvas;
  with FActiveCanvas do begin
    savedBrushStyle := Brush.Style;
    try
      Brush.Style := bsSolid;
      Brush.Color := FBkColorRGB;
      FillRect(0, 0, FCanvasWidth, FCanvasHeight);
    finally
      Brush.Style := savedBrushStyle;
    end;
  end;
end;

{ Cleans up memory }
procedure CloseGraph;
begin
  FreeAndNil(FFillBitmap);
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
    dec(i);
  end;
  FActiveCanvas.Polyline(@PolyPoints, NumPoints);
end;

procedure Ellipse(x, y: integer; StAngle, EndAngle, xRadius, yRadius:word);
var
  x1,y1, x2,y2, x3,y3, x4,y4: integer;
  cosphi, sinphi: double;
  savedBrushStyle: TBrushStyle;
begin
  CheckCanvas;
  if (StAngle = 0) and (EndAngle = 360) then
  begin
    savedBrushStyle := FActiveCanvas.Brush.Style;
    try
      FActiveCanvas.Brush.Style := bsClear;
      GlobalPoint(x, y);
      FActiveCanvas.Ellipse(x-xRadius, y-yRadius, x+xRadius, y+yRadius);
    finally
      FActiveCanvas.Brush.Style := savedBrushStyle;
    end;
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

    FActiveCanvas.Arc(x1,y1, x2,y2, x3,y3, x4, y4);
  end;
end;

procedure FillEllipse(x, y: integer; xRadius, yRadius: word);
begin
  CheckCanvas;
  GlobalPoint(x, y);
  FActiveCanvas.Ellipse(x - xRadius, y - yRadius, x + xRadius, y + yRadius);
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
  FActiveCanvas.Polygon(@PolyPoints, NumPoints);
end;

{ Like BGI: Starting at (x, y), fills all pixels until color "ABorder" is found. }
procedure FloodFill(x, y: integer; ABorder: word);
begin
  CheckCanvas;
  GlobalPoint(x, y);
  FActiveCanvas.FloodFill(x, y, PaletteToColor(ABorder), fsBorder);
  // Remark:
  // Using "fsSurface" it would be possible to fill all pixels of the specific color.
end;

procedure GetArcCoords(out ArcCoords: ArcCoordsType);
begin
  ArcCoords := FArcCoords;
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
  Result := ColorToPalette(FActiveCanvas.Brush.Color);
end;

function GetColor: word;
begin
  CheckCanvas;
  Result := ColorToPalette(FActiveCanvas.Pen.Color);
end;

function GetFgColor: word;
begin
  CheckCanvas;
  Result := ColorToPalette(FActiveCanvas.Pen.Color);
end;

{ Just for compatibility. There is no BGI driver any more... }
function GetDriverName: string;
begin
  result := '';
end;

procedure GetFillSettings(out FillInfo: FillSettingsType);
begin
  FillInfo := FFillSettings;
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
  LineInfo.LineStyle := BgiLinestyle(FActiveCanvas.Pen.Style);
  LineInfo.Pattern := 0;
  LineInfo.Thickness := FActiveCanvas.Pen.Width;
end;

{ Only 16 VGA colors! More colors can be created by SetRGBColor }
function GetMaxColor: word;
begin
  result := MaxVGAColor;
end;

{ Similar to BGI, but now the size of the drawing area is not defined by the
  monitor but by the size of the rectangle specified in the InitGraph call. }
function GetMaxX: integer;
begin
  Result := FCanvasWidth;
end;

{ See also GetMaxX }
function GetMaxY: integer;
begin
  Result := FCanvasHeight;
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
  result := ColorToPalette(FActiveCanvas.Pixels[x,y]);
end;

procedure GetTextSettings(out TextInfo: TextSettingsType);
begin
  CheckCanvas;
  TextInfo.Font := FBgiFont;
  TextInfo.Direction := FTextDir;
  TextInfo.CharSize := FBgiSize;
  TextInfo.Horiz := FTextAlignHor;
  TextInfo.Vert := FTextAlignVert;
end;

procedure GraphDefaults;
begin
  CheckCanvas;
  SetViewPort(0, 0, GetMaxX, GetMaxY, ClipOn);
  UseAsBkColor(FDefaultBkColorRGB);
  UseAsFgColor(FDefaultColorRGB);
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
  Result := '';
end;

{ Unlike BGI, the destination of GetImage is not a pointer any more but a
  TBitmap instance which must have been created before calling GetImage
    ABitmap := TBitmap.Create;
  and which must be destroyed when no longer needed.
    ABitmap.Free; }
procedure GetImage(x1, y1, x2, y2: integer; ABitmap: TBitmap);
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
    Canvas.CopyRect(Rd, FActiveCanvas, Rs);
  end;
end;

{ Is ignored. The typical BGI errors cannot happen here or raise exceptions. }
function GraphResult: integer;
begin
  Result := grOK;
end;

procedure GetViewSettings(out ViewPort: ViewportType);
begin
  ViewPort := FViewPort;
end;

function GetX: integer;
begin
  CheckCanvas;
  Result := FActiveCanvas.PenPos.X - FViewPort.x1;
end;

function GetY: integer;
begin
  CheckCanvas;
  Result := FActiveCanvas.PenPos.Y - FViewPort.y1;
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
procedure InitGraph(ACanvas: TCanvas; AWidth, AHeight: Integer);
begin
  FCanvas := ACanvas;
  FCanvasWidth := AWidth;
  FCanvasHeight := AHeight;
  if FCanvas = nil then
    raise EBGIGraphError.Create('[InitGraph] No canvas specified.');
  if FCanvasWidth <= 1 then
    raise EBGIGraphError.Create('[InitGraph] Invalid canvas width.');
  if FCanvasHeight <= 1 then
    raise EBGIGraphError.Create('[InitGraph] Invalid canvas height.');
  FActiveCanvas := ACanvas;
  GraphDefaults;
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
  inc(FNumFonts);
  FFonts[FNumFonts].Name := AFontName;
  Move(FFonts[FNumFonts-1].Sizes, FFonts[FNumFonts].Sizes, SizeOf(TFontSizeArray));
  FFonts[FNumFonts].Style := AStyle;
  FFonts[FNumFonts].Sizes := ASizes;
  Result := FNumFonts;
end;

procedure Line(x1, y1, x2, y2: integer);
begin
  CheckCanvas;
  GlobalPoint(x1, y1);
  FActiveCanvas.MoveTo(x1, y1);
  GlobalPoint(x2, y2);
  FActiveCanvas.LineTo(x2, y2);
end;

procedure LineRel(dx, dy: integer);
begin
  CheckCanvas;
  with FActiveCanvas do
    LineTo(PenPos.X + dx, PenPos.Y + dy);
end;

procedure LineTo(x, y: integer);
begin
  CheckCanvas;
  FActiveCanvas.LineTo(x + FViewport.x1, y + FViewport.y1);
end;

procedure MoveTo(x, y: integer);
begin
  CheckCanvas;
  FActiveCanvas.MoveTo(x + FViewport.x1, y + FViewport.y1);
end;

procedure MoveRel(dx, dy: integer);
begin
  CheckCanvas;
  with FActiveCanvas do MoveTo(PenPos.X + dx, PenPos.Y + dy);
end;

procedure OutText(s: string);
var
  w : integer;
begin
  CheckCanvas;
  OutTextXY(FActiveCanvas.PenPos.X, FActiveCanvas.PenPos.Y, s);
  w := TextWidth(s);
  case FTextDir of
    HorizDir :
      with FActiveCanvas do MoveTo(PenPos.X + w, PenPos.Y);
    VertDir :
      with FActiveCanvas do MoveTo(PenPos.X, PenPos.Y - w);
  end;
end;

procedure OutTextXY(x, y: integer; s: string);
var
  R: TRect;
  savedBrushStyle: TBrushStyle;
  w, h: integer;
begin
  CheckCanvas;
  savedBrushStyle := FActiveCanvas.Brush.Style;
  try
    FActiveCanvas.Brush.Style := bsClear;
    w := TextWidth(s);
    h := TextHeight(s);
    case FTextDir of
      HorizDir :
        begin
          case FTextAlignHor of
            LeftText   : ;
            CenterText : x := x - w div 2;
            RightText  : x := x - w;
          end;
          case FTextAlignVert of
            TopText    : ;
            CenterText : y := y - h div 2;
            BottomText : y := y - h;
          end;
          with FViewport do
          begin
            if Clip=ClipOn then
            begin
              R := Rect(x1, y1, x2, y2);
              FActiveCanvas.TextRect(R, x+x1, y+y1, s);
            end else
              FActiveCanvas.TextOut(x+x1, y+y1, s);
          end;
        end;  // HorizDir
      VertDir :
        begin
          case FTextAlignHor of
            LeftText   : ;
            CenterText : x := x - h div 2;
            RightText  : x := x - h;
          end;
          case FTextAlignVert of
            TopText    : y := y + w;
            CenterText : y := y + w div 2;
            BottomText : ;
          end;
          FActiveCanvas.Font.Orientation := 900;
          try
            with FViewPort do
            begin
              if Clip = ClipOn then
              begin
                R := Rect(x1, y1, x2, y2);
                FActiveCanvas.TextRect(R, x+x1, y+y1, s);
              end else
                FActiveCanvas.TextOut(x+x1, y+y1, s);
            end;
          finally
            FActiveCanvas.Font.Orientation := 0;
          end;
        end;  // VertDir
    end;
  finally
    FActiveCanvas.Brush.Style := savedBrushStyle;
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
    NormalPut : FActiveCanvas.CopyMode := cmSrcCopy;
    XorPut    : FActiveCanvas.CopyMode := cmPatInvert;
    OrPut     : FActiveCanvas.CopyMode := cmSrcPaint;
    AndPut    : FActiveCanvas.CopyMode := cmSrcAnd;
    NotPut    : FActiveCanvas.CopyMode := cmMergeCopy;
  end;
  { The LCL canvas has many options for CopyMode (TCanvas.CopyMode).
    Possibly this is not 100% correct here... }
  FActiveCanvas.CopyRect(Rdest, ABitmap.Canvas, Rsrc);
end;

procedure PutPixel(x, y: integer; AColor: word);
begin
  CheckCanvas;
  GlobalPoint(x, y);
  FActiveCanvas.Pixels[x,y] := VGAColors[AColor];
end;

procedure Rectangle(x1, y1, x2, y2: integer);
var
  savedBrushStyle: TBrushStyle;
begin
  CheckCanvas;
  inc(x2);    // LCL ignores the lower right edge.
  inc(y2);
  GlobalPoint(x1, y1);
  GlobalPoint(x2, y2);
  CheckCorners(x1,y1, x2,y2);
  with FActiveCanvas do begin
    savedBrushStyle := Brush.Style;
    try
      Brush.Style := bsClear;
      Rectangle(x1, y1, x2, y2);
    finally
      Brush.Style := savedBrushStyle;
    end;
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
  sinPhi, cosPhi: Double;
begin
  CheckCanvas;

  x1 := x - xRadius;
  y1 := y - yRadius;
  GlobalPoint(x1,y1);

  x2 := x + xRadius;
  y2 := y + yRadius;
  GlobalPoint(x2,y2);

  SinCos(DegToRad(StAngle), sinPhi, cosPhi);
  x3 := x + Round(xRadius * cosPhi);
  y3 := y - round(yRadius * sinPhi);
  GlobalPoint(x3,y3);

  SinCos(DegToRad(EndAngle), sinPhi, cosPhi);
  x4 := x + round(xRadius * cosPhi);
  y4 := y - round(yRadius * sinPhi);
  GlobalPoint(x4, y4);

  FActiveCanvas.Pie(x1,y1, x2,y2, x3,y3, x4,y4);
end;

procedure SetBkColor(AColorNum: word);
begin
  UseAsBkColor(PaletteToColor(AColorNum));
end;

{ Sets the line and text color.
  BUT: When calling SetColor from a TControl event handler does not call THIS
  routine but the inherited SetColor of the control. It is recommended to
  use SetFgColor as a replacement. }
procedure SetColor(AColor: word);
begin
  UseAsFgColor(PaletteToColor(AColor));
end;

{ Replacement for SetColor which cannot be called in the context of a TControl }
procedure SetFgColor(AColor: word);
begin
  UseAsFgColor(PaletteToColor(AColor));
end;

procedure SetFillPattern(APattern: FillPatternType; AColor: word);
begin
  CheckCanvas;
  User_Fill := APattern;
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

  FreeAndNil(FFillBitmap);

  case APattern of
    EmptyFill :
      with FActiveCanvas do begin
        Brush.Bitmap := nil;
        Brush.Style := bsClear;
      end;
    SolidFill :
      with FActiveCanvas do begin
        Brush.BitMap := nil;
        Brush.Style := bsSolid;
        Brush.Color := PaletteToColor(AColor);
      end;
    else begin
      patt := FILL_PATTERNS[APattern]^;
      FFillBitmap := TBitmap.Create;
      col := PaletteToColor(AColor);
      FFillBitmap.Width := 8;
      FFillBitmap.Height := 8;
      for i := 1 to 8 do begin
        y := i-1;
        for j := 0 to 7 do begin
          x := j;
          if patt[i] and Bits[j] <> 0 then
            FFillBitmap.Canvas.Pixels[x,y] := col
          else
            FFillBitmap.Canvas.Pixels[x,y] := FBkColorRGB;
        end;
      end;
    end;
    FFillSettings.Pattern := APattern;
    FFillSettings.Color := AColor;
    FActiveCanvas.Brush.Bitmap := FFillBitmap;
  end;
end;

{ Pattern has not effect.
  Line widths different from 1 or 3 are working, too, but line types are working
  only for line width 1. }
procedure SetLineStyle(ALineStyle, APattern, AThickness: word);
begin
  CheckCanvas;
  with FActiveCanvas.Pen do begin
    Style := PenStyle(ALineStyle);
    Width := AThickness;
  end;
end;

procedure SetTextJustify(Horiz, Vert: word);
var
  ts: TTextStyle;
begin
  CheckCanvas;
  FTextAlignHor := Horiz;
  FTextAlignVert := Vert;

  ts := FActiveCanvas.TextStyle;
  ts.Alignment := taLeftJustify;
  ts.Layout := tlTop;
  FActiveCanvas.TextStyle := ts;
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

  if (AFont > FNumFonts) then
    InvalidFontError;

  FBgiFont := AFont;
  FBgiSize := ACharSize;
  FTextDir := ADirection;
  nam := FFonts[AFont].Name;
  siz := FFonts[AFont].Sizes[ACharsize];
  sty := FFonts[AFont].Style;
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
  FViewport.x1 := x1;
  FViewport.y1 := y1;
  FViewport.x2 := x2;
  FViewport.y2 := y2;
  FViewport.Clip := AClip;
  FActiveCanvas.Clipping := AClip;
  if AClip then
    FActiveCanvas.ClipRect := Rect(x1, y1, x2, y2);
end;

procedure SetWriteMode(AMode: integer);
begin
  CheckCanvas;
  case AMode of
    CopyPut : FActiveCanvas.Pen.Mode := pmCopy;     // also: NormalPut
    XorPut  : FActiveCanvas.Pen.Mode := pmXor;
  end;
end;

function TextHeight(s: string): integer;
begin
  CheckCanvas;
  Result := FActiveCanvas.TextHeight(s);
end;

function TextWidth(s: string): integer;
begin
  CheckCanvas;
  Result := FActiveCanvas.TextWidth(s);
end;


//==============================================================================
//                        Additional routines
//==============================================================================

{ Returns the current background color as RGB value.
  See also GetBkColor. }
function GetBkColorRGB: TColor;
begin
  CheckCanvas;
  Result := FActiveCanvas.Brush.Color;
end;

{ Returns the current foreground painting color as RGB value.
  See also GetColor and GetFgColor. }
function GetColorRGB: TColor;
begin
  CheckCanvas;
  Result := FActiveCanvas.Pen.Color;
end;

function GetDefaultBkColorRGB: TColor;
begin
  Result := FDefaultColorRGB;
end;

{ See SetTextFont. }
procedure GetTextFont(out AFontName: TFontName; out ASize: integer;
  out AStyle: TFontStyles);
begin
  AFontName := FFontName;
  ASize := FFontSize;
  AStyle := FFontStyle;
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
  FActiveCanvas.GradientFill(Rect(x1, y1, x2, y2), StartColor, EndColor, dir);
end;

{ Set background color as RGB value.
  See also: SetBkColor
  Can be called BEFORE InitGraph. }
procedure SetBkColorRGB(AColor: TColor);
begin
  UseAsBkColor(AColor);
  FBkColorRGB := AColor;
end;

{ Set line and text color as RGB value.
  See also: SetColor
  Can be called BEFORE InitGraph. }
procedure SetColorRGB(AColor:TColor);
begin
  UseAsFgColor(AColor);
end;

{ Defines the background rgb color used by GraphDefaults }
procedure SetDefaultBkColorRGB(AColor: TColor);
begin
  FDefaultBkColorRGB := AColor;
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
  FActiveCanvas.Brush.Bitmap := ABitmap;
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
  When the font does not exist it is replaced by a similar one. No error message. }
procedure SetTextFont(AFontName: TFontName; ASize: integer; AStyle: TFontStyles);
begin
  FFontName := AFontName;
  FFontSize := ASize;
  FFontStyle := AStyle;
  if Assigned(FActiveCanvas) then
    with FActiveCanvas.Font do
    begin
      Name := FFontName;
      Size := FFontSize;
      Style := FFontStyle;
    end;
end;

{ Defines the TriplexFont for text output }
procedure SetTriplexFont(AFontName: TFontName; ASizes: array of integer;
  AStyle: TFontStyles);
begin
  SetFontParams(TriplexFont, AFontName, ASizes, AStyle);
end;


initialization
  FDefaultBkColorRGB := clBlack;
  FDefaultColorRGB := clWhite;
  FBkColorRGB := clBlack;
  FFgColorRGB := clWhite;
  FFillBitmap := nil;
  InitFonts;

finalization
  CloseGraph;

end.
