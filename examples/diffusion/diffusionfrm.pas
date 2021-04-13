unit DiffusionFrm;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ComCtrls;

const
  NumClasses = 10;
  LeftDir = 0;
  RightDir = 1;
  TopDir = 2;
  BottomDir = 3;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel2: TBevel;
    BtnStart: TBitBtn;
    BtnQuit: TBitBtn;
    BtnStop: TBitBtn;
    Bevel1: TBevel;
    BtnReset: TBitBtn;
    LblSprungweite: TLabel;
    PaintBox: TPaintBox;
    Plot: TPaintBox;
    TrackBar1: TTrackBar;
    EdNumPts: TEdit;
    UpDown1: TUpDown;
    LblNumPts: TLabel;
    procedure BtnQuitClick(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure BtnResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PlotPaint(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure EdNumPtsChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen}
    RedPoints: array of TPoint;
    BluePoints: array of TPoint;
    RedClasses: array[0..NumClasses-1] of integer;
    BlueClasses: array[0..NumClasses-1] of integer;
    Calculating: boolean;
    BoxHeight: integer;
    BoxWidth: integer;
    MaxJumpWidth: integer;
    NumPts: integer;
    procedure InitPoints;
    procedure DiffusePoint(var P: TPoint);
    procedure DiffusePoints;
    procedure DrawPoints;
    procedure DrawPlot;
    function  JumpWidth(P: TPoint) : integer;
    function  IsInBox(P: TPoint) : boolean;
    procedure Simulate;
    procedure Classify;
    procedure UpdateDisplay;
  public
    { Public-Deklarationen}
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  lazBGI;

const
  RedParticleColor = LightRed;
  BlueParticleColor = Yellow;

//==============================================================================

{ Initializes the particle coordinates and displays them on the screen }
procedure TForm1.InitPoints;
var
  i : integer;
  w : integer;
  h : integer;
begin
  BoxWidth := Paintbox.Width;
  BoxHeight := Paintbox.Height;

  w := BoxWidth div 2;
  h := BoxHeight;

  SetLength(RedPoints, NumPts);
  SetLength(BluePoints, NumPts);
  for i:=0 to NumPts-1 do begin
    RedPoints[i] := Point(Random(w), Random(h));
    BluePoints[i] := Point(w+Random(w), Random(h));
  end;
  Classify;

  UpdateDisplay;
end;

{ Checks whether point P is inside the simulation window. }
function TForm1.IsInBox(P:TPoint) : boolean;
begin
  Result := (P.x >= 0) and (P.x <= BoxWidth) and (P.y >= 0) and (P.y <= BoxHeight);
end;

{ Returns the max jump width of a diffusion step. This could be the place to
  to implement space-dependent diffusion. }
function TForm1.JumpWidth(P: TPoint):integer;
begin
  Result := MaxJumpWidth;
end;

{ Calculates a diffusion step for the particle at position P and returns the
  new position.
  Only jumps in one of the four directions, left, right, up, down.
  Jump width is a uniformly distributes random number between 0 and JumpWidth
  (extremely simplified...) }
procedure TForm1.DiffusePoint(var P:TPoint);
var
  dir : integer;
  w : integer;
  Q : TPoint;
begin
  repeat
    dir := random(4);  // 0 = to the left, 1=right, 2=up, 3=down
    w := Random(JumpWidth(P));
    case dir of
      LeftDir   : Q := Point(P.x - w, P.y);
      RightDir  : Q := Point(P.x + w, P.y);
      TopDir    : Q := Point(P.x, P.y - w);
      BottomDir : Q := Point(P.x, P.y + w);
    end;
  until IsInBox(Q);
  P := Q;
end;

{ Calculations one iteration step of the diffusion: particle coordinates and
  distribution along x. Displays the results on the screen, }
procedure TForm1.DiffusePoints;
var
  i: integer;
begin
  for i := 0 to NumPts - 1 do begin
    DiffusePoint(RedPoints[i]);
    DiffusePoint(BluePoints[i]);
  end;

  Classify;

  Paintbox.Invalidate;
  Plot.Invalidate;
end;

{ Paints the diffusion particles as small circles. }
procedure TForm1.DrawPoints;
const
  R = 3;
var
  i : integer;
begin
  InitGraph(Paintbox);
  SetFgColor(Black);

  SetFillStyle(SolidFill, RedParticleColor);
  for i := 0 to NumPts - 1 do
    FillEllipse(RedPoints[i].x, RedPoints[i].y, R, R);

  SetFillStyle(SolidFill, BlueParticleColor);
  for i := 0 to NumPts - 1 do
    FillEllipse(BluePoints[i].x, BluePoints[i].y, R, R);

  CloseGraph;
end;

procedure TForm1.DrawPlot;
var
  w, h: integer;
  x, y: integer;
  x1, y1, x2, y2: integer;
  j: integer;
  maxY: integer;
begin
  InitGraph(Plot);
  SetLineStyle(SolidLn, 0, NormWidth);
  SetFgColor(Black);

  if NumPts = 0 then
    exit;

  w := Round(BoxWidth / NumClasses);
  maxY := NumPts div NumClasses*5 div 2;

  for j := 0 to NumClasses - 1 do
  begin
    x := j * w;
    y := round(RedClasses[j] / maxY * GetMaxY);
    x1 := x;
    x2 := x + w div 2;
    y1 := GetMaxY;
    y2 := y1 - y;
    SetFillStyle(SolidFill, RedParticleColor);
    Bar(x1, y1, x2, y2);
    Rectangle(x1, y1, x2, y2);
    x1 := x + w div 2;
    x2 := x + w;
    y := round(BlueClasses[j] / maxY * GetMaxY);
    y2 := y1 - y;
    SetFillStyle(SolidFill, BlueParticleColor);
    Bar(x1, y1, x2, y2);
    Rectangle(x1, y1, x2, y2);
  end;

  CloseGraph;
end;

{ Calculates the particle distribution in bins along x }
procedure TForm1.Classify;
var
  i,j : integer;
  w : double;
begin
  for j:=0 to NumClasses-1 do begin
    RedClasses[j] := 0;
    BlueClasses[j] := 0;
  end;

  w := BoxWidth / NumClasses;

  for i := 0 to NumPts - 1 do begin
    for j := 0 to NumClasses - 1 do begin
      if (RedPoints[i].x >= j*w) and (RedPoints[i].x < (j+1)*w) then begin
        inc(RedClasses[j]);
        break;
      end;
    end;
    for j := 0 to NumClasses - 1 do begin
      if (BluePoints[i].x >= j*w) and (BluePoints[i].x < (j+1)*w) then begin
        inc(BlueClasses[j]);
        break;
      end;
    end;
  end;
end;

{ Calculates the simulation loop until the STOP button is clicked. }
procedure TForm1.Simulate;
begin
  repeat
    DiffusePoints;
    Application.ProcessMessages; // make the application react on the button events
  until not Calculating;
end;

procedure TForm1.UpdateDisplay;
begin
  Paintbox.Invalidate;
  Plot.Invalidate;
end;


//=== Form =====================================================================

procedure TForm1.FormActivate(Sender: TObject);
begin
  InitPoints;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  NumPts := UpDown1.Position;
  MaxJumpWidth := 5;
  Trackbar1.Position := MaxJumpWidth;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  RedPoints := nil;
  BluePoints := nil;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  BtnStopClick(nil);
  InitPoints;
end;

procedure TForm1.PaintBoxPaint(Sender: TObject);
begin
  DrawPoints;
end;

procedure TForm1.PlotPaint(Sender: TObject);
begin
  DrawPlot;
end;


//=== Buttons ==================================================================

procedure TForm1.BtnStartClick(Sender: TObject);
begin
  BtnStop.Enabled := true;
  BtnStart.Enabled := false;
  BtnQuit.Enabled := false;
  EdNumPts.Enabled := false;
  LblNumPts.Enabled := false;
  UpDown1.Enabled := false;
  Calculating := true;
  Simulate;
end;

procedure TForm1.BtnStopClick(Sender: TObject);
begin
  Calculating := false;
  BtnQuit.Enabled := true;
  BtnStart.Enabled := true;
  BtnStop.Enabled := false;
  EdNumPts.Enabled := true;
  LblNumPts.Enabled := true;
  UpDown1.Enabled := true;
end;

procedure TForm1.BtnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.BtnResetClick(Sender: TObject);
begin
  InitPoints;
end;


//==== Trackbar etc. ===========================================================

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  MaxJumpWidth := Trackbar1.Position;
end;

procedure TForm1.EdNumPtsChange(Sender: TObject);
begin
  NumPts := UpDown1.Position;
  InitPoints;
end;

//==============================================================================

end.
