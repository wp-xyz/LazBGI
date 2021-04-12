unit BgiDemo_Form;

interface

uses
  //Windows, Messages, 
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ComCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    BtnQuit: TBitBtn;
    FontDialog: TFontDialog;
    FontBtn: TBitBtn;
    CharSet: TRadioGroup;
    BtnBkColor: TBitBtn;
    ColorDialog: TColorDialog;
    PageControl: TPageControl;
    BorlandDemosSheet: TTabSheet;
    OwnDemosSheet: TTabSheet;
    PanelL: TPanel;
    BorlandPaintBox: TPaintBox;
    Splitter1: TSplitter;
    PanelR: TPanel;
    Label1: TLabel;
    OwnPanelL: TPanel;
    OwnPaintbox: TPaintBox;
    OwnPanelR: TPanel;
    Label2: TLabel;
    OwnDemoList: TListBox;
    BorlandDemoList: TListBox;
    procedure BorlandPaintBoxPaint(Sender: TObject);
    procedure BtnQuitClick(Sender: TObject);
    procedure BorlandDemoListClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FontBtnClick(Sender: TObject);
    procedure CharSetClick(Sender: TObject);
    procedure BtnBkColorClick(Sender: TObject);
    procedure OwnPaintboxPaint(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure OwnDemoListClick(Sender: TObject);
    procedure BufferedCheckboxClick(Sender: TObject);
  private
    { Private-Deklarationen}
    procedure RefreshPage;
  public
    { Public-Deklarationen}
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  lazBGI, BgiDemo_Unit;

procedure TMainForm.BorlandPaintBoxPaint(Sender: TObject);
begin
  if BorlandDemoList.ItemIndex = 24 then begin
    WriteModePlay;
    Exit;
  end;

  InitGraph(BorlandPaintBox);
  Initialize;
  SetGothicFont('Old English Text MT', [10, 12, 14, 16, 20, 24, 28, 32, 38, 48, 72], []);

  case BorlandDemoList.ItemIndex of
    0    : ReportStatus;
    1    : TextPlay;
    2    : BarPlay;
    3    : Bar3DPlay;
    4    : RandBarPlay;
    5    : CirclePlay(false);
    6    : CirclePlay(true);
    7..11: TextDump(BorlandDemoList.ItemIndex - 7);
    12   : LinetoPlay;
    13   : LineRelPlay;
    14   : LineStylePlay;
    15   : ColorPlay;
    16   : PolyPlay(false);
    17   : PolyPlay(true);
    18   : FillStylePlay;
    19   : FillPatternPlay;
    20   : PutPixelPlay;
    21   : ArcPlay;
    22   : FillEllipsePlay;
    23   : SectorPlay;
    24   : WriteModePlay;
    25   : PiePlay;
  end;
end;

procedure TMainForm.BtnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.BorlandDemoListClick(Sender: TObject);
begin
  BorlandPaintBoxPaint(Sender);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  BorlandDemoList.ItemIndex := 0;
  OwnDemoList.ItemIndex := 0;
  PageControl.ActivePage := BorlandDemosSheet;
  PageControlChange(Sender);
end;

procedure TMainForm.FontBtnClick(Sender: TObject);
const
  BaseSizes : array [1..10] of integer
    = (10, 12, 14, 16, 20, 24, 28, 32, 38, 48);
var
  FontSizes : array[1..10] of integer;
  i : integer;
  f : double;
  nam : TFontName;
  siz : integer;
  sty : TFontStyles = [];
begin
  with FontDialog do begin
    GetTextFont(nam, siz, sty);
    Font.Name := nam;
    Font.Size := siz;
    Font.style := sty;
    if Execute then begin
      f := Font.Size/BaseSizes[1];
      for i:=1 to 10 do FontSizes[i] := round(BaseSizes[i]*f);
      SetDefaultFont(Font.Name, FontSizes, Font.Style);
      RefreshPage;
    end;
  end;
end;

procedure TMainForm.CharSetClick(Sender: TObject);
begin
  UseOEMCharset := CharSet.ItemIndex = 1;
  RefreshPage;
end;

procedure TMainForm.BtnBkColorClick(Sender: TObject);
begin
  with ColorDialog do begin
    Color := GetRGBBkColor;
    if Execute then begin
      SetRGBBkColor(Color);
      RefreshPage;
    end;
  end;
end;

procedure TMainForm.OwnPaintboxPaint(Sender: TObject);
begin
  InitGraph(OwnPaintBox);
  Initialize;
  case OwnDemoList.ItemIndex of
    0   : FloodFillPlay;
    1   : FillBitmapPlay;
    2   : GradientDemo;
    3   : UserFontDemo;
  end;
  CloseGraph;
end;

procedure TMainForm.PageControlChange(Sender: TObject);
begin
  RefreshPage;
end;

procedure TMainForm.Refreshpage;
begin
  if PageControl.ActivePage = BorlandDemosSheet
    then BorlandPaintboxPaint(Self)
  else if PageControl.ActivePage = OwnDemosSheet
    then OwnPaintboxPaint(self);
end;

procedure TMainForm.OwnDemoListClick(Sender: TObject);
begin
  OwnPaintBoxPaint(Sender);
end;

procedure TMainForm.BufferedCheckboxClick(Sender: TObject);
begin
  RefreshPage;
end;

//==============================================================================

end.
