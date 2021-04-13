unit simple_demo_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    CloseBtn: TButton;
    PaintBox1: TPaintBox;
    procedure CloseBtnClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  lazBGI;

{ TMainForm }

procedure TMainForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.PaintBox1Paint(Sender: TObject);

  procedure PutPixels;
  var
    x,y : integer;
    col : TColor;
  begin
    for x:=0 to 15 do begin
      col := x;
      for y:=0 to 30 do begin
        PutPixel(x*10+0, y, col);
        PutPixel(x*10+1, y, col);
        PutPixel(x*10+2, y, col);
        PutPixel(x*10+3, y, col);
        PutPixel(x*10+4, y, col);
        PutPixel(x*10+5, y, col);
        PutPixel(x*10+6, y, col);
        PutPixel(x*10+7, y, col);
        PutPixel(x*10+8, y, col);
      end;
    end;
  end;

const
  testString = 'This is a text.';
begin
  InitGraph(PaintBox1.Canvas, Paintbox1.Width, Paintbox1.Height);

  Line(0, 0, GetMaxX, GetMaxY);
  Line(GetMaxX, 0, 0, GetMaxY);

  PutPixels;

  SetLineStyle(DottedLn, 0, NormWidth);
  Rectangle(100, 100, 200, 200);
  SetFillStyle(SolidFill, LightBlue);
  Bar(200, 200, 300, 300);
  SetLineStyle(SolidLn, 0, NormWidth);
  Rectangle(100, 100, 100+TextWidth(TestString), 100+TextHeight(TestString));
  OutTextXY(100, 100, TestString);
  SetTextJustify(CenterText, CenterText);
  OutTextXY(GetMaxX div 2, GetMaxY div 2, TestString);

  lazBGI.SetColor(Lightred);  // DANGER: Calls inherited SetColor without "lazGraph."
  Circle(300,150, 5);
  SetTextJustify(CenterText, BottomText);
  SetTextStyle(TriplexFont, HorizDir, 4);
  OutTextXY(300,150, 'ABC');

  Line(-100,0, 100, 100);

  SetFgColor(LightCyan);
  SetFillStyle(XHatchFill, Lightred);
  Rectangle(350, 100, 400, 150);
  SetViewport(350, 100, 400, 150, ClipOn);
  MoveTo(0, 0);
  LineRel(100, 50);
  Line(100, 0, 0, 50);

  SetFgColor(Yellow);
  SetTextJustify(CenterText, CenterText);
  SetTextStyle(DefaultFont, VertDir, 1);
  OutTextXY(3, 0, '123456789012345678901234567890');

  CloseGraph;
end;

end.

