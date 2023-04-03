unit disptab;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ GUI for a disposition table}

{ Version 3.1.0 (Challenger) }

{ (c) Johannes W. Dietrich, 1994 - 2023 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2023 }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://simulabeta.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, SimulaBetaTypes, SimulationEngine, SensitivityAnalysis,
  SimulaBetaGUIServices;

type

  { TDispTabWindow }

  TDispTabWindow = class(TForm)
    ComboBox_y: TComboBox;
    ComboBox_x: TComboBox;
    DispositionGrid: TStringGrid;
    Arrow1: TImage;
    Arrow2: TImage;
    Arrow3: TImage;
    IconStorage: TImageList;
    procedure ComboBox_xChange(Sender: TObject);
    procedure ComboBox_yChange(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveGrid(const theFileName: string; const theDelimiter: char);
  private

  public
    procedure CopyCells;
  end;

var
  DispTabWindow: TDispTabWindow;

implementation

{$R *.lfm}

{ TDispTabWindow }

procedure TDispTabWindow.FormShow(Sender: TObject);
begin

end;

procedure TDispTabWindow.SaveGrid(const theFileName: string;
  const theDelimiter: char);
{saves the contents of the DispTab window}
{file type and, where applicable, delimiter are defined by variable theDelimiter}
var
  theCode: integer;
begin
  theCode := 0;
  SaveGridToFile(DispTabWindow.DispositionGrid, theFileName, theDelimiter, true, true, theCode);
  if theCode = 0 then
    SetFileName(DispTabWindow, theFileName)
  else
    ShowSaveError;
end;

procedure TDispTabWindow.CopyCells;
begin
  CutorCopyfromGrid(DispositionGrid, False);
end;

procedure TDispTabWindow.FormPaint(Sender: TObject);
const
  MinGR = 0.5;    // mol/s
  MaxGR = 15;
  MinDR = 0.3;    // nmol/l
  MaxDR = 8;
  MinGBeta = 0.1; // pmol/s
  MaxGBeta = 5;
  MinDBeta = 1.5; // mmol/l
  MaxDBeta = 35;
  MinGE = 10;     // s/mol
  MaxGE = 250;
var
  i, j: integer;
  xmin, xmax, ymin, ymax, resx, resy: real;
  SensitivityTable: tTwoWaySensTable;
begin
  if DarkTheme then
    begin
      IconStorage.GetBitmap(1, Arrow1.Picture.Bitmap);
      IconStorage.GetBitmap(3, Arrow2.Picture.Bitmap);
      IconStorage.GetBitmap(1, Arrow3.Picture.Bitmap);
    end
  else
    begin
      IconStorage.GetBitmap(0, Arrow1.Picture.Bitmap);
      IconStorage.GetBitmap(2, Arrow2.Picture.Bitmap);
      IconStorage.GetBitmap(0, Arrow3.Picture.Bitmap);
    end;
  case ComboBox_x.ItemIndex of
    0:
    begin
      xmin := MinGR;
      xmax := MaxGR;
    end;
    1:
    begin
      xmin := MinDR;
      xmax := MaxDR;
    end;
    2:
    begin
      xmin := MinGBeta;
      xmax := MaxGBeta;
    end;
    3:
    begin
      xmin := MinDBeta;
      xmax := MaxDBeta;
    end;
    4:
    begin
      xmin := MinGE;
      xmax := MaxGE;
    end;
  end;
  case ComboBox_y.ItemIndex of
    0:
    begin
      ymin := MinGR;
      ymax := MaxGR;
    end;
    1:
    begin
      ymin := MinDR;
      ymax := MaxDR;
    end;
    2:
    begin
      ymin := MinGBeta;
      ymax := MaxGBeta;
    end;
    3:
    begin
      ymin := MinDBeta;
      ymax := MaxDBeta;
    end;
    4:
    begin
      ymin := MinGE;
      ymax := MaxGE;
    end;
  end;   resx := (xmax - xmin) / (DispositionGrid.ColCount - 2);
  resy := (ymax - ymin) / (DispositionGrid.RowCount - 2);
  for i := 1 to DispositionGrid.ColCount - 1 do
    // GR
    begin
      DispositionGrid.Cells[i, 0] := FormatFloat(gNumberFormat, xmin + resx * (i - 1));
    end;
  for j := 1 to DispositionGrid.RowCount - 1 do
    // GBeta
    begin
      DispositionGrid.Cells[0, j] := FormatFloat(gNumberFormat, ymin + resy * (j - 1));
    end;
  SensitivityTable := TwoWayTable(xmin, xmax, ymin, ymax, resx, resy, gActiveModel.StrucPars, GR, GBeta);
  for i := 1 to DispositionGrid.ColCount - 1 do
    for j := 1 to DispositionGrid.RowCount - 1 do
      begin
        DispositionGrid.Cells[i, j] := FormatFloat(gNumberFormat, SensitivityTable[i, j].G / GFactor * gGlucoseConversionFactor);
      end;
end;

procedure TDispTabWindow.ComboBox_yChange(Sender: TObject);
begin
  FormPaint(Sender);
end;

procedure TDispTabWindow.ComboBox_xChange(Sender: TObject);
begin
  FormPaint(Sender);
end;

end.

