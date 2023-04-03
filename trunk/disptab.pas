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
var
  i, j: integer;
  x, y, xmin, xmax, ymin, ymax, resx, resy: real;
  SensitivityTable: tTwoWaySensTable;
const
  MinGR = 0.5;
  MaxGR = 15;
  MinGBeta = 0.1;
  MaxGBeta = 5;
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
  resx := (MaxGR - MinGR) / (DispositionGrid.ColCount - 2);
  resy := (MaxGBeta - MinGBeta) / (DispositionGrid.RowCount - 2);
  for i := 1 to DispositionGrid.ColCount - 1 do
    // GR
    begin
      DispositionGrid.Cells[i, 0] := FloatToStrF(MinGR + resx * (i - 1), ffFixed, 0, 4);
    end;
  for j := 1 to DispositionGrid.RowCount - 1 do
    // GBeta
    begin
      DispositionGrid.Cells[0, j] := FloatToStrF(MinGBeta + resy * (j - 1), ffFixed, 0, 4);
    end;
  xmin := StrToFloatDef(DispositionGrid.Cells[1, 0], 0);
  xmax := StrToFloatDef(DispositionGrid.Cells[DispositionGrid.ColCount - 1, 0], 0);
  ymin := StrToFloatDef(DispositionGrid.Cells[0, 1], 0);
  ymax := StrToFloatDef(DispositionGrid.Cells[0, DispositionGrid.RowCount - 1], 0);
  SensitivityTable := TwoWayTable(xmin, xmax, ymin, ymax, resx, resy, gActiveModel.StrucPars, GR, GBeta);
  for i := 1 to DispositionGrid.ColCount - 1 do
    for j := 1 to DispositionGrid.RowCount - 1 do
      begin
        DispositionGrid.Cells[i, j] := FloatToStrF(SensitivityTable[i, j].G / GFactor * gGlucoseConversionFactor, ffFixed, 0, 4);
      end;
end;

procedure TDispTabWindow.ComboBox_yChange(Sender: TObject);
begin

end;

end.

