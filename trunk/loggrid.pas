unit LogGrid;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Log Grid }

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, SimulaBetaTypes,
  SimulaBetaBaseServices, SimulaBetaGUIServices, SimulationEngine, DIFSupport,
  PreferencesServices;

type

  { TLogWindow }

  TLogWindow = class(TForm)
    ValuesGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private

  public
    procedure EmptyGrid;
    procedure FillGrid(maxRow: integer);
    procedure SaveGrid(const theFileName: string; const theDelimiter: char);
    procedure CopyCells;
    procedure UpdateHeaders;
  end;

var
  LogWindow: TLogWindow;

implementation

{$R *.lfm}

{ TLogWindow }

procedure TLogWindow.FormPaint(Sender: TObject);
begin
  UpdateHeaders;
end;

procedure TLogWindow.FormCreate(Sender: TObject);
begin
  Left := 13;
end;

procedure TLogWindow.EmptyGrid;
{ emties the grid and sets it to standard size }
var
  i, j: integer;
begin
  UpdateHeaders;
  ValuesGrid.RowCount := 26;
  for i := 0 to ValuesGrid.ColCount - 1 do
    for j := 1 to ValuesGrid.RowCount - 1 do
      ValuesGrid.Cells[i, j] := '';
end;

procedure TLogWindow.FillGrid(maxRow: integer);
{ fills the grid with the contents of the gValues recors }
var
  i: integer;
begin
  UpdateHeaders;
  ValuesGrid.BeginUpdate;
  if maxRow > ValuesGrid.RowCount then
    ValuesGrid.RowCount := maxRow + 1;
  for i := 0 to maxRow - 1 do
  begin
    ValuesGrid.Cells[0, i + 1] := IntToStr(i + 1);
    ValuesGrid.Cells[1, i + 1] := FormattedTime(gValues.t[i]);
    ValuesGrid.Cells[2, i + 1] := FloatToStrF(gValues.P[i] / PFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[3, i + 1] := FloatToStrF(gValues.W[i] / MicroFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[4, i + 1] := FloatToStrF(gValues.Q[i] / MicroFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[5, i + 1] := FloatToStrF(gValues.R[i] / MicroFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[6, i + 1] := FloatToStrF(gValues.G[i] / GFactor * gGlucoseConversionFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[7, i + 1] := FloatToStrF(gValues.S[i] / PicoFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[8, i + 1] := FloatToStrF(gValues.I[i] / IFactor * gInsulinConversionFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[9, i + 1] := FloatToStrF(gValues.M[i], ffFixed, 0, 4);
    ValuesGrid.Cells[10, i + 1] := FloatToStrF(gValues.N[i], ffFixed, 0, 4);
  end;
  ValuesGrid.EndUpdate(true);
end;

procedure TLogWindow.SaveGrid(const theFileName: string;
  const theDelimiter: char);
{saves the contents of the log window}
{file type and, where applicable, delimiter are defined by variable theDelimiter}
var
  theCode: integer;
begin
  theCode := 0;
  SaveGridToFile(LogWindow.ValuesGrid, theFileName, theDelimiter, true, true, theCode);
  if theCode = 0 then
    SetFileName(LogWindow, theFileName)
  else
    ShowSaveError;
end;

procedure TLogWindow.CopyCells;
begin
  CutorCopyfromGrid(valuesGrid, False);
end;

procedure TLogWindow.UpdateHeaders;
begin
  ValuesGrid.Columns[5].Title.Caption := GHeader + '(' + gUnits.G + ')';
  ValuesGrid.Columns[7].Title.Caption := IHeader + '(' + gUnits.I + ')';
end;

end.

