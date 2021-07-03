unit LogGrid;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Log Grid }

{ Version 2.0.0 (Sleeping Briar Rose) }

{ (c) Johannes W. Dietrich, 1994 - 2021 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2021 }

{ Standard blocks for systems modelling and simulation }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://cyberunits.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids,
  SimulationEngine;

type

  { TLogWindow }

  TLogWindow = class(TForm)
    ValuesGrid: TStringGrid;
    procedure FormPaint(Sender: TObject);
  private

  public
    procedure EmptyGrid;
    procedure FillGrid(maxRow: integer);
  end;

var
  LogWindow: TLogWindow;

implementation

{$R *.lfm}

{ TLogWindow }

procedure TLogWindow.FormPaint(Sender: TObject);
begin
  Left := 13;
end;

procedure TLogWindow.EmptyGrid;
var
  i, j: integer;
begin
  ValuesGrid.RowCount := 26;
  for i := 0 to ValuesGrid.ColCount - 1 do
    for j := 1 to ValuesGrid.RowCount - 1 do
      ValuesGrid.Cells[i, j] := '';
end;

procedure TLogWindow.FillGrid(maxRow: integer);
var
  i: integer;
begin
  ValuesGrid.BeginUpdate;
  if maxRow > ValuesGrid.RowCount then
    ValuesGrid.RowCount := maxRow + 1;
  for i := 0 to maxRow - 1 do
  begin
    ValuesGrid.Cells[0, i + 1] := IntToStr(i + 1);
    ValuesGrid.Cells[1, i + 1] := FloatToStrF(gValues.P[i] / PFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[2, i + 1] := FloatToStrF(gValues.R[i] / MicroFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[3, i + 1] := FloatToStrF(gValues.G[i] / GFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[4, i + 1] := FloatToStrF(gValues.S[i] / PicoFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[5, i + 1] := FloatToStrF(gValues.I[i] / IFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[6, i + 1] := FloatToStrF(gValues.M[i], ffFixed, 0, 4);
    ValuesGrid.Cells[7, i + 1] := FloatToStrF(gValues.N[i], ffFixed, 0, 4);
  end;
  ValuesGrid.EndUpdate(true);
end;

end.

