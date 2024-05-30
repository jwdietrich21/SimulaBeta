unit Stats;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Statistics window }

{ Version 3.2.0 (Donostia) }

{ (c) Johannes W. Dietrich, 1994 - 2024 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2024 }

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, SimulaBetaTypes,
  StatsEngine, SimulationEngine, SimulaBetaBaseServices, SimulaBetaGUIServices;

type

  { TStatsForm }

  TStatsForm = class(TForm)
    StatsGrid: TStringGrid;
    procedure FormActivate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure GetStats;
  public
    FirstV, LastV, MinV, MaxV, MeanV, MedianV, SDV: TState;
    SEMV, CoVV: TState;
    procedure ShowContent(Sender: TObject);
    procedure UpdateHeaders;
    procedure CopyCells;
    procedure SaveGrid(const theFileName: string; const theDelimiter: char);
  end;

var
  StatsForm: TStatsForm;

implementation

{$R *.lfm}

{ TStatsForm }

procedure TStatsForm.GetStats;
begin
  FirstV := FirstVector(gValues);
  LastV := LastVector(gValues);
  MinV := MinVector(gValues);
  MaxV := MaxVector(gValues);
  MeanV := MeanVector(gValues);
  medianV := MedianVector(gValues);
  SDV := SDVector(gValues);
  SEMV := SEMVector(gValues);
  CoVV := CoVVector(gValues);
end;

procedure TStatsForm.ShowContent(Sender: TObject);
begin
  GetStats;

  StatsGrid.Cells[1, 1] := FormatFloatDefault(gNumberFormat, FirstV.P / PFactor, 'NaN');
  StatsGrid.Cells[2, 1] := FormatFloatDefault(gNumberFormat, FirstV.W / MicroFactor, 'NaN');
  StatsGrid.Cells[3, 1] := FormatFloatDefault(gNumberFormat, FirstV.Q / MicroFactor, 'NaN');
  StatsGrid.Cells[4, 1] := FormatFloatDefault(gNumberFormat, FirstV.R / MicroFactor, 'NaN');
  StatsGrid.Cells[5, 1] := FormatFloatDefault(gNumberFormat, FirstV.G / GFactor * gGlucoseConversionFactor, 'NaN');
  StatsGrid.Cells[6, 1] := FormatFloatDefault(gNumberFormat, FirstV.S / PicoFactor, 'NaN');
  StatsGrid.Cells[7, 1] := FormatFloatDefault(gNumberFormat, FirstV.I / IFactor * gInsulinConversionFactor, 'NaN');
  StatsGrid.Cells[8, 1] := FormatFloatDefault(gNumberFormat, FirstV.M, 'NaN');
  StatsGrid.Cells[9, 1] := FormatFloatDefault(gNumberFormat, FirstV.N, 'NaN');

  StatsGrid.Cells[1, 2] := FormatFloatDefault(gNumberFormat, LastV.P / PFactor, 'NaN');
  StatsGrid.Cells[2, 2] := FormatFloatDefault(gNumberFormat, LastV.W / MicroFactor, 'NaN');
  StatsGrid.Cells[3, 2] := FormatFloatDefault(gNumberFormat, LastV.Q / MicroFactor, 'NaN');
  StatsGrid.Cells[4, 2] := FormatFloatDefault(gNumberFormat, LastV.R / MicroFactor, 'NaN');
  StatsGrid.Cells[5, 2] := FormatFloatDefault(gNumberFormat, LastV.G / GFactor * gGlucoseConversionFactor, 'NaN');
  StatsGrid.Cells[6, 2] := FormatFloatDefault(gNumberFormat, LastV.S / PicoFactor, 'NaN');
  StatsGrid.Cells[7, 2] := FormatFloatDefault(gNumberFormat, LastV.I / IFactor * gInsulinConversionFactor, 'NaN');
  StatsGrid.Cells[8, 2] := FormatFloatDefault(gNumberFormat, LastV.M, 'NaN');
  StatsGrid.Cells[9, 2] := FormatFloatDefault(gNumberFormat, LastV.N, 'NaN');

  StatsGrid.Cells[1, 3] := FormatFloatDefault(gNumberFormat, MinV.P / PFactor, 'NaN');
  StatsGrid.Cells[2, 3] := FormatFloatDefault(gNumberFormat, MinV.W / MicroFactor, 'NaN');
  StatsGrid.Cells[3, 3] := FormatFloatDefault(gNumberFormat, MinV.Q / MicroFactor, 'NaN');
  StatsGrid.Cells[4, 3] := FormatFloatDefault(gNumberFormat, MinV.R / MicroFactor, 'NaN');
  StatsGrid.Cells[5, 3] := FormatFloatDefault(gNumberFormat, MinV.G / GFactor * gGlucoseConversionFactor, 'NaN');
  StatsGrid.Cells[6, 3] := FormatFloatDefault(gNumberFormat, MinV.S / PicoFactor, 'NaN');
  StatsGrid.Cells[7, 3] := FormatFloatDefault(gNumberFormat, MinV.I / IFactor * gInsulinConversionFactor, 'NaN');
  StatsGrid.Cells[8, 3] := FormatFloatDefault(gNumberFormat, MinV.M, 'NaN');
  StatsGrid.Cells[9, 3] := FormatFloatDefault(gNumberFormat, MinV.N, 'NaN');

  StatsGrid.Cells[1, 4] := FormatFloatDefault(gNumberFormat, MaxV.P / PFactor, 'NaN');
  StatsGrid.Cells[2, 4] := FormatFloatDefault(gNumberFormat, MaxV.W / MicroFactor, 'NaN');
  StatsGrid.Cells[3, 4] := FormatFloatDefault(gNumberFormat, MaxV.Q / MicroFactor, 'NaN');
  StatsGrid.Cells[4, 4] := FormatFloatDefault(gNumberFormat, MaxV.R / MicroFactor, 'NaN');
  StatsGrid.Cells[5, 4] := FormatFloatDefault(gNumberFormat, MaxV.G / GFactor * gGlucoseConversionFactor, 'NaN');
  StatsGrid.Cells[6, 4] := FormatFloatDefault(gNumberFormat, MaxV.S / PicoFactor, 'NaN');
  StatsGrid.Cells[7, 4] := FormatFloatDefault(gNumberFormat, MaxV.I / IFactor * gInsulinConversionFactor, 'NaN');
  StatsGrid.Cells[8, 4] := FormatFloatDefault(gNumberFormat, MaxV.M, 'NaN');
  StatsGrid.Cells[9, 4] := FormatFloatDefault(gNumberFormat, MaxV.N, 'NaN');

  StatsGrid.Cells[1, 5] := FormatFloatDefault(gNumberFormat, MeanV.P / PFactor, 'NaN');
  StatsGrid.Cells[2, 5] := FormatFloatDefault(gNumberFormat, MeanV.W / MicroFactor, 'NaN');
  StatsGrid.Cells[3, 5] := FormatFloatDefault(gNumberFormat, MeanV.Q / MicroFactor, 'NaN');
  StatsGrid.Cells[4, 5] := FormatFloatDefault(gNumberFormat, MeanV.R / MicroFactor, 'NaN');
  StatsGrid.Cells[5, 5] := FormatFloatDefault(gNumberFormat, MeanV.G / GFactor * gGlucoseConversionFactor, 'NaN');
  StatsGrid.Cells[6, 5] := FormatFloatDefault(gNumberFormat, MeanV.S / PicoFactor, 'NaN');
  StatsGrid.Cells[7, 5] := FormatFloatDefault(gNumberFormat, MeanV.I / IFactor * gInsulinConversionFactor, 'NaN');
  StatsGrid.Cells[8, 5] := FormatFloatDefault(gNumberFormat, MeanV.M, 'NaN');
  StatsGrid.Cells[9, 5] := FormatFloatDefault(gNumberFormat, MeanV.N, 'NaN');

  StatsGrid.Cells[1, 6] := FormatFloatDefault(gNumberFormat, MedianV.P / PFactor, 'NaN');
  StatsGrid.Cells[2, 6] := FormatFloatDefault(gNumberFormat, MedianV.W / MicroFactor, 'NaN');
  StatsGrid.Cells[3, 6] := FormatFloatDefault(gNumberFormat, MedianV.Q / MicroFactor, 'NaN');
  StatsGrid.Cells[4, 6] := FormatFloatDefault(gNumberFormat, MedianV.R / MicroFactor, 'NaN');
  StatsGrid.Cells[5, 6] := FormatFloatDefault(gNumberFormat, MedianV.G / GFactor * gGlucoseConversionFactor, 'NaN');
  StatsGrid.Cells[6, 6] := FormatFloatDefault(gNumberFormat, MedianV.S / PicoFactor, 'NaN');
  StatsGrid.Cells[7, 6] := FormatFloatDefault(gNumberFormat, MedianV.I / IFactor * gInsulinConversionFactor, 'NaN');
  StatsGrid.Cells[8, 6] := FormatFloatDefault(gNumberFormat, MedianV.M, 'NaN');
  StatsGrid.Cells[9, 6] := FormatFloatDefault(gNumberFormat, MedianV.N, 'NaN');

  StatsGrid.Cells[1, 7] := FormatFloatDefault(gNumberFormat, SDV.P / PFactor, 'NaN');
  StatsGrid.Cells[2, 7] := FormatFloatDefault(gNumberFormat, SDV.W / MicroFactor, 'NaN');
  StatsGrid.Cells[3, 7] := FormatFloatDefault(gNumberFormat, SDV.Q / MicroFactor, 'NaN');
  StatsGrid.Cells[4, 7] := FormatFloatDefault(gNumberFormat, SDV.R / MicroFactor, 'NaN');
  StatsGrid.Cells[5, 7] := FormatFloatDefault(gNumberFormat, SDV.G / GFactor * gGlucoseConversionFactor, 'NaN');
  StatsGrid.Cells[6, 7] := FormatFloatDefault(gNumberFormat, SDV.S / PicoFactor, 'NaN');
  StatsGrid.Cells[7, 7] := FormatFloatDefault(gNumberFormat, SDV.I / IFactor * gInsulinConversionFactor, 'NaN');
  StatsGrid.Cells[8, 7] := FormatFloatDefault(gNumberFormat, SDV.M, 'NaN');
  StatsGrid.Cells[9, 7] := FormatFloatDefault(gNumberFormat, SDV.N, 'NaN');

  StatsGrid.Cells[1, 8] := FormatFloatDefault(gNumberFormat, SEMV.P / PFactor, 'NaN');
  StatsGrid.Cells[2, 8] := FormatFloatDefault(gNumberFormat, SEMV.W / MicroFactor, 'NaN');
  StatsGrid.Cells[3, 8] := FormatFloatDefault(gNumberFormat, SEMV.Q / MicroFactor, 'NaN');
  StatsGrid.Cells[4, 8] := FormatFloatDefault(gNumberFormat, SEMV.R / MicroFactor, 'NaN');
  StatsGrid.Cells[5, 8] := FormatFloatDefault(gNumberFormat, SEMV.G / GFactor * gGlucoseConversionFactor, 'NaN');
  StatsGrid.Cells[6, 8] := FormatFloatDefault(gNumberFormat, SEMV.S / PicoFactor, 'NaN');
  StatsGrid.Cells[7, 8] := FormatFloatDefault(gNumberFormat, SEMV.I / IFactor * gInsulinConversionFactor, 'NaN');
  StatsGrid.Cells[8, 8] := FormatFloatDefault(gNumberFormat, SEMV.M, 'NaN');
  StatsGrid.Cells[9, 8] := FormatFloatDefault(gNumberFormat, SEMV.N, 'NaN');

  StatsGrid.Cells[1, 9] := FormatFloatDefault(gNumberFormat, CoVV.P, 'NaN');
  StatsGrid.Cells[2, 9] := FormatFloatDefault(gNumberFormat, CoVV.W, 'NaN');
  StatsGrid.Cells[3, 9] := FormatFloatDefault(gNumberFormat, CoVV.Q, 'NaN');
  StatsGrid.Cells[4, 9] := FormatFloatDefault(gNumberFormat, CoVV.R, 'NaN');
  StatsGrid.Cells[5, 9] := FormatFloatDefault(gNumberFormat, CoVV.G, 'NaN');
  StatsGrid.Cells[6, 9] := FormatFloatDefault(gNumberFormat, CoVV.S, 'NaN');
  StatsGrid.Cells[7, 9] := FormatFloatDefault(gNumberFormat, CoVV.I, 'NaN');
  StatsGrid.Cells[8, 9] := FormatFloatDefault(gNumberFormat, CoVV.M, 'NaN');
  StatsGrid.Cells[9, 9] := FormatFloatDefault(gNumberFormat, CoVV.N, 'NaN');

end;

procedure TStatsForm.UpdateHeaders;
begin
  StatsGrid.Columns[4].Title.Caption := GHeader + '(' + gUnits.G + ')';
  StatsGrid.Columns[6].Title.Caption := IHeader + '(' + gUnits.I + ')';
end;

procedure TStatsForm.CopyCells;
begin
  CutorCopyfromGrid(StatsGrid, False);
end;

procedure TStatsForm.SaveGrid(const theFileName: string;
  const theDelimiter: char);
{saves the contents of the statistics window}
{file type and, where applicable, delimiter are defined by variable theDelimiter}
var
  theCode: integer;
begin
  theCode := 0;
  SaveGridToFile(StatsForm.StatsGrid, theFileName, theDelimiter, true, true, theCode);
  if theCode = 0 then
    SetFileName(StatsForm, theFileName)
  else
    ShowSaveError;
end;

procedure TStatsForm.FormShow(Sender: TObject);
begin
  UpdateHeaders;
  ShowContent(Sender);
end;

procedure TStatsForm.FormActivate(Sender: TObject);
begin

end;

procedure TStatsForm.FormPaint(Sender: TObject);
begin
  UpdateHeaders;
end;

end.

