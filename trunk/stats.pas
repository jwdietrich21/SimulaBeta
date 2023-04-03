unit Stats;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Statistics window }

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, SimulaBetaTypes,
  StatsEngine, SimulationEngine, SimulaBetaGUIServices;

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

  StatsGrid.Cells[1, 1] := FormatFloat(gNumberFormat, FirstV.P / PFactor);
  StatsGrid.Cells[2, 1] := FormatFloat(gNumberFormat, FirstV.W / MicroFactor);
  StatsGrid.Cells[3, 1] := FormatFloat(gNumberFormat, FirstV.Q / MicroFactor);
  StatsGrid.Cells[4, 1] := FormatFloat(gNumberFormat, FirstV.R / MicroFactor);
  StatsGrid.Cells[5, 1] := FormatFloat(gNumberFormat, FirstV.G / GFactor * gGlucoseConversionFactor);
  StatsGrid.Cells[6, 1] := FormatFloat(gNumberFormat, FirstV.S / PicoFactor);
  StatsGrid.Cells[7, 1] := FormatFloat(gNumberFormat, FirstV.I / IFactor * gInsulinConversionFactor);
  StatsGrid.Cells[8, 1] := FormatFloat(gNumberFormat, FirstV.M);
  StatsGrid.Cells[9, 1] := FormatFloat(gNumberFormat, FirstV.N);

  StatsGrid.Cells[1, 2] := FormatFloat(gNumberFormat, LastV.P / PFactor);
  StatsGrid.Cells[2, 2] := FormatFloat(gNumberFormat, LastV.W / MicroFactor);
  StatsGrid.Cells[3, 2] := FormatFloat(gNumberFormat, LastV.Q / MicroFactor);
  StatsGrid.Cells[4, 2] := FormatFloat(gNumberFormat, LastV.R / MicroFactor);
  StatsGrid.Cells[5, 2] := FormatFloat(gNumberFormat, LastV.G / GFactor * gGlucoseConversionFactor);
  StatsGrid.Cells[6, 2] := FormatFloat(gNumberFormat, LastV.S / PicoFactor);
  StatsGrid.Cells[7, 2] := FormatFloat(gNumberFormat, LastV.I / IFactor * gInsulinConversionFactor);
  StatsGrid.Cells[8, 2] := FormatFloat(gNumberFormat, LastV.M);
  StatsGrid.Cells[9, 2] := FormatFloat(gNumberFormat, LastV.N);

  StatsGrid.Cells[1, 3] := FormatFloat(gNumberFormat, MinV.P / PFactor);
  StatsGrid.Cells[2, 3] := FormatFloat(gNumberFormat, MinV.W / MicroFactor);
  StatsGrid.Cells[3, 3] := FormatFloat(gNumberFormat, MinV.Q / MicroFactor);
  StatsGrid.Cells[4, 3] := FormatFloat(gNumberFormat, MinV.R / MicroFactor);
  StatsGrid.Cells[5, 3] := FormatFloat(gNumberFormat, MinV.G / GFactor * gGlucoseConversionFactor);
  StatsGrid.Cells[6, 3] := FormatFloat(gNumberFormat, MinV.S / PicoFactor);
  StatsGrid.Cells[7, 3] := FormatFloat(gNumberFormat, MinV.I / IFactor * gInsulinConversionFactor);
  StatsGrid.Cells[8, 3] := FormatFloat(gNumberFormat, MinV.M);
  StatsGrid.Cells[9, 3] := FormatFloat(gNumberFormat, MinV.N);

  StatsGrid.Cells[1, 4] := FormatFloat(gNumberFormat, MaxV.P / PFactor);
  StatsGrid.Cells[2, 4] := FormatFloat(gNumberFormat, MaxV.W / MicroFactor);
  StatsGrid.Cells[3, 4] := FormatFloat(gNumberFormat, MaxV.Q / MicroFactor);
  StatsGrid.Cells[4, 4] := FormatFloat(gNumberFormat, MaxV.R / MicroFactor);
  StatsGrid.Cells[5, 4] := FormatFloat(gNumberFormat, MaxV.G / GFactor * gGlucoseConversionFactor);
  StatsGrid.Cells[6, 4] := FormatFloat(gNumberFormat, MaxV.S / PicoFactor);
  StatsGrid.Cells[7, 4] := FormatFloat(gNumberFormat, MaxV.I / IFactor * gInsulinConversionFactor);
  StatsGrid.Cells[8, 4] := FormatFloat(gNumberFormat, MaxV.M);
  StatsGrid.Cells[9, 4] := FormatFloat(gNumberFormat, MaxV.N);

  StatsGrid.Cells[1, 5] := FormatFloat(gNumberFormat, MeanV.P / PFactor);
  StatsGrid.Cells[2, 5] := FormatFloat(gNumberFormat, MeanV.W / MicroFactor);
  StatsGrid.Cells[3, 5] := FormatFloat(gNumberFormat, MeanV.Q / MicroFactor);
  StatsGrid.Cells[4, 5] := FormatFloat(gNumberFormat, MeanV.R / MicroFactor);
  StatsGrid.Cells[5, 5] := FormatFloat(gNumberFormat, MeanV.G / GFactor * gGlucoseConversionFactor);
  StatsGrid.Cells[6, 5] := FormatFloat(gNumberFormat, MeanV.S / PicoFactor);
  StatsGrid.Cells[7, 5] := FormatFloat(gNumberFormat, MeanV.I / IFactor * gInsulinConversionFactor);
  StatsGrid.Cells[8, 5] := FormatFloat(gNumberFormat, MeanV.M);
  StatsGrid.Cells[9, 5] := FormatFloat(gNumberFormat, MeanV.N);

  StatsGrid.Cells[1, 6] := FormatFloat(gNumberFormat, MedianV.P / PFactor);
  StatsGrid.Cells[2, 6] := FormatFloat(gNumberFormat, MedianV.W / MicroFactor);
  StatsGrid.Cells[3, 6] := FormatFloat(gNumberFormat, MedianV.Q / MicroFactor);
  StatsGrid.Cells[4, 6] := FormatFloat(gNumberFormat, MedianV.R / MicroFactor);
  StatsGrid.Cells[5, 6] := FormatFloat(gNumberFormat, MedianV.G / GFactor * gGlucoseConversionFactor);
  StatsGrid.Cells[6, 6] := FormatFloat(gNumberFormat, MedianV.S / PicoFactor);
  StatsGrid.Cells[7, 6] := FormatFloat(gNumberFormat, MedianV.I / IFactor * gInsulinConversionFactor);
  StatsGrid.Cells[8, 6] := FormatFloat(gNumberFormat, MedianV.M);
  StatsGrid.Cells[9, 6] := FormatFloat(gNumberFormat, MedianV.N);

  StatsGrid.Cells[1, 7] := FormatFloat(gNumberFormat, SDV.P / PFactor);
  StatsGrid.Cells[2, 7] := FormatFloat(gNumberFormat, SDV.W / MicroFactor);
  StatsGrid.Cells[3, 7] := FormatFloat(gNumberFormat, SDV.Q / MicroFactor);
  StatsGrid.Cells[4, 7] := FormatFloat(gNumberFormat, SDV.R / MicroFactor);
  StatsGrid.Cells[5, 7] := FormatFloat(gNumberFormat, SDV.G / GFactor * gGlucoseConversionFactor);
  StatsGrid.Cells[6, 7] := FormatFloat(gNumberFormat, SDV.S / PicoFactor);
  StatsGrid.Cells[7, 7] := FormatFloat(gNumberFormat, SDV.I / IFactor * gInsulinConversionFactor);
  StatsGrid.Cells[8, 7] := FormatFloat(gNumberFormat, SDV.M);
  StatsGrid.Cells[9, 7] := FormatFloat(gNumberFormat, SDV.N);

  StatsGrid.Cells[1, 8] := FormatFloat(gNumberFormat, SEMV.P / PFactor);
  StatsGrid.Cells[2, 8] := FormatFloat(gNumberFormat, SEMV.W / MicroFactor);
  StatsGrid.Cells[3, 8] := FormatFloat(gNumberFormat, SEMV.Q / MicroFactor);
  StatsGrid.Cells[4, 8] := FormatFloat(gNumberFormat, SEMV.R / MicroFactor);
  StatsGrid.Cells[5, 8] := FormatFloat(gNumberFormat, SEMV.G / GFactor * gGlucoseConversionFactor);
  StatsGrid.Cells[6, 8] := FormatFloat(gNumberFormat, SEMV.S / PicoFactor);
  StatsGrid.Cells[7, 8] := FormatFloat(gNumberFormat, SEMV.I / IFactor * gInsulinConversionFactor);
  StatsGrid.Cells[8, 8] := FormatFloat(gNumberFormat, SEMV.M);
  StatsGrid.Cells[9, 8] := FormatFloat(gNumberFormat, SEMV.N);

  StatsGrid.Cells[1, 9] := FormatFloat(gNumberFormat, CoVV.P);
  StatsGrid.Cells[2, 9] := FormatFloat(gNumberFormat, CoVV.W);
  StatsGrid.Cells[3, 9] := FormatFloat(gNumberFormat, CoVV.Q);
  StatsGrid.Cells[4, 9] := FormatFloat(gNumberFormat, CoVV.R);
  StatsGrid.Cells[5, 9] := FormatFloat(gNumberFormat, CoVV.G);
  StatsGrid.Cells[6, 9] := FormatFloat(gNumberFormat, CoVV.S);
  StatsGrid.Cells[7, 9] := FormatFloat(gNumberFormat, CoVV.I);
  StatsGrid.Cells[8, 9] := FormatFloat(gNumberFormat, CoVV.M);
  StatsGrid.Cells[9, 9] := FormatFloat(gNumberFormat, CoVV.N);

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

