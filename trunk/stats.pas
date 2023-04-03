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

  StatsGrid.Cells[1, 1] := FloatToStrF(FirstV.P / PFactor, ffFixed, 0, 4);
  StatsGrid.Cells[2, 1] := FloatToStrF(FirstV.W / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[3, 1] := FloatToStrF(FirstV.Q / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[4, 1] := FloatToStrF(FirstV.R / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[5, 1] := FloatToStrF(FirstV.G / GFactor * gGlucoseConversionFactor, ffFixed, 0, 4);
  StatsGrid.Cells[6, 1] := FloatToStrF(FirstV.S / PicoFactor, ffFixed, 0, 4);
  StatsGrid.Cells[7, 1] := FloatToStrF(FirstV.I / IFactor * gInsulinConversionFactor, ffFixed, 0, 4);
  StatsGrid.Cells[8, 1] := FloatToStrF(FirstV.M, ffFixed, 0, 4);
  StatsGrid.Cells[9, 1] := FloatToStrF(FirstV.N, ffFixed, 0, 4);

  StatsGrid.Cells[1, 2] := FloatToStrF(LastV.P / PFactor, ffFixed, 0, 4);
  StatsGrid.Cells[2, 2] := FloatToStrF(LastV.W / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[3, 2] := FloatToStrF(LastV.Q / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[4, 2] := FloatToStrF(LastV.R / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[5, 2] := FloatToStrF(LastV.G / GFactor * gGlucoseConversionFactor, ffFixed, 0, 4);
  StatsGrid.Cells[6, 2] := FloatToStrF(LastV.S / PicoFactor, ffFixed, 0, 4);
  StatsGrid.Cells[7, 2] := FloatToStrF(LastV.I / IFactor * gInsulinConversionFactor, ffFixed, 0, 4);
  StatsGrid.Cells[8, 2] := FloatToStrF(LastV.M, ffFixed, 0, 4);
  StatsGrid.Cells[9, 2] := FloatToStrF(LastV.N, ffFixed, 0, 4);

  StatsGrid.Cells[1, 3] := FloatToStrF(MinV.P / PFactor, ffFixed, 0, 4);
  StatsGrid.Cells[2, 3] := FloatToStrF(MinV.W / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[3, 3] := FloatToStrF(MinV.Q / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[4, 3] := FloatToStrF(MinV.R / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[5, 3] := FloatToStrF(MinV.G / GFactor * gGlucoseConversionFactor, ffFixed, 0, 4);
  StatsGrid.Cells[6, 3] := FloatToStrF(MinV.S / PicoFactor, ffFixed, 0, 4);
  StatsGrid.Cells[7, 3] := FloatToStrF(MinV.I / IFactor * gInsulinConversionFactor, ffFixed, 0, 4);
  StatsGrid.Cells[8, 3] := FloatToStrF(MinV.M, ffFixed, 0, 4);
  StatsGrid.Cells[9, 3] := FloatToStrF(MinV.N, ffFixed, 0, 4);

  StatsGrid.Cells[1, 4] := FloatToStrF(MaxV.P / PFactor, ffFixed, 0, 4);
  StatsGrid.Cells[2, 4] := FloatToStrF(MaxV.W / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[3, 4] := FloatToStrF(MaxV.Q / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[4, 4] := FloatToStrF(MaxV.R / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[5, 4] := FloatToStrF(MaxV.G / GFactor * gGlucoseConversionFactor, ffFixed, 0, 4);
  StatsGrid.Cells[6, 4] := FloatToStrF(MaxV.S / PicoFactor, ffFixed, 0, 4);
  StatsGrid.Cells[7, 4] := FloatToStrF(MaxV.I / IFactor * gInsulinConversionFactor, ffFixed, 0, 4);
  StatsGrid.Cells[8, 4] := FloatToStrF(MaxV.M, ffFixed, 0, 4);
  StatsGrid.Cells[9, 4] := FloatToStrF(MaxV.N, ffFixed, 0, 4);

  StatsGrid.Cells[1, 5] := FloatToStrF(MeanV.P / PFactor, ffFixed, 0, 4);
  StatsGrid.Cells[2, 5] := FloatToStrF(MeanV.W / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[3, 5] := FloatToStrF(MeanV.Q / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[4, 5] := FloatToStrF(MeanV.R / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[5, 5] := FloatToStrF(MeanV.G / GFactor * gGlucoseConversionFactor, ffFixed, 0, 4);
  StatsGrid.Cells[6, 5] := FloatToStrF(MeanV.S / PicoFactor, ffFixed, 0, 4);
  StatsGrid.Cells[7, 5] := FloatToStrF(MeanV.I / IFactor * gInsulinConversionFactor, ffFixed, 0, 4);
  StatsGrid.Cells[8, 5] := FloatToStrF(MeanV.M, ffFixed, 0, 4);
  StatsGrid.Cells[9, 5] := FloatToStrF(MeanV.N, ffFixed, 0, 4);

  StatsGrid.Cells[1, 6] := FloatToStrF(MedianV.P / PFactor, ffFixed, 0, 4);
  StatsGrid.Cells[2, 6] := FloatToStrF(MedianV.W / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[3, 6] := FloatToStrF(MedianV.Q / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[4, 6] := FloatToStrF(MedianV.R / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[5, 6] := FloatToStrF(MedianV.G / GFactor * gGlucoseConversionFactor, ffFixed, 0, 4);
  StatsGrid.Cells[6, 6] := FloatToStrF(MedianV.S / PicoFactor, ffFixed, 0, 4);
  StatsGrid.Cells[7, 6] := FloatToStrF(MedianV.I / IFactor * gInsulinConversionFactor, ffFixed, 0, 4);
  StatsGrid.Cells[8, 6] := FloatToStrF(MedianV.M, ffFixed, 0, 4);
  StatsGrid.Cells[9, 6] := FloatToStrF(MedianV.N, ffFixed, 0, 4);

  StatsGrid.Cells[1, 7] := FloatToStrF(SDV.P / PFactor, ffFixed, 0, 4);
  StatsGrid.Cells[2, 7] := FloatToStrF(SDV.W / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[3, 7] := FloatToStrF(SDV.Q / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[4, 7] := FloatToStrF(SDV.R / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[5, 7] := FloatToStrF(SDV.G / GFactor * gGlucoseConversionFactor, ffFixed, 0, 4);
  StatsGrid.Cells[6, 7] := FloatToStrF(SDV.S / PicoFactor, ffFixed, 0, 4);
  StatsGrid.Cells[7, 7] := FloatToStrF(SDV.I / IFactor * gInsulinConversionFactor, ffFixed, 0, 4);
  StatsGrid.Cells[8, 7] := FloatToStrF(SDV.M, ffFixed, 0, 4);
  StatsGrid.Cells[9, 7] := FloatToStrF(SDV.N, ffFixed, 0, 4);

  StatsGrid.Cells[1, 8] := FloatToStrF(SEMV.P / PFactor, ffFixed, 0, 4);
  StatsGrid.Cells[2, 8] := FloatToStrF(SEMV.W / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[3, 8] := FloatToStrF(SEMV.Q / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[4, 8] := FloatToStrF(SEMV.R / MicroFactor, ffFixed, 0, 4);
  StatsGrid.Cells[5, 8] := FloatToStrF(SEMV.G / GFactor * gGlucoseConversionFactor, ffFixed, 0, 4);
  StatsGrid.Cells[6, 8] := FloatToStrF(SEMV.S / PicoFactor, ffFixed, 0, 4);
  StatsGrid.Cells[7, 8] := FloatToStrF(SEMV.I / IFactor * gInsulinConversionFactor, ffFixed, 0, 4);
  StatsGrid.Cells[8, 8] := FloatToStrF(SEMV.M, ffFixed, 0, 4);
  StatsGrid.Cells[9, 8] := FloatToStrF(SEMV.N, ffFixed, 0, 4);

  StatsGrid.Cells[1, 9] := FloatToStrF(CoVV.P, ffFixed, 0, 4);
  StatsGrid.Cells[2, 9] := FloatToStrF(CoVV.W, ffFixed, 0, 4);
  StatsGrid.Cells[3, 9] := FloatToStrF(CoVV.Q, ffFixed, 0, 4);
  StatsGrid.Cells[4, 9] := FloatToStrF(CoVV.R, ffFixed, 0, 4);
  StatsGrid.Cells[5, 9] := FloatToStrF(CoVV.G, ffFixed, 0, 4);
  StatsGrid.Cells[6, 9] := FloatToStrF(CoVV.S, ffFixed, 0, 4);
  StatsGrid.Cells[7, 9] := FloatToStrF(CoVV.I, ffFixed, 0, 4);
  StatsGrid.Cells[8, 9] := FloatToStrF(CoVV.M, ffFixed, 0, 4);
  StatsGrid.Cells[9, 9] := FloatToStrF(CoVV.N, ffFixed, 0, 4);

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

