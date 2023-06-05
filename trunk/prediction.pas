unit Prediction;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Predictor }

{ Version 3.1.2 (Challenger) }

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ValEdit,
  Grids, SimulaBetaTypes, SimulationEngine;

type

  { TPredictionForm }

  TPredictionForm = class(TForm)
    PredictionList: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure DisplayPrediction(Parameters: TPrediction);
  end;

var
  PredictionForm: TPredictionForm;

implementation

{$R *.lfm}

procedure TPredictionForm.FormPaint(Sender: TObject);
begin
  PredictionList.Cells[0, 1] := 'P';
  PredictionList.Cells[0, 2] := 'W';
  PredictionList.Cells[0, 3] := 'Q';
  PredictionList.Cells[0, 4] := 'R';
  PredictionList.Cells[0, 5] := 'G';
  PredictionList.Cells[0, 6] := 'S';
  PredictionList.Cells[0, 7] := 'I';
  PredictionList.Cells[0, 8] := 'M';
  PredictionList.Cells[0, 9] := 'N';
end;

procedure TPredictionForm.FormCreate(Sender: TObject);
begin
  Left := Screen.Width - Width - 26;
end;

procedure TPredictionForm.DisplayPrediction(Parameters: TPrediction);
begin
  PredictionList.Cells[1, 1] := FormatFloat(gNumberFormat, Parameters[0].P / MicroFactor) + ' ' + gUnits.P;
  PredictionList.Cells[1, 2] := FormatFloat(gNumberFormat, Parameters[0].W / MicroFactor) + ' ' + gUnits.W;
  PredictionList.Cells[1, 3] := FormatFloat(gNumberFormat, Parameters[0].Q / MicroFactor) + ' ' + gUnits.Q;
  PredictionList.Cells[1, 4] := FormatFloat(gNumberFormat, Parameters[0].R / MicroFactor) + ' ' + gUnits.R;
  PredictionList.Cells[1, 5] := FormatFloat(gNumberFormat, Parameters[0].G / MilliFactor * gGlucoseConversionFactor) + ' ' + gUnits.G;
  PredictionList.Cells[1, 6] := FormatFloat(gNumberFormat, Parameters[0].S / PicoFactor) + ' ' + gUnits.S;
  PredictionList.Cells[1, 7] := FormatFloat(gNumberFormat, Parameters[0].I / PicoFactor * gInsulinConversionFactor) + ' ' + gUnits.I;
  PredictionList.Cells[1, 8] := FormatFloat(gNumberFormat, Parameters[0].M) + ' ' + gUnits.M;
  PredictionList.Cells[1, 9] := FormatFloat(gNumberFormat, Parameters[0].N) + ' ' + gUnits.N;
  PredictionList.Cells[2, 1] := FormatFloat(gNumberFormat, Parameters[1].P / MicroFactor) + ' ' + gUnits.P;
  PredictionList.Cells[2, 2] := FormatFloat(gNumberFormat, Parameters[1].W / MicroFactor) + ' ' + gUnits.W;
  PredictionList.Cells[2, 3] := FormatFloat(gNumberFormat, Parameters[1].Q / MicroFactor) + ' ' + gUnits.Q;
  PredictionList.Cells[2, 4] := FormatFloat(gNumberFormat, Parameters[1].R / MicroFactor) + ' ' + gUnits.R;
  PredictionList.Cells[2, 5] := FormatFloat(gNumberFormat, Parameters[1].G / MilliFactor * gGlucoseConversionFactor) + ' ' + gUnits.G;
  PredictionList.Cells[2, 6] := FormatFloat(gNumberFormat, Parameters[1].S / PicoFactor) + ' ' + gUnits.S;
  PredictionList.Cells[2, 7] := FormatFloat(gNumberFormat, Parameters[1].I / PicoFactor * gInsulinConversionFactor) + ' ' + gUnits.I;
  PredictionList.Cells[2, 8] := FormatFloat(gNumberFormat, Parameters[1].M) + ' ' + gUnits.M;
  PredictionList.Cells[2, 9] := FormatFloat(gNumberFormat, Parameters[1].N) + ' ' + gUnits.N;
end;

end.

