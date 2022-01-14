unit Prediction;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Predictor }

{ Version 2.1.0 (Turning the tides) }

{ (c) Johannes W. Dietrich, 1994 - 2022 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2022 }

{ Standard blocks for systems modelling and simulation }

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
  Grids, SimulationEngine;

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
  PredictionList.Cells[0, 2] := 'R';
  PredictionList.Cells[0, 3] := 'G';
  PredictionList.Cells[0, 4] := 'S';
  PredictionList.Cells[0, 5] := 'I';
  PredictionList.Cells[0, 6] := 'M';
  PredictionList.Cells[0, 7] := 'N';
end;

procedure TPredictionForm.FormCreate(Sender: TObject);
begin
  Left := Screen.Width - Width - 26;
end;

procedure TPredictionForm.DisplayPrediction(Parameters: TPrediction);
begin
  PredictionList.Cells[1, 1] := FloatToStrF(Parameters[0].P / MicroFactor, ffFixed, 0, 4);
  PredictionList.Cells[1, 2] := FloatToStrF(Parameters[0].R / MicroFactor, ffFixed, 0, 4);
  PredictionList.Cells[1, 3] := FloatToStrF(Parameters[0].G / MilliFactor, ffFixed, 0, 4);
  PredictionList.Cells[1, 4] := FloatToStrF(Parameters[0].S / PicoFactor, ffFixed, 0, 4);
  PredictionList.Cells[1, 5] := FloatToStrF(Parameters[0].I / PicoFactor, ffFixed, 0, 4);
  PredictionList.Cells[1, 6] := FloatToStrF(Parameters[0].M, ffFixed, 0, 4);
  PredictionList.Cells[1, 7] := FloatToStrF(Parameters[0].N, ffFixed, 0, 4);
  PredictionList.Cells[2, 1] := FloatToStrF(Parameters[1].P / MicroFactor, ffFixed, 0, 4);
  PredictionList.Cells[2, 2] := FloatToStrF(Parameters[1].R / MicroFactor, ffFixed, 0, 4);
  PredictionList.Cells[2, 3] := FloatToStrF(Parameters[1].G / MilliFactor, ffFixed, 0, 4);
  PredictionList.Cells[2, 4] := FloatToStrF(Parameters[1].S / PicoFactor, ffFixed, 0, 4);
  PredictionList.Cells[2, 5] := FloatToStrF(Parameters[1].I / PicoFactor, ffFixed, 0, 4);
  PredictionList.Cells[2, 6] := FloatToStrF(Parameters[1].M, ffFixed, 0, 4);
  PredictionList.Cells[2, 7] := FloatToStrF(Parameters[1].N, ffFixed, 0, 4);
end;

end.

