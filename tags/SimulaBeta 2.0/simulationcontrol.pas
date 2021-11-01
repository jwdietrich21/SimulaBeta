unit SimulationControl;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Control Window for overall simulation }

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
{ http://simulabeta.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  SimulationEngine, Prediction, Plot, LogGrid;

type

  { TControlWindow }

  TControlWindow = class(TForm)
    IterationsUnitLabel: TLabel;
    PUnitLabel: TLabel;
    StandardButton: TButton;
    ControlGroup: TGroupBox;
    DBetaEdit: TFloatSpinEdit;
    DBetaLabel: TLabel;
    DRUnitLabel: TLabel;
    DREdit: TFloatSpinEdit;
    DRLabel: TLabel;
    GBetaEdit: TFloatSpinEdit;
    GBetaLabel: TLabel;
    GEEdit: TFloatSpinEdit;
    GELabel: TLabel;
    GLabel: TLabel;
    GREdit: TFloatSpinEdit;
    GRLabel: TLabel;
    GSpinEdit: TFloatSpinEdit;
    GUnitLabel: TLabel;
    ILabel: TLabel;
    InitialConditionsGroups: TGroupBox;
    ISpinEdit: TFloatSpinEdit;
    IterationsLabel: TLabel;
    IterationsSpinEdit: TSpinEdit;
    IUnitLabel: TLabel;
    GBetaUnitLabel: TLabel;
    DBetaUnitLabel: TLabel;
    PLabel: TLabel;
    PSpinEdit: TFloatSpinEdit;
    StartButton: TButton;
    StrucParsGroup: TGroupBox;
    procedure FormCreate(Sender: TObject);
    procedure PUnitLabelClick(Sender: TObject);
    procedure StandardButtonClick(Sender: TObject);
    procedure SetEditControls;
    procedure StartButtonClick(Sender: TObject);
  private

  public

  end;

var
  ControlWindow: TControlWindow;

implementation

procedure TControlWindow.SetEditControls;
begin
  PSpinEdit.Value := P0 / PFactor;
  GSpinEdit.Value := G0 / GFactor;
  ISpinEdit.Value := I0 / IFactor;
  GEEdit.Value := gStrucPars.GE;
  GBetaEdit.Value := gStrucPars.GBeta / PicoFactor;
  DBetaEdit.Value := gStrucPars.DBeta / MilliFactor;
  GREdit.Value := gStrucPars.GR;
  DREdit.Value := gStrucPars.DR / NanoFactor;
end;

{$R *.lfm}

{ TControlWindow }

procedure TControlWindow.StartButtonClick(Sender: TObject);
var
  i: 0..1;
  P, Glc, Ins: extended;
  iterations: integer;
  Prediction: TPrediction;
begin
  Screen.Cursor := crHourGlass;
  P := PSpinEdit.Value * PFactor;
  Glc := GSpinEdit.Value * GFactor;
  Ins := ISpinEdit.Value * IFactor;
  iterations := IterationsSpinEdit.Value * 60;
  LogWindow.EmptyGrid;
  gValues := TValues.Create;
  gValues.size := 0; // delete content
  gValues.size := iterations + 1;
  PlotForm.PSeries.Clear;
  PlotForm.RSeries.Clear;
  PlotForm.GSeries.Clear;
  PlotForm.SSeries.Clear;
  PlotForm.ISeries.Clear;
  PlotForm.MSeries.Clear;
  PlotForm.NSeries.Clear;
  gStrucPars.GE := GEEdit.Value;
  gStrucPars.GBeta := GBetaEdit.Value * PicoFactor;
  gStrucPars.DBeta := DBetaEdit.Value * MilliFactor;
  gStrucPars.GR := GREdit.Value;
  gStrucPars.DR := DREdit.Value * NanoFactor;
  Prediction := PredictedEquilibrium(P, gStrucPars);
  PredictionForm.DisplayPrediction(Prediction);
  application.ProcessMessages;
  if Prediction[0].G < 0 then  // use positive solution for initial values
    i := 1
  else
    i := 0;
  gValues.t[0] := 0;
  gValues.P[0] := P;
  gValues.R[0] := Prediction[i].R;
  gValues.G[0] := Glc;
  gValues.S[0] := Prediction[i].S;
  gValues.I[0] := Ins;
  gValues.M[0] := Prediction[i].M;
  gValues.N[0] := Prediction[i].N;
  RunSimulation(P, Glc, Ins, iterations, Prediction);
  LogWindow.FillGrid(iterations);
  application.ProcessMessages;
  PlotForm.ShowPlot;
  gValues.Destroy;
  Screen.Cursor := crDefault;
  Hide;
end;

procedure TControlWindow.FormCreate(Sender: TObject);
begin
  InitSimulation;
  SetEditControls;
end;

procedure TControlWindow.PUnitLabelClick(Sender: TObject);
begin

end;

procedure TControlWindow.StandardButtonClick(Sender: TObject);
begin
  InitSimulation;
  SetEditControls;
end;


end.

