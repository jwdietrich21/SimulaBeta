unit SimulationControl;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Control Window for overall simulation }

{ Version 3.0.2 (Tournado) }

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, SimulaBetaTypes, SimulationEngine, Prediction, Plot, LogGrid,
  SimulaBetaGUIServices;

type

  { TControlWindow }

  TControlWindow = class(TForm)
    MoreButton: TButton;
    oGTTButton: TRadioButton;
    ZLabel: TLabel;
    ZSpinEdit: TFloatSpinEdit;
    ZUnitLabel: TLabel;
    TestStartUnitLabel: TLabel;
    TestStartSpin: TSpinEdit;
    TestStartLabel: TLabel;
    OffButton: TRadioButton;
    fsIGTButton: TRadioButton;
    TestGroup: TGroupBox;
    GRUnitLabel: TLabel;
    GEUnitLabel: TLabel;
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
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure fsIGTButtonChange(Sender: TObject);
    procedure MoreButtonClick(Sender: TObject);
    procedure OffButtonChange(Sender: TObject);
    procedure oGTTButtonChange(Sender: TObject);
    procedure StandardButtonClick(Sender: TObject);
    procedure SwitchTest(Sender: TObject);
    procedure SetEditControls;
    procedure StartButtonClick(Sender: TObject);
  private

  public

  end;

var
  ControlWindow: TControlWindow;

implementation

procedure TControlWindow.SwitchTest(Sender: TObject);
begin
  if fsIGTButton.Checked then
    begin
     fsIGTButton.Checked := true;
     oGTTButton.checked := false;
     OffButton.Checked := false;
     TestStartLabel.Enabled := true;
     TestStartSpin.Enabled := true;
     TestStartUnitLabel.Enabled := true;
     gTestInfo.kind := tkfsIGT;
     gTestInfo.startTime := TestStartSpin.Value * 60;
    end
  else if oGTTButton.checked then
    begin
      fsIGTButton.Checked := false;
      oGTTButton.checked := true;
      OffButton.Checked := false;
      TestStartLabel.Enabled := true;
      TestStartSpin.Enabled := true;
      TestStartUnitLabel.Enabled := true;
      gTestInfo.kind := tkoGTT;
      gTestInfo.startTime := TestStartSpin.Value * 60;
    end
  else if OffButton.checked then
    begin
      fsIGTButton.Checked := false;
      oGTTButton.checked := false;
      OffButton.Checked := true;
      TestStartLabel.Enabled := false;
      TestStartSpin.Enabled := false;
      TestStartUnitLabel.Enabled := false;
      gTestInfo.kind := tkNone;
      gTestInfo.startTime := 0;
    end


end;

procedure TControlWindow.SetEditControls;
begin
  PSpinEdit.Value := P0 / PFactor;
  ZSpinEdit.Value := Z0;
  GSpinEdit.Value := G0 / GFactor;
  ISpinEdit.Value := I0 / IFactor;
  GEEdit.Value := gActiveModel.StrucPars.GE;
  GBetaEdit.Value := gActiveModel.StrucPars.GBeta / PicoFactor;
  DBetaEdit.Value := gActiveModel.StrucPars.DBeta / MilliFactor;
  GREdit.Value := gActiveModel.StrucPars.GR;
  DREdit.Value := gActiveModel.StrucPars.DR / NanoFactor;
  OffButton.Checked := true;
  oGTTButton.checked := false;
  fsIGTButton.Checked := false;
end;

{$R *.lfm}

{ TControlWindow }

procedure TControlWindow.StartButtonClick(Sender: TObject);
var
  i: 0..1;
  P, W, Z, Glc, Ins: extended;
  iterations, startpoint: integer;
  Prediction: TPrediction;
begin
  Screen.Cursor := crHourGlass;
  FormStyle := fsNormal;
  W := 0;
  Z := ZSpinEdit.Value;
  P := PSpinEdit.Value * PFactor;
  Glc := GSpinEdit.Value * GFactor;
  Ins := ISpinEdit.Value * IFactor;
  startpoint := gValues.size;
  iterations := startpoint + IterationsSpinEdit.Value * 60;
  LogWindow.EmptyGrid;
  gValues.size := iterations + 1;
  PlotForm.PSeries.Clear;
  PlotForm.RSeries.Clear;
  PlotForm.GSeries.Clear;
  PlotForm.SSeries.Clear;
  PlotForm.ISeries.Clear;
  PlotForm.MSeries.Clear;
  PlotForm.NSeries.Clear;
  gActiveModel.StrucPars.GE := GEEdit.Value;
  gActiveModel.StrucPars.GBeta := GBetaEdit.Value * PicoFactor;
  gActiveModel.StrucPars.DBeta := DBetaEdit.Value * MilliFactor;
  gActiveModel.StrucPars.GR := GREdit.Value;
  gActiveModel.StrucPars.DR := DREdit.Value * NanoFactor;
  Prediction := PredictedEquilibrium(P, W, Z, gActiveModel.StrucPars);
  PredictionForm.DisplayPrediction(Prediction);
  application.ProcessMessages;
  if Prediction[0].G < 0 then  // use positive solution for initial values
    i := 1
  else
    i := 0;
  gValues.t[0] := 0;
  gValues.P[0] := Prediction[i].P;
  gValues.Z[0] := Prediction[i].Z;
  gValues.R[0] := Prediction[i].R;
  gValues.G[0] := Glc;
  gValues.S[0] := Prediction[i].S;
  gValues.I[0] := Ins;
  gValues.M[0] := Prediction[i].M;
  gValues.N[0] := Prediction[i].N;
  SwitchTest(Sender);
  Hide;
  RunSimulation(P, Glc, Ins, startpoint, iterations, Prediction);
  LogWindow.FillGrid(iterations);
  application.ProcessMessages;
  PlotForm.ShowPlot;
  Screen.Cursor := crDefault;
end;

procedure TControlWindow.FormCreate(Sender: TObject);
begin
  TestStartSpin.MaxValue := IterationsSpinEdit.MaxValue;
  InitUnits;
  InitSimulation;
  SetEditControls;
end;

procedure TControlWindow.FormDestroy(Sender: TObject);
begin
  if assigned(gValues) then
    FreeAndNil(gValues);
end;

procedure TControlWindow.FormPaint(Sender: TObject);
begin
  if DarkTheme then
    begin
      PSpinEdit.Font.Color := clBlack;
      ISpinEdit.Font.Color := clBlack;
      GSpinEdit.Font.Color := clBlack;
    end
  else
    begin
      PSpinEdit.Font.Color := clDefault;
      ISpinEdit.Font.Color := clDefault;
      GSpinEdit.Font.Color := clDefault;
    end;
end;

procedure TControlWindow.FormShow(Sender: TObject);
begin
  SetEditControls;
end;

procedure TControlWindow.fsIGTButtonChange(Sender: TObject);
begin
  SwitchTest(Sender);
end;

procedure TControlWindow.MoreButtonClick(Sender: TObject);
begin

end;

procedure TControlWindow.OffButtonChange(Sender: TObject);
begin
  SwitchTest(Sender);
end;

procedure TControlWindow.oGTTButtonChange(Sender: TObject);
begin
  SwitchTest(Sender);
end;

procedure TControlWindow.StandardButtonClick(Sender: TObject);
begin
  InitSimulation;
  SetEditControls;
end;


end.

