unit SimulationControl;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Control Window for overall simulation }

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, Math, SimulaBetaTypes, SimulationEngine, Prediction, Plot, LogGrid,
  SimulaBetaGUIServices, SequencerEngine, Sequencer, Stats, EnvironmentInfo;

type

  TStartMode = (manual, predicted, continued);

  { TControlWindow }

  TControlWindow = class(TForm)
    CancelButton: TButton;
    oGTTButton: TRadioButton;
    MinutesButton: TRadioButton;
    HoursButton: TRadioButton;
    EnterButton: TRadioButton;
    PredictionButton: TRadioButton;
    ContinueButton: TRadioButton;
    SequenceButton: TRadioButton;
    TestStartUnitLabel: TLabel;
    VersionControl: TLabel;
    ZLabel: TLabel;
    ZSpinEdit: TFloatSpinEdit;
    ZUnitLabel: TLabel;
    TestStartSpin: TSpinEdit;
    TestStartLabel: TLabel;
    OffButton: TRadioButton;
    fsIGTButton: TRadioButton;
    TestGroup: TGroupBox;
    GRUnitLabel: TLabel;
    GEUnitLabel: TLabel;
    PUnitLabel: TLabel;
    ResetButton: TButton;
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
    InitialConditionsGroup: TGroupBox;
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
    procedure CancelButtonClick(Sender: TObject);
    procedure ContinueButtonChange(Sender: TObject);
    procedure EnterButtonChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure fsIGTButtonChange(Sender: TObject);
    procedure GroupBox1Click(Sender: TObject);
    procedure HoursButtonChange(Sender: TObject);
    procedure MinutesButtonChange(Sender: TObject);
    procedure MoreButtonClick(Sender: TObject);
    procedure OffButtonChange(Sender: TObject);
    procedure oGTTButtonChange(Sender: TObject);
    procedure PredictionButtonChange(Sender: TObject);
    procedure SequenceButtonChange(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure SwitchTest(Sender: TObject);
    procedure SwitchInitialConditions(Sender: TObject);
    procedure SetEditControls;
    procedure StartButtonClick(Sender: TObject);
  private
    StartMode: TStartMode;
  public
    SimTimeUnit, TestTimeUnit: TTimeUnit;
    LOREMOSActive: boolean;
  end;

var
  ControlWindow: TControlWindow;

implementation

procedure TControlWindow.SwitchTest(Sender: TObject);
begin
  if TestTimeUnit = minutes then
    gTestTimeFactor := SecsPerMin
  else
    gTestTimeFactor := SecsPerMin * MinsPerHour;
  if fsIGTButton.Checked then
    begin
     fsIGTButton.Checked := true;
     oGTTButton.checked := false;
     SequenceButton.Checked := false;
     OffButton.Checked := false;
     TestStartLabel.Enabled := true;
     TestStartSpin.Enabled := true;
     gTestInfo.kind := tkfsIGT;
     gTestInfo.startTime := TestStartSpin.Value * gTestTimeFactor;
     SequencerWindow.Close;
    end
  else if oGTTButton.checked then
    begin
      fsIGTButton.Checked := false;
      oGTTButton.checked := true;
      SequenceButton.Checked := false;
      OffButton.Checked := false;
      TestStartLabel.Enabled := true;
      TestStartSpin.Enabled := true;
     gTestInfo.kind := tkoGTT;
      gTestInfo.startTime := TestStartSpin.Value * gTestTimeFactor;
      SequencerWindow.Close;
    end
  else if SequenceButton.checked then
    begin
      fsIGTButton.Checked := false;
      oGTTButton.checked := false;
      SequenceButton.Checked := true;
      OffButton.Checked := false;
      TestStartLabel.Enabled := false;
      TestStartSpin.Enabled := false;
      gTestInfo.kind := tkSequence;
      LOREMOSActive := true;
      SequencerWindow.CancelButton.Enabled := true;
      SequencerWindow.ApplyButton.Enabled := true;
      gSectionIterations := IterationsSpinEdit.Value * gTestTimeFactor;
      if SequencerWindow.Visible then
        SequencerWindow.Close;
      SequencerWindow.ShowModal;
    end
  else if OffButton.checked then
    begin
      fsIGTButton.Checked := false;
      oGTTButton.checked := false;
      OffButton.Checked := true;
      TestStartLabel.Enabled := false;
      TestStartSpin.Enabled := false;
      gTestInfo.kind := tkNone;
      gTestInfo.startTime := 0;
      SequencerWindow.Close;
    end
end;

procedure TControlWindow.SwitchInitialConditions(Sender: TObject);
var
  k: integer;
  P, W, Z: extended;
begin
  if gActiveModel.Imported then
     IterationsSpinEdit.Value := gActiveModel.iterations div SecsPerMin;
  If EnterButton.Checked then
    begin
      StartMode := manual;
      PredictionButton.Checked := false;
      ContinueButton.Checked := false;
      ZSpinEdit.Enabled := true;
      PSpinEdit.Enabled := true;
      ISpinEdit.Enabled := true;
      GSpinEdit.Enabled := true;
    end
  else if PredictionButton.Checked then
    begin
      StartMode := predicted;
      EnterButton.Checked := false;
      ContinueButton.Checked := false;
      ZSpinEdit.Enabled := false;
      PSpinEdit.Enabled := false;
      ISpinEdit.Enabled := false;
      GSpinEdit.Enabled := false;
      if gActiveModel.Iterations = 0 then
      begin
        W := 0;
        Z := ZSpinEdit.Value;
        P := PSpinEdit.Value * PFactor;
        gActiveModel.Prediction := PredictedEquilibrium(P, W, Z, gActiveModel.StrucPars);
        PredictionForm.DisplayPrediction(gActiveModel.Prediction);
        application.ProcessMessages;
      end;
      if gActiveModel.prediction[0].G > 0 then
        k := 0
      else
        K := 1;
      GSpinEdit.Value := gActiveModel.Prediction[k].G / GFactor * gGlucoseConversionFactor;
      ISpinEdit.Value := gActiveModel.Prediction[k].I / IFactor * gInsulinConversionFactor;
      SetInitialConditions(gActiveModel.Prediction);
    end
  else if ContinueButton.Checked then
    begin
      StartMode := continued;
      EnterButton.Checked := false;
      PredictionButton.Checked := false;
      ZSpinEdit.Enabled := false;
      PSpinEdit.Enabled := false;
      ISpinEdit.Enabled := false;
      GSpinEdit.Enabled := false;
      if gActiveModel.Iterations > 0 then
        begin
          if gActiveModel.iterations <= length(gValues.G) then
            GSpinEdit.Value := gValues.G[gActiveModel.iterations] / GFactor * gGlucoseConversionFactor;
          if gActiveModel.iterations <= length(gValues.I) then
            ISpinEdit.Value := gValues.I[gActiveModel.iterations] / IFactor * gInsulinConversionFactor;
        end;
    end;
end;

procedure TControlWindow.SetEditControls;
begin
  IUnitLabel.Caption := gUnits.I;
  GUnitLabel.Caption := gUnits.G;
  PSpinEdit.Value := P0 / PFactor;
  ZSpinEdit.Value := Z0;
  GSpinEdit.Value := G0 / GFactor * gGlucoseConversionFactor;
  ISpinEdit.Value := I0 / IFactor * gInsulinConversionFactor;
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
  startpoint: integer;
  EventMatrix: TEventMatrix;
  continue: boolean;
begin
  Screen.Cursor := crHourGlass;
  if LOREMOSActive then
    begin
      SequencerWindow.CancelButton.Enabled := false;
      SequencerWindow.ApplyButton.Enabled := false;
      SequencerWindow.Show;
    end;
  if TestTimeUnit = minutes then
    gTestTimeFactor := SecsPerMin
  else
    gTestTimeFactor := SecsPerMin * MinsPerHour;
  W := 0;
  Z := ZSpinEdit.Value;
  P := PSpinEdit.Value * PFactor;
  SwitchInitialConditions(Sender);
  Glc := GSpinEdit.Value * GFactor / gGlucoseConversionFactor;
  Ins := ISpinEdit.Value * IFactor / gInsulinConversionFactor;
  EventMatrix := SequencerWindow.EventMatrix;
  startpoint := gValues.size;
  gActiveModel.iterations := startpoint + IterationsSpinEdit.Value * SecsPerMin;
  if SimTimeUnit = hours then
    gActiveModel.iterations := gActiveModel.iterations * MinsPerHour;
  LogWindow.EmptyGrid;
  gValues.size := gActiveModel.iterations + 1;
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
  gActiveModel.Prediction := PredictedEquilibrium(P, W, Z, gActiveModel.StrucPars);
  PredictionForm.DisplayPrediction(gActiveModel.Prediction);
  application.ProcessMessages;
  if gActiveModel.Prediction[0].G < 0 then  // use positive solution for initial values
    i := 1
  else
    i := 0;
  if gActiveModel.Iterations = 0 then
    begin
      gValues.t[0] := 0;
      gValues.P[0] := gActiveModel.Prediction[i].P;
      gValues.Z[0] := gActiveModel.Prediction[i].Z;
      gValues.R[0] := gActiveModel.Prediction[i].R;
      gValues.G[0] := Glc;
      gValues.S[0] := gActiveModel.Prediction[i].S;
      gValues.I[0] := Ins;
      gValues.M[0] := gActiveModel.Prediction[i].M;
      gValues.N[0] := gActiveModel.Prediction[i].N;
    end;
  //SwitchTest(Sender);
  Close;
  application.ProcessMessages;
  if StartMode = continued then
    continue := true
  else
    continue := false;
  RunSimulation(P, Glc, Ins, startpoint, gActiveModel.iterations, gActiveModel.Prediction, EventMatrix, continue);
  LogWindow.FillGrid(gActiveModel.iterations);
  application.ProcessMessages;
  PlotForm.ShowPlot;
  StatsForm.ShowContent(Sender);
  Screen.Cursor := crDefault;
end;

procedure TControlWindow.FormCreate(Sender: TObject);
begin
  LOREMOSActive := false;
  TestStartSpin.MaxValue := IterationsSpinEdit.MaxValue;
  SetEditControls;
  SimTimeUnit := minutes;
  TestTimeUnit := SimTimeUnit;
  gTestTimeFactor := SecsPerMin;
  VersionControl.Caption := 'SimulaBeta ' + FileVersion;
end;

procedure TControlWindow.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SequencerWindow.FormStyle := fsNormal;
end;

procedure TControlWindow.CancelButtonClick(Sender: TObject);
begin
  SequencerWindow.FormStyle := fsNormal;
  Close;
end;

procedure TControlWindow.ContinueButtonChange(Sender: TObject);
begin
  SwitchInitialConditions(Sender);
end;

procedure TControlWindow.EnterButtonChange(Sender: TObject);
begin
  SwitchInitialConditions(Sender);
end;

procedure TControlWindow.FormDestroy(Sender: TObject);
begin
  if assigned(gValues) then
    FreeAndNil(gValues);
end;

procedure TControlWindow.FormHide(Sender: TObject);
begin
  SequencerWindow.FormStyle := fsNormal;
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
  if (gActiveModel.Iterations > 0) and (gValues.Size > 0) then
  begin
    ContinueButton.Enabled := true;
    ContinueButton.Checked := true;
    EnterButton.Checked := false;
    PredictionButton.Checked := false;
  end;
  SwitchInitialConditions(Sender);
end;

procedure TControlWindow.fsIGTButtonChange(Sender: TObject);
begin
  SwitchTest(Sender);
end;

procedure TControlWindow.GroupBox1Click(Sender: TObject);
begin

end;

procedure TControlWindow.HoursButtonChange(Sender: TObject);
begin
  if HoursButton.checked then
    begin
      MinutesButton.Checked := false;
      SimTimeUnit := hours;
      TestStartUnitLabel.Caption := HoursButton.Caption;
    end
  else
    begin
      SimTimeUnit := minutes;
      TestStartUnitLabel.Caption := MinutesButton.Caption;
    end;
  TestTimeUnit := SimTimeUnit;
end;

procedure TControlWindow.MinutesButtonChange(Sender: TObject);
begin
  if MinutesButton.checked then
    begin
      HoursButton.Checked := false;
      SimTimeUnit := minutes;
      TestStartUnitLabel.Caption := MinutesButton.Caption;
    end
  else
    begin
      SimTimeUnit := hours;
      TestStartUnitLabel.Caption := HoursButton.Caption;
    end;
  TestTimeUnit := SimTimeUnit;
end;

procedure TControlWindow.MoreButtonClick(Sender: TObject);
begin
  SequencerWindow.Show;
  SequencerWindow.BringToFront;
  SequencerWindow.FormStyle := fsStayOnTop;
end;

procedure TControlWindow.OffButtonChange(Sender: TObject);
begin
  SwitchTest(Sender);
end;

procedure TControlWindow.oGTTButtonChange(Sender: TObject);
begin
  SwitchTest(Sender);
end;

procedure TControlWindow.PredictionButtonChange(Sender: TObject);
begin
  SwitchInitialConditions(Sender)
end;

procedure TControlWindow.SequenceButtonChange(Sender: TObject);
begin
  SwitchTest(Sender);
end;

procedure TControlWindow.ResetButtonClick(Sender: TObject);
begin
  EnterButton.Checked := false;
  PredictionButton.Checked := true;
  ContinueButton.Checked := false;
  SwitchInitialConditions(Sender);
  InitSimulation;
  SetEditControls;
end;

end.

