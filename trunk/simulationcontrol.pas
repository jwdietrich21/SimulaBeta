unit SimulationControl;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Control Window for overall simulation }

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, SimulaBetaTypes, SimulationEngine, Prediction, Plot, LogGrid,
  SimulaBetaGUIServices, SequencerEngine, Sequencer, Stats;

type

  { TControlWindow }

  TControlWindow = class(TForm)
    CancelButton: TButton;
    oGTTButton: TRadioButton;
    MinutesButton: TRadioButton;
    HoursButton: TRadioButton;
    SequenceButton: TRadioButton;
    TestStartUnitLabel: TLabel;
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
    procedure SequenceButtonChange(Sender: TObject);
    procedure StandardButtonClick(Sender: TObject);
    procedure SwitchTest(Sender: TObject);
    procedure SetEditControls;
    procedure StartButtonClick(Sender: TObject);
  private

  public
    SimTimeUnit, TestTimeUnit: TTimeUnit;
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
      SequencerWindow.Show;
      SequencerWindow.BringToFront;
      SequencerWindow.FormStyle := fsStayOnTop;
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
  startpoint: integer;
  EventMatrix: TEventMatrix;
begin
  Screen.Cursor := crHourGlass;
  if TestTimeUnit = minutes then
    gTestTimeFactor := SecsPerMin
  else
    gTestTimeFactor := SecsPerMin * MinsPerHour;
  W := 0;
  Z := ZSpinEdit.Value;
  P := PSpinEdit.Value * PFactor;
  Glc := GSpinEdit.Value * GFactor;
  Ins := ISpinEdit.Value * IFactor;
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
  gValues.t[0] := 0;
  gValues.P[0] := gActiveModel.Prediction[i].P;
  gValues.Z[0] := gActiveModel.Prediction[i].Z;
  gValues.R[0] := gActiveModel.Prediction[i].R;
  gValues.G[0] := Glc;
  gValues.S[0] := gActiveModel.Prediction[i].S;
  gValues.I[0] := Ins;
  gValues.M[0] := gActiveModel.Prediction[i].M;
  gValues.N[0] := gActiveModel.Prediction[i].N;
  SwitchTest(Sender);
  Close;
  RunSimulation(P, Glc, Ins, startpoint, gActiveModel.iterations, gActiveModel.Prediction, EventMatrix);
  LogWindow.FillGrid(gActiveModel.iterations);
  application.ProcessMessages;
  PlotForm.ShowPlot;
  StatsForm.ShowContent(Sender);
  Screen.Cursor := crDefault;
end;

procedure TControlWindow.FormCreate(Sender: TObject);
begin
  TestStartSpin.MaxValue := IterationsSpinEdit.MaxValue;
  SetEditControls;
  SimTimeUnit := minutes;
  TestTimeUnit := SimTimeUnit;
  gTestTimeFactor := SecsPerMin;
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

procedure TControlWindow.SequenceButtonChange(Sender: TObject);
begin
  SwitchTest(Sender);
end;

procedure TControlWindow.StandardButtonClick(Sender: TObject);
begin
  InitSimulation;
  SetEditControls;
end;


end.

