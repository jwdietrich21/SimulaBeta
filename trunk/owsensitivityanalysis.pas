unit OWSensitivityAnalysis;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ GUI for one-way sensitivity analysis }

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ColorBox,
  Spin, StdCtrls, TAGraph, TASeries, SimulaBetaTypes, SimulationEngine,
  SensitivityAnalysis;

type

  { TOWSensitivityAnalysisForm }

  TOWSensitivityAnalysisForm = class(TForm)
    Chart1: TChart;
    PSeries: TLineSeries;
    RSeries: TLineSeries;
    SSeries: TLineSeries;
    ISeries: TLineSeries;
    MSeries: TLineSeries;
    NSeries: TLineSeries;
    GSeries: TLineSeries;
    MaxSpinEdit: TFloatSpinEdit;
    StrucParCombo: TComboBox;
    MinSpinEdit: TFloatSpinEdit;
    GColorBox: TColorBox;
    MColorBox: TColorBox;
    IColorBox: TColorBox;
    nColorBox: TColorBox;
    PColorBox: TColorBox;
    RColorBox: TColorBox;
    SColorBox: TColorBox;
    VariablesCheckGroup: TCheckGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MaxSpinEditChange(Sender: TObject);
    procedure MinSpinEditChange(Sender: TObject);
    procedure StrucParComboChange(Sender: TObject);
    procedure SetBottomAxisCaption;
    procedure VariablesCheckGroupClick(Sender: TObject);
    procedure DrawSensitivityPlot(empty: boolean);
    procedure VariablesCheckGroupItemClick(Sender: TObject; Index: integer);
  private

  public

  end;

var
  OWSensitivityAnalysisForm: TOWSensitivityAnalysisForm;

implementation

{$R *.lfm}

{ TOWSensitivityAnalysisForm }

procedure TOWSensitivityAnalysisForm.FormCreate(Sender: TObject);
begin

end;

procedure TOWSensitivityAnalysisForm.FormPaint(Sender: TObject);
begin

end;

procedure TOWSensitivityAnalysisForm.FormShow(Sender: TObject);
begin

end;

procedure TOWSensitivityAnalysisForm.MaxSpinEditChange(Sender: TObject);
begin
  DrawSensitivityPlot(False);
end;

procedure TOWSensitivityAnalysisForm.MinSpinEditChange(Sender: TObject);
begin
  DrawSensitivityPlot(False);
end;

procedure TOWSensitivityAnalysisForm.StrucParComboChange(Sender: TObject);
begin
  case StrucParCombo.ItemIndex of
    1:
    begin
      MinSpinEdit.Value := MinGBeta;
      MaxSpinEdit.Value := MaxGBeta;
    end;
    2:
    begin
      MinSpinEdit.Value := MinDBeta;
      MaxSpinEdit.Value := MaxDBeta;
    end;
    3:
    begin
      MinSpinEdit.Value := MinGR;
      MaxSpinEdit.Value := MaxGR;
    end;
    4:
    begin
      MinSpinEdit.Value := MinDR;
      MaxSpinEdit.Value := MaxDR;
    end;
    5:
    begin
      MinSpinEdit.Value := MinGE;
      MaxSpinEdit.Value := MaxGE;
    end;
  end;
  SetBottomAxisCaption;
  DrawSensitivityPlot(False);
end;

function ComposedAxisCaption(theString: string): string;
  { sets caption and unit of measurement of bottom axis according to item }
begin
  if theString = 'GBeta' then
    theString := theString + ' (pmol/s)'
  else if theString = 'DBeta' then
    theString := theString + ' (mmol/l)'
  else if theString = 'GR' then
    theString := theString + ' (mol/s)'
  else if theString = 'DR' then
    theString := theString + ' (nmol/l)'
  else if theString = 'GE' then
    theString := theString + ' (s/mol)';
  Result := theString;
end;

procedure TOWSensitivityAnalysisForm.SetBottomAxisCaption;
var
  theCaption: string;
begin
  theCaption := ComposedAxisCaption(StrucParCombo.Text);
  Chart1.BottomAxis.Title.Caption := theCaption;
end;

procedure TOWSensitivityAnalysisForm.VariablesCheckGroupClick(Sender: TObject);
begin
  DrawSensitivityPlot(False);
end;

procedure TOWSensitivityAnalysisForm.DrawSensitivityPlot(empty: boolean);
var
  thePar: TParameter;
  stateVector: tOneWaySensVector;
  minx, maxx, resolution: real;
  i: integer;
begin
  resolution := 0.01;
  PSeries.Clear;
  RSeries.Clear;
  ISeries.Clear;
  GSeries.Clear;
  SSeries.Clear;
  MSeries.Clear;
  NSeries.Clear;
  PSeries.SeriesColor := PColorBox.Selected;
  RSeries.SeriesColor := RColorBox.Selected;
  ISeries.SeriesColor := IColorBox.Selected;
  GSeries.SeriesColor := GColorBox.Selected;
  SSeries.SeriesColor := SColorBox.Selected;
  MSeries.SeriesColor := MColorBox.Selected;
  NSeries.SeriesColor := NColorBox.Selected;
  if VariablesCheckGroup.Checked[0] then
    PSeries.Active := True
  else
    PSeries.Active := False;
  if VariablesCheckGroup.Checked[1] then
    RSeries.Active := True
  else
    RSeries.Active := False;
  if VariablesCheckGroup.Checked[2] then
    GSeries.Active := True
  else
    GSeries.Active := False;
  if VariablesCheckGroup.Checked[3] then
    SSeries.Active := True
  else
    SSeries.Active := False;
  if VariablesCheckGroup.Checked[4] then
    ISeries.Active := True
  else
    ISeries.Active := False;
  if VariablesCheckGroup.Checked[5] then
    MSeries.Active := True
  else
    MSeries.Active := False;
  if VariablesCheckGroup.Checked[6] then
    NSeries.Active := True
  else
    NSeries.Active := False;
  minx := MinSpinEdit.value;
  maxx := MaxSpinEdit.value;
  case StrucParCombo.ItemIndex of
    1:
    begin
      thePar := GBeta;
      stateVector := OneWayVector(minx, maxx, resolution,
        gActiveModel.StrucPars, thePar);
    end;
    2:
    begin
      thePar := DBeta;
      stateVector := OneWayVector(minx, maxx, resolution,
        gActiveModel.StrucPars, thePar);
    end;
    3:
    begin
      thePar := GR;
      stateVector := OneWayVector(minx, maxx, resolution,
        gActiveModel.StrucPars, thePar);
    end;
    4:
    begin
      thePar := DR;
      stateVector := OneWayVector(minx, maxx, resolution,
        gActiveModel.StrucPars, thePar);
    end;
    5:
    begin
      thePar := GE;
      stateVector := OneWayVector(minx, maxx, resolution,
        gActiveModel.StrucPars, thePar);
    end;
  end;
  { #todo -oJWD -cDonostia : check ranges of parameters other than GBeta }
  for i := 0 to length(stateVector) - 1 do
  begin
    PSeries.AddXY(stateVector[i].multi1, stateVector[i].P / PFactor);
    RSeries.AddXY(stateVector[i].multi1, stateVector[i].R / MicroFactor);
    ISeries.AddXY(stateVector[i].multi1, stateVector[i].I / IFactor *
      gInsulinConversionFactor);
    GSeries.AddXY(stateVector[i].multi1, stateVector[i].G / GFactor *
      gGlucoseConversionFactor);
    SSeries.AddXY(stateVector[i].multi1, stateVector[i].S / PicoFactor);
    MSeries.AddXY(stateVector[i].multi1, stateVector[i].M);
    NSeries.AddXY(stateVector[i].multi1, stateVector[i].N);
  end;
end;

procedure TOWSensitivityAnalysisForm.VariablesCheckGroupItemClick(Sender: TObject;
  Index: integer);
begin
  DrawSensitivityPlot(False);
end;

end.
