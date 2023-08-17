unit OWSensitivityAnalysis;

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
    GSeries: TLineSeries;
    MaxSpinEdit: TFloatSpinEdit;
    StrucParCombo: TComboBox;
    MinSpinEdit: TFloatSpinEdit;
    GColorBox: TColorBox;
    IColorBox: TColorBox;
    mColorBox: TColorBox;
    nColorBox: TColorBox;
    PColorBox: TColorBox;
    RColorBox: TColorBox;
    SColorBox: TColorBox;
    VariablesCheckGroup: TCheckGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StrucParComboChange(Sender: TObject);
    procedure SetBottomAxisCaption;
    procedure VariablesCheckGroupClick(Sender: TObject);
    procedure DrawSensitivityPlot(empty: boolean);
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

procedure TOWSensitivityAnalysisForm.StrucParComboChange(Sender: TObject);
begin
  SetBottomAxisCaption;
  DrawSensitivityPlot(false);
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
  result := theString;
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
  DrawSensitivityPlot(false);
end;

procedure TOWSensitivityAnalysisForm.DrawSensitivityPlot(empty: boolean);
var
  PVector, RVector, GVector, SVector: tOneWaySensVector;
  IVector, MVector, NVector: tOneWaySensVector;
  thePar: TParameter;
  stateVector: tOneWaySensVector;
  minx, maxx, resolution: real;
  i: integer;
begin
  resolution := 0.1;
  GSeries.clear;
  GSeries.SeriesColor := GColorBox.Selected;
  { #todo -oJWD -cDonostia : Add other series here }
  case StrucParCombo.ItemIndex of
  1:
    begin
      thePar := GBeta;
      minx := MinGBeta;
      maxx := MaxGBeta;
      stateVector := OneWayVector(minx, maxx, resolution, gActiveModel.StrucPars, thePar);
    end;
  end;
  { #todo -oJWD -cDonostia : add other items here }
  for i := 0 to length(stateVector) - 1 do
  begin
    GSeries.AddXY(stateVector[i].multi1, stateVector[i].G / GFactor * gGlucoseConversionFactor);
    { #todo -oJWD -cDonostia : Add additional variables here }
  end;
end;

end.
