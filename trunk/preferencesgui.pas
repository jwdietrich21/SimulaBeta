unit PreferencesGUI;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ GUI for Preferences }

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UnitConverter, SimulaBetaTypes, SimulaBetaResources, PreferencesServices,
  SimulationEngine, Prediction, LogGrid, Plot, Stats;

type

  { TPreferencesDialog }

  TPreferencesDialog = class(TForm)
    CancelButton: TButton;
    DivLabel1: TLabel;
    DivLabel2: TLabel;
    InsLoadExampleLabel: TLabel;
    grammLabel: TLabel;
    GlucLoadExampleLabel: TLabel;
    GlucLoadLabel: TLabel;
    InsLoadLabel: TLabel;
    IULabel: TLabel;
    NumbersExampleLabel: TLabel;
    NumberFormatEdit: TEdit;
    DateTimeFormatEdit: TEdit;
    NumberFormatLabel: TLabel;
    InsulinExampleLabel: TLabel;
    GlucoseExampleLabel: TLabel;
    GlucoseMassPrefixCombo: TComboBox;
    GlucoseMassUnitCombo: TComboBox;
    GlucoseVolumePrefixCombo: TComboBox;
    DateTimeFormatFormatLabel: TLabel;
    VolLabel1: TLabel;
    InsulinMassPrefixCombo: TComboBox;
    InsulinLabel: TLabel;
    GlucoseLabel: TLabel;
    InsulinVolumePrefixCombo: TComboBox;
    InsulinMassUnitCombo: TComboBox;
    OKButton: TButton;
    UOMGroupBox: TGroupBox;
    FormatsGroupBox: TGroupBox;
    VolLabel2: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GlucoseMassPrefixComboChange(Sender: TObject);
    procedure GlucoseMassUnitComboChange(Sender: TObject);
    procedure GlucoseVolumePrefixComboChange(Sender: TObject);
    procedure InsulinMassPrefixComboChange(Sender: TObject);
    procedure InsulinMassUnitComboChange(Sender: TObject);
    procedure InsulinVolumePrefixComboChange(Sender: TObject);
    procedure NumberFormatEditChange(Sender: TObject);
    procedure NumberFormatEditEditingDone(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure DisplayExamples;
  private

  public
    InsulinUoM, GlucoseUoM, GlucLoadUoM, InsLoadUoM: string;
  end;

const
  kExampleNumber = 123456.789;
  kInsulinExample = 60;     // pmol/L, corresponding to 10 mIU/L
  kGlucoseExample = 5.56;   // mmol/L, corresponding to 100 mg/dL
  kOralGlucoseExample = 75; // usual oGTT dosage in g
  kExtInsulinExample = 10;   // International units

var
  PreferencesDialog: TPreferencesDialog;

procedure UpdateReporting;

implementation

procedure UpdateReporting;
begin
  PredictionForm.DisplayPrediction(gActiveModel.Prediction);
  LogWindow.FillGrid(gActiveModel.iterations);
  PlotForm.ShowPlot;
  if StatsForm.Visible then StatsForm.ShowContent(nil);
end;

{$R *.lfm}

{ TPreferencesDialog }

procedure TPreferencesDialog.OKButtonClick(Sender: TObject);
begin
  gUnits.I := InsulinUoM;
  gUnits.G := GlucoseUoM;
  SetConversionFactors;
  gNumberFormat := NumberFormatEdit.Text;
  gDateTimeFormat := DateTimeFormatEdit.Text;
  UpdateReporting;
  SavePreferences;
  Close;
end;

procedure TPreferencesDialog.DisplayExamples;
begin
  InsulinUoM := InsulinMassPrefixCombo.Text + InsulinMassUnitCombo.Text + '/' + InsulinVolumePrefixCombo.Text + 'l';
  GlucoseUoM := GlucoseMassPrefixCombo.Text + GlucoseMassUnitCombo.Text + '/' + GlucoseVolumePrefixCombo.Text + 'l';
  GlucLoadUoM := 'g';
  InsLoadUoM := 'IU';
  InsulinExampleLabel.Caption := EXAMPLE_STRING + UnitFromValueF(kInsulinExample, kInsulinActivity, 'pmol/l', InsulinUoM, ffNumber, 2, 2);
  GlucoseExampleLabel.Caption := EXAMPLE_STRING + UnitFromValueF(kGlucoseExample, kMolarMassGlucose, 'mmol/l', GlucoseUoM, ffNumber, 2, 2);
  GlucLoadExampleLabel.Caption := EXAMPLE_STRING + IntToStr(kOralGlucoseExample) + ' ' + GlucLoadUoM;
  InsLoadExampleLabel.Caption := EXAMPLE_STRING + IntToStr(kExtInsulinExample) + ' ' + InsLoadUoM;
  NumbersExampleLabel.Caption := EXAMPLE_STRING + FormatFloat(NumberFormatEdit.Text, kExampleNumber);
end;

procedure TPreferencesDialog.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TPreferencesDialog.FormShow(Sender: TObject);
begin
  NumberFormatEdit.Text := gNumberFormat;
  DateTimeFormatEdit.Text := gDateTimeFormat;
  DisplayExamples;
end;

procedure TPreferencesDialog.GlucoseMassPrefixComboChange(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.GlucoseMassUnitComboChange(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.GlucoseVolumePrefixComboChange(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.InsulinMassPrefixComboChange(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.InsulinMassUnitComboChange(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.InsulinVolumePrefixComboChange(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.NumberFormatEditChange(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.NumberFormatEditEditingDone(Sender: TObject);
begin
  DisplayExamples;
end;

end.

