unit Sequencer;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ LOREMOS: Load, Reference and Modulation Sequencer (GUI) }

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  ExtDlgs, Math, Types, SimulaBetaTypes, SimulationEngine, SequencerEngine;

type

  tMarking = (mOff, mW, mG, mI);

  TInputFields = record
    Enabled: TCheckBoxState;
    Name: string;
    ModType: TEventType;
    Delay: string;
    ka: string;
    alpha: string;
    beta: string;
    c0: string;
    f0: string;
    p1: string;
    Variable: TVariable;
    ModOp: TOperator;
    Amplitude: string;
  end;

  { TSequencerWindow }

  TSequencerWindow = class(TForm)
    ApplyButton: TButton;
    CancelButton: TButton;
    SequencerGrid: TDrawGrid;
    ParameterGrid: TStringGrid;
    procedure ApplyButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ParameterGridCheckboxToggled(Sender: TObject;
      aCol, aRow: integer; aState: TCheckboxState);
    procedure ParameterGridDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure ParameterGridGetCellHint(Sender: TObject; ACol, ARow: integer;
      var HintText: string);
    procedure ParameterGridGetCheckboxState(Sender: TObject;
      ACol, ARow: integer; var Value: TCheckboxState);
    procedure ParameterGridPickListSelect(Sender: TObject);
    procedure ParameterGridSetEditText(Sender: TObject; ACol, ARow: integer;
      const Value: string);
    procedure SequencerGridDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DrawSequencerGrid(Sender: TObject);
  private
    StateTable: array of array of tMarking;
  public
    InputFields: array of TInputFields;
    EventMatrix: TEventMatrix;
  end;

var
  SequencerWindow: TSequencerWindow;

implementation

{$R *.lfm}

{ TSequencerWindow }

procedure TSequencerWindow.FormCreate(Sender: TObject);
var
  i: integer;
begin
  SetLength(InputFields, ParameterGrid.RowCount - 1);
  for i := 0 to length(InputFields) - 1 do
  begin
    ParameterGrid.Cells[0, i + 1] := IntToStr(i);
    InputFields[i].Enabled := cbUnChecked;
    InputFields[i].Delay := IntToStr(0);
    InputFields[i].p1 := '1';
    ParameterGrid.Cells[4, i + 1] := InputFields[i].Delay;
  end;
end;

procedure TSequencerWindow.ApplyButtonClick(Sender: TObject);
var
  i, k, l: integer;
begin
  k := 0;
  l := 0;
  for i := 0 to Length(InputFields) - 1 do
    if InputFields[i].Enabled = cbChecked then Inc(l);
  SetLength(EventMatrix, l);
  for i := 0 to Length(InputFields) - 1 do
    if InputFields[i].Enabled = cbChecked then
    begin
      EventMatrix[k].Name := InputFields[i].Name;
      EventMatrix[k].ModType := InputFields[i].ModType;
      EventMatrix[k].Delay := StrToIntDef(InputFields[i].Delay, -1) * gTestTimeFactor;
      EventMatrix[k].ka := StrToFloatDef(InputFields[i].ka, NaN, gUSFormatSettings);
      EventMatrix[k].alpha := StrToFloatDef(InputFields[i].alpha, NaN, gUSFormatSettings);
      EventMatrix[k].beta := StrToFloatDef(InputFields[i].beta, NaN, gUSFormatSettings);
      EventMatrix[k].c0 := StrToFloatDef(InputFields[i].c0, NaN, gUSFormatSettings);
      EventMatrix[k].f0 := StrToFloatDef(InputFields[i].f0, NaN, gUSFormatSettings);
      EventMatrix[k].p1 := StrToFloatDef(InputFields[i].p1, NaN, gUSFormatSettings);
      EventMatrix[k].Variable := InputFields[i].Variable;
      EventMatrix[k].ModOp := InputFields[i].ModOp;
      if EventMatrix[k].Variable = vI then
        EventMatrix[k].Amplitude :=
          StrToFloatDef(InputFields[i].Amplitude, NaN, gUSFormatSettings) * gInsulinLoadConversionFactor
      else if (EventMatrix[k].Variable = vG) or (EventMatrix[k].Variable = vW) then
        EventMatrix[k].Amplitude :=
          StrToFloatDef(InputFields[i].Amplitude, NaN, gUSFormatSettings) / gGlucLoadConversionFactor
      else
        EventMatrix[k].Amplitude := StrToFloatDef(InputFields[i].Amplitude, NaN, gUSFormatSettings);
      Inc(k);
    end;
  Close;
end;

procedure TSequencerWindow.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TSequencerWindow.ParameterGridCheckboxToggled(Sender: TObject;
  aCol, aRow: integer; aState: TCheckboxState);
begin
  if (aRow > 0) and (aCol = 1) then
    InputFields[aRow - 1].Enabled := aState;
  DrawSequencerGrid(Sender);
end;

procedure TSequencerWindow.ParameterGridDrawCell(Sender: TObject;
  aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
begin
  if (aRow > 0) then
    if (aCol = 2) then
    begin
      // for future extensions
    end;
end;

procedure TSequencerWindow.ParameterGridGetCellHint(Sender: TObject;
  ACol, ARow: integer; var HintText: string);
begin
  case ACol of
    1: HintText := 'The event is only fired if "On" is checked';
    2: HintText := 'Name of the event';
    3: HintText := 'Route of signal';
    4: HintText := 'Begin of the event';
    5: HintText := 'Absorption rate (Bateman function) or duration of signal (iv route)';
    6: HintText := 'beta (clearance exponent, ke or rate constant)';
    7: HintText := 'c0 (calibration factor)';
    8: HintText := 'f0 (bioavailability)';
    9: HintText := 'p1 = (1 / w0 - 1)';
    10: HintText := 'Variable to be modified';
    11: HintText := 'Operand: kind of modification';
    12: HintText := 'Amplitude of modification';
  end;

end;

procedure TSequencerWindow.ParameterGridGetCheckboxState(Sender: TObject;
  ACol, ARow: integer; var Value: TCheckboxState);
begin
  if (aRow > 0) and (aCol = 1) then
    Value := InputFields[aRow - 1].Enabled;
end;

procedure TSequencerWindow.ParameterGridPickListSelect(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to ParameterGrid.RowCount - 1 do
  begin
    if lowercase(ParameterGrid.Cells[3, i]) = 'oral' then
    begin
      ParameterGrid.Cells[4, i] := FloatToStr(3600 / gTestTimeFactor, gUSFormatSettings);
      inputFields[i - 1].Delay := ParameterGrid.Cells[4, i];
      ParameterGrid.Cells[5, i] := 'NaN';
      inputFields[i - 1].ka := ParameterGrid.Cells[5, i];
      ParameterGrid.Cells[6, i] := '0.0002';
      inputFields[i - 1].beta := ParameterGrid.Cells[6, i];
      ParameterGrid.Cells[7, i] := '1360';
      inputFields[i - 1].c0 := ParameterGrid.Cells[7, i];
      ParameterGrid.Cells[8, i] := '0.85';
      inputFields[i - 1].f0 := ParameterGrid.Cells[8, i];
      ParameterGrid.Cells[9, i] := '5.7';
      inputFields[i - 1].p1 := ParameterGrid.Cells[9, i];
      ParameterGrid.Cells[10, i] := 'W';
      inputFields[i - 1].Variable := vW;
      ParameterGrid.Cells[11, i] := ':=';
      inputFields[i - 1].ModOp := assignop;
    end
    else if lowercase(ParameterGrid.Cells[3, i]) = 'iv' then
    begin
      ParameterGrid.Cells[4, i] := FloatToStr(3600 / gTestTimeFactor, gUSFormatSettings);
      inputFields[i - 1].Delay := ParameterGrid.Cells[4, i];
      ParameterGrid.Cells[5, i] := '0';
      inputFields[i - 1].ka := ParameterGrid.Cells[5, i];
      ParameterGrid.Cells[6, i] := 'NaN';
      inputFields[i - 1].beta := ParameterGrid.Cells[6, i];
      ParameterGrid.Cells[7, i] := 'NaN';
      inputFields[i - 1].c0 := ParameterGrid.Cells[7, i];
      ParameterGrid.Cells[8, i] := '1';
      inputFields[i - 1].f0 := ParameterGrid.Cells[8, i];
      ParameterGrid.Cells[9, i] := 'NaN';
      inputFields[i - 1].p1 := ParameterGrid.Cells[9, i];
      ParameterGrid.Cells[11, i] := '+';
      inputFields[i - 1].ModOp := plus;
    end
    else if lowercase(ParameterGrid.Cells[3, i]) = 'sc' then
    begin
      ParameterGrid.Cells[5, i] := '';
      inputFields[i - 1].ka := ParameterGrid.Cells[5, i];
      ParameterGrid.Cells[6, i] := '';
      inputFields[i - 1].beta := ParameterGrid.Cells[6, i];
      ParameterGrid.Cells[7, i] := 'NaN';
      inputFields[i - 1].c0 := ParameterGrid.Cells[7, i];
      ParameterGrid.Cells[8, i] := '1';
      inputFields[i - 1].f0 := ParameterGrid.Cells[8, i];
      ParameterGrid.Cells[9, i] := 'NaN';
      inputFields[i - 1].p1 := ParameterGrid.Cells[9, i];
      ParameterGrid.Cells[11, i] := '+';
      inputFields[i - 1].ModOp := plus;
    end
    else if pos('insulin', LowerCase(ParameterGrid.Cells[3, i])) > 0 then
    begin
      ParameterGrid.Cells[5, i] := '';
      inputFields[i - 1].ka := ParameterGrid.Cells[5, i];
      ParameterGrid.Cells[6, i] := '';
      inputFields[i - 1].beta := ParameterGrid.Cells[6, i];
      ParameterGrid.Cells[7, i] := 'NaN';
      inputFields[i - 1].c0 := ParameterGrid.Cells[7, i];
      ParameterGrid.Cells[8, i] := '0.7'; // sc-bioavailability of most insulins
      inputFields[i - 1].f0 := ParameterGrid.Cells[8, i];
      ParameterGrid.Cells[9, i] := 'NaN';
      inputFields[i - 1].p1 := ParameterGrid.Cells[9, i];
      ParameterGrid.Cells[10, i] := 'I';
      InputFields[i - 1].Variable := vI;
      ParameterGrid.Cells[11, i] := '+';
      inputFields[i - 1].ModOp := plus;
      inputFields[i - 1].ModType := sc;
      // Methods for estimating ka:
      // ka = 1 / (ln(c(t90%)) - ln(c(t10%))) / (t90% - t10%)
      // ka = ln(2) / t50 (radioactivity after applying labelled insulin)
      // (t50 is in most cases very similar to tmax)
      if pos('glulisin', LowerCase(ParameterGrid.Cells[3, i])) > 0 then
      begin
        // https://go.drugbank.com/drugs/DB01309
        // Becker and Frick 2008, PMID 18076215
        // Arnolds et al. 2010, PMID 20429049
        ParameterGrid.Cells[5, i] := FloatToStr(ln(2) / (60 * SecsPerMin), gUSFormatSettings);
        inputFields[i - 1].ka := ParameterGrid.Cells[5, i];
        inputFields[i - 1].alpha := FloatToStr(1 / 13, gUSFormatSettings);
        ParameterGrid.Cells[6, i] := FloatToStr(ln(2) / (42 * SecsPerMin), gUSFormatSettings);
        inputFields[i - 1].beta := ParameterGrid.Cells[6, i];
      end
      else if pos('lispro', LowerCase(ParameterGrid.Cells[3, i])) > 0 then
      begin
        // https://go.drugbank.com/drugs/DB00046
        // Roach and Woodworth 2002, PMID 12403642
        // Becker and Frick 2008, PMID 18076215
        // Leohr et al. 2020, PMID 32468448
        // Lilly Factsheet PA 9351 FSAMP
        ParameterGrid.Cells[5, i] := FloatToStr(ln(2) / (60 * SecsPerMin), gUSFormatSettings);
        inputFields[i - 1].ka := ParameterGrid.Cells[5, i];
        inputFields[i - 1].alpha := FloatToStr(1 / 21, gUSFormatSettings);
        ParameterGrid.Cells[6, i] := FloatToStr(ln(2) / (60 * SecsPerMin), gUSFormatSettings);
        inputFields[i - 1].beta := ParameterGrid.Cells[6, i];
      end
      else if pos('aspart', LowerCase(ParameterGrid.Cells[3, i])) > 0 then
      begin
        // https://go.drugbank.com/drugs/DB01306
        // Arnolds et al. 2010, PMID 20429049
        // Liu et al. 2021, PMID 33947913
        // Drai et al. 2022, PMID 35230749
        ParameterGrid.Cells[5, i] := FloatToStr(ln(2) / (90 * SecsPerMin), gUSFormatSettings);
        inputFields[i - 1].ka := ParameterGrid.Cells[5, i];
        inputFields[i - 1].alpha := FloatToStr(1 / 22, gUSFormatSettings);
        ParameterGrid.Cells[6, i] := FloatToStr(ln(2) / (60 * SecsPerMin), gUSFormatSettings);
        inputFields[i - 1].beta := ParameterGrid.Cells[6, i];
        ParameterGrid.Cells[8, i] := '0.8';
        inputFields[i - 1].f0 := ParameterGrid.Cells[8, i];
      end
      else if pos('human', LowerCase(ParameterGrid.Cells[3, i])) > 0 then
      begin
        // https://go.drugbank.com/drugs/DB00030
        // Becker and Frick 2008, PMID 18076215
        ParameterGrid.Cells[5, i] := FloatToStr(ln(2) / (100 * SecsPerMin), gUSFormatSettings);
        inputFields[i - 1].ka := ParameterGrid.Cells[5, i];
        inputFields[i - 1].alpha := FloatToStr(1 / 22, gUSFormatSettings);
        ParameterGrid.Cells[6, i] := FloatToStr(ln(2) / (120 * SecsPerMin), gUSFormatSettings);
        inputFields[i - 1].beta := ParameterGrid.Cells[6, i];
      end
      else if pos('nph', LowerCase(ParameterGrid.Cells[3, i])) > 0 then
      begin
        // Kølendorf and Bojsen 1982, PMID 7060331
        // Lauritzen et al. 1982, PMID 6807390
        ParameterGrid.Cells[5, i] := FloatToStr(ln(2) / (13 * MinsPerHour * SecsPerMin), gUSFormatSettings);
        inputFields[i - 1].ka := ParameterGrid.Cells[5, i];
        inputFields[i - 1].alpha := FloatToStr(1 / 10, gUSFormatSettings);
        ParameterGrid.Cells[6, i] :=
          FloatToStr(ln(2) / (6.6 * MinsPerHour * SecsPerMin), gUSFormatSettings);
        inputFields[i - 1].beta := ParameterGrid.Cells[6, i];
      end
      else if pos('detemir', LowerCase(ParameterGrid.Cells[3, i])) > 0 then
      begin
        // https://go.drugbank.com/drugs/DB01307
        // Danne et al. 2003, PMID 14578244
        ParameterGrid.Cells[5, i] := FloatToStr(ln(2) / (8 * MinsPerHour * SecsPerMin), gUSFormatSettings);
        inputFields[i - 1].ka := ParameterGrid.Cells[5, i];
        inputFields[i - 1].alpha := FloatToStr(1 / 7, gUSFormatSettings);
        ParameterGrid.Cells[6, i] := FloatToStr(ln(2) / (6 * MinsPerHour * SecsPerMin), gUSFormatSettings);
        inputFields[i - 1].beta := ParameterGrid.Cells[6, i];
        ParameterGrid.Cells[8, i] := '0.6';
        inputFields[i - 1].f0 := ParameterGrid.Cells[8, i];
      end
      else if pos('glargin', LowerCase(ParameterGrid.Cells[3, i])) > 0 then
      begin
        // Lindauer and Becker 2010, PMID 30369394
        // Heise et al. 2015, PMID 26086190
        // Porcellati at al. 2015, PMID 25524950
        // Lucidi et al. 2021, PMID 33444161
        ParameterGrid.Cells[5, i] := FloatToStr(ln(2) / (12 * MinsPerHour * SecsPerMin), gUSFormatSettings);
        inputFields[i - 1].ka := ParameterGrid.Cells[5, i];
        inputFields[i - 1].alpha := FloatToStr(1 / 10, gUSFormatSettings);
        ParameterGrid.Cells[6, i] := FloatToStr(ln(2) / (12 * MinsPerHour * SecsPerMin), gUSFormatSettings);
        inputFields[i - 1].beta := ParameterGrid.Cells[6, i];
      end
      else if pos('degludec', LowerCase(ParameterGrid.Cells[3, i])) > 0 then
      begin
        // https://go.drugbank.com/drugs/DB09564
        // Biester et al. 2014, PMID 24467565
        // Heise et al. 2015, PMID 26086190
        // Lucidi et al. 2021, PMID 33444161
        // Therapeutic Goods Administration: AusPAR Attachment 2 PM-2016-02721-1-5
        ParameterGrid.Cells[5, i] :=
          FloatToStr(ln(2) / (11 * MinsPerHour * SecsPerMin), gUSFormatSettings);
        inputFields[i - 1].ka := ParameterGrid.Cells[5, i];
        inputFields[i - 1].alpha := FloatToStr(1 / 17, gUSFormatSettings);
        ParameterGrid.Cells[6, i] := FloatToStr(ln(2) / (25 * MinsPerHour * SecsPerMin), gUSFormatSettings);
        inputFields[i - 1].beta := ParameterGrid.Cells[6, i];
        ParameterGrid.Cells[8, i] := '0.88';
        inputFields[i - 1].f0 := ParameterGrid.Cells[8, i];
      end;
    end;
  end;
  DrawSequencerGrid(Sender);
end;

procedure TSequencerWindow.ParameterGridSetEditText(Sender: TObject;
  ACol, ARow: integer; const Value: string);
begin
  if (ARow > 0) then
    case aCol of
      1: if ParameterGrid.Cells[ACol, ARow] = '1' then
          InputFields[aRow - 1].Enabled := cbChecked
        else
          InputFields[aRow - 1].Enabled := cbUnchecked;
      2: InputFields[aRow - 1].Name := ParameterGrid.Cells[ACol, ARow];
      3: if ParameterGrid.Cells[ACol, ARow] = 'iv' then
          InputFields[aRow - 1].ModType := iv
        else if ParameterGrid.Cells[ACol, ARow] = 'sc' then
          InputFields[aRow - 1].ModType := sc
        else if ParameterGrid.Cells[ACol, ARow] = 'oral' then
          InputFields[aRow - 1].ModType := oral;
      4: InputFields[aRow - 1].Delay := ParameterGrid.Cells[ACol, ARow];
      5: InputFields[aRow - 1].ka := ParameterGrid.Cells[ACol, ARow];
      6: InputFields[aRow - 1].beta := ParameterGrid.Cells[ACol, ARow];
      7: InputFields[aRow - 1].c0 := ParameterGrid.Cells[ACol, ARow];
      8: InputFields[aRow - 1].f0 := ParameterGrid.Cells[ACol, ARow];
      9: InputFields[aRow - 1].p1 := ParameterGrid.Cells[ACol, ARow];
      10: if ParameterGrid.Cells[ACol, ARow] = 'W' then
          InputFields[aRow - 1].Variable := vW
        else if ParameterGrid.Cells[ACol, ARow] = 'G' then
          InputFields[aRow - 1].Variable := vG
        else if ParameterGrid.Cells[ACol, ARow] = 'I' then
          InputFields[aRow - 1].Variable := vI;
      11: if ParameterGrid.Cells[ACol, ARow] = '+' then
          InputFields[aRow - 1].ModOp := plus
        else if ParameterGrid.Cells[ACol, ARow] = '*' then
          InputFields[aRow - 1].ModOp := times
        else if ParameterGrid.Cells[ACol, ARow] = ':=' then
          InputFields[aRow - 1].ModOp := assignop;
      12: InputFields[aRow - 1].Amplitude := ParameterGrid.Cells[ACol, ARow];
    end;
  DrawSequencerGrid(Sender);
end;

procedure TSequencerWindow.SequencerGridDrawCell(Sender: TObject;
  aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
// Draws cells in colours according to sequencer entry
const
  clTangerine = $000093FF;
var
  theCanvas: TCanvas;
begin
  theCanvas := TDrawGrid(Sender).Canvas;
  if Assigned(StateTable) then
    case StateTable[aCol, aRow] of
      mOff: theCanvas.Brush.Color := SequencerGrid.Color;
      mI: theCanvas.Brush.Color := clRed;
      mG: theCanvas.Brush.Color := clBlue;
      mW: theCanvas.Brush.Color := clTangerine;
    end;
    theCanvas.FillRect(aRect);
end;

procedure TSequencerWindow.DrawSequencerGrid(Sender: TObject);
var
  i, j, k, l, m: integer;
  timeRatio: real;
begin
  k := SequencerGrid.ColCount;
  l := SequencerGrid.RowCount;
  SetLength(StateTable, k, l);
  for i := 0 to l - 2 do
  begin
    timeRatio := StrToFloatDef(InputFields[i].Delay, -1, gUSFormatSettings) * gTestTimeFactor /
      (gSectionIterations);
    if (timeRatio >= 0) and (timeRatio <= 1) then
    begin
      for j := 0 to k - 2 do
        StateTable[j, i] := mOff;  // delete if time has changed
      m := trunc(timeRatio * (k - 1));
      case InputFields[i].Variable of
        vW: StateTable[m, i] := mW;
        vG: StateTable[m, i] := mG;
        vI: StateTable[m, i] := mI;
        otherwise StateTable[m, i] := mOff;
      end;
    end;
  end;
  SequencerGrid.Invalidate;
end;

end.
