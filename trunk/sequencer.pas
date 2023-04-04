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

  { TSequencerWindow }

  TInputFields = record
    Enabled: TCheckBoxState;
    Name: string;
    ModType: TEventType;
    Delay: string;
    ka: string;
    beta: string;
    c0: string;
    f0: string;
    p1: string;
    Variable: TVariable;
    ModOp: TOperator;
    Amplitude: string;
  end;

  TSequencerWindow = class(TForm)
    ApplyButton: TButton;
    CancelButton: TButton;
    SequencerGrid: TDrawGrid;
    ParameterGrid: TStringGrid;
    procedure ApplyButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ParameterGridCheckboxToggled(Sender: TObject; aCol,
      aRow: Integer; aState: TCheckboxState);
    procedure ParameterGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure ParameterGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure ParameterGridGetCheckboxState(Sender: TObject; ACol,
      ARow: Integer; var Value: TCheckboxState);
    procedure ParameterGridPickListSelect(Sender: TObject);
    procedure ParameterGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private

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
    if InputFields[i].Enabled = cbChecked then inc(l);
  SetLength(EventMatrix, l);
  for i := 0 to Length(InputFields) - 1 do
    if InputFields[i].Enabled = cbChecked then
    begin
      EventMatrix[k].Name := InputFields[i].Name;
      EventMatrix[k].ModType := InputFields[i].ModType;
      EventMatrix[k].Delay := StrToIntDef(InputFields[i].Delay, -1) * gTestTimeFactor;
      EventMatrix[k].ka := StrToFloatDef(InputFields[i].ka, NaN);
      EventMatrix[k].beta := StrToFloatDef(InputFields[i].beta, NaN);
      EventMatrix[k].c0 := StrToFloatDef(InputFields[i].c0, NaN);
      EventMatrix[k].f0 := StrToFloatDef(InputFields[i].f0, NaN);
      EventMatrix[k].p1 := StrToFloatDef(InputFields[i].p1, NaN);
      EventMatrix[k].Variable := InputFields[i].Variable;
      EventMatrix[k].ModOp := InputFields[i].ModOp;
      if EventMatrix[k].Variable = vI then
        EventMatrix[k].Amplitude := StrToFloatDef(InputFields[i].Amplitude, NaN) * IFactor / gInsulinConversionFactor
      else if (EventMatrix[k].Variable = vG) or (EventMatrix[k].Variable = vW) then
        EventMatrix[k].Amplitude := StrToFloatDef(InputFields[i].Amplitude, NaN) / gGlucLoadConversionFactor
      else
        EventMatrix[k].Amplitude := StrToFloatDef(InputFields[i].Amplitude, NaN);
      inc(k);
    end;
  Close;
end;

procedure TSequencerWindow.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TSequencerWindow.ParameterGridCheckboxToggled(Sender: TObject; aCol,
  aRow: Integer; aState: TCheckboxState);
begin
  if (aRow > 0) and (aCol = 1) then
    InputFields[aRow - 1].Enabled := aState;
end;

procedure TSequencerWindow.ParameterGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  if (aRow > 0) then
    if (aCol = 2) then
    begin
      // for future extensions
    end;
end;

procedure TSequencerWindow.ParameterGridGetCellHint(Sender: TObject; ACol,
  ARow: Integer; var HintText: String);
begin
  case ACol of
    1:  HintText := 'The event is only fired if "On" is checked';
    2:  HintText := 'Name of the event';
    3:  HintText := 'Route of signal';
    4:  HintText := 'Begin of the event';
    5:  HintText := 'Absorption rate (Bateman function) or duration of signal (iv route)';
    6:  HintText := 'beta (clearance exponent, ke or rate constant)';
    7:  HintText := 'c0 (calibration factor)';
    8:  HintText := 'f0 (bioavailability)';
    9:  HintText := 'p1 = (1 / w0 - 1)';
    10:  HintText := 'Variable to be modified';
    11: HintText := 'Operand: kind of modification';
    12: HintText := 'Amplitude of modification';
  end;

end;

procedure TSequencerWindow.ParameterGridGetCheckboxState(Sender: TObject; ACol,
  ARow: Integer; var Value: TCheckboxState);
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
      ParameterGrid.Cells[4, i] := FloatToStr(3600 / gTestTimeFactor);
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
      ParameterGrid.Cells[4, i] := FloatToStr(3600 / gTestTimeFactor);
      inputFields[i - 1].Delay := ParameterGrid.Cells[4, i];
      //ParameterGrid.Cells[5, i] := '';
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
      ParameterGrid.Cells[8, i] := '1';
      inputFields[i - 1].f0 := ParameterGrid.Cells[8, i];
      ParameterGrid.Cells[9, i] := 'NaN';
      inputFields[i - 1].p1 := ParameterGrid.Cells[9, i];
      ParameterGrid.Cells[10, i] := 'I';
      InputFields[i - 1].Variable := vI;
      ParameterGrid.Cells[11, i] := '+';
      inputFields[i - 1].ModOp := plus;
      inputFields[i - 1].ModType := sc;
      if pos('glulisin', LowerCase(ParameterGrid.Cells[3, i])) > 0 then
      begin
        // https://go.drugbank.com/drugs/DB01309
        ParameterGrid.Cells[5, i] := FloatToStr(ln(2) / (1 * SecsPerMin)); ;
        inputFields[i - 1].ka := ParameterGrid.Cells[5, i];
        ParameterGrid.Cells[6, i] := FloatToStr(ln(2) / (42 * SecsPerMin));
        inputFields[i - 1].beta := ParameterGrid.Cells[6, i];
      end
      else if pos('lispro', LowerCase(ParameterGrid.Cells[3, i])) > 0 then
      begin
        // https://go.drugbank.com/drugs/DB00046
        ParameterGrid.Cells[5, i] := FloatToStr(ln(2) / (1 * SecsPerMin)); ;
        inputFields[i - 1].ka := ParameterGrid.Cells[5, i];
        ParameterGrid.Cells[6, i] := FloatToStr(ln(2) / (60 * SecsPerMin));
        inputFields[i - 1].beta := ParameterGrid.Cells[6, i];
      end
      else if pos('aspart', LowerCase(ParameterGrid.Cells[3, i])) > 0 then
      begin
        // https://go.drugbank.com/drugs/DB01306
        ParameterGrid.Cells[5, i] := FloatToStr(ln(2) / (1 * SecsPerMin)); ;
        inputFields[i - 1].ka := ParameterGrid.Cells[5, i];
        ParameterGrid.Cells[6, i] := FloatToStr(ln(2) / (81 * SecsPerMin));
        inputFields[i - 1].beta := ParameterGrid.Cells[6, i];
      end
      else if pos('human', LowerCase(ParameterGrid.Cells[3, i])) > 0 then
      begin
        // https://go.drugbank.com/drugs/DB00030
        ParameterGrid.Cells[5, i] := FloatToStr(ln(2) / (1 * SecsPerMin)); ;
        inputFields[i - 1].ka := ParameterGrid.Cells[5, i];
        ParameterGrid.Cells[6, i] := FloatToStr(ln(2) / (120 * SecsPerMin));
        inputFields[i - 1].beta := ParameterGrid.Cells[6, i];
      end
      else if pos('nph', LowerCase(ParameterGrid.Cells[3, i])) > 0 then
      begin
        // Kølendorf and Bojsen 1982, PMID 7060331
        ParameterGrid.Cells[5, i] := FloatToStr(ln(2) / (1 * SecsPerMin)); ;
        inputFields[i - 1].ka := ParameterGrid.Cells[5, i];
        ParameterGrid.Cells[6, i] := FloatToStr(ln(2) / (6.6 * MinsPerHour * SecsPerMin));
        inputFields[i - 1].beta := ParameterGrid.Cells[6, i];
      end
      else if pos('detemir', LowerCase(ParameterGrid.Cells[3, i])) > 0 then
      begin
        // https://go.drugbank.com/drugs/DB01307
        ParameterGrid.Cells[5, i] := FloatToStr(ln(2) / (1 * SecsPerMin)); ;
        inputFields[i - 1].ka := ParameterGrid.Cells[5, i];
        ParameterGrid.Cells[6, i] := FloatToStr(ln(2) / (6 * MinsPerHour * SecsPerMin));
        inputFields[i - 1].beta := ParameterGrid.Cells[6, i];
      end
      else if pos('glargin', LowerCase(ParameterGrid.Cells[3, i])) > 0 then
      begin
        // Heise et al. 2015, PMID 26086190
        ParameterGrid.Cells[5, i] := FloatToStr(ln(2) / (1 * SecsPerMin)); ;
        inputFields[i - 1].ka := ParameterGrid.Cells[5, i];
        ParameterGrid.Cells[6, i] := FloatToStr(ln(2) / (12.1 * MinsPerHour * SecsPerMin));
        inputFields[i - 1].beta := ParameterGrid.Cells[6, i];
      end
      else if pos('degludec', LowerCase(ParameterGrid.Cells[3, i])) > 0 then
      begin
        // Heise et al. 2015, PMID 26086190
        // https://go.drugbank.com/drugs/DB09564
        ParameterGrid.Cells[5, i] := FloatToStr(ln(2) / (1 * SecsPerMin)); ;
        inputFields[i - 1].ka := ParameterGrid.Cells[5, i];
        ParameterGrid.Cells[6, i] := FloatToStr(ln(2) / (25.4 * MinsPerHour * SecsPerMin));
        inputFields[i - 1].beta := ParameterGrid.Cells[6, i];
      end;
    end;
  end;
end;

procedure TSequencerWindow.ParameterGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  if (ARow > 0) then
    case aCol of
    1:  if ParameterGrid.Cells[ACol, ARow] = '1' then
          InputFields[aRow - 1].Enabled := cbChecked
        else
          InputFields[aRow - 1].Enabled := cbUnchecked;
    2:  InputFields[aRow - 1].Name := ParameterGrid.Cells[ACol, ARow];
    3:  if ParameterGrid.Cells[ACol, ARow] = 'iv' then
          InputFields[aRow - 1].ModType := iv
        else if  ParameterGrid.Cells[ACol, ARow] = 'sc' then
          InputFields[aRow - 1].ModType := sc
        else if ParameterGrid.Cells[ACol, ARow] = 'oral' then
          InputFields[aRow - 1].ModType := oral;
    4:  InputFields[aRow - 1].Delay := ParameterGrid.Cells[ACol, ARow];
    5:  InputFields[aRow - 1].ka := ParameterGrid.Cells[ACol, ARow];
    6:  InputFields[aRow - 1].beta := ParameterGrid.Cells[ACol, ARow];
    7:  InputFields[aRow - 1].c0 := ParameterGrid.Cells[ACol, ARow];
    8:  InputFields[aRow - 1].f0 := ParameterGrid.Cells[ACol, ARow];
    9:  InputFields[aRow - 1].p1 := ParameterGrid.Cells[ACol, ARow];
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
end;

end.

