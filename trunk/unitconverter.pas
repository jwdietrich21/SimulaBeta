unit UnitConverter;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ Unit Converter }

{ Version 1.5.0 (Felis) }

{ (c) J. W. Dietrich, 1994 - 2023 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2024 }

{ Parser and converter for measurement units }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{�http://puma-repository.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$IFDEF FPC}   {Lazarus and Free Pascal}
{$mode objfpc}
{$ENDIF}

{$H+}

{$IFDEF VER10}   {Turbo Pascal 1}
{$DEFINE TURBOPASCAL}
{$ENDIF}

{$IFDEF VER20}   {Turbo Pascal 2}
{$DEFINE TURBOPASCAL}
{$ENDIF}

{$IFDEF VER30}   {Turbo Pascal 3}
{$DEFINE TURBOPASCAL}
{$ENDIF}

{$IFDEF VER40}   {Turbo Pascal 4}
{$DEFINE TURBOPASCAL}
{$ENDIF}

{$IFDEF VER50}   {Turbo Pascal 5}
{$DEFINE TURBOPASCAL}
{$ENDIF}

{$IFDEF VER60}   {Turbo Pascal 6}
{$DEFINE TURBOPASCAL}
{$ENDIF}

{$IFDEF VER70}   {Turbo Pascal 7}
{$DEFINE TURBOPASCAL}
{$ENDIF}

{$IFDEF VER80}   {Delphi 1}
{$DEFINE DELPHI}
{$DEFINE ADVANCEDOBJECTPASCAL}
{$ENDIF}

{$IFDEF VER90}   {Delphi 2}
{$DEFINE DELPHI}
{$DEFINE ADVANCEDOBJECTPASCAL}
{$ENDIF}

{$IFDEF VER100}   {Delphi 3}
{$DEFINE DELPHI}
{$DEFINE ADVANCEDOBJECTPASCAL}
{$ENDIF}

{$IFDEF VER120}   {Delphi 4}
{$DEFINE DELPHI}
{$DEFINE ADVANCEDOBJECTPASCAL}
{$ENDIF}

{$IFDEF VER130}   {Delphi 5}
{$DEFINE DELPHI}
{$DEFINE ADVANCEDOBJECTPASCAL}
{$ENDIF}

{$IFDEF VER140}   {Delphi 6}
{$DEFINE DELPHI}
{$DEFINE ADVANCEDOBJECTPASCAL}
{$ENDIF}

{$IFDEF VER150}   {Delphi 7}
{$DEFINE DELPHI}
{$DEFINE ADVANCEDOBJECTPASCAL}
{$DEFINE FULLMATHAVAILABLE}
{$ENDIF}

{$IFDEF VER160}   {Delphi 8}
{$DEFINE DELPHI}
{$DEFINE ADVANCEDOBJECTPASCAL}
{$DEFINE FULLMATHAVAILABLE}
{$ENDIF}

{$IFDEF VER170}   {Delphi 9}
{$DEFINE DELPHI}
{$DEFINE ADVANCEDOBJECTPASCAL}
{$DEFINE FULLMATHAVAILABLE}
{$ENDIF}

{$IFDEF VER180}   {Delphi 10}
{$DEFINE DELPHI}
{$DEFINE ADVANCEDOBJECTPASCAL}
{$DEFINE FULLMATHAVAILABLE}
{$ENDIF}

{$IFDEF VER185}   {Delphi 11 - Spacely}
{$DEFINE DELPHI}
{$DEFINE ADVANCEDOBJECTPASCAL}
{$DEFINE FULLMATHAVAILABLE}
{$ENDIF}

{$IFDEF VER190}   {Delphi 11 - Highlander and Delphi 12}
{$DEFINE DELPHI}
{$DEFINE ADVANCEDOBJECTPASCAL}
{$DEFINE FULLMATHAVAILABLE}
{$ENDIF}

{$IFDEF VER200}   {Delphi 12}
{$DEFINE DELPHI}
{$DEFINE ADVANCEDOBJECTPASCAL}
{$DEFINE FULLMATHAVAILABLE}
{$ENDIF}

{$IFDEF VER210}   {Delphi 2010}
{$DEFINE DELPHI}
{$DEFINE ADVANCEDOBJECTPASCAL}
{$DEFINE FULLMATHAVAILABLE}
{$ENDIF}

{$IFDEF DCC}   {Delphi XE and newer versions}
{$DEFINE DELPHI}
{$DEFINE DELPHIXE}
{$DEFINE ADVANCEDOBJECTPASCAL}
{$DEFINE FULLMATHAVAILABLE}
{$ENDIF}

{$IFDEF FPC}   {Lazarus and Free Pascal}
{$DEFINE ADVANCEDOBJECTPASCAL}
{$DEFINE FULLMATHAVAILABLE}
{$ENDIF}

interface

uses
  Classes, SysUtils, Math;

const
  UnitConverter_major = 1;
  UnitConverter_minor = 5;
  UnitConverter_release = 0;
  UnitConverter_patch = 0;
  UnitConverter_fullversion = ((UnitConverter_major *  100 + UnitConverter_minor) * 100 + UnitConverter_release) * 100 + UnitConverter_patch;
  UnitConverter_version = '1.5.0.0';
  UnitConverter_internalversion = 'Felis';

  MAXFACTORS = 12; {number of supported prefixes for measurement units}
  {$IFNDEF FULLMATHAVAILABLE}
  NaN = 0.0 / 0.0;
  {$ENDIF}

type
  tMeasurement = record
    Value: extended;
    uom: string;
  end;

  tUnitElements = record
       MassPrefix, MassUnit, VolumePrefix, VolumeUnit: String;
end;

var
  gPosition: integer;
  { the following arrays are provided as global variables rather than as }
  { constants in order to ensure backwards-compatibility with very old }
  { Pascal compilers }
  PrefixLabel: array[0..MAXFACTORS - 1] of string[2];
  PrefixFactor: array[0..MAXFACTORS - 1] of real;
  UnitLabel: array[0..MAXFACTORS - 1] of string;

{$IFNDEF FPC}
function RightStr(const S: string; count: Word): string;
function LeftStr(const S: string; Count: integer): string;
{$ENDIF}
{$IFNDEF FULLMATHAVAILABLE}
function isNaN(const d : Extended): boolean;
{$ENDIF}
procedure InitConversionFactors;
function DecodeGreek(theString: string): string;
function EncodeGreek(theString: string): string;
function ParsedUnitString(theString: String): TUnitElements;
function ParsedMeasurement(measurement: string): tMeasurement;
function ConvertedValue(value, molarMass: real; fromUnit, toUnit: string): real;
function ValueFromUnit(fromValue: string; molarMass: real; toUnit: string): real;
function UnitFromValue(value, molarMass: real; fromUnit, toUnit: string): string;
function UnitFromValueF(value, molarMass: real; fromUnit, toUnit: string; format: TFloatFormat; precision, digits: integer): string;
function ConvertedUnit(fromValue: string; molarMass: real; toUnit: string): string;
function ConvertedUnitF(fromValue: string; molarMass: real; toUnit: string; format: TFloatFormat; precision, digits: integer): string;

implementation

const
  kNUL = chr(0);
  kTAB = chr(9);
  kLF = chr(10);
  kCR = chr(13);
  kETB = char(23);
  DEC_POINT = '.';
  DEC_COMMA = ',';

{ -- FPC adapter functions -- }
{ -- Emulate functionality of Free Pascal in TurboPascal and/or Delphi -- }

{$IFDEF TURBOPASCAL}

function IntToStr(value : integer): string;
var
  theString : string;
begin
  str(value, theString);
  IntToStr := theString;
end;

function FloatToStr(value: Real): string;
var
  theString : string;
begin
  Str(value, theString);
  FloatToStr := theString;
end;

function StrToInt(s: string): integer;
var
  theNumber, theCode: integer;
begin
  val(s, theNumber, theCode);
  StrToInt:= theNumber;
end;

function StrToFloat(s: string): real;
var
  theNumber: real;
  theCode: integer;
begin
  val(s, theNumber, theCode);
  StrToFloat:= theNumber;
end;

{$ENDIF}

{$IFNDEF FPC}

function RightStr(const S: string; count: Word): string;
begin
  if count > Length(S) then count := Length(S) ;
  RightStr := copy(S, Length(S) - count + 1, count)
end;

function LeftStr(const S: string; count: integer): string;
begin
  result := copy(S, 1, count);
end;

{$ENDIF}

{$IFNDEF FULLMATHAVAILABLE}
function isNaN(const d : Extended): boolean;
{isNan support for old Pascal compilers}
{adapted from E. F. Glynn (1998) Mathematics Tech Note}
{Exploring Numbers, Not-A-Number, and Infinity}
{Delphi Developer, 9-14}
VAR
  Overlay: Int64 absolute d;
begin
  result := ((Overlay and $7FF0000000000000) = $7FF0000000000000) and
    ((Overlay and $000FFFFFFFFFFFFF) <> $0000000000000000);
end;
{$ENDIF}

procedure InitConversionFactors;
{sets labels and appropriate conversion factors for the elements of units of measurement}
begin
  PrefixLabel[0] := '';
  PrefixLabel[1] := 'd';        // deci
  PrefixLabel[2] := 'c';        // centi
  PrefixLabel[3] := 'm';        // milli
  PrefixLabel[4] := 'dm';       // dimi, a historical French unit
  PrefixLabel[5] := #194#181;   // micro
  PrefixLabel[6] := 'n';        // nano
  PrefixLabel[7] := 'p';        // pico
  PrefixLabel[8] := 'f';        // femto
  PrefixLabel[9] := 'a';        // atto
  PrefixLabel[10] := 'z';        // zepto
  PrefixLabel[11] := 'y';       // yokto
  PrefixFactor[0] := 1;
  PrefixFactor[1] := 1e-1;
  PrefixFactor[2] := 1e-2;
  PrefixFactor[3] := 1e-3;
  PrefixFactor[4] := 1e-4;
  PrefixFactor[5] := 1e-6;
  PrefixFactor[6] := 1e-9;
  PrefixFactor[7] := 1e-12;
  PrefixFactor[8] := 1e-15;
  PrefixFactor[9] := 1e-18;
  PrefixFactor[10] := 1e-21;
  PrefixFactor[11] := 1e-24;
  UnitLabel[0] := 'g';
  UnitLabel[1] := 'mol';
end;

function ValidChar(theChar: char): boolean;
  {check a character in a string representing a number for validity}
type
  format1 = set of char;
var
  formatn, formatd, formate, formato,
  validformat: format1;
begin
  formatn := ['1'..'9', '0'];
  formatd := ['.', ','];
  formate := ['e', 'E'];
  formato := ['+', '-'];
  validformat := formatn + formatd + formate + formato;
  if theChar in validformat then
    ValidChar := True
  else
    ValidChar := False;
end;

function NextChar(theString: string): char;
{ read next char of string }
begin
  if gPosition <= length(theString) then
  begin
    NextChar := theString[gPosition];
    gPosition := gPosition + 1;
  end
  else
    NextChar := kNUL;
end;

function EncodeGreek(theString: string): string;
{encodes greek mu letter as ASCII substitution sequence}
var
  theFlags: TReplaceFlags;
begin
  theFlags := [rfReplaceAll, rfIgnoreCase];
  Result := stringReplace(theString, #194#181, 'mc', theFlags);
end;

function DecodeGreek(theString: string): string;
{decodes ASCII substitution sequence for greek mu letter}
var
  theFlags: TReplaceFlags;
begin
  theFlags := [rfReplaceAll, rfIgnoreCase];
  {$IFDEF FPC}
  result := UTF8Decode(StringReplace(theString, 'mc', #194#181, theFlags));
  {$ELSE}
  result := stringReplace(theString, 'mc', #194#181, theFlags);
  {$ENDIF}
end;

function ParsedUnitstring(theString: string): TUnitElements;
  {parses a string for measurement unit and breaks it up in single components of a TUnitElements record}
var
  theElements: TUnitElements;
begin
  with theElements do
  begin
    if theString = '' then
    begin
      MassPrefix := '';
      MassUnit := '';
      VolumePrefix := '';
      VolumeUnit := '';
    end
    else if theString = 'NA' then
    begin
      MassPrefix := 'NA';
      MassUnit := 'NA';
      VolumePrefix := 'NA';
      VolumeUnit := 'NA';
    end
    else
      with theElements do
      begin
        if (copy(theString, 1, 1) = 'g') and (copy(theString, 2, 1) = '/') then {e.g. 'g/l'}
          begin
            MassPrefix := '';
            MassUnit := 'g';
          end
        else if copy(theString, 1, 1) = '/' then
          begin
            MassPrefix := 'NA';
            MassUnit := 'NA';
          end
        else if copy(theString, 1, 3) = 'mol' then {e.g. 'mol/l'}
          begin
            MassPrefix := '';
            MassUnit := 'mol';
          end
        else
          begin
            if copy(theString, 1, 1) = 'm' then
            begin
              if copy(theString, 2, 1) = 'c' then
              begin
                MassPrefix := PrefixLabel[5]; {mc -> µ}
                 MassUnit := copy(theString, 3, pos('/', theString) - 3);
              end
              else
              begin
                MassPrefix := 'm';
                MassUnit := copy(theString, 2, pos('/', theString) - 2);
              end;
            end
            else if copy(theString, 1, 1) = 'd' then
            begin
              if copy(theString, 2, 1) = 'd' then
              begin
                MassPrefix := PrefixLabel[4]; {dm}
                 MassUnit := copy(theString, 3, pos('/', theString) - 3);
              end
              else
              begin
                MassPrefix := 'd';
                MassUnit := copy(theString, 2, pos('/', theString) - 2);
              end;
            end
            else if copy(theString, 1, 1) = 'u' then
            begin
              MassPrefix := PrefixLabel[5];
              MassUnit := copy(theString, 2, pos('/', theString) - 2);
            end
            else
            begin
              MassPrefix := copy(theString, 1, 1);
              MassUnit := copy(theString, 2, pos('/', theString) - 2);
            end;
          end;
        if copy(theString, pos('/', theString) + 1, 1) = 'm' then
          begin
            if copy(theString, pos('/', theString) + 2, 1) = 'c' then
              VolumePrefix := PrefixLabel[5] {mc -> µ}
            else
              VolumePrefix := 'm';
          end
        else if copy(theString, pos('/', theString) + 1, 1) = 'd' then
          begin
            if copy(theString, pos('/', theString) + 2, 1) = 'm' then
              VolumePrefix := PrefixLabel[4] {dm}
            else
              VolumePrefix := 'd';
          end
        else if copy(theString, pos('/', theString) + 1, 1) = 'u' then
          VolumePrefix := PrefixLabel[5]
        else
            VolumePrefix := copy(theString, pos('/', theString) + 1, 1);
        VolumeUnit := 'l';
        if VolumePrefix = VolumeUnit then
          VolumePrefix := '';  {no prefix set}
      end;
  end;
  ParsedUnitstring := theElements;
end;

function ParsedMeasurement(measurement: string): tMeasurement;
{ decompose measurement result into numeric value and unit }
var
  ch: char;

  function Number: extended;
  {$IFDEF ADVANCEDOBJECTPASCAL}  {version for FPC, Lazarus and Delphi}
  var
    valstring: string;
  begin
    valstring := '';
    Number := NaN;
    while (ValidChar(ch)) do
    begin
      valstring := valstring + ch;
      ch := NextChar(measurement);
    end;
    {$IFDEF FPC}
    with DefaultFormatSettings do
    {$ENDIF}
    {$IFDEF DELPHIXE}
    with FormatSettings do
    {$ENDIF}
    if pos(DEC_COMMA, valstring) > 0 then
      DecimalSeparator := DEC_COMMA
    else
      DecimalSeparator := DEC_POINT;
    try
      Number := StrToFloat(valstring)
    except
      Number := NaN;
    end;
  end;
  {$ELSE}  {version for other compilers}
  var
    i, j, d, e, f: integer;
    k, l: extended;
    dig: array[0..31] of char;
    wholes, exponent: boolean;
    exponent_sign, base_sign: integer;
  begin
    wholes := True;
    exponent := False;
    f := 0;
    i := 0;
    exponent_sign := 1;
    base_sign := 1;
    repeat
      dig[i] := ch;
      i := i + 1;
      ch := NextChar(measurement);
    until not (ValidChar(ch));
    j := 0;
    k := 0;
    l := 0;
    repeat
      if (dig[j] <> '.') and (dig[j] <> ',') and (uppercase(dig[j]) <>
        uppercase('e')) and (dig[j] <> '+') and (dig[j] <> '-') then
      begin
        d := integer(dig[j]) - $30;
        if not exponent then
        begin
          if wholes then
          begin
            if (d < 10) {and ((maxlongint - d) div 10 >= k)} then
              k := k * 10 + d;
          end
          else
          begin
            f := f + 1;
            k := k + d / (exp(ln(10) * f));
          end;
        end
        else
        begin
          if wholes then
          begin
            if (d < 10) and ((maxlongint - d) div 10 >= l) then
              l := l * 10 + d;
          end
          else
          begin
            f := f + 1;
            l := l + d / (exp(ln(10) * f));
          end;
        end;
      end
      else if (dig[j] = '.') or (dig[j] = ',') then
        wholes := False
      else if uppercase(dig[j]) = uppercase('e') then
      begin
        exponent := True;
        wholes := True;
      end
      else if dig[j] = '-' then
        if exponent then
          exponent_sign := -1
        else
          base_sign := -1;
      j := j + 1;
    until j = i;
    k := k * base_sign;
    l := l * exponent_sign;
    number := k * exp(ln(10) * l);
  end;
  {$ENDIF}

begin
  gPosition := 1;
  ch := NextChar(measurement);
  if not ValidChar(ch) then
  begin
    parsedMeasurement.Value := NaN;
    parsedMeasurement.uom := '';
  end
  else
  begin
    parsedMeasurement.Value := Number;
    parsedMeasurement.uom := RightStr(measurement, length(measurement) - gPosition + 1);
  end;
end;

function ConvertedValue(value, molarMass: real; fromUnit, toUnit: string): real;
{ converts value from one measurement unit to another one }
var
  fromUnitElements, toUnitElements: TUnitElements;
  i, fromMpIndex, fromMuIndex, fromVpIndex, toMpIndex, toMuIndex, toVpIndex: integer;
  conversionFactor: real;
begin
  if isNaN(value) then
  ConvertedValue := -NaN
  else
    begin
      fromMpIndex := 0;    {Index for mass prefix}
      fromMuIndex := 0;    {index for mass unit}
      fromVpIndex := 0;    {index for volume prefix}
      toMpIndex := 0;    {Index for mass prefix}
      toMuIndex := 0;    {index for mass unit}
      toVpIndex := 0;    {index for volume prefix}
      fromUnitElements := ParsedUnitstring(EncodeGreek(fromUnit));
      toUnitElements := ParsedUnitstring(EncodeGreek(toUnit));
      {$IFDEF FPC}   {Lazarus and Free Pascal}
      for i := MAXFACTORS - 1 downto 0 do
        begin
          if fromUnitElements.MassPrefix = PrefixLabel[i] then fromMpIndex := i;
          if fromUnitElements.MassUnit = UnitLabel[i] then fromMuIndex := i;
          if fromUnitElements.VolumePrefix = PrefixLabel[i] then fromVpIndex := i;
          if toUnitElements.MassPrefix = PrefixLabel[i] then toMpIndex := i;
          if toUnitElements.MassUnit = UnitLabel[i] then toMuIndex := i;
          if toUnitElements.VolumePrefix = PrefixLabel[i] then toVpIndex := i;
        end;
      {$ELSE}
      for i := MAXFACTORS - 1 downto 0 do
       begin
         if fromUnitElements.MassPrefix = RightStr(PrefixLabel[i], 1) then fromMpIndex := i;
         if fromUnitElements.MassUnit = UnitLabel[i] then fromMuIndex := i;
         if fromUnitElements.VolumePrefix = RightStr(PrefixLabel[i], 1) then fromVpIndex := i;
         if toUnitElements.MassPrefix = RightStr(PrefixLabel[i], 1) then toMpIndex := i;
         if toUnitElements.MassUnit = UnitLabel[i] then toMuIndex := i;
         if toUnitElements.VolumePrefix = RightStr(PrefixLabel[i], 1) then toVpIndex := i;
       end;
      {$ENDIF}
      if (fromUnitElements.MassUnit = 'mol') and (toUnitElements.MassUnit = 'g') then        {SI to conventional}
        conversionFactor := PrefixFactor[fromMpIndex] * molarMass / PrefixFactor[fromVpIndex] * PrefixFactor[toVpIndex] / PrefixFactor[toMpIndex]
      else if (fromUnitElements.MassUnit = 'g') and (toUnitElements.MassUnit = 'mol') then   {conventional to SI}
        conversionFactor := PrefixFactor[fromMpIndex] * 1 / molarMass / PrefixFactor[fromVpIndex] * PrefixFactor[toVpIndex] / PrefixFactor[toMpIndex]
      else if (fromUnitElements.MassUnit = 'mol') and ((toUnitElements.MassUnit = 'U') or (toUnitElements.MassUnit = 'IU')) then        {SI to activity}
        conversionFactor := PrefixFactor[fromMpIndex] * molarMass / PrefixFactor[fromVpIndex] * PrefixFactor[toVpIndex] / PrefixFactor[toMpIndex]
      else if ((fromUnitElements.MassUnit = 'U') or (fromUnitElements.MassUnit = 'IU')) and (toUnitElements.MassUnit = 'mol') then      {activity to SI}
        conversionFactor := PrefixFactor[fromMpIndex] * 1 / molarMass / PrefixFactor[fromVpIndex] * PrefixFactor[toVpIndex] / PrefixFactor[toMpIndex]
      else if fromUnitElements.MassUnit = toUnitElements.MassUnit then                       {identical units}
        conversionFactor := PrefixFactor[fromMpIndex] / PrefixFactor[fromVpIndex] * PrefixFactor[toVpIndex] / PrefixFactor[toMpIndex]
      else conversionFactor := NaN;
      ConvertedValue := value * conversionFactor;
    end;
end;

function ValueFromUnit(fromValue: string; molarMass: real; toUnit: string): real;
{ converts value from one measurement unit to another one and delivers numeric result }
var
  value, target: real;
  fromUnit: string;
  theMeasurement: tMeasurement;
begin
  if fromValue = '' then
  ValueFromUnit := NaN
  else
    begin
      theMeasurement := ParsedMeasurement(fromValue);
      value := theMeasurement.Value;
      fromUnit := theMeasurement.uom;
      target := ConvertedValue(value, molarMass, fromUnit, toUnit);
      ValueFromUnit := target;
    end;
end;

function UnitFromValue(value, molarMass: real; fromUnit, toUnit: string): string;
var
  target: real;
begin
  if isNaN(value) then
  UnitFromValue := 'NaN'
  else
    begin
      target := ConvertedValue(value, molarMass, fromUnit, toUnit);
      UnitFromValue := FloatToStr(target) + ' ' + toUnit;
    end;
end;

function UnitFromValueF(value, molarMass: real; fromUnit, toUnit: string; format: TFloatFormat; precision, digits: integer): string;
var
  target: real;
begin
  if isNaN(value) then
  UnitFromValueF := 'NaN'
  else
    begin
      target := ConvertedValue(value, molarMass, fromUnit, toUnit);
      UnitFromValueF := FloatToStrF(target, format, precision, digits) + ' ' + toUnit;
    end;
end;

function ConvertedUnit(fromValue: string; molarMass: real; toUnit: string): string;
{ converts value from one measurement unit to another one and delivers string with result }
var
  value, target: real;
  fromUnit: string;
  theMeasurement: tMeasurement;
begin
  if fromValue = '' then
  ConvertedUnit := ''
  else
    begin
      theMeasurement := ParsedMeasurement(fromValue);
      value := theMeasurement.Value;
      fromUnit := theMeasurement.uom;
      target := ConvertedValue(value, molarMass, fromUnit, toUnit);
      ConvertedUnit := FloatToStr(target) + ' ' + toUnit;
    end;
end;

function ConvertedUnitF(fromValue: string; molarMass: real; toUnit: string; format: TFloatFormat; precision, digits: integer): string;
var
  value, target: real;
  fromUnit: string;
  theMeasurement: tMeasurement;
begin
  if fromValue = '' then
  ConvertedUnitF := ''
  else
    begin
      theMeasurement := ParsedMeasurement(fromValue);
      value := theMeasurement.Value;
      fromUnit := theMeasurement.uom;
      target := ConvertedValue(value, molarMass, fromUnit, toUnit);
      ConvertedUnitF := FloatToStrF(target, format, precision, digits) + ' ' + toUnit;
    end;
end;

initialization
  InitConversionFactors;

end.
