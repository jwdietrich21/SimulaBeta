unit SimulaBetaBaseServices;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ General GUI-independent services for use by several units }

{ Version 2.2.0 (Malakow) }

{ (c) Johannes W. Dietrich, 1994 - 2022 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2022 }

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
  Classes, SysUtils, DateUtils
  {$IFDEF WINDOWS}
  , Windows, Win32Proc, registry
  {$ENDIF}
  {$IFDEF DARWIN}
  , MacOSAll
  {$ENDIF}
  {$IFDEF LCLCocoa}
  , CocoaAll, CocoaUtils
  {$ENDIF}
  {$IFDEF UNIX}
  , Unix
  {$ENDIF}
  , SimulaBetaTypes;

function AsTime(x: real): TDateTime;
function FormattedTime(x: real): String;
procedure bell;

implementation

function AsTime(x: real): TDateTime;
  {Converts second values to TDateTime representation}
var
  r: longint;
  y, d: word;
  theTime, theDate: TDateTime;
begin
  y := 1900;
  d := 1;
  theDate := EncodeDateDay(y, d);
  r := trunc(x);
  theTime := IncSecond(theDate, r);
  AsTime := theTime;
end;

function FormattedTime(x: real): String;   {Converts second values to a formatted time}
begin
  gDateTimeFormat := STANDARD_TIME_FORMAT;
  FormattedTime := FormatDateTime(gDateTimeFormat, AsTime(x));
end;

procedure bell; {platform-independent implementation of acustical warning}
var
  s: longint;
begin
  {$IFDEF WINDOWS}
  MessageBeep(0);
  {$ELSE}
  {$IFDEF LCLCarbon}
  SysBeep(30);
  {$ELSE}
  s := fpSystem('echo -ne ''\007''');
  {s := fpSystem('echo -ne "\a"');}
  {s := fpSystem('tput bel');}
  {beep;}
    {$ENDIF}
  {$ENDIF}
end;

end.

