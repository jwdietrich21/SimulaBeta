unit SimulaBetaServices;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ General services for use by several units }

{ Version 2.0.0 (Sleeping Briar Rose) }

{ (c) Johannes W. Dietrich, 1994 - 2021 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2021 }

{ Standard blocks for systems modelling and simulation }

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
  Classes, SysUtils, DateUtils;

const
  STANDARD_TIME_FORMAT = '"d"D hh:nn:ss';

var
  gDateTimeFormat: String;

function AsTime(x: real): TDateTime;
function FormattedTime(x: real): String;

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

end.

