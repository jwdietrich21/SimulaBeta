unit SensitivityAnalysis;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Engine for sensitivity analysis }

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
  Classes, SysUtils, SimulaBetaTypes, SimulationEngine;

type
  tTwoWaySensTable = array of array of TState;

function TwoWayTable(const xmin, xmax, ymin, ymax, resolutionx, resolutiony: real;
    const StrucPars: tParameterSpace; modX, modY: TParameter): tTwoWaySensTable;

implementation

function TwoWayTable(const xmin, xmax, ymin, ymax, resolutionx, resolutiony: real;
  const StrucPars: tParameterSpace; modX, modY: TParameter): tTwoWaySensTable;
var
  i, j, k: integer;
  maxi, maxj: integer;
  params: tParameterSpace;
  prediction: TPrediction;
begin
  params := StrucPars;
  prediction := PredictedEquilibrium(P0, 0, Z0, params);
  if prediction[0].G > 0 then
    k := 0
  else
    K := 1;
  maxi := trunc((xmax - xmin) / resolutionx) + 3;
  maxj := trunc((ymax - ymin) / resolutiony) + 3;
  SetLength(result, maxi, maxj);
  for i := 0 to maxi - 1 do
    for j := 0 to maxj - 1 do
      begin
        case modX of
          GR: params.GR := xmin + resolutionx * (i);
          GBeta: params.GBeta := (xmin + resolutionx * (i)) * PicoFactor;
        end;
        case modY of
          GR: params.GR := ymin + resolutiony * (j);
          GBeta: params.GBeta := (ymin + resolutiony * (j)) * PicoFactor;
        end;
        prediction := PredictedEquilibrium(P0, 0, Z0, params);
        result[i, j] := prediction[k];
      end;
end;


end.

