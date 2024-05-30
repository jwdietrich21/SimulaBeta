unit StatsEngine;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Engine for statistics }

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
  Classes, SysUtils, Math, SimulaBetaTypes, SimulationEngine;

function FirstVector(values: TValues): TState;
function LastVector(values: TValues): TState;
function MinVector(values: TValues): TState;
function MaxVector(values: TValues): TState;
function MeanVector(values: TValues): TState;
Function MedianVector(values: TValues): TState;
function SDVector(values: TValues): TState;
function SEMVector(values: TValues): TState;
function CoVVector(values: TValues): TState;

type
  TExtArray = array of Extended;

implementation

function SortExtArray(const data: TExtArray): TExtArray;
{ Based on Shell Sort - avoiding recursion allows for sorting of very
  large arrays, too }
var
  data2: TExtArray;
  arrayLength, i, j, k: longint;
  h: extended;
begin
  arrayLength := high(data);
  data2 := copy(data, 0, arrayLength + 1);
  k := arrayLength div 2;
  while k > 0 do
  begin
    for i := 0 to arrayLength - k do
    begin
      j := i;
      while (j >= 0) and (data2[j] > data2[j + k]) do
      begin
        h := data2[j];
        data2[j] := data2[j + k];
        data2[j + k] := h;
        if j > k then
          dec(j, k)
        else
          j := 0;
      end;
    end;
    k := k div 2
  end;
  result := data2;
end;

function median(const data: TExtArray): extended;
{ calculates the median (50% quantile) of a vector of extended }
var
  centralElement: integer;
  sortedData: TExtArray;
begin
  sortedData := SortExtArray(data);
  centralElement := length(sortedData) div 2;
  if odd(length(sortedData)) then
    result := sortedData[centralElement]
  else
    result := (sortedData[centralElement - 1] + sortedData[centralElement]) / 2;
end;

function sem(const data: array of extended): extended;
{ calculates the standard error of the mean of a vector of extended }
  begin
    if length(data) > 0 then
      sem := math.stddev(data) / sqrt(length(data))
    else
      sem := math.NaN;
  end;

function cv(const data: TExtArray): extended;
{ calculates the coefficient of variation (CV or CoV) of a vector of extended }
begin
  if (IsNaN(math.mean(data))) or (math.mean(data) = 0) then
    result := math.NaN
  else
    result := math.stddev(data) / math.mean(data);
end;

function emptyResult: TState;
begin
  result.G := math.NaN;
  result.I := math.NaN;
  result.P := math.NaN;
  result.W := math.NaN;
  result.Q := math.NaN;
  result.R := math.NaN;
  result.S := math.NaN;
  result.M := math.NaN;
  result.N := math.NaN;
end;

function FirstVector(values: TValues): TState;
begin
  if values.size > 0 then
  begin
    result.G := values.G[0];
    result.I := values.I[0];
    result.P := values.P[0];
    result.W := values.W[0];
    result.Q := values.Q[0];
    result.R := values.R[0];
    result.S := values.S[0];
    result.M := values.M[0];
    result.N := values.N[0];
  end
  else
    result := emptyResult;
end;

function LastVector(values: TValues): TState;
begin
  if values.size > 0 then
  begin
    result.G := values.G[values.Size - 1];
    result.I := values.I[values.Size - 1];
    result.P := values.P[values.Size - 1];
    result.W := values.W[values.Size - 1];
    result.Q := values.Q[values.Size - 1];
    result.R := values.R[values.Size - 1];
    result.S := values.S[values.Size - 1];
    result.M := values.M[values.Size - 1];
    result.N := values.N[values.Size - 1];
  end
  else
    result := emptyResult;
end;

function MinVector(values: TValues): TState;
begin
  if values.size > 0 then
  begin
    result.G := MinValue(values.G);
    result.I := MinValue(values.I);
    result.P := MinValue(values.P);
    result.W := MinValue(values.W);
    result.Q := MinValue(values.Q);
    result.R := MinValue(values.R);
    result.S := MinValue(values.S);
    result.M := MinValue(values.M);
    result.N := MinValue(values.N);
  end
  else
    result := emptyResult;
end;

function MaxVector(values: TValues): TState;
begin
  if values.size > 0 then
  begin
    result.G := MaxValue(values.G);
    result.I := MaxValue(values.I);
    result.P := MaxValue(values.P);
    result.W := MaxValue(values.W);
    result.Q := MaxValue(values.Q);
    result.R := MaxValue(values.R);
    result.S := MaxValue(values.S);
    result.M := MaxValue(values.M);
    result.N := MaxValue(values.N);
  end
  else
    result := emptyResult;
end;

function MeanVector(values: TValues): TState;
begin
  if values.size > 0 then
  begin
    result.G := Mean(values.G);
    result.I := Mean(values.I);
    result.P := Mean(values.P);
    result.W := Mean(values.W);
    result.Q := Mean(values.Q);
    result.R := Mean(values.R);
    result.S := Mean(values.S);
    result.M := Mean(values.M);
    result.N := Mean(values.N);
  end
  else
    result := emptyResult;
end;

function MedianVector(values: TValues): TState;
begin
  if values.size > 0 then
  begin
    result.G := median(values.G);
    result.I := median(values.I);
    result.P := median(values.P);
    result.W := median(values.W);
    result.Q := median(values.Q);
    result.R := median(values.R);
    result.S := median(values.S);
    result.M := median(values.M);
    result.N := median(values.N);
  end
  else
    result := emptyResult;
end;

function SDVector(values: TValues): TState;
begin
  if values.size > 0 then
  begin
    result.G := StdDev(values.G);
    result.I := StdDev(values.I);
    result.P := StdDev(values.P);
    result.W := StdDev(values.W);
    result.Q := StdDev(values.Q);
    result.R := StdDev(values.R);
    result.S := StdDev(values.S);
    result.M := StdDev(values.M);
    result.N := StdDev(values.N);
  end
  else
    result := emptyResult;
end;

function SEMVector(values: TValues): TState;
begin
  if values.size > 0 then
  begin
    result.G := sem(values.G);
    result.I := sem(values.I);
    result.P := sem(values.P);
    result.W := sem(values.W);
    result.Q := sem(values.Q);
    result.R := sem(values.R);
    result.S := sem(values.S);
    result.M := sem(values.M);
    result.N := sem(values.N);
  end
  else
    result := emptyResult;
end;

function CoVVector(values: TValues): TState;
begin
  if values.size > 0 then
  begin
    result.G := cv(values.G);
    result.I := cv(values.I);
    result.P := cv(values.P);
    result.W := cv(values.W);
    result.Q := cv(values.Q);
    result.R := cv(values.R);
    result.S := cv(values.S);
    result.M := cv(values.M);
    result.N := cv(values.N);
  end
  else
    result := emptyResult;
end;

end.

