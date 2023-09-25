unit SequencerEngine;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ LOREMOS: Load, Reference and Modulation Sequencer (Engine) }

{ Version 3.2.0 (Donostia) }

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
  Classes, SysUtils, LifeBlocks, SimulaBetaTypes;

var
  gTestTimeFactor: integer;
  gSectionIterations: integer;

procedure SimOralLoad(var x: extended; const d, f, c, p, beta: extended;
  ModOp: TOperator; t: extended);

procedure SimIv(var x: extended; var a: TASIA; const d, td: extended; ModOp: TOperator;
  t: extended);

procedure SimSc(var x: extended; const d, f, ka, beta, v: extended;
  ModOp: TOperator; t: extended);

implementation

function bateman(f, D, V, ka, ke, t: real): real;
begin
  if ka = ke then
    ka := 0.99 * ka; // prevent the denominator from getting zero
  result := f * D / V * ka / (ka - ke) * (exp(-ke * t) - exp(-ka * t));
end;

procedure SimOralLoad(var x: extended; const d, f, c, p, beta: extended;
  ModOp: TOperator; t: extended);
var
  y: extended;
begin
  y := d * f / c / (p + exp(beta * t));
  case ModOp of
    plus:
      x := x + y;
    times:
      x := x * y;
    assignop:
      x := y;
  end;
end;

procedure SimIv(var x: extended; var a: TASIA; const d, td: extended; ModOp: TOperator;
  t: extended);
var
  y: extended;
begin
  if t <= td then
  begin
    y := d;
    case ModOp of
      plus:
        x := x + y;
      times:
        x := x * y;
      assignop:
        x := y;
    end;
    a.x1 := x;
  end;
end;

procedure SimSc(var x: extended; const d, f, ka, beta, v: extended;
  ModOp: TOperator; t: extended);
var
  y: extended;
begin
  y := bateman(f, d, v, ka, beta, t);
  case ModOp of
    plus:
      x := x + y;
    times:
      x := x * y;
    assignop:
      x := y;
  end;
end;

end.

