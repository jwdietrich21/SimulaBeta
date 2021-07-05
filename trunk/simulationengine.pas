unit SimulationEngine;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Simulation Engine }

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
  Classes, SysUtils, Forms, bricks, lifeblocks;

type
  tParameterSpace = record
    alphaG, betaG, alphaI, betaI, GBeta, DBeta, GR, DR, GE: extended;
  end;

const
  MilliFactor = 1e-3;
  MicroFactor = 1e-6;
  NanoFactor = 1e-9;
  PicoFactor = 1e-12;

  InitialStrucPars: tParameterSpace =
  (
  alphaG: 0.14;          // 1 / VD for glucose (initial phase, 1/l)
                         // [Hirota et al. 1999, PMID 10233198]
                         // [Sjoestrand and Hahn 2004, PMID 14977794]
                         // [van Tulder et al. 2005, PMID 16192526]
  betaG: 5e-4;           // Clearance exponent (ln2 / half-life) for glucose
                         // [Sjoestrand et al. 2001, PMID 11878683
                         // [Sjoestrand and Hahn 2004, PMID 14977794]
                         // [Strandberg and Hahn 2005, PMID 15486008]
  alphaI: 0.2;           // 1 / VD for insulin (1/l)
                         // [Rang, H. P. (2003). Pharmacology. Edinburgh:
                         //    Churchill Livingstone. ISBN 0-443-07145-4]
  betaI: 4.8e-3;         // Clearance exponent for insulin
                         // [Turnheim and Waldhaeusl 1988, PMID 3281377]]
  GBeta: 3.2e-12;        // to be validated from pilot study (mol/s)
  DBeta: 6e-3;           // EC50 of glucose (mol/l)
                         // [Byrne et al. 1994, PMID 8132752]
                         // [Jones et al. 1997, PMID 9177392]
                         // [Jones et al. 2000, PMID 10710505]
                         // Toschi et al. 2002, PMID 11815471]
  GR: 2;                 // to be validated from pilot study
  DR: 1.5e-9;            // EC50 of insulin (mol/l)
                         // [Natali et al. 2000, PMID 10780934]
  GE: 100;                // Calibration factor
  );

  PFactor = MicroFactor;
  P0 = 150 * PFactor;    // Glucose arrival (production + absorption)
                         // Estimated to deliver fasting R between 10 and
                         // 20 mcmol/s
                         // [Sjoestrand and Hahn 2004, PMID 14977794]
                         // [Giebelstein et al. 2012, PMID 22282162]
  IFactor = PicoFactor;
  I0 = 100 * IFactor;    // Fasting insulin concentration in mol/l
                         // should be 35 to 210 pmol/l (5 to 30 mIU/l)
  GFactor = MilliFactor;
  G0 = 5 * GFactor;      // Fasting glucose concentration in mol/l
                         // should be 3.3 to 5.5 mmol/l (60 to 100 mg/dl)

type

  { TValues }

  TValues = class
  protected
    function GetSize: integer;
    procedure SetSize(aValue: integer);
  public
    P, R, G, S, I, M, N: array of extended;
    constructor Create;
    destructor Destroy;
    property size: integer read GetSize write SetSize;
  end;

  TBlocks = record
    G1, G3: TASIA;
    MiMeBeta, MiMeR: TMiMe;
    GE: TP;
    NoCoDI: TNoCoDI;
  end;

  TSolution = record
    P, R, G, S, I, M, N: extended;
  end;
  TPrediction = array[0..1] of TSolution;
  TQRoots = array[0..1] of extended;

var
  gStrucPars: tParameterSpace;
  gValues: TValues;

procedure InitSimulation;
function PredictedEquilibrium(P: extended; StrucPars: tParameterSpace): TPrediction;
procedure RunSimulation(P, Glc, Ins: extended; nmax: integer);

implementation

function SolveQuadratic(a, b, c: extended): TQRoots;
{ solves quadratic equation with parameters a, b and c }
begin
  Result[0] := -(b + sqrt(sqr(b) - 4 * a * c)) / (2 * a);
  Result[1] := -(b - sqrt(sqr(b) - 4 * a * c)) / (2 * a);
end;

procedure InitSimulation;
begin
  gStrucPars := InitialStrucPars;
end;

procedure SetInitialConditions(Prediction: TPrediction);
{ sets initial conditions from predicted equilibrium, modifiable by GUI }
var
  i: 0..1;
begin
  if gValues.size > 0 then
  begin
    if Prediction[0].G < 0 then  // use positive solution for initial values
      i := 1
    else
      i := 0;
    gValues.P[0] := Prediction[i].P;
    gValues.R[0] := Prediction[i].R;
    gValues.G[0] := Prediction[i].G;
    gValues.S[0] := Prediction[i].S;
    gValues.I[0] := Prediction[i].I;
    gValues.M[0] := Prediction[i].M;
    gValues.N[0] := Prediction[i].N;
  end;
end;

function PredictedEquilibrium(P: extended; StrucPars: tParameterSpace): TPrediction;
var
    a, b, c, K1, K2: extended;
    G1, G3: extended;
begin
  G1 := StrucPars.alphaG / StrucPars.betaG; // Gain of ASIA element
  G3 := StrucPars.alphaI / StrucPars.betaI; // Gain of ASIA element
  Result[0].P := P;
  Result[1].P := P;

  { Solving for G: }
  with StrucPars do
  begin
    K1 := GE * GR * G3 * GBeta / (DR + G3 * GBeta);
    K2 := DR * DBeta / (DR + G3 * GBeta);

    a := 1 + K1;
    b := K2 - G1 * Result[0].P;
    c := -G1 * K2 * Result[1].P;

    Result[0].G := SolveQuadratic(a, b, c)[0];
    Result[0].S := GBeta * Result[0].G / (DBeta + Result[0].G);
    Result[0].I := G3 * Result[0].S;
    Result[0].M := GR * Result[0].I / (DR + Result[0].I);
    Result[0].N := GE * Result[0].M;
    Result[0].R := Result[0].P / (1 + Result[0].N);

    Result[1].G := SolveQuadratic(a, b, c)[1];
    Result[1].S := GBeta * Result[1].G / (DBeta + Result[1].G);
    Result[1].I := G3 * Result[1].S;
    Result[1].M := GR * Result[1].I / (DR + Result[1].I);
    Result[1].N := GE * Result[1].M;
    Result[1].R := Result[1].P / (1 + Result[1].N);
  end;
end;

procedure RunSimulation(P, Glc, Ins: extended; nmax: integer);
var
  blocks: TBlocks;
  R, S, M, N: extended;
  i: integer;
  prediction: TPrediction;
begin
  if nmax > 0 then
  begin
    gValues.size := 0; // delete content
    gValues.size := nmax;
    blocks.G1 := TASIA.Create;
    blocks.G3 := TASIA.Create;
    blocks.GE := TP.Create;
    blocks.MiMeBeta := TMiMe.Create;
    blocks.MiMeR := TMiMe.Create;
    blocks.NoCoDI := TNoCoDI.Create;

    with gStrucPars do
    begin
      blocks.G1.alpha := alphaG;
      blocks.G1.beta := betaG;
      blocks.G1.delta := 1;
      blocks.MiMeBeta.G := GBeta;
      blocks.MiMeBeta.D := DBeta;
      blocks.G3.alpha := alphaI;
      blocks.G3.beta := betaI;
      blocks.G3.delta := 1;
      blocks.MiMeR.G := GR;
      blocks.MiMeR.D := DR;
      blocks.GE.G := GE;
      //Prediction := PredictedEquilibrium(P, gStrucPars);
      //SetInitialConditions(prediction);  // for future extension
      N := GE * GR * Ins / (DR + Ins);
    end;
    blocks.G1.x1 := Glc; // "prefill" memory elements
    blocks.G3.x1 := Ins; // with provided values
    for i := 0 to nmax - 1 do
    begin
      blocks.NoCoDI.input1 := P;
      blocks.NoCoDI.input2 := N;
      R := blocks.NoCoDI.simOutput;
      blocks.G1.input := R;
      Glc := blocks.G1.simOutput;
      blocks.MiMeBeta.input := Glc;
      S := blocks.MiMeBeta.simOutput;
      blocks.G3.input := S;
      Ins := blocks.G3.simOutput;
      blocks.MiMeR.input := Ins;
      M := blocks.MiMeR.simOutput;
      blocks.GE.input := M;
      N := blocks.GE.simOutput;
      gValues.P[i] := P;
      gValues.R[i] := R;
      gValues.G[i] := Glc;
      gValues.S[i] := S;
      gValues.I[i] := Ins;
      gValues.M[i] := M;
      gValues.N[i] := N;
      application.ProcessMessages;
    end;
    blocks.G1.Destroy;
    blocks.G3.Destroy;
    blocks.MiMeBeta.Destroy;
    blocks.MiMeR.Destroy;
    blocks.GE.Destroy;
    blocks.NoCoDI.Destroy;
  end;
end;

{ TValues }

function TValues.GetSize: integer;
begin
  result := Length(R);
end;

procedure TValues.SetSize(aValue: integer);
begin
  SetLength(P, aValue);
  SetLength(R, aValue);
  SetLength(G, aValue);
  SetLength(S, aValue);
  SetLength(I, aValue);
  SetLength(M, aValue);
  SetLength(N, aValue);
end;

constructor TValues.Create;
begin
  inherited Create;
end;

destructor TValues.Destroy;
begin
  inherited Destroy;
end;

end.

