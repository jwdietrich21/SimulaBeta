unit SimulationEngine;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Simulation Engine }

{ Version 2.1.0 (Turning the tides) }

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, bricks, lifeblocks;

type
  tTestKind = (tkNone, tkfsIGT);
  tParameterSpace = record
    alphaG, betaG, alphaI, betaI, GBeta, DBeta, GR, DR, GE: extended;
  end;
  tUnits = record
    P, R, G, S, I, M, N: string;
  end;
  tTestInfo = record
    kind: tTestKind;
    startTime: integer;
  end;


const
  MilliFactor = 1e-3;
  MicroFactor = 1e-6;
  NanoFactor = 1e-9;
  PicoFactor = 1e-12;

  {Conversions:
  alpha = 1 / VD
  t1/2 = VD * ln2 / Clearance
  beta = ln2 / t1/2 = Clearance / VD
  }

  InitialStrucPars: tParameterSpace =
  (
  alphaG: 0.11;          // 1 / VD for glucose (initial phase, 1/L)
                         // [Hirota et al. 1999, PMID 10233198]: 7.24 and 7.27 L
                         // [Sjoestrand et al. 2001, PMID 11878683: 12.3 L
                         // [Sjoestrand and Hahn 2004, PMID 14977794]: 9.14 L
                         // [van Tulder et al. 2005, PMID 16192526]: 90 mL/kg
  betaG: 7.1e-4;         // Clearance exponent (ln2 / half-life) for glucose
                         // [Sjoestrand et al. 2001, PMID 11878683]: t1/2 12.1 min
                         // [Sjoestrand and Hahn 2004, PMID 14977794]: t1/2 11..16 min
                         // [Strandberg and Hahn 2005, PMID 15486008]: t1/2 12..30 min
  alphaI: 0.2;           // 1 / VD for insulin (1/l)
                         // [Rang, H. P. (2003). Pharmacology. Edinburgh:
                         //    Churchill Livingstone. ISBN 0-443-07145-4]
  betaI: 3.4e-3;         // Clearance exponent (Clearance / VD) for insulin
                         // [Turnheim and Waldhaeusl 1988, PMID 3281377]]: 700..800 mL/min
                         // [[Weiss et al. 2015, PMID 26608654]: CL 141..571 mL/min
                         // [[Koschorreck and Gilles 2008, PMID 18477391]: 700..3350 mL/min
  GBeta: 2.8e-12;        // Estimated from NHANES study (mol/s)
  DBeta: 7e-3;           // EC50 of glucose (mol/l)
                         // [Byrne et al. 1994, PMID 8132752]
                         // [Jones et al. 1997, PMID 9177392]: ca. 7 mmol/L
                         // [Jones et al. 2000, PMID 10710505]: ca. 6 mmol/L
                         // Toschi et al. 2002, PMID 11815471]: ca 8 mmol/L
  GR: 2.3;               // Estimated from NHANES study
  DR: 1.6e-9;            // EC50 of insulin (mol/l)
                         // [Natali et al. 2000, PMID 10780934]: 240 mU/L
  GE: 50;                // Calibration factor
  );

  PFactor = MicroFactor;
  P0 = 150 * PFactor;    // Glucose arrival (production + absorption)
                         // Estimated to deliver fasting R between 10 and
                         // 100 mcmol/s
                         // R = P / (1 + N)  =>  P = R + RN
                         // [Sjoestrand and Hahn 2004, PMID 14977794]: 15..85 mcmol/s
                         // [Giebelstein et al. 2012, PMID 22282162]: 129 mcmol/s
  IFactor = PicoFactor;
  I0 = 100 * IFactor;    // Fasting insulin concentration in mol/l
                         // should be 20 to 210 pmol/l (3 to 30 mIU/l)
                         // [Giebelstein et al. 2012, PMID 22282162]: 24 pmol/l
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
    t, P, R, G, S, I, M, N: array of extended;
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
  gUnits: TUnits;
  t: extended;
  delta: real;
  gTestInfo: tTestInfo;

procedure InitUnits;
procedure InitSimulation;
function PredictedEquilibrium(P: extended; StrucPars: tParameterSpace): TPrediction;
procedure RunSimulation(P, Glc, Ins: extended; nmax: integer; prediction: TPrediction);

implementation

function SolveQuadratic(a, b, c: extended): TQRoots;
{ solves quadratic equation with parameters a, b and c }
begin
  Result[0] := -(b + sqrt(sqr(b) - 4 * a * c)) / (2 * a);
  Result[1] := -(b - sqrt(sqr(b) - 4 * a * c)) / (2 * a);
end;

procedure InitUnits;
begin
  gUnits.G := 'mmol/L';
  gUnits.I := 'pmol/L';
  gUnits.P := 'µmol/L';
  gUnits.R := 'µmol/s';
  gUnits.S := 'pmol/s';
  gUnits.M := 'nmol/s';
  gUnits.N := '';
end;

procedure InitSimulation;
begin
  gStrucPars := InitialStrucPars;
  delta := 1;
  t := 0;
  gTestInfo.kind := tkNone;
  gTestInfo.startTime := 0;
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
    gValues.t[0] := 0;
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

procedure RunSimulation(P, Glc, Ins: extended; nmax: integer; prediction: TPrediction);
var
  blocks: TBlocks;
  R, S, M, N: extended;
  i: integer;
begin
  if nmax > 0 then
  begin
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
      blocks.G1.delta := delta;
      blocks.MiMeBeta.G := GBeta;
      blocks.MiMeBeta.D := DBeta;
      blocks.G3.alpha := alphaI;
      blocks.G3.beta := betaI;
      blocks.G3.delta := delta;
      blocks.MiMeR.G := GR;
      blocks.MiMeR.D := DR;
      blocks.GE.G := GE;
      // SetInitialConditions(prediction);  // for future extension
      N := GE * GR * Ins / (DR + Ins);
    end;
    blocks.G1.x1 := Glc; // "prefill" memory elements...
    blocks.G3.x1 := Ins; // ...with provided values
    for i := 1 to nmax do
    begin
      blocks.NoCoDI.input1 := P;
      blocks.NoCoDI.input2 := N;
      R := blocks.NoCoDI.simOutput;
      blocks.G1.input := R;
      Glc := blocks.G1.simOutput;
      if (gTestInfo.kind = tkfsIGT) and (i >= gTestInfo.startTime) then
      begin
        Glc := Glc + (gStrucPars.alphaG * 21000 / 180 * GFactor); // Dosage 21 g for 70 kg
        blocks.G1.x1 := Glc;
        gTestInfo.kind := tkNone; // switch off again
      end;
      blocks.MiMeBeta.input := Glc;
      S := blocks.MiMeBeta.simOutput;
      blocks.G3.input := S;
      Ins := blocks.G3.simOutput;
      blocks.MiMeR.input := Ins;
      M := blocks.MiMeR.simOutput;
      blocks.GE.input := M;
      N := blocks.GE.simOutput;
      t := t + delta;
      gValues.P[i] := P;
      gValues.R[i] := R;
      gValues.G[i] := Glc;
      gValues.S[i] := S;
      gValues.I[i] := Ins;
      gValues.M[i] := M;
      gValues.N[i] := N;
      gValues.t[i] := t;
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
  SetLength(t, aValue);
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

