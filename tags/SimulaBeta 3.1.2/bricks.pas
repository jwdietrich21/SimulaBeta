unit Bricks;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Bricks: Basic blocks for information processing structures }

{ Version 2.0.0 (Escorpión) }

{ (c) Johannes W. Dietrich, 1994 - 2023 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2023 }

{ Standard blocks for systems modelling and simulation }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://cyberunits.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}
{$H+}
{$ASSERTIONS ON}

interface

uses
  Classes, SysUtils, Math, ucomplex;

const
  Bricks_major = 2;
  Bricks_minor = 0;
  Bricks_release = 0;
  Bricks_patch = 0;
  Bricks_fullversion = ((Bricks_major *  100 + Bricks_minor) * 100 + Bricks_release) * 100 + Bricks_patch;
  Bricks_version = '2.0.0.0';
  Bricks_internalversion = 'Escorpión';

  kError101 = 'Runtime error: Negative parameter(s)';
  kError102 = 'Runtime error: Parameter(s) out of range';
  kError103 = 'Runtime error: min > max';
  kError104 = 'Runtime error: max = 0';
  kError105 = 'Runtime error: Denominator is zero';
  kError210 = 'Runtime error: Nil pointer';

type

  TVector = array of extended;
  TMatrix = array of array of extended;
  TBlock = class;

  { TFR }
  { frequency response }

  TFR = record
    M, phi: extended; { magnitude and phase }
    F: complex;       { complex FR (F = M(omega) * exp(i * phi(omega)) }
  end;

  { TModel }
  { Base class for simulation models }

  TModel = class
    delta, time: extended;
    firstBlock: TBlock;
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
  end;

  { TBlock }
  { Abstract base class for IPS blocks }

  TBlock = class
  protected
    Foutput: extended;
    FFr: TFR;
  public
    name: string;
    model: TModel;
    destructor Destroy; override;
    procedure simulate; virtual; abstract;
    property output: extended read Foutput;
    property fr: TFR read FFR;
  end;

  { TControlledBlock }
  { Abstract base class for IPS blocks }

  TControlledBlock = class(TBlock)
  protected
    function SimAndGetOutput: extended; virtual; abstract;
    function GetFR: TFR; virtual; abstract;
  public
    input, G, amplitude, omega: extended;
  end;

  { TInvertableBlock }
  { Abstract base class for IPS blocks }

  TInvertableBlock = class(TBlock)
  protected
    function SimAndGetOutput: extended; virtual; abstract;
    function GetFR: TFR; virtual; abstract;
  public
    input1, input2, G: extended;
  end;

  { TTestSignal }

  TTestSignal = class(TBlock)
  protected
    function SimAndGetOutput: extended; virtual; abstract;
  public
    destructor Destroy; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TTHarmonic }
  { Harmonic test signal }

  TTHarmonic = class(TTestSignal)
  protected
    function SimAndGetOutput: extended; override;
  public
    G, omega, phi, delta: extended;
    updateTime: boolean;
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TP }
  { Proportional block }

  TP = class(TControlledBlock)
  protected
    function SimAndGetOutput: extended; override;
    function GetFR: TFR; override;
  public
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    property fr: TFR read GetFR;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TPT0 }
  { Dead-time element, improved from Neuber 1989 }

  TPT0 = class(TControlledBlock)
  protected
    function GetQueueLength: integer;
    procedure SetQueueLength(AValue: integer);
    function SimAndGetOutput: extended; override;
    function GetFR: TFR; override;
  public
    delta: extended;
    xt: array of extended;
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    property fr: TFR read GetFR;
    property nt: integer read GetQueueLength write SetQueueLength;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TPT1 }
  { First order delay element, changed from Neuber 1989 }

  TPT1 = class(TControlledBlock)
  protected
    function SimAndGetOutput: extended; override;
    function GetFR: TFR; override;
  public
    t1, x1, delta: extended;
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    property fr: TFR read GetFR;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TPT2 }
  { Second order delay element, changed from Neuber 1989 }

  TPT2 = class(TControlledBlock)
  protected
    function SimAndGetOutput: extended; override;
    function GetFR: TFR; override;
  public
    t2, dmp, x1, x2, delta: extended;
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    property fr: TFR read GetFR;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TInt }
  { Integrator block, changed from Neuber 1989 }

  TInt = class(TControlledBlock)
  protected
    function SimAndGetOutput: extended; override;
    function GetFR: TFR; override;
  public
    delta: extended;
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    property fr: TFR read GetFR;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TIT1 }
  { IT1 block, changed from Neuber 1989 }

  TIT1 = class(TControlledBlock)
  protected
    function SimAndGetOutput: extended; override;
    function GetFR: TFR; override;
  public
    t1, x1, delta: extended;
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    property fr: TFR read GetFR;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TDT1 }
  { DT1 block, changed from Neuber 1989 }

  TDT1 = class(TControlledBlock)
  protected
    function SimAndGetOutput: extended; override;
    function GetFR: TFR; override;
  public
    t1, x1, delta: extended;
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    property fr: TFR read GetFR;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TIT2 }
  { IT2 block, changed from Neuber 1989 }

  TIT2 = class(TControlledBlock)
  protected
    function SimAndGetOutput: extended; override;
    function GetFR: TFR; override;
  public
    t2, dmp, x1, x2, x3, delta: extended;
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    property fr: TFR read GetFR;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TPAdd }
  { Summation block }

  TPAdd = class(TInvertableBlock)
  protected
    function SimAndGetOutput: extended; override;
  public
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TPSub }
  { Substraction block, comparator }

  TPSub = class(TInvertableBlock)
  protected
    function SimAndGetOutput: extended; override;
  public
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TPMul }
  { Multiplicator }

  TPMul = class(TInvertableBlock)
  protected
    function SimAndGetOutput: extended; override;
  public
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TPDiv }
  { Divider}

  TPDiv = class(TInvertableBlock)
  protected
    function SimAndGetOutput: extended; override;
  public
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

implementation

{ TModel }

constructor TModel.Create;
begin
  inherited create;
  time := 0;
  delta := 1;
end;

destructor TModel.Destroy;
begin
  // Child object are to be deleted by the main program
  inherited destroy;
end;

procedure TModel.Reset;
begin
  time := 0;
end;

{ TTestSignal }

destructor TTestSignal.Destroy;
begin
  inherited Destroy;
end;

{ TTHarmonic }

function TTHarmonic.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

constructor TTHarmonic.Create;
begin
  inherited Create;
  delta := 1;
end;

destructor TTHarmonic.Destroy;
begin
  inherited Destroy;
end;

procedure TTHarmonic.simulate;
begin
  assert(assigned(model), kError210);
  fOutput := (G + G * sin(omega * model.time + phi)) / 2;
  if updateTime then
    model.time := model.time + delta;
end;

{ TIT2 }

function TIT2.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

function TIT2.GetFR: TFR;
var
  frT2, frInt: TFR;
begin
  assert(G >= 0, kError101);
  assert(t2 >= 0, kError101);
  assert(dmp >= 0, kError101);
  assert(omega >= 0, kError101);
  frT2.M := amplitude * G /
    sqrt(sqr(1 - sqr(omega * t2)) + sqr(2 * dmp * omega * t2)); ;
  if omega = 0 then
    frInt.M := infinity
  else
    frInt.M := amplitude * G / omega;
  if omega < 1 / t2 then
    frT2.phi := -arctan(2 * dmp * omega * t2 / (1 - sqr(omega * t2)))
  else
    frT2.phi := -pi - arctan(2 * dmp * omega * t2 / (1 - sqr(omega * t2)));
  frInt.phi := -90 * pi / 180;
  FFr.M := frT2.M * frInt.M;
  FFr.phi := frT2.phi + frInt.phi;
  if omega < 1 / t2 then
    FFr.phi := -90 * pi / 180 - arctan(2 * dmp * omega * t2 / (1 - sqr(omega * t2)))
  else
    FFr.phi := -90 * pi / 180 - pi - arctan(2 * dmp * omega * t2 / (1 - sqr(omega * t2)));
  FFr.F := FFr.M * cexp(i * FFr.phi); { M and phi encoded in polar coordinates }
  result := FFR;
end;

constructor TIT2.Create;
begin
  inherited Create;
  G := 1;
  x1 := 0;
  x2 := 0;
  x3 := 0;
  fOutput := 0;
end;

destructor TIT2.Destroy;
begin
  inherited Destroy;
end;

procedure TIT2.simulate;
var
  a, b, c, d, e, f, k, omg: extended;
  xn1, xn2, xn3, a1, a2, w, tau1, tau2, b1, b2, x1n, x2n: extended;
begin
  if dmp < 1 then
    begin
      omg := 1 / t2;
      a := exp(-dmp * omg * delta);
      b := sqrt(1 - dmp * dmp) * omg;
      c := arctan(dmp * omg / b);
      d := omg * omg;
      e := d * omg / b * a * cos(b * delta + c);
      f := d / b * a * sin(b * delta);
      k := f * 2 * dmp / omg;
      xn1 := x1 * e / d - x2 * f + f * g * input;
      xn2 := x1 * f / d + x2 * e / d + x2 * k - (e / d - 1 + k) * G * input;
      xn3 := x3 + g * delta * xn2;
      x1 := xn1;
      x2 := xn2;
      x3 := xn3;
      fOutput := x3;
    end
  else if dmp = 1 then
    begin
      a := exp(-delta / t2);
      a1 := a - 1;
      a2 := a1 * t2;
      c := g * input;
      e := g * t2;
      x1n := a * x1 - a1 * c;
      x2n := e * x1 + a * x2 + c * g;
      fOutput := e * x1 + a2 * x2 + x3 + delta * c * g;
      x1 := x1n;
      x2 := x2n;
      x3 := fOutput;
    end
  else
  begin
    w := sqrt(dmp * dmp - 1) * t2;
    tau1 := dmp * t2 + w;
    tau2 := dmp * t2 - w;
    a := exp(-delta / tau1);
    a1 := a - 1;
    a2 := tau1 * a1;
    b := exp(-delta / tau2);
    b1 := b - 1;
    b2 := tau2 * b1;
    c := g * input;
    d := tau1 - tau2;
    e := g * tau1;
    x1n := a * x1 - a1 * c;
    x2n := (b - a) * e * x1 / d + b * x2 + (a2 - b2) * c * g / d;
    fOutput := (1 + (tau2 * b - tau1 * a) / d) * e * x1 + b2 * x2 + x3 +
      (delta + (tau1 * a2 - tau2 * b2) / d) * c * g;
    x1 := x1n;
    x2 := x2n;
    x3 := fOutput;
  end;
end;

{ TDT1 }

function TDT1.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

function TDT1.GetFR: TFR;
begin
  assert(G >= 0, kError101);
  assert(omega >= 0, kError101);
  FFr.M := amplitude * G * omega / sqrt(1 + sqr(omega) * sqr(t1));
  FFr.phi := arctan(1 / (omega * t1));
  FFr.F := FFr.M * cexp(i * FFr.phi); { M and phi encoded in polar coordinates }
  result := FFR;
end;

constructor TDT1.Create;
begin
  inherited Create;
  G := 1;
  x1 := 0;
  fOutput := 0;
end;

destructor TDT1.Destroy;
begin
  inherited Destroy;
end;

procedure TDT1.simulate;
var
  a: extended;
begin
  assert((G >= 0) and (t1 >=0), kError101);
  a := exp(-delta / t1);
  fOutput := x1 + G * input;
  x1 := a * x1 + G * input * (a - 1);
end;

{ TIT1 }

function TIT1.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

function TIT1.GetFR: TFR;
begin
  assert(G >= 0, kError101);
  assert(omega >= 0, kError101);
  FFr.M := amplitude * G / (omega * sqrt(1 + sqr(omega) * sqr(t1)));
  FFr.phi := -90 * pi / 180 - arctan(omega * t1);
  FFr.F := FFr.M * cexp(i * FFr.phi); { M and phi encoded in polar coordinates }
  result := FFR;
end;

constructor TIT1.Create;
begin
  inherited Create;
  G := 1;
  x1 := 0;
  fOutput := 0;
end;

destructor TIT1.Destroy;
begin
  inherited Destroy;
end;

procedure TIT1.simulate;
var
  a, x1n: extended;
begin
  assert((G >= 0) and (t1 >=0), kError101);
  a := 1 - exp(-delta / t1);
  x1n := exp(-delta / t1) * x1 + G * a * input;
  fOutput := fOutput + delta * a * x1 + G * (delta - a * t1) * input;
  x1 := x1n;
end;

{ TInt }

function TInt.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

function TInt.GetFR: TFR;
begin
  assert(G >= 0, kError101);
  assert(omega >= 0, kError101);
  if omega = 0 then
    FFr.M := infinity
  else
    FFr.M := amplitude * G / omega;
  FFr.phi := -90 * pi / 180;
  FFr.F := FFr.M * cexp(i * FFr.phi); { M and phi encoded in polar coordinates }
  result := FFR;
end;

constructor TInt.Create;
begin
  inherited Create;
  G := 1;
  fOutput := 0;
end;

destructor TInt.Destroy;
begin
  inherited Destroy;
end;

procedure TInt.simulate;
begin
  assert(G >= 0, kError101);
  fOutput := fOutput + G * delta * input;
end;

{ TPSub }

procedure TPSub.simulate;
begin
  assert(G >= 0, kError101);
  fOutput := G * (input1 - input2);
end;

function TPSub.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

constructor TPSub.Create;
begin
  inherited Create;
  G := 1;
  fOutput := 0;
end;

destructor TPSub.Destroy;
begin
  inherited Destroy;
end;

{ TPAdd }

procedure TPAdd.simulate;
begin
  assert(G >= 0, kError101);
  fOutput := G * (input1 + input2);
end;

function TPAdd.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

constructor TPAdd.Create;
begin
  inherited Create;
  G := 1;
  fOutput := 0;
end;

destructor TPAdd.Destroy;
begin
  inherited Destroy;
end;

{ TPMul }

procedure TPMul.simulate;
begin
  assert(G >= 0, kError101);
  fOutput := G * input1 * input2;
end;

function TPMul.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

constructor TPMul.Create;
begin
  inherited Create;
  G := 1;
  fOutput := 0;
end;

destructor TPMul.Destroy;
begin
  inherited Destroy;
end;

{ TPDiv }

procedure TPDiv.simulate;
begin
  assert(G >= 0, kError101);
  assert(input2 <> 0, kError105);
  fOutput := G * input1 / input2;
end;

function TPDiv.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

constructor TPDiv.Create;
begin
  inherited Create;
  G := 1;
  fOutput := 0;
end;

destructor TPDiv.Destroy;
begin
  inherited Destroy;
end;

{ TPT0 }

function TPT0.GetQueueLength: integer;
begin
  result := Length(xt)
end;

procedure TPT0.SetQueueLength(AValue: integer);
begin
  SetLength(xt, AValue);
end;

procedure TPT0.simulate;
var
  i: integer;
begin
  assert(nt >= 0, kError101);
  if nt = 0 then
    fOutput := input
  else
  begin
    for i := nt - 1 downto 1 do
      xt[i] := xt[i - 1];
    fOutput := G * xt[nt - 1];
    xt[0] := input;
  end;
end;

function TPT0.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

function TPT0.GetFR: TFR;
begin
  assert(G >= 0, kError101);
  assert(omega >= 0, kError101);
  FFr.M := amplitude * G;
  FFr.phi := -omega * nt * delta;
  FFr.F := FFr.M * cexp(i * FFr.phi); { M and phi encoded in polar coordinates }
  result := FFR;
end;

constructor TPT0.Create;
begin
  inherited Create;
  G := 1;
  fOutput := 0;
end;

destructor TPT0.Destroy;
begin
  inherited Destroy;
end;

{ TPT1 }

function TPT1.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

function TPT1.GetFR: TFR;
begin
  assert(G >= 0, kError101);
  assert(t1 >= 0, kError101);
  assert(omega >= 0, kError101);
  FFr.M := amplitude * G / sqrt(1 + sqr(omega) * sqr(t1));
  FFr.phi := -arctan(omega * t1);
  FFr.F := FFr.M * cexp(i * FFr.phi); { M and phi encoded in polar coordinates }
  result := FFR;
end;

procedure TPT1.simulate;
var
  f: extended;
begin
  assert((G >= 0) and (t1 >=0), kError101);
  f := exp(-delta / t1);
  fOutput := f * x1 + G * (1 - f) * input;
  x1 := fOutput;
end;

constructor TPT1.Create;
begin
  inherited Create;
  G := 1;
  x1 := 0;
  fOutput := 0;
end;

destructor TPT1.Destroy;
begin
  inherited Destroy;
end;

{ TPT2 }

function TPT2.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

function TPT2.GetFR: TFR;
begin
  assert(G >= 0, kError101);
  assert(t2 >= 0, kError101);
  assert(omega >= 0, kError101);
  FFr.M := amplitude * G / sqrt(sqr(1 - sqr(omega * t2)) + sqr(2 * dmp * omega * t2));
  if omega < 1 / t2 then
    FFr.phi := -arctan(2 * dmp * omega * t2 / (1 - sqr(omega * t2)))
  else if omega > 1 / t2 then
    FFr.phi := -pi - arctan(2 * dmp * omega * t2 / (1 - sqr(omega * t2)))
  else
    FFr.phi := NaN;
  FFr.F := FFr.M * cexp(i * FFr.phi); { M and phi encoded in polar coordinates }
  result := FFR;
end;

constructor TPT2.Create;
begin
  inherited Create;
  G := 1;
  x1 := 0;
  x2 := 0;
  fOutput := 0;
end;

destructor TPT2.Destroy;
begin
  inherited Destroy;
end;

procedure TPT2.simulate;
var
  a, b, c, d, e, f, h, o, k, omg: extended;
  xn1, xn2, x1n, x2n, tau1, tau2: extended;
begin
  assert(G >= 0, kError101);
  assert(t2 >= 0, kError101);
  if dmp < 1 then
    begin
      omg := 1 / t2;
      a := exp(-dmp * delta * omg);
      b := sqrt(1 - dmp * dmp) * omg;
      c := arctan(dmp * omg / b);
      d := omg * omg;
      e := d * omg / b * a * cos(b * delta + c);
      f := d / b * a * sin(b * delta);
      k := f * 2 * dmp / omg;
      xn1 := x1 * e / d - x2 * f + f * G * input;
      xn2 := x1 * f / d + x2 * e / d + x2 * k - (e / d - 1 + k) * G * input;
      x1 := xn1;
      x2 := xn2;
      fOutput := x2;
    end
  else if dmp = 1 then
    begin
      a := exp(-delta / t2);
      x1n := a * x1 + G * (1 - a) * input;
      x2n := a * x2 + (1 - a) * x1n;
      x1 := x1n;
      x2 := x2n;
      fOutput := x2;
    end
  else // dmp > 1
    begin
      omg := 1 / t2;
      a := sqrt(dmp * dmp - 1);
      tau1 := (dmp + a) / omg;
      tau2 := (dmp - a) / omg;
      b := exp(-delta / tau1);
      c := exp(-delta / tau2);
      d := b - c;
      e := tau1 - tau2;
      f := x2 - G * input;
      k := 2 * dmp * omg;
      h := tau1 * c - tau2 * b;
      o := omg * omg;
      xn1 := (h * x1 - d * f) / e;
      xn2 := (d * (x1 + g * f) + h * f * o) / (o * e) + G * input;
      x1 := xn1;
      x2 := xn2;
      fOutput := x2;
    end;
end;

{ TP }

function TP.SimAndGetOutput: extended;
begin
simulate;
result := fOutput;
end;

function TP.GetFR: TFR;
begin
assert(G >= 0, kError101);
FFr.M := amplitude * G;
FFr.phi := 0;
FFr.F := FFr.M * cexp(i * FFr.phi); { M and phi encoded in polar coordinates }
result := FFR;
end;

procedure TP.simulate;
begin
assert(G >= 0, kError101);
fOutput := input * G;
end;

constructor TP.Create;
begin
inherited Create;
G := 1;
fOutput := 0;
end;

destructor TP.Destroy;
begin
inherited Destroy;
end;

  { TBlock }

destructor TBlock.Destroy;
begin
  inherited Destroy;
end;

end.

{References:  }

{1. Röhler, R., "Biologische Kybernetik", B. G. Teubner, Stuttgart 1973 }

{2. Neuber, H., "Simulation von Regelkreisen auf Personal Computern  }
{   in Pascal und Fortran 77", IWT, Vaterstetten 1989 }

{3. Lutz H. and Wendt, W., "Taschenbuch der Regelungstechnik" }
{   Verlag Harri Deutsch, Frankfurt am Main 2005 }

