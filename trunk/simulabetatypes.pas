unit SimulaBetaTypes;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Types, global constants and global variables }

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
  Classes, SysUtils, bricks, lifeblocks;

type

Str13 = string[13];

TState = record
  P, Q, R, G, S, I, M, N, W, Z: extended;
end;
TPrediction = array[0..1] of TState;

TBlocks = record
  G1, G3: TASIA;
  MiMeBeta, MiMeR: TMiMe;
  GE: TP;
  NoCoDI: TNoCoDI;
end;

tParameterSpace = record
  alphaG, betaG, alphaI, betaI, GBeta, DBeta, GR, DR, GE: extended;
end;

tParameter = (alphaG, betaG, alphaI, betaI, GBeta, DBeta, GR, DR, GE);

tModel = record
  Name: AnsiString; // MIRIAM 1
  Reference: AnsiString; // MIRIAM 2
  Species: AnsiString;
  Creators: AnsiString; // MIRIAM 3
  Created: TDateTime; // MIRIAM 4
  LastModified: TDateTime; // MIRIAM 4
  Terms: AnsiString; // MIRIAM 5
  Code: AnsiString; // MIASE
  Comments: AnsiString; // MIASE
  StrucPars: tParameterSpace;
  Prediction: TPrediction;
  Iterations: integer;
end;

TTimeUnit = (minutes, hours);

const
  kNUL = char(0);           {Special ASCII characters}
  kENTER = char(3);
  kTAB = char(9);
  kLF = char(10);
  kRETURN = char(13);
  kESCAPE = char(27);
  kPERIOD = '.';
  kSPACE = ' ';
  kSLASH = '/';
  kCOLON = ':';
  kSEMICOLON = ';';
  kOMIT = '•';
  kCRLF = #13#10;

  IHeader = 'I(t) ';
  GHeader = 'G(t) ';

  ISO_8601_DATE_FORMAT = 'YYYY-MM-DD"T"hh:nn:ss'; {Date/time format in XML representation}
  STANDARD_NUM_FORMAT = '###,##0.0000';
  SHORT_NUM_FORMAT = '###,###.00';
  STANDARD_TIME_FORMAT = '"d"D hh:nn:ss';

  kMolarMassGlucose = 180.156; // molar mass in g/mol
  kInsulinActivity  = 1e9/6;   // corresponds to 6 nmol/IU, Knopp et al. 2019, PMID 30318910

var
  gNumberFormat, gDateTimeFormat: String;
  gActiveModel: TModel;
  gInsulinConversionFactor, gGlucoseConversionFactor: real;

implementation

initialization
  gNumberFormat := STANDARD_NUM_FORMAT;
  gDateTimeFormat := STANDARD_TIME_FORMAT;

end.

