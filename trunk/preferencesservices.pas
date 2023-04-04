unit PreferencesServices;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Basic (GUI-independent) services for Preferences }

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

const
  kInsulinUoM = 'pmol/l';
  kGlucoseUoM = 'mmol/l';

implementation

initialization
  gInsulinConversionFactor := 1;
  gGlucoseConversionFactor := 1;
  gGlucLoadConversionFactor := kMolarMassGlucose;
  gInsulinLoadConversionFactor := 1 / kInsulinActivity;
  gUnits.I := kInsulinUoM;
  gUnits.G := kGlucoseUoM;
end.

