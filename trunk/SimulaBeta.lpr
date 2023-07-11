program SimulaBeta;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Main project file }

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

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Controls, Forms, tachartlazaruspkg, Classes, Sysutils, GUI, SimulationEngine,
  IPS, Plot, prediction, LogGrid, SimulationControl, SimulaBetaBaseServices,
  SimulaBetaGUIServices, EnvironmentInfo, ScenarioHandler, SimulaBetaResources,
  UnitConverter, SimulaBetaAboutwindow, Sequencer, SequencerEngine,
  PreferencesGUI, PreferencesServices, Stats, StatsEngine, disptab,
  SensitivityAnalysis;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TToolbarWindow, ToolbarWindow);
  Application.CreateForm(TIPSForm, IPSForm);
  Application.CreateForm(TPlotForm, PlotForm);
  Application.CreateForm(TPredictionForm, PredictionForm);
  Application.CreateForm(TLogWindow, LogWindow);
  Application.CreateForm(TAboutWindow, Aboutwindow);
  Application.CreateForm(TControlWindow, ControlWindow);
  Application.CreateForm(TSequencerWindow, SequencerWindow);
  Application.CreateForm(TPreferencesDialog, PreferencesDialog);
  Application.CreateForm(TStatsForm, StatsForm);
  Application.CreateForm(TDispTabWindow, DispTabWindow);
  Application.BringToFront;
  ToolbarWindow.Show;
  ControlWindow.Visible := false;
  ControlWindow.ShowModal;
  InitMetabolicConversionFactors;
  Application.Run;
  if assigned(gValues) then
    FreeAndNil(gValues);
end.

