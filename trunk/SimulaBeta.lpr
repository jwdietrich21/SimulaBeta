program SimulaBeta;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Main project file }

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

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Controls, Forms, tachartlazaruspkg, GUI, SimulationEngine, IPS,
  Plot, prediction, LogGrid, SimulationControl, SimulaBetaServices;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TToolbarWindow, ToolbarWindow);
  Application.CreateForm(TIPSForm, IPSForm);
  Application.CreateForm(TPlotForm, PlotForm);
  Application.CreateForm(TPredictionForm, PredictionForm);
  Application.CreateForm(TLogWindow, LogWindow);
  Application.CreateForm(TControlWindow, ControlWindow);
  Application.BringToFront;
  ToolbarWindow.Show;
  ControlWindow.BringToFront;
  ControlWindow.FormStyle := fsStayOnTop;
  Application.Run;
end.

