unit SimulaBetaResources;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ This unit provides URLs and global strings for other SimulaBeta units }

{ Version 3.0.2 (Tournado) }

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

interface

uses
  Classes, SysUtils, SimulaBetaTypes;

const
  BASE_URL = 'http://simulabeta.sf.net';
  SIMTHYR_GLOBAL_ID = 'net.sf.simulabeta';
  HELP_URL = 'http://simulabeta.sf.net';

  SCICRUNCH_URL = 'http://scicrunch.org/browse/resources/SCR_021900';

  MIASE_URL = 'http://co.mbine.org/standards/miase';
  MIASE_SIMTHYR_STANDARD_CODE = 'Model of insulin-glucose homeostasis for use with SimulaBeta, as available from ' +  BASE_URL;
  MIRIAM_URL = 'http://www.ebi.ac.uk/miriam/main/';
  MIBBI_URL = 'http://biosharing.org/collection/MIBBI';

  kSTANDARD_MODEL_NAME = 'Model 3';
  kSTANDARD_MODEL_REFERENCE = 'Dietrich JW, Dasgupta R, Anoop S, Jebasingh F, Kurian ME, Inbakumari M, Boehm BO, Thomas N. SPINA Carb: a simple mathematical model supporting fast in-vivo estimation of insulin sensitivity and beta cell function. Sci Rep. 2022 Oct 21;12(1):17659. doi: 10.1038/s41598-022-22531-3. PMID: 36271244; PMCID: PMC9587026.';
  kSTANDARD_MODEL_SPECIES = 'Homo sapiens (NCBI Taxonomy ID 9606)';
  kSTANDARD_MODEL_CREATORS = 'Dietrich, Dasgupta, Anoop, Jebasingh, Kurian, Inbakumari, Boehma and Thomas';
  kSTANDARD_MODEL_CREATED_Y = 2021;
  kSTANDARD_MODEL_CREATED_M = 11;
  kSTANDARD_MODEL_CREATED_D = 1;
  kSTANDARD_MODEL_CREATED_H = 12;
  kSTANDARD_MODEL_CREATED_N = 27;
  kSTANDARD_MODEL_CREATED_S = 0;
  kSTANDARD_MODEL_MODIFIED_Y = 2022;
  kSTANDARD_MODEL_MODIFIED_M = 5;
  kSTANDARD_MODEL_MODIFIED_D = 7;
  kSTANDARD_MODEL_MODIFIED_H = 10;
  kSTANDARD_MODEL_MODIFIED_N = 45;
  kSTANDARD_MODEL_MODIFIED_S = 0;
  kSTANDARD_MODEL_TERMS = 'Creative Commons Attributions License 4.0 (CC BY 4.0)';

  IMPLEMENTATION_MESSAGE = 'Function not implemented in this version of SimulaBeta';
  FILE_VERSION_MESSAGE = 'This scenario file has a file version that is not supported by SimulaBeta.';
  FILE_FORMAT_ERROR_MESSAGE = 'This is not a valid XML file that can be used by SimulaBeta.';
  DEBUG_VERSION_MESSAGE = 'This is a pre-release version of SimulaBeta, which is intended for testing purposes only. Usage is on your own risk.';
  FORMAT_MESSAGE = 'Please check your input.';
  URL_STATUS_MESSAGE = 'Server status code: ';
  PREFERENCES_READ_ERROR_MESSAGE = 'Preferences could not be read. Please check access rights of your user or home folder';
  PREFERENCES_SAVE_ERROR_MESSAGE = 'The preferences could not be saved permanently, however, they are valid for this session';
  INSUFFICIENT_MEMORY_MESSAGE = 'This machine has not enough free memory to perform this function';
  SAVE_ERROR_MESSAGE = 'Error saving file.';
  CLI_MISSING_FILE_NAME = 'No file name specified.';

  URL_TITLE = 'Read URL';
  URL_QUERY = 'Please enter the URL of a Scenario:';

  EXAMPLE_STRING = 'Example: ';
  NEGATIVE_VALUES_HINT = 'Negative values represent virtual solutions delivered by equilibrium polynomials.';
  DEVIATION_STRING = 'Small deviations between predictions and simulated values may result from temporal dynamics including circadian and ultradian rhythms, transition effects and rounding.';

  CHANGE_IN_STRING = 'Change in ';
  DEPENDEND_VAR_STRING = 'dependent variable';
  DECREASE_STRING = 'decrease';
  INCREASE_STRING = 'increase';

  ISOKLINE_1_STRING = 'Concentration 1';
  ISOKLINE_2_STRING = 'Concentration 2';
  A_AXIS_STRING = ' a';
  B_AXIS_STRING = ' b';

  kError101 = 'Runtime error: Negative parameter(s)';
  kError102 = 'Runtime error: Parameter(s) out of range';
  kError103 = 'Runtime error: min > max';
  kError104 = 'Runtime error: max = 0';
  kError105 = 'Runtime error: max = NaN';


  {$IFDEF UNIX}
  kHeapTraceFile = '~/simulabeta_heaptrace.trc';
  {$ELSE}
  kHeapTraceFile = 'simulabeta_heaptrace.trc';
  {$ENDIF}

implementation

end.

