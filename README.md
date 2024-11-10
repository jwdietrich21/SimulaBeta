# SimulaBeta
A simulation program for insulin-glucose homeostasis.

## Notes

SimulaBeta is a continuous simulator for insulin-glucose feedback control. It is based on a nonlinear MiMe-NoCoDI model. Pre-compiled Software is available for macOS and Windows. SimulaBeta was developed with Lazarus / Free Pascal. Source code is provided for all supported platforms and may also be used in order to compile SimulaBeta for additional platforms.


## Installation

In order to install the portable version of SimulaBeta for Windows simply drag the application file from the archive to the desired location on your file system. You may even start SimulaBeta directly from a web server (depending from the settings of your browser). This may have advantages e.g. for CBT labs and other computer resource centers.

In order to install SimulaBeta on macOS, simply double click the DMG file and drag the application file from its window to the desired location on your file system. SimulaBeta for Mac doesn't come with and doesn't require an installer. It may also be used as a portable application that is started from USB sticks, CDs, server volumes or other media.

On Linux, please download the source code and compile it for your processor and operating system with Lazarus / Free Pascal.


## Source Code

Sources are available for SimulaBeta 2.0 and all newer versions. For compiling you will need Lazarus / Free Pascal.

In order to compile SimulaBeta from source please download (or check-out) the source code archive and decompress the folder at a location of your choice. This folder is your working environment.

With the working environment in place you are ready to compile SimulaBeta as follows:

1. Start Lazarus.
2. Close any existing project by selecting the menu entry "Project" > "Close Project".
3. Subsequently the Project Wizard will pop up.
   - Here please click the "Open Project" button.
   - Navigate to the folder of your working environemt.
   - Select file "SimulaBeta.lpi" and "Open".
   - Subsequently the project will be loaded and be ready for being compiled.
4. For a quick test run of the project select the menu entry "Run" > "Run", press F9
   (Windows or Linux) or Command + R (macOS).
   In the "Messages" window the last line should read "Project SimulaBeta successfully built" (or an appropriate translation).
5. SimulaBeta should run.

On certain platforms the steps 1. to 3. may be replaced by simply double-clicking the file "SimulaBeta.lpi".

For documentation purposes, a historic version of SimGluc, an early prototype of SimulaBeta for Mac OS Classic, is provided as well. It is available as source code only. You need THINK Pascal 4.0 or newer for compiling and building.


## Version history

A complete and regularly updated list of all SimulaBeta revisions is available from https://sourceforge.net/p/simulabeta/wiki/Version%20History/.


## Puclications

1. Dietrich JW, Dasgupta R, Anoop S, Jebasingh F, Kurian ME, Inbakumari M, Boehm BO, Thomas N. SPINA Carb: a simple mathematical model supporting fast in-vivo estimation of insulin sensitivity and beta cell function. Sci Rep. 2022 Oct 21;12(1):17659. doi: 10.1038/s41598-022-22531-3. PMID: [36271244](https://pubmed.ncbi.nlm.nih.gov/36271244/); PMCID: PMC9587026.

2. Dietrich JW, Abood A, Dasgupta R, Anoop S, Jebasingh FK, Spurgeon R, Thomas N, Boehm BO. A novel simple disposition index (SPINA-DI) from fasting insulin and glucose concentration as a robust measure of carbohydrate homeostasis. J Diabetes. 2024 Sep;16(9):e13525. doi: 10.1111/1753-0407.13525. Epub 2024 Jan 2. PMID: [38169110](https://pubmed.ncbi.nlm.nih.gov/38169110/); PMCID: PMC11418405.


J. W. Dietrich
