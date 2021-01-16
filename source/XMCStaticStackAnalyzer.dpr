// =============================================================================
//! @file
//! @brief      Infineon XMC4 static stack analyzer
//!
//! @author     Dipl.Inf. Thomas Schneider (c) 2020
//! @copyright  GPLv3 (https://www.gnu.org/licenses/gpl-3.0)
//!
//! @details
//!
//! XMCStaticStackAnalyzer
//! ----------------------
//!
//! This program parses a compiler list file file and calculates the estimated stack usage of it.
//! It is build for list files compiled by ARM gcc in general and XMC4 from Infineon
//! in special with Cortex M4 architecture.
//! May be also functional for Infineon XMC1 / Cortex M0 (not extensively tested).
//!
//! @DModuleRef{Analyzer}
//!
//! @todo Command line interface
// =============================================================================
//! @cond
program XMCStaticStackAnalyzer;

uses
  Vcl.Forms,
  frm_StackAnalyzer in 'src\frm_StackAnalyzer.pas' {Form1},
  Analyzer in 'src\Analyzer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
//! @endcond

