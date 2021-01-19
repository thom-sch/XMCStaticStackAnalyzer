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
  Windows,
  Vcl.Forms,
  frm_StackAnalyzer in 'src\frm_StackAnalyzer.pas' {frmStackAnalyzer},
  Analyzer in 'src\Analyzer.pas';

{$R *.res}

FUNCTION IsConsole(): BOOLEAN;
CONST ATTACH_PARENT_PROCESS = $FFFFFFFF;
VAR AttachConsole: FUNCTION(dwProcessId: LongWord): LongBool; StdCall;
BEGIN
  AttachConsole := GetProcAddress(GetModuleHandle('kernel32.dll'), 'AttachConsole');
  Result := Assigned(AttachConsole) AND AttachConsole(ATTACH_PARENT_PROCESS);
END;

begin
  IF (ParamCount > 0) AND IsConsole() THEN BEGIN
    WriteLn(#13#10'not implemented'#13#10'press "Return"!');
  END ELSE BEGIN
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.CreateForm(TfrmStackAnalyzer, frmStackAnalyzer);
  Application.Run;
  END;
end.
//! @endcond

