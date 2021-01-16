// CodeGear C++Builder
// Copyright (c) 1995, 2018 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Analyzer.pas' rev: 33.00 (Windows)

#ifndef AnalyzerHPP
#define AnalyzerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <Vcl.ComCtrls.hpp>

//-- user supplied -----------------------------------------------------------
//! @file Analyzer.pas
//! @file
//! @DModuleRef{Analyzer}
#include "doxygen.h"

//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TViewOption : unsigned char { InclNoStackUsage, InclSysFunc };

typedef Set<TViewOption, TViewOption::InclNoStackUsage, TViewOption::InclSysFunc> TViewOptions;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall sortForName();
extern DELPHI_PACKAGE void __fastcall sortForDeepest();
extern DELPHI_PACKAGE void __fastcall sortForOwn();
extern DELPHI_PACKAGE void __fastcall FillStringList(TStrings* sl, TViewOptions ViewOptions = TViewOptions() );
extern DELPHI_PACKAGE void __fastcall FillListView(TListView* lv, int MaxDeepth = 0x5, TViewOptions ViewOptions = TViewOptions() );
extern DELPHI_PACKAGE void __fastcall Analyse(UnicodeString filename);
extern DELPHI_PACKAGE void __fastcall GetSU(UnicodeString DirName, TStrings* Fct);
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AnalyzerHPP
