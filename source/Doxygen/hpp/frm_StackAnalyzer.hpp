// CodeGear C++Builder
// Copyright (c) 1995, 2018 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'frm_StackAnalyzer.pas' rev: 33.00 (Windows)

#ifndef Frm_stackanalyzerHPP
#define Frm_stackanalyzerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.SysUtils.hpp>
#include <System.Variants.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <System.ImageList.hpp>
#include <Vcl.ImgList.hpp>
#include <Vcl.ExtCtrls.hpp>

//-- user supplied -----------------------------------------------------------

//-- forward type declarations -----------------------------------------------
class DELPHICLASS TForm1;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TForm1 : public TForm
{
	typedef TForm inherited;
	
__published:
	TMemo* Memo1;
	TOpenDialog* OpenDialog1;
	TListView* ListView1;
	TImageList* ImageList1;
	TLabel* lblFileName;
	TPanel* Panel1;
	TButton* btnLoad;
	TComboBox* cbSorting;
	TLabeledEdit* edtMaxDeepth;
	TUpDown* udMaxDeepth;
	TButton* btnSave;
	TSaveDialog* SaveDialog1;
	TRadioGroup* rgMode;
	TGroupBox* GroupBox1;
	TCheckBox* cbInclSysFunc;
	TCheckBox* cbInclNoStackUsage;
	TLabel* Label1;
	void __fastcall btnLoadClick(TObject* Sender);
	void __fastcall cbSortingChange(TObject* Sender);
	void __fastcall FormCreate(TObject* Sender);
	void __fastcall rgModeClick(TObject* Sender);
	void __fastcall udMaxDeepthClick(TObject* Sender, TUDBtnType Button);
	void __fastcall btnSaveClick(TObject* Sender);
	
private:
	void __fastcall ShowResult();
	void __fastcall ModeChanged();
public:
	/* TCustomForm.Create */ inline __fastcall virtual TForm1(TComponent* AOwner) : TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TForm1(TComponent* AOwner, int Dummy) : TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TForm1() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TForm1(HWND ParentWindow) : TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TForm1* Form1;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Frm_stackanalyzerHPP
