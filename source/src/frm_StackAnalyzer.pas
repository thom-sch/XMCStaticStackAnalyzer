//!@cond
unit frm_StackAnalyzer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  System.ImageList, Vcl.ImgList, Vcl.ExtCtrls;

type
  TfrmStackAnalyzer = class(TForm)
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    ListView1: TListView;
    ImageList1: TImageList;
    lblFileName: TLabel;
    Panel1: TPanel;
    btnLoad: TButton;
    cbSorting: TComboBox;
    edtMaxDeepth: TLabeledEdit;
    udMaxDeepth: TUpDown;
    btnSave: TButton;
    SaveDialog1: TSaveDialog;
    rgMode: TRadioGroup;
    gbOptions: TGroupBox;
    cbInclSysFunc: TCheckBox;
    cbInclNoStackUsage: TCheckBox;
    Label1: TLabel;
    procedure btnLoadClick(Sender: TObject);
    procedure cbSortingChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rgModeClick(Sender: TObject);
    procedure udMaxDeepthClick(Sender: TObject; Button: TUDBtnType);
    procedure btnSaveClick(Sender: TObject);
    procedure cbOptionsClick(Sender: TObject);
  private
    { Private-Deklarationen }
    PROCEDURE ShowResult(Clr: BOOLEAN);
    PROCEDURE ModeChanged();
  public
    { Public-Deklarationen }
  end;

var
  frmStackAnalyzer: TfrmStackAnalyzer;

implementation

uses Analyzer;
{$R *.dfm}

function sGetFileVersion(): String;
var
  FileName: String;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  Result := '';
  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the string const into a local variable to create a writeable copy.
  FileName := ParamStr(0);
  //UniqueString(FileName);
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
        begin
          Result := IntToStr(HiWord(FI.dwFileVersionMS))
            + '.' + IntToStr(LoWord(FI.dwFileVersionMS))
            + '.' + IntToStr(HiWord(FI.dwFileVersionLS))
            + '.' + IntToStr(LoWord(FI.dwFileVersionLS))
          ;
        end;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

procedure TfrmStackAnalyzer.FormCreate(Sender: TObject);
begin
  Caption := Caption + ' - Version: ' + sGetFileVersion();
  Memo1.Align     := alClient;
  ListView1.Align := alClient;
  Memo1.Clear;
  ModeChanged();
end;

PROCEDURE TfrmStackAnalyzer.ModeChanged();
BEGIN
  btnSave.Visible := rgMode.ItemIndex = 0;
  edtMaxDeepth.Visible := rgMode.ItemIndex = 1;
  udMaxDeepth.Visible  := rgMode.ItemIndex = 1;
END;

procedure TfrmStackAnalyzer.btnLoadClick(Sender: TObject);
begin
  WITH OpenDialog1 DO
    IF Execute() THEN BEGIN
      lblFileName.Caption := FileName;
//      GetSU(ExtractFilePath(FileName),Memo1.Lines);
      Analyse(FileName);
      rgModeClick(Sender);
      //ShowResult(TRUE);
    END;
end;

procedure TfrmStackAnalyzer.btnSaveClick(Sender: TObject);
begin
  WITH SaveDialog1 DO BEGIN
    FileName := ChangeFileExt(lblFileName.Caption,'.md');
    IF Execute() THEN BEGIN
//      FillStringList(Memo1.Lines);
      Memo1.Lines.SaveToFile(FileName);
    END;
  END;
end;

procedure TfrmStackAnalyzer.rgModeClick(Sender: TObject);
begin
  ModeChanged();
  Memo1.Visible     := rgMode.ItemIndex = 0;
  ListView1.Visible := rgMode.ItemIndex = 1;
  CASE rgMode.ItemIndex OF
    0: IF Memo1.Lines.Count = 0     THEN ShowResult(FALSE);
    1: IF ListView1.Items.Count = 0 THEN ShowResult(FALSE);
  END;
end;

procedure TfrmStackAnalyzer.cbOptionsClick(Sender: TObject);
begin
  ShowResult(TRUE);
end;

procedure TfrmStackAnalyzer.cbSortingChange(Sender: TObject);
begin
  ShowResult(TRUE);
end;

procedure TfrmStackAnalyzer.udMaxDeepthClick(Sender: TObject; Button: TUDBtnType);
begin
  ShowResult(TRUE);
end;

PROCEDURE TfrmStackAnalyzer.ShowResult(Clr: BOOLEAN);
VAR ViewOptions: TViewOptions;
BEGIN
  IF Clr THEN BEGIN
    ListView1.Clear;
    Memo1.Clear;
  END;
  CASE cbSorting.ItemIndex OF
    0: SortForName();
    1: SortForOwn();
    2: SortForDeepest();
  END;
  ViewOptions := [];
  IF cbInclSysFunc.Checked      THEN Include(ViewOptions,InclSysFunc);
  IF cbInclNoStackUsage.Checked THEN Include(ViewOptions,InclNoStackUsage);
  IF ListView1.Visible // AND (ListView1.Items.Count = 0)
  THEN BEGIN
    ListView1.Align := alNone;
    FillListView(ListView1,udMaxDeepth.Position,ViewOptions);
    ListView1.Align := alClient;
  END;
  IF Memo1.Visible // AND (Memo1.Lines.Count = 0)
  THEN FillStringList(Memo1.Lines,ViewOptions);
END;

end.
//!@endcond

