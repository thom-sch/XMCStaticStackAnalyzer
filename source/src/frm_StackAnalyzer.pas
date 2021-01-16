//!@cond
unit frm_StackAnalyzer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  System.ImageList, Vcl.ImgList, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
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
    GroupBox1: TGroupBox;
    cbInclSysFunc: TCheckBox;
    cbInclNoStackUsage: TCheckBox;
    Label1: TLabel;
    procedure btnLoadClick(Sender: TObject);
    procedure cbSortingChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rgModeClick(Sender: TObject);
    procedure udMaxDeepthClick(Sender: TObject; Button: TUDBtnType);
    procedure btnSaveClick(Sender: TObject);
  private
    { Private-Deklarationen }
    PROCEDURE ShowResult();
    PROCEDURE ModeChanged();
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

uses Analyzer;
{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Align     := alClient;
  ListView1.Align := alClient;
  ModeChanged();
end;

PROCEDURE TForm1.ModeChanged();
BEGIN
  btnSave.Visible := rgMode.ItemIndex = 0;
  edtMaxDeepth.Visible := rgMode.ItemIndex = 1;
  udMaxDeepth.Visible  := rgMode.ItemIndex = 1;
END;

procedure TForm1.btnLoadClick(Sender: TObject);
begin
  WITH OpenDialog1 DO
    IF Execute() THEN BEGIN
      lblFileName.Caption := FileName;
//      GetSU(ExtractFilePath(FileName),Memo1.Lines);
      Analyse(FileName);
      ShowResult();
    END;
end;

procedure TForm1.btnSaveClick(Sender: TObject);
begin
  WITH SaveDialog1 DO BEGIN
    FileName := ChangeFileExt(lblFileName.Caption,'.md');
    IF Execute() THEN BEGIN
//      FillStringList(Memo1.Lines);
      Memo1.Lines.SaveToFile(FileName);
    END;
  END;
end;

procedure TForm1.rgModeClick(Sender: TObject);
begin
  ModeChanged();
  ShowResult();
end;

procedure TForm1.cbSortingChange(Sender: TObject);
begin
  ShowResult();
end;

procedure TForm1.udMaxDeepthClick(Sender: TObject; Button: TUDBtnType);
begin
  ShowResult();
end;

PROCEDURE TForm1.ShowResult();
VAR ViewOptions: TViewOptions;
BEGIN
  ListView1.Clear;
  Memo1.Clear;
  CASE cbSorting.ItemIndex OF
    0: SortForName();
    1: SortForOwn();
    2: SortForDeepest();
  END;
  ViewOptions := [];
  IF cbInclSysFunc.Checked      THEN Include(ViewOptions,InclSysFunc);
  IF cbInclNoStackUsage.Checked THEN Include(ViewOptions,InclNoStackUsage);
  Memo1.Visible     := rgMode.ItemIndex = 0;
  ListView1.Visible := rgMode.ItemIndex = 1;
  IF ListView1.Visible
  THEN BEGIN
    ListView1.Align := alNone;
    FillListView(ListView1,udMaxDeepth.Position,ViewOptions);
    ListView1.Align := alClient;
  END;
  IF Memo1.Visible     THEN FillStringList(Memo1.Lines,ViewOptions);
END;

end.
//!@endcond

