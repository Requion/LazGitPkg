unit GitSettingsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Dialogs, StdCtrls,
  EditBtn, process, Controls, LazIDEIntf;

type

  { TGitSettingsFrm }

  TGitSettingsFrm = class(TForm)
    btnSearchGitExec: TButton;
    btnApply: TButton;
    btnTest: TButton;
    btnClose: TButton;
    chkShowBranchName: TCheckBox;
    edtDefPrjDir: TDirectoryEdit;
    edtGitExec: TFileNameEdit;
    lblDefPrjDir: TLabel;
    lblGitExec: TLabel;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnSearchGitExecClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure edtDefPrjDirEditingDone(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public

  end;

procedure ShowSettings;

var
  GitSettingsFrm: TGitSettingsFrm;

implementation

uses
  GitResStr, CustomMsgDialog, GitSettingsManager;

procedure ShowSettings;
var
  SettingsFrm: TGitSettingsFrm;
begin
  SettingsFrm := TGitSettingsFrm.Create(nil);
  try
    SettingsFrm.ShowModal;
  finally
    SettingsFrm.Free;
  end;
end;

{$R *.lfm}

{ TGitSettingsFrm }

procedure TGitSettingsFrm.btnSearchGitExecClick(Sender: TObject);
var
  FileName, FoundFile: string;
begin
  FileName := 'git';
  {$IFDEF Windows}
  FileName := FileName + '.exe';
  {$ENDIF}

  FoundFile := FindDefaultExecutablePath(FileName);

  if SameText(FoundFile, '') then
    ShowMessage(rsGitExecNotFound)
  else
    edtGitExec.FileName := FoundFile;
end;

procedure TGitSettingsFrm.btnTestClick(Sender: TObject);
var
  TmpProcess: TProcess;
  TmpStringlist: TStringList;
begin
  if not SameText(edtGitExec.FileName, '') and FileExists(edtGitExec.FileName) and
    SameText(ExtractFileName(edtGitExec.FileName), 'git'
{$IFDEF Windows}
    + '.exe'
{$ENDIF}
    ) then
  begin
    TmpProcess := TProcess.Create(nil);
    TmpStringlist := TStringList.Create;
    try
      TmpProcess.Executable := edtGitExec.FileName;
      TmpProcess.Parameters.Add('--version');
      TmpProcess.Options := TmpProcess.Options + [poWaitOnExit, poUsePipes, poNoConsole];
      TmpProcess.Execute;
      TmpStringlist.LoadFromStream(TmpProcess.Output);
      Information(TmpStringlist.Text);
    finally
      TmpStringlist.Free;
      TmpProcess.Free;
    end;
  end
  else
    Error('FileError //TODO');
end;

procedure TGitSettingsFrm.edtDefPrjDirEditingDone(Sender: TObject);
var
  DirName: string;
begin
  DirName := (Sender as TDirectoryEdit).Directory;
  if not DirectoryExists(DirName) then
  begin
    if MessageDlg('Directory does not exist', Format(rsDirNotExists, [DirName]),
      mtConfirmation, mbYesNo, '') = mrYes then
      CreateDir(DirName)
    else
    begin
      (Sender as TDirectoryEdit).Clear;
      (Sender as TDirectoryEdit).SetFocus;
    end;
  end;
end;

procedure TGitSettingsFrm.FormShow(Sender: TObject);
begin
  edtGitExec.FileName := LoadValue(cPathGitExec);
  edtDefPrjDir.Directory := LoadValue(cPathDefPrjDir);
  chkShowBranchName.Checked := LoadValue(cPathShowBranch);
end;

procedure TGitSettingsFrm.btnApplyClick(Sender: TObject);
var
  Saved: boolean;
begin
  { TODO -oRequion : think about a better way. But not now because it is 0:40 AM and you are tired }
  Saved := SaveValue(cPathGitExec, edtGitExec.FileName);
  Saved := SaveValue(cPathDefPrjDir, edtDefPrjDir.Directory);
  Saved := SaveValue(cPathShowBranch, chkShowBranchName.Checked);

  if Saved then
    Close
  else
    Error('SaveError //TODO');
end;

procedure TGitSettingsFrm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
