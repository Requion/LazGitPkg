unit GitSettingsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn;

type

  { TGitSettingsFrm }

  TGitSettingsFrm = class(TForm)
    btnSearchGitExec: TButton;
    edtGitExec: TFileNameEdit;
    lblGitExec: TLabel;
    procedure btnSearchGitExecClick(Sender: TObject);
  private

  public

  end;

procedure ShowSettings;

var
  GitSettingsFrm: TGitSettingsFrm;

implementation

uses
  GitResStr;

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

end.

