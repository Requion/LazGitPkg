unit LazGitGeneral;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ProjectIntf, GitResStr, GitSettingsManager,
  process, Dialogs;

type

  { TLazGitGeneral }

  TLazGitGeneral = class
    function GetBranchName(AProjectDir: string): string;
    function OnProjectOpened(Sender: TObject): TModalResult;
    function OnProjectBuilding(Sender: TObject): TModalResult;
    procedure SetIdeTitle(AProjDir: string);
  end;

implementation

{ TLazGitGeneral }

function TLazGitGeneral.GetBranchName(AProjectDir: string): string;
var
  ShowBranchName: boolean;
  GitExec, GitDir: string;
  TmpProcess: TProcess;
  TmpStringlist: TStringList;
begin
  Result := '';
  if SameText(LoadValue(cPathShowBranch), '') then
    ShowBranchName := False
  else
    ShowBranchName := LoadValue(cPathShowBranch);
  if ShowBranchName then
  begin
    GitExec := LoadValue(cPathGitExec);
    if not SameText(GitExec, '') then
    begin
      GitDir := AProjectDir + '.git';
      if DirectoryExists(GitDir) then
      begin
        TmpProcess := TProcess.Create(nil);
        TmpStringlist := TStringList.Create;
        try
          TmpProcess.Executable := GitExec;
          TmpProcess.Parameters.Add('--git-dir=' + GitDir);
          TmpProcess.Parameters.Add('name-rev');
          TmpProcess.Parameters.Add('--name-only');
          TmpProcess.Parameters.Add('HEAD');

          TmpProcess.Options := [poUsePipes, poNoConsole];

          TmpProcess.Execute;
          TmpStringlist.LoadFromStream(TmpProcess.Output);
          Result := TmpStringlist.Text;
        finally
          TmpProcess.Free;
          TmpStringlist.Free;
        end;
      end;
    end;
  end;
end;

function TLazGitGeneral.OnProjectOpened(Sender: TObject): TModalResult;
begin
  SetIdeTitle(Application.);
end;

function TLazGitGeneral.OnProjectBuilding(Sender: TObject): TModalResult;
begin
    SetIdeTitle(AProject.Directory);
end;

procedure TLazGitGeneral.SetIdeTitle(AProjDir: string);
begin
  if not SameText(GetBranchName(AProjDir), '') then
    Application.MainForm.Caption :=
      Application.MainForm.Caption + ' [' + GetBranchName(AProjDir) + ']';
end;

end.
