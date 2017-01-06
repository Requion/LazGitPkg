unit LazGitGeneral;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ProjectIntf, GitResStr, GitSettingsManager, process;

type

  { TLazGitGeneral }

  TLazGitGeneral = class
    function OnProjectOpened(Sender: TObject; AProject: TLazProject): TModalResult;
  end;

implementation

{ TLazGitGeneral }

function TLazGitGeneral.OnProjectOpened(Sender: TObject;
  AProject: TLazProject): TModalResult;
var
  ShowBranchName: boolean;
  GitExec, GitDir: string;
  TmpProcess: TProcess;
  TmpStringlist: TStringList;
begin
  ShowBranchName := LoadValue(cPathShowBranch);
  if ShowBranchName then
  begin
    GitExec := LoadValue(cPathGitExec);
    if not SameText(GitExec, '') then
    begin
      GitDir := AProject.Directory + PathDelim + '.git';
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

          TmpProcess.Options:=[poUsePipes];

          TmpProcess.Execute;
          TmpStringlist.LoadFromStream(TmpProcess.Output);
          Application.MainForm.Caption :=
            Application.MainForm.Caption + TmpStringlist.Text;
        finally
          TmpProcess.Free;
          TmpStringlist.Free;
        end;
      end;
    end;
  end;
end;

end.
