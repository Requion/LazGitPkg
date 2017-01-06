unit GitCloneForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ExtCtrls, process;

type

  { TGitCloneFrm }

  TGitCloneFrm = class(TForm)
    btnClone: TButton;
    btnCancel: TButton;
    Edit1: TEdit;
    edtCloneDirCust: TDirectoryEdit;
    edtRepoUrl: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblCloneInto: TLabel;
    lblRepoUrl: TLabel;
    memCloneOutput: TMemo;
    rbCloneIntoCust: TRadioButton;
    rbCloneIntoDef: TRadioButton;
    tmCloneProgress: TTimer;
    procedure btnCancelClick(Sender: TObject);
    procedure btnCloneClick(Sender: TObject);
    procedure rbChangeCloneInto(Sender: TObject);
    procedure tmCloneProgressTimer(Sender: TObject);
  private
    FGitExec: string;
    FProcess: TProcess;
    function GetCloneIntoDir: string;
    function GetRepositoryNameFromUrl(const AUrl: string): string;
  public
    property GitExec: string read FGitExec write FGitExec;
  end;

procedure ShowGitClone;

var
  GitCloneFrm: TGitCloneFrm;

implementation

uses
  GitResStr, GitSettingsManager, CustomMsgDialog;

procedure ShowGitClone;
var
  CloneFrm: TGitCloneFrm;
begin
  CloneFrm := TGitCloneFrm.Create(nil);
  try
    CloneFrm.GitExec := LoadValue(cPathGitExec);
    if SameText(CloneFrm.GitExec, '') then
      Error('Git executable not found. Did you set one in settings?' +
        #10#13 + '(Tools > VCS > Git > Settings > Git executable)')
    else
      CloneFrm.rbCloneIntoDef.Caption :=
        CloneFrm.rbCloneIntoDef.Caption + ' (' + LoadValue(cPathDefPrjDir) + ')';
    CloneFrm.ShowModal;
  finally
    CloneFrm.Free;
  end;
end;

{$R *.lfm}

{ TGitCloneFrm }

procedure TGitCloneFrm.rbChangeCloneInto(Sender: TObject);
begin
  edtCloneDirCust.Enabled := rbCloneIntoCust.Checked;
end;

procedure TGitCloneFrm.tmCloneProgressTimer(Sender: TObject);
var
  b: string;
  r: integer;
begin
  if Assigned(FProcess.Output) and (FProcess.Output.NumBytesAvailable > 0) then
  begin
    memCloneOutput.Lines.BeginUpdate;
    try
      SetLength(b, 1024 * 64); //64k chunks
      r := FProcess.Output.Read(b[1], length(b));
      if r > 0 then
      begin
        SetLength(b, r);
        memCloneOutput.Append(b);
      end;
    finally
      memCloneOutput.Lines.EndUpdate;
    end;
  end;
end;

function TGitCloneFrm.GetCloneIntoDir: string;
begin
  if rbCloneIntoDef.Checked then
    Result := LoadValue(cPathDefPrjDir)
  else
    Result := edtCloneDirCust.Directory;
end;

function TGitCloneFrm.GetRepositoryNameFromUrl(const AUrl: string): string;
var
  I, PathDelimIndex: integer;
begin
  if Pos('.git', AUrl) > 0 then
  begin
    for I := Low(AUrl) to High(AUrl) do
    begin
      if AUrl[I] = '/' then
        PathDelimIndex := I;
    end;
    Result := Copy(AUrl, PathDelimIndex + 1, Length(AUrl) - 4);
    Delete(Result, Length(Result) - 3, Length(Result));
  end
  else
    ShowMessage('TODO');
end;

procedure TGitCloneFrm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TGitCloneFrm.btnCloneClick(Sender: TObject);
var
  CloneIntoDir, ProjectName: string;
begin
  CloneIntoDir := GetCloneIntoDir;
  ProjectName := GetRepositoryNameFromUrl(edtRepoUrl.Text);

  if SameText(CloneIntoDir, '') then
  begin
    Error('No Clone dir');
    Exit;
  end;

  if not DirectoryExists(CloneIntoDir) then
  begin
    if MessageDlg('Directory does not exist', Format(rsDirNotExists, [CloneIntoDir]),
      mtConfirmation, mbYesNo, '') = mrYes then
      CreateDir(CloneIntoDir)
    else
      Exit;
  end;

  FProcess := TProcess.Create(nil);
  try
    FProcess.Executable := FGitExec;
    FProcess.Parameters.Add('clone');
    FProcess.Parameters.Add('--progress');
    FProcess.Parameters.Add(edtRepoUrl.Text);
    FProcess.Parameters.Add(CloneIntoDir + PathDelim + ProjectName);
    FProcess.Options := [poUsePipes, poStderrToOutPut];
    FProcess.ShowWindow := swoHIDE;

    FProcess.Execute;
  finally
    FProcess.Free;
  end;
end;

end.
