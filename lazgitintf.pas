unit LazGitIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LazIDEIntf, ProjectIntf, Forms;

procedure ProcGitSettings(Sender: TObject);
procedure ProcGitClone(Sender: TObject);
procedure Register;

implementation

{$R lazgitpkg_images.res}

uses
  MenuIntf, IDECommands, Dialogs, Controls, GitResStr, GitSettingsForm, GitCloneForm, LazGitGeneral;

var
  CmdGitSettings: TIDECommand;
  CmdGitClone: TIDECommand;

procedure ProcGitSettings(Sender: TObject);
begin
  ShowSettings;
end;

procedure ProcGitClone(Sender: TObject);
begin
  ShowGitClone;
end;

procedure Register;
var
  Key: TIDEShortCut;
  Cat: TIDECommandCategory;
  mnuVCSSection: TIDEMenuSection;
  mnuGitSection: TIDEMenuSection;
  LazGit: TLazGitGeneral;
begin
  Key := IDEShortCut(VK_UNKNOWN, [], VK_UNKNOWN, []);

  {$ifndef USECustomCategory}
  Cat := IDECommandList.CreateCategory(nil, 'Git', rsGitTools, IDECmdScopeSrcEditOnly);
  {$else}
  Cat := nil;
  {$endif}

  CmdGitSettings := RegisterIDECommand(Cat, 'GitSettings', rsGitSettings,
    Key, nil, @ProcGitSettings);
  CmdGitClone := RegisterIDECommand(Cat, 'GitClone', rsGitClone, Key,
    nil, @ProcGitClone);

  mnuVCSSection := RegisterIDESubMenu(itmSecondaryTools, 'VCS', 'VCS', nil, nil, '');
  mnuGitSection := RegisterIDESubMenu(mnuVCSSection, 'Git', 'Git', nil, nil, 'menu_git');

  RegisterIDEMenuCommand(mnuGitSection, 'GitClone', rsClone, nil, nil, CmdGitClone, '');
  RegisterIDEMenuCommand(mnuGitSection, 'GitSettings', rsSettings,
    nil, nil, CmdGitSettings, 'menu_environment_options');

  LazGit := TLazGitGeneral.Create;
  LazarusIDE.AddHandlerOnProjectOpened(@LazGit.OnProjectOpened, True);
  LazarusIDE.AddHandlerOnProjectBuilding(@LazGit.OnProjectBuilding, True);
  LazarusIDE.AddHandlerOnProjectBuildingFinished(@LazGit.OnProjectBuilding, True);
end;

end.
