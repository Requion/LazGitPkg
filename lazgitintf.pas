unit LazGitIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LazIDEIntf;

procedure ProcGitSettings(Sender: TObject);
procedure Register;

implementation

uses
  MenuIntf, IDECommands, Dialogs, Controls, GitResStr, GitSettingsForm;

var
  CmdGitSettings: TIDECommand;

procedure ProcGitSettings(Sender: TObject);
begin
  ShowSettings;
end;

procedure Register;
var
  Key: TIDEShortCut;
  Cat: TIDECommandCategory;
  mnuVCSSection : TIDEMenuSection;
  mnuGitSection : TIDEMenuSection;
begin
  Key := IDEShortCut(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  {$ifndef USECustomCategory}
    Cat := IDECommandList.CreateCategory(nil, 'Git', rsGitTools, IDECmdScopeSrcEditOnly);
  {$else}
    Cat:=nil;
  {$endif}

  CmdGitSettings := RegisterIDECommand(Cat, 'GitSettings', rsGitSettings, Key, nil, @ProcGitSettings);

  mnuVCSSection := RegisterIDESubMenu(itmSecondaryTools, 'VCS', 'VCS', nil, nil, 'menu_vcs');
  mnuGitSection := RegisterIDESubMenu(mnuVCSSection, 'Git', 'Git', nil, nil, 'menu_git');

  RegisterIDEMenuCommand(mnuGitSection, 'GitSettings', rsSettings, nil, nil, CmdGitSettings);

  LazarusIDE.GetMainBar.Caption:=  LazarusIDE.GetMainBar.Caption + ' gittest';
end;

end.

