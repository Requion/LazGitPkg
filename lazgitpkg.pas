{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazGitPkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  LazGitIntf, GitResStr, GitSettingsForm, CustomMsgDialog, GitSettingsManager, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LazGitIntf', @LazGitIntf.Register);
end;

initialization
  RegisterPackage('LazGitPkg', @Register);
end.
