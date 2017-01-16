unit GitSettingsManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazConfigStorage, BaseIDEIntf, LazIDEIntf, Dialogs;

const
  cConfigFileName = 'lazgitconf.xml';
  cVersion = 1;
  cPathDefPrjDir = 'General/Git/DefPrjDir';
  cPathGitExec = 'General/Git/Executable';
  cPathVersion = 'General/Config/Version';
  cPathShowBranch = 'General/Git/ShowBranchInTitle';

function SaveValue(const APath: string; AValue: Variant): boolean;
function LoadValue(const APath: string): Variant;

implementation

function SaveValue(const APath: string; AValue: Variant): boolean;
var
  TmpConfigStorage: TConfigStorage;
begin
   try
    TmpConfigStorage := GetIDEConfigStorage(cConfigFileName, FileExists(LazarusIDE.GetPrimaryConfigPath + PathDelim + cConfigFileName)); //
    try
      TmpConfigStorage.SetDeleteValue(cPathVersion, cVersion, 0);
      TmpConfigStorage.SetDeleteValue(APath, AValue, '');
      Result := True;
    finally
      TmpConfigStorage.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
    end;
  end;
end;

function LoadValue(const APath: string): Variant;
var
  TmpConfigStorage: TConfigStorage;
begin
  try
    TmpConfigStorage := GetIDEConfigStorage(cConfigFileName, True);
    try
      Result := TmpConfigStorage.GetValue(APath, '');
    finally
      TmpConfigStorage.Free;
    end;
  except
    on E: Exception do
    begin
      Result := E.Message;
    end;
  end;
end;

end.

