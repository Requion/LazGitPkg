unit GitSettingsManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazConfigStorage, BaseIDEIntf;

function SaveValue(const APath: string; AValue: Variant): boolean;
function LoadValue(const APath: string): Variant;

implementation

const
  cConfigFileName = 'lazgitconf.xml';
  cVersion = 1;

function SaveValue(const APath: string; AValue: Variant): boolean;
var
  TmpConfigStorage: TConfigStorage;
begin
   try
    TmpConfigStorage := GetIDEConfigStorage(cConfigFileName, False);
    try
      TmpConfigStorage.SetDeleteValue('General/Config/Version', cVersion, 0);
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

