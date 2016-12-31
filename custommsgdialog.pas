unit CustomMsgDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, LCLType;

resourcestring
  rsErrorCaption = 'Error';
  rsInfoCaption = 'Information';

procedure Error(AMsg: String; ACaption: String = '');
procedure Information(AMsg: String; ACaption: String = '');

implementation

procedure Error(AMsg: String; ACaption: String);
begin
  if SameText(ACaption, '') then
    ACaption := rsErrorCaption;
  MessageBoxFunction(PChar(AMsg), PChar(ACaption), MB_ICONERROR);
end;

procedure Information(AMsg: String; ACaption: String);
begin
   if SameText(ACaption, '') then
    ACaption := rsInfoCaption;
   MessageBoxFunction(PChar(AMsg), PChar(ACaption), MB_ICONINFORMATION);
end;

end.

