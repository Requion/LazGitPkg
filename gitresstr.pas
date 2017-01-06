unit GitResStr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  rsSettings = 'Settings';
  rsClone = 'Clone';
  rsGitSettings = 'Git settings';
  rsGitClone = 'Git clone';
  rsGitTools = 'Git Tools';
  rsGitExecNotFound = 'Git executable not found.';
  rsDirNotExists = 'Directory %s does not exist. Do you want to create it?';
  rsSavingFailed = 'Saving %s failed: %s';
  rsLoadingFailed = 'Loading %s failed: %s';

implementation

end.

