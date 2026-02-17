program Paths;

{$include PasUtilsMode.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils;

procedure Run;
begin
  WriteLn('AppPath: ', UAppPath);
  WriteLn('ConfigPath: ', UConfigPath);
  WriteLn('DataPath: ', UDataPath);
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

