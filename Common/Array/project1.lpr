program Project1;

{$include PasUtilsMode.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils;

procedure Run;
begin

end;

begin
  Run;
  {$if defined(windows)}
  ReadLn;
  {$endif}
end.

