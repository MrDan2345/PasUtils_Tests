program project1;

{$include PasUtilsMode.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils
;
const TestJsonEscape = (
`{"name" : "test escape\r \"characters\"\\"}`
);
const TestJsonArray = (
`[1, 2, 3, 4]`
);
const TestJsonString = (
`"Hello World"`
);
const TestJsonInt = (
`42`
);
const TestJsonBool = (
`true`
);
const TestJsonNull = (
`null`
);
const TestJsonString2 = (
`"Text with \"quotes\", \\ backslashes, and \/ forward slashes"`
);
const TestJsonFloat1 = (
`1.23e10`
);
const TestJsonFloat2 = (
`76E-13`
);
const TestJsonFloat3 = (
`-4.5e+2`
);
 var Json: TUJson;
begin
  WriteLn('Json Escape');
  Json := TUJson.Load(TestJsonEscape);
  WriteLn(Json.FormatJson());
  WriteLn('Json Array');
  Json := TUJson.Load(TestJsonArray);
  WriteLn(Json.FormatJson());
  WriteLn('Json String');
  Json := TUJson.Load(TestJsonString);
  WriteLn(Json.FormatJson());
  WriteLn('Json Int');
  Json := TUJson.Load(TestJsonInt);
  WriteLn(Json.FormatJson());
  WriteLn('Json Bool');
  Json := TUJson.Load(TestJsonBool);
  WriteLn(Json.FormatJson());
  WriteLn('Json Null');
  Json := TUJson.Load(TestJsonNull);
  WriteLn(Json.FormatJson());
  WriteLn('Json String2');
  Json := TUJson.Load(TestJsonString2);
  WriteLn(Json.FormatJson());
  WriteLn('Json Float1');
  Json := TUJson.Load(TestJsonFloat1);
  WriteLn(Json.ValueAsFloat);
  WriteLn('Json Float2');
  Json := TUJson.Load(TestJsonFloat2);
  WriteLn(Json.ValueAsFloat);
  WriteLn('Json Float3');
  Json := TUJson.Load(TestJsonFloat3);
  WriteLn(Json.ValueAsFloat);
{$if defined(windows)}
  ReadLn;
{$endif}
  Json.Free;
end.

