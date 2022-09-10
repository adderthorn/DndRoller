program DndRoller;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,
   {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  Roller;

type

  { TDndRoller }

  TDndRoller = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TDndRoller }

  procedure TDndRoller.DoRun;
  var
    ErrorMsg, Input: string;
    Roller: TRoller;
    i: integer;
  begin
    Randomize;

    // quick check parameters
    ErrorMsg := CheckOptions('h', 'help');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    if paramCount = 0 then
    begin
      ShowException(Exception.Create('No specified input.'));
      Terminate;
      Exit;
    end;

    // parse required params
    for i:=1 to paramCount do
    begin
      if (paramStr(i).Length > 0) then
      begin
        Roller:=TRoller.Create(paramStr(i));
        WriteLn(Roller.Roll);
      end;
    end;
    // stop program loop
    Terminate;
  end;

  constructor TDndRoller.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TDndRoller.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TDndRoller.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
  end;

var
  Application: TDndRoller;
begin
  Application := TDndRoller.Create(nil);
  Application.Title := 'DnD Roller';
  Application.Run;
  Application.Free;
end.
