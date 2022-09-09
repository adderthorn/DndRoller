program DndRoller;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,
   {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  Roller { you can add units after this };

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
  begin
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

    { add your program here }
    Randomize;
    ReadLn(Input);
    while Input <> 'q' do
    begin
      Roller := TRoller.Create(1, 20, 0);
      WriteLn(Roller.Roll);
      ReadLn(Input);
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
