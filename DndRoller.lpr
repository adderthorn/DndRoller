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
    i, c: integer;
  begin
    Randomize;
    c:=1;

    // quick check parameters
    ErrorMsg := CheckOptions('h c', 'help count');
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

    if HasOption('c', 'count') then
    begin
      if not integer.TryParse(GetOptionValue('c', 'count'), c) then
      begin
        ShowException(Exception.Create('Count option must be an integer value'));
        Terminate;
        Exit;
      end;
    end;

    if paramCount = 0 then
    begin
      //ShowException(Exception.Create('No specified input.'));
      WriteHelp;
      Terminate(-1);
      Exit;
    end;

    Input := Self.Params[ParamCount];
    Roller := TRoller.Create(Input);
    if not Roller.IsRegExParsed then
    begin
      ShowException(Exception.Create('Roll format is invalid, see help for details with --help'));
      Terminate(-1);
      Exit;
    end;

    // do rolls
    for i := 1 to c do
      WriteLn(Roller.Roll);

    // free objects
    FreeAndNil(Roller);

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
//    writeln('Usage: ', ExeName, ' -h');
    WriteLn;
    WriteLn('Usage: ');
    WriteLn;
    WriteLn('  dndroller [--OPTIONS] [ROLL]');
    WriteLn;
    WriteLn('Options:');
    WriteLn;
    WriteLn('  --help, -h', #9#9#9, 'Help.');
    WriteLn('  --count, -c <INTEGER>', #9#9, 'Count of times to run the roll');
    WriteLn;
    WriteLn('Roll Format:');
    WriteLn;
    WriteLn('  Roll format is standard DnD rolling id+/-x where:');
    WriteLn('    i = number of rolls of the dice');
    WriteLn('    d = constant to indicate dice');
    WriteLn('    n = number of sides of the die to roll');
    WriteLn('    x = modifier to add/subtract after the roll is complete');
    WriteLn;
    WriteLn('Examples:');
    WriteLn;
    WriteLn('  dndroller 5d4+4', #9#9, 'Rolls 5 4-sided dice and adds 4 to the result of the roll');
    WriteLn('  dndroller -c 3 1d20', #9#9, 'Rolls a 20-sided die and output three different rolls');
    WriteLn('  dndroller 1d100-16', #9#9, 'Rolls a 100-sided die and subtracts 16 from the result of the roll');
    WriteLn;
  end;

var
  Application: TDndRoller;
begin
  Application := TDndRoller.Create(nil);
  Application.Title := 'DnD Roller';
  Application.Run;
  Application.Free;
end.
