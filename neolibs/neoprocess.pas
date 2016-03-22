unit neoprocess;

interface

uses Windows, neofileutils;

function StartProcess(Cmd : String) : integer; // uses command line, windows32\cmd.exe, can also be used with batch files
function RunProcess(Cmd : string) : integer;
function Exec(Cmd : string) : integer;  // run any command, no window

implementation

// ****************************************************************************
// A generic procedure to start a process (uses the windows command line
// window)
// ****************************************************************************
function StartProcess(Cmd : String) : integer;
var
    ProcessAttr   : TSecurityAttributes;
    ThreadAttr    : TSecurityAttributes;
    StartupInfo   : TStartupInfo;
    ProcessHandle : TProcessInformation;
begin
    Result := 0;
    with ProcessAttr do
    begin
       nLength               := sizeof(ProcessAttr);
       lpSecurityDescriptor  := nil;
       bInheritHandle        := True;
    end;
    with ThreadAttr do
    begin
       nLength               := sizeof(ThreadAttr);
       lpSecurityDescriptor  := nil;
       bInheritHandle        := True;
    end;

    with StartupInfo do
    begin
       cb              := sizeof(StartupInfo);
       lpReserved      := nil;
       lpDesktop       := nil;
       lpTitle         := nil;
       dwX             := 0;
       dwY             := 0;
       dwXSize         := 600;
       dwYSize         := 400;
       dwXCountChars   := 0;
       dwYCountChars   := 0;
       dwFillAttribute := 0;
       dwFlags         := STARTF_USESIZE;
       wShowWindow     := 0;
       cbReserved2     := 0;
       lpReserved2     := nil;
       hStdInput       := 0;
       hStdOutput      := 0;
       hStdError       := 0;
    end;

    if not CreateProcess(PChar(GetWindowsDir+'\system32\cmd.exe'),PChar('/c call '+Cmd),@ProcessAttr,@ThreadAttr,
              True,CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
              nil,nil,StartupInfo,ProcessHandle) then
    begin
       Result := -1;
       Exit;
    end;
end;

// ****************************************************************************
// A generic procedure to Run a process
// will wait forever for process to complete!
// ****************************************************************************
function RunProcess(Cmd : string) : integer;
var
    ProcessAttr   : TSecurityAttributes;
    ThreadAttr    : TSecurityAttributes;
    StartupInfo   : TStartupInfo;
    ProcessHandle : TProcessInformation;
    ExitCode      : cardinal;
begin
    with ProcessAttr do
    begin
       nLength               := sizeof(ProcessAttr);
       lpSecurityDescriptor  := nil;
       bInheritHandle        := True;
    end;
    with ThreadAttr do
    begin
       nLength               := sizeof(ThreadAttr);
       lpSecurityDescriptor  := nil;
       bInheritHandle        := True;
    end;

    with StartupInfo do
    begin
       cb              := sizeof(StartupInfo);
       lpReserved      := nil;
       lpDesktop       := nil;
       lpTitle         := nil;
       dwX             := 0;
       dwY             := 0;
       dwXSize         := 600;
       dwYSize         := 400;
       dwXCountChars   := 0;
       dwYCountChars   := 0;
       dwFillAttribute := 0;
       dwFlags         := STARTF_USESIZE;
       wShowWindow     := 0;
       cbReserved2     := 0;
       lpReserved2     := nil;
       hStdInput       := 0;
       hStdOutput      := 0;
       hStdError       := 0;
    end;

    if not CreateProcess(PChar(GetWindowsDir+'\system32\cmd.exe'),PChar('/c call '+Cmd),@ProcessAttr,@ThreadAttr,
              True,CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
              nil,nil,StartupInfo,ProcessHandle) then
    begin
       Result := -1;
       Exit;
    end;

    ExitCode := STILL_ACTIVE;
    while ExitCode = STILL_ACTIVE do
    begin
       GetExitCodeProcess(Processhandle.hProcess,ExitCode);
    end;

    Result := integer(ExitCode);
end;

// ****************************************************************************
// Run a command
// ****************************************************************************
function Exec(Cmd : string) : integer;  // run any command, no window
var
    ProcessAttr   : TSecurityAttributes;
    ThreadAttr    : TSecurityAttributes;
    StartupInfo   : TStartupInfo;
    ProcessHandle : TProcessInformation;
begin
    Result := 0;
    with ProcessAttr do
    begin
       nLength               := sizeof(ProcessAttr);
       lpSecurityDescriptor  := nil;
       bInheritHandle        := True;
    end;
    with ThreadAttr do
    begin
       nLength               := sizeof(ThreadAttr);
       lpSecurityDescriptor  := nil;
       bInheritHandle        := True;
    end;

    with StartupInfo do
    begin
       cb              := sizeof(StartupInfo);
       lpReserved      := nil;
       lpDesktop       := nil;
       lpTitle         := nil;
       dwX             := 0;
       dwY             := 0;
       dwXSize         := 600;
       dwYSize         := 400;
       dwXCountChars   := 0;
       dwYCountChars   := 0;
       dwFillAttribute := 0;
       dwFlags         := STARTF_USESIZE;
       wShowWindow     := 0;
       cbReserved2     := 0;
       lpReserved2     := nil;
       hStdInput       := 0;
       hStdOutput      := 0;
       hStdError       := 0;
    end;

    if not CreateProcess(PChar(GetWindowsDir+'\system32\cmd.exe'),PChar('/c call '+Cmd),@ProcessAttr,@ThreadAttr,
              True,NORMAL_PRIORITY_CLASS,
              nil,nil,StartupInfo,ProcessHandle) then
    begin
       Result := -1;
       Exit;
    end;
end;

end.
