unit neoCircBuf;

interface
uses
   neotypedef, classes, syncobjs;

type
   TCircBuf = class(TObject)
   private
   public
      Elements : array of TVarElement;
      Size    : integer;
      StartP  : integer;
      EndP    : integer;
      Num     : integer;

      function  PushBack(x : TVarElement) : Boolean; virtual;
      procedure SetSize(x : integer);  virtual;
      function  Add(x : TVarElement) : Boolean; virtual;
      function  AddString(x : string) : Boolean; virtual;
      function  Get(var x : TVarElement) : Boolean;  virtual;
      procedure Clear;  virtual;
      constructor Create;  virtual;

   end;

   TThreadCircBuf = class(TCircBuf)
   private
      Lock : TCriticalSection;
   public
      function  PushBack(x : TVarElement) : Boolean;
      procedure SetSize(x : integer); override;
      function  Add(x : TVarElement) : Boolean; override;
      function  Get(var x : TVarElement) : Boolean;  override;
      procedure Clear;  override;
      constructor Create;  override;
      procedure LockBuf;
      procedure UnlockBuf;
   end;

   TStack = class(TObject) // a simple lifo stack -> also see TLifoStack - not tested yet
   private
   public
      Elements : array of TVarElement;
      Size    : integer;
      StackP  : integer;

      procedure SetSize(x : integer);  virtual;
      function  Push(x : TVarElement) : Boolean; virtual;
      function  Pop(var x : TVarElement) : Boolean;  virtual;
      procedure Clear;  virtual;
      constructor Create;  virtual;
   end;

implementation
// **************************************************************************
// Create method
// **************************************************************************
constructor TCircBuf.Create;
begin
   inherited Create;

   SetSize(16);
end;

// **************************************************************************
// Set the size of the buffer
// **************************************************************************
procedure TCircBuf.SetSize(x : integer);
begin
   SetLength(Elements,x);
   Size := x;
   Clear;
end;

// **************************************************************************
// Push back into buffer
// **************************************************************************
function TCircBuf.PushBack(x : TVarElement) : Boolean;
begin
   PushBack := False;
   if Num = Size then Exit;

   StartP := StartP - 1;
   if StartP < 0 then StartP := Size-1;
   Elements[StartP] := x;
   inc(Num);
   PushBack := True;
end;

// **************************************************************************
// Set the size of the buffer
// **************************************************************************
function  TCircBuf.Add(x : TVarElement) : Boolean;
begin
   Add := False;
   if Num = Size then Exit;

   Elements[EndP] := x;
   inc(Endp);
   if EndP >= Size then Endp := 0;
   inc(Num);
   Add := True;
end;

// **************************************************************************
// Set the size of the buffer
// **************************************************************************
function  TCircBuf.AddString(x : string) : Boolean;
var
    i : integer;
    t : TVarElement;
begin
    for i:= 1 to length(x) do
    begin
        t.vChar := x[i];
        Add(t);
    end;
end;

// **************************************************************************
// Set the size of the buffer
// **************************************************************************
function  TCircBuf.Get(var x : TVarElement) : Boolean;
begin
   get := False;
   if Num = 0 then Exit;

   x := Elements[StartP];
   inc(StartP);
   if StartP >= Size then StartP := 0;
   dec(Num);
   get := true;
end;

// **************************************************************************
// Set the size of the buffer
// **************************************************************************
procedure TCircBuf.Clear;
begin
   StartP := 0;
   Endp   := 0;
   Num    := 0;
end;

// **************************************************************************
// Lock the buffer
// **************************************************************************
procedure TThreadCircBuf.LockBuf;
begin
   if Lock = nil then Lock := TCriticalSection.create;
   Lock.Acquire;
end;

// **************************************************************************
// UnLock the buffer
// **************************************************************************
procedure TThreadCircBuf.UnlockBuf;
begin
   Lock.Release;
end;

// **************************************************************************
// Push back an element into the beginning of the buffer
// **************************************************************************
function  TThreadCircBuf.PushBack(x : TVarElement) : Boolean;
begin
    LockBuf;
    PushBack := inherited PushBack(x);
    UnlockBuf
end;

// **************************************************************************
// Add to the buffer
// **************************************************************************
function  TThreadCircBuf.Add(x : TVarElement) : Boolean;
begin
   LockBuf;
   Add := inherited Add(x);
   UnlockBuf
end;

// **************************************************************************
// get the buffer
// **************************************************************************
function  TThreadCircBuf.Get(var x : TVarElement) : Boolean;
begin
   LockBuf;
   get := inherited Get(x);
   UnlockBuf
end;

// **************************************************************************
// Clear the buffer
// **************************************************************************
procedure TThreadCircBuf.Clear;
begin
   LockBuf;
   inherited Clear;
   UnlockBuf
end;

// **************************************************************************
// Create method for TThreadCircBuf
// **************************************************************************
constructor TThreadCircBuf.Create;
begin
    inherited Create;
end;

// **************************************************************************
// Set the buffer size
// **************************************************************************
procedure TThreadCircBuf.SetSize(x : integer);
begin
   LockBuf;
   inherited SetSize(x);
   UnlockBuf;
end;


// **************************************************************************
// Set the stack size
// **************************************************************************
procedure TStack.SetSize(x : integer);
begin
   SetLength(Elements,x);
   Size := x;
   Clear;
end;

// **************************************************************************
// Push item onto stack
// **************************************************************************
function  TStack.Push(x : TVarElement) : Boolean;
begin
   if StackP < (Size-1) then
   begin
      Elements[StackP] := x;
      inc(StackP);
      Push := true;
   end
   else
      Push := False;
end;


// **************************************************************************
// Pop item from stack
// **************************************************************************
function  TStack.Pop(var x : TVarElement) : Boolean;
begin
   if StackP > 0 then
   begin
      dec(StackP);
      x := Elements[StackP];
      Pop := true;
   end
   else
      Pop := False;
end;

// **************************************************************************
// Clear the stack
// **************************************************************************
procedure TStack.Clear;
begin
   StackP := 0;
end;

// **************************************************************************
// Create the stack;
// **************************************************************************
constructor Tstack.Create;
begin
   inherited Create;

   SetSize(100);
end;

end.
