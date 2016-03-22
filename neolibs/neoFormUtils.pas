unit neoFormUtils;

{$mode delphi}

interface

uses
   Controls, ComCtrls, INIFiles, SysUtils, neoTextUtils, Dialogs,
   neotypedef, stdctrls, classes, graphics, forms, windows, messages, menus,
   Contnrs;

const
  ptBoolean     = $01;
  ptString      = $02;
  ptInteger     = $03;
  ptByte        = $04;
  ptHexByte     = $05;
  ptStringList  = $06;
type
  TProperty = record
     Name      : string;
     PropType  : integer;
     PropKey   : string;
     DefVal    : string;
     Data      : Pointer;
  end;

  TButtonList = class(TList)
  public
     function GetButton(idx : integer) : TButton;
  end;

function  TestWithinBounds(X,Y : integer; Control : TControl) : Boolean;
function  FindtabSheetByText(x : string; PageControl : TPageControl) : TTabSheet;
procedure LoadProperties(Section : string; x : TListItems; y : Array of TProperty; FN : string);
Procedure EditProperty(ListItem : TListItem; y : TProperty);
procedure SaveProperties(Section : string; x : TListItems; y : Array of TProperty; FN : string);
function  FontToStringDesc(x : TFont) : string;
function  SpreadButtons(Form : TForm; ButtonList : TButtonList; SpaceBetween : integer; Warn : Boolean = true) : integer;
function  FindInListView(x : TListView; y : string) : TListItem;
function  FindInTreeView(x : TTreeView; y : string) : TTreeNode;
function  GetChildCalled(TreeNode : TTreeNode; Text : string) : TTreeNode;
procedure CopyNodeTo(DstTreeNode,SrcTreeNode : TTreeNode; TreeView : TTreeView);
procedure SizeForTaskBar(MyForm: TForm);
procedure DisableEntireMenu( x: TMenu);
procedure EnableEntireMenu( x: TMenu);
procedure AssignHint(Control: TControl; Str : string);
procedure SendDownKeyToActiveControl(Form: TForm; Sender: TObject; Key : word; ShiftState : TShiftState);
procedure DisplayMemBuffer(AddrOffset : integer; MemSize : integer;
                           BytesPerLine : integer; Buffer : pByteArray; Memo : TMemo);
procedure HandleCopy(Form : TForm);
procedure HandlePaste(Form : TForm);
procedure HandleCut(Form : TForm);
function  FindComponentByName(Name : String; CompList : TComponentList) : TComponent; overload;
function  FindComponentByName(Name : String; Comp : TWinControl) : TComponent;  overload;
procedure SaveMenuToFile(mnu : TMenu; fn : string); overload;
procedure LoadMenuFromFile(x : TMenu; fn : string);

implementation
uses
   neoGenTextEntry, neoGenComboForm;

// ***********************************************************
// Find a button in the button list
// ***********************************************************
function TButtonList.GetButton(idx : integer) : TButton;
begin
   if Idx < count then
      GetButton := TButton(Items[idx])
   else
      getButton := nil;
end;

// ***********************************************************
// Find a tabsheet in a pagecontrol by the caption text
// ***********************************************************
function FindtabSheetByText(x : string; PageControl : TPageControl) : TTabSheet;
var
   i : integer;
begin
   FindtabSheetByText := nil;

   for i:= 0 to PageControl.PageCount-1 do
   begin
      if PageControl.Pages[i].Caption = x then
      begin
         FindtabSheetByText := PageControl.Pages[i];
         Exit;
      end;
   end;
end;

// ***********************************************************
// End of drag
// ***********************************************************
function TestWithinBounds(X,Y : integer; Control : TControl) : Boolean;
var
   x1,x2,y1,y2 : integer;
begin
   TestWithinBounds := False;

   x1 := Control.Left;
   y1 := Control.Top;

   x2 := x1 + Control.Width;
   y2 := y1 + Control.Height;

   if (x>x1) and (x<x2) and (y>y1) and (y<y2) then
   begin
      TestWithinBounds := True;
   end;
end;

// ***********************************************************
// Load properties into
// ***********************************************************
procedure LoadProperties(Section : string; x : TListItems; y : Array of TProperty; FN : string);
var
   INIFile : TINIFile;
   i       : integer;
   NewItem : TListItem;
   tempstr : string;

   tempStrList : TStringList;
begin
   INIFile := TINIFile.Create(FN);

   i := 0;

   while(y[i].PropType >= 0) do
   begin
      NewItem := x.Add;
      NewItem.Caption := y[i].name;
      tempstr := INIFile.ReadString(Section,y[i].PropKey,y[i].DefVal);
      if y[i].PropType = ptStringList then
      begin
         TempStrList := TStringList(y[i].Data^);
         try
            Tempstr := TempStrList.strings[FindSubStrInList(TempStr+':',TempStrList)];
         except
            TempStr := TempStrList.Strings[0];
         end;
      end;
      NewItem.Subitems.Add(TempStr);
      inc(i);
   end;

   INIFile.Free;
end;

// ***********************************************************
// Save properties
// ***********************************************************
procedure SaveProperties(Section : string; x : TListItems; y : Array of TProperty; FN : string);
var
   INIFile : TINIFile;
   i       : integer;
   temp    : integer;
   tempstr : string;
begin
   INIFile := TINIFile.Create(FN);

   for i:=0 to x.count-1 do
   begin
      if y[i].PropType = ptHexByte then
      begin
         INIFile.WriteString(Section,y[i].PropKey,x.item[i].subitems.strings[0]);
      end
      else if y[i].PropType = ptStringList then
      begin
         tempstr := x.item[i].subitems.strings[0];
         temp    := pos(':',TempStr);
         TempStr := copy(tempstr,1,temp-1);
         INIFile.WriteString(Section,y[i].PropKey,TempStr);
      end
   end;

   INIFile.Free;
end;

// ***********************************************************
// Edit a generic property
// ***********************************************************
procedure  EditProperty(ListItem : TListItem; y : TProperty);
var
   temp : string;
   i    : integer;
begin
   if y.PropType = ptHexByte then
   begin
      GenTextEdit.Caption := 'Edit property';
      GenTextEdit.LabelText.Caption := 'Change: '+y.Name;
      GenTextEdit.EditBox.text      := ListItem.SubItems.strings[0];
      if GenTextEdit.Execute = mrOK then
      begin
         temp := GenTextEdit.EditBox.text;
         temp := trim(temp);
         if (length(temp) > 2) or (not CheckHexCharsOnly(temp)) then
            MessageDlg('Please enter a hex byte value!',mtWarning,[mbOK],0)
         else
         begin
            ListItem.SubItems.strings[0] := temp;
         end;
      end;
   end
   else if y.propType = ptStringList then
   begin
      GenComboFrm.caption := 'Please select an option';
      genComboFrm.Combo.Items.Clear;
      genComboFrm.Combo.Style := csDropDownList;
      GenComboFrm.Combo.Items := TStrings(y.Data^);
      GenComboFrm.TextLabel.Caption := 'Change: Addressing mode';
      i := FindTextInList(ListItem.Subitems[0],GenComboFrm.Combo.Items);
      if i < 0 then i := 0;
      GenComboFrm.Combo.ItemIndex := i;
      if GenComboFrm.Execute = mrOK then
      begin
         ListItem.SubItems.strings[0] :=
             GenComboFrm.Combo.Items.strings[GenComboFrm.Combo.ItemIndex];
      end;
   end;
end;

// ***********************************************************
// Generate a string description for a font
// ***********************************************************
function  FontToStringDesc(x : TFont) : string;
var
   Desc : string;
begin
   Desc := x.Name+', ';

   Desc := Desc + ' Height='+Sysutils.IntToStr(x.Height);
   Desc := Desc + ' Size='+Sysutils.IntToStr(x.Size);

        if fpDefault = x.Pitch   then Desc := Desc + ' Default pitch, '
   else if fpFixed = x.Pitch     then Desc := Desc + ' Fixed pitch, '
   else if fpVariable = x.Pitch  then Desc := Desc + ' Variable pitch, ';

   if (fsBold in x.Style)      then Desc := Desc + ' Bold, ';
   if (fsUnderline in x.Style) then Desc := Desc + ' Underline, ';
   if (fsStrikeOut in x.Style) then Desc := Desc + ' Strikeout, ';
   if (fsItalic in x.Style)    then Desc := Desc + ' Italic, ';

   Result := Desc;
end;

// ***********************************************************
// Spread buttons evenly across a form
// ***********************************************************
function SpreadButtons(Form : TForm; ButtonList : TButtonList; SpaceBetween : integer; Warn : Boolean = True) : integer;
var
   i              : integer;
   TotButtonWidth : integer;
   TotWidth       : integer;
   Left           : integer;
   Pos            : integer;
   TempButton     : TButton;
begin
   Result := -1;
   if ButtonList.Count = 0 then Exit;


   TotButtonWidth := 0;

   for i := 0 to ButtonList.count-1 do
   begin
      TotButtonWidth := TotButtonWidth + ButtonList.getButton(i).Width;
   end;

   TotWidth := TotButtonWidth + (SpaceBetween * ButtonList.Count-1);
   if TotWidth > Form.Width then
   begin
      result := TotWidth;
      if Warn then
      begin
         if MessageDlg('SpreadButtons warning! The buttons will not fit into the form! '+
           'Continue anyway?',mtWarning,[mbYes, mbNo],0) = mrNo then Exit;
      end;
   end;

   Left := (Form.Width div 2) - (TotWidth div 2);
   Pos  := Left;

   // now place the buttons
   for i := 0 to ButtonList.count-1 do
   begin
      TempButton      := ButtonList.GetButton(i);
      TempButton.Left := Pos;

      Pos := Pos + TempButton.Width + SpaceBetween;
   end;

end;

// ***********************************************************
// Find an item in a list view
// ***********************************************************
function  FindInListView(x : TListView; y : string) : TListItem;
var
   i            : integer;
   TempListItem : TListItem;
begin
   Result := nil;

   if x.Items.count = 0 then Exit;

   for i := 0 to x.Items.Count-1 do
   begin
      TempListItem := x.Items[i];
      if TempListItem.Caption = y then
         Result := TempListItem;
   end;

end;

// ***********************************************************
// Find an item in a list view
// ***********************************************************
function  FindInTreeView(x : TTreeView; y : string) : TTreeNode;
var
   i            : integer;
   TempTreeNode : TTreeNode;
begin
   Result := nil;

   if x.Items.count = 0 then Exit;

   for i := 0 to x.Items.Count-1 do
   begin
      TempTreeNode := x.Items[i];
      if TemptreeNode.Text = y then
         Result := TempTreeNode;
   end;
end;

// ***********************************************************
// Check a child node by its name for existance
// ***********************************************************
function GetChildCalled(TreeNode : TTreeNode; Text : string) : TtreeNode;
var
   ChildNode : TTreeNode;
begin
   GetChildCalled := nil;
   ChildNode      := TreeNode.getFirstChild;

   while (ChildNode <> nil) do
   begin
      if ChildNode.Text = Text then
      begin
         GetChildCalled := ChildNode;
         Exit;
      end;
      ChildNode := TreeNode.getNextChild(ChildNode);
   end;
end;

// ***********************************************************
// Make a copy of a node and its children
// ***********************************************************
procedure CopyNodeTo(DstTreeNode,SrcTreeNode : TTreeNode; TreeView : TTreeView);
var
   SrcChildNode  : TTreeNode;
   DstChildNode : TTreeNode;
begin
   DstTreeNode.Text := SrcTreeNode.Text;
   SrcChildNode := SrcTreeNode.getFirstChild;
   while SrcChildNode <> nil do
   begin
      DstChildNode := GetChildCalled(DstTreeNode,SrcChildNode.Text);
      if DstChildNode = nil then
         DstChildNode := TreeView.Items.AddChild(DsttreeNode,SrcChildNode.Text);
      CopyNodeTo(DstChildNode,SrcChildNode,TreeView);
      SrcChildNode := SrcTreeNode.getNextChild(SrcChildNode);
   end;
end;

// ***********************************************************
// Maximise a form
// ***********************************************************
procedure SizeForTaskBar(MyForm: TForm);
var
   TaskBarHandle: HWnd; { Handle to the Win95 Taskbar }
   TaskBarCoord: TRect; { Coordinates of the Win95 Taskbar }
   CxScreen, { Width of screen in pixels }
   CyScreen, { Height of screen in pixels }
   CxFullScreen, { Width of client area in pixels }
   CyFullScreen, { Heigth of client area in pixels }
   CyCaption: Integer; { Height of a window's title bar in pixels }

begin
{      CyCaption     := GetSystemMetrics(SM_CYCAPTION);
      MyForm.Top    := Screen.DesktopTop;
      MyForm.Left   := Screen.DesktopLeft;
      MyForm.Width  := Screen.DesktopWidth;
      MyForm.Height := 250; //Screen.DesktopHeight - CyCaption;
      Exit;}

   TaskBarHandle := FindWindow('Shell_TrayWnd',Nil); { Get Win95 Taskbar =
   handle }

   if TaskBarHandle = 0 then { We're running Win 3.x or WinNT w/o Win95 =
   shell, so just maximize }
   begin
   MessageDlg('Win 3.x / WinNT detected for maximising',mtWarning,[mbOK],0);
      MyForm.WindowState := wsMaximized;
   end
   else { We're running Win95 or WinNT w/Win95 shell }
   begin
      MyForm.WindowState := wsNormal;
      GetWindowRect(TaskBarHandle,TaskBarCoord); { Get coordinates of Win95 =
      Taskbar }
      CxScreen := GetSystemMetrics(SM_CXSCREEN); { Get various screen =
      dimensions and set form's width/height }
      CyScreen := GetSystemMetrics(SM_CYSCREEN);
      CxFullScreen := GetSystemMetrics(SM_CXFULLSCREEN);
      CyFullScreen := GetSystemMetrics(SM_CYFULLSCREEN);
      CyCaption := GetSystemMetrics(SM_CYCAPTION);
      MyForm.Top := 0;
      MyForm.Left := 0;
      MyForm.Width := CxScreen - (CxScreen - CxFullScreen) {+ 1};
      MyForm.Height := CyScreen - (CyScreen - CyFullScreen) - 10 {+ CyCaption + 1};
      if (TaskBarCoord.Top = -2) and (TaskBarCoord.Left = -2) then { =
      Taskbar on either top or left }
          if TaskBarCoord.Right > TaskBarCoord.Bottom then { Taskbar on top }
              MyForm.Top := TaskBarCoord.Bottom
      else { Taskbar on left }
          MyForm.Left := TaskBarCoord.Right;
   end;
End;

// ***********************************************************
// disable the entire menu
// ***********************************************************
procedure DisableEntireMenu( x: TMenu);
var
   i            : integer;
   tempMenuItem : TMenuItem;
begin
   for i:= 0 to x.Items.Count-1 do
   begin
      tempMenuItem := x.Items[i];
      TempMenuItem.enabled := False;
   end;
   Application.ProcessMessages;
   Application.HandleMessage;
end;

// ***********************************************************
// enable the entire menu
// ***********************************************************
procedure EnableEntireMenu( x: TMenu);
var
   i            : integer;
   tempMenuItem : TMenuItem;
begin
   for i:= 0 to x.Items.Count-1 do
   begin
      tempMenuItem := x.Items[i];
      TempMenuItem.enabled := true;
   end;

   Application.ProcessMessages;
   Application.HandleMessage;
end;

// ***********************************************************
// Assign a hint to a control and enable the hint
// ***********************************************************
procedure AssignHint(Control: TControl; Str : string);
begin
   if Control <> nil then
   begin
      Control.Hint     := Str;
      Control.ShowHint := true;
   end;
end;

// ***********************************************************
// Send a key to the active control in a form
// ***********************************************************
procedure SendDownKeyToActiveControl(Form: TForm; Sender: TObject; Key : word; ShiftState : TShiftState);
//var
{ TODO : fix this
 }
   //TempSheet : TSpSheet;
   //Temptable : Ttable;
begin
   (*if Form.ActiveControl is TTable then
   begin
      TempTable := Form.ActiveControl as TTable;

      if Assigned(TempTable.OnKeyDown) then
      begin
         Temptable.OnkeyDown(Sender,Key,ShiftState);
      end;
   end
   else if Form.ActiveControl is TSpSheet then
   begin
      TempSheet := Form.ActiveControl as TSpSheet;

      if Assigned(TempSheet.OnKeyDown) then
      begin
         TempSheet.OnkeyDown(Sender,Key,ShiftState);
      end;
   end;
     *)
end;

(*
// **************************************************************************
// func:    DisplayMemBuffer
// inputs:  int address offset,
//          int memory size
//          int bytes to display per line
//          pointer to memory buffer
//          Memp to display the buffer in
// outputs: None
// Effect:  Displays a memory buffer in a TMemo component
// ***************************************************************************)
procedure DisplayMemBuffer(AddrOffset : integer; MemSize : integer;
                           BytesPerLine : integer; Buffer : pByteArray; Memo : TMemo);
var
    CurAddr : integer;
    i       : integer;
begin

   if (Memo = nil) then exit;

   Memo.Lines.Clear;

   CurAddr := AddrOffset;
   i := 0;
   while (i < (MemSize+BytesPerLine)) do
   begin
      Memo.Lines.Add(ConvertBufToLine(CurAddr,PByteArray(@Buffer[i]),BytesPerLine));
      CurAddr := CurAddr +  BytesPerLine;
      i := i + BytesPerLine;
   end;
end;

// **************************************************************************
// func:    HandleCopy
// inputs:  TForm component handle
// outputs: None
// Effect:  Distribute the Copy command to the active control in the form
// **************************************************************************
procedure HandleCopy(Form : TForm);
var
    temp : TWinControl;
begin
   Temp := Form.ActiveControl;
   PostMessage(Temp.Handle,WM_COPY,0,0);
end;

// **************************************************************************
// method:  HandlePaste
// inputs:  TForm component handle
// outputs: None
// Effect:  Distribute the paste command to the active control in the form
// **************************************************************************
procedure HandlePaste(Form : TForm);
var
    temp : TWinControl;
begin
   Temp := Form.ActiveControl;
   PostMessage(Temp.Handle,WM_PASTE,0,0);
end;

// **************************************************************************
// method:  HandleCut
// inputs:  TForm component handle
// outputs: None
// Effect:  Distribute the cut command to the active control in the form
// **************************************************************************
procedure HandleCut(Form : TForm);
var
    temp : TWinControl;
begin
   Temp := Form.ActiveControl;
   PostMessage(Temp.Handle,WM_CUT,0,0);
end;

// **************************************************************************
// Func:    FindComponentByName
// inputs:  Name of component to find
//          Componentlist to look in
// outputs: TComponent
// Effect:  Returns nil if not found, otherwise returns the component
// **************************************************************************
function  FindComponentByName(Name : String; CompList : TComponentList) : TComponent;
var
    i : integer;
begin
    Result := nil;

    for i := 0 to CompList.Count - 1 do
    begin
        if CompList.Items[i].Name = Name then
        begin
            result := CompList.Items[i];
            exit;
        end;
    end;
end;

// **************************************************************************
// Func:    FindComponentByName
// inputs:  Name of component to find
//          WinControl to look in
// outputs: TComponent
// Effect:  Returns nil if not found, otherwise returns the component
// **************************************************************************
function  FindComponentByName(Name : String; Comp : TWinControl) : TComponent;
var
    i : integer;
begin
    Result := nil;

    for i := 0 to Comp.ComponentCount - 1 do
    begin
        if Comp.Components[i].Name = Name then
        begin
            result := Comp.Components[i];
            exit;
        end;
    end;
end;

// **************************************************************************
// Func:    SaveMenuToFile
// inputs:  Object ref to menu
//          filename to save to
// outputs: None
// Effect:  Save the menu structure of the application to a file
// **************************************************************************
procedure SaveMenuToFile(mnu : TMenu; fn : string);
var
    fh : TextFile;
    lev : integer;
    k   : integer;

    procedure WriteMenu(mitm : TMenuItem);
    var
        i : integer;
        j : integer;
    begin
        lev := lev + 1;
        for i := 0 to mitm.Count-1 do;
        begin
            for j := 1 to lev do
                write(fh,#9); // tab
            writeln(fh,mitm.Caption);

            if (mitm.Items[i].Count > 1) then
                WriteMenu(mitm.Items[i]);
        end;
        lev := lev - 1;
    end;

begin
    AssignFile(fh,fn);
    {$I-}
    Rewrite(fh);
    {$I+}

    lev := 0;

    writeln(fh,'Item count = '+SysUtils.IntToStr(mnu.Items.Count));

    {if (mnu.Items.Count > 0) then
    begin
        for k := 0 to mnu.Items.Count-1 do
            WriteMenu(mnu.Items[k]);
    end;}

    CloseFile(FH);
end;

// **************************************************************************
// Func:    SaveMenuToFile
// inputs:  Object ref to menu
//          filename to save to
// outputs: None
// Effect:  Save the menu structure of the application to a file
// **************************************************************************
procedure LoadMenuFromFile(x : TMenu; fn : string);
begin
end;

end.

