unit MsgMon.System.Data.MessageName;

interface

uses
  System.Generics.Collections,
  Winapi.Messages;

type
  TMessageNameScope = (mnsGlobal, mnsEdit, mnsButton, mnsList, mnsApp, mnsUser, mnsCustom);

  TMsgMonMessageName = class
  public
    id: Integer;
    name: string;
    scope: TMessageNameScope;
    ScopeName: string;
    constructor Create(id: Integer; const name: string; const scope: TMessageNameScope);
  end;

  TMsgMonMessageNameDictionary = class(TObjectDictionary<Integer,TMsgMonMessageName>)
  private
    procedure AddDefault(id: Integer; const name: string; const scope: TMessageNameScope);
  public
    constructor Create;
    procedure FillDefault;
  end;

implementation

uses
  System.TypInfo;

{ TMessageName }

constructor TMsgMonMessageName.Create(id: Integer; const name: string;
  const scope: TMessageNameScope);
begin
  inherited Create;
  Self.id := id;
  Self.name := name;
  Self.scope := scope;
  Self.ScopeName := Copy(GetEnumName(TypeInfo(TMessageNameScope), Ord(scope)), 4, MaxInt);
end;

{----------------------------------------}

{ TMsgMonMessageNameDictionary }

procedure TMsgMonMessageNameDictionary.AddDefault(id: Integer;
  const name: string; const scope: TMessageNameScope);
begin
  Assert(not Self.ContainsKey(id));
  Add(id, TMsgMonMessageName.Create(id, name, scope));
end;

constructor TMsgMonMessageNameDictionary.Create;
begin
  inherited Create;
  FillDefault;
end;

procedure TMsgMonMessageNameDictionary.FillDefault;
begin
  AddDefault(WM_NULL, 'WM_NULL', mnsGlobal);
  AddDefault(WM_CREATE, 'WM_CREATE', mnsGlobal);
  AddDefault(WM_DESTROY, 'WM_DESTROY', mnsGlobal);
  AddDefault(WM_MOVE, 'WM_MOVE', mnsGlobal);
  AddDefault(WM_SIZE, 'WM_SIZE', mnsGlobal);
  AddDefault(WM_ACTIVATE, 'WM_ACTIVATE', mnsGlobal);
  AddDefault(WM_SETFOCUS, 'WM_SETFOCUS', mnsGlobal);
  AddDefault(WM_KILLFOCUS, 'WM_KILLFOCUS', mnsGlobal);
  AddDefault(WM_ENABLE, 'WM_ENABLE', mnsGlobal);
  AddDefault(WM_SETREDRAW, 'WM_SETREDRAW', mnsGlobal);
  AddDefault(WM_SETTEXT, 'WM_SETTEXT', mnsGlobal);
  AddDefault(WM_GETTEXT, 'WM_GETTEXT', mnsGlobal);
  AddDefault(WM_GETTEXTLENGTH, 'WM_GETTEXTLENGTH', mnsGlobal);
  AddDefault(WM_PAINT, 'WM_PAINT', mnsGlobal);
  AddDefault(WM_CLOSE, 'WM_CLOSE', mnsGlobal);
  AddDefault(WM_QUERYENDSESSION, 'WM_QUERYENDSESSION', mnsGlobal);
  AddDefault(WM_QUIT, 'WM_QUIT', mnsGlobal);
  AddDefault(WM_QUERYOPEN, 'WM_QUERYOPEN', mnsGlobal);
  AddDefault(WM_ERASEBKGND, 'WM_ERASEBKGND', mnsGlobal);
  AddDefault(WM_SYSCOLORCHANGE, 'WM_SYSCOLORCHANGE', mnsGlobal);
  AddDefault(WM_ENDSESSION, 'WM_ENDSESSION', mnsGlobal);
  AddDefault(WM_SYSTEMERROR, 'WM_SYSTEMERROR', mnsGlobal);
  AddDefault(WM_SHOWWINDOW, 'WM_SHOWWINDOW', mnsGlobal);
  AddDefault(WM_CTLCOLOR, 'WM_CTLCOLOR', mnsGlobal);
  AddDefault(WM_WININICHANGE, 'WM_WININICHANGE', mnsGlobal);
//  AddDefault(WM_SETTINGCHANGE, 'WM_SETTINGCHANGE', mnsGlobal); = WM_WININICHANGE
  AddDefault(WM_DEVMODECHANGE, 'WM_DEVMODECHANGE', mnsGlobal);
  AddDefault(WM_ACTIVATEAPP, 'WM_ACTIVATEAPP', mnsGlobal);
  AddDefault(WM_FONTCHANGE, 'WM_FONTCHANGE', mnsGlobal);
  AddDefault(WM_TIMECHANGE, 'WM_TIMECHANGE', mnsGlobal);
  AddDefault(WM_CANCELMODE, 'WM_CANCELMODE', mnsGlobal);
  AddDefault(WM_SETCURSOR, 'WM_SETCURSOR', mnsGlobal);
  AddDefault(WM_MOUSEACTIVATE, 'WM_MOUSEACTIVATE', mnsGlobal);
  AddDefault(WM_CHILDACTIVATE, 'WM_CHILDACTIVATE', mnsGlobal);
  AddDefault(WM_QUEUESYNC, 'WM_QUEUESYNC', mnsGlobal);
  AddDefault(WM_GETMINMAXINFO, 'WM_GETMINMAXINFO', mnsGlobal);
  AddDefault(WM_PAINTICON, 'WM_PAINTICON', mnsGlobal);
  AddDefault(WM_ICONERASEBKGND, 'WM_ICONERASEBKGND', mnsGlobal);
  AddDefault(WM_NEXTDLGCTL, 'WM_NEXTDLGCTL', mnsGlobal);
  AddDefault(WM_SPOOLERSTATUS, 'WM_SPOOLERSTATUS', mnsGlobal);
  AddDefault(WM_DRAWITEM, 'WM_DRAWITEM', mnsGlobal);
  AddDefault(WM_MEASUREITEM, 'WM_MEASUREITEM', mnsGlobal);
  AddDefault(WM_DELETEITEM, 'WM_DELETEITEM', mnsGlobal);
  AddDefault(WM_VKEYTOITEM, 'WM_VKEYTOITEM', mnsGlobal);
  AddDefault(WM_CHARTOITEM, 'WM_CHARTOITEM', mnsGlobal);
  AddDefault(WM_SETFONT, 'WM_SETFONT', mnsGlobal);
  AddDefault(WM_GETFONT, 'WM_GETFONT', mnsGlobal);
  AddDefault(WM_SETHOTKEY, 'WM_SETHOTKEY', mnsGlobal);
  AddDefault(WM_GETHOTKEY, 'WM_GETHOTKEY', mnsGlobal);
  AddDefault(WM_QUERYDRAGICON, 'WM_QUERYDRAGICON', mnsGlobal);
  AddDefault(WM_COMPAREITEM, 'WM_COMPAREITEM', mnsGlobal);
  AddDefault(WM_GETOBJECT, 'WM_GETOBJECT', mnsGlobal);
  AddDefault(WM_COMPACTING, 'WM_COMPACTING', mnsGlobal);

  AddDefault(WM_COMMNOTIFY, 'WM_COMMNOTIFY', mnsGlobal);    { obsolete in Win32}

  AddDefault(WM_WINDOWPOSCHANGING, 'WM_WINDOWPOSCHANGING', mnsGlobal);
  AddDefault(WM_WINDOWPOSCHANGED, 'WM_WINDOWPOSCHANGED', mnsGlobal);
  AddDefault(WM_POWER, 'WM_POWER', mnsGlobal);
  AddDefault(WM_COPYGLOBALDATA, 'WM_COPYGLOBALDATA', mnsGlobal);
  AddDefault(WM_COPYDATA, 'WM_COPYDATA', mnsGlobal);
  AddDefault(WM_CANCELJOURNAL, 'WM_CANCELJOURNAL', mnsGlobal);
  AddDefault(WM_NOTIFY, 'WM_NOTIFY', mnsGlobal);
  AddDefault(WM_INPUTLANGCHANGEREQUEST, 'WM_INPUTLANGCHANGEREQUEST', mnsGlobal);
  AddDefault(WM_INPUTLANGCHANGE, 'WM_INPUTLANGCHANGE', mnsGlobal);
  AddDefault(WM_TCARD, 'WM_TCARD', mnsGlobal);
  AddDefault(WM_HELP, 'WM_HELP', mnsGlobal);
  AddDefault(WM_USERCHANGED, 'WM_USERCHANGED', mnsGlobal);
  AddDefault(WM_NOTIFYFORMAT, 'WM_NOTIFYFORMAT', mnsGlobal);

  AddDefault(WM_CONTEXTMENU, 'WM_CONTEXTMENU', mnsGlobal);
  AddDefault(WM_STYLECHANGING, 'WM_STYLECHANGING', mnsGlobal);
  AddDefault(WM_STYLECHANGED, 'WM_STYLECHANGED', mnsGlobal);
  AddDefault(WM_DISPLAYCHANGE, 'WM_DISPLAYCHANGE', mnsGlobal);
  AddDefault(WM_GETICON, 'WM_GETICON', mnsGlobal);
  AddDefault(WM_SETICON, 'WM_SETICON', mnsGlobal);

  AddDefault(WM_NCCREATE, 'WM_NCCREATE', mnsGlobal);
  AddDefault(WM_NCDESTROY, 'WM_NCDESTROY', mnsGlobal);
  AddDefault(WM_NCCALCSIZE, 'WM_NCCALCSIZE', mnsGlobal);
  AddDefault(WM_NCHITTEST, 'WM_NCHITTEST', mnsGlobal);
  AddDefault(WM_NCPAINT, 'WM_NCPAINT', mnsGlobal);
  AddDefault(WM_NCACTIVATE, 'WM_NCACTIVATE', mnsGlobal);
  AddDefault(WM_GETDLGCODE, 'WM_GETDLGCODE', mnsGlobal);
  AddDefault(WM_NCMOUSEMOVE, 'WM_NCMOUSEMOVE', mnsGlobal);
  AddDefault(WM_NCLBUTTONDOWN, 'WM_NCLBUTTONDOWN', mnsGlobal);
  AddDefault(WM_NCLBUTTONUP, 'WM_NCLBUTTONUP', mnsGlobal);
  AddDefault(WM_NCLBUTTONDBLCLK, 'WM_NCLBUTTONDBLCLK', mnsGlobal);
  AddDefault(WM_NCRBUTTONDOWN, 'WM_NCRBUTTONDOWN', mnsGlobal);
  AddDefault(WM_NCRBUTTONUP, 'WM_NCRBUTTONUP', mnsGlobal);
  AddDefault(WM_NCRBUTTONDBLCLK, 'WM_NCRBUTTONDBLCLK', mnsGlobal);
  AddDefault(WM_NCMBUTTONDOWN, 'WM_NCMBUTTONDOWN', mnsGlobal);
  AddDefault(WM_NCMBUTTONUP, 'WM_NCMBUTTONUP', mnsGlobal);
  AddDefault(WM_NCMBUTTONDBLCLK, 'WM_NCMBUTTONDBLCLK', mnsGlobal);

  AddDefault(WM_NCXBUTTONDOWN, 'WM_NCXBUTTONDOWN', mnsGlobal);
  AddDefault(WM_NCXBUTTONUP, 'WM_NCXBUTTONUP', mnsGlobal);
  AddDefault(WM_NCXBUTTONDBLCLK, 'WM_NCXBUTTONDBLCLK', mnsGlobal);
  AddDefault(WM_INPUT_DEVICE_CHANGE, 'WM_INPUT_DEVICE_CHANGE', mnsGlobal);
  AddDefault(WM_INPUT, 'WM_INPUT', mnsGlobal);

//  AddDefault(WM_KEYFIRST, 'WM_KEYFIRST', mnsGlobal);
  AddDefault(WM_KEYDOWN, 'WM_KEYDOWN', mnsGlobal);
  AddDefault(WM_KEYUP, 'WM_KEYUP', mnsGlobal);
  AddDefault(WM_CHAR, 'WM_CHAR', mnsGlobal);
  AddDefault(WM_DEADCHAR, 'WM_DEADCHAR', mnsGlobal);
  AddDefault(WM_SYSKEYDOWN, 'WM_SYSKEYDOWN', mnsGlobal);
  AddDefault(WM_SYSKEYUP, 'WM_SYSKEYUP', mnsGlobal);
  AddDefault(WM_SYSCHAR, 'WM_SYSCHAR', mnsGlobal);
  AddDefault(WM_SYSDEADCHAR, 'WM_SYSDEADCHAR', mnsGlobal);
  AddDefault(WM_UNICHAR, 'WM_UNICHAR', mnsGlobal);
//  AddDefault(WM_KEYLAST, 'WM_KEYLAST', mnsGlobal);

  AddDefault(WM_INITDIALOG, 'WM_INITDIALOG', mnsGlobal);
  AddDefault(WM_COMMAND, 'WM_COMMAND', mnsGlobal);
  AddDefault(WM_SYSCOMMAND, 'WM_SYSCOMMAND', mnsGlobal);
  AddDefault(WM_TIMER, 'WM_TIMER', mnsGlobal);
  AddDefault(WM_HSCROLL, 'WM_HSCROLL', mnsGlobal);
  AddDefault(WM_VSCROLL, 'WM_VSCROLL', mnsGlobal);
  AddDefault(WM_INITMENU, 'WM_INITMENU', mnsGlobal);
  AddDefault(WM_INITMENUPOPUP, 'WM_INITMENUPOPUP', mnsGlobal);

  AddDefault(WM_GESTURE, 'WM_GESTURE', mnsGlobal);
  AddDefault(WM_GESTURENOTIFY, 'WM_GESTURENOTIFY', mnsGlobal);

  AddDefault(WM_MENUSELECT, 'WM_MENUSELECT', mnsGlobal);
  AddDefault(WM_MENUCHAR, 'WM_MENUCHAR', mnsGlobal);
  AddDefault(WM_ENTERIDLE, 'WM_ENTERIDLE', mnsGlobal);

  AddDefault(WM_MENURBUTTONUP, 'WM_MENURBUTTONUP', mnsGlobal);
  AddDefault(WM_MENUDRAG, 'WM_MENUDRAG', mnsGlobal);
  AddDefault(WM_MENUGETOBJECT, 'WM_MENUGETOBJECT', mnsGlobal);
  AddDefault(WM_UNINITMENUPOPUP, 'WM_UNINITMENUPOPUP', mnsGlobal);
  AddDefault(WM_MENUCOMMAND, 'WM_MENUCOMMAND', mnsGlobal);

  AddDefault(WM_CHANGEUISTATE, 'WM_CHANGEUISTATE', mnsGlobal);
  AddDefault(WM_UPDATEUISTATE, 'WM_UPDATEUISTATE', mnsGlobal);
  AddDefault(WM_QUERYUISTATE, 'WM_QUERYUISTATE', mnsGlobal);

  AddDefault(WM_CTLCOLORMSGBOX, 'WM_CTLCOLORMSGBOX', mnsGlobal);
  AddDefault(WM_CTLCOLOREDIT, 'WM_CTLCOLOREDIT', mnsGlobal);
  AddDefault(WM_CTLCOLORLISTBOX, 'WM_CTLCOLORLISTBOX', mnsGlobal);
  AddDefault(WM_CTLCOLORBTN, 'WM_CTLCOLORBTN', mnsGlobal);
  AddDefault(WM_CTLCOLORDLG, 'WM_CTLCOLORDLG', mnsGlobal);
  AddDefault(WM_CTLCOLORSCROLLBAR, 'WM_CTLCOLORSCROLLBAR', mnsGlobal);
  AddDefault(WM_CTLCOLORSTATIC, 'WM_CTLCOLORSTATIC', mnsGlobal);

//  AddDefault(WM_MOUSEFIRST, 'WM_MOUSEFIRST', mnsGlobal);
  AddDefault(WM_MOUSEMOVE, 'WM_MOUSEMOVE', mnsGlobal);
  AddDefault(WM_LBUTTONDOWN, 'WM_LBUTTONDOWN', mnsGlobal);
  AddDefault(WM_LBUTTONUP, 'WM_LBUTTONUP', mnsGlobal);
  AddDefault(WM_LBUTTONDBLCLK, 'WM_LBUTTONDBLCLK', mnsGlobal);
  AddDefault(WM_RBUTTONDOWN, 'WM_RBUTTONDOWN', mnsGlobal);
  AddDefault(WM_RBUTTONUP, 'WM_RBUTTONUP', mnsGlobal);
  AddDefault(WM_RBUTTONDBLCLK, 'WM_RBUTTONDBLCLK', mnsGlobal);
  AddDefault(WM_MBUTTONDOWN, 'WM_MBUTTONDOWN', mnsGlobal);
  AddDefault(WM_MBUTTONUP, 'WM_MBUTTONUP', mnsGlobal);
  AddDefault(WM_MBUTTONDBLCLK, 'WM_MBUTTONDBLCLK', mnsGlobal);
  AddDefault(WM_MOUSEWHEEL, 'WM_MOUSEWHEEL', mnsGlobal);

  AddDefault(WM_XBUTTONDOWN, 'WM_XBUTTONDOWN', mnsGlobal);
  AddDefault(WM_XBUTTONUP, 'WM_XBUTTONUP', mnsGlobal);
  AddDefault(WM_XBUTTONDBLCLK, 'WM_XBUTTONDBLCLK', mnsGlobal);
  AddDefault(WM_MOUSEHWHEEL, 'WM_MOUSEHWHEEL', mnsGlobal);

//  AddDefault(WM_MOUSELAST, 'WM_MOUSELAST', mnsGlobal);

  AddDefault(WM_PARENTNOTIFY, 'WM_PARENTNOTIFY', mnsGlobal);
  AddDefault(WM_ENTERMENULOOP, 'WM_ENTERMENULOOP', mnsGlobal);
  AddDefault(WM_EXITMENULOOP, 'WM_EXITMENULOOP', mnsGlobal);
  AddDefault(WM_NEXTMENU, 'WM_NEXTMENU', mnsGlobal);

  AddDefault(WM_SIZING, 'WM_SIZING', mnsGlobal);
  AddDefault(WM_CAPTURECHANGED, 'WM_CAPTURECHANGED', mnsGlobal);
  AddDefault(WM_MOVING, 'WM_MOVING', mnsGlobal);
  AddDefault(WM_POWERBROADCAST, 'WM_POWERBROADCAST', mnsGlobal);
  AddDefault(WM_DEVICECHANGE, 'WM_DEVICECHANGE', mnsGlobal);

  AddDefault(WM_IME_STARTCOMPOSITION, 'WM_IME_STARTCOMPOSITION', mnsGlobal);
  AddDefault(WM_IME_ENDCOMPOSITION, 'WM_IME_ENDCOMPOSITION', mnsGlobal);
  AddDefault(WM_IME_COMPOSITION, 'WM_IME_COMPOSITION', mnsGlobal);
//  AddDefault(WM_IME_KEYLAST, 'WM_IME_KEYLAST', mnsGlobal);

  AddDefault(WM_IME_SETCONTEXT, 'WM_IME_SETCONTEXT', mnsGlobal);
  AddDefault(WM_IME_NOTIFY, 'WM_IME_NOTIFY', mnsGlobal);
  AddDefault(WM_IME_CONTROL, 'WM_IME_CONTROL', mnsGlobal);
  AddDefault(WM_IME_COMPOSITIONFULL, 'WM_IME_COMPOSITIONFULL', mnsGlobal);
  AddDefault(WM_IME_SELECT, 'WM_IME_SELECT', mnsGlobal);
  AddDefault(WM_IME_CHAR, 'WM_IME_CHAR', mnsGlobal);
  AddDefault(WM_IME_REQUEST, 'WM_IME_REQUEST', mnsGlobal);

  AddDefault(WM_IME_KEYDOWN, 'WM_IME_KEYDOWN', mnsGlobal);
  AddDefault(WM_IME_KEYUP, 'WM_IME_KEYUP', mnsGlobal);

  AddDefault(WM_MDICREATE, 'WM_MDICREATE', mnsGlobal);
  AddDefault(WM_MDIDESTROY, 'WM_MDIDESTROY', mnsGlobal);
  AddDefault(WM_MDIACTIVATE, 'WM_MDIACTIVATE', mnsGlobal);
  AddDefault(WM_MDIRESTORE, 'WM_MDIRESTORE', mnsGlobal);
  AddDefault(WM_MDINEXT, 'WM_MDINEXT', mnsGlobal);
  AddDefault(WM_MDIMAXIMIZE, 'WM_MDIMAXIMIZE', mnsGlobal);
  AddDefault(WM_MDITILE, 'WM_MDITILE', mnsGlobal);
  AddDefault(WM_MDICASCADE, 'WM_MDICASCADE', mnsGlobal);
  AddDefault(WM_MDIICONARRANGE, 'WM_MDIICONARRANGE', mnsGlobal);
  AddDefault(WM_MDIGETACTIVE, 'WM_MDIGETACTIVE', mnsGlobal);
  AddDefault(WM_MDISETMENU, 'WM_MDISETMENU', mnsGlobal);

  AddDefault(WM_ENTERSIZEMOVE, 'WM_ENTERSIZEMOVE', mnsGlobal);
  AddDefault(WM_EXITSIZEMOVE, 'WM_EXITSIZEMOVE', mnsGlobal);
  AddDefault(WM_DROPFILES, 'WM_DROPFILES', mnsGlobal);
  AddDefault(WM_MDIREFRESHMENU, 'WM_MDIREFRESHMENU', mnsGlobal);

  AddDefault(WM_TOUCH, 'WM_TOUCH', mnsGlobal);

  AddDefault(WM_MOUSEHOVER, 'WM_MOUSEHOVER', mnsGlobal);
  AddDefault(WM_MOUSELEAVE, 'WM_MOUSELEAVE', mnsGlobal);

  AddDefault(WM_NCMOUSEHOVER, 'WM_NCMOUSEHOVER', mnsGlobal);
  AddDefault(WM_NCMOUSELEAVE, 'WM_NCMOUSELEAVE', mnsGlobal);
  AddDefault(WM_WTSSESSION_CHANGE, 'WM_WTSSESSION_CHANGE', mnsGlobal);

//  AddDefault(WM_TABLET_FIRST, 'WM_TABLET_FIRST', mnsGlobal);
//  AddDefault(WM_TABLET_LAST, 'WM_TABLET_LAST', mnsGlobal);

  AddDefault(WM_DPICHANGED, 'WM_DPICHANGED', mnsGlobal);

  AddDefault(WM_CUT, 'WM_CUT', mnsGlobal);
  AddDefault(WM_COPY, 'WM_COPY', mnsGlobal);
  AddDefault(WM_PASTE, 'WM_PASTE', mnsGlobal);
  AddDefault(WM_CLEAR, 'WM_CLEAR', mnsGlobal);
  AddDefault(WM_UNDO, 'WM_UNDO', mnsGlobal);
  AddDefault(WM_RENDERFORMAT, 'WM_RENDERFORMAT', mnsGlobal);
  AddDefault(WM_RENDERALLFORMATS, 'WM_RENDERALLFORMATS', mnsGlobal);
  AddDefault(WM_DESTROYCLIPBOARD, 'WM_DESTROYCLIPBOARD', mnsGlobal);
  AddDefault(WM_DRAWCLIPBOARD, 'WM_DRAWCLIPBOARD', mnsGlobal);
  AddDefault(WM_PAINTCLIPBOARD, 'WM_PAINTCLIPBOARD', mnsGlobal);
  AddDefault(WM_VSCROLLCLIPBOARD, 'WM_VSCROLLCLIPBOARD', mnsGlobal);
  AddDefault(WM_SIZECLIPBOARD, 'WM_SIZECLIPBOARD', mnsGlobal);
  AddDefault(WM_ASKCBFORMATNAME, 'WM_ASKCBFORMATNAME', mnsGlobal);
  AddDefault(WM_CHANGECBCHAIN, 'WM_CHANGECBCHAIN', mnsGlobal);
  AddDefault(WM_HSCROLLCLIPBOARD, 'WM_HSCROLLCLIPBOARD', mnsGlobal);
  AddDefault(WM_QUERYNEWPALETTE, 'WM_QUERYNEWPALETTE', mnsGlobal);
  AddDefault(WM_PALETTEISCHANGING, 'WM_PALETTEISCHANGING', mnsGlobal);
  AddDefault(WM_PALETTECHANGED, 'WM_PALETTECHANGED', mnsGlobal);
  AddDefault(WM_HOTKEY, 'WM_HOTKEY', mnsGlobal);

  AddDefault(WM_PRINT, 'WM_PRINT', mnsGlobal);
  AddDefault(WM_PRINTCLIENT, 'WM_PRINTCLIENT', mnsGlobal);
  AddDefault(WM_APPCOMMAND, 'WM_APPCOMMAND', mnsGlobal);
  AddDefault(WM_THEMECHANGED, 'WM_THEMECHANGED', mnsGlobal);

  AddDefault(WM_CLIPBOARDUPDATE, 'WM_CLIPBOARDUPDATE', mnsGlobal);

  AddDefault(WM_HANDHELDFIRST, 'WM_HANDHELDFIRST', mnsGlobal);
  AddDefault(WM_HANDHELDLAST, 'WM_HANDHELDLAST', mnsGlobal);

  AddDefault(WM_PENWINFIRST, 'WM_PENWINFIRST', mnsGlobal);
  AddDefault(WM_PENWINLAST, 'WM_PENWINLAST', mnsGlobal);

  AddDefault(WM_COALESCE_FIRST, 'WM_COALESCE_FIRST', mnsGlobal);
  AddDefault(WM_COALESCE_LAST, 'WM_COALESCE_LAST', mnsGlobal);

//  AddDefault(WM_DDE_FIRST, 'WM_DDE_FIRST', mnsGlobal);
  AddDefault(WM_DDE_INITIATE, 'WM_DDE_INITIATE', mnsGlobal);
  AddDefault(WM_DDE_TERMINATE, 'WM_DDE_TERMINATE', mnsGlobal);
  AddDefault(WM_DDE_ADVISE, 'WM_DDE_ADVISE', mnsGlobal);
  AddDefault(WM_DDE_UNADVISE, 'WM_DDE_UNADVISE', mnsGlobal);
  AddDefault(WM_DDE_ACK, 'WM_DDE_ACK', mnsGlobal);
  AddDefault(WM_DDE_DATA, 'WM_DDE_DATA', mnsGlobal);
  AddDefault(WM_DDE_REQUEST, 'WM_DDE_REQUEST', mnsGlobal);
  AddDefault(WM_DDE_POKE, 'WM_DDE_POKE', mnsGlobal);
  AddDefault(WM_DDE_EXECUTE, 'WM_DDE_EXECUTE', mnsGlobal);
//  AddDefault(WM_DDE_LAST, 'WM_DDE_LAST', mnsGlobal);

  AddDefault(WM_DWMCOMPOSITIONCHANGED, 'WM_DWMCOMPOSITIONCHANGED', mnsGlobal);
  AddDefault(WM_DWMNCRENDERINGCHANGED, 'WM_DWMNCRENDERINGCHANGED', mnsGlobal);
  AddDefault(WM_DWMCOLORIZATIONCOLORCHANGED, 'WM_DWMCOLORIZATIONCOLORCHANGED', mnsGlobal);
  AddDefault(WM_DWMWINDOWMAXIMIZEDCHANGE, 'WM_DWMWINDOWMAXIMIZEDCHANGE', mnsGlobal);

  AddDefault(WM_DWMSENDICONICTHUMBNAIL, 'WM_DWMSENDICONICTHUMBNAIL', mnsGlobal);
  AddDefault(WM_DWMSENDICONICLIVEPREVIEWBITMAP, 'WM_DWMSENDICONICLIVEPREVIEWBITMAP', mnsGlobal);

  AddDefault(WM_GETTITLEBARINFOEX, 'WM_GETTITLEBARINFOEX', mnsGlobal);

  AddDefault(WM_TABLET_ADDED, 'WM_TABLET_ADDED', mnsGlobal);
  AddDefault(WM_TABLET_DELETED, 'WM_TABLET_DELETED', mnsGlobal);
  AddDefault(WM_TABLET_FLICK, 'WM_TABLET_FLICK', mnsGlobal);
  AddDefault(WM_TABLET_QUERYSYSTEMGESTURESTATUS, 'WM_TABLET_QUERYSYSTEMGESTURESTATUS', mnsGlobal);
end;

end.
