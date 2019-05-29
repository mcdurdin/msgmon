#include "stdafx.h"

PWCHAR szWindowsMessageNames[] = {
	L"WM_NULL",
	L"WM_CREATE",
	L"WM_DESTROY",
	L"WM_MOVE",
	L"WM_0004",
	L"WM_SIZE",
	L"WM_ACTIVATE",
	L"WM_SETFOCUS",
	L"WM_KILLFOCUS",
	L"WM_0009",
	L"WM_ENABLE",
	L"WM_SETREDRAW",
	L"WM_SETTEXT",
	L"WM_GETTEXT",
	L"WM_GETTEXTLENGTH",
	L"WM_PAINT",
	L"WM_CLOSE",
	L"WM_QUERYENDSESSION",
	L"WM_QUIT",
	L"WM_QUERYOPEN",
	L"WM_ERASEBKGND",
	L"WM_SYSCOLORCHANGE",
	L"WM_ENDSESSION",
	L"WM_SYSTEMERROR",
	L"WM_SHOWWINDOW",
	L"WM_CTLCOLOR",
	L"WM_WININICHANGE",
	L"WM_DEVMODECHANGE",
	L"WM_ACTIVATEAPP",
	L"WM_FONTCHANGE",
	L"WM_TIMECHANGE",
	L"WM_CANCELMODE",
	L"WM_SETCURSOR",
	L"WM_MOUSEACTIVATE",
	L"WM_CHILDACTIVATE",
	L"WM_QUEUESYNC",
	L"WM_GETMINMAXINFO",
	L"WM_0025",
	L"WM_PAINTICON",
	L"WM_ICONERASEBKGND",
	L"WM_NEXTDLGCTL",
	L"WM_0029",
	L"WM_SPOOLERSTATUS",
	L"WM_DRAWITEM",
	L"WM_MEASUREITEM",
	L"WM_DELETEITEM",
	L"WM_VKEYTOITEM",
	L"WM_CHARTOITEM",
	L"WM_SETFONT",
	L"WM_GETFONT",
	L"WM_SETHOTKEY",
	L"WM_GETHOTKEY",
	L"WM_0034",
	L"WM_0035",
	L"WM_0036",
	L"WM_QUERYDRAGICON",
	L"WM_0038",
	L"WM_COMPAREITEM",
	L"WM_003A",
	L"WM_003B",
	L"WM_003C",
	L"WM_GETOBJECT",
	L"WM_003E",
	L"WM_003F",
	L"WM_0040",
	L"WM_COMPACTING",
	L"WM_0042",
	L"WM_0043",
	L"WM_COMMNOTIFY",
	L"WM_0045",
	L"WM_WINDOWPOSCHANGING",
	L"WM_WINDOWPOSCHANGED",
	L"WM_POWER",
	L"WM_COPYGLOBALDATA",
	L"WM_COPYDATA",
	L"WM_CANCELJOURNAL",
	L"WM_004C",
	L"WM_004D",
	L"WM_NOTIFY",
	L"WM_004F",
	L"WM_INPUTLANGCHANGEREQUEST",
	L"WM_INPUTLANGCHANGE",
	L"WM_TCARD",
	L"WM_HELP",
	L"WM_USERCHANGED",
	L"WM_NOTIFYFORMAT",
	L"WM_0056",
	L"WM_0057",
	L"WM_0058",
	L"WM_0059",
	L"WM_005A",
	L"WM_005B",
	L"WM_005C",
	L"WM_005D",
	L"WM_005E",
	L"WM_005F",
	L"WM_0060",
	L"WM_0061",
	L"WM_0062",
	L"WM_0063",
	L"WM_0064",
	L"WM_0065",
	L"WM_0066",
	L"WM_0067",
	L"WM_0068",
	L"WM_0069",
	L"WM_006A",
	L"WM_006B",
	L"WM_006C",
	L"WM_006D",
	L"WM_006E",
	L"WM_006F",
	L"WM_0070",
	L"WM_0071",
	L"WM_0072",
	L"WM_0073",
	L"WM_0074",
	L"WM_0075",
	L"WM_0076",
	L"WM_0077",
	L"WM_0078",
	L"WM_0079",
	L"WM_007A",
	L"WM_CONTEXTMENU",
	L"WM_STYLECHANGING",
	L"WM_STYLECHANGED",
	L"WM_DISPLAYCHANGE",
	L"WM_GETICON",
	L"WM_SETICON",
	L"WM_NCCREATE",
	L"WM_NCDESTROY",
	L"WM_NCCALCSIZE",
	L"WM_NCHITTEST",
	L"WM_NCPAINT",
	L"WM_NCACTIVATE",
	L"WM_GETDLGCODE",
	L"WM_0088",
	L"WM_0089",
	L"WM_008A",
	L"WM_008B",
	L"WM_008C",
	L"WM_008D",
	L"WM_008E",
	L"WM_008F",
	L"WM_0090",
	L"WM_0091",
	L"WM_0092",
	L"WM_0093",
	L"WM_0094",
	L"WM_0095",
	L"WM_0096",
	L"WM_0097",
	L"WM_0098",
	L"WM_0099",
	L"WM_009A",
	L"WM_009B",
	L"WM_009C",
	L"WM_009D",
	L"WM_009E",
	L"WM_009F",
	L"WM_NCMOUSEMOVE",
	L"WM_NCLBUTTONDOWN",
	L"WM_NCLBUTTONUP",
	L"WM_NCLBUTTONDBLCLK",
	L"WM_NCRBUTTONDOWN",
	L"WM_NCRBUTTONUP",
	L"WM_NCRBUTTONDBLCLK",
	L"WM_NCMBUTTONDOWN",
	L"WM_NCMBUTTONUP",
	L"WM_NCMBUTTONDBLCLK",
	L"WM_00AA",
	L"WM_NCXBUTTONDOWN",
	L"WM_NCXBUTTONUP",
	L"WM_NCXBUTTONDBLCLK",
	L"WM_00AE",
	L"WM_00AF",
	L"WM_00B0",
	L"WM_00B1",
	L"WM_00B2",
	L"WM_00B3",
	L"WM_00B4",
	L"WM_00B5",
	L"WM_00B6",
	L"WM_00B7",
	L"WM_00B8",
	L"WM_00B9",
	L"WM_00BA",
	L"WM_00BB",
	L"WM_00BC",
	L"WM_00BD",
	L"WM_00BE",
	L"WM_00BF",
	L"WM_00C0",
	L"WM_00C1",
	L"WM_00C2",
	L"WM_00C3",
	L"WM_00C4",
	L"WM_00C5",
	L"WM_00C6",
	L"WM_00C7",
	L"WM_00C8",
	L"WM_00C9",
	L"WM_00CA",
	L"WM_00CB",
	L"WM_00CC",
	L"WM_00CD",
	L"WM_00CE",
	L"WM_00CF",
	L"WM_00D0",
	L"WM_00D1",
	L"WM_00D2",
	L"WM_00D3",
	L"WM_00D4",
	L"WM_00D5",
	L"WM_00D6",
	L"WM_00D7",
	L"WM_00D8",
	L"WM_00D9",
	L"WM_00DA",
	L"WM_00DB",
	L"WM_00DC",
	L"WM_00DD",
	L"WM_00DE",
	L"WM_00DF",
	L"WM_00E0",
	L"WM_00E1",
	L"WM_00E2",
	L"WM_00E3",
	L"WM_00E4",
	L"WM_00E5",
	L"WM_00E6",
	L"WM_00E7",
	L"WM_00E8",
	L"WM_00E9",
	L"WM_00EA",
	L"WM_00EB",
	L"WM_00EC",
	L"WM_00ED",
	L"WM_00EE",
	L"WM_00EF",
	L"WM_00F0",
	L"WM_00F1",
	L"WM_00F2",
	L"WM_00F3",
	L"WM_00F4",
	L"WM_00F5",
	L"WM_00F6",
	L"WM_00F7",
	L"WM_00F8",
	L"WM_00F9",
	L"WM_00FA",
	L"WM_00FB",
	L"WM_00FC",
	L"WM_00FD",
	L"WM_INPUT_DEVICE_CHANGE",
	L"WM_INPUT",
	L"WM_KEYDOWN", 
	L"WM_KEYUP",
	L"WM_CHAR",
	L"WM_DEADCHAR",
	L"WM_SYSKEYDOWN",
	L"WM_SYSKEYUP",
	L"WM_SYSCHAR",
	L"WM_SYSDEADCHAR",
	L"WM_0108",
	L"WM_UNICHAR",
  L"WM_010A",
  L"WM_010B",
	L"WM_010C",
	L"WM_IME_STARTCOMPOSITION",
	L"WM_IME_ENDCOMPOSITION",
	L"WM_IME_COMPOSITION",
	L"WM_IME_KEYLAST",
	L"WM_INITDIALOG",
	L"WM_COMMAND",
	L"WM_SYSCOMMAND",
	L"WM_TIMER",
	L"WM_HSCROLL",
	L"WM_VSCROLL",
	L"WM_INITMENU",
	L"WM_INITMENUPOPUP",
	L"WM_GESTURE",
	L"WM_GESTURENOTIFY",
	L"WM_011B",
	L"WM_011C",
	L"WM_011D",
	L"WM_011E",
	L"WM_MENUSELECT",
	L"WM_MENUCHAR",
	L"WM_ENTERIDLE",
	L"WM_MENURBUTTONUP",
	L"WM_MENUDRAG",
	L"WM_MENUGETOBJECT",
	L"WM_UNINITMENUPOPUP",
	L"WM_MENUCOMMAND",
	L"WM_CHANGEUISTATE",
	L"WM_UPDATEUISTATE",
	L"WM_QUERYUISTATE",
	L"WM_012A",
	L"WM_012B",
	L"WM_012C",
	L"WM_012D",
	L"WM_012E",
	L"WM_012F",
	L"WM_0130",
	L"WM_0131",
	L"WM_CTLCOLORMSGBOX",
	L"WM_CTLCOLOREDIT",
	L"WM_CTLCOLORLISTBOX",
	L"WM_CTLCOLORBTN",
	L"WM_CTLCOLORDLG",
	L"WM_CTLCOLORSCROLLBAR",
	L"WM_CTLCOLORSTATIC",
	L"WM_0139",
	L"WM_013A",
	L"WM_013B",
	L"WM_013C",
	L"WM_013D",
	L"WM_013E",
	L"WM_013F",
	L"WM_0140",
	L"WM_0141",
	L"WM_0142",
	L"WM_0143",
	L"WM_0144",
	L"WM_0145",
	L"WM_0146",
	L"WM_0147",
	L"WM_0148",
	L"WM_0149",
	L"WM_014A",
	L"WM_014B",
	L"WM_014C",
	L"WM_014D",
	L"WM_014E",
	L"WM_014F",
	L"WM_0150",
	L"WM_0151",
	L"WM_0152",
	L"WM_0153",
	L"WM_0154",
	L"WM_0155",
	L"WM_0156",
	L"WM_0157",
	L"WM_0158",
	L"WM_0159",
	L"WM_015A",
	L"WM_015B",
	L"WM_015C",
	L"WM_015D",
	L"WM_015E",
	L"WM_015F",
	L"WM_0160",
	L"WM_0161",
	L"WM_0162",
	L"WM_0163",
	L"WM_0164",
	L"WM_0165",
	L"WM_0166",
	L"WM_0167",
	L"WM_0168",
	L"WM_0169",
	L"WM_016A",
	L"WM_016B",
	L"WM_016C",
	L"WM_016D",
	L"WM_016E",
	L"WM_016F",
	L"WM_0170",
	L"WM_0171",
	L"WM_0172",
	L"WM_0173",
	L"WM_0174",
	L"WM_0175",
	L"WM_0176",
	L"WM_0177",
	L"WM_0178",
	L"WM_0179",
	L"WM_017A",
	L"WM_017B",
	L"WM_017C",
	L"WM_017D",
	L"WM_017E",
	L"WM_017F",
	L"WM_0180",
	L"WM_0181",
	L"WM_0182",
	L"WM_0183",
	L"WM_0184",
	L"WM_0185",
	L"WM_0186",
	L"WM_0187",
	L"WM_0188",
	L"WM_0189",
	L"WM_018A",
	L"WM_018B",
	L"WM_018C",
	L"WM_018D",
	L"WM_018E",
	L"WM_018F",
	L"WM_0190",
	L"WM_0191",
	L"WM_0192",
	L"WM_0193",
	L"WM_0194",
	L"WM_0195",
	L"WM_0196",
	L"WM_0197",
	L"WM_0198",
	L"WM_0199",
	L"WM_019A",
	L"WM_019B",
	L"WM_019C",
	L"WM_019D",
	L"WM_019E",
	L"WM_019F",
	L"WM_01A0",
	L"WM_01A1",
	L"WM_01A2",
	L"WM_01A3",
	L"WM_01A4",
	L"WM_01A5",
	L"WM_01A6",
	L"WM_01A7",
	L"WM_01A8",
	L"WM_01A9",
	L"WM_01AA",
	L"WM_01AB",
	L"WM_01AC",
	L"WM_01AD",
	L"WM_01AE",
	L"WM_01AF",
	L"WM_01B0",
	L"WM_01B1",
	L"WM_01B2",
	L"WM_01B3",
	L"WM_01B4",
	L"WM_01B5",
	L"WM_01B6",
	L"WM_01B7",
	L"WM_01B8",
	L"WM_01B9",
	L"WM_01BA",
	L"WM_01BB",
	L"WM_01BC",
	L"WM_01BD",
	L"WM_01BE",
	L"WM_01BF",
	L"WM_01C0",
	L"WM_01C1",
	L"WM_01C2",
	L"WM_01C3",
	L"WM_01C4",
	L"WM_01C5",
	L"WM_01C6",
	L"WM_01C7",
	L"WM_01C8",
	L"WM_01C9",
	L"WM_01CA",
	L"WM_01CB",
	L"WM_01CC",
	L"WM_01CD",
	L"WM_01CE",
	L"WM_01CF",
	L"WM_01D0",
	L"WM_01D1",
	L"WM_01D2",
	L"WM_01D3",
	L"WM_01D4",
	L"WM_01D5",
	L"WM_01D6",
	L"WM_01D7",
	L"WM_01D8",
	L"WM_01D9",
	L"WM_01DA",
	L"WM_01DB",
	L"WM_01DC",
	L"WM_01DD",
	L"WM_01DE",
	L"WM_01DF",
	L"WM_01E0",
	L"WM_01E1",
	L"WM_01E2",
	L"WM_01E3",
	L"WM_01E4",
	L"WM_01E5",
	L"WM_01E6",
	L"WM_01E7",
	L"WM_01E8",
	L"WM_01E9",
	L"WM_01EA",
	L"WM_01EB",
	L"WM_01EC",
	L"WM_01ED",
	L"WM_01EE",
	L"WM_01EF",
	L"WM_01F0",
	L"WM_01F1",
	L"WM_01F2",
	L"WM_01F3",
	L"WM_01F4",
	L"WM_01F5",
	L"WM_01F6",
	L"WM_01F7",
	L"WM_01F8",
	L"WM_01F9",
	L"WM_01FA",
	L"WM_01FB",
	L"WM_01FC",
	L"WM_01FD",
	L"WM_01FE",
	L"WM_01FF",
	L"WM_MOUSEMOVE",
	L"WM_MOUSEFIRST",
	L"WM_LBUTTONDOWN",
	L"WM_LBUTTONUP",
	L"WM_LBUTTONDBLCLK",
	L"WM_RBUTTONDOWN",
	L"WM_RBUTTONUP",
	L"WM_RBUTTONDBLCLK",
	L"WM_MBUTTONDOWN",
	L"WM_MBUTTONUP",
	L"WM_MBUTTONDBLCLK",
	L"WM_MOUSEWHEEL",
	L"WM_XBUTTONDOWN",
	L"WM_XBUTTONUP",
	L"WM_XBUTTONDBLCLK",
	L"WM_MOUSELAST",
	L"WM_MOUSEHWHEEL",
	L"WM_PARENTNOTIFY",
	L"WM_ENTERMENULOOP",
	L"WM_EXITMENULOOP",
	L"WM_NEXTMENU",
	L"WM_SIZING",
	L"WM_CAPTURECHANGED",
	L"WM_MOVING",
	L"WM_POWERBROADCAST",
	L"WM_DEVICECHANGE",
	L"WM_021A",
	L"WM_021B",
	L"WM_021C",
	L"WM_021D",
	L"WM_021E",
	L"WM_021F",
	L"WM_MDICREATE",
	L"WM_MDIDESTROY",
	L"WM_MDIACTIVATE",
	L"WM_MDIRESTORE",
	L"WM_MDINEXT",
	L"WM_MDIMAXIMIZE",
	L"WM_MDITILE",
	L"WM_MDICASCADE",
	L"WM_MDIICONARRANGE",
	L"WM_MDIGETACTIVE",
	L"WM_022A",
	L"WM_022B",
	L"WM_022C",
	L"WM_022D",
	L"WM_022E",
	L"WM_022F",
	L"WM_MDISETMENU",
	L"WM_ENTERSIZEMOVE",
	L"WM_EXITSIZEMOVE",
	L"WM_DROPFILES",
	L"WM_MDIREFRESHMENU",
	L"WM_0235",
	L"WM_0236",
	L"WM_0237",
	L"WM_0238",
	L"WM_0239",
	L"WM_023A",
	L"WM_023B",
	L"WM_023C",
	L"WM_023D",
	L"WM_023E",
	L"WM_023F",
	L"WM_TOUCH",
	L"WM_0241",
	L"WM_0242",
	L"WM_0243",
	L"WM_0244",
	L"WM_0245",
	L"WM_0246",
	L"WM_0247",
	L"WM_0248",
	L"WM_0249",
	L"WM_024A",
	L"WM_024B",
	L"WM_024C",
	L"WM_024D",
	L"WM_024E",
	L"WM_024F",
	L"WM_0250",
	L"WM_0251",
	L"WM_0252",
	L"WM_0253",
	L"WM_0254",
	L"WM_0255",
	L"WM_0256",
	L"WM_0257",
	L"WM_0258",
	L"WM_0259",
	L"WM_025A",
	L"WM_025B",
	L"WM_025C",
	L"WM_025D",
	L"WM_025E",
	L"WM_025F",
	L"WM_0260",
	L"WM_0261",
	L"WM_0262",
	L"WM_0263",
	L"WM_0264",
	L"WM_0265",
	L"WM_0266",
	L"WM_0267",
	L"WM_0268",
	L"WM_0269",
	L"WM_026A",
	L"WM_026B",
	L"WM_026C",
	L"WM_026D",
	L"WM_026E",
	L"WM_026F",
	L"WM_0270",
	L"WM_0271",
	L"WM_0272",
	L"WM_0273",
	L"WM_0274",
	L"WM_0275",
	L"WM_0276",
	L"WM_0277",
	L"WM_0278",
	L"WM_0279",
	L"WM_027A",
	L"WM_027B",
	L"WM_027C",
	L"WM_027D",
	L"WM_027E",
	L"WM_027F",
	L"WM_0280",
	L"WM_IME_SETCONTEXT",
	L"WM_IME_NOTIFY",
	L"WM_IME_CONTROL",
	L"WM_IME_COMPOSITIONFULL",
	L"WM_IME_SELECT",
	L"WM_IME_CHAR",
	L"WM_0287",
	L"WM_IME_REQUEST",
	L"WM_0289",
	L"WM_028A",
	L"WM_028B",
	L"WM_028C",
	L"WM_028D",
	L"WM_028E",
	L"WM_028F",
	L"WM_IME_KEYDOWN",
	L"WM_IME_KEYUP",
	L"WM_0292",
	L"WM_0293",
	L"WM_0294",
	L"WM_0295",
	L"WM_0296",
	L"WM_0297",
	L"WM_0298",
	L"WM_0299",
	L"WM_029A",
	L"WM_029B",
	L"WM_029C",
	L"WM_029D",
	L"WM_029E",
	L"WM_029F",
	L"WM_NCMOUSEHOVER",
	L"WM_MOUSEHOVER",
	L"WM_NCMOUSELEAVE",
	L"WM_MOUSELEAVE",
	L"WM_02A4",
	L"WM_02A5",
	L"WM_02A6",
	L"WM_02A7",
	L"WM_02A8",
	L"WM_02A9",
	L"WM_02AA",
	L"WM_02AB",
	L"WM_02AC",
	L"WM_02AD",
	L"WM_02AE",
	L"WM_02AF",
	L"WM_02B0",
	L"WM_WTSSESSION_CHANGE",
	L"WM_02B2",
	L"WM_02B3",
	L"WM_02B4",
	L"WM_02B5",
	L"WM_02B6",
	L"WM_02B7",
	L"WM_02B8",
	L"WM_02B9",
	L"WM_02BA",
	L"WM_02BB",
	L"WM_02BC",
	L"WM_02BD",
	L"WM_02BE",
	L"WM_02BF",
	L"WM_TABLET_DEFBASE",
	L"WM_TABLET_FIRST",
	L"WM_02C2",
	L"WM_02C3",
	L"WM_02C4",
	L"WM_02C5",
	L"WM_02C6",
	L"WM_02C7",
	L"WM_TABLET_ADDED",
	L"WM_TABLET_DELETED",
	L"WM_02CA",
	L"WM_TABLET_FLICK",
	L"WM_TABLET_QUERYSYSTEMGESTURESTATUS",
	L"WM_02CD",
	L"WM_02CE",
	L"WM_02CF",
	L"WM_02D0",
	L"WM_02D1",
	L"WM_02D2",
	L"WM_02D3",
	L"WM_02D4",
	L"WM_02D5",
	L"WM_02D6",
	L"WM_02D7",
	L"WM_02D8",
	L"WM_02D9",
	L"WM_02DA",
	L"WM_02DB",
	L"WM_02DC",
	L"WM_02DD",
	L"WM_02DE",
	L"WM_TABLET_LAST",
	L"WM_DPICHANGED",
	L"WM_02E1",
	L"WM_02E2",
	L"WM_02E3",
	L"WM_02E4",
	L"WM_02E5",
	L"WM_02E6",
	L"WM_02E7",
	L"WM_02E8",
	L"WM_02E9",
	L"WM_02EA",
	L"WM_02EB",
	L"WM_02EC",
	L"WM_02ED",
	L"WM_02EE",
	L"WM_02EF",
	L"WM_02F0",
	L"WM_02F1",
	L"WM_02F2",
	L"WM_02F3",
	L"WM_02F4",
	L"WM_02F5",
	L"WM_02F6",
	L"WM_02F7",
	L"WM_02F8",
	L"WM_02F9",
	L"WM_02FA",
	L"WM_02FB",
	L"WM_02FC",
	L"WM_02FD",
	L"WM_02FE",
	L"WM_02FF",
	L"WM_CUT",
	L"WM_COPY",
	L"WM_PASTE",
	L"WM_CLEAR",
	L"WM_UNDO",
	L"WM_RENDERFORMAT",
	L"WM_RENDERALLFORMATS",
	L"WM_DESTROYCLIPBOARD",
	L"WM_DRAWCLIPBOARD",
	L"WM_PAINTCLIPBOARD",
	L"WM_VSCROLLCLIPBOARD",
	L"WM_SIZECLIPBOARD",
	L"WM_ASKCBFORMATNAME",
	L"WM_CHANGECBCHAIN",
	L"WM_HSCROLLCLIPBOARD",
	L"WM_QUERYNEWPALETTE",
	L"WM_PALETTEISCHANGING",
	L"WM_PALETTECHANGED",
	L"WM_HOTKEY",
	L"WM_0313",
	L"WM_0314",
	L"WM_0315",
	L"WM_0316",
	L"WM_PRINT",
	L"WM_PRINTCLIENT",
	L"WM_APPCOMMAND",
	L"WM_THEMECHANGED",
	L"WM_031B",
	L"WM_031C",
	L"WM_CLIPBOARDUPDATE",
	L"WM_DWMCOMPOSITIONCHANGED",
	L"WM_DWMNCRENDERINGCHANGED",
	L"WM_DWMCOLORIZATIONCOLORCHANGED",
	L"WM_DWMWINDOWMAXIMIZEDCHANGE",
	L"WM_0322",
	L"WM_DWMSENDICONICTHUMBNAIL",
	L"WM_0324",
	L"WM_0325",
	L"WM_DWMSENDICONICLIVEPREVIEWBITMAP",
	L"WM_0327",
	L"WM_0328",
	L"WM_0329",
	L"WM_032A",
	L"WM_032B",
	L"WM_032C",
	L"WM_032D",
	L"WM_032E",
	L"WM_032F",
	L"WM_0330",
	L"WM_0331",
	L"WM_0332",
	L"WM_0333",
	L"WM_0334",
	L"WM_0335",
	L"WM_0336",
	L"WM_0337",
	L"WM_0338",
	L"WM_0339",
	L"WM_033A",
	L"WM_033B",
	L"WM_033C",
	L"WM_033D",
	L"WM_033E",
	L"WM_GETTITLEBARINFOEX",
	L"WM_0340",
	L"WM_0341",
	L"WM_0342",
	L"WM_0343",
	L"WM_0344",
	L"WM_0345",
	L"WM_0346",
	L"WM_0347",
	L"WM_0348",
	L"WM_0349",
	L"WM_034A",
	L"WM_034B",
	L"WM_034C",
	L"WM_034D",
	L"WM_034E",
	L"WM_034F",
	L"WM_0350",
	L"WM_0351",
	L"WM_0352",
	L"WM_0353",
	L"WM_0354",
	L"WM_0355",
	L"WM_0356",
	L"WM_0357",
	L"WM_HANDHELDFIRST",
	L"WM_0359",
	L"WM_035A",
	L"WM_035B",
	L"WM_035C",
	L"WM_035D",
	L"WM_035E",
	L"WM_HANDHELDLAST",
	L"WM_0360",
	L"WM_0361",
	L"WM_0362",
	L"WM_0363",
	L"WM_0364",
	L"WM_0365",
	L"WM_0366",
	L"WM_0367",
	L"WM_0368",
	L"WM_0369",
	L"WM_036A",
	L"WM_036B",
	L"WM_036C",
	L"WM_036D",
	L"WM_036E",
	L"WM_036F",
	L"WM_0370",
	L"WM_0371",
	L"WM_0372",
	L"WM_0373",
	L"WM_0374",
	L"WM_0375",
	L"WM_0376",
	L"WM_0377",
	L"WM_0378",
	L"WM_0379",
	L"WM_037A",
	L"WM_037B",
	L"WM_037C",
	L"WM_037D",
	L"WM_037E",
	L"WM_037F",
	L"WM_PENWINFIRST",
	L"WM_0381",
	L"WM_0382",
	L"WM_0383",
	L"WM_0384",
	L"WM_0385",
	L"WM_0386",
	L"WM_0387",
	L"WM_0388",
	L"WM_0389",
	L"WM_038A",
	L"WM_038B",
	L"WM_038C",
	L"WM_038D",
	L"WM_038E",
	L"WM_PENWINLAST",
	L"WM_COALESCE_FIRST",
	L"WM_0391",
	L"WM_0392",
	L"WM_0393",
	L"WM_0394",
	L"WM_0395",
	L"WM_0396",
	L"WM_0397",
	L"WM_0398",
	L"WM_0399",
	L"WM_039A",
	L"WM_039B",
	L"WM_039C",
	L"WM_039D",
	L"WM_039E",
	L"WM_COALESCE_LAST",
	L"WM_03A0",
	L"WM_03A1",
	L"WM_03A2",
	L"WM_03A3",
	L"WM_03A4",
	L"WM_03A5",
	L"WM_03A6",
	L"WM_03A7",
	L"WM_03A8",
	L"WM_03A9",
	L"WM_03AA",
	L"WM_03AB",
	L"WM_03AC",
	L"WM_03AD",
	L"WM_03AE",
	L"WM_03AF",
	L"WM_03B0",
	L"WM_03B1",
	L"WM_03B2",
	L"WM_03B3",
	L"WM_03B4",
	L"WM_03B5",
	L"WM_03B6",
	L"WM_03B7",
	L"WM_03B8",
	L"WM_03B9",
	L"WM_03BA",
	L"WM_03BB",
	L"WM_03BC",
	L"WM_03BD",
	L"WM_03BE",
	L"WM_03BF",
	L"WM_03C0",
	L"WM_03C1",
	L"WM_03C2",
	L"WM_03C3",
	L"WM_03C4",
	L"WM_03C5",
	L"WM_03C6",
	L"WM_03C7",
	L"WM_03C8",
	L"WM_03C9",
	L"WM_03CA",
	L"WM_03CB",
	L"WM_03CC",
	L"WM_03CD",
	L"WM_03CE",
	L"WM_03CF",
	L"WM_03D0",
	L"WM_03D1",
	L"WM_03D2",
	L"WM_03D3",
	L"WM_03D4",
	L"WM_03D5",
	L"WM_03D6",
	L"WM_03D7",
	L"WM_03D8",
	L"WM_03D9",
	L"WM_03DA",
	L"WM_03DB",
	L"WM_03DC",
	L"WM_03DD",
	L"WM_03DE",
	L"WM_03DF",
	L"WM_DDE_INITIATE",
	L"WM_DDE_FIRST",
	L"WM_DDE_TERMINATE",
	L"WM_DDE_ADVISE",
	L"WM_DDE_UNADVISE",
	L"WM_DDE_ACK",
	L"WM_DDE_DATA",
	L"WM_DDE_REQUEST",
	L"WM_DDE_POKE",
	L"WM_DDE_LAST",
	L"WM_DDE_EXECUTE"
};
