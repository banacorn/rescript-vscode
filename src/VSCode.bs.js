// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var $$Array = require("rescript/lib/js/array.js");
var Curry = require("rescript/lib/js/curry.js");
var Vscode = require("vscode");
var Js_dict = require("rescript/lib/js/js_dict.js");
var $$Promise = require("reason-promise/src/js/promise.bs.js");
var Js_array = require("rescript/lib/js/js_array.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");

function map(x, f) {
  return Belt_Option.map(x, (function (result) {
                return $$Promise.map(result, f);
              }));
}

var ProviderResult = {
  map: map
};

var ThemeColor = {};

function string(v) {
  return v;
}

function others(v) {
  return v;
}

function classify(v) {
  if (typeof v === "string") {
    return {
            TAG: 0,
            _0: v,
            [Symbol.for("name")]: "String"
          };
  } else {
    return {
            TAG: 1,
            _0: v,
            [Symbol.for("name")]: "Others"
          };
  }
}

function map$1(f, xs) {
  var s = classify(xs);
  if (s.TAG === /* String */0) {
    return s._0;
  } else {
    return Curry._1(f, s._0);
  }
}

var StringOr = {
  string: string,
  others: others,
  classify: classify,
  map: map$1
};

function array(v) {
  return v;
}

function single(v) {
  return v;
}

function classify$1(v) {
  if (Array.isArray(v)) {
    return {
            TAG: 0,
            _0: v,
            [Symbol.for("name")]: "Array"
          };
  } else {
    return {
            TAG: 1,
            _0: v,
            [Symbol.for("name")]: "Single"
          };
  }
}

function map$2(f, xs) {
  var s = classify$1(xs);
  if (s.TAG === /* Array */0) {
    return Js_array.map(f, s._0);
  } else {
    return Curry._1(f, s._0);
  }
}

var ArrayOr = {
  array: array,
  single: single,
  classify: classify$1,
  map: map$2
};

function onMessage(callback) {
  var onMessage$1 = (callback => window.addEventListener('message', event => callback(event.data)));
  onMessage$1(callback);
}

var Api = {
  onMessage: onMessage
};

var Disposable = {};

var $$Event = {};

var Memento = {};

var Uri = {};

function toEnum(x) {
  return x + 1 | 0;
}

function fromEnum(x) {
  if (x !== 1) {
    if (x !== 2) {
      return /* Prepend */2;
    } else {
      return /* Append */1;
    }
  } else {
    return /* Replace */0;
  }
}

var EnvironmentVariableMutatorType = {
  toEnum: toEnum,
  fromEnum: fromEnum
};

var Command = {};

function type_(self) {
  return fromEnum(self.type);
}

var EnvironmentVariableMutator = {
  type_: type_
};

var EnvironmentVariableCollection = {};

function toEnum$1(x) {
  return x + 1 | 0;
}

function fromEnum$1(x) {
  if (x !== 1) {
    if (x !== 2) {
      return /* Test */2;
    } else {
      return /* Development */1;
    }
  } else {
    return /* Production */0;
  }
}

var ExtensionMode = {
  toEnum: toEnum$1,
  fromEnum: fromEnum$1
};

function extensionMode(self) {
  return fromEnum$1(self.extensionMode);
}

var ExtensionContext = {
  extensionMode: extensionMode
};

var simple = {};

function sized(v) {
  return v;
}

var Layout = {
  simple: simple,
  sized: sized
};

var Commands = {
  Layout: Layout
};

var DebugConsole = {};

var DebugConfiguration = {};

var WorkspaceFolder = {};

var Breakpoint = {};

var DebugProtocolBreakpoint = {};

var DebugSession = {};

var BreakpointsChangeEvent = {};

var DebugSessionCustomEvent = {};

var DebugProtocolSource = {};

var DebugAdapterExecutableOptions = {};

var DebugAdapterExecutable = {};

var DebugAdapterDescriptorFactory = {};

var Debug = {};

var Clipboard = {};

function toEnum$2(x) {
  if (x) {
    return 2;
  } else {
    return 1;
  }
}

function fromEnum$2(x) {
  if (x !== 1) {
    return /* Web */1;
  } else {
    return /* Desktop */0;
  }
}

var UIKind = {
  toEnum: toEnum$2,
  fromEnum: fromEnum$2
};

function uiKind(param) {
  return fromEnum$2(Vscode.env.uiKind);
}

var Env = {
  uiKind: uiKind
};

function toEnum$3(x) {
  switch (x) {
    case /* Active */0 :
        return -1;
    case /* Beside */1 :
        return -2;
    case /* Eight */2 :
        return 8;
    case /* Five */3 :
        return 5;
    case /* Four */4 :
        return 4;
    case /* Nine */5 :
        return 9;
    case /* One */6 :
        return 1;
    case /* Seven */7 :
        return 7;
    case /* Six */8 :
        return 6;
    case /* Three */9 :
        return 3;
    case /* Two */10 :
        return 2;
    
  }
}

function fromEnum$3(x) {
  switch (x) {
    case -2 :
        return /* Beside */1;
    case -1 :
        return /* Active */0;
    case 1 :
        return /* One */6;
    case 0 :
    case 2 :
        return /* Two */10;
    case 3 :
        return /* Three */9;
    case 4 :
        return /* Four */4;
    case 5 :
        return /* Five */3;
    case 6 :
        return /* Six */8;
    case 7 :
        return /* Seven */7;
    case 8 :
        return /* Eight */2;
    case 9 :
        return /* Nine */5;
    default:
      return /* Two */10;
  }
}

var ViewColumn = {
  toEnum: toEnum$3,
  fromEnum: fromEnum$3
};

var WebviewOptions = {};

var Webview = {};

var OnDidChangeViewStateEvent = {};

var Options = {};

function iconPath(self) {
  return Belt_Option.map(self.iconPath, (function ($$case) {
                if (Belt_Option.isSome(Js_dict.get($$case, "dark"))) {
                  return {
                          TAG: 1,
                          _0: $$case,
                          [Symbol.for("name")]: "LightAndDark"
                        };
                } else {
                  return {
                          TAG: 0,
                          _0: $$case,
                          [Symbol.for("name")]: "Uri"
                        };
                }
              }));
}

function viewColumn(self) {
  return Belt_Option.map(self.viewColumn, fromEnum$3);
}

function reveal(self, viewColumn, preserveFocus, param) {
  var viewColumn$1 = Belt_Option.map(viewColumn, toEnum$3);
  if (viewColumn$1 !== undefined) {
    if (preserveFocus !== undefined) {
      self.reveal(viewColumn$1, preserveFocus);
    } else {
      self.reveal(viewColumn$1, undefined);
    }
  } else if (preserveFocus !== undefined) {
    self.reveal(undefined, preserveFocus);
  } else {
    self.reveal(undefined, undefined);
  }
}

var WebviewPanel = {
  OnDidChangeViewStateEvent: OnDidChangeViewStateEvent,
  Options: Options,
  iconPath: iconPath,
  viewColumn: viewColumn,
  reveal: reveal
};

var Position = {};

var $$Range = {};

var TextLine = {};

function toEnum$4(x) {
  if (x) {
    return 1;
  } else {
    return 2;
  }
}

function fromEnum$4(x) {
  if (x !== 2) {
    return /* LF */1;
  } else {
    return /* CRLF */0;
  }
}

var EndOfLine = {
  toEnum: toEnum$4,
  fromEnum: fromEnum$4
};

function eol(self) {
  return fromEnum$4(self.eol);
}

var TextDocument = {
  eol: eol
};

function toEnum$5(x) {
  switch (x) {
    case /* Block */0 :
        return 2;
    case /* BlockOutline */1 :
        return 5;
    case /* Line */2 :
        return 1;
    case /* LineThing */3 :
        return 4;
    case /* Underline */4 :
        return 3;
    case /* UnderlineThin */5 :
        return 6;
    
  }
}

function fromEnum$5(x) {
  switch (x) {
    case 1 :
        return /* Line */2;
    case 2 :
        return /* Block */0;
    case 3 :
        return /* Underline */4;
    case 4 :
        return /* LineThing */3;
    case 5 :
        return /* BlockOutline */1;
    default:
      return /* UnderlineThin */5;
  }
}

var TextEditorCursorStyle = {
  toEnum: toEnum$5,
  fromEnum: fromEnum$5
};

function toEnum$6(x) {
  return x;
}

function fromEnum$6(x) {
  if (x !== 0) {
    if (x !== 1) {
      return /* Relative */2;
    } else {
      return /* On */1;
    }
  } else {
    return /* Off */0;
  }
}

var TextEditorLineNumbersStyle = {
  toEnum: toEnum$6,
  fromEnum: fromEnum$6
};

function cursorStyle(self) {
  return Belt_Option.map(self.cursorStyle, fromEnum$5);
}

function lineNumbers(self) {
  return Belt_Option.map(self.lineNumbers, fromEnum$6);
}

var TextEditorOptions = {
  cursorStyle: cursorStyle,
  lineNumbers: lineNumbers
};

var $$Selection = {};

function setEndOfLine(self, eol) {
  self.setEndOfLine(eol ? 1 : 2);
}

var TextEditorEdit = {
  setEndOfLine: setEndOfLine
};

var SnippetString = {};

function toEnum$7(x) {
  switch (x) {
    case /* AtTop */0 :
        return 3;
    case /* Default */1 :
        return 0;
    case /* InCenter */2 :
        return 1;
    case /* InCenterIfOutsideViewport */3 :
        return 2;
    
  }
}

function fromEnum$7(x) {
  switch (x) {
    case 1 :
        return /* InCenter */2;
    case 2 :
        return /* InCenterIfOutsideViewport */3;
    case 3 :
        return /* AtTop */0;
    default:
      return /* Default */1;
  }
}

var TextEditorRevealType = {
  toEnum: toEnum$7,
  fromEnum: fromEnum$7
};

var TextEditorDecorationType = {};

var MarkdownString = {};

var ThemableDecorationAttachmentRenderOptions = {};

var ThemableDecorationInstanceRenderOptions = {};

var DecorationInstanceRenderOptions = {};

var DecorationOptions = {};

function viewColumn$1(self) {
  return Belt_Option.map(self.viewColumn, fromEnum$3);
}

function revealRange(self, range, option) {
  self.revealRange(range, Belt_Option.map(option, toEnum$7));
}

function show(self, viewColumn) {
  self.show(Belt_Option.map(viewColumn, toEnum$3));
}

var TextEditor = {
  viewColumn: viewColumn$1,
  revealRange: revealRange,
  show: show
};

var TerminalOptions = {};

var Pseudoterminal = {};

var ExtensionTerminalOptions = {};

function terminalOptions(v) {
  return v;
}

function extensionTerminalOptions(v) {
  return v;
}

function classify$2(v) {
  if ((v.hasOwnProperty('pty'))) {
    return {
            TAG: 1,
            _0: v,
            [Symbol.for("name")]: "ExtensionTerminalOptions"
          };
  } else {
    return {
            TAG: 0,
            _0: v,
            [Symbol.for("name")]: "TerminalOptions"
          };
  }
}

var TerminalOptionsOrExtensionTerminalOptions = {
  terminalOptions: terminalOptions,
  extensionTerminalOptions: extensionTerminalOptions,
  classify: classify$2
};

var Terminal = {};

var WindowState = {};

var TextEditorOptionsChangeEvent = {};

function toEnum$8(x) {
  return x + 1 | 0;
}

function fromEnum$8(x) {
  if (x !== 1) {
    if (x !== 2) {
      return /* Command */2;
    } else {
      return /* Mouse */1;
    }
  } else {
    return /* Keyboard */0;
  }
}

var TextEditorSelectionChangeKind = {
  toEnum: toEnum$8,
  fromEnum: fromEnum$8
};

function kind(self) {
  var n = self.kind;
  if (n !== undefined) {
    return fromEnum$8(n);
  }
  
}

var TextEditorSelectionChangeEvent = {
  kind: kind
};

var TextEditorViewColumnChangeEvent = {};

var TextEditorVisibleRangesChangeEvent = {};

var InputBox = {};

var OutputChannel = {};

var QuickPickItem = {};

var QuickPick = {};

var AccessibilityInformation = {};

function toEnum$9(x) {
  if (x) {
    return 2;
  } else {
    return 1;
  }
}

function fromEnum$9(x) {
  if (x !== 1) {
    return /* Right */1;
  } else {
    return /* Left */0;
  }
}

var StatusBarAlignment = {
  toEnum: toEnum$9,
  fromEnum: fromEnum$9
};

function alignment(self) {
  return fromEnum$9(self.alignment);
}

function setAlignment(self, alignment) {
  self.alignment = alignment ? 2 : 1;
}

var StatusBarItem = {
  alignment: alignment,
  setAlignment: setAlignment
};

function toEnum$10(x) {
  switch (x) {
    case /* Left */0 :
        return 1;
    case /* Center */1 :
        return 2;
    case /* Right */2 :
        return 4;
    case /* Full */3 :
        return 7;
    
  }
}

function fromEnum$10(x) {
  switch (x) {
    case 1 :
        return /* Left */0;
    case 2 :
        return /* Center */1;
    case 3 :
        return /* Full */3;
    case 4 :
        return /* Right */2;
    default:
      return /* Full */3;
  }
}

var OverviewRulerLane = {
  toEnum: toEnum$10,
  fromEnum: fromEnum$10
};

function toEnum$11(x) {
  return x;
}

function fromEnum$11(x) {
  if (x > 3 || x < 0) {
    return /* OpenOpen */0;
  } else {
    return x;
  }
}

var DecorationRangeBehavior = {
  toEnum: toEnum$11,
  fromEnum: fromEnum$11
};

var DecorationRenderOptions = {};

var TreeViewOptions = {};

var TreeView = {};

function make(enableCommandUris, enableScripts, localResourceRoots, portMapping, enableFindWidget, retainContextWhenHidden, param) {
  return {
          enableCommandUris: enableCommandUris,
          enableScripts: enableScripts,
          localResourceRoots: localResourceRoots,
          portMapping: portMapping,
          enableFindWidget: enableFindWidget,
          retainContextWhenHidden: retainContextWhenHidden
        };
}

var WebviewAndWebviewPanelOptions = {
  make: make
};

var TreeDataProvider = {};

var WebviewPanelSerializer = {};

var MessageOptions = {};

var MessageItem = {};

var InputBoxOptions = {};

var CancellationToken = {};

var CancellationTokenSource = {};

var OpenDialogOptions = {};

var QuickPickOptions = {};

var SaveDialogOptions = {};

var WorkspaceFolderPickOptions = {};

function encode(x) {
  if (typeof x !== "number") {
    return {
            viewId: x._0
          };
  }
  switch (x) {
    case /* Notification */0 :
        return 15;
    case /* SourceControl */1 :
        return 1;
    case /* Window */2 :
        return 10;
    
  }
}

function classify$3(v) {
  if (typeof v === "int") {
    if (v !== 1) {
      if (v !== 15) {
        return /* Window */2;
      } else {
        return /* Notification */0;
      }
    } else {
      return /* SourceControl */1;
    }
  } else {
    return {
            _0: v,
            [Symbol.for("name")]: "ViewId"
          };
  }
}

var ProcessLocationOrViewId = {
  encode: encode,
  classify: classify$3
};

var ProgressOptions = {
  ProcessLocationOrViewId: ProcessLocationOrViewId
};

var Progress = {};

function make$1(preserveFocus, preview, selection, viewColumn, param) {
  return {
          preserveFocus: preserveFocus,
          preview: preview,
          selection: selection,
          viewColumn: viewColumn
        };
}

var TextDocumentShowOptions = {
  make: make$1
};

function toEnum$12(x) {
  return x + 1 | 0;
}

function fromEnum$12(x) {
  if (x !== 1) {
    if (x !== 2) {
      return /* HighContrast */2;
    } else {
      return /* Dark */1;
    }
  } else {
    return /* Light */0;
  }
}

var ColorThemeKind = {
  toEnum: toEnum$12,
  fromEnum: fromEnum$12
};

function kind$1(self) {
  return fromEnum$12(self.kind);
}

var ColorTheme = {
  kind: kind$1
};

var CustomDocumentOpenContext = {};

var CustomReadonlyEditorProvider = {};

var CustomTextEditorProvider = {};

var CustomEditorProvider = {};

var TerminalLinkProvider = {};

var WebviewView = {};

var WebviewViewResolveContext = {};

var WebviewViewProvider = {};

var UriHandler = {};

var FileDecoration = {};

var FileDecorationProvider = {};

var $$Window = {};

function toEnum$13(x) {
  switch (x) {
    case /* Unknown */0 :
        return 0;
    case /* File */1 :
        return 1;
    case /* Directory */2 :
        return 2;
    case /* SymbolicLink */3 :
        return 64;
    
  }
}

function fromEnum$13(x) {
  if (x >= 3) {
    if (x !== 64) {
      return /* Unknown */0;
    } else {
      return /* SymbolicLink */3;
    }
  } else if (x >= 0) {
    return x;
  } else {
    return /* Unknown */0;
  }
}

var FileType = {
  toEnum: toEnum$13,
  fromEnum: fromEnum$13
};

function type_$1(self) {
  return fromEnum$13(self.type);
}

var FileStat = {
  type_: type_$1
};

function readDirectory(self, uri) {
  return $$Promise.map(self.readDirectory(uri), (function (xs) {
                return Belt_Array.map(xs, (function (param) {
                              return map$1(fromEnum$13, param);
                            }));
              }));
}

var FileSystem = {
  readDirectory: readDirectory
};

var ConfigurationChangeEvent = {};

var TextDocumentContentChangeEvent = {};

var TextDocumentChangeEvent = {};

var WorkspaceFoldersChangeEvent = {};

var FileCreateEvent = {};

var FileDeleteEvent = {};

var FileRenameEvent = {};

var WorkspaceEditEntryMetadata = {};

function setEndOfLine$1(eol) {
  return Vscode.TextEdit.setEndOfLine(eol ? 1 : 2);
}

var TextEdit = {
  setEndOfLine: setEndOfLine$1
};

function entries(self) {
  return $$Array.map((function (shit) {
                var toUri = (function (shit) { return shit[0] });
                var toTextEdits = (function (shit) { return shit[1] });
                return [
                        toUri(shit),
                        toTextEdits(shit)
                      ];
              }), self.entries());
}

var WorkspaceEdit = {
  entries: entries
};

var FileWillCreateEvent = {};

var FileWillDeleteEvent = {};

var FileWillRenameEvent = {};

function toEnum$14(x) {
  switch (x) {
    case /* AfterDelay */0 :
        return 2;
    case /* FocusOut */1 :
        return 3;
    case /* Manual */2 :
        return 1;
    
  }
}

function fromEnum$14(x) {
  if (x !== 2) {
    if (x !== 3) {
      return /* Manual */2;
    } else {
      return /* FocusOut */1;
    }
  } else {
    return /* AfterDelay */0;
  }
}

var TextDocumentSaveReason = {
  toEnum: toEnum$14,
  fromEnum: fromEnum$14
};

function reason(self) {
  return fromEnum$14(self.reason);
}

var TextDocumentWillSaveEvent = {
  reason: reason
};

var RelativePattern = {};

var GlobPattern = {};

var FileSystemWatcher = {};

var WorkspaceConfiguration = {};

var TextDocumentContentProvider = {};

var TaskProvider = {};

var FileSystemProvider = {};

var Workspace = {};

function toEnum$15(x) {
  if (x) {
    return 2;
  } else {
    return 1;
  }
}

function fromEnum$15(x) {
  if (x !== 1) {
    return /* Workspace */1;
  } else {
    return /* UI */0;
  }
}

var ExtensionKind = {
  toEnum: toEnum$15,
  fromEnum: fromEnum$15
};

function extensionKind(self) {
  return fromEnum$15(self.extensionKind);
}

var Extension = {
  extensionKind: extensionKind
};

var Extensions = {};

var DiagnosticChangeEvent = {};

var $$Location = {};

var LocationLink = {};

var DiagnosticRelatedInformation = {};

function toEnum$16(x) {
  return x;
}

function fromEnum$16(x) {
  if (x > 2 || x < 0) {
    return /* Hint */3;
  } else {
    return x;
  }
}

var DiagnosticSeverity = {
  toEnum: toEnum$16,
  fromEnum: fromEnum$16
};

function toEnum$17(x) {
  if (x) {
    return 2;
  } else {
    return 1;
  }
}

function fromEnum$17(x) {
  if (x !== 1) {
    return /* Deprecated */1;
  } else {
    return /* Unnecessary */0;
  }
}

var DiagnosticTag = {
  toEnum: toEnum$17,
  fromEnum: fromEnum$17
};

var Diagnostic = {};

var DocumentFilter = {};

var DocumentSelector = {};

var DiagnosticCollection = {};

var CallHierarchyItem = {};

var CallHierarchyOutgoingCall = {};

var CallHierarchyIncomingCall = {};

var CallHierarchyProvider = {};

var CodeActionProvider = {};

var CodeActionProviderMetadata = {};

var CodeLens = {};

var CodeLensProvider = {};

var DocumentColorProvider = {};

var CompletionItemProvider = {};

var DeclarationProvider = {};

var Hover = {};

var HoverProvider = {};

function locations(v) {
  return v;
}

function locationLinks(v) {
  return v;
}

function classify$4(v) {
  if ((function (a) { return a.targetRange === undefined})(v)) {
    return {
            TAG: 0,
            _0: v,
            [Symbol.for("name")]: "Location"
          };
  } else {
    return {
            TAG: 1,
            _0: v,
            [Symbol.for("name")]: "LocationLink"
          };
  }
}

var LocationLinkOrLocation = {
  locations: locations,
  locationLinks: locationLinks,
  classify: classify$4
};

var DefinitionProvider = {};

var SemanticsTokens = {};

var SemanticTokensLegend = {};

var SemanticTokensBuilder = {};

var DocumentSemanticTokensProvider = {};

var Languages = {};

exports.ProviderResult = ProviderResult;
exports.ThemeColor = ThemeColor;
exports.StringOr = StringOr;
exports.ArrayOr = ArrayOr;
exports.Api = Api;
exports.Disposable = Disposable;
exports.$$Event = $$Event;
exports.Memento = Memento;
exports.Uri = Uri;
exports.EnvironmentVariableMutatorType = EnvironmentVariableMutatorType;
exports.Command = Command;
exports.EnvironmentVariableMutator = EnvironmentVariableMutator;
exports.EnvironmentVariableCollection = EnvironmentVariableCollection;
exports.ExtensionMode = ExtensionMode;
exports.ExtensionContext = ExtensionContext;
exports.Commands = Commands;
exports.DebugConsole = DebugConsole;
exports.DebugConfiguration = DebugConfiguration;
exports.WorkspaceFolder = WorkspaceFolder;
exports.Breakpoint = Breakpoint;
exports.DebugProtocolBreakpoint = DebugProtocolBreakpoint;
exports.DebugSession = DebugSession;
exports.BreakpointsChangeEvent = BreakpointsChangeEvent;
exports.DebugSessionCustomEvent = DebugSessionCustomEvent;
exports.DebugProtocolSource = DebugProtocolSource;
exports.DebugAdapterExecutableOptions = DebugAdapterExecutableOptions;
exports.DebugAdapterExecutable = DebugAdapterExecutable;
exports.DebugAdapterDescriptorFactory = DebugAdapterDescriptorFactory;
exports.Debug = Debug;
exports.Clipboard = Clipboard;
exports.UIKind = UIKind;
exports.Env = Env;
exports.ViewColumn = ViewColumn;
exports.WebviewOptions = WebviewOptions;
exports.Webview = Webview;
exports.WebviewPanel = WebviewPanel;
exports.Position = Position;
exports.$$Range = $$Range;
exports.TextLine = TextLine;
exports.EndOfLine = EndOfLine;
exports.TextDocument = TextDocument;
exports.TextEditorCursorStyle = TextEditorCursorStyle;
exports.TextEditorLineNumbersStyle = TextEditorLineNumbersStyle;
exports.TextEditorOptions = TextEditorOptions;
exports.$$Selection = $$Selection;
exports.TextEditorEdit = TextEditorEdit;
exports.SnippetString = SnippetString;
exports.TextEditorRevealType = TextEditorRevealType;
exports.TextEditorDecorationType = TextEditorDecorationType;
exports.MarkdownString = MarkdownString;
exports.ThemableDecorationAttachmentRenderOptions = ThemableDecorationAttachmentRenderOptions;
exports.ThemableDecorationInstanceRenderOptions = ThemableDecorationInstanceRenderOptions;
exports.DecorationInstanceRenderOptions = DecorationInstanceRenderOptions;
exports.DecorationOptions = DecorationOptions;
exports.TextEditor = TextEditor;
exports.TerminalOptions = TerminalOptions;
exports.Pseudoterminal = Pseudoterminal;
exports.ExtensionTerminalOptions = ExtensionTerminalOptions;
exports.TerminalOptionsOrExtensionTerminalOptions = TerminalOptionsOrExtensionTerminalOptions;
exports.Terminal = Terminal;
exports.WindowState = WindowState;
exports.TextEditorOptionsChangeEvent = TextEditorOptionsChangeEvent;
exports.TextEditorSelectionChangeKind = TextEditorSelectionChangeKind;
exports.TextEditorSelectionChangeEvent = TextEditorSelectionChangeEvent;
exports.TextEditorViewColumnChangeEvent = TextEditorViewColumnChangeEvent;
exports.TextEditorVisibleRangesChangeEvent = TextEditorVisibleRangesChangeEvent;
exports.InputBox = InputBox;
exports.OutputChannel = OutputChannel;
exports.QuickPickItem = QuickPickItem;
exports.QuickPick = QuickPick;
exports.AccessibilityInformation = AccessibilityInformation;
exports.StatusBarAlignment = StatusBarAlignment;
exports.StatusBarItem = StatusBarItem;
exports.OverviewRulerLane = OverviewRulerLane;
exports.DecorationRangeBehavior = DecorationRangeBehavior;
exports.DecorationRenderOptions = DecorationRenderOptions;
exports.TreeViewOptions = TreeViewOptions;
exports.TreeView = TreeView;
exports.WebviewAndWebviewPanelOptions = WebviewAndWebviewPanelOptions;
exports.TreeDataProvider = TreeDataProvider;
exports.WebviewPanelSerializer = WebviewPanelSerializer;
exports.MessageOptions = MessageOptions;
exports.MessageItem = MessageItem;
exports.InputBoxOptions = InputBoxOptions;
exports.CancellationToken = CancellationToken;
exports.CancellationTokenSource = CancellationTokenSource;
exports.OpenDialogOptions = OpenDialogOptions;
exports.QuickPickOptions = QuickPickOptions;
exports.SaveDialogOptions = SaveDialogOptions;
exports.WorkspaceFolderPickOptions = WorkspaceFolderPickOptions;
exports.ProgressOptions = ProgressOptions;
exports.Progress = Progress;
exports.TextDocumentShowOptions = TextDocumentShowOptions;
exports.ColorThemeKind = ColorThemeKind;
exports.ColorTheme = ColorTheme;
exports.CustomDocumentOpenContext = CustomDocumentOpenContext;
exports.CustomReadonlyEditorProvider = CustomReadonlyEditorProvider;
exports.CustomTextEditorProvider = CustomTextEditorProvider;
exports.CustomEditorProvider = CustomEditorProvider;
exports.TerminalLinkProvider = TerminalLinkProvider;
exports.WebviewView = WebviewView;
exports.WebviewViewResolveContext = WebviewViewResolveContext;
exports.WebviewViewProvider = WebviewViewProvider;
exports.UriHandler = UriHandler;
exports.FileDecoration = FileDecoration;
exports.FileDecorationProvider = FileDecorationProvider;
exports.$$Window = $$Window;
exports.FileType = FileType;
exports.FileStat = FileStat;
exports.FileSystem = FileSystem;
exports.ConfigurationChangeEvent = ConfigurationChangeEvent;
exports.TextDocumentContentChangeEvent = TextDocumentContentChangeEvent;
exports.TextDocumentChangeEvent = TextDocumentChangeEvent;
exports.WorkspaceFoldersChangeEvent = WorkspaceFoldersChangeEvent;
exports.FileCreateEvent = FileCreateEvent;
exports.FileDeleteEvent = FileDeleteEvent;
exports.FileRenameEvent = FileRenameEvent;
exports.WorkspaceEditEntryMetadata = WorkspaceEditEntryMetadata;
exports.TextEdit = TextEdit;
exports.WorkspaceEdit = WorkspaceEdit;
exports.FileWillCreateEvent = FileWillCreateEvent;
exports.FileWillDeleteEvent = FileWillDeleteEvent;
exports.FileWillRenameEvent = FileWillRenameEvent;
exports.TextDocumentSaveReason = TextDocumentSaveReason;
exports.TextDocumentWillSaveEvent = TextDocumentWillSaveEvent;
exports.RelativePattern = RelativePattern;
exports.GlobPattern = GlobPattern;
exports.FileSystemWatcher = FileSystemWatcher;
exports.WorkspaceConfiguration = WorkspaceConfiguration;
exports.TextDocumentContentProvider = TextDocumentContentProvider;
exports.TaskProvider = TaskProvider;
exports.FileSystemProvider = FileSystemProvider;
exports.Workspace = Workspace;
exports.ExtensionKind = ExtensionKind;
exports.Extension = Extension;
exports.Extensions = Extensions;
exports.DiagnosticChangeEvent = DiagnosticChangeEvent;
exports.$$Location = $$Location;
exports.LocationLink = LocationLink;
exports.DiagnosticRelatedInformation = DiagnosticRelatedInformation;
exports.DiagnosticSeverity = DiagnosticSeverity;
exports.DiagnosticTag = DiagnosticTag;
exports.Diagnostic = Diagnostic;
exports.DocumentFilter = DocumentFilter;
exports.DocumentSelector = DocumentSelector;
exports.DiagnosticCollection = DiagnosticCollection;
exports.CallHierarchyItem = CallHierarchyItem;
exports.CallHierarchyOutgoingCall = CallHierarchyOutgoingCall;
exports.CallHierarchyIncomingCall = CallHierarchyIncomingCall;
exports.CallHierarchyProvider = CallHierarchyProvider;
exports.CodeActionProvider = CodeActionProvider;
exports.CodeActionProviderMetadata = CodeActionProviderMetadata;
exports.CodeLens = CodeLens;
exports.CodeLensProvider = CodeLensProvider;
exports.DocumentColorProvider = DocumentColorProvider;
exports.CompletionItemProvider = CompletionItemProvider;
exports.DeclarationProvider = DeclarationProvider;
exports.Hover = Hover;
exports.HoverProvider = HoverProvider;
exports.LocationLinkOrLocation = LocationLinkOrLocation;
exports.DefinitionProvider = DefinitionProvider;
exports.SemanticsTokens = SemanticsTokens;
exports.SemanticTokensLegend = SemanticTokensLegend;
exports.SemanticTokensBuilder = SemanticTokensBuilder;
exports.DocumentSemanticTokensProvider = DocumentSemanticTokensProvider;
exports.Languages = Languages;
/* vscode Not a pure module */
