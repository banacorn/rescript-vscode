// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var $$Array = require("bs-platform/lib/js/array.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Vscode = require("vscode");
var Js_dict = require("bs-platform/lib/js/js_dict.js");
var $$Promise = require("reason-promise/src/js/promise.bs.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");

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

var StringOr = {
  string: string,
  others: others,
  classify: classify
};

function onMessage(callback) {
  window.addEventListener("message", (function (_event) {
          return Curry._1(callback, _event.data);
        }));
  
}

var Api = {
  onMessage: onMessage
};

var Disposable = {};

var Memento = {};

var ExtensionContext = {};

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

function makeChange(authority, fragment, path, prompt, scheme, param) {
  return {
          authority: authority,
          fragment: fragment,
          path: path,
          prompt: prompt,
          scheme: scheme
        };
}

var Uri = {
  makeChange: makeChange
};

var Clipboard = {};

var UIKind = {};

var Env = {};

function toEnum(param) {
  switch (param) {
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

function fromEnum(param) {
  switch (param) {
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
  toEnum: toEnum,
  fromEnum: fromEnum
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
  return Belt_Option.map(self.viewColumn, fromEnum);
}

function reveal(self, viewColumn, preserveFocus, param) {
  var viewColumn$1 = Belt_Option.map(viewColumn, toEnum);
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

function toEnum$1(param) {
  if (param) {
    return 1;
  } else {
    return 2;
  }
}

function fromEnum$1(param) {
  if (param !== 2) {
    return /* LF */1;
  } else {
    return /* CRLF */0;
  }
}

var EndOfLine = {
  toEnum: toEnum$1,
  fromEnum: fromEnum$1
};

function eol(self) {
  return fromEnum$1(self.eol);
}

var TextDocument = {
  eol: eol
};

function toEnum$2(param) {
  switch (param) {
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

function fromEnum$2(param) {
  switch (param) {
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
  toEnum: toEnum$2,
  fromEnum: fromEnum$2
};

function toEnum$3(param) {
  return param;
}

function fromEnum$3(param) {
  if (param !== 0) {
    if (param !== 1) {
      return /* Relative */2;
    } else {
      return /* On */1;
    }
  } else {
    return /* Off */0;
  }
}

var TextEditorLineNumbersStyle = {
  toEnum: toEnum$3,
  fromEnum: fromEnum$3
};

function cursorStyle(self) {
  return Belt_Option.map(self.cursorStyle, fromEnum$2);
}

function lineNumbers(self) {
  return Belt_Option.map(self.lineNumbers, fromEnum$3);
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

function toEnum$4(param) {
  switch (param) {
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

function fromEnum$4(param) {
  switch (param) {
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
  toEnum: toEnum$4,
  fromEnum: fromEnum$4
};

var TextEditorDecorationType = {};

var MarkdownString = {};

var ThemableDecorationAttachmentRenderOptions = {};

var ThemableDecorationInstanceRenderOptions = {};

var DecorationInstanceRenderOptions = {};

var DecorationOptions = {};

function viewColumn$1(self) {
  return Belt_Option.map(self.viewColumn, fromEnum);
}

function revealRange(self, range, option) {
  self.revealRange(range, Belt_Option.map(option, toEnum$4));
  
}

function show(self, viewColumn) {
  self.show(Belt_Option.map(viewColumn, toEnum));
  
}

var TextEditor = {
  viewColumn: viewColumn$1,
  revealRange: revealRange,
  show: show
};

var Terminal = {};

var WindowState = {};

var TextEditorOptionsChangeEvent = {};

function toEnum$5(param) {
  return param + 1 | 0;
}

function fromEnum$5(param) {
  if (param !== 1) {
    if (param !== 2) {
      return /* Command */2;
    } else {
      return /* Mouse */1;
    }
  } else {
    return /* Keyboard */0;
  }
}

var TextEditorSelectionChangeKind = {
  toEnum: toEnum$5,
  fromEnum: fromEnum$5
};

function kind(self) {
  var n = self.kind;
  if (n !== undefined) {
    return fromEnum$5(n);
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

var StatusBarItem = {};

var TerminalOptions = {};

var ExtensionTerminalOptions = {};

function toEnum$6(param) {
  switch (param) {
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

function fromEnum$6(param) {
  switch (param) {
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
  toEnum: toEnum$6,
  fromEnum: fromEnum$6
};

function toEnum$7(param) {
  return param;
}

function fromEnum$7(param) {
  if (param > 3 || param < 0) {
    return /* OpenOpen */0;
  } else {
    return param;
  }
}

var DecorationRangeBehavior = {
  toEnum: toEnum$7,
  fromEnum: fromEnum$7
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

var ViewColumnAndPreserveFocus = {};

var TreeDataProvider = {};

var UriHandler = {};

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

var WorkspaceFolder = {};

var ProgressOptions = {};

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

function toEnum$8(param) {
  return param + 1 | 0;
}

function fromEnum$8(param) {
  if (param !== 1) {
    if (param !== 2) {
      return /* HighContrast */2;
    } else {
      return /* Dark */1;
    }
  } else {
    return /* Light */0;
  }
}

var ColorThemeKind = {
  toEnum: toEnum$8,
  fromEnum: fromEnum$8
};

function kind$1(self) {
  return fromEnum$8(self.kind);
}

var ColorTheme = {
  kind: kind$1
};

var CustomDocumentOpenContext = {};

var CustomReadonlyEditorProvider = {};

var CustomTextEditorProvider = {};

var $$Window = {};

var FileSystem = {};

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

function toEnum$9(param) {
  switch (param) {
    case /* AfterDelay */0 :
        return 2;
    case /* FocusOut */1 :
        return 3;
    case /* Manual */2 :
        return 1;
    
  }
}

function fromEnum$9(param) {
  if (param !== 2) {
    if (param !== 3) {
      return /* Manual */2;
    } else {
      return /* FocusOut */1;
    }
  } else {
    return /* AfterDelay */0;
  }
}

var TextDocumentSaveReason = {
  toEnum: toEnum$9,
  fromEnum: fromEnum$9
};

function reason(self) {
  return fromEnum$9(self.reason);
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

var $$Event = {};

var Workspace = {};

function toEnum$10(param) {
  if (param) {
    return 2;
  } else {
    return 1;
  }
}

function fromEnum$10(param) {
  if (param !== 1) {
    return /* Workspace */1;
  } else {
    return /* UI */0;
  }
}

var ExtensionKind = {
  toEnum: toEnum$10,
  fromEnum: fromEnum$10
};

function extensionKind(self) {
  return fromEnum$10(self.extensionKind);
}

var Extension = {
  extensionKind: extensionKind
};

var Extensions = {};

var DiagnosticChangeEvent = {};

var $$Location = {};

var LocationLink = {};

var DiagnosticRelatedInformation = {};

function toEnum$11(param) {
  return param;
}

function fromEnum$11(param) {
  if (param > 2 || param < 0) {
    return /* Hint */3;
  } else {
    return param;
  }
}

var DiagnosticSeverity = {
  toEnum: toEnum$11,
  fromEnum: fromEnum$11
};

function toEnum$12(param) {
  if (param) {
    return 2;
  } else {
    return 1;
  }
}

function fromEnum$12(param) {
  if (param !== 1) {
    return /* Deprecated */1;
  } else {
    return /* Unnecessary */0;
  }
}

var DiagnosticTag = {
  toEnum: toEnum$12,
  fromEnum: fromEnum$12
};

var Diagnostic = {};

var DocumentFilter = {};

var DocumentSelector = {};

var DiagnosticCollection = {};

function map(x, f) {
  return Belt_Option.map(x, (function (result) {
                return $$Promise.map(result, f);
              }));
}

var ProviderResult = {
  map: map
};

var CallHierarchyItem = {};

var CallHierarchyOutgoingCall = {};

var CallHierarchyIncomingCall = {};

var CallHierarchyProvider = {};

var CodeActionProvider = {};

var CodeActionProviderMetadata = {};

var Command = {};

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

function classify$1(v) {
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
  classify: classify$1
};

var DefinitionProvider = {};

var SemanticsTokens = {};

var SemanticTokensLegend = {};

var SemanticTokensBuilder = {};

var DocumentSemanticTokensProvider = {};

var Languages = {};

exports.ThemeColor = ThemeColor;
exports.StringOr = StringOr;
exports.Api = Api;
exports.Disposable = Disposable;
exports.Memento = Memento;
exports.ExtensionContext = ExtensionContext;
exports.Commands = Commands;
exports.Uri = Uri;
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
exports.StatusBarItem = StatusBarItem;
exports.TerminalOptions = TerminalOptions;
exports.ExtensionTerminalOptions = ExtensionTerminalOptions;
exports.OverviewRulerLane = OverviewRulerLane;
exports.DecorationRangeBehavior = DecorationRangeBehavior;
exports.DecorationRenderOptions = DecorationRenderOptions;
exports.TreeViewOptions = TreeViewOptions;
exports.TreeView = TreeView;
exports.WebviewAndWebviewPanelOptions = WebviewAndWebviewPanelOptions;
exports.ViewColumnAndPreserveFocus = ViewColumnAndPreserveFocus;
exports.TreeDataProvider = TreeDataProvider;
exports.UriHandler = UriHandler;
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
exports.WorkspaceFolder = WorkspaceFolder;
exports.ProgressOptions = ProgressOptions;
exports.Progress = Progress;
exports.TextDocumentShowOptions = TextDocumentShowOptions;
exports.ColorThemeKind = ColorThemeKind;
exports.ColorTheme = ColorTheme;
exports.CustomDocumentOpenContext = CustomDocumentOpenContext;
exports.CustomReadonlyEditorProvider = CustomReadonlyEditorProvider;
exports.CustomTextEditorProvider = CustomTextEditorProvider;
exports.$$Window = $$Window;
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
exports.$$Event = $$Event;
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
exports.ProviderResult = ProviderResult;
exports.CallHierarchyItem = CallHierarchyItem;
exports.CallHierarchyOutgoingCall = CallHierarchyOutgoingCall;
exports.CallHierarchyIncomingCall = CallHierarchyIncomingCall;
exports.CallHierarchyProvider = CallHierarchyProvider;
exports.CodeActionProvider = CodeActionProvider;
exports.CodeActionProviderMetadata = CodeActionProviderMetadata;
exports.Command = Command;
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
