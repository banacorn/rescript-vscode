// https://code.visualstudio.com/api/references/vscode-api#ProviderResult
// 1.52.0
module ProviderResult = {
  type t<'a> = option<Promise.t<'a>>
  let map = (x, f) => x->Belt.Option.map(result => result->Promise.map(f))
}

// https://code.visualstudio.com/api/references/vscode-api#ThemeColor
module ThemeColor = {
  type t
  // constructors
  @module("vscode") @new external make: string => t = "ThemeColor"
}

// "string | xxx", for modeling the union type of String and something else
module StringOr: {
  type t<'a>
  type case<'a> =
    | String(string)
    | Others('a)
  let string: string => t<'a>
  let others: 'a => t<'a>
  let classify: t<'a> => case<'a>

  let map: ('a => 'b, t<'a>) => t<'b>
} = {
  @unboxed
  type rec t<'a> = Any('x): t<'a>
  type case<'a> =
    | String(string)
    | Others('a)
  let string = (v: string) => Any(v)
  let others = (v: 'a) => Any(v)
  let classify = (Any(v): t<'a>): case<'a> =>
    if Js.typeof(v) == "string" {
      String((Obj.magic(v): string))
    } else {
      Others((Obj.magic(v): 'a))
    }
  let map = (f: 'a => 'b, xs: t<'a>): t<'b> =>
    switch classify(xs) {
    | String(s) => string(s)
    | Others(x) => others(f(x))
    }
}

// "A | A[]", for modeling the union type of something or array of that thing
module ArrayOr: {
  type t<'a>
  type case<'a> =
    | Array(array<'a>)
    | Single('a)
  let array: array<'a> => t<'a>
  let single: 'a => t<'a>
  let classify: t<'a> => case<'a>
  let map: ('a => 'b, t<'a>) => t<'b>
} = {
  @unboxed
  type rec t<'a> = Any('x): t<'a>
  type case<'a> =
    | Array(array<'a>)
    | Single('a)
  let array = (v: array<'a>) => Any(v)
  let single = (v: 'a) => Any(v)
  let classify = (Any(v): t<'a>): case<'a> =>
    if Js.Array.isArray(v) {
      Array((Obj.magic(v): array<'a>))
    } else {
      Single((Obj.magic(v): 'a))
    }
  let map = (f: 'a => 'b, xs: t<'a>): t<'b> =>
    switch classify(xs) {
    | Array(s) => array(Js.Array.map(f, s))
    | Single(x) => single(f(x))
    }
}

module Api = {
  type t

  @val external acquireVsCodeApi: unit => t = "acquireVsCodeApi"

  @bs.send external postMessage: (t, 'a) => unit = "postMessage"

  let onMessage = (callback: 'a => unit): unit => {
    let onMessage = %raw(
      "callback => window.addEventListener('message', event => callback(event.data))"
    )
    onMessage(callback)
  }
}

module Disposable = {
  type t
  // static
  @module("vscode") @scope("Disposable")
  external from: array<{"dispose": unit => 'a}> => t = "from"
  // constructor
  @module("vscode") @new
  external make: (unit => unit) => t = "Disposable"
  // methods
  @bs.send external dispose: t => 'a = "dispose"
}

// https://code.visualstudio.com/api/references/vscode-api#Event
module Event = {
  type t<'a> = (. 'a => unit) => Disposable.t
}

// https://code.visualstudio.com/api/references/vscode-api#Memento
module Memento = {
  type t
  // methods
  @bs.send external get: (t, string) => option<'a> = "get"
  @bs.send external getWithDefault: (t, string, 'a) => 'a = "get"
  @bs.send external update: (t, string, 'a) => Promise.t<unit> = "update"
}

// https://code.visualstudio.com/api/references/vscode-api#Uri
// 1.51.0
module Uri = {
  type t

  // static
  @module("vscode") @scope("Uri") external file: string => t = "file"
  @module("vscode") @scope("Uri") @variadic
  external joinPath: (t, array<string>) => t = "joinPath"
  @module("vscode") @scope("Uri")
  external parse: (string, option<bool>) => t = "file"

  @new
  external make: (string, string, string, string, string) => t = "Uri"

  @get external authority: t => string = "authority"
  @get external fragment: t => string = "fragment"
  @get external fsPath: t => string = "fsPath"
  @get external path: t => string = "path"
  @get external prompt: t => string = "prompt"
  @get external scheme: t => string = "scheme"

  @bs.send external toJSON: t => Js.Json.t = "toJSON"
  @bs.send external toString: t => string = "toString"
  @bs.send external toStringWithOptions: (t, bool) => string = "toString"

  @bs.send
  external with_: (
    t,
    {
      "authority": option<string>,
      "fragment": option<string>,
      "path": option<string>,
      "prompt": option<string>,
      "scheme": option<string>,
    },
  ) => t = "with"
}

// https://code.visualstudio.com/api/references/vscode-api#EnvironmentVariableMutatorType
// 1.52.0
module EnvironmentVariableMutatorType = {
  type t =
    | Replace
    | Append
    | Prepend
  let toEnum = x =>
    switch x {
    | Replace => 1
    | Append => 2
    | Prepend => 3
    }
  let fromEnum = x =>
    switch x {
    | 1 => Replace
    | 2 => Append
    | _ => Prepend
    }
}

// https://code.visualstudio.com/api/references/vscode-api#Command
// 1.55.0
module Command = {
  type t
  // properties
  @get external arguments: t => option<array<'a>> = "arguments"
  @get external command: t => string = "command"
  @get external title: t => string = "title"
  @get external tooltip: t => option<string> = "tooltip"
}
// module Command = {
//   type t<'a> = {
//     arguments: option<array<'a>>,
//     command: string,
//     title: string,
//     tooltip: option<string>,
//   }
// }

// https://code.visualstudio.com/api/references/vscode-api#EnvironmentVariableMutator
// 1.52.0
module EnvironmentVariableMutator = {
  type t
  // properties
  @get external type_raw: t => int = "type"
  let type_ = self => EnvironmentVariableMutatorType.fromEnum(type_raw(self))
  @get external value: t => bool = "value"
}

// https://code.visualstudio.com/api/references/vscode-api#EnvironmentVariableCollection
// 1.52.0
module EnvironmentVariableCollection = {
  type t
  // properties
  @get external persistent: t => bool = "persistent"
  // methods
  @bs.send external append: (t, string, string) => unit = "append"
  @bs.send external clear: t => unit = "clear"
  @bs.send external delete: (t, string) => unit = "delete"
  @bs.send
  external forEach: (t, (string, EnvironmentVariableMutator.t, t) => 'a) => unit = "forEach"
  @bs.send
  external forEachWithThisArg: (t, (string, EnvironmentVariableMutator.t, t) => 'a, 'b) => unit =
    "forEach"
  @bs.send external get: (t, string) => option<EnvironmentVariableMutator.t> = "get"
  @bs.send external prepend: (t, string, string) => unit = "prepend"
  @bs.send external replace: (t, string, string) => unit = "replace"
}

// https://code.visualstudio.com/api/references/vscode-api#ExtensionMode
// 1.52.0
module ExtensionMode = {
  type t =
    | Production
    | Development
    | Test
  let toEnum = x =>
    switch x {
    | Production => 1
    | Development => 2
    | Test => 3
    }
  let fromEnum = x =>
    switch x {
    | 1 => Production
    | 2 => Development
    | _ => Test
    }
}

// https://code.visualstudio.com/api/references/vscode-api#ExtensionContext
// 1.52.0
module ExtensionContext = {
  type t
  // properties
  @get
  external environmentVariableCollection: t => EnvironmentVariableCollection.t =
    "environmentVariableCollection"
  @get external extensionMode_raw: t => int = "extensionMode"
  let extensionMode = self => ExtensionMode.fromEnum(extensionMode_raw(self))
  @get external extensionMode_raw: t => ExtensionMode.t = "extensionMode"
  @get external extensionPath: t => string = "extensionPath"
  @get external extensionUri: t => Uri.t = "extensionUri"
  @get external globalState: t => Memento.t = "globalState"
  @get external globalStoragePath: t => string = "globalStoragePath"
  @get external logPath: t => string = "logPath"
  @get external logUri: t => Uri.t = "logUri"
  @get external storagePath: t => option<string> = "storagePath"
  @get external storageUri: t => option<Uri.t> = "storageUri"
  @get
  external subscriptions: t => array<Disposable.t> = "subscriptions"
  @get external workspaceState: t => Memento.t = "workspaceState"
  // methods
  @bs.send external asAbsolutePath: (t, string) => string = "asAbsolutePath"
}

module Commands = {
  module Layout = {
    @unboxed
    type rec group = Group('a): group

    type sized = {
      groups: array<group>,
      size: float,
    }

    let simple = Group(Js.Dict.empty())
    let sized = (v: sized) => Group(v)

    type t = {
      orientation: int,
      groups: array<group>,
    }
  }

  // methods
  @module("vscode") @scope("commands")
  external executeCommand: @string
  [
    | @as("vscode.setEditorLayout") #setEditorLayout(Layout.t)
    | @as("setContext") #setContext(string, bool)
  ] => Promise.t<'a> = "executeCommand"
  @module("vscode") @scope("commands")
  external getEditorLayout: (@as("vscode.getEditorLayout") _, unit) => Promise.t<Layout.t> = "executeCommand"
  @module("vscode") @scope("commands")
  external executeCommand0: string => Promise.t<'a> = "executeCommand"
  @module("vscode") @scope("commands")
  external executeCommand1: (string, 'arg0) => Promise.t<'a> = "executeCommand"
  @module("vscode") @scope("commands")
  external setContext: (@as("setContext") _, string, bool) => Promise.t<unit> = "executeCommand"

  @module("vscode") @scope("commands")
  external getCommands: option<bool> => Promise.t<array<string>> = "getCommands"
  @module("vscode") @scope("commands")
  external registerCommand: (string, unit => 'a) => Disposable.t = "registerCommand"
}

// https://code.visualstudio.com/api/references/vscode-api#DebugConsole
// 1.52.0
module DebugConsole = {
  type t
  // methods
  @bs.send external append: (t, string) => unit = "append"
  @bs.send external appendLine: (t, string) => unit = "appendLine"
}

// https://code.visualstudio.com/api/references/vscode-api#DebugConfiguration
// 1.52.0
module DebugConfiguration = {
  type t
  // properties
  @get external name: t => string = "name"
  @get external request: t => string = "request"
  @get external type_: t => string = "type"
}

// https://code.visualstudio.com/api/references/vscode-api#WorkspaceFolder
// 1.52.0
module WorkspaceFolder = {
  type t
  // properties
  @get external index: t => int = "index"
  @get external name: t => string = "name"
  @get external uri: t => Uri.t = "uri"
}

// https://code.visualstudio.com/api/references/vscode-api#Breakpoint
// 1.52.0
module Breakpoint = {
  type t
  // constructors
  @module("vscode") @new
  external make: (option<bool>, option<string>, option<string>, option<string>) => t = "Breakpoint"
  // properties
  @get external condition: t => option<string> = "condition"
  @get external enabled: t => bool = "enabled"
  @get external hitCondition: t => option<string> = "hitCondition"
  @get external id: t => string = "id"
  @get external logMessage: t => option<string> = "logMessage"
}

// https://code.visualstudio.com/api/references/vscode-api#DebugProtocolBreakpoint
// 1.52.0
module DebugProtocolBreakpoint = {
  // A DebugProtocolBreakpoint is an opaque stand-in type for the Breakpoint type defined in the Debug Adapter Protocol.
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#DebugSession
// 1.52.0
module DebugSession = {
  type t
  // properties
  @get external configuration: t => DebugConfiguration.t = "configuration"
  @get external id: t => string = "id"
  @get external name: t => string = "name"
  @get external type_: t => string = "type"
  @get external workspaceFolder: t => option<WorkspaceFolder.t> = "workspaceFolder"
  // methods
  @bs.send external customRequest: (t, string) => Promise.t<'a> = "customRequest"
  @bs.send external customRequestWithArgs: (t, string, 'a) => Promise.t<'b> = "customRequest"
  @bs.send
  external getDebugProtocolBreakpoint: (
    t,
    Breakpoint.t,
  ) => Promise.t<option<DebugProtocolBreakpoint.t>> = "getDebugProtocolBreakpoint"
}

// https://code.visualstudio.com/api/references/vscode-api#BreakpointsChangeEvent
// 1.52.0
module BreakpointsChangeEvent = {
  type t
  // properties
  @get external added: t => array<Breakpoint.t> = "added"
  @get external changed: t => array<Breakpoint.t> = "changed"
  @get external removed: t => array<Breakpoint.t> = "removed"
}

// https://code.visualstudio.com/api/references/vscode-api#DebugSessionCustomEvent
// 1.52.0
module DebugSessionCustomEvent = {
  type t
  // properties
  @get external body: t => option<'a> = "body"
  @get external event: t => string = "event"
  @get external session: t => DebugSession.t = "session"
}

// https://code.visualstudio.com/api/references/vscode-api#DebugProtocolSource
// 1.52.0
module DebugProtocolSource = {
  // A DebugProtocolSource is an opaque stand-in type for the Source type defined in the Debug Adapter Protocol.
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#DebugAdapterExecutableOptions
// 1.52.0
module DebugAdapterExecutableOptions = {
  type t
  // properties
  @get external cwd: t => option<string> = "cwd"
  @get external env: t => option<'a> = "env"
}

// https://code.visualstudio.com/api/references/vscode-api#DebugAdapterExecutable
// 1.52.0
module DebugAdapterExecutable = {
  type t
  // constructor
  @module("vscode") @new
  external make: (string, array<string>) => t = "DebugAdapterExecutable"
  @module("vscode") @new
  external makeWithOptions: (string, array<string>, DebugAdapterExecutableOptions.t) => t =
    "DebugAdapterExecutable"
}

// https://code.visualstudio.com/api/references/vscode-api#DebugAdapterDescriptorFactory
// 1.52.0 (WIP)
module DebugAdapterDescriptorFactory = {
  type t
  // methods
  @bs.send
  external createDebugAdapterDescriptor: (
    t,
    DebugSession.t,
    option<DebugAdapterExecutable.t>,
  ) => ProviderResult.t<'a> = "createDebugAdapterDescriptor"
}

// https://code.visualstudio.com/api/references/vscode-api#debug
// 1.52.0 (WIP)
module Debug = {
  // variables
  @module("vscode") @scope("debug")
  external activeDebugConsole: option<DebugConsole.t> = "activeDebugConsole"
  @module("vscode") @scope("debug")
  external activeDebugSession: option<DebugSession.t> = "activeDebugSession"
  @module("vscode") @scope("debug")
  external breakpoints: array<Breakpoint.t> = "breakpoints"

  // events
  @module("vscode") @scope("debug")
  external onDidChangeActiveDebugSession: Event.t<option<DebugSession.t>> =
    "onDidChangeActiveDebugSession"
  @module("vscode") @scope("debug")
  external onDidChangeBreakpoints: Event.t<BreakpointsChangeEvent.t> = "onDidChangeBreakpoints"
  @module("vscode") @scope("debug")
  external onDidReceiveDebugSessionCustomEvent: Event.t<DebugSessionCustomEvent.t> =
    "onDidReceiveDebugSessionCustomEvent"
  @module("vscode") @scope("debug")
  external onDidStartDebugSession: Event.t<DebugSession.t> = "onDidStartDebugSession"
  @module("vscode") @scope("debug")
  external onDidTerminateDebugSession: Event.t<DebugSession.t> = "onDidTerminateDebugSession"

  // functions
  @module("vscode") @scope("debug")
  external addBreakpoints: array<Breakpoint.t> => unit = "addBreakpoints"
  @module("vscode") @scope("debug")
  external asDebugSourceUri: DebugProtocolSource.t => unit = "asDebugSourceUri"
  @module("vscode") @scope("debug")
  external asDebugSourceUriWithSession: (DebugProtocolSource.t, DebugSession.t) => unit =
    "asDebugSourceUri"
  @module("vscode") @scope("debug")
  external registerDebugAdapterDescriptorFactory: (
    string,
    DebugAdapterDescriptorFactory.t,
    Disposable.t,
  ) => unit = "registerDebugAdapterDescriptorFactory"
}

// https://code.visualstudio.com/api/references/vscode-api#Clipboard
module Clipboard = {
  type t
  // methods
  @bs.send external readText: (t, unit) => Promise.t<string> = "readText"
  @bs.send external writeText: (t, string) => Promise.t<unit> = "writeText"
}

// https://code.visualstudio.com/api/references/vscode-api#UIKind;
module UIKind = {
  type t =
    | Desktop
    | Web
  let toEnum = x =>
    switch x {
    | Desktop => 1
    | Web => 2
    }
  let fromEnum = x =>
    switch x {
    | 1 => Desktop
    | _ => Web
    }
}

// https://code.visualstudio.com/api/references/vscode-api#env
// 1.52.0
module Env = {
  type t

  // variables
  @module("vscode") @scope("env") external appName: string = "appName"
  @module("vscode") @scope("env") external appRoot: string = "appRoot"
  @module("vscode") @scope("env")
  external clipboard: Clipboard.t = "clipboard"
  @module("vscode") @scope("env")
  external language: string = "language"
  @module("vscode") @scope("env")
  external machineId: string = "machineId"
  @module("vscode") @scope("env")
  external remoteName: option<string> = "remoteName"
  @module("vscode") @scope("env")
  external sessionId: string = "sessionId"
  @module("vscode") @scope("env") external shell: string = "shell"
  @module("vscode") @scope("env") external uiKind_raw: int = "uiKind"
  let uiKind: unit => UIKind.t = () => UIKind.fromEnum(uiKind_raw)
  @module("vscode") @scope("env")
  external uriScheme: string = "uriScheme"

  // functions
  @module("vscode") @scope("env")
  external asExternalUri: Uri.t => Promise.t<Uri.t> = "asExternalUri"
  @module("vscode") @scope("env")
  external openExternal: Uri.t => Promise.t<bool> = "openExternal"
}

module ViewColumn = {
  type t =
    | Active
    | Beside
    | Eight
    | Five
    | Four
    | Nine
    | One
    | Seven
    | Six
    | Three
    | Two

  let toEnum = x =>
    switch x {
    | Active => -1
    | Beside => -2
    | Eight => 8
    | Five => 5
    | Four => 4
    | Nine => 9
    | One => 1
    | Seven => 7
    | Six => 6
    | Three => 3
    | Two => 2
    }
  let fromEnum = x =>
    switch x {
    | -1 => Active
    | -2 => Beside
    | 8 => Eight
    | 5 => Five
    | 4 => Four
    | 9 => Nine
    | 1 => One
    | 7 => Seven
    | 6 => Six
    | 3 => Three
    | _ => Two
    }
}
module WebviewOptions = {
  type portMapping
  type t = {
    enableCommandUris: option<bool>,
    enableScripts: option<bool>,
    localResourceRoots: option<array<Uri.t>>,
    portMapping: option<array<portMapping>>,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#Webview
module Webview = {
  type t
  // events
  @bs.send
  external onDidReceiveMessage: (t, 'a => unit) => Disposable.t = "onDidReceiveMessage"
  // properties
  @get external cspSource: t => string = "cspSource"
  @get external html: t => string = "html"
  @set external setHtml: (t, string) => unit = "html"
  @get external options: t => WebviewOptions.t = "options"
  // methods
  @bs.send external asWebviewUri: (t, Uri.t) => Uri.t = "asWebviewUri"
  @bs.send external postMessage: (t, 'a) => Promise.t<bool> = "postMessage"
}

module WebviewPanel = {
  type t

  // https://code.visualstudio.com/api/references/vscode-api#WebviewPanelOnDidChangeViewStateEvent
  module OnDidChangeViewStateEvent = {
    type webviewPanel = t
    type t
    // properties
    @get external webviewPanel: t => webviewPanel = "webviewPanel"
  }

  // https://code.visualstudio.com/api/references/vscode-api#WebviewPanelOptions
  module Options = {
    type t
    // properties
    @get
    external enableFindWidget: t => option<bool> = "enableFindWidget"
    @get
    external retainContextWhenHidden: t => option<bool> = "retainContextWhenHidden"
  }

  // events
  @bs.send
  external onDidChangeViewState: (t, OnDidChangeViewStateEvent.t => unit) => Disposable.t =
    "onDidChangeViewState"
  @bs.send
  external onDidDispose: (t, unit => unit) => Disposable.t = "onDidDispose"

  // properties
  @get external active: t => bool = "active"
  type uriOrLightAndDark =
    | Uri(Uri.t)
    | LightAndDark({"dark": Uri.t, "light": Uri.t})

  @get
  external iconPath_raw: t => option<Js.Dict.t<Uri.t>> = "iconPath"
  let iconPath = (self): option<uriOrLightAndDark> =>
    iconPath_raw(self)->Belt.Option.map(case =>
      if Belt.Option.isSome(Js.Dict.get(case, "dark")) {
        LightAndDark((Obj.magic(case): {"dark": Uri.t, "light": Uri.t}))
      } else {
        Uri((Obj.magic(case): Uri.t))
      }
    )
  @get external options: t => Options.t = "options"
  @get external title: t => string = "title"
  @get external viewColumn_raw: t => option<int> = "viewColumn"
  let viewColumn = (self: t): option<ViewColumn.t> =>
    viewColumn_raw(self)->Belt.Option.map(ViewColumn.fromEnum)

  @get external viewType: t => string = "viewType"
  @get external visible: t => bool = "visible"
  @get external webview: t => Webview.t = "webview"
  // methods
  @bs.send external dispose: t => unit = "dispose"
  @bs.send
  external reveal_raw: (t, ~viewColumn: int=?, ~preserveFocus: bool=?, unit) => unit = "reveal"
  let reveal = (self: t, ~viewColumn=?, ~preserveFocus=?, ()): unit => {
    let viewColumn = viewColumn->Belt.Option.map(ViewColumn.toEnum)
    switch (viewColumn, preserveFocus) {
    | (None, None) => reveal_raw(self, ())
    | (None, Some(preserveFocus)) => reveal_raw(self, ~preserveFocus, ())
    | (Some(viewColumn), None) => reveal_raw(self, ~viewColumn, ())
    | (Some(viewColumn), Some(preserveFocus)) => reveal_raw(self, ~viewColumn, ~preserveFocus, ())
    }
  }
}

// https://code.visualstudio.com/api/references/vscode-api#Position
module Position = {
  type t
  // constructor
  @module("vscode") @new external make: (int, int) => t = "Position"
  // properties
  @get external character: t => int = "character"
  @get external line: t => int = "line"
  // methods
  @bs.send external compareTo: (t, t) => int = "compareTo"
  @bs.send external isAfter: (t, t) => bool = "isAfter"
  @bs.send external isAfterOrEqual: (t, t) => bool = "isAfterOrEqual"
  @bs.send external isBefore: (t, t) => bool = "isBefore"
  @bs.send external isBeforeOrEqual: (t, t) => bool = "isBeforeOrEqual"
  @bs.send external isEqual: (t, t) => bool = "isEqual"
  @bs.send external translate: (t, int, int) => t = "translate"
  @bs.send external with_: (t, int, int) => t = "with"
}

// https://code.visualstudio.com/api/references/vscode-api#Range
module Range = {
  type t
  // constructor
  @module("vscode") @new
  external make: (Position.t, Position.t) => t = "Range"
  @module("vscode") @new
  external makeWithNumbers: (int, int, int, int) => t = "Range"
  // properties
  @get external end_: t => Position.t = "end"
  @get external isEmpty: t => bool = "isEmpty"
  @get external isSingleLine: t => bool = "isSingleLine"
  @get external start: t => Position.t = "start"
  // methods
  @bs.send external contains: (t, Position.t) => bool = "contains"
  @bs.send external containsRange: (t, t) => bool = "contains"
  @bs.send external intersection: (t, t) => option<t> = "intersection"
  @bs.send external isEqual: (t, t) => bool = "isEqual"
  @bs.send external union: (t, t) => t = "union"
  @bs.send external with_: (t, Position.t, Position.t) => t = "with"
}

// https://code.visualstudio.com/api/references/vscode-api#TextLine
module TextLine = {
  type t
  // properties
  @get
  external firstNonWhitespaceCharacterIndex: t => int = "firstNonWhitespaceCharacterIndex"
  @get external isEmptyOrWhitespace: t => bool = "isEmptyOrWhitespace"
  @get external lineNumber: t => int = "lineNumber"
  @get external range: t => Range.t = "range"
  @get
  external rangeIncludingLineBreak: t => Range.t = "rangeIncludingLineBreak"
  @get external text: t => string = "text"
}

// https://code.visualstudio.com/api/references/vscode-api#EndOfLine
module EndOfLine = {
  type t =
    | CRLF
    | LF

  let toEnum = x =>
    switch x {
    | CRLF => 2
    | LF => 1
    }
  let fromEnum = x =>
    switch x {
    | 2 => CRLF
    | _ => LF
    }
}

module TextDocument = {
  type t
  // properties
  @get external eol_raw: t => int = "eol"
  let eol = (self: t): EndOfLine.t => EndOfLine.fromEnum(eol_raw(self))
  @get external fileName: t => string = "fileName"
  @get external isClosed: t => bool = "isClosed"
  @get external isDirty: t => bool = "isDirty"
  @get external isUntitled: t => bool = "isUntitled"
  @get external languageId: t => string = "languageId"
  @get external lineCount: t => int = "lineCount"
  @get external uri: t => Uri.t = "uri"
  @get external version: t => int = "version"
  // methods
  @bs.send external getText: (t, option<Range.t>) => string = "getText"
  @bs.send
  external getWordRangeAtPosition: (t, Position.t, option<Js.Re.t>) => option<Range.t> =
    "getWordRangeAtPosition"
  @bs.send external lineAt: (t, int) => TextLine.t = "lineAt"
  @bs.send external lineAtPosition: (t, Position.t) => TextLine.t = "lineAt"
  @bs.send external offsetAt: (t, Position.t) => int = "offsetAt"
  @bs.send external positionAt: (t, int) => Position.t = "positionAt"
  @bs.send external save: t => Promise.t<bool> = "save"
  @bs.send
  external validatePosition: (t, Position.t) => Position.t = "validatePosition"
  @bs.send external validateRange: (t, Range.t) => Range.t = "validateRange"
}

// https://code.visualstudio.com/api/references/vscode-api#TextEditorCursorStyle
module TextEditorCursorStyle = {
  type t =
    | Block
    | BlockOutline
    | Line
    | LineThing
    | Underline
    | UnderlineThin
  let toEnum = x =>
    switch x {
    | Block => 2
    | BlockOutline => 5
    | Line => 1
    | LineThing => 4
    | Underline => 3
    | UnderlineThin => 6
    }
  let fromEnum = x =>
    switch x {
    | 2 => Block
    | 5 => BlockOutline
    | 1 => Line
    | 4 => LineThing
    | 3 => Underline
    | _ => UnderlineThin
    }
}

// https://code.visualstudio.com/api/references/vscode-api#TextEditorLineNumbersStyle
module TextEditorLineNumbersStyle = {
  type t =
    | Off
    | On
    | Relative
  let toEnum = x =>
    switch x {
    | Off => 0
    | On => 1
    | Relative => 2
    }
  let fromEnum = x =>
    switch x {
    | 0 => Off
    | 1 => On
    | _ => Relative
    }
}

// https://code.visualstudio.com/api/references/vscode-api#TextEditorOptions
module TextEditorOptions = {
  type t
  // properties
  @get external cursorStyle_raw: t => option<int> = "cursorStyle"
  let cursorStyle = (self: t): option<TextEditorCursorStyle.t> =>
    cursorStyle_raw(self)->Belt.Option.map(TextEditorCursorStyle.fromEnum)
  @get
  external insertSpaces: t => option<StringOr.t<bool>> = "insertSpaces"
  @get external lineNumbers_raw: t => option<int> = "lineNumbers"
  let lineNumbers = (self: t): option<TextEditorLineNumbersStyle.t> =>
    lineNumbers_raw(self)->Belt.Option.map(TextEditorLineNumbersStyle.fromEnum)
  @get external tabSize: t => option<StringOr.t<int>> = "tabSize"
}

// https://code.visualstudio.com/api/references/vscode-api#Selection
module Selection = {
  type t
  // constructors
  @module("vscode") @new
  external make: (Position.t, Position.t) => t = "Selection"
  @module("vscode") @new
  external makeWithNumbers: (int, int, int, int) => t = "Selection"
  // properties
  @get external active: t => Position.t = "active"
  @get external anchor: t => Position.t = "anchor"
  @get external end_: t => Position.t = "end"
  @get external isEmpty: t => bool = "isEmpty"
  @get external isReversed: t => bool = "isReversed"
  @get external isSingleLine: t => bool = "isSingleLine"
  @get external start: t => Position.t = "start"
  // methods
  @bs.send external contains: (t, Position.t) => bool = "contains"
  @bs.send external containsRange: (t, Range.t) => bool = "contains"
  @bs.send
  external intersection: (t, Range.t) => option<Range.t> = "intersection"
  @bs.send external isEqual: (t, Range.t) => bool = "isEqual"
  @bs.send external union: (t, Range.t) => Range.t = "union"
  @bs.send external with_: (t, Position.t, Position.t) => Range.t = "with"
}

// https://code.visualstudio.com/api/references/vscode-api#TextEditorEdit
module TextEditorEdit = {
  type t
  // methods
  @bs.send external delete: (t, Range.t) => unit = "delete"
  @bs.send external deleteAtSelection: (t, Selection.t) => unit = "delete"
  @bs.send external insert: (t, Position.t, string) => unit = "insert"
  @bs.send external replace: (t, Position.t, string) => unit = "replace"
  @bs.send external replaceAtRange: (t, Range.t, string) => unit = "replace"
  @bs.send
  external replaceAtSelection: (t, Selection.t, string) => unit = "replace"
  @bs.send external setEndOfLine_raw: (t, int) => unit = "setEndOfLine"
  let setEndOfLine = (self: t, eol: EndOfLine.t): unit =>
    setEndOfLine_raw(self, EndOfLine.toEnum(eol))
}

// https://code.visualstudio.com/api/references/vscode-api#SnippetString
module SnippetString = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#TextEditorRevealType
module TextEditorRevealType = {
  type raw = int
  type t =
    | AtTop
    | Default
    | InCenter
    | InCenterIfOutsideViewport

  let toEnum = x =>
    switch x {
    | AtTop => 3
    | Default => 0
    | InCenter => 1
    | InCenterIfOutsideViewport => 2
    }
  let fromEnum = x =>
    switch x {
    | 1 => InCenter
    | 2 => InCenterIfOutsideViewport
    | 3 => AtTop
    | _ => Default
    }
}

// https://code.visualstudio.com/api/references/vscode-api#TextEditorDecorationType
module TextEditorDecorationType = {
  type t
  // properties
  @get external key: t => string = "key"
  // methods
  @bs.send external dispose: t => unit = "dispose"
}

// https://code.visualstudio.com/api/references/vscode-api#MarkedString;
module MarkdownString = {
  type t
  // constructors
  @module("vscode") @new
  external make: (string, bool) => t = "MarkdownString"
  // properties
  @get external isTrusted: t => option<bool> = "isTrusted"
  @get external value: t => string = "value"
  // methods
  @bs.send
  external appendCodeblock: (t, string, option<string>) => t = "appendCodeblock"
  @bs.send external appendMarkdown: (t, string) => t = "appendMarkdown"
  @bs.send external appendText: (t, string) => t = "appendText"
}

// https://code.visualstudio.com/api/references/vscode-api#ThemableDecorationAttachmentRenderOptions
module ThemableDecorationAttachmentRenderOptions = {
  @deriving(abstract)
  type t = {
    @optional
    backgroundColor: StringOr.t<ThemeColor.t>,
    @optional
    border: string,
    @optional
    borderColor: StringOr.t<ThemeColor.t>,
    @optional
    color: StringOr.t<ThemeColor.t>,
    @optional
    contentIconPath: StringOr.t<Uri.t>,
    @optional
    contentText: string,
    @optional
    fontStyle: string,
    @optional
    fontWeight: string,
    @optional
    height: string,
    @optional
    margin: string,
    @optional
    textDecoration: string,
    @optional
    width: string,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#ThemableDecorationInstanceRenderOptions
module ThemableDecorationInstanceRenderOptions = {
  @deriving(abstract)
  type t = {
    @optional
    after: ThemableDecorationAttachmentRenderOptions.t,
    @optional
    before: ThemableDecorationAttachmentRenderOptions.t,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#DecorationInstanceRenderOptions;
module DecorationInstanceRenderOptions = {
  @deriving(abstract)
  type t = {
    @optional
    after: ThemableDecorationAttachmentRenderOptions.t,
    @optional
    before: ThemableDecorationAttachmentRenderOptions.t,
    @optional
    dark: ThemableDecorationInstanceRenderOptions.t,
    @optional
    light: ThemableDecorationInstanceRenderOptions.t,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#DecorationOptions
module DecorationOptions = {
  type t
  // properties
  @get
  external hoverMessage: t => option<MarkdownString.t> = "hoverMessage"
  @get external range: t => Range.t = "range"
  @get
  external renderOptions: t => option<DecorationInstanceRenderOptions.t> = "renderOptions"
}

module TextEditor = {
  type t
  // properties
  @get external document: t => TextDocument.t = "document"
  @get external options: t => TextEditorOptions.t = "options"
  @get external selection: t => Selection.t = "selection"
  @set external setSelection: (t, Selection.t) => unit = "selection"
  @get external selections: t => array<Selection.t> = "selections"
  @set
  external setSelections: (t, array<Selection.t>) => unit = "selections"
  @get external viewColumn_raw: t => option<int> = "viewColumn"
  let viewColumn = (self: t): option<ViewColumn.t> =>
    viewColumn_raw(self)->Belt.Option.map(ViewColumn.fromEnum)
  @get external visibleRanges: t => array<Range.t> = "visibleRanges"
  // methods
  @bs.send
  external edit: (
    t,
    TextEditorEdit.t => unit,
    option<{"undoStopAfter": bool, "undoStopBefore": bool}>,
  ) => Promise.t<bool> = "edit"
  @bs.send external hide: (t, unit) => unit = "hide"
  @bs.send
  external insertSnippet: (
    t,
    SnippetString.t,
    Position.t,
    option<{"undoStopAfter": bool, "undoStopBefore": bool}>,
  ) => Promise.t<bool> = "insertSnippet"
  @bs.send
  external insertSnippetAtRange: (
    t,
    SnippetString.t,
    Range.t,
    option<{"undoStopAfter": bool, "undoStopBefore": bool}>,
  ) => Promise.t<bool> = "insertSnippet"
  @bs.send
  external insertSnippetAtPositions: (
    t,
    SnippetString.t,
    array<Position.t>,
    option<{"undoStopAfter": bool, "undoStopBefore": bool}>,
  ) => Promise.t<bool> = "insertSnippet"
  @bs.send
  external insertSnippetAtRanges: (
    t,
    SnippetString.t,
    array<Range.t>,
    option<{"undoStopAfter": bool, "undoStopBefore": bool}>,
  ) => Promise.t<bool> = "insertSnippet"
  @bs.send
  external revealRange_raw: (t, Range.t, option<int>) => unit = "revealRange"
  let revealRange = (self: t, range: Range.t, option: option<TextEditorRevealType.t>): unit =>
    revealRange_raw(self, range, option->Belt.Option.map(TextEditorRevealType.toEnum))
  @bs.send
  external setDecorations: (t, TextEditorDecorationType.t, array<Range.t>) => unit =
    "setDecorations"
  @bs.send
  external setDecorationsWithOptions: (
    t,
    TextEditorDecorationType.t,
    array<DecorationOptions.t>,
  ) => unit = "setDecorations"
  @bs.send external show_raw: (t, option<int>) => unit = "show"
  let show = (self: t, viewColumn: option<ViewColumn.t>): unit =>
    show_raw(self, viewColumn->Belt.Option.map(ViewColumn.toEnum))
}

// https://code.visualstudio.com/api/references/vscode-api#TerminalOptions
// 1.51.0
module TerminalOptions = {
  type t
  // properties
  @get external cwd: t => option<string> = "cwd"
  @get external env: t => option<'a> = "env"
  @get external hideFromUser: t => option<bool> = "hideFromUser"
  @get external name: t => option<string> = "name"
  @get external shellArgs: t => StringOr.t<array<string>> = "shellArgs"
  @get external shellPath: t => option<string> = "shellPath"
  @get external strictEnv: t => option<bool> = "strictEnv"
}

// https://code.visualstudio.com/api/references/vscode-api#Pseudoterminal
module Pseudoterminal = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#ExtensionTerminalOptions
// 1.51.0
module ExtensionTerminalOptions = {
  type t
  // properties
  @get external name: t => string = "name"
  @get external pty: t => Pseudoterminal.t = "pty"
}

// "TerminalOptions | ExtensionTerminalOptions", FUCK UNION TYPE
// 1.51.0
module TerminalOptionsOrExtensionTerminalOptions: {
  type t
  type case =
    | TerminalOptions(TerminalOptions.t)
    | ExtensionTerminalOptions(ExtensionTerminalOptions.t)
  let terminalOptions: TerminalOptions.t => t
  let extensionTerminalOptions: ExtensionTerminalOptions.t => t
  let classify: t => case
} = {
  @unboxed
  type rec t = Any('x): t
  type case =
    | TerminalOptions(TerminalOptions.t)
    | ExtensionTerminalOptions(ExtensionTerminalOptions.t)
  let terminalOptions = (v: TerminalOptions.t) => Any(v)
  let extensionTerminalOptions = (v: ExtensionTerminalOptions.t) => Any(v)
  let classify = (Any(v): t): case =>
    if %raw("v.hasOwnProperty('pty')") {
      ExtensionTerminalOptions((Obj.magic(v): ExtensionTerminalOptions.t))
    } else {
      TerminalOptions((Obj.magic(v): TerminalOptions.t))
    }
}

// https://code.visualstudio.com/api/references/vscode-api#Terminal
// 1.51.0
module Terminal = {
  type t
  // properties
  @get
  external creationOptions: t => TerminalOptionsOrExtensionTerminalOptions.t = "creationOptions"
  @get external exitStatus: t => option<int> = "exitStatus"
  @get external name: t => string = "name"
  @get external processId: t => option<int> = "processId"
  // methods
  @bs.send external dispose: t => unit = "dispose"
  @bs.send external hide: t => unit = "hide"
  @bs.send external sendText: (t, string) => unit = "sendText"
  @bs.send
  external sendTextWithOptions: (t, string, bool) => unit = "sendText"
  @bs.send external show: t => unit = "show"
  @bs.send external showWithOptions: (t, bool) => unit = "show"
}

// https://code.visualstudio.com/api/references/vscode-api#WindowState
// 1.51.0
module WindowState = {
  type t
  // properties
  @get external focused: t => bool = "focused"
}

// https://code.visualstudio.com/api/references/vscode-api#TextEditorOptionsChangeEvent
module TextEditorOptionsChangeEvent = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#TextEditorSelectionChangeKind
module TextEditorSelectionChangeKind = {
  type raw = int
  type t =
    | Keyboard
    | Mouse
    | Command

  let toEnum = x =>
    switch x {
    | Keyboard => 1
    | Mouse => 2
    | Command => 3
    }
  let fromEnum = x =>
    switch x {
    | 1 => Keyboard
    | 2 => Mouse
    | _ => Command
    }
}
// https://code.visualstudio.com/api/references/vscode-api#TextEditorSelectionChangeEvent
module TextEditorSelectionChangeEvent = {
  type t
  // properties
  @get
  external kind_raw: t => option<TextEditorSelectionChangeKind.raw> = "kind"
  let kind: t => option<TextEditorSelectionChangeKind.t> = self =>
    switch kind_raw(self) {
    | None => None
    | Some(n) => Some(TextEditorSelectionChangeKind.fromEnum(n))
    }
  @get external selections: t => array<Selection.t> = "selections"
  @get external textEditor: t => TextEditor.t = "textEditor"
}
// https://code.visualstudio.com/api/references/vscode-api#TextEditorViewColumnChangeEvent
module TextEditorViewColumnChangeEvent = {
  type t
}
// https://code.visualstudio.com/api/references/vscode-api#TextEditorVisibleRangesChangeEvent
module TextEditorVisibleRangesChangeEvent = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#InputBox
module InputBox = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#OutputChannel
module OutputChannel = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#QuickPickItem
module QuickPickItem = {
  type t
}
// https://code.visualstudio.com/api/references/vscode-api#QuickPick
module QuickPick = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#AccessibilityInformation
// 1.55.0
module AccessibilityInformation = {
  type t
  // properties
  @get external label: t => string = "label"
  @get external role: t => option<string> = "role"
}

// https://code.visualstudio.com/api/references/vscode-api#StatusBarAlignment
// 1.55.0
module StatusBarAlignment = {
  type raw = int
  type t =
    | Left
    | Right

  let toEnum = x =>
    switch x {
    | Left => 1
    | Right => 2
    }
  let fromEnum = x =>
    switch x {
    | 1 => Left
    | 2 => Right
    | _ => Right
    }
}

// https://code.visualstudio.com/api/references/vscode-api#StatusBarItem
module StatusBarItem = {
  type t
  // properties
  @get
  external accessibilityInformation: t => option<AccessibilityInformation.t> =
    "accessibilityInformation"
  @set
  external setAccessibilityInformation: (t, AccessibilityInformation.t) => unit =
    "accessibilityInformation"
  @get external alignment_raw: t => int = "alignment"
  let alignment: t => StatusBarAlignment.t = self =>
    StatusBarAlignment.fromEnum(self->alignment_raw)
  @set
  external setAlignment_raw: (t, int) => unit = "alignment"
  let setAlignment: (t, StatusBarAlignment.t) => unit = (self, alignment) =>
    setAlignment_raw(self, StatusBarAlignment.toEnum(alignment))
  @get external backgroundColor: t => option<ThemeColor.t> = "backgroundColor"
  @set
  external setBackgroundColor: (t, ThemeColor.t) => unit = "backgroundColor"
  @get external color: t => option<StringOr.t<ThemeColor.t>> = "color"
  @set
  external setColor: (t, ThemeColor.t) => unit = "color"
  @set
  external setColorWithString: (t, string) => unit = "color"
  @get external command: t => option<StringOr.t<Command.t>> = "command"
  @set
  external setCommand: (t, Command.t) => unit = "command"
  @set
  external setCommandWithString: (t, string) => unit = "command"
  @get external priority: t => option<int> = "priority"
  @set
  external setPriority: (t, int) => unit = "priority"
  @get external text: t => string = "text"
  @set
  external setText: (t, string) => unit = "text"
  @get external tooltip: t => option<string> = "tooltip"
  @set
  external setTooltip: (t, string) => unit = "tooltip"
  // methods
  @bs.send external dispose: t => unit = "dispose"
  @bs.send external hide: t => unit = "hide"
  @bs.send external show: t => unit = "show"
}

// https://code.visualstudio.com/api/references/vscode-api#OverviewRulerLane;
module OverviewRulerLane = {
  type raw = int
  type t =
    | Left
    | Center
    | Right
    | Full
  let toEnum = x =>
    switch x {
    | Left => 1
    | Center => 2
    | Right => 4
    | Full => 7
    }
  let fromEnum = x =>
    switch x {
    | 1 => Left
    | 2 => Center
    | 4 => Right
    | _ => Full
    }
}

// https://code.visualstudio.com/api/references/vscode-api#DecorationRangeBehavior;
module DecorationRangeBehavior = {
  type raw = int
  type t =
    | OpenOpen
    | ClosedClosed
    | OpenClosed
    | ClosedOpen
  let toEnum = x =>
    switch x {
    | OpenOpen => 0
    | ClosedClosed => 1
    | OpenClosed => 2
    | ClosedOpen => 3
    }
  let fromEnum = x =>
    switch x {
    | 0 => OpenOpen
    | 1 => ClosedClosed
    | 2 => OpenClosed
    | 3 => ClosedOpen
    | _ => OpenOpen
    }
}

// https://code.visualstudio.com/api/references/vscode-api#DecorationRenderOptions
module DecorationRenderOptions = {
  @deriving(abstract)
  type t = {
    @optional
    after: ThemableDecorationAttachmentRenderOptions.t,
    @optional
    backgroundColor: StringOr.t<ThemeColor.t>,
    @optional
    before: ThemableDecorationAttachmentRenderOptions.t,
    @optional
    border: string,
    @optional
    borderColor: StringOr.t<ThemeColor.t>,
    @optional
    borderRadius: string,
    @optional
    borderSpacing: string,
    @optional
    borderStyle: string,
    @optional
    borderWidth: string,
    @optional
    color: StringOr.t<ThemeColor.t>,
    @optional
    cursor: string,
    @optional
    dark: ThemableDecorationInstanceRenderOptions.t,
    @optional
    fontStyle: string,
    @optional
    fontWeight: string,
    @optional
    gutterIconPath: StringOr.t<Uri.t>,
    @optional
    gutterIconSize: string,
    @optional
    isWholeLine: bool,
    @optional
    letterSpacing: string,
    @optional
    light: ThemableDecorationInstanceRenderOptions.t,
    @optional
    opacity: string,
    @optional
    outline: string,
    @optional
    outlineColor: StringOr.t<ThemeColor.t>,
    @optional
    outlineStyle: string,
    @optional
    outlineWidth: string,
    @optional
    overviewRulerColor: StringOr.t<ThemeColor.t>,
    @optional
    overviewRulerLane: OverviewRulerLane.raw,
    @optional
    rangeBehavior: DecorationRangeBehavior.raw,
    @optional
    textDecoration: string,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#TreeViewOptions
module TreeViewOptions = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#TreeView
module TreeView = {
  type t
}

// WebviewPanelOptions & WebviewOptions
module WebviewAndWebviewPanelOptions = {
  type t = {
    enableCommandUris: option<bool>,
    enableScripts: option<bool>,
    localResourceRoots: option<array<Uri.t>>,
    portMapping: option<array<WebviewOptions.portMapping>>,
    enableFindWidget: option<bool>,
    retainContextWhenHidden: option<bool>,
  }

  let make = (
    ~enableCommandUris=?,
    ~enableScripts=?,
    ~localResourceRoots=?,
    ~portMapping=?,
    ~enableFindWidget=?,
    ~retainContextWhenHidden=?,
    (),
  ): t => {
    enableCommandUris: enableCommandUris,
    enableScripts: enableScripts,
    localResourceRoots: localResourceRoots,
    portMapping: portMapping,
    enableFindWidget: enableFindWidget,
    retainContextWhenHidden: retainContextWhenHidden,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#TreeDataProvider
module TreeDataProvider = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#WebviewPanelSerializer
module WebviewPanelSerializer = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#MessageOptions
module MessageOptions = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#MessageItem
module MessageItem = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#InputBoxOptions
// 1.59.0
module InputBoxOptions = {
  type t
  // properties
  @get external ignoreFocusOut: t => option<bool> = "ignoreFocusOut"
  @get external password: t => option<bool> = "password"
  @get external placeHolder: t => option<string> = "placeHolder"
  @get external prompt: t => option<string> = "prompt"
  @get external title: t => option<string> = "title"
  @get external value: t => option<string> = "value"
  @get external valueSelection: t => option<(int, int)> = "valueSelection"
  // methods
  @bs.send
  external validateInput: (t, string) => option<StringOr.t<Js.Promise.t<option<string>>>> =
    "validateInput"
}

// https://code.visualstudio.com/api/references/vscode-api#CancellationToken
module CancellationToken = {
  type t
  // properties
  @get
  external isCancellationRequested: t => bool = "isCancellationRequested"
  // methods
  @bs.send
  external onCancellationRequested: (t, 'a => unit) => Disposable.t = "onCancellationRequested"
}

// https://code.visualstudio.com/api/references/vscode-api#CancellationTokenSource
module CancellationTokenSource = {
  type t
  // properties
  @get external token: t => CancellationToken.t = "token"
  // methods
  @bs.send external cancel: t => unit = "cancel"
  @bs.send external dispose: t => unit = "dispose"
}

// https://code.visualstudio.com/api/references/vscode-api#OpenDialogOptions
module OpenDialogOptions = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#QuickPickOptions
module QuickPickOptions = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#SaveDialogOptions
module SaveDialogOptions = {
  type t
}
// https://code.visualstudio.com/api/references/vscode-api#WorkspaceFolderPickOptions
module WorkspaceFolderPickOptions = {
  type t
  // properties
  @get external ignoreFocusOut: t => option<bool> = "ignoreFocusOut"
  @get external placeHolder: t => option<string> = "placeHolder"
}

// https://code.visualstudio.com/api/references/vscode-api#ProgressOptions
// 1.5560
module ProgressOptions = {
  // "ProcessLocation | {viewId: string}"
  module ProcessLocationOrViewId: {
    type t
    type case =
      | Notification
      | SourceControl
      | Window
      | ViewId(string)
    let encode: case => t
    let classify: t => case
  } = {
    @unboxed
    type rec t = Any('x): t
    type case =
      | Notification
      | SourceControl
      | Window
      | ViewId(string)
    let encode = x =>
      switch x {
      | Notification => Any(15)
      | SourceControl => Any(1)
      | Window => Any(10)
      | ViewId(string) => Any({"viewId": string})
      }
    let classify = (Any(v): t): case =>
      if Js.typeof(v) == "int" {
        switch (Obj.magic(v): int) {
        | 15 => Notification
        | 1 => SourceControl
        | _ => Window
        }
      } else {
        ViewId(Obj.magic(v))
      }
  }

  type t
  // properties
  @get external cancellable: t => option<bool> = "cancellable"
  @get external location: t => ProcessLocationOrViewId.t = "location"
  @get external title: t => option<string> = "title"
}

// https://code.visualstudio.com/api/references/vscode-api#Progress
// 1.56.0
module Progress = {
  type t<'a>
  // methods
  @bs.send external report: (t<'a>, 'a) => unit = "report"
}

// https://code.visualstudio.com/api/references/vscode-api#TextDocumentShowOptions;
module TextDocumentShowOptions = {
  type t = {
    preserveFocus: option<bool>,
    preview: option<bool>,
    selection: option<Range.t>,
    viewColumn: option<ViewColumn.t>,
  }

  let make = (~preserveFocus=?, ~preview=?, ~selection=?, ~viewColumn=?, ()): t => {
    preserveFocus: preserveFocus,
    preview: preview,
    selection: selection,
    viewColumn: viewColumn,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#ColorThemeKind;
module ColorThemeKind = {
  type raw = int
  type t =
    | Light
    | Dark
    | HighContrast

  let toEnum = x =>
    switch x {
    | Light => 1
    | Dark => 2
    | HighContrast => 3
    }
  let fromEnum = x =>
    switch x {
    | 1 => Light
    | 2 => Dark
    | _ => HighContrast
    }
}

// https://code.visualstudio.com/api/references/vscode-api#ColorTheme
// 1.55.0
module ColorTheme = {
  type t
  // properties
  @get external kind_raw: t => int = "kind"
  let kind: t => ColorThemeKind.t = self => ColorThemeKind.fromEnum(self->kind_raw)
}

// https://code.visualstudio.com/api/references/vscode-api#CustomDocumentOpenContext
module CustomDocumentOpenContext = {
  type t
  // properties
  @get external backupId: t => option<string> = "backupId"
}

// https://code.visualstudio.com/api/references/vscode-api#CustomEditorProvider
// module CustomEditorProvider = {
//   type t('a);
//   // events
//   [@bs.send]
//   external onDidChangeCustomDocument: (t('a), Uri.t => unit) => Disposable.t =
//     "onDidChange";
//   // methods
//   [@bs.send]
//   external openCustomDocument:
//     (t('a), Uri.t, CustomDocumentOpenContext.t, CancellationToken.t) =>
//     option(Promise.t('a)) =
//     "openCustomDocument";
//   [@bs.send]
//   external resolveCustomEditor:
//     (t('a), 'a, WebviewPanel.t, CancellationToken.t) =>
//     option(Promise.t(unit)) =
//     "resolveCustomEditor";
// };

// https://code.visualstudio.com/api/references/vscode-api#CustomReadonlyEditorProvider
module CustomReadonlyEditorProvider = {
  type t<'a>
  // methods
  @bs.send
  external openCustomDocument: (
    t<'a>,
    Uri.t,
    CustomDocumentOpenContext.t,
    CancellationToken.t,
  ) => option<Promise.t<'a>> = "openCustomDocument"
  @bs.send
  external resolveCustomEditor: (
    t<'a>,
    'a,
    WebviewPanel.t,
    CancellationToken.t,
  ) => option<Promise.t<unit>> = "resolveCustomEditor"
}

// https://code.visualstudio.com/api/references/vscode-api#CustomTextEditorProvider
module CustomTextEditorProvider = {
  type t
  // methods
  @bs.send
  external resolveCustomTextEditor: (
    t,
    TextDocument.t,
    WebviewPanel.t,
    CancellationToken.t,
  ) => option<Promise.t<unit>> = "resolveCustomTextEditor"
}

// https://code.visualstudio.com/api/references/vscode-api#CustomEditorProvider
module CustomEditorProvider = {
  type t<'a>
}

// https://code.visualstudio.com/api/references/vscode-api#TerminalLinkProvider
module TerminalLinkProvider = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#WebviewViewProvider
// 1.51.0
module WebviewView = {
  type t
  // events
  @bs.send
  external onDidChangeVisibility: (t, unit => unit) => Disposable.t = "onDidChangeVisibility"
  @bs.send
  external onDidDispose: (t, unit => unit) => Disposable.t = "onDidDispose"
  // properties
  @get external description: t => option<string> = "description"
  @get external title: t => option<string> = "title"
  @get external viewType: t => string = "viewType"
  @get external visible: t => bool = "visible"
  @get external webview: t => Webview.t = "webview"
  // methods
  @bs.send external show: t => unit = "show"
  @bs.send external showWithOptions: (t, bool) => unit = "show"
}

// https://code.visualstudio.com/api/references/vscode-api#WebviewViewResolveContext
// 1.51.0
module WebviewViewResolveContext = {
  type t<'a>
  // properties
  @get external state: t<'a> => option<'a> = "state"
}

// https://code.visualstudio.com/api/references/vscode-api#WebviewViewProvider
// 1.51.0
module WebviewViewProvider = {
  type t
  // methods
  @bs.send
  external resolveWebviewView: (
    t,
    WebviewView.t,
    WebviewViewResolveContext.t<'a>,
    CancellationToken.t,
  ) => option<Promise.t<unit>> = "resolveWebviewView"
}

// https://code.visualstudio.com/api/references/vscode-api#UriHandler
// 1.51.0
module UriHandler = {
  type t
  // methods
  @bs.send
  external handleUri: (t, Uri.t) => ProviderResult.t<unit> = "handleUri"
}

// https://code.visualstudio.com/api/references/vscode-api#FileDecoration
// 1.56.0
module FileDecoration = {
  type t
  // constructors
  @module("vscode") @new
  external make: (option<string>, option<string>, option<ThemeColor.t>) => t = "FileDecoration"
  // properties
  @get external badge: t => option<string> = "badge"
  @get external color: t => option<ThemeColor.t> = "color"
  @get external propagate: t => option<bool> = "propagate"
  @get external tooltip: t => option<string> = "tooltip"
}

// https://code.visualstudio.com/api/references/vscode-api#FileDecorationProvider
// 1.56.0
module FileDecorationProvider = {
  type t
  // events
  @bs.send
  external onDidChangeFileDecorations: (t, option<ArrayOr.t<Uri.t>> => unit) => Disposable.t =
    "onDidChangeFileDecorations"
  // methods
  @bs.send
  external provideFileDecoration: (
    t,
    Uri.t,
    CancellationToken.t,
  ) => ProviderResult.t<FileDecoration.t> = "show"
}

// https://code.visualstudio.com/api/references/vscode-api#window
// 1.51.0
module Window = {
  // variables
  @module("vscode") @scope("window")
  external activeColorTheme: ColorTheme.t = "activeColorTheme"
  @module("vscode") @scope("window")
  external activeTerminal: option<Terminal.t> = "activeTerminal"
  @module("vscode") @scope("window")
  external activeTextEditor: option<TextEditor.t> = "activeTextEditor"
  @module("vscode") @scope("window")
  external state: WindowState.t = "state"
  @module("vscode") @scope("window")
  external terminals: array<Terminal.t> = "terminals"
  @module("vscode") @scope("window")
  external visibleTextEditors: array<TextEditor.t> = "visibleTextEditors"
  // events
  @module("vscode") @scope("window")
  external onDidChangeActiveColorTheme: Event.t<ColorTheme.t> = "onDidChangeActiveColorTheme"
  @module("vscode") @scope("window")
  external onDidChangeActiveTerminal: Event.t<option<Terminal.t>> = "onDidChangeActiveTerminal"
  @module("vscode") @scope("window")
  external onDidChangeActiveTextEditor: Event.t<option<TextEditor.t>> =
    "onDidChangeActiveTextEditor"
  @module("vscode") @scope("window")
  external onDidChangeTextEditorOptions: Event.t<TextEditorOptionsChangeEvent.t> =
    "onDidChangeTextEditorOptions"
  @module("vscode") @scope("window")
  external onDidChangeTextEditorSelection: Event.t<TextEditorSelectionChangeEvent.t> =
    "onDidChangeTextEditorSelection"
  @module("vscode") @scope("window")
  external onDidChangeTextEditorViewColumn: Event.t<TextEditorViewColumnChangeEvent.t> =
    "onDidChangeTextEditorViewColumn"
  @module("vscode") @scope("window")
  external onDidChangeTextEditorVisibleRanges: Event.t<TextEditorVisibleRangesChangeEvent.t> =
    "onDidChangeTextEditorVisibleRanges"
  @module("vscode") @scope("window")
  external onDidChangeVisibleTextEditors: Event.t<array<TextEditor.t>> =
    "onDidChangeVisibleTextEditors"
  @module("vscode") @scope("window")
  external onDidChangeWindowState: Event.t<WindowState.t> = "onDidChangeWindowState"
  @module("vscode") @scope("window")
  external onDidCloseTerminal: Event.t<Terminal.t> = "onDidCloseTerminal"
  @module("vscode") @scope("window")
  external onDidOpenTerminal: Event.t<Terminal.t> = "onDidOpenTerminal"

  // functions
  @module("vscode") @scope("window")
  external createInputBox: unit => InputBox.t = "createInputBox"
  @module("vscode") @scope("window")
  external createOutputChannel: string => OutputChannel.t = "createOutputChannel"
  @module("vscode") @scope("window")
  external createQuickPick: QuickPickItem.t => QuickPick.t = "createQuickPick"
  @module("vscode") @scope("window")
  external createStatusBarItem: (option<StatusBarAlignment.t>, option<int>) => StatusBarItem.t =
    "createStatusBarItem"
  @module("vscode") @scope("window")
  external createTerminal: (
    ~name: string=?,
    ~shellPath: string=?,
    ~shellArgs: array<string>=?,
    unit,
  ) => Terminal.t = "createTerminal"
  @module("vscode") @scope("window")
  external createTerminalWithTerminalOptions: TerminalOptions.t => Terminal.t = "createTerminal"
  @module("vscode") @scope("window")
  external createTerminalWithExtensionTerminalOptions: ExtensionTerminalOptions.t => Terminal.t =
    "createTerminal"
  @module("vscode") @scope("window")
  external createTextEditorDecorationType: DecorationRenderOptions.t => TextEditorDecorationType.t =
    "createTextEditorDecorationType"
  @module("vscode") @scope("window")
  external createTreeView: (string, TreeViewOptions.t) => TreeView.t = "createTreeView"
  @module("vscode") @scope("window")
  external createWebviewPanel: (
    string,
    string,
    {"preserveFocus": bool, "viewColumn": int},
    option<WebviewAndWebviewPanelOptions.t>,
  ) => WebviewPanel.t = "createWebviewPanel"
  @module("vscode") @scope("window")
  external registerCustomTextEditorProvider: (string, CustomTextEditorProvider.t) => Disposable.t =
    "registerCustomEditorProvider"
  @module("vscode") @scope("window")
  external registerCustomTextEditorProviderWithOptions: (
    string,
    CustomTextEditorProvider.t,
    {"supportsMultipleEditorsPerDocument": bool, "webviewOption": WebviewPanel.Options.t},
  ) => Disposable.t = "registerCustomEditorProvider"

  @module("vscode") @scope("window")
  external registerCustomReadonlyEditorProvider: (
    string,
    CustomReadonlyEditorProvider.t<'a>,
  ) => Disposable.t = "registerCustomEditorProvider"
  @module("vscode") @scope("window")
  external registerCustomReadonlyEditorProviderWithOptions: (
    string,
    CustomReadonlyEditorProvider.t<'a>,
    {"supportsMultipleEditorsPerDocument": bool, "webviewOption": WebviewPanel.Options.t},
  ) => Disposable.t = "registerCustomEditorProvider"

  @module("vscode") @scope("window")
  external registerCustomEditorProvider: (string, CustomEditorProvider.t<'a>) => Disposable.t =
    "registerCustomEditorProvider"
  @module("vscode") @scope("window")
  external registerCustomEditorProviderWithOptions: (
    string,
    CustomEditorProvider.t<'a>,
    {"supportsMultipleEditorsPerDocument": bool, "webviewOption": WebviewPanel.Options.t},
  ) => Disposable.t = "registerCustomEditorProvider"
  @module("vscode") @scope("window")
  external registerFileDecorationProvider: FileDecorationProvider.t => Disposable.t =
    "registerFileDecorationProvider"
  @module("vscode") @scope("window")
  external registerTerminalLinkProvider: (string, TerminalLinkProvider.t) => Disposable.t =
    "registerTerminalLinkProvider"
  @module("vscode") @scope("window")
  external registerTreeDataProvider: (string, TreeDataProvider.t) => Disposable.t =
    "registerTreeDataProvider"
  @module("vscode") @scope("window")
  external registerUriHandler: UriHandler.t => Disposable.t = "registerUriHandler"
  @module("vscode") @scope("window")
  external registerWebviewPanelSerializer: (string, WebviewPanelSerializer.t) => Disposable.t =
    "registerWebviewPanelSerializer"
  @module("vscode") @scope("window")
  external registerWebviewViewProvider: (string, WebviewViewProvider.t) => Disposable.t =
    "registerWebviewViewProvider"
  @module("vscode") @scope("window")
  external registerWebviewViewProviderWithOptions: (
    string,
    WebviewViewProvider.t,
    {"webviewOptions": {"retainContextWhenHidden": bool}},
  ) => Disposable.t = "registerWebviewViewProvider"
  @module("vscode") @scope("window")
  external setStatusBarMessageAndHideAfterTimeout: (string, int) => Disposable.t =
    "setStatusBarMessage"
  @module("vscode") @scope("window")
  external setStatusBarMessageAndHideWhenDone: (string, Promise.t<'a>) => Disposable.t =
    "setStatusBarMessage"
  @module("vscode") @scope("window")
  external setStatusBarMessage: string => Disposable.t = "setStatusBarMessage"
  @module("vscode") @scope("window") @variadic
  external showErrorMessage: (string, array<string>) => Promise.t<option<string>> =
    "showErrorMessage"
  @module("vscode") @scope("window") @variadic
  external showErrorMessageWithOptions: (
    string,
    MessageOptions.t,
    array<string>,
  ) => Promise.t<option<string>> = "showErrorMessage"
  @module("vscode") @scope("window") @variadic
  external showInformationMessage: (string, array<string>) => Promise.t<option<string>> =
    "showInformationMessage"
  @module("vscode") @scope("window") @variadic
  external showInformationMessageWithOptions: (
    string,
    MessageOptions.t,
    array<string>,
  ) => Promise.t<option<string>> = "showInformationMessage"
  @module("vscode") @scope("window")
  external showInputBox: (
    ~option: InputBoxOptions.t=?,
    ~token: CancellationToken.t=?,
    unit,
  ) => Promise.t<option<string>> = "showInputBox"
  @module("vscode") @scope("window")
  external showOpenDialog: OpenDialogOptions.t => Promise.t<option<Uri.t>> = "shoeOpenDialog"
  @module("vscode") @scope("window")
  external showQuickPick: (
    Promise.t<array<string>>,
    QuickPickOptions.t,
    option<CancellationToken.t>,
  ) => Promise.t<option<array<string>>> = "showQuickPick"
  @module("vscode") @scope("window")
  external showSaveDialog: SaveDialogOptions.t => Promise.t<option<Uri.t>> = "showSaveDialog"
  @module("vscode") @scope("window")
  external showTextDocument: (
    TextDocument.t,
    ~column: ViewColumn.t=?,
    ~preserveFocus: bool=?,
    unit,
  ) => Promise.t<TextEditor.t> = "showTextDocument"
  @module("vscode") @scope("window")
  external showTextDocumentWithShowOptions: (
    TextDocument.t,
    option<TextDocumentShowOptions.t>,
  ) => Promise.t<TextEditor.t> = "showTextDocument"
  @module("vscode") @scope("window")
  external showTextDocumentWithUri: (
    Uri.t,
    option<TextDocumentShowOptions.t>,
  ) => Promise.t<TextEditor.t> = "showTextDocument"
  @module("vscode") @scope("window") @variadic
  external showWarningMessage: (string, array<string>) => Promise.t<option<string>> =
    "showWarningMessage"
  @module("vscode") @scope("window") @variadic
  external showWarningMessageWithOptions: (
    string,
    MessageOptions.t,
    array<string>,
  ) => Promise.t<option<string>> = "showWarningMessage"
  @module("vscode") @scope("window")
  external showWorkspaceFolderPick: option<WorkspaceFolderPickOptions.t> => Promise.t<
    option<WorkspaceFolder.t>,
  > = "showWorkspaceFolderPick"
  @module("vscode") @scope("window")
  external withProgress: (
    ProgressOptions.t,
    (Progress.t<{"increment": int, "message": string}>, CancellationToken.t) => Promise.t<'a>,
  ) => Promise.t<'a> = "withProgress"
  @module("vscode") @scope("window")
  external withScmProgress: ((Progress.t<int>, CancellationToken.t) => Promise.t<'a>) => Promise.t<
    'a,
  > = "withScmProgress"
}

// https://code.visualstudio.com/api/references/vscode-api#FileType
module FileType = {
  type raw = int
  type t =
    | Unknown
    | File
    | Directory
    | SymbolicLink

  let toEnum = x =>
    switch x {
    | Unknown => 0
    | File => 1
    | Directory => 2
    | SymbolicLink => 64
    }
  let fromEnum = x =>
    switch x {
    | 0 => Unknown
    | 1 => File
    | 2 => Directory
    | 64 => SymbolicLink
    | _ => Unknown
    }
}

// https://code.visualstudio.com/api/references/vscode-api#FileStat
module FileStat = {
  type t
  @get external ctime: t => int = "ctime"
  @get external mtime: t => int = "mtime"
  @get external size: t => int = "size"
  @get external type_raw: t => FileType.raw = "type"
  let type_ = (self: t): FileType.t => type_raw(self)->FileType.fromEnum
}

// https://code.visualstudio.com/api/references/vscode-api#FileSystem
module FileSystem = {
  type t
  // methods
  @bs.send external copy: (t, Uri.t, Uri.t) => Promise.t<unit> = "copy"
  @bs.send
  external copyWithOptions: (t, Uri.t, Uri.t, {"overwrite": bool}) => Promise.t<unit> = "copy"
  @bs.send
  external createDirectory: (t, Uri.t) => Promise.t<unit> = "createDirectory"
  @bs.send external delete: (t, Uri.t) => Promise.t<unit> = "delete"
  @bs.send
  external deleteWithOptions: (t, Uri.t, {"recursive": bool, "useTrash": bool}) => Promise.t<unit> =
    "delete"
  @bs.send
  external readDirectory_raw: (t, Uri.t) => Promise.t<array<StringOr.t<FileType.raw>>> =
    "readDirectory"
  let readDirectory = (self: t, uri: Uri.t): Promise.t<array<StringOr.t<FileType.t>>> =>
    readDirectory_raw(self, uri)->Promise.map(xs =>
      xs->Belt.Array.map(StringOr.map(FileType.fromEnum))
    )

  @bs.send
  external readFile: (t, Uri.t) => Promise.t<Js.TypedArray2.Int8Array.t> = "readFile"
  @bs.send external rename: (t, Uri.t, Uri.t) => Promise.t<unit> = "rename"
  @bs.send
  external renameWithOptions: (t, Uri.t, Uri.t, {"overwrite": bool}) => Promise.t<unit> = "rename"
  @bs.send external stat: (t, Uri.t) => Promise.t<FileStat.t> = "stat"
  @bs.send
  external stwriteFileat: (t, Uri.t, Js.TypedArray2.Uint8Array.t) => Promise.t<unit> = "writeFile"
}

// https://code.visualstudio.com/api/references/vscode-api#ConfigurationChangeEvent
module ConfigurationChangeEvent = {
  type t

  @bs.send
  external affectsConfiguration: (
    t,
    string,
    @unwrap
    [
      | #Uri(Uri.t)
      | #TextDocument(TextDocument.t)
      | #WorkspaceFolder(WorkspaceFolder.t)
      | #Others(option<{"languageId": string, "uri": Uri.t}>)
    ],
  ) => bool = "affectsConfiguration"
}

// https://code.visualstudio.com/api/references/vscode-api#TextDocumentContentChangeEvent
module TextDocumentContentChangeEvent = {
  type t
  // properties
  @get external range: t => Range.t = "range"
  @get external rangeLength: t => int = "rangeLength"
  @get external rangeOffset: t => int = "rangeOffset"
  @get external text: t => string = "text"
}

// https://code.visualstudio.com/api/references/vscode-api#TextDocumentChangeEvent
module TextDocumentChangeEvent = {
  type t
  // properties
  @get
  external contentChanges: t => array<TextDocumentContentChangeEvent.t> = "contentChanges"
  @get external document: t => TextDocument.t = "document"
}

// https://code.visualstudio.com/api/references/vscode-api#WorkspaceFoldersChangeEvent
module WorkspaceFoldersChangeEvent = {
  type t
  // properties
  @get external added: t => array<WorkspaceFolder.t> = "added"
  @get external removed: t => array<WorkspaceFolder.t> = "removed"
}

// https://code.visualstudio.com/api/references/vscode-api#FileCreateEvent
module FileCreateEvent = {
  type t
  // properties
  @get external files: t => array<Uri.t> = "files"
}

// https://code.visualstudio.com/api/references/vscode-api#FileDeleteEvent
module FileDeleteEvent = {
  type t
  // properties
  @get external files: t => array<Uri.t> = "files"
}

// https://code.visualstudio.com/api/references/vscode-api#FileRenameEvent
module FileRenameEvent = {
  type t
  // properties
  @get
  external files: t => array<{"newUri": Uri.t, "oldUri": Uri.t}> = "files"
}

// https://code.visualstudio.com/api/references/vscode-api#WorkspaceEditEntryMetadata;
module WorkspaceEditEntryMetadata = {
  type t
  // properties
  @get external description: t => option<string> = "description"
  // TODO: [@bs.get] external iconPath: t => ??? = "iconPath";
  @get external label: t => string = "label"
  @get external needsConfirmation: t => bool = "needsConfirmation"
}

// https://code.visualstudio.com/api/references/vscode-api#TextEdit
module TextEdit = {
  type t

  // static
  @module("vscode") @scope("TextEdit")
  external delete: Range.t => t = "delete"
  @module("vscode") @scope("TextEdit")
  external insert: (Position.t, string) => t = "insert"
  @module("vscode") @scope("TextEdit")
  external replace: (Range.t, string) => t = "replace"
  @module("vscode") @scope("TextEdit")
  external setEndOfLine_raw: int => t = "setEndOfLine"
  let setEndOfLine = (eol: EndOfLine.t): t => setEndOfLine_raw(EndOfLine.toEnum(eol))

  // constructors
  @module("vscode") @new
  external make: (Range.t, string) => t = "TextEdit"

  // properties
  @get external newEol: t => option<EndOfLine.t> = "newEol"
  @get external newText: t => string = "newText"
  @get external range: t => Range.t = "range"
}

// https://code.visualstudio.com/api/references/vscode-api#WorkspaceEdit
module WorkspaceEdit = {
  type t
  // NOTE: this is missing in the API
  // constructors
  @module("vscode") @new external make: unit => t = "WorkspaceEdit"

  // properties
  @get external size: t => int = "size"
  // methods
  @bs.send
  external createFile: (
    t,
    Uri.t,
    option<{"ignoreIfExists": bool, "overwrite": bool}>,
    option<WorkspaceEditEntryMetadata.t>,
    unit,
  ) => unit = "createFile"
  @bs.send
  external delete: (t, Uri.t, Range.t, option<WorkspaceEditEntryMetadata.t>) => unit = "delete"
  @bs.send
  external deleteFile: (
    t,
    Uri.t,
    Range.t,
    option<{"ignoreIfNotExists": bool, "recursive": bool}>,
    option<WorkspaceEditEntryMetadata.t>,
  ) => unit = "deleteFile"
  @bs.send external entries_raw: t => array<'shit> = "entries"
  let entries = (self: t): array<(Uri.t, array<TextEdit.t>)> => Array.map(shit => {
      let toUri = %raw("function (shit) { return shit[0] }")
      let toTextEdits = %raw("function (shit) { return shit[1] }")
      (toUri(shit), toTextEdits(shit))
    }, entries_raw(self))
  @bs.send external get: (t, Uri.t) => array<TextEdit.t> = "get"
  @bs.send external has: (t, Uri.t) => bool = "has"
  @bs.send
  external insert: (t, Uri.t, Position.t, string, option<WorkspaceEditEntryMetadata.t>) => unit =
    "insert"
  @bs.send
  external renameFile: (
    t,
    Uri.t,
    Uri.t,
    option<{"ignoreIfExists": bool, "overwrite": bool}>,
    option<WorkspaceEditEntryMetadata.t>,
  ) => unit = "renameFile"
  @bs.send
  external replace: (t, Uri.t, Range.t, string, option<WorkspaceEditEntryMetadata.t>) => unit =
    "replace"
  @bs.send external set: (t, Uri.t, array<TextEdit.t>) => unit = "set"
}

// https://code.visualstudio.com/api/references/vscode-api#FileWillCreateEvent
module FileWillCreateEvent = {
  type t
  // properties
  @get external files: t => array<Uri.t> = "files"
  // methods
  @bs.send
  external waitUntilWithWorkspaceEdit: (t, Promise.t<WorkspaceEdit.t>) => unit = "waitUntil"
  @bs.send external waitUntil: (t, Promise.t<'a>) => unit = "waitUntil"
}
// https://code.visualstudio.com/api/references/vscode-api#FileWillDeleteEvent
module FileWillDeleteEvent = {
  type t
  // properties
  @get external files: t => array<Uri.t> = "files"
  // methods
  @bs.send
  external waitUntilWithWorkspaceEdit: (t, Promise.t<WorkspaceEdit.t>) => unit = "waitUntil"
  @bs.send external waitUntil: (t, Promise.t<'a>) => unit = "waitUntil"
}
// https://code.visualstudio.com/api/references/vscode-api#FileWillRenameEvent
module FileWillRenameEvent = {
  type t
  // properties
  @get
  external files: t => array<{"newUri": Uri.t, "oldUri": Uri.t}> = "files"
  // methods
  @bs.send
  external waitUntilWithWorkspaceEdit: (t, Promise.t<WorkspaceEdit.t>) => unit = "waitUntil"
  @bs.send external waitUntil: (t, Promise.t<'a>) => unit = "waitUntil"
}

// https://code.visualstudio.com/api/references/vscode-api#TextDocumentSaveReason
module TextDocumentSaveReason = {
  type t =
    | AfterDelay
    | FocusOut
    | Manual

  let toEnum = x =>
    switch x {
    | AfterDelay => 2
    | FocusOut => 3
    | Manual => 1
    }
  let fromEnum = x =>
    switch x {
    | 2 => AfterDelay
    | 3 => FocusOut
    | _ => Manual
    }
}

// https://code.visualstudio.com/api/references/vscode-api#TextDocumentWillSaveEvent
module TextDocumentWillSaveEvent = {
  type t
  // properties
  @get external document: t => TextDocument.t = "document"
  @get external reason_raw: t => int = "reason"
  let reason = self => TextDocumentSaveReason.fromEnum(self->reason_raw)
  // methods
  @bs.send
  external waitUntilWithTextEdit: (t, Promise.t<TextEdit.t>) => unit = "waitUntil"
  @bs.send external waitUntil: (t, Promise.t<'a>) => unit = "waitUntil"
}

// https://code.visualstudio.com/api/references/vscode-api#RelativePattern
module RelativePattern = {
  type t
  // constructors
  @module("vscode") @new
  external make: (string, string) => t = "RelativePattern"
  @module("vscode") @new
  external makeWithWorkspaceFolder: (WorkspaceFolder.t, string) => t = "RelativePattern"
  // properties
  @get external base: t => string = "base"
  @get external pattern: t => string = "pattern"
}

// https://code.visualstudio.com/api/references/vscode-api#GlobPattern
module GlobPattern = {
  type t = StringOr.t<RelativePattern.t>
}

// https://code.visualstudio.com/api/references/vscode-api#FileSystemWatcher
module FileSystemWatcher = {
  type t
  // events
  @bs.send
  external onDidChange: (t, Uri.t => unit) => Disposable.t = "onDidChange"
  @bs.send
  external onDidCreate: (t, Uri.t => unit) => Disposable.t = "onDidCreate"
  @bs.send
  external onDidDelete: (t, Uri.t => unit) => Disposable.t = "onDidDelete"
  // static
  @module("vscode") @scope("FileSystemWatcher")
  external from: array<{"dispose": unit => 'a}> => Disposable.t = "from"
  // constructors
  @module("vscode") @new
  external make: (unit => unit) => t = "FileSystemWatcher"
  // properties
  @get external ignoreChangeEvents: t => bool = "ignoreChangeEvents"
  @get external ignoreCreateEvents: t => bool = "ignoreCreateEvents"
  @get external ignoreDeleteEvents: t => bool = "ignoreDeleteEvents"
  // methods
  @bs.send external dispose: t => 'a = "dispose"
}

// https://code.visualstudio.com/api/references/vscode-api#WorkspaceConfiguration
module WorkspaceConfiguration = {
  type t
  // methods
  @bs.send external get: (t, string) => option<'a> = "get"
  @bs.send external getWithDefault: (t, string, 'a) => 'a = "get"
  @bs.send external has: (t, string) => bool = "has"
  @bs.send
  external inspect: (
    t,
    string,
  ) => option<{
    "defaultLanguageValue": 'a,
    "defaultValue": 'a,
    "globalLanguageValue": 'a,
    "globalValue": 'a,
    "key": string,
    "languageIds": array<string>,
    "workspaceFolderLanguageValue": 'a,
    "workspaceFolderValue": 'a,
    "workspaceLanguageValue": 'a,
    "workspaceValue": 'a,
  }> = "inspect"
  @bs.send
  external updateGlobalSettings: (t, string, 'a, @as(1) _, option<bool>) => Promise.t<unit> =
    "update"
  @bs.send
  external updateWorkspaceSettings: (t, string, 'a, @as(2) _, option<bool>) => Promise.t<unit> =
    "update"
  @bs.send
  external updateWorkspaceFolderSettings: (
    t,
    string,
    'a,
    @as(3) _,
    option<bool>,
  ) => Promise.t<unit> = "update"
}

// https://code.visualstudio.com/api/references/vscode-api#TextDocumentContentProvider
// 1.59.0
module TextDocumentContentProvider = {
  type t
  // events
  @module("vscode") @scope("workspace")
  external onDidChange: option<Event.t<Uri.t>> = "onDidChange"
  // methods
  @bs.send
  external provideTextDocumentContent: (t, Uri.t, CancellationToken.t) => ProviderResult.t<string> =
    "provideTextDocumentContent"
}

// https://code.visualstudio.com/api/references/vscode-api#TaskProvider
// 1.59.0
module TaskProvider = {
  type t<'a>
  // methods
  @bs.send
  external provideTasks: (t<'a>, CancellationToken.t) => ProviderResult.t<array<'a>> =
    "provideTasks"
  @bs.send
  external resolveTask: (t<'a>, 'a, CancellationToken.t) => ProviderResult.t<'a> = "resolveTask"
}

// https://code.visualstudio.com/api/references/vscode-api#FileSystemProvider
module FileSystemProvider = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#workspace
// 1.85.1 WIP
module Workspace = {
  // variables
  @module("vscode") @scope("workspace")
  external fs: FileSystem.t = "fs"
  @module("vscode") @scope("workspace")
  external isTrusted: bool = "isTrusted"
  @module("vscode") @scope("workspace")
  external name: option<string> = "name"
  @module("vscode") @scope("workspace")
  external rootPath: option<string> = "rootPath"
  @module("vscode") @scope("workspace")
  external textDocuments: array<TextDocument.t> = "textDocuments"
  @module("vscode") @scope("workspace")
  external workspaceFile: option<Uri.t> = "workspaceFile"
  @module("vscode") @scope("workspace")
  external workspaceFolders: option<array<WorkspaceFolder.t>> = "workspaceFolders"

  // events
  @module("vscode") @scope("workspace")
  external onDidChangeConfiguration: Event.t<ConfigurationChangeEvent.t> =
    "onDidChangeConfiguration"
  @module("vscode") @scope("workspace")
  external onDidChangeTextDocument: Event.t<TextDocumentChangeEvent.t> = "onDidChangeTextDocument"
  @module("vscode") @scope("workspace")
  external onDidChangeWorkspaceFolders: Event.t<WorkspaceFoldersChangeEvent.t> =
    "onDidChangeWorkspaceFolders"
  @module("vscode") @scope("workspace")
  external onDidCloseTextDocument: Event.t<TextDocument.t> = "onDidCloseTextDocument"
  @module("vscode") @scope("workspace")
  external onDidCreateFiles: Event.t<FileCreateEvent.t> = "onDidCreateFiles"
  @module("vscode") @scope("workspace")
  external onDidDeleteFiles: Event.t<FileDeleteEvent.t> = "onDidDeleteFiles"
  @module("vscode") @scope("workspace")
  external onDidOpenTextDocument: Event.t<TextDocument.t> = "onDidOpenTextDocument"
  @module("vscode") @scope("workspace")
  external onDidRenameFiles: Event.t<FileRenameEvent.t> = "onDidRenameFiles"
  @module("vscode") @scope("workspace")
  external onDidSaveTextDocument: Event.t<TextDocument.t> = "onDidSaveTextDocument"
  @module("vscode") @scope("workspace")
  external onWillCreateFiles: Event.t<FileWillCreateEvent.t> = "onWillCreateFiles"
  @module("vscode") @scope("workspace")
  external onWillDeleteFiles: Event.t<FileWillDeleteEvent.t> = "onWillDeleteFiles"
  @module("vscode") @scope("workspace")
  external onWillRenameFiles: Event.t<FileWillRenameEvent.t> = "onWillRenameFiles"
  @module("vscode") @scope("workspace")
  external onWillSaveTextDocument: Event.t<TextDocumentWillSaveEvent.t> = "onWillSaveTextDocument"
  // functions
  @module("vscode") @scope("workspace")
  external applyEdit: WorkspaceEdit.t => Promise.t<bool> = "applyEdit"
  @module("vscode") @scope("workspace")
  external asRelativePath: (string, option<bool>) => string = "asRelativePath"
  @module("vscode") @scope("workspace")
  external asRelativePathWithUri: (Uri.t, option<bool>) => string = "asRelativePath"
  @module("vscode") @scope("workspace")
  external createFileSystemWatcher: (
    GlobPattern.t,
    ~ignoreCreateEvents: bool=?,
    ~ignoreChangeEvents: bool=?,
    ~ignoreDeleteEvents: bool=?,
  ) => FileSystemWatcher.t = "createFileSystemWatcher"
  @module("vscode") @scope("workspace")
  external findFiles: (
    GlobPattern.t,
    ~exclude: Js.nullable<GlobPattern.t>=?,
    ~token: CancellationToken.t=?,
  ) => Promise.t<array<Uri.t>> = "findFiles"
  @module("vscode") @scope("workspace")
  external getConfiguration: (option<string>, option<Uri.t>) => WorkspaceConfiguration.t =
    "getConfiguration"
  external getConfigurationOfTextDocument: (
    option<string>,
    option<TextDocument.t>,
  ) => WorkspaceConfiguration.t = "getConfiguration"
  external getConfigurationOfWorkspaceFolder: (
    option<string>,
    option<WorkspaceFolder.t>,
  ) => WorkspaceConfiguration.t = "getConfiguration"
  external getConfigurationOfLanguage: (
    option<string>,
    option<{"languageId": string, "uri": Uri.t}>,
  ) => WorkspaceConfiguration.t = "getConfiguration"
  @module("vscode") @scope("workspace")
  external getWorkspaceFolder: Uri.t => option<WorkspaceFolder.t> = "getWorkspaceFolder"
  @module("vscode") @scope("workspace")
  external openTextDocument: Uri.t => Promise.t<TextDocument.t> = "openTextDocument"
  @module("vscode") @scope("workspace")
  external openTextDocumentWithFileName: string => Promise.t<TextDocument.t> = "openTextDocument"
  @module("vscode") @scope("workspace")
  external openTextDocumentWithOptions: option<{
    "content": string,
    "language": string,
  }> => Promise.t<TextDocument.t> = "openTextDocument"
  @module("vscode") @scope("workspace")
  external registerFileSystemProvider: (
    string,
    FileSystemProvider.t,
    option<{"isCaseSensitive": bool, "isReadonly": bool}>,
  ) => Disposable.t = "registerFileSystemProvider"
  @module("vscode") @scope("workspace")
  external registerTaskProvider: (string, TaskProvider.t<'a>) => Disposable.t =
    "registerTaskProvider"
  @module("vscode") @scope("workspace")
  external registerTextDocumentContentProvider: (
    string,
    TextDocumentContentProvider.t,
  ) => Disposable.t = "registerTextDocumentContentProvider"
  @module("vscode") @scope("workspace")
  external saveAll: option<bool> => Promise.t<bool> = "saveAll"
  @module("vscode") @scope("workspace") @variadic
  external updateWorkspaceFolders: (
    int,
    option<int>,
    array<{"name": string, "uri": Uri.t}>,
  ) => Promise.t<bool> = "updateWorkspaceFolders"
}

// https://code.visualstudio.com/api/references/vscode-api#ExtensionKind
// 1.52.0
module ExtensionKind = {
  type t =
    | UI
    | Workspace
  let toEnum = x =>
    switch x {
    | UI => 1
    | Workspace => 2
    }
  let fromEnum = x =>
    switch x {
    | 1 => UI
    | _ => Workspace
    }
}

// https://code.visualstudio.com/api/references/vscode-api#Extension
module Extension = {
  type t<'a>

  // properties
  @get external exports: t<'a> => 'a = "exports"
  @get external extensionKind_raw: t<'a> => int = "extensionKind"
  let extensionKind = (self: t<'a>): ExtensionKind.t =>
    ExtensionKind.fromEnum(extensionKind_raw(self))
  @get external extensionPath: t<'a> => string = "extensionPath"
  @get external extensionUri: t<'a> => Uri.t = "extensionUri"
  @get external id: t<'a> => string = "id"
  @get external isActive: t<'a> => bool = "isActive"
  @get external packageJSON: t<'a> => 'json = "packageJSON"

  // methods
  @bs.send external activate: t<'a> => Promise.t<'a> = "activate"
}

// https://code.visualstudio.com/api/references/vscode-api#extensions
// 1.52.0
module Extensions = {
  // variables
  @module("vscode") @scope("extensions")
  external all: array<Extension.t<'a>> = "all"
  // events
  @module("vscode") @scope("extensions")
  external onDidChange: Event.t<unit> = "onDidChange"
  // functions
  @module("vscode") @scope("extensions")
  external getExtension: string => option<Extension.t<'a>> = "getExtension"
}

// https://code.visualstudio.com/api/references/vscode-api#DiagnosticChangeEvent
module DiagnosticChangeEvent = {
  type t
  // properties
  @get external uris: t => array<Uri.t> = "uris"
}

// https://code.visualstudio.com/api/references/vscode-api#Location
module Location = {
  type t
  // constructors
  @module("vscode") @new
  external makeWithRange: (Uri.t, Range.t) => t = "Location"
  @module("vscode") @new
  external makeWithPosition: (Uri.t, Position.t) => t = "Location"

  // properties
  @get external range: t => Range.t = "range"
  @get external uri: t => Uri.t = "uri"
}

// https://code.visualstudio.com/api/references/vscode-api#LocationLink
module LocationLink = {
  type t = {
    originSelectionRange: option<Range.t>,
    targetRange: Range.t,
    targetSelectionRange: option<Range.t>,
    targetUri: Uri.t,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#DiagnosticRelatedInformation
module DiagnosticRelatedInformation = {
  type t

  // constructors
  @module("vscode") @new
  external make: (Location.t, string) => t = "DiagnosticRelatedInformation"

  // properties
  @get external location: t => Location.t = "location"
  @get external message: t => string = "message"
}

// https://code.visualstudio.com/api/references/vscode-api#DiagnosticSeverity
module DiagnosticSeverity = {
  type t =
    | Error
    | Warning
    | Information
    | Hint

  let toEnum = x =>
    switch x {
    | Error => 0
    | Warning => 1
    | Information => 2
    | Hint => 3
    }
  let fromEnum = x =>
    switch x {
    | 0 => Error
    | 1 => Warning
    | 2 => Information
    | _ => Hint
    }
}

// https://code.visualstudio.com/api/references/vscode-api#DiagnosticTag
module DiagnosticTag = {
  type t =
    | Unnecessary
    | Deprecated

  let toEnum = x =>
    switch x {
    | Unnecessary => 1
    | Deprecated => 2
    }
  let fromEnum = x =>
    switch x {
    | 1 => Unnecessary
    | _ => Deprecated
    }
}

// https://code.visualstudio.com/api/references/vscode-api#Diagnostic
module Diagnostic = {
  type t

  // constructors
  @module("vscode") @new
  external make: (Range.t, string, option<DiagnosticSeverity.t>) => t = "Diagnostic"

  // properties
  @get external code: t => option<string> = "code" // FIX THIS
  @get external message: t => string = "message"
  @get external range: t => Range.t = "range"
  @get
  external relatedInformation: t => option<array<DiagnosticRelatedInformation.t>> =
    "relatedInformation"
  @get external severity: t => DiagnosticSeverity.t = "severity"
  @get external source: t => option<string> = "source"
  @get external tags: t => option<array<DiagnosticTag.t>> = "tags"
}

// https://code.visualstudio.com/api/references/vscode-api#DocumentFilter
module DocumentFilter = {
  type t = {
    language: option<string>,
    pattern: option<GlobPattern.t>,
    scheme: option<string>,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#DocumentSelector
module DocumentSelector = {
  type t = array<StringOr.t<DocumentFilter.t>>
}

// https://code.visualstudio.com/api/references/vscode-api#DiagnosticCollection
module DiagnosticCollection = {
  type t
  // properties
  @get external name: t => string = "name"
  // methods
  @bs.send external clear: t => unit = "clear"
  @bs.send external delete: (t, Uri.t) => unit = "delete"
  @bs.send external dispose: t => unit = "dispose"
  @bs.send
  external forEach: (t, (Uri.t, array<Diagnostic.t>, t) => 'a) => unit = "forEach"
  @bs.send external get: (t, Uri.t) => option<array<Diagnostic.t>> = "get"
  @bs.send external has: (t, Uri.t) => bool = "has"
  @bs.send external set: (t, Uri.t) => unit = "set"
  @bs.send
  external setDiagnostics: (t, Uri.t, array<Diagnostic.t>) => unit = "set"
  @bs.send
  external setDiagnosticEntries: (t, array<(Uri.t, option<array<Diagnostic.t>>)>) => unit = "set"
}

// https://code.visualstudio.com/api/references/vscode-api#CallHierarchyItem
module CallHierarchyItem = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#CallHierarchyOutgoingCall
module CallHierarchyOutgoingCall = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#CallHierarchyIncomingCall
module CallHierarchyIncomingCall = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#CallHierarchyProvider
module CallHierarchyProvider = {
  type t
  // methods
  @bs.send
  external prepareCallHierarchy: (
    t,
    TextDocument.t,
    Position.t,
    CancellationToken.t,
  ) => ProviderResult.t<array<CallHierarchyItem.t>> = "prepareCallHierarchy"
  @bs.send
  external provideCallHierarchyIncomingCalls: (
    t,
    CallHierarchyItem.t,
    CancellationToken.t,
  ) => ProviderResult.t<array<CallHierarchyIncomingCall.t>> = "provideCallHierarchyIncomingCalls"
  @bs.send
  external provideCallHierarchyOutgoingCalls: (
    t,
    CallHierarchyItem.t,
    CancellationToken.t,
  ) => ProviderResult.t<array<CallHierarchyOutgoingCall.t>> = "provideCallHierarchyOutgoingCalls"
}

// https://code.visualstudio.com/api/references/vscode-api#CodeActionProvider
module CodeActionProvider = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#CodeActionProviderMetadata
module CodeActionProviderMetadata = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#CodeLens
module CodeLens = {
  type t
  // constructors
  @module("vscode") @new external make: Range.t => t = "CodeLens"
  @module("vscode") @new
  external makeWithCommand: (Range.t, Command.t) => t = "CodeLens"
  // properties
  @get external command: t => option<Command.t> = "command"
  @get external isResolved: t => bool = "isResolved"
  @get external range: t => Range.t = "range"
}

// https://code.visualstudio.com/api/references/vscode-api#CodeLensProvider
module CodeLensProvider = {
  type t<'a> = {
    onDidChangeCodeLenses: option<Event.t<unit>>,
    resolveCodeLens: ('a, CancellationToken.t) => ProviderResult.t<'a>,
    provideCodeLenses: (TextDocument.t, CancellationToken.t) => ProviderResult.t<array<'a>>,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#DocumentColorProvider
module DocumentColorProvider = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#CompletionItemProvider
module CompletionItemProvider = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#DeclarationProvider
module DeclarationProvider = {
  type t
}

// MarkedString is deprecated in favor of MarkdownString

// https://code.visualstudio.com/api/references/vscode-api#Hover
module Hover = {
  type t
  // constructors
  @module("vscode") @new
  external make: array<MarkdownString.t> => t = "Hover"
  @module("vscode") @new
  external makeWithRange: (array<MarkdownString.t>, Range.t) => t = "Hover"
  // properties
  @get external contents: t => array<MarkdownString.t> = "contents"
  @get external range: t => option<Range.t> = "range"
}

// https://code.visualstudio.com/api/references/vscode-api#HoverProvider
module HoverProvider = {
  type t = {
    provideHover: (TextDocument.t, Position.t, CancellationToken.t) => ProviderResult.t<Hover.t>,
  }
}

module LocationLinkOrLocation: {
  type t
  type case =
    | Location(array<Location.t>)
    | LocationLink(array<LocationLink.t>)
  let locations: array<Location.t> => t
  let locationLinks: array<LocationLink.t> => t
  let classify: t => case
} = {
  @unboxed
  type rec t = Any('a): t
  type case =
    | Location(array<Location.t>)
    | LocationLink(array<LocationLink.t>)
  let locations = (v: array<Location.t>) => Any(v)
  let locationLinks = (v: array<LocationLink.t>) => Any(v)
  let classify = (Any(v): t): case =>
    if %raw(`function (a) { return a.targetRange === undefined}`)(v) {
      Location((Obj.magic(v): array<Location.t>))
    } else {
      LocationLink((Obj.magic(v): array<LocationLink.t>))
    }
}

// https://code.visualstudio.com/api/references/vscode-api#DefinitionProvider
module DefinitionProvider = {
  type t = {
    provideDefinition: (
      TextDocument.t,
      Position.t,
      CancellationToken.t,
    ) => ProviderResult.t<LocationLinkOrLocation.t>,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#SemanticsTokens
module SemanticsTokens = {
  type t
  // constructors
  @module("vscode") @new
  external make: array<int> => t = "SemanticsTokens"
  @module("vscode") @new
  external makeWithResultId: (array<int>, string) => t = "SemanticsTokens"
  // properties
  @get external data: t => array<int> = "data"
  @get external resultId: t => option<string> = "resultId"
}

// https://code.visualstudio.com/api/references/vscode-api#SemanticTokensLegend
module SemanticTokensLegend = {
  type t
  // constructors
  @module("vscode") @new
  external make: array<string> => t = "SemanticTokensLegend"
  @module("vscode") @new
  external makeWithTokenModifiers: (array<string>, array<string>) => t = "SemanticTokensLegend"
  // properties
  @get external tokenModifiers: t => array<string> = "tokenModifiers"
  @get external tokenTypes: t => array<string> = "tokenTypes"
}

// https://code.visualstudio.com/api/references/vscode-api#SemanticTokensBuilder
module SemanticTokensBuilder = {
  type t
  // constructors
  @module("vscode") @new
  external make: unit => t = "SemanticTokensBuilder"
  @module("vscode") @new
  external makeWithLegend: SemanticTokensLegend.t => t = "SemanticTokensBuilder"
  // methods
  @bs.send external build: unit => SemanticsTokens.t = "build"
  @bs.send external buildWithResultId: string => SemanticsTokens.t = "build"
  @bs.send
  external push: (int, int, int, int, option<int>) => unit = "push"
  @bs.send
  external pushLegend: (Range.t, string, option<array<string>>) => unit = "push"
}

// https://code.visualstudio.com/api/references/vscode-api#DocumentSemanticTokensProvider
module DocumentSemanticTokensProvider = {
  // missing: onDidChangeSemanticTokens
  // missing: provideDocumentSemanticTokensEdits
  type t = {
    provideDocumentSemanticTokens: (
      TextDocument.t,
      CancellationToken.t,
    ) => ProviderResult.t<SemanticsTokens.t>,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#languages
module Languages = {
  // events
  @module("vscode") @scope("languages")
  external onDidChangeDiagnostics: Event.t<DiagnosticChangeEvent.t> = "onDidChangeDiagnostics"

  // functions
  @module("vscode") @scope("languages")
  external createDiagnosticCollection: option<string> => DiagnosticCollection.t =
    "createDiagnosticCollection"
  @module("vscode") @scope("languages")
  external getDiagnostics: Uri.t => array<Diagnostic.t> = "getDiagnostics"
  @module("vscode") @scope("languages")
  external getDiagnosticEntries: Uri.t => array<(Uri.t, array<Diagnostic.t>)> = "getDiagnostics"
  @module("vscode") @scope("languages")
  external getLanguages: unit => Promise.t<array<string>> = "getLanguages"
  @module("vscode") @scope("languages")
  external match_: (DocumentSelector.t, TextDocument.t) => int = "match"
  @module("vscode") @scope("languages")
  external registerCallHierarchyProvider: (
    DocumentSelector.t,
    CallHierarchyProvider.t,
  ) => Disposable.t = "registerCallHierarchyProvider"
  @module("vscode") @scope("languages")
  external registerCodeActionsProvider: (
    DocumentSelector.t,
    CodeActionProvider.t,
    option<CodeActionProviderMetadata.t>,
  ) => Disposable.t = "registerCodeActionsProvider"
  @module("vscode") @scope("languages")
  external registerCodeLensProvider: (DocumentSelector.t, CodeLensProvider.t<'a>) => Disposable.t =
    "registerCodeLensProvider"
  @module("vscode") @scope("languages")
  external registerColorProvider: (DocumentSelector.t, DocumentColorProvider.t) => Disposable.t =
    "registerColorProvider"
  @module("vscode") @scope("languages")
  external registerCompletionItemProvider0: (
    DocumentSelector.t,
    CompletionItemProvider.t,
  ) => Disposable.t = "registerCompletionItemProvider"
  @module("vscode") @scope("languages")
  external registerCompletionItemProvider1: (
    DocumentSelector.t,
    CompletionItemProvider.t,
    string,
  ) => Disposable.t = "registerCompletionItemProvider"
  @module("vscode") @scope("languages")
  external registerCompletionItemProvider2: (
    DocumentSelector.t,
    CompletionItemProvider.t,
    string,
    string,
  ) => Disposable.t = "registerCompletionItemProvider"
  @module("vscode") @scope("languages")
  external registerCompletionItemProvider3: (
    DocumentSelector.t,
    CompletionItemProvider.t,
    string,
    string,
    string,
  ) => Disposable.t = "registerCompletionItemProvider"
  @module("vscode") @scope("languages")
  external registerDeclarationProvider: (
    DocumentSelector.t,
    DeclarationProvider.t,
  ) => Disposable.t = "registerDeclarationProvider"
  @module("vscode") @scope("languages")
  external registerDefinitionProvider: (DocumentSelector.t, DefinitionProvider.t) => Disposable.t =
    "registerDefinitionProvider"
  // registerDocumentFormattingEditProvider(selector: DocumentSelector, provider: DocumentFormattingEditProvider): Disposable
  // registerDocumentHighlightProvider(selector: DocumentSelector, provider: DocumentHighlightProvider): Disposable
  // registerDocumentLinkProvider(selector: DocumentSelector, provider: DocumentLinkProvider): Disposable
  // registerDocumentRangeFormattingEditProvider(selector: DocumentSelector, provider: DocumentRangeFormattingEditProvider): Disposable
  // registerDocumentRangeSemanticTokensProvider(selector: DocumentSelector, provider: DocumentRangeSemanticTokensProvider, legend: SemanticTokensLegend): Disposable
  @module("vscode") @scope("languages")
  external registerDocumentSemanticTokensProvider: (
    DocumentSelector.t,
    DocumentSemanticTokensProvider.t,
    SemanticTokensLegend.t,
  ) => Disposable.t = "registerDocumentSemanticTokensProvider"
  // registerDocumentSymbolProvider(selector: DocumentSelector, provider: DocumentSymbolProvider, metaData?: DocumentSymbolProviderMetadata): Disposable
  // registerEvaluatableExpressionProvider(selector: DocumentSelector, provider: EvaluatableExpressionProvider): Disposable
  // registerFoldingRangeProvider(selector: DocumentSelector, provider: FoldingRangeProvider): Disposable

  @module("vscode") @scope("languages")
  external registerHoverProvider: (DocumentSelector.t, HoverProvider.t) => Disposable.t =
    "registerHoverProvider"
  // registerImplementationProvider(selector: DocumentSelector, provider: ImplementationProvider): Disposable
  // registerOnTypeFormattingEditProvider(selector: DocumentSelector, provider: OnTypeFormattingEditProvider, firstTriggerCharacter: string, ...moreTriggerCharacter: string[]): Disposable
  // registerReferenceProvider(selector: DocumentSelector, provider: ReferenceProvider): Disposable
  // registerRenameProvider(selector: DocumentSelector, provider: RenameProvider): Disposable
  // registerSelectionRangeProvider(selector: DocumentSelector, provider: SelectionRangeProvider): Disposable
  // registerSignatureHelpProvider(selector: DocumentSelector, provider: SignatureHelpProvider, ...triggerCharacters: string[]): Disposable
  // registerSignatureHelpProvider(selector: DocumentSelector, provider: SignatureHelpProvider, metadata: SignatureHelpProviderMetadata): Disposable
  // registerTypeDefinitionProvider(selector: DocumentSelector, provider: TypeDefinitionProvider): Disposable
  // registerWorkspaceSymbolProvider(provider: WorkspaceSymbolProvider): Disposable
  // setLanguageConfiguration(language: string, configuration: LanguageConfiguration): Disposable
  // setTextDocumentLanguage(document: TextDocument, languageId: string): Thenable<TextDocument>
}
