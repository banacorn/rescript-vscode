// https://code.visualstudio.com/api/references/vscode-api#ProviderResult
// 1.52.0
module ProviderResult = {
  type t<'a> = option<promise<'a>>
  let map = (x, f) =>
    x->Belt.Option.map(async result => {
      let result' = await result
      f(result')
    })
}

// https://code.visualstudio.com/api/references/vscode-api#ThemeColor
module ThemeColor = {
  type t
  // constructors
  @module("vscode") @new external make: string => t = "ThemeColor"
}

// https://code.visualstudio.com/api/references/vscode-api#ThemeIcon
// 1.95
module ThemeIcon = {
  type t

  // constructors
  @module("vscode") @new
  external make: (string, option<ThemeColor.t>) => t = "ThemeIcon"

  // static properties
  @module("vscode") @scope("ThemeIcon")
  external file: t = "File"

  @module("vscode") @scope("ThemeIcon")
  external folder: t = "Folder"

  // properties
  @get external color: t => option<ThemeColor.t> = "color"
  @get external id: t => string = "id"
}

// "string | xxx", for modeling the union type of String and something else
module StringOr: {
  type t<'a>
  type case<'a> =
    | String(string)
    | Others('a)
  let make: case<'a> => t<'a>
  let classify: t<'a> => case<'a>
  let map: (t<'a>, 'a => 'b) => t<'b>
} = {
  @unboxed type rec t<'a> = Any('x): t<'a>
  type case<'a> =
    | String(string)
    | Others('a)
  let make = (value: case<'a>): t<'a> =>
    switch value {
    | String(string) => Any(string)
    | Others(others) => Any(others)
    }
  let classify = (Any(v): t<'a>): case<'a> =>
    if Js.typeof(v) == "string" {
      String((Obj.magic(v): string))
    } else {
      Others((Obj.magic(v): 'a))
    }
  let map = (xs: t<'a>, f: 'a => 'b): t<'b> =>
    switch classify(xs) {
    | String(s) => make(String(s))
    | Others(x) => make(Others(f(x)))
    }
}

// "A | promise<A>", for modeling the union type of something and promise of that thing
module PromiseOr: {
  type t<'a>
  type case<'a> =
    | Others('a)
    | Promise(promise<'a>)
  let make: case<'a> => t<'a>
  let classify: t<'a> => case<'a>
} = {
  @unboxed type rec t<'a> = Any('x): t<'a>
  type case<'a> =
    | Others('a)
    | Promise(promise<'a>)
  let make = (value: case<'a>): t<'a> =>
    switch value {
    | Others(value) => Any(value)
    | Promise(promise) => Any(promise)
    }
  let classify = (Any(v): t<'a>): case<'a> =>
    if Obj.magic(v)["then"] && Js.typeof(Obj.magic(v)["then"]) == "function" {
      Promise((Obj.magic(v): promise<'a>))
    } else {
      Others((Obj.magic(v): 'a))
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

  @send external postMessage: (t, 'a) => unit = "postMessage"

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
  @send external dispose: t => 'a = "dispose"
}

// https://code.visualstudio.com/api/references/vscode-api#Event
//
//  UPDATE: deprecated at the moment, because somehow `('a => unit) => Disposable.t` causes function to be curried while the expaneded `('a => unit) => Disposable.t` does not
//
// module Event = {
//   type t<'a> = ('a => unit) => Disposable.t
// }

// https://code.visualstudio.com/api/references/vscode-api#Memento
// 1.96
module Memento = {
  type t
  // methods
  @send external get: (t, string) => option<'a> = "get"
  @send external getWithDefault: (t, string, 'a) => 'a = "get"
  @send external keys: t => array<string> = "keys"
  @send external update: (t, string, 'a) => promise<unit> = "update"
}

// https://code.visualstudio.com/api/references/vscode-api#Uri
// 1.96
module Uri = {
  type t
  type components = {
    scheme: string,
    authority?: string,
    path?: string,
    query?: string,
    fragment?: string,
  }

  // static
  @module("vscode") @scope("Uri") external file: string => t = "file"
  @module("vscode") @scope("Uri") external from: components => t = "from"
  @module("vscode") @scope("Uri") @variadic
  external joinPath: (t, array<string>) => t = "joinPath"
  @module("vscode") @scope("Uri")
  external parse: (string, ~strict: bool=?) => t = "parse"

  // constructors
  @new
  external make: (
    ~scheme: string,
    ~authority: string,
    ~path: string,
    ~query: string,
    ~fragment: string,
  ) => t = "Uri"

  // properties
  @get external authority: t => string = "authority"
  @get external fragment: t => string = "fragment"
  @get external fsPath: t => string = "fsPath"
  @get external path: t => string = "path"
  @get external query: t => string = "query"
  @get external scheme: t => string = "scheme"

  // methods
  @send external toJSON: t => Js.Json.t = "toJSON"
  @send external toString: (t, ~skipEncoding: bool=?) => string = "toString"
  @send external with_: (t, components) => t = "with"
}

// https://code.visualstudio.com/api/references/vscode-api#EnvironmentVariableMutatorType
// 1.95.0
module EnvironmentVariableMutatorType = {
  type t =
    | @as(1) Replace
    | @as(2) Append
    | @as(3) Prepend
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
  @get external type_: EnvironmentVariableMutatorType.t => int = "type"
  @get external value: t => bool = "value"
}

// https://code.visualstudio.com/api/references/vscode-api#EnvironmentVariableCollection
// 1.52.0
module EnvironmentVariableCollection = {
  type t
  // properties
  @get external persistent: t => bool = "persistent"
  // methods
  @send external append: (t, string, string) => unit = "append"
  @send external clear: t => unit = "clear"
  @send external delete: (t, string) => unit = "delete"
  @send
  external forEach: (t, (string, EnvironmentVariableMutator.t, t) => 'a) => unit = "forEach"
  @send
  external forEachWithThisArg: (t, (string, EnvironmentVariableMutator.t, t) => 'a, 'b) => unit =
    "forEach"
  @send external get: (t, string) => option<EnvironmentVariableMutator.t> = "get"
  @send external prepend: (t, string, string) => unit = "prepend"
  @send external replace: (t, string, string) => unit = "replace"
}

// https://code.visualstudio.com/api/references/vscode-api#LanguageModelAccessInformation
// 1.95
module LanguageModelAccessInformation = {
  type t
  // events
  @send external onDidChange: (t, unit => unit) => Disposable.t = "onDidChange"
  // methods
  // @send external canSendRequests: (t, %todo) => bool = "canSendRequests"
}

// https://code.visualstudio.com/api/references/vscode-api#SecretStorageChangeEvent
// 1.95
module SecretStorageChangeEvent = {
  type t
  // properties
  @get external key: t => string = "key"
}

// https://code.visualstudio.com/api/references/vscode-api#SecretStorage
// 1.95
module SecretStorage = {
  type t
  // events
  @send
  external onDidChange: (t, SecretStorageChangeEvent.t => unit) => Disposable.t = "onDidChange"
  // methods
  @send external delete: (t, string) => promise<unit> = "delete"
  @send external get: (t, string) => promise<string> = "get"
  @send external store: (t, string, string) => promise<unit> = "store"
}

// https://code.visualstudio.com/api/references/vscode-api#ExtensionMode
// 1.95
module ExtensionMode = {
  type t =
    | @as(1) Production
    | @as(2) Development
    | @as(3) Test
}

// https://code.visualstudio.com/api/references/vscode-api#ExtensionKind
// 1.95
module ExtensionKind = {
  type t =
    | @as(1) UI
    | @as(2) Workspace
}

// https://code.visualstudio.com/api/references/vscode-api#Extension
// 1.95
module Extension = {
  type t<'a>

  // properties
  @get external exports: t<'a> => 'a = "exports"
  @get external extensionKind: t<'a> => ExtensionKind.t = "extensionKind"
  @get external extensionPath: t<'a> => string = "extensionPath"
  @get external extensionUri: t<'a> => Uri.t = "extensionUri"
  @get external id: t<'a> => string = "id"
  @get external isActive: t<'a> => bool = "isActive"
  @get external packageJSON: t<'a> => 'json = "packageJSON"

  // methods
  @send external activate: t<'a> => promise<'a> = "activate"
}

// https://code.visualstudio.com/api/references/vscode-api#ExtensionContext
module ExtensionContext = {
  type t
  // properties
  @get
  external environmentVariableCollection: t => EnvironmentVariableCollection.t =
    "environmentVariableCollection"
  @get external extension: t => Extension.t<'a> = "extension"
  @get external extensionMode: t => ExtensionMode.t = "extensionMode"
  @get external extensionPath: t => string = "extensionPath"
  @get external extensionUri: t => Uri.t = "extensionUri"
  @get external globalState: t => Memento.t = "globalState"
  @get external globalStorageUri: t => Uri.t = "globalStorageUri"
  @get
  external languageModelAccessInformation: t => LanguageModelAccessInformation.t =
    "languageModelAccessInformation"
  @get external logUri: t => Uri.t = "logUri"
  @get external secrets: t => SecretStorage.t = "secrets"
  @get external storageUri: t => option<Uri.t> = "storageUri"
  @get
  external subscriptions: t => array<Disposable.t> = "subscriptions"
  @get external workspaceState: t => Memento.t = "workspaceState"
  // methods
  @send external asAbsolutePath: (t, string) => string = "asAbsolutePath"
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
  ] => promise<'a> = "executeCommand"
  @module("vscode") @scope("commands")
  external getEditorLayout: (@as("vscode.getEditorLayout") _, unit) => promise<Layout.t> =
    "executeCommand"
  @module("vscode") @scope("commands")
  external executeCommand0: string => promise<'a> = "executeCommand"
  @module("vscode") @scope("commands")
  external executeCommand1: (string, 'arg0) => promise<'a> = "executeCommand"
  @module("vscode") @scope("commands")
  external setContext: (@as("setContext") _, string, bool) => promise<unit> = "executeCommand"

  @module("vscode") @scope("commands")
  external getCommands: option<bool> => promise<array<string>> = "getCommands"
  @module("vscode") @scope("commands")
  external registerCommand: (string, unit => 'a) => Disposable.t = "registerCommand"
}

// https://code.visualstudio.com/api/references/vscode-api#DebugConsole
// 1.52.0
module DebugConsole = {
  type t
  // methods
  @send external append: (t, string) => unit = "append"
  @send external appendLine: (t, string) => unit = "appendLine"
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
  @send external customRequest: (t, string) => promise<'a> = "customRequest"
  @send external customRequestWithArgs: (t, string, 'a) => promise<'b> = "customRequest"
  @send
  external getDebugProtocolBreakpoint: (
    t,
    Breakpoint.t,
  ) => promise<option<DebugProtocolBreakpoint.t>> = "getDebugProtocolBreakpoint"
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
  @send
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
  external onDidChangeActiveDebugSession: (option<DebugSession.t> => unit) => Disposable.t =
    "onDidChangeActiveDebugSession"
  @module("vscode") @scope("debug")
  external onDidChangeBreakpoints: (BreakpointsChangeEvent.t => unit) => Disposable.t =
    "onDidChangeBreakpoints"
  @module("vscode") @scope("debug")
  external onDidReceiveDebugSessionCustomEvent: (
    DebugSessionCustomEvent.t => unit
  ) => Disposable.t = "onDidReceiveDebugSessionCustomEvent"
  @module("vscode") @scope("debug")
  external onDidStartDebugSession: (DebugSession.t => unit) => Disposable.t =
    "onDidStartDebugSession"
  @module("vscode") @scope("debug")
  external onDidTerminateDebugSession: (DebugSession.t => unit) => Disposable.t =
    "onDidTerminateDebugSession"

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
  @send external readText: (t, unit) => promise<string> = "readText"
  @send external writeText: (t, string) => promise<unit> = "writeText"
}

// https://code.visualstudio.com/api/references/vscode-api#UIKind;
// 1.95
module UIKind = {
  type t =
    | @as(1) Desktop
    | @as(2) Web
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
  @module("vscode") @scope("env") external uiKind: UIKind.t = "uiKind"
  @module("vscode") @scope("env")
  external uriScheme: string = "uriScheme"

  // functions
  @module("vscode") @scope("env")
  external asExternalUri: Uri.t => promise<Uri.t> = "asExternalUri"
  @module("vscode") @scope("env")
  external openExternal: Uri.t => promise<bool> = "openExternal"
}

// https://code.visualstudio.com/api/references/vscode-api#ViewColumn
// 1.95
module ViewColumn = {
  type t =
    | @as(-2) Beside
    | @as(-1) Active
    | @as(1) One
    | @as(2) Two
    | @as(3) Three
    | @as(4) Four
    | @as(5) Five
    | @as(6) Six
    | @as(7) Seven
    | @as(8) Eight
    | @as(9) Nine
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
  @send
  external onDidReceiveMessage: (t, 'a => unit) => Disposable.t = "onDidReceiveMessage"
  // properties
  @get external cspSource: t => string = "cspSource"
  @get external html: t => string = "html"
  @set external setHtml: (t, string) => unit = "html"
  @get external options: t => WebviewOptions.t = "options"
  // methods
  @send external asWebviewUri: (t, Uri.t) => Uri.t = "asWebviewUri"
  @send external postMessage: (t, 'a) => promise<bool> = "postMessage"
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
  @send
  external onDidChangeViewState: (t, OnDidChangeViewStateEvent.t => unit) => Disposable.t =
    "onDidChangeViewState"
  @send
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
  @get external viewColumn: t => option<ViewColumn.t> = "viewColumn"
  @get external viewType: t => string = "viewType"
  @get external visible: t => bool = "visible"
  @get external webview: t => Webview.t = "webview"
  // methods
  @send external dispose: t => unit = "dispose"
  @send
  external reveal: (t, ~viewColumn: ViewColumn.t=?, ~preserveFocus: bool=?) => unit = "reveal"
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
  @send external compareTo: (t, t) => int = "compareTo"
  @send external isAfter: (t, t) => bool = "isAfter"
  @send external isAfterOrEqual: (t, t) => bool = "isAfterOrEqual"
  @send external isBefore: (t, t) => bool = "isBefore"
  @send external isBeforeOrEqual: (t, t) => bool = "isBeforeOrEqual"
  @send external isEqual: (t, t) => bool = "isEqual"
  @send external translate: (t, int, int) => t = "translate"
  @send external with_: (t, int, int) => t = "with"
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
  @send external contains: (t, Position.t) => bool = "contains"
  @send external containsRange: (t, t) => bool = "contains"
  @send external intersection: (t, t) => option<t> = "intersection"
  @send external isEqual: (t, t) => bool = "isEqual"
  @send external union: (t, t) => t = "union"
  @send external with_: (t, Position.t, Position.t) => t = "with"
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
// 1.95
module EndOfLine = {
  type t =
    | @as(1) LF
    | @as(2) CRLF
}

module TextDocument = {
  type t
  // properties
  @get external eol: t => EndOfLine.t = "eol"
  @get external fileName: t => string = "fileName"
  @get external isClosed: t => bool = "isClosed"
  @get external isDirty: t => bool = "isDirty"
  @get external isUntitled: t => bool = "isUntitled"
  @get external languageId: t => string = "languageId"
  @get external lineCount: t => int = "lineCount"
  @get external uri: t => Uri.t = "uri"
  @get external version: t => int = "version"
  // methods
  @send external getText: (t, option<Range.t>) => string = "getText"
  @send
  external getWordRangeAtPosition: (t, Position.t, option<Js.Re.t>) => option<Range.t> =
    "getWordRangeAtPosition"
  @send external lineAt: (t, int) => TextLine.t = "lineAt"
  @send external lineAtPosition: (t, Position.t) => TextLine.t = "lineAt"
  @send external offsetAt: (t, Position.t) => int = "offsetAt"
  @send external positionAt: (t, int) => Position.t = "positionAt"
  @send external save: t => promise<bool> = "save"
  @send
  external validatePosition: (t, Position.t) => Position.t = "validatePosition"
  @send external validateRange: (t, Range.t) => Range.t = "validateRange"
}

// https://code.visualstudio.com/api/references/vscode-api#TextEditorCursorStyle
// 1.95
module TextEditorCursorStyle = {
  type t =
    | @as(1) Line
    | @as(2) Block
    | @as(3) Underline
    | @as(4) LineThin
    | @as(5) BlockOutline
    | @as(6) UnderlineThin
}

// https://code.visualstudio.com/api/references/vscode-api#TextEditorLineNumbersStyle
// 1.95
module TextEditorLineNumbersStyle = {
  type t =
    | @as(0) Off
    | @as(1) On
    | @as(2) Relative
    | @as(3) Interval
}

// https://code.visualstudio.com/api/references/vscode-api#TextEditorOptions
module TextEditorOptions = {
  type t
  // properties
  @get external cursorStyle: t => option<TextEditorCursorStyle.t> = "cursorStyle"
  @get
  external insertSpaces: t => option<StringOr.t<bool>> = "insertSpaces"
  @get external lineNumbers: t => option<TextEditorLineNumbersStyle.t> = "lineNumbers"
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
  @send external contains: (t, Position.t) => bool = "contains"
  @send external containsRange: (t, Range.t) => bool = "contains"
  @send
  external intersection: (t, Range.t) => option<Range.t> = "intersection"
  @send external isEqual: (t, Range.t) => bool = "isEqual"
  @send external union: (t, Range.t) => Range.t = "union"
  @send external with_: (t, Position.t, Position.t) => Range.t = "with"
}

// https://code.visualstudio.com/api/references/vscode-api#TextEditorEdit
module TextEditorEdit = {
  type t
  // methods
  @send external delete: (t, Range.t) => unit = "delete"
  @send external deleteAtSelection: (t, Selection.t) => unit = "delete"
  @send external insert: (t, Position.t, string) => unit = "insert"
  @send external replace: (t, Position.t, string) => unit = "replace"
  @send external replaceAtRange: (t, Range.t, string) => unit = "replace"
  @send
  external replaceAtSelection: (t, Selection.t, string) => unit = "replace"
  @send external setEndOfLine: (t, EndOfLine.t) => unit = "setEndOfLine"
}

// https://code.visualstudio.com/api/references/vscode-api#SnippetString
module SnippetString = {
  type t
}

// https://code.visualstudio.com/api/references/vscode-api#TextEditorRevealType
// 1.95
module TextEditorRevealType = {
  type t =
    | @as(0) Default
    | @as(1) InCenter
    | @as(2) InCenterIfOutsideViewport
    | @as(3) AtTop
}

// https://code.visualstudio.com/api/references/vscode-api#TextEditorDecorationType
module TextEditorDecorationType = {
  type t
  // properties
  @get external key: t => string = "key"
  // methods
  @send external dispose: t => unit = "dispose"
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
  @send
  external appendCodeblock: (t, string, option<string>) => t = "appendCodeblock"
  @send external appendMarkdown: (t, string) => t = "appendMarkdown"
  @send external appendText: (t, string) => t = "appendText"
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
  @get external viewColumn: t => option<ViewColumn.t> = "viewColumn"
  @get external visibleRanges: t => array<Range.t> = "visibleRanges"
  // methods
  @send
  external edit: (
    t,
    TextEditorEdit.t => unit,
    option<{"undoStopAfter": bool, "undoStopBefore": bool}>,
  ) => promise<bool> = "edit"
  @send external hide: (t, unit) => unit = "hide"
  @send
  external insertSnippet: (
    t,
    SnippetString.t,
    Position.t,
    option<{"undoStopAfter": bool, "undoStopBefore": bool}>,
  ) => promise<bool> = "insertSnippet"
  @send
  external insertSnippetAtRange: (
    t,
    SnippetString.t,
    Range.t,
    option<{"undoStopAfter": bool, "undoStopBefore": bool}>,
  ) => promise<bool> = "insertSnippet"
  @send
  external insertSnippetAtPositions: (
    t,
    SnippetString.t,
    array<Position.t>,
    option<{"undoStopAfter": bool, "undoStopBefore": bool}>,
  ) => promise<bool> = "insertSnippet"
  @send
  external insertSnippetAtRanges: (
    t,
    SnippetString.t,
    array<Range.t>,
    option<{"undoStopAfter": bool, "undoStopBefore": bool}>,
  ) => promise<bool> = "insertSnippet"
  @send
  external revealRange: (t, Range.t, option<TextEditorRevealType.t>) => unit = "revealRange"
  @send
  external setDecorations: (t, TextEditorDecorationType.t, array<Range.t>) => unit =
    "setDecorations"
  @send
  external setDecorationsWithOptions: (
    t,
    TextEditorDecorationType.t,
    array<DecorationOptions.t>,
  ) => unit = "setDecorations"
  @send external show: (t, option<ViewColumn.t>) => unit = "show"
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
  @send external dispose: t => unit = "dispose"
  @send external hide: t => unit = "hide"
  @send external sendText: (t, string) => unit = "sendText"
  @send
  external sendTextWithOptions: (t, string, bool) => unit = "sendText"
  @send external show: t => unit = "show"
  @send external showWithOptions: (t, bool) => unit = "show"
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
// 1.95
module TextEditorSelectionChangeKind = {
  type t =
    | @as(1) Keyboard
    | @as(2) Mouse
    | @as(3) Command
}
// https://code.visualstudio.com/api/references/vscode-api#TextEditorSelectionChangeEvent
module TextEditorSelectionChangeEvent = {
  type t
  // properties
  @get
  external kind: t => option<TextEditorSelectionChangeKind.t> = "kind"
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

// https://github.com/microsoft/vscode/blob/ad42640a78864e43008b8d5b750d04e96dd05218/src/vscode-dts/vscode.d.ts#L953
// 1.95
module IconPath = {
  type case = Uri(Uri.t) | ThemeIcon(ThemeIcon.t) | DarkAndLight({"dark": Uri.t, "light": Uri.t})
  type t

  external fromUri: Uri.t => t = "%identity"
  external fromDarkAndLight: {"dark": Uri.t, "light": Uri.t} => t = "%identity"
  external fromThemeIcon: ThemeIcon.t => t = "%identity"

  external toUri: 'a => Uri.t = "%identity"
  external toDarkAndLight: 'a => {"dark": Uri.t, "light": Uri.t} = "%identity"
  external toThemeIcon: 'a => ThemeIcon.t = "%identity"

  let case = (raw: 'a): case =>
    switch raw->Object.get("color") {
    | Some(_) => Uri(toUri(raw))
    | None =>
      switch raw->Object.get("dark") {
      | Some(_) => DarkAndLight(toDarkAndLight(raw))
      | None => ThemeIcon(toThemeIcon(raw))
      }
    }
}

// https://code.visualstudio.com/api/references/vscode-api#QuickInput
// 1.95
module QuickInput = {
  type t

  // events
  @send external onDidHide: (t, unit => unit) => Disposable.t = "onDidHide"

  // properties
  @get external busy: t => bool = "busy"
  @set external setBusy: (t, bool) => unit = "busy"

  @get external enabled: t => bool = "enabled"
  @set external setEnabled: (t, bool) => unit = "enabled"

  @get external ignoreFocusOut: t => bool = "ignoreFocusOut"
  @set external setIgnoreFocusOut: (t, bool) => unit = "ignoreFocusOut"

  @get external step: t => int = "step"
  @set external setStep: (t, int) => unit = "step"

  @get external title: t => string = "title"
  @set external setTitle: (t, string) => unit = "title"

  @get external totalSteps: t => int = "totalSteps"
  @set external setTotalSteps: (t, int) => unit = "totalSteps"

  // methods
  @send external dispose: t => unit = "dispose"
  @send external hide: t => unit = "hide"
  @send external show: t => unit = "show"
}

// https://code.visualstudio.com/api/references/vscode-api#QuickInputButton
// 1.95
module QuickInputButton = {
  type t = {
    iconPath: IconPath.t,
    tooltip: option<string>,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#QuickInputButtons
// 1.95
module QuickInputButtons = {
  type t
  // static
  @module("vscode") @scope("QuickInputButtons")
  external back: t => QuickInputButton.t = "Back"
}

// https://code.visualstudio.com/api/references/vscode-api#QuickPickItemKind
// 1.95
module QuickPickItemKind = {
  type t =
    | @as(-1) Separator
    | @as(0) Default
}

// https://code.visualstudio.com/api/references/vscode-api#QuickPickItem
// 1.95
module QuickPickItem = {
  type t = {
    alwaysShow?: bool,
    buttons?: array<QuickInputButton.t>,
    description?: string,
    detail?: string,
    iconPath?: IconPath.t,
    kind?: QuickPickItemKind.t,
    label: string,
    picked?: bool,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#QuickPickItemButtonEvent
// 1.95
module QuickPickItemButtonEvent = {
  type t<'a>
  // properties
  @get external button: t<'a> => QuickInputButton.t = "button"
  @get external item: t<'a> => 'a = "item"
}

// https://code.visualstudio.com/api/references/vscode-api#QuickPickOptions
// 1.95
module QuickPickOptions = {
  type t = {
    onDidSelectItem?: StringOr.t<QuickPickItem.t> => unit,
    canPickMany?: bool,
    ignoreFocusOut?: bool,
    matchOnDescription?: bool,
    matchOnDetail?: bool,
    placeHolder?: string,
    title?: string,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#QuickPick
// 1.95
module QuickPick = {
  type t<'a>
  // events
  @send external onDidAccept: (t<'a>, unit => unit) => Disposable.t = "onDidAccept"
  @send external onDidChangeActive: (t<'a>, array<'a> => unit) => Disposable.t = "onDidChangeActive"
  @send
  external onDidChangeSelection: (t<'a>, array<'a> => unit) => Disposable.t = "onDidChangeSelection"
  @send external onDidChangeValue: (t<'a>, string => unit) => Disposable.t = "onDidChangeValue"
  @send external onDidHide: (t<'a>, unit => unit) => Disposable.t = "onDidHide"
  @send
  external onDidTriggerButton: (t<'a>, QuickInputButton.t => unit) => Disposable.t =
    "onDidTriggerButton"
  @send
  external onDidTriggerItemButton: (t<'a>, QuickPickItemButtonEvent.t<'a> => unit) => Disposable.t =
    "onDidTriggerItemButton"
  // properties
  @get external activeItems: t<'a> => array<'a> = "activeItems"

  @get external busy: t<'a> => bool = "busy"
  @set external setBusy: (t<'a>, bool) => unit = "busy"

  @get external buttons: t<'a> => array<QuickInputButton.t> = "buttons"

  @get external canSelectMany: t<'a> => bool = "canSelectMany"
  @set external setCanSelectMany: (t<'a>, bool) => unit = "canSelectMany"

  @get external enabled: t<'a> => bool = "enabled"
  @set external setEnabled: (t<'a>, bool) => unit = "enabled"

  @get external ignoreFocusOut: t<'a> => bool = "ignoreFocusOut"
  @set external setIgnoreFocusOut: (t<'a>, bool) => unit = "ignoreFocusOut"

  @get external items: t<'a> => array<'a> = "items"
  @set external setItems: (t<'a>, array<'a>) => unit = "items"

  @get external keepScrollPosition: t<'a> => option<bool> = "keepScrollPosition"
  @set external setKeepScrollPosition: (t<'a>, option<bool>) => unit = "keepScrollPosition"

  @get external matchOnDescription: t<'a> => bool = "matchOnDescription"
  @set external setMatchOnDescription: (t<'a>, bool) => unit = "matchOnDescription"

  @get external matchOnDetail: t<'a> => bool = "matchOnDetail"
  @set external setMatchOnDetail: (t<'a>, bool) => unit = "matchOnDetail"

  @get external placeholder: t<'a> => option<string> = "placeholder"
  @set external setPlaceholder: (t<'a>, string) => unit = "placeholder"

  @get external selectedItems: t<'a> => array<'a> = "selectedItems"
  @set external setSelectedItems: (t<'a>, array<'a>) => unit = "selectedItems"

  @get external step: t<'a> => int = "step"
  @set external setStep: (t<'a>, int) => unit = "step"

  @get external title: t<'a> => string = "title"
  @set external setTitle: (t<'a>, string) => unit = "title"

  @get external totalSteps: t<'a> => int = "totalSteps"
  @set external setTotalSteps: (t<'a>, int) => unit = "totalSteps"

  @get external value: t<'a> => string = "value"
  @set external setValue: (t<'a>, string) => unit = "value"

  // methods
  @send external dispose: t<'a> => unit = "dispose"
  @send external hide: t<'a> => unit = "hide"
  @send external show: t<'a> => unit = "show"
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
// 1.95.0
module StatusBarAlignment = {
  type t =
    | @as(1) Left
    | @as(2) Right
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
  @get external alignment: t => StatusBarAlignment.t = "alignment"
  @set
  external setAlignment: (t, StatusBarAlignment.t) => unit = "alignment"
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
  @send external dispose: t => unit = "dispose"
  @send external hide: t => unit = "hide"
  @send external show: t => unit = "show"
}

// https://code.visualstudio.com/api/references/vscode-api#OverviewRulerLane;
// 1.95
module OverviewRulerLane = {
  type t =
    | @as(1) Left
    | @as(2) Center
    | @as(4) Right
    | @as(7) Full
}

// https://code.visualstudio.com/api/references/vscode-api#DecorationRangeBehavior;
// 1.95
module DecorationRangeBehavior = {
  type t =
    | @as(0) OpenOpen
    | @as(1) ClosedClosed
    | @as(2) OpenClosed
    | @as(3) ClosedOpen
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
    overviewRulerLane: OverviewRulerLane.t,
    @optional
    rangeBehavior: DecorationRangeBehavior.t,
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
    enableCommandUris,
    enableScripts,
    localResourceRoots,
    portMapping,
    enableFindWidget,
    retainContextWhenHidden,
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

// https://code.visualstudio.com/api/references/vscode-api#MessageItem
// 1.96
module MessageItem = {
  type t = {
    title: string,
    isCloseAffordance?: bool,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#MessageOptions
// 1.96
module MessageOptions = {
  type t = {
    modal?: bool,
    detail?: string,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#InputBoxValidationSeverity
// 1.95

module InputBoxValidationSeverity = {
  type t =
    | @as(1) Info
    | @as(2) Warning
    | @as(3) Error
}

// https://code.visualstudio.com/api/references/vscode-api#InputBoxValidationMessage
// 1.95
module InputBoxValidationMessage = {
  type t = {
    message: string,
    severity: InputBoxValidationSeverity.t,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#InputBoxOptions
module InputBoxOptions = {
  type t = {
    ignoreFocusOut?: bool,
    password?: bool,
    placeHolder?: string,
    prompt?: string,
    title?: string,
    value?: string,
    valueSelection?: (int, int),
    validateInput?: string => PromiseOr.t<option<StringOr.t<InputBoxValidationMessage.t>>>,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#CancellationToken
// 1.96
module CancellationToken = {
  @unboxed
  type rec any = Any('x): any
  type t = {
    isCancellationRequested: bool,
    onCancellationRequested: (any => unit) => Disposable.t,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#CancellationTokenSource
// 1.96
module CancellationTokenSource = {
  type t
  // properties
  @get external token: t => CancellationToken.t = "token"
  // methods
  @send external cancel: t => unit = "cancel"
  @send external dispose: t => unit = "dispose"
}

// https://code.visualstudio.com/api/references/vscode-api#OpenDialogOptions
// 1.96
module OpenDialogOptions = {
  type t = {
    defaultUri?: Uri.t,
    openLabel?: string,
    canSelectFiles?: bool,
    canSelectFolders?: bool,
    canSelectMany?: bool,
    filters?: Dict.t<array<string>>,
    title?: string,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#SaveDialogOptions
// 1.96
module SaveDialogOptions = {
  type t = {
    defaultUri?: Uri.t,
    saveLabel?: string,
    filters?: Dict.t<array<string>>,
    title?: string,
  }
}
// https://code.visualstudio.com/api/references/vscode-api#WorkspaceFolderPickOptions
module WorkspaceFolderPickOptions = {
  type t
  // properties
  @get external ignoreFocusOut: t => option<bool> = "ignoreFocusOut"
  @get external placeHolder: t => option<string> = "placeHolder"
}

// https://code.visualstudio.com/api/references/vscode-api#Progress
// 1.96
module Progress = {
  type t<'a>
  // methods
  @send external report: (t<'a>, 'a) => unit = "report"
}

// https://code.visualstudio.com/api/references/vscode-api#ProgressLocation
// 1.96
module ProgressLocation = {
  type t =
    | @as(1) SourceControl
    | @as(10) Window
    | @as(15) Notification
}

// https://code.visualstudio.com/api/references/vscode-api#ProgressOptions
// 1.96
module ProgressOptions = {
  type t = {
    cancellable?: bool,
    location: ProgressLocation.t,
    title?: string,
  }
}


// https://code.visualstudio.com/api/references/vscode-api#ProgressOptions
// 1.96
module ProcessOptionsWithViewIdAsLocation = {
  type t = {
    cancellable?: bool,
    location: {"viewId": string},
    title?: string,
  }
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
    preserveFocus,
    preview,
    selection,
    viewColumn,
  }
}

// https://code.visualstudio.com/api/references/vscode-api#ColorThemeKind;
// 1.95
module ColorThemeKind = {
  type t =
    | @as(1) Light
    | @as(2) Dark
    | @as(3) HighContrast
    | @as(4) HighContrastLight
}

// https://code.visualstudio.com/api/references/vscode-api#ColorTheme
// 1.95.0
module ColorTheme = {
  type t
  // properties
  @get external kind: t => ColorThemeKind.t = "kind"
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
//     option(promise('a)) =
//     "openCustomDocument";
//   [@bs.send]
//   external resolveCustomEditor:
//     (t('a), 'a, WebviewPanel.t, CancellationToken.t) =>
//     option(promise(unit)) =
//     "resolveCustomEditor";
// };

// https://code.visualstudio.com/api/references/vscode-api#CustomReadonlyEditorProvider
module CustomReadonlyEditorProvider = {
  type t<'a>
  // methods
  @send
  external openCustomDocument: (
    t<'a>,
    Uri.t,
    CustomDocumentOpenContext.t,
    CancellationToken.t,
  ) => option<promise<'a>> = "openCustomDocument"
  @send
  external resolveCustomEditor: (
    t<'a>,
    'a,
    WebviewPanel.t,
    CancellationToken.t,
  ) => option<promise<unit>> = "resolveCustomEditor"
}

// https://code.visualstudio.com/api/references/vscode-api#CustomTextEditorProvider
module CustomTextEditorProvider = {
  type t
  // methods
  @send
  external resolveCustomTextEditor: (
    t,
    TextDocument.t,
    WebviewPanel.t,
    CancellationToken.t,
  ) => option<promise<unit>> = "resolveCustomTextEditor"
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
  @send
  external onDidChangeVisibility: (t, unit => unit) => Disposable.t = "onDidChangeVisibility"
  @send
  external onDidDispose: (t, unit => unit) => Disposable.t = "onDidDispose"
  // properties
  @get external description: t => option<string> = "description"
  @get external title: t => option<string> = "title"
  @get external viewType: t => string = "viewType"
  @get external visible: t => bool = "visible"
  @get external webview: t => Webview.t = "webview"
  // methods
  @send external show: t => unit = "show"
  @send external showWithOptions: (t, bool) => unit = "show"
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
  @send
  external resolveWebviewView: (
    t,
    WebviewView.t,
    WebviewViewResolveContext.t<'a>,
    CancellationToken.t,
  ) => option<promise<unit>> = "resolveWebviewView"
}

// https://code.visualstudio.com/api/references/vscode-api#UriHandler
// 1.51.0
module UriHandler = {
  type t
  // methods
  @send
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
  @send
  external onDidChangeFileDecorations: (t, option<ArrayOr.t<Uri.t>> => unit) => Disposable.t =
    "onDidChangeFileDecorations"
  // methods
  @send
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
  external onDidChangeActiveColorTheme: (ColorTheme.t => unit) => Disposable.t =
    "onDidChangeActiveColorTheme"
  @module("vscode") @scope("window")
  external onDidChangeActiveTerminal: (option<Terminal.t> => unit) => Disposable.t =
    "onDidChangeActiveTerminal"
  @module("vscode") @scope("window")
  external onDidChangeActiveTextEditor: (option<TextEditor.t> => unit) => Disposable.t =
    "onDidChangeActiveTextEditor"
  @module("vscode") @scope("window")
  external onDidChangeTextEditorOptions: (TextEditorOptionsChangeEvent.t => unit) => Disposable.t =
    "onDidChangeTextEditorOptions"
  @module("vscode") @scope("window")
  external onDidChangeTextEditorSelection: (
    TextEditorSelectionChangeEvent.t => unit
  ) => Disposable.t = "onDidChangeTextEditorSelection"
  @module("vscode") @scope("window")
  external onDidChangeTextEditorViewColumn: (
    TextEditorViewColumnChangeEvent.t => unit
  ) => Disposable.t = "onDidChangeTextEditorViewColumn"
  @module("vscode") @scope("window")
  external onDidChangeTextEditorVisibleRanges: (
    TextEditorVisibleRangesChangeEvent.t => unit
  ) => Disposable.t = "onDidChangeTextEditorVisibleRanges"
  @module("vscode") @scope("window")
  external onDidChangeVisibleTextEditors: (array<TextEditor.t> => unit) => Disposable.t =
    "onDidChangeVisibleTextEditors"
  @module("vscode") @scope("window")
  external onDidChangeWindowState: (WindowState.t => unit) => Disposable.t =
    "onDidChangeWindowState"
  @module("vscode") @scope("window")
  external onDidCloseTerminal: (Terminal.t => unit) => Disposable.t = "onDidCloseTerminal"
  @module("vscode") @scope("window")
  external onDidOpenTerminal: (Terminal.t => unit) => Disposable.t = "onDidOpenTerminal"

  // functions
  @module("vscode") @scope("window")
  external createInputBox: unit => InputBox.t = "createInputBox"
  @module("vscode") @scope("window")
  external createOutputChannel: string => OutputChannel.t = "createOutputChannel"
  @module("vscode") @scope("window")
  external createQuickPick: unit => QuickPick.t<'a> = "createQuickPick"
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
  external setStatusBarMessageAndHideWhenDone: (string, promise<'a>) => Disposable.t =
    "setStatusBarMessage"
  @module("vscode") @scope("window")
  external setStatusBarMessage: string => Disposable.t = "setStatusBarMessage"
  @module("vscode") @scope("window") @variadic
  external showErrorMessage: (string, array<string>) => promise<option<string>> = "showErrorMessage"
  @module("vscode") @scope("window") @variadic
  external showErrorMessageWithOptions: (
    string,
    MessageOptions.t,
    array<string>,
  ) => promise<option<string>> = "showErrorMessage"
  @module("vscode") @scope("window") @variadic
  external showErrorMessageWithMessageItem: (
    string,
    array<MessageItem.t>,
  ) => promise<option<MessageItem.t>> = "showErrorMessage"
  @module("vscode") @scope("window") @variadic
  external showErrorMessageWithOptionsAndMessageItem: (
    string,
    MessageOptions.t,
    array<MessageItem.t>,
  ) => promise<option<MessageItem.t>> = "showErrorMessage"
  @module("vscode") @scope("window") @variadic
  external showInformationMessage: (string, array<string>) => promise<option<string>> =
    "showInformationMessage"
  @module("vscode") @scope("window") @variadic
  external showInformationMessageWithOptions: (
    string,
    MessageOptions.t,
    array<string>,
  ) => promise<option<string>> = "showInformationMessage"
  @module("vscode") @scope("window") @variadic
  external showInformationMessageWithMessageItem: (
    string,
    array<MessageItem.t>,
  ) => promise<option<MessageItem.t>> = "showInformationMessage"
  @module("vscode") @scope("window") @variadic
  external showInformationMessageWithOptionsAndMessageItem: (
    string,
    MessageOptions.t,
    array<MessageItem.t>,
  ) => promise<option<MessageItem.t>> = "showInformationMessage"
  @module("vscode") @scope("window")
  external showInputBox: (
    ~option: InputBoxOptions.t=?,
    ~token: CancellationToken.t=?,
  ) => promise<option<string>> = "showInputBox"
  @module("vscode") @scope("window")
  external showOpenDialog: OpenDialogOptions.t => promise<option<Uri.t>> = "shoeOpenDialog"
  @module("vscode") @scope("window")
  external showQuickPick: (
    promise<array<string>>,
    QuickPickOptions.t,
    option<CancellationToken.t>,
  ) => promise<option<array<string>>> = "showQuickPick"
  @module("vscode") @scope("window")
  external showSaveDialog: SaveDialogOptions.t => promise<option<Uri.t>> = "showSaveDialog"
  @module("vscode") @scope("window")
  external showTextDocument: (
    TextDocument.t,
    ~column: ViewColumn.t=?,
    ~preserveFocus: bool=?,
    unit,
  ) => promise<TextEditor.t> = "showTextDocument"
  @module("vscode") @scope("window")
  external showTextDocumentWithShowOptions: (
    TextDocument.t,
    option<TextDocumentShowOptions.t>,
  ) => promise<TextEditor.t> = "showTextDocument"
  @module("vscode") @scope("window")
  external showTextDocumentWithUri: (
    Uri.t,
    option<TextDocumentShowOptions.t>,
  ) => promise<TextEditor.t> = "showTextDocument"
  @module("vscode") @scope("window") @variadic
  external showWarningMessage: (string, array<string>) => promise<option<string>> =
    "showWarningMessage"
  @module("vscode") @scope("window") @variadic
  external showWarningMessageWithOptions: (
    string,
    MessageOptions.t,
    array<string>,
  ) => promise<option<string>> = "showWarningMessage"
  @module("vscode") @scope("window") @variadic
  external showWarningMessageWithMessageItem: (
    string,
    array<MessageItem.t>,
  ) => promise<option<MessageItem.t>> = "showWarningMessage"
  @module("vscode") @scope("window") @variadic
  external showWarningMessageWithOptionsAndMessageItem: (
    string,
    MessageOptions.t,
    array<MessageItem.t>,
  ) => promise<option<MessageItem.t>> = "showWarningMessage"
  @module("vscode") @scope("window")
  external showWorkspaceFolderPick: option<WorkspaceFolderPickOptions.t> => promise<
    option<WorkspaceFolder.t>,
  > = "showWorkspaceFolderPick"
  @module("vscode") @scope("window")
  external withProgress: (
    ProgressOptions.t,
    (Progress.t<{"increment": int, "message": string}>, CancellationToken.t) => promise<'a>,
  ) => promise<'a> = "withProgress"
  @module("vscode") @scope("window")
  external withScmProgress: ((Progress.t<int>, CancellationToken.t) => promise<'a>) => promise<'a> =
    "withScmProgress"
}

// https://code.visualstudio.com/api/references/vscode-api#FileType
// 1.95
module FileType = {
  type t =
    | @as(0) Unknown
    | @as(1) File
    | @as(2) Directory
    | @as(64) SymbolicLink
}

// https://code.visualstudio.com/api/references/vscode-api#FileStat
module FileStat = {
  type t
  @get external ctime: t => int = "ctime"
  @get external mtime: t => int = "mtime"
  @get external size: t => int = "size"
  @get external type_: t => FileType.t = "type"
}

// https://code.visualstudio.com/api/references/vscode-api#FileSystem
module FileSystem = {
  type t
  // methods
  @send external copy: (t, Uri.t, Uri.t) => promise<unit> = "copy"
  @send
  external copyWithOptions: (t, Uri.t, Uri.t, {"overwrite": bool}) => promise<unit> = "copy"
  @send
  external createDirectory: (t, Uri.t) => promise<unit> = "createDirectory"
  @send external delete: (t, Uri.t) => promise<unit> = "delete"
  @send
  external deleteWithOptions: (t, Uri.t, {"recursive": bool, "useTrash": bool}) => promise<unit> =
    "delete"
  @send
  external readDirectory: (t, Uri.t) => promise<array<StringOr.t<FileType.t>>> = "readDirectory"
  @send
  external readFile: (t, Uri.t) => promise<Js.TypedArray2.Int8Array.t> = "readFile"
  @send external rename: (t, Uri.t, Uri.t) => promise<unit> = "rename"
  @send
  external renameWithOptions: (t, Uri.t, Uri.t, {"overwrite": bool}) => promise<unit> = "rename"
  @send external stat: (t, Uri.t) => promise<FileStat.t> = "stat"
  @send
  external stwriteFileat: (t, Uri.t, Js.TypedArray2.Uint8Array.t) => promise<unit> = "writeFile"
}

// https://code.visualstudio.com/api/references/vscode-api#ConfigurationChangeEvent
module ConfigurationChangeEvent = {
  type t

  @send
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
  external setEndOfLine: EndOfLine.t => t = "setEndOfLine"

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
  @send
  external createFile: (
    t,
    Uri.t,
    option<{"ignoreIfExists": bool, "overwrite": bool}>,
    option<WorkspaceEditEntryMetadata.t>,
    unit,
  ) => unit = "createFile"
  @send
  external delete: (t, Uri.t, Range.t, option<WorkspaceEditEntryMetadata.t>) => unit = "delete"
  @send
  external deleteFile: (
    t,
    Uri.t,
    Range.t,
    option<{"ignoreIfNotExists": bool, "recursive": bool}>,
    option<WorkspaceEditEntryMetadata.t>,
  ) => unit = "deleteFile"
  @send external entries_raw: t => array<'shit> = "entries"
  let entries = (self: t): array<(Uri.t, array<TextEdit.t>)> =>
    entries_raw(self)->Array.map(shit => {
      let toUri = %raw("function (shit) { return shit[0] }")
      let toTextEdits = %raw("function (shit) { return shit[1] }")
      (toUri(shit), toTextEdits(shit))
    })
  @send external get: (t, Uri.t) => array<TextEdit.t> = "get"
  @send external has: (t, Uri.t) => bool = "has"
  @send
  external insert: (t, Uri.t, Position.t, string, option<WorkspaceEditEntryMetadata.t>) => unit =
    "insert"
  @send
  external renameFile: (
    t,
    Uri.t,
    Uri.t,
    option<{"ignoreIfExists": bool, "overwrite": bool}>,
    option<WorkspaceEditEntryMetadata.t>,
  ) => unit = "renameFile"
  @send
  external replace: (t, Uri.t, Range.t, string, option<WorkspaceEditEntryMetadata.t>) => unit =
    "replace"
  @send external set: (t, Uri.t, array<TextEdit.t>) => unit = "set"
}

// https://code.visualstudio.com/api/references/vscode-api#FileWillCreateEvent
module FileWillCreateEvent = {
  type t
  // properties
  @get external files: t => array<Uri.t> = "files"
  // methods
  @send
  external waitUntilWithWorkspaceEdit: (t, promise<WorkspaceEdit.t>) => unit = "waitUntil"
  @send external waitUntil: (t, promise<'a>) => unit = "waitUntil"
}
// https://code.visualstudio.com/api/references/vscode-api#FileWillDeleteEvent
module FileWillDeleteEvent = {
  type t
  // properties
  @get external files: t => array<Uri.t> = "files"
  // methods
  @send
  external waitUntilWithWorkspaceEdit: (t, promise<WorkspaceEdit.t>) => unit = "waitUntil"
  @send external waitUntil: (t, promise<'a>) => unit = "waitUntil"
}
// https://code.visualstudio.com/api/references/vscode-api#FileWillRenameEvent
module FileWillRenameEvent = {
  type t
  // properties
  @get
  external files: t => array<{"newUri": Uri.t, "oldUri": Uri.t}> = "files"
  // methods
  @send
  external waitUntilWithWorkspaceEdit: (t, promise<WorkspaceEdit.t>) => unit = "waitUntil"
  @send external waitUntil: (t, promise<'a>) => unit = "waitUntil"
}

// https://code.visualstudio.com/api/references/vscode-api#TextDocumentSaveReason
// 1.95
module TextDocumentSaveReason = {
  type t =
    | @as(1) Manual
    | @as(2) AfterDelay
    | @as(3) FocusOut
}

// https://code.visualstudio.com/api/references/vscode-api#TextDocumentWillSaveEvent
// 1.95
module TextDocumentWillSaveEvent = {
  type t
  // properties
  @get external document: t => TextDocument.t = "document"
  @get external reason: t => TextDocumentSaveReason.t = "reason"
  // methods
  @send
  external waitUntilWithTextEdit: (t, promise<array<TextEdit.t>>) => unit = "waitUntil"
  @send external waitUntil: (t, promise<'a>) => unit = "waitUntil"
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
  @send
  external onDidChange: (t, Uri.t => unit) => Disposable.t = "onDidChange"
  @send
  external onDidCreate: (t, Uri.t => unit) => Disposable.t = "onDidCreate"
  @send
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
  @send external dispose: t => 'a = "dispose"
}

// https://code.visualstudio.com/api/references/vscode-api#WorkspaceConfiguration
module WorkspaceConfiguration = {
  type t
  // methods
  @send external get: (t, string) => option<'a> = "get"
  @send external getWithDefault: (t, string, 'a) => 'a = "get"
  @send external has: (t, string) => bool = "has"
  @send
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
  @send
  external updateGlobalSettings: (t, string, 'a, @as(1) _, option<bool>) => promise<unit> = "update"
  @send
  external updateWorkspaceSettings: (t, string, 'a, @as(2) _, option<bool>) => promise<unit> =
    "update"
  @send
  external updateWorkspaceFolderSettings: (t, string, 'a, @as(3) _, option<bool>) => promise<unit> =
    "update"
}

// https://code.visualstudio.com/api/references/vscode-api#TextDocumentContentProvider
// 1.59.0
module TextDocumentContentProvider = {
  type t
  // events
  @module("vscode") @scope("workspace")
  external onDidChange: option<(Uri.t => unit) => Disposable.t> = "onDidChange"
  // methods
  @send
  external provideTextDocumentContent: (t, Uri.t, CancellationToken.t) => ProviderResult.t<string> =
    "provideTextDocumentContent"
}

// https://code.visualstudio.com/api/references/vscode-api#TaskProvider
// 1.59.0
module TaskProvider = {
  type t<'a>
  // methods
  @send
  external provideTasks: (t<'a>, CancellationToken.t) => ProviderResult.t<array<'a>> =
    "provideTasks"
  @send
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
  external onDidChangeConfiguration: (ConfigurationChangeEvent.t => unit) => Disposable.t =
    "onDidChangeConfiguration"
  @module("vscode") @scope("workspace")
  external onDidChangeTextDocument: (TextDocumentChangeEvent.t => unit) => Disposable.t =
    "onDidChangeTextDocument"

  @module("vscode") @scope("workspace")
  external onDidChangeWorkspaceFolders: (WorkspaceFoldersChangeEvent.t => unit) => Disposable.t =
    "onDidChangeWorkspaceFolders"
  @module("vscode") @scope("workspace")
  external onDidCloseTextDocument: (TextDocument.t => unit) => Disposable.t =
    "onDidCloseTextDocument"
  @module("vscode") @scope("workspace")
  external onDidCreateFiles: (FileCreateEvent.t => unit) => Disposable.t = "onDidCreateFiles"
  @module("vscode") @scope("workspace")
  external onDidDeleteFiles: (FileDeleteEvent.t => unit) => Disposable.t = "onDidDeleteFiles"
  @module("vscode") @scope("workspace")
  external onDidOpenTextDocument: (TextDocument.t => unit) => Disposable.t = "onDidOpenTextDocument"
  @module("vscode") @scope("workspace")
  external onDidRenameFiles: (FileRenameEvent.t => unit) => Disposable.t = "onDidRenameFiles"
  @module("vscode") @scope("workspace")
  external onDidSaveTextDocument: (TextDocument.t => unit) => Disposable.t = "onDidSaveTextDocument"
  @module("vscode") @scope("workspace")
  external onWillCreateFiles: (FileWillCreateEvent.t => unit) => Disposable.t = "onWillCreateFiles"
  @module("vscode") @scope("workspace")
  external onWillDeleteFiles: (FileWillDeleteEvent.t => unit) => Disposable.t = "onWillDeleteFiles"
  @module("vscode") @scope("workspace")
  external onWillRenameFiles: (FileWillRenameEvent.t => unit) => Disposable.t = "onWillRenameFiles"
  @module("vscode") @scope("workspace")
  external onWillSaveTextDocument: (TextDocumentWillSaveEvent.t => unit) => Disposable.t =
    "onWillSaveTextDocument"
  // functions
  @module("vscode") @scope("workspace")
  external applyEdit: WorkspaceEdit.t => promise<bool> = "applyEdit"
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
  ) => promise<array<Uri.t>> = "findFiles"
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
  external openTextDocument: Uri.t => promise<TextDocument.t> = "openTextDocument"
  @module("vscode") @scope("workspace")
  external openTextDocumentWithFileName: string => promise<TextDocument.t> = "openTextDocument"
  @module("vscode") @scope("workspace")
  external openTextDocumentWithOptions: option<{
    "content": string,
    "language": string,
  }> => promise<TextDocument.t> = "openTextDocument"
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
  external saveAll: option<bool> => promise<bool> = "saveAll"
  @module("vscode") @scope("workspace") @variadic
  external updateWorkspaceFolders: (
    int,
    option<int>,
    array<{"name": string, "uri": Uri.t}>,
  ) => promise<bool> = "updateWorkspaceFolders"
}

// https://code.visualstudio.com/api/references/vscode-api#extensions
// 1.52.0
module Extensions = {
  // variables
  @module("vscode") @scope("extensions")
  external all: array<Extension.t<'a>> = "all"
  // events
  @module("vscode") @scope("extensions")
  external onDidChange: (unit => unit) => Disposable.t = "onDidChange"
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
// 1.95
module DiagnosticSeverity = {
  type t =
    | @as(0) Error
    | @as(1) Warning
    | @as(2) Information
    | @as(3) Hint
}

// https://code.visualstudio.com/api/references/vscode-api#DiagnosticTag
// 1.95
module DiagnosticTag = {
  type t =
    | @as(1) Unnecessary
    | @as(2) Deprecated
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
  @send external clear: t => unit = "clear"
  @send external delete: (t, Uri.t) => unit = "delete"
  @send external dispose: t => unit = "dispose"
  @send
  external forEach: (t, (Uri.t, array<Diagnostic.t>, t) => 'a) => unit = "forEach"
  @send external get: (t, Uri.t) => option<array<Diagnostic.t>> = "get"
  @send external has: (t, Uri.t) => bool = "has"
  @send external set: (t, Uri.t) => unit = "set"
  @send
  external setDiagnostics: (t, Uri.t, array<Diagnostic.t>) => unit = "set"
  @send
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
  @send
  external prepareCallHierarchy: (
    t,
    TextDocument.t,
    Position.t,
    CancellationToken.t,
  ) => ProviderResult.t<array<CallHierarchyItem.t>> = "prepareCallHierarchy"
  @send
  external provideCallHierarchyIncomingCalls: (
    t,
    CallHierarchyItem.t,
    CancellationToken.t,
  ) => ProviderResult.t<array<CallHierarchyIncomingCall.t>> = "provideCallHierarchyIncomingCalls"
  @send
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
    onDidChangeCodeLenses: option<(unit => unit) => Disposable.t>,
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
  @send external build: unit => SemanticsTokens.t = "build"
  @send external buildWithResultId: string => SemanticsTokens.t = "build"
  @send
  external push: (int, int, int, int, option<int>) => unit = "push"
  @send
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
  external onDidChangeDiagnostics: (DiagnosticChangeEvent.t => unit) => Disposable.t =
    "onDidChangeDiagnostics"

  // functions
  @module("vscode") @scope("languages")
  external createDiagnosticCollection: option<string> => DiagnosticCollection.t =
    "createDiagnosticCollection"
  @module("vscode") @scope("languages")
  external getDiagnostics: Uri.t => array<Diagnostic.t> = "getDiagnostics"
  @module("vscode") @scope("languages")
  external getDiagnosticEntries: Uri.t => array<(Uri.t, array<Diagnostic.t>)> = "getDiagnostics"
  @module("vscode") @scope("languages")
  external getLanguages: unit => promise<array<string>> = "getLanguages"
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
