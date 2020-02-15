module HotReload.Host

open System
open System.IO
open System.Reflection
open System.Runtime.Loader
open HotReload

// type PluginFromAssembly private (fileLock : FileStream) =
//   interface IDisposable with
//     member this.Dispose () =
//       fileLock.Close ()
//       fileLock.Dispose ()
//       ()

//   with
//     static member TryCreate (pluginPath : string) =

type PluginLoadContext (pluginPath : string) =
  inherit AssemblyLoadContext () with
    let mutable resolver : AssemblyDependencyResolver = AssemblyDependencyResolver (pluginPath)

    // member this.PluginLoadContext () =
    //   resolver <- AssemblyDependencyResolver (pluginPath)

    override this.Load (assemblyName : AssemblyName) =
      let assemblyPath = resolver.ResolveAssemblyToPath (assemblyName)

      if not (isNull assemblyPath)
      then
        base.LoadFromAssemblyPath (assemblyPath)
      else
        null

    override this.LoadUnmanagedDll (unmanagedDllName : string) : IntPtr =
      let libraryPath = resolver.ResolveUnmanagedDllToPath (unmanagedDllName)
      if isNull libraryPath
      then
        IntPtr.Zero
      else
        base.LoadUnmanagedDllFromPath (libraryPath)

open System
open System.Threading

type HotPlugin (pluginPath : string) =
  let source = new CancellationTokenSource ()
  let mutable plugin = None

  let rec workflow lastWriteTime =
    async {
      let writeTime =
        if File.Exists pluginPath
        then
          File.GetLastWriteTimeUtc pluginPath
        else
          printfn "File not found! "
          lastWriteTime

      if writeTime > lastWriteTime
      then
        printfn "Change detected! "

        use stream = File.Open (pluginPath, FileMode.Open, FileAccess.ReadWrite, FileShare.None)

        let loadContext = PluginLoadContext pluginPath

        let assemblyName = AssemblyName (Path.GetFileNameWithoutExtension pluginPath)

        let assembly = loadContext.LoadFromAssemblyName assemblyName

        printfn "Got assembly: %s" assembly.FullName

        stream.Close () // ?????

        printfn "Searching for plugin types... "

        let freshPlugin =
          seq {
            for t in assembly.GetTypes() do
              printfn "Type: %A" t
              let isPlugin =
                not t.IsAbstract &&
                not t.IsInterface &&
                typeof<IPlugin>.IsAssignableFrom t

              if isPlugin
              then
                match Activator.CreateInstance t with
                | :? IPlugin as plugin ->
                  yield plugin
                | _ -> ()
          }
          |> Seq.tryHead

        match freshPlugin with
        | Some next ->
          printfn "New plugin found: %s" next.Name
          plugin <- Some next
        | None ->
          printfn "No plugin was found in the assembly"

      do! Async.Sleep 1000
      do! workflow writeTime
    }

  do
    Async.StartAsTask (workflow DateTime.MinValue, cancellationToken = source.Token) |> ignore
    ()

  interface IPlugin with
    member this.Name =
      plugin
      |> Option.map (fun x -> x.Name)
      |> Option.defaultValue (sprintf "HotPlugin[%s]" pluginPath)

    member this.Update dt state =
      match plugin with
      | Some p -> p.Update dt state
      | None -> { X = 32.0; Y = 32.0 }

  interface IDisposable with
    member this.Dispose () =
      source.Cancel ()
      source.Dispose ()
      plugin <- None

[<EntryPoint>]
let main argv =
  let pluginPath = argv.[0]

  printfn "Plugin path: %s" pluginPath

  use plugin = new HotPlugin (pluginPath)

  // let mutable lastWriteTime = DateTime.MinValue

  // while true do
  //   let writeTime =
  //     if File.Exists pluginPath
  //     then
  //       File.GetLastWriteTimeUtc pluginPath
  //     else
  //       lastWriteTime

  //   if writeTime > lastWriteTime
  //   then
  //     lastWriteTime <- writeTime

  //     printfn "Change detected! "

  //     Threading.Thread.Sleep 3000

  //     printfn "Loading assembly... "

  //     use stream = File.Open (pluginPath, FileMode.Open, FileAccess.ReadWrite, FileShare.None)

  //     let loadContext = PluginLoadContext pluginPath

  //     let assemblyName = AssemblyName (Path.GetFileNameWithoutExtension pluginPath)

  //     let assembly = loadContext.LoadFromAssemblyName assemblyName

  //     printfn "Got assembly: %s" assembly.FullName
  //     printfn "GlobalAssemblyCache? %b" assembly.GlobalAssemblyCache

  //     let plugins =
  //       seq {
  //         for t in assembly.GetTypes() do
  //           let isPlugin =
  //             not t.IsAbstract &&
  //             not t.IsInterface &&
  //             typeof<IPlugin>.IsAssignableFrom t

  //           if isPlugin
  //           then
  //             match Activator.CreateInstance t with
  //             | :? IPlugin as plugin ->
  //               yield plugin
  //             | _ -> ()
  //       }
  //       |> Seq.toList

  //     for plugin in plugins do
  //       printfn "Found plugin: %s" plugin.Name

  //     stream.Close ()

  //     printfn "Waiting for changes... "

  //   Threading.Thread.Sleep 1000

  async {
    let mutable last = { X = 0.0; Y = 0.0 }

    while true do
      let next = (plugin :> IPlugin).Update 0.0 last

      if next <> last
      then
        printfn "%A" next
        last <- next
  }
  |> Async.RunSynchronously

  0
