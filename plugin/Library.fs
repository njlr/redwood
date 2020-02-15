namespace HotReload

[<Struct>]
type Vector2F =
  {
    X : float
    Y : float
  }

type IPlugin =
  interface
    abstract Name : string
    abstract Update : float -> Vector2F -> Vector2F
  end
