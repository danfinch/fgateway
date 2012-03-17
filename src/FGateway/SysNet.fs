
/// Functions for hosting with HttpListener.
module FGateway.SysNet

open System
open System.IO
open System.Net
open System.Text
open System.Collections.Generic
open System.Threading
open Futility

let private toList a =
  match a with
  | null -> []
  | a -> a |> List.ofArray

/// Create a Pound.Http.Request from an HttpListenerContext.
let request (context : HttpListenerContext) =
  let req = context.Request
  let res = context.Response
  { State = TypeMap ()
    IsAsync =
      let h = req.Headers.["X-Requested-With"]
      h <> null && h |> String.upper = "XMLHTTPREQUEST"
    IsSecure = req.IsSecureConnection
    IsLocal = req.IsLocal
    Url = req.Url
    Path = req.Url.AbsolutePath
    FullPath = req.Url.AbsolutePath
    Host = req.Url.Host
    Method = req.HttpMethod
    QueryString = req.Url.Query
    Referrer = req.UrlReferrer
    Headers = HttpUtil.nvc req.Headers
    Form =
      if req.ContentType <> null 
          && req.HttpMethod = "POST"
          && req.ContentType |> String.upper = "APPLICATION/X-WWW-FORM-URLENCODED"
          then
        let reader = new BinaryReader (context.Request.InputStream)
        let content = Array.create (int context.Request.ContentLength64) 0uy
        let read = reader.Read (content, 0, (int context.Request.ContentLength64))
        let str = context.Request.ContentEncoding.GetString content
        let nvc = Web.HttpUtility.ParseQueryString (str, context.Request.ContentEncoding)
        HttpUtil.nvc nvc
      else
        Map []
    Query = HttpUtil.nvc req.QueryString
    Cookies =
      [ for c in req.Cookies ->
          c.Name
          , { Name = c.Name
              Value = c.Value
              Domain = c.Domain
              Path = c.Path
              WhenExpires = Some c.Expires
              IsHttpOnly = c.HttpOnly
              IsSecure = c.Secure
            }
      ]
      |> Map.ofList
    Files = [] // todo: implement
    ContentLength = req.ContentLength64
    ContentEncoding = req.ContentEncoding
    ContentType = req.ContentType
    InputStream = req.InputStream
    ClientAddress = req.RemoteEndPoint.Address
    ClientHostName = req.UserHostName
    UserAgent = req.UserAgent
    AcceptLanguages = req.UserLanguages |> toList
    AcceptTypes = req.AcceptTypes |> toList
    SetStatusCode = fun x -> res.StatusCode <- x
    SetStatusDescription = fun x -> res.StatusDescription <- x
    SetContentType = fun x -> res.ContentType <- x
    SetContentEncoding = fun x -> res.ContentEncoding <- x
    OutputStream = res.OutputStream
    Abort = res.Abort
    Redirect = res.Redirect
    End = res.Close
    SetCookie =
      fun c ->
        let hc = Cookie ()
        hc.Name <- c.Name
        hc.Value <- c.Value
        hc.Domain <- c.Domain
        hc.Path <- c.Path
        match c.WhenExpires with
        | Some x -> hc.Expires <- x
        | _ -> ()
        hc.HttpOnly <- c.IsHttpOnly
        hc.Secure <- c.IsSecure
    SetHeader = fun name value -> res.AddHeader (name, value)
    AppendHeader = fun name value -> res.AppendHeader (name, value)
  }

let server (prefixes : string list) (threads : int) (respond : Request -> unit) =
  let listener = new HttpListener ()
  let cts = new CancellationTokenSource ()
  let getContext () = Async.FromBeginEnd (listener.BeginGetContext, listener.EndGetContext)
  let rec loop () =
    async {
      let! context = getContext ()
      try
        context
        |> request
        |> respond
      finally
        dispose context.Response
      return! loop ()
    }
  for p in prefixes do listener.Prefixes.Add p
  listener.Start ()
  [1 .. threads]
  |> List.iter (fun _ -> Async.Start (loop (), cts.Token))
  { new IDisposable with
      member self.Dispose () =
        cts.Cancel ()
        listener.Stop () 
  }

