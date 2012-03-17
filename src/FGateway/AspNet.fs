
/// Functions for hosting with ASP.NET.
module FGateway.AspNet

open System
open System.IO
open System.Net
open System.Text
open System.Collections.Generic
open System.Threading
open Futility
open System.Web


/// Create a Pound.Http.Request from an ASP.NET HttpContext.
let request (context : HttpContext) =
  let req = context.Request
  let res = context.Response
  { State = TypeMap ()
    IsAsync = 
      let h = req.Headers.["X-Requested-With"]
      h <> null && h |> String.upper = "XMLHTTPREQUEST"
    IsSecure = req.IsSecureConnection
    IsLocal = req.IsLocal
    Url = req.Url
    Path = req.Path
    FullPath = req.Url.AbsolutePath
    Host = req.Url.Host
    Method = req.HttpMethod
    QueryString = req.Url.Query
    Referrer = req.UrlReferrer
    Headers = HttpUtil.nvc req.Headers
    Form = HttpUtil.nvc req.Form
    Query = HttpUtil.nvc req.QueryString
    Cookies =
      [ for cn in req.Cookies.AllKeys ->
          let c = req.Cookies.[cn]
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
    Files =
      [ for f in req.Files ->
          { Name = f.FileName
            Length = int64 f.ContentLength
            Input = f.InputStream
          }
      ]
    ContentLength = int64 req.ContentLength
    ContentEncoding = req.ContentEncoding
    ContentType = req.ContentType
    InputStream = req.InputStream
    ClientAddress = Net.IPAddress.Parse req.UserHostAddress
    ClientHostName = req.UserHostName
    UserAgent = req.UserAgent
    AcceptLanguages = if req.UserLanguages <> null then req.UserLanguages |> List.ofArray else []
    AcceptTypes = if req.AcceptTypes <> null then req.AcceptTypes |> List.ofArray else []
    SetStatusCode = fun x -> res.StatusCode <- x
    SetStatusDescription = fun x -> res.StatusDescription <- x
    SetContentType = fun x -> res.ContentType <- x
    SetContentEncoding = fun x -> res.ContentEncoding <- x
    OutputStream = res.OutputStream
    Abort = res.End
    Redirect = res.Redirect
    End = res.Close
    SetCookie =
      fun c ->
        let hc = HttpCookie (c.Name)
        hc.Value <- c.Value
        hc.Domain <- c.Domain
        hc.Path <- c.Path
        match c.WhenExpires with
        | Some x -> hc.Expires <- x
        | _ -> ()
        hc.HttpOnly <- c.IsHttpOnly
        hc.Secure <- c.IsSecure
        res.Cookies.Add hc
    SetHeader = fun name value -> res.AddHeader (name, value)
    AppendHeader = fun name value -> res.AppendHeader (name, value)
  }


