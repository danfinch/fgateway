
// An HTTP abstraction.
namespace FGateway

open System
open System.IO
open System.Net
open System.Text
open System.Collections.Generic
open System.Threading
open Futility

// todo: adapters for OWIN, Manos, Kayak, SharpCGI, HttpMachine, Fracture, Frack, etc.?
// todo: implement Request.Files for HttpListener

/// A cookie.
type Cookie = {
  Name                        : string
  Value                       : string
  Domain                      : string
  Path                        : string
  WhenExpires                 : DateTime option
  IsHttpOnly                  : bool
  IsSecure                    : bool
} with
  static member create name value =
    { Name = name
      Value = value
      Domain = null
      Path = null
      WhenExpires = None
      IsHttpOnly = false
      IsSecure = false
    }

/// A file uploaded through HTTP.
type HttpFile = {
  Name                        : string
  Length                      : int64
  Input                       : Stream
}

/// Represents an HTTP request.
type Request = {
  /// Container for generic per-request shared cache.
  State                       : TypeMap
  /// Indicates whether the request was sent with XMLHttpRequest (AJAX).
  IsAsync                     : bool
  /// Indicates whether this request was made over a secure HTTPS connection.
  IsSecure                    : bool
  /// Indicates whether this request originates from the local machine.
  IsLocal                     : bool
  /// The full requested URL.
  Url                         : Uri
  /// The path relative to the current stage of the request.
  Path                        : string
  /// The path requested by the user agent.
  FullPath                    : string
  /// Header indicating the domain name requested by the user agent.
  Host                        : string
  /// The raw HTTP method sent by the user agent.
  Method                      : string
  /// Raw query-string component of the requested path.
  QueryString                 : string
  /// Header indicating a referring URL.
  Referrer                    : Uri
  /// The HTTP headers for this request.
  Headers                     : Map<string, string list>
  /// Values of a form submitted by POST.
  Form                        : Map<string, string list>
  /// Parsed values of the query string component of the requested path.
  Query                       : Map<string, string list>
  /// Collection of cookies submitted with this request.
  Cookies                     : Map<string, Cookie>
  /// Collection of files submitted with this request.
  Files                       : HttpFile list
  /// The length of the request body.
  ContentLength               : int64
  /// The encoding of the request body.
  ContentEncoding             : Encoding
  /// The content type of the request body.
  ContentType                 : string
  /// Stream for the request body.
  InputStream                 : Stream
  /// IP address of the client.
  ClientAddress               : IPAddress
  /// If available, the hostname of the client.
  ClientHostName              : string
  /// User agent header.
  UserAgent                   : string
  /// Languages requested by the user agent.
  AcceptLanguages             : string list
  /// Content types requested by the user agent.
  AcceptTypes                 : string list
  /// Set the HTTP status code of the response.
  SetStatusCode               : int -> unit
  /// Set the HTTP status description of the response.
  SetStatusDescription        : string -> unit
  /// Set the content type of the response body.
  SetContentType              : string -> unit
  /// Set the content encoding of the response body.
  SetContentEncoding          : Encoding -> unit
  /// Stream for the response body.
  OutputStream                : Stream
  /// Abort the current request.
  Abort                       : unit -> unit
  /// Immediately redirect the request to another URL.
  Redirect                    : string -> unit
  /// End the current request and close the connection.
  End                         : unit -> unit
  /// Set a response cookie.
  SetCookie                   : Cookie -> unit
  /// Set a response header.
  SetHeader                   : string -> string -> unit
  /// Append a value to a response header.
  AppendHeader                : string -> string -> unit
}

module internal HttpUtil =
  let inline nvc (coll : System.Collections.Specialized.NameValueCollection) =
    [ for i in coll do
        yield i, coll.GetValues i |> List.ofArray
    ]
    |> Map.ofList

module Cache =
  let noStore (r : Request) = r.AppendHeader "Cache-Control" "no-store"; r
  let noCache (r : Request) = r.AppendHeader "Cache-Control" "no-cache"; r

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Request =
  let authorize (realm : string) (auth : string -> string -> bool) =
    let handle r =
      let authorized =
        let h = "Authorization"
        match r.Headers.TryFind h with
        | Some [t] when t.StartsWith "Basic " ->
          let lp =
            t
            |> String.split [' ']
            |> List.get 1
            |> Convert.FromBase64String
            |> ASCIIEncoding.ASCII.GetString
            |> String.split [':']
          match lp with
          | [l; p] -> auth l p
          | _ -> false
        | _ -> false
      if not authorized then
        r.SetStatusCode 401
        r.SetHeader "WWW-Authenticate" ("Basic realm=\"" + realm + "\"")
        r.End ()
      r
    handle

module Route =
  let byPath (t : (string * (Request -> Request)) list) =
    let rt r =
      let _,h = t |> List.find (fun (p, h) -> p = r.Path)
      let r = r |> h
      { r with Path = "" }
    rt
  // byPrefix
  // byDirectory
