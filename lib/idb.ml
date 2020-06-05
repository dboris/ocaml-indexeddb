(* Copyright (C) 2015, Thomas Leonard. See the LICENSE file for
   details. *)

(** js_of_ocaml type declarations for the w3c IndexedDB spec:

    http://www.w3.org/TR/IndexedDB/

    Currently only covers the bits needed for CueKeeper.
    IndexedDB_lwt provides a more friendly API. *)

open Js_of_ocaml

(* Note: we currently assume all keys are strings. This will always
   be the case for entries added using this interface. *)
type key = Js.js_string Js.t
type _ store_name = Js.js_string Js.t
type mode = Js.js_string Js.t

class type versionChangeEvent = object
  inherit Dom_html.event
  method oldVersion : int Js.readonly_prop
  method newVersion : int Js.readonly_prop
end

class type ['a] errorEvent = object
  inherit ['a] Dom.event
end

class type completeEvent = object
  inherit Dom_html.event
end

class type successEvent = object
  inherit Dom_html.event
end

class type cursor = object
  method key : key Js.readonly_prop
  method continue : unit Js.meth
end

class type ['a] cursorWithValue = object
  inherit cursor
  method value : 'a Js.readonly_prop
end

class type domException = object
  (* Being a bit paranoid marking all these as optdef *)
  method name : Js.js_string Js.t Js.Optdef.t Js.readonly_prop
  method message : Js.js_string Js.t Js.Optdef.t Js.readonly_prop
  method code : int Js.Optdef.t Js.readonly_prop
end

class type request = object
  method error : domException Js.t Js.Opt.t Js.readonly_prop
  method onerror : ('self Js.t, request errorEvent Js.t) Dom.event_listener Js.prop
  method onsuccess : ('self Js.t, successEvent Js.t) Dom.event_listener Js.prop
end

class type ['a] requestWithResult = object
  inherit request
  method result : 'a Js.Optdef.t Js.readonly_prop
end

class type ['a] openCursorRequest = object
  inherit request
  method result : 'a cursorWithValue Js.t Js.Opt.t Js.readonly_prop
end

class type createObjectStoreOptions = object
  method keyPath : Js.js_string Js.t Js.writeonly_prop
  method autoIncrement : bool Js.t Js.writeonly_prop
end

let createObjectStoreOptions () : createObjectStoreOptions Js.t =
  Js.Unsafe.obj [||]

class type keyRange = object
  method lower : key Js.Optdef.t Js.readonly_prop
  method upper : key Js.Optdef.t Js.readonly_prop
  method lowerOpen : bool Js.t Js.readonly_prop
  method upperOpen : bool Js.t Js.readonly_prop
  method includes : key -> bool Js.t Js.meth
end

class type keyRange_constr = object
  method bound : key -> key -> keyRange Js.t Js.meth
  method bound_lowerOpen : key -> key -> bool Js.t -> keyRange Js.t Js.meth
  method bound_lowerAndUpperOpen :
    key -> key -> bool Js.t -> bool Js.t -> keyRange Js.t Js.meth
  method lowerBound : key -> keyRange Js.t Js.meth
  method lowerBound_open : key -> bool Js.t -> keyRange Js.t Js.meth
  method upperBound : key -> keyRange Js.t Js.meth
  method upperBound_open : key -> bool Js.t -> keyRange Js.t Js.meth
  method only : key -> keyRange Js.t Js.meth
end

let keyRange_constr = Js.Unsafe.global##._IDBKeyRange

let keyRange : keyRange_constr Js.t = keyRange_constr

class type ['a] objectStore = object
  method keyPath : key Js.readonly_prop
  method autoIncrement : bool Js.t Js.readonly_prop
  method add : 'a Js.t -> key -> request Js.t Js.meth
  method put : 'a Js.t -> key -> request Js.t Js.meth
  method delete : key -> request Js.t Js.meth
  method get : key -> 'a Js.t requestWithResult Js.t Js.meth
  method openCursor : 'a Js.t openCursorRequest Js.t Js.meth
  method openCursor_query :
    keyRange Js.t -> 'a Js.t openCursorRequest Js.t Js.meth
  method openCursor_queryAndDirection :
    keyRange Js.t Js.Opt.t -> Js.js_string Js.t ->
    'a Js.t openCursorRequest Js.t Js.meth
  method add_object : 'a Js.t -> key requestWithResult Js.t Js.meth
  method put_object : 'a Js.t -> key requestWithResult Js.t Js.meth
  method getAll : 'a Js.t Js.js_array Js.t requestWithResult Js.t Js.meth
  method getAll_query :
    keyRange Js.t -> 'a Js.t Js.js_array Js.t requestWithResult Js.t Js.meth
  method getAll_queryAndCount :
    keyRange Js.t Js.Opt.t -> int ->
    'a Js.t Js.js_array Js.t requestWithResult Js.t Js.meth
end

class type ['a] transaction = object
  method oncomplete :
    ('self Js.t, completeEvent Js.t) Dom.event_listener Js.prop
  method onerror :
    ('self Js.t, request errorEvent Js.t) Dom.event_listener Js.prop
  method objectStore : 'a store_name -> 'a objectStore Js.t Js.meth
  method abort : unit Js.meth
end

class type database = object
  method close : unit Js.meth
  method createObjectStore :
    'a . 'a store_name -> 'a objectStore Js.t Js.meth
  method createObjectStore_withOptions :
    'a . 'a store_name -> createObjectStoreOptions Js.t ->
    'a objectStore Js.t Js.meth
  method deleteObjectStore :
    'a . 'a store_name -> unit Js.meth
  method onerror :
    ('self Js.t, request errorEvent Js.t) Dom.event_listener Js.prop
  method transaction :
    'a . 'a store_name Js.js_array Js.t -> mode ->
    'a transaction Js.t Js.meth
end

class type openDBRequest = object ('self)
  inherit request
  method onupgradeneeded :
    ('self Js.t, versionChangeEvent Js.t) Dom_html.event_listener Js.prop
  method onblocked :
    ('self Js.t, versionChangeEvent Js.t) Dom_html.event_listener Js.prop
  method result : database Js.t Js.readonly_prop
end

class type factory = object
  method _open : Js.js_string Js.t -> int -> openDBRequest Js.t Js.meth
  method deleteDatabase : Js.js_string Js.t -> openDBRequest Js.t Js.meth
end
