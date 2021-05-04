module Make (Heap : Heap_intf.S) : Heap_intf.S with module Elm = Heap.Elm
