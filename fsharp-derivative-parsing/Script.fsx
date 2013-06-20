﻿// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Parser.fs"
open DerivativeParser

// Define your library scripting code here
open Grammar

type StringToken(str) =
  interface IToken with
    member this.Str() = str

let simpleMap : Map<string, StringToken LazyRule> = 
  Map.empty 
  |> Map.add "S" (Rule(Disj(Epsilon, Disj(NT "B",Conj(NT "S", NT "S"))))) 
  |> Map.add "B" (Rule(Disj(Token (new StringToken("0")), Token(new StringToken("1"))))) 
  |> Map.add "X" (Rule(Token (new StringToken "X")))
//  Map.add "B" (Rule(Disj(Token "0", Token "1")))
//    (Map.add "S" (Rule(Disj(Epsilon, Disj(NT "B", Conj(NT "S", NT "S"))))) Map.empty)

let simpleGrm = "S", simpleMap

let simpleNullables = compute_nullable simpleGrm

let d1 = derive (new StringToken "1") simpleGrm
let d11 = derive (new StringToken "1") d1
let d21 = derive (new StringToken "2") d1
let d1nullables = compute_nullable d1

//"hej  med dig".Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)

//
//open System
//
//Set.iter (fun e -> Console.WriteLine ("Elem : " + e)) simpleNullables

