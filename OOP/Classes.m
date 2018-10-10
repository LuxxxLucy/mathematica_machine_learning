(* :Title: Object-oriented Programming *)

(* :Name: Classes` *)

(* :Author: Roman E. Maeder, June 1993. *)

(* :Summary:
This packages provides the support for object-oriented programming in Mathematica.
*)

(* :Context: Classes` *)

(* :Package Version: 1.1 *)

(* :Copyright: Copyright 1993, Roman E. Maeder.

   Permission is granted to distribute verbatim copies of this package
   together with any of your packages that use it, provided the following
   acknowledgement is printed in a standard place:
 
	"Classes.m is distributed with permission by Roman E. Maeder."
  
   The newest release of Classes.m is available through MathSource.
*)

(* :History:
   Improved efficiency, June 1993..
   Version 1.0 for the Mathematica Journal, October 1992.
*)

(* :Keywords: object-oriented programming, classes, methods, objects *)

(* :Source: 
    Maeder, Roman E.. 1993. Object-oriented Programming.
        The Mathematica Journal, 3(1).
*)

(* :Mathematica Version: 2.2 *)

(* :Limitation:
   There are no checks for the correct number of arguments of methods.
*)

(* :Discussion: 
   This version of the package is more efficient than the original one
   from the Journal. Objects use less space and are freed
   automatically, as soon as they are no longer referenced.  Therefore,
   delete[obj] should normally not be used any more.  The external
   representation of objects has been changed.  The functionality is
   the same, however.
 

*)

BeginPackage["Classes`"]

Class::usage = "Class[class, superclass, variables, methods] defines
	a new class as a subclass of superclass.
	Class[object] gives the class of an object."
ClassQ::usage = "ClassQ[symbol] is True, if symbol is a class."
Methods::usage = "Methods[class] gives the list of methods of class."
InstanceVariables::usage = "InstanceVariables[class] gives the
	list of instance variables of class."
SuperClass::usage = "SuperClass[class] gives the superclass of class."
new::usage = "new[class, args...] generates a new object of class. Any
	method with name 'new' is called to initialize the new object."
delete::usage = "delete[obj] deletes an object (for special occasions only)."
self::usage = "self denotes the object inside methods."
super::usage = "super denotes the object as a member of its superclass."
isa::usage = "isa[obj, class] is true if obj belongs to class
	or a subclass of it."
NIM::usage = "NIM[self, <method>]& can be used as body of a
	pure virtual method."

Object::usage = "Object is the root class."

Object::nim = "Method `1` not implemented for class `2`."

Begin["`Private`"]

context = $Context

(* private rules for instances *) {variables}
(* private class methods *)       {methodHandler}
(* other private symbols *)       {raise}

ClassQ[_] := False (* default *)

Class[ class_Symbol,
       superclass_?ClassQ,
       variables:{_Symbol...},
       methods:{{_Symbol, _Function}...}|{}
     ] :=
    Module[{apply, standard,
            localmethods, allvariables, messages,
            methodQ},

        standard = { (* standard methods *)
            {Class,    class&},
            {isa, (class===#1 || isa[super, #1])&} };
        localmethods = Join[standard, methods];
        messages = Union[ Join[First /@ localmethods,
                               Methods[superclass]] ];
        allvariables = Join[variables, InstanceVariables[superclass]];
        (* class methods *)
        class/: Methods[class] = messages;
        class/: InstanceVariables[class] = allvariables;
        class/: SuperClass[class] = superclass;

        With[{ivars = Hold @@ allvariables,
              nvars = -Length[allvariables],
              localnames = First /@ localmethods,
              cookie = ToExpression[ context <> ToString[class] ],
              allvariables = allvariables},

          (* the head used for objects of this class *)
          SetAttributes[cookie, HoldAll];

          (* aux predicate for apply and message passing *)
          (methodQ[#] = True)& /@ messages;
          methodQ[_] = False;

          (* definitions of aux method applicator ``apply'' *)
          Apply[
            (apply[#1, obj_] :=
              With[{lvars = Take[Hold @@ obj, nvars]},
                #2 /.
                 List @@ Thread[ ivars :> lvars, Hold] /.
                 { self -> obj, super -> raise[obj, superclass] }
              ])&,
            localmethods, {1} ];
          (* inheritance, if not local method *)
          apply[f_, obj_] := methodHandler[superclass][f, obj];
          class/: methodHandler[class] = apply;

          (* message passing *)
          cookie/: (f_Symbol?methodQ)[obj_cookie, args___] :=
                   apply[f, obj][args];
          (* super *)
          raise/: (f_Symbol?methodQ)[raise[obj_, class], args___] :=
                   apply[f, obj][args];

          (* create instances of this class *)
          class/: new[class, init___] :=
            Module[{obj, syms = Unique[allvariables]},
	      SetAttributes[Evaluate[syms], Temporary];
              obj = cookie @@ syms;
              new[obj, init]; (* call any constructor defined *)
              obj
            ];
          (* formatting *)
          Format[obj_cookie] := StringForm["-`1`-", class]
        ];
        class/: ClassQ[class] = True; (* seal of approval *)
        class
    ]

(* the class Object: we need a dummy superclass *)

Block[{noClass},
  noClass/: Methods[noClass] = {};
  noClass/: InstanceVariables[noClass] = {};
  noClass/: methodHandler[noClass] = badmessage;
  noClass/: SuperClass[noClass] = noClass;
  noClass/: ClassQ[noClass] = True;

  Class[ Object, noClass, {},
    {{new,               self&},
     {delete,            (Remove @@ self; Null)&},
     {isa,               #1===Object&},
     {Methods,           Methods[Class[self]]&},
     {InstanceVariables, InstanceVariables[Class[self]]&},
     {SuperClass,        SuperClass[Class[self]]&},
     {NIM,		 Message[Object::nim, #1, Class[self]]&}
    }
  ]
]

End[]

Protect[ Class, Methods, InstanceVariables, SuperClass,
         new, delete, isa, Object ]

EndPackage[]
