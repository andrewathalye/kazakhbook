pragma Ada_2012;

with DOM.Core;
with Toolkit.Features;

private with Ada.Containers.Vectors;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Hash;

package Toolkit.Contexts_Impl is
   type Context_Scope is
     (None, Phoneme, Syllable, Symbol, Morpheme, Word, Clause, Sentence, Text);

   type Context_Database is private;
   type Context is private;

   Invalid_Context : exception;
   function Lookup (DB : Context_Database; Name : String) return Context;

   No_Cursor : exception;
   type Cursor is interface;
   function Scope (C : Cursor) return Context_Scope is abstract;
   function Before (C : Cursor) return Features.Feature_Set_List is abstract;
   function This (C : Cursor) return Features.Feature_Set is abstract;
   function After (C : Cursor) return Features.Feature_Set_List is abstract;
   function Sub (C : Cursor) return Cursor'Class is abstract;
   function Super (C : Cursor) return Cursor'Class is abstract;

   State_Incomplete : exception;
   function Applicable (Cur : Cursor'Class; Ctx : Context) return Boolean;

   function To_XML (L : Context) return String;

   Duplicate_Context : exception;
   procedure Read
     (Doc :     DOM.Core.Document; FDB : Features.Feature_Database;
      CDB : out Context_Database);
private

   type Context_Kind is (None, Anyprev, Anynext, Prev, Next, Unique, Super);

   type Context_Single (K : Context_Kind := None) is record
      FS : Features.Feature_Set;
   end record;

   package CM is new Ada.Containers.Vectors (Natural, Context_Single);
   subtype Context_Multiple is CM.Vector;

   type Scoped_Context (Level, Within : Context_Scope := None) is record
      C_Not : Context_Multiple;
      C_Any : Context_Multiple;
      C_All : Context_Multiple;
   end record;

   package Scoped_Context_Lists is new Ada.Containers.Vectors
     (Natural, Scoped_Context);
   subtype Scoped_Context_List is Scoped_Context_Lists.Vector;

   use type Scoped_Context_List;
   package Context_Databases is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Scoped_Context_List, Ada.Strings.Hash, "=");

   type Context_Database is new Context_Databases.Map with null record;
   type Context is new Context_Databases.Cursor;
end Toolkit.Contexts_Impl;
