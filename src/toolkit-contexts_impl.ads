------------------------
-- NOT FOR DIRECT USE --
------------------------
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

   ------------
   -- CURSOR --
   ------------
   type Cursor is interface;
   type Cursor_Placement is (First, Last);

   function No_Cursor return Cursor'Class;
   pragma Pure_Function (No_Cursor);

   function Is_Null (C : Cursor'Class) return Boolean;

   function Scope (C : Cursor) return Context_Scope is abstract;
   function Rescope
     (C : Cursor'Class; Target : Context_Scope; Placement : Cursor_Placement)
      return Cursor'Class;

   function Features
     (C : Cursor) return Toolkit.Features.Feature_Set is abstract;

   function Previous (C : Cursor) return Cursor is abstract;
   function Next (C : Cursor) return Cursor is abstract;

   function Sub
     (C : Cursor; Placement : Cursor_Placement)
      return Cursor'Class is abstract;
   function Super (C : Cursor) return Cursor'Class is abstract;

   -----------
   -- STATE --
   -----------
   State_Incomplete : exception;
   function Applicable (Cur : Cursor'Class; Ctx : Context) return Boolean;

   function To_XML (L : Context) return String;

   Duplicate_Context : exception;
   procedure Read
     (Doc :     DOM.Core.Document; FDB : Toolkit.Features.Feature_Database;
      CDB : out Context_Database);
private
   type Context_Kind is (None, Anyprev, Anynext, Prev, Next, Unique, Super);

   type Context_Single (K : Context_Kind := None) is record
      FS : Toolkit.Features.Feature_Set;
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

   ---------------
   -- No_Cursor --
   ---------------
   type No_Cursor_Type is new Cursor with null record;
   function Scope (C : No_Cursor_Type) return Context_Scope is (None);

   function Features
     (C : No_Cursor_Type) return Toolkit.Features.Feature_Set is
     (Toolkit.Features.Null_Feature_Set);

   function Previous (C : No_Cursor_Type) return No_Cursor_Type is
     (No_Cursor_Type (No_Cursor));
   function Next (C : No_Cursor_Type) return No_Cursor_Type is
     (No_Cursor_Type (No_Cursor));

   function Sub
     (C : No_Cursor_Type; Placement : Cursor_Placement) return Cursor'Class is
     (No_Cursor);
   function Super (C : No_Cursor_Type) return Cursor'Class is (No_Cursor);

   L_No_Cursor : aliased constant No_Cursor_Type := (null record);

   function No_Cursor return Cursor'Class is (L_No_Cursor);

   function Is_Null (C : Cursor'Class) return Boolean is
     (C in No_Cursor_Type'Class);

end Toolkit.Contexts_Impl;
