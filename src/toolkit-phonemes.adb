pragma Ada_2022;

with Ada.Strings.Unbounded;
with Toolkit.XML;

package body Toolkit.Phonemes is

   ---------------
   -- To_Cursor --
   ---------------
   use all type Contexts.Context_Scope;
   package Phoneme_Cursors is new Contexts.Generic_Cursors
     (Phoneme, Phoneme_Lists, Phonemes_Impl.Get_Features);
   package Abstract_Phoneme_Cursors is new Contexts.Generic_Cursors
     (Phoneme, Abstract_Phoneme_Lists, Phonemes_Impl.Get_Features);

   function To_Cursor (C : Phoneme_Lists.Cursor) return Contexts.Cursor'Class
   is
   begin
      return Phoneme_Cursors.Create (C);
   end To_Cursor;

   function To_Cursor
     (C : Abstract_Phoneme_Lists.Cursor) return Contexts.Cursor'Class
   is
   begin
      return Abstract_Phoneme_Cursors.Create (C);
   end To_Cursor;

   -------------
   -- Resolve --
   -------------
   function Resolve
     (PDB : Phoneme_Database; List : Abstract_Phoneme_List;
      Cur : Contexts.Cursor'Class) return Phoneme_List
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Resolve unimplemented");
      return raise Program_Error with "Unimplemented function Resolve";
   end Resolve;

   -----------------
   -- Abstractise --
   -----------------
   function Abstractise (List : Phoneme_List) return Abstract_Phoneme_List is
      Result : Abstract_Phoneme_List;
   begin
      for PI of List loop
         Result.Append (Abstractise (PI));
      end loop;

      return Result;
   end Abstractise;

   ------------
   -- To_Ada --
   ------------
   function To_Ada
     (FDB : Features.Feature_Database; PDB : Phoneme_Database;
      XML : DOM.Core.Node) return Abstract_Phoneme
   is
   begin
      return To_Ada (FDB, PDB, Toolkit.XML.Get_Text (XML));
   end To_Ada;

   ----------------
   -- Transcribe --
   ----------------
   function Transcribe (PL : Phoneme_List) return String is
      use Ada.Strings.Unbounded;

      Buffer : Unbounded_String;
   begin
      for PI of PL loop
         Append (Buffer, Transcribe (PI));
      end loop;

      return To_String (Buffer);
   end Transcribe;

end Toolkit.Phonemes;
