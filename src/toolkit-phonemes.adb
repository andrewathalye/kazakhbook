pragma Ada_2022;

with Ada.Strings.Unbounded;

with Toolkit.XML;
with Toolkit.Log; use Toolkit.Log;

package body Toolkit.Phonemes is

   ---------------
   -- To_Cursor --
   ---------------
   use all type Contexts.Context_Scope;

   pragma Warnings (Off, "is not referenced");
   function Get_Child (LE : Phoneme_Instance) return Contexts.Cursor'Class is
     (raise Contexts.Invalid_Cursor);
   function Get_Child (LE : Abstract_Phoneme) return Contexts.Cursor'Class is
     (raise Contexts.Invalid_Cursor);
   pragma Warnings (On, "is not referenced");

   package Phoneme_Cursors is new Contexts.Generic_Cursors
     (Phoneme, Phoneme_Lists, Phonemes_Impl.Get_Features, Get_Child);
   package Abstract_Phoneme_Cursors is new Contexts.Generic_Cursors
     (Phoneme, Abstract_Phoneme_Lists, Phonemes_Impl.Get_Features, Get_Child);

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
      use type Abstract_Phoneme_Lists.Cursor;
      Result : Phoneme_List;

      L_Cur : Contexts.Cursor'Class := Cur;
   begin
      Put_Log (Log.Phonemes, "RESOLVE");

      for AP_C in List.Iterate loop
         Result.Append
           (Resolve (PDB, Abstract_Phoneme_Lists.Element (AP_C), L_Cur));

         if AP_C /= List.Last then
            L_Cur := L_Cur.Next;
         end if;
      end loop;

      return Result;
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

   ----------------
   -- Transcribe --
   ----------------
   function Transcribe
     (PDB : Phoneme_Database; IPA : String) return Abstract_Phoneme_List
   is
      Result : Abstract_Phoneme_List;
      C      : Natural := IPA'First;
   begin
      Put_Log (Log.Phonemes, "TRANSCRIBE: " & IPA);
      while C /= 0 loop
         Result.Append (Phonemes_Impl.Transcribe (PDB, IPA, C));
      end loop;
      return Result;
   end Transcribe;

end Toolkit.Phonemes;
