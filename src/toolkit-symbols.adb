pragma Ada_2012;

with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Toolkit.Contexts_Impl;
with Toolkit.Log; use Toolkit.Log;

package body Toolkit.Symbols is

   package Symbol_List_Cursors is new Contexts.Generic_Cursors
     (Cursor_Scope => Contexts_Impl.Symbol, List => Symbol_Lists,
      Get_Features => Symbols_Impl.Dump_Features,
      Get_Child    => Symbols_Impl.Get_Child);

   package Abstract_Symbol_List_Cursors is new Contexts.Generic_Cursors
     (Cursor_Scope => Contexts_Impl.Symbol, List => Abstract_Symbol_Lists,
      Get_Features => Symbols_Impl.Dump_Features,
      Get_Child    => Symbols_Impl.Get_Child);

   ---------------
   -- To_Cursor --
   ---------------
   function To_Cursor (Pos : Symbol_Lists.Cursor) return Contexts.Cursor'Class
   is
   begin
      return Symbol_List_Cursors.Create (Pos);
   end To_Cursor;

   ---------------
   -- To_Cursor --
   ---------------
   function To_Cursor
     (Pos : Abstract_Symbol_Lists.Cursor) return Contexts.Cursor'Class
   is
   begin
      return Abstract_Symbol_List_Cursors.Create (Pos);
   end To_Cursor;

   -------------
   -- Resolve --
   -------------
   function Resolve
     (PDB : Phonemes.Phoneme_Database; ASL : Abstract_Symbol_List;
      Cur : Contexts.Cursor'Class) return Symbol_List
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Resolve unimplemented");
      return raise Program_Error with "Unimplemented function Resolve";
   end Resolve;

   ------------
   -- To_Ada --
   ------------
   function To_Ada
     (SDB : Symbol_Database; Text : String) return Abstract_Symbol_List
   is
      use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
      WW_Text     : Wide_Wide_String := Decode (Text);
      Result      : Abstract_Symbol_List;
      Start_Index : Positive         := WW_Text'First;
   begin
      Put_Log (Log.Symbols, "Resolve " & Text);
      while Start_Index <= WW_Text'Last loop
         for Symbol_Length in reverse 1 .. 4 loop
            begin
               Put_Log
                 (Log.Symbols,
                  "Try " &
                  Encode
                    (WW_Text (Start_Index .. Start_Index + Symbol_Length - 1)));

               Result.Append
                 (Symbols_Impl.To_Ada
                    (SDB,
                     Encode
                       (WW_Text
                          (Start_Index .. Start_Index + Symbol_Length - 1))));
               Start_Index := Start_Index + Symbol_Length;
               goto Found_Symbol;
            exception
               when Constraint_Error => null;
               when Unknown_Symbol =>
                  null;
            end;
         end loop;
         raise Unknown_Symbol;
         <<Found_Symbol>>
      end loop;

      return Result;
   end To_Ada;

   ----------------
   -- To_Unicode --
   ----------------
   function To_Unicode (S : Symbol_List) return String is
      use Ada.Strings.Unbounded;
      Buf : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for Sym of S loop
         Append (Buf, Symbols_Impl.To_Unicode (Sym));
      end loop;

      return To_String (Buf);
   end To_Unicode;

   -----------------
   -- To_Phonemes --
   -----------------
   function To_Phonemes (SL : Symbol_List) return Phonemes.Phoneme_List is
      Result : Phonemes.Phoneme_List;
   begin
      for S of SL loop
         Result.Append_Vector (Symbols_Impl.To_Phonemes (S));
      end loop;

      return Result;
   end To_Phonemes;

end Toolkit.Symbols;
