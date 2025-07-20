pragma Ada_2012;

pragma Warnings (Off);

package body Toolkit.Symbols_Impl is

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (SDB : Symbol_Database; Symbol : String; Context : Contexts.Context)
      return Symbol_Instance
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "To_Ada unimplemented");
      return raise Program_Error with "Unimplemented function To_Ada";
   end To_Ada;

   ----------------
   -- To_Unicode --
   ----------------

   function To_Unicode (S : Symbol_Instance) return String is
   begin
      pragma Compile_Time_Warning (Standard.True, "To_Unicode unimplemented");
      return raise Program_Error with "Unimplemented function To_Unicode";
   end To_Unicode;

   ----------
   -- Read --
   ----------

   procedure Read
     (Doc : DOM.Core.Document; FDB : Features.Feature_Database;
      PDB : Phonemes.Phoneme_Database; SDB : out Symbol_Database)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Read unimplemented");
      raise Program_Error with "Unimplemented procedure Read";
   end Read;

end Toolkit.Symbols_Impl;
