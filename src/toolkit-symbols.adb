pragma Ada_2012;

pragma Warnings (Off); --  TODO

package body Toolkit.Symbols is

   ------------
   -- To_Ada --
   ------------

   function To_Ada (SDB : Symbol_Database; Text : String) return Symbol_List is
   begin
      pragma Compile_Time_Warning (Standard.True, "To_Ada unimplemented");
      return raise Program_Error with "Unimplemented function To_Ada";
   end To_Ada;

   ----------------
   -- To_Unicode --
   ----------------

   function To_Unicode (S : Symbol_List) return String is
   begin
      pragma Compile_Time_Warning (Standard.True, "To_Unicode unimplemented");
      return raise Program_Error with "Unimplemented function To_Unicode";
   end To_Unicode;

end Toolkit.Symbols;
