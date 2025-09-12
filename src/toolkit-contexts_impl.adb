pragma Ada_2012;

pragma Warnings (Off);

package body Toolkit.Contexts_Impl is

   ----------------
   -- Applicable --
   ----------------

   function Applicable (Cur : Cursor'Class; Ctx : Context) return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "Applicable unimplemented");
      return raise Program_Error with "Unimplemented function Applicable";
   end Applicable;

   ------------
   -- To_XML --
   ------------

   function To_XML (L : Context) return String is
   begin
      pragma Compile_Time_Warning (Standard.True, "To_XML unimplemented");
      return raise Program_Error with "Unimplemented function To_XML";
   end To_XML;

   ----------
   -- Read --
   ----------

   procedure Read
     (Doc :     DOM.Core.Document; FDB : Features.Feature_Database;
      CDB : out Context_Database)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Read unimplemented");
      raise Program_Error with "Unimplemented procedure Read";
   end Read;

end Toolkit.Contexts_Impl;
