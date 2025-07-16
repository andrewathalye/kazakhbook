pragma Ada_2012;

with DOM.Core;

package Toolkit.XML is
   function To_String (XML : DOM.Core.Node) return String;
   --  Convert an XMLAda Node to a String

   function Get_Text (N : DOM.Core.Node) return String;
   --  Get the text contained within N or raise Constraint_Error
end Toolkit.XML;
