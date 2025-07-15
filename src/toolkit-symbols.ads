pragma Ada_2012;

with Toolkit.Phonemes;

with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Toolkit.Symbols is
   type Form is record
      Contexts : Toolkit.Phonemes.Context_List;
      Text     : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Form_Lists is new Ada.Containers.Vectors (Natural, Form);
   subtype Form_List is Form_Lists.Vector;

   type Symbol is record
      Forms         : Form_List;
      Pronunciation : Toolkit.Phonemes.Phone_List;
   end record;

   package Symbol_Lists is new Ada.Containers.Vectors (Natural, Symbol);
   subtype Symbol_List is Symbol_Lists.Vector;
end Toolkit.Symbols;
