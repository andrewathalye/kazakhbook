pragma Ada_2012;

with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with DOM.Core;

with Toolkit.Features;
with Toolkit.Phonemes;
with Toolkit.Contexts;

package Toolkit.Syllables is
   type Syllable_Database is private;
   --  Contains a record of syllabification rules
   --  Associated with a Feature Database

   type Syllable is record
      Sounds   : Phonemes.Phoneme_List;
      Features : Toolkit.Features.Feature_Set;
   end record;
   --  Sounds produced in one single movement of the vocal apparatus

   package Syllable_Lists is new Ada.Containers.Vectors (Positive, Syllable);
   subtype Syllable_List is Syllable_Lists.Vector;

   -------------
   -- CURSORS --
   -------------
   function To_Cursor
     (SLC : Syllable_Lists.Cursor) return Contexts.Cursor'Class;

   ----------------
   -- CONVERSION --
   ----------------
   Syllable_Error : exception;
   function Syllabify
     (SDB : Syllable_Database; Phonetic_Word : Phonemes.Phoneme_List)
      return Syllable_List;
   --  Syllabify a list of phonemes according to the rules provided
   --  @exception Syllable_Error
   --    Raised when the given sounds cannot form a valid syllable

   function Flatten (SL : Syllable_List) return Phonemes.Phoneme_List;
   --  Flatten a list of syllables into a list of phonemes

   ------------
   -- OUTPUT --
   ------------
   function Transcribe (S : Syllable) return String;
   function Transcribe (SL : Syllable_List) return String;
   --  Transcribe a syllable or list of syllables in IPA notation

   procedure Read
     (Doc :     DOM.Core.Document; FDB : Features.Feature_Database;
      CDB :     Contexts.Context_Database; PDB : Phonemes.Phoneme_Database;
      SDB : out Syllable_Database);
   --  Read a syllable database from XML data
private
   type Syllable_Element_Kind is (Forbid, Require);
   type Syllable_Element
     (Kind : Syllable_Element_Kind := Syllable_Element_Kind'First) is
   record
      FS : Features.Feature_Set;
   end record;

   package Syllable_Element_Lists is new Ada.Containers.Vectors
     (Positive, Syllable_Element);
   subtype Syllable_Element_List is Syllable_Element_Lists.Vector;
   type Syllable_Definition is record
      Initial_Context : Contexts.Context;
      Elements        : Syllable_Element_List;
      Final_Context   : Contexts.Context;
      Provides        : Features.Feature_Set;
   end record;

   package Syllable_Definition_Maps is new Ada.Containers
     .Indefinite_Hashed_Maps
     (String, Syllable_Definition, Ada.Strings.Hash, "=");
   type Syllable_Database is
   new Syllable_Definition_Maps.Map with null record;
end Toolkit.Syllables;
