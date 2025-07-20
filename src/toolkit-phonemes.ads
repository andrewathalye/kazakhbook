pragma Ada_2012;

with Ada.Containers.Vectors;

with Toolkit.Features;
with Toolkit.Contexts;
with Toolkit.Phonemes_Impl;

with DOM.Core;

package Toolkit.Phonemes is
   subtype Phoneme_Database is Phonemes_Impl.Phoneme_Database;
   --  Database of all known phonemes.
   --  Linked to a Feature Database

   subtype Phoneme_Instance is Phonemes_Impl.Phoneme_Instance;
   use type Phoneme_Instance;
   --  One concrete phoneme realisation

   package Phoneme_Sets is new Ada.Containers.Vectors
     (Positive, Phoneme_Instance);
   subtype Phoneme_Set is Phoneme_Sets.Vector;
   --  A list of phoneme instances

   ----------------
   -- RESOLUTION --
   ----------------
   Unknown_Phoneme : exception renames Phonemes_Impl.Unknown_Phoneme;
   function Resolve_Set
     (DB      : Phoneme_Database; Required_Set : Features.Feature_Set;
      Context : Contexts.Context) return Phoneme_Instance renames
     Phonemes_Impl.Resolve_Set;
   --  Resolve a set of sounds to a specific phoneme in a given context
   --  Optionally provide a phoneme for resolution

   function Resolve_Text
     (FDB         : Features.Feature_Database; PDB : Phoneme_Database;
      Description : String; Context : Contexts.Context)
      return Phoneme_Instance renames
     Phonemes_Impl.Resolve_Text;
   --  Resolve a textual feature list to a specific phoneme in a given context

   ----------------
   -- CONVERSION --
   ----------------
   function To_XML (Instance : Phoneme_Instance) return String renames
     Phonemes_Impl.To_XML;
   --  Convert a phoneme instance to an XML feature description
   --  This omits any features inherent to a phoneme

   --------------------
   -- INITIALISATION --
   --------------------
   Duplicate_Phoneme : exception;
   procedure Read
     (Doc :     DOM.Core.Document; FDB : Toolkit.Features.Feature_Database;
      PDB : out Phoneme_Database) renames
     Phonemes_Impl.Read;
   --  Read a phoneme database from an XML file
end Toolkit.Phonemes;
