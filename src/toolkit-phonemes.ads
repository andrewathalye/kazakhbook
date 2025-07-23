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

   package Phoneme_Lists is new Ada.Containers.Vectors
     (Positive, Phoneme_Instance);
   subtype Phoneme_List is Phoneme_Lists.Vector;

   subtype Abstract_Phoneme is Phonemes_Impl.Abstract_Phoneme;
   use type Abstract_Phoneme;
   --  A phoneme outside of its context, which must be resolved
   --  to a concrete phoneme realisation.

   package Abstract_Phoneme_Lists is new Ada.Containers.Vectors
     (Positive, Abstract_Phoneme);
   subtype Abstract_Phoneme_List is Abstract_Phoneme_Lists.Vector;

   ----------------
   -- RESOLUTION --
   ----------------
   Indeterminate_Phoneme : exception;
   function Resolve
     (PDB     : Phoneme_Database; AP : Abstract_Phoneme;
      Context : Contexts.Context) return Phoneme_Instance renames
     Phonemes_Impl.Resolve;
   --  Resolve a single abstract phoneme to a phoneme instance within a context
   --
   --  Raise Indeterminate_Phoneme if the context is insufficient to identify
   --  a phone

   function Resolve
     (PDB              : Phoneme_Database; List : Abstract_Phoneme_List;
      External_Context : Contexts.Context) return Phoneme_List;
   --  Resolve a list of abstract phonemes to a list of concrete
   --  phoneme instances based upon internal and external context.

   ----------------
   -- CONVERSION --
   ----------------
   function To_XML (Instance : Phoneme_Instance) return String renames
     Phonemes_Impl.To_XML;
   --  Convert a phoneme instance to an XML feature description
   --  This omits any features inherent to a phoneme

   Unknown_Phoneme : exception renames Phonemes_Impl.Unknown_Phoneme;
   function To_Ada
     (FDB : Features.Feature_Database; PDB : Phoneme_Database; Text : String)
      return Abstract_Phoneme renames
     Phonemes_Impl.To_Ada;
   --  Convert a featural text description to an abstract phoneme.

   function To_Ada
     (FDB : Features.Feature_Database; PDB : Phoneme_Database;
      XML : DOM.Core.Node) return Abstract_Phoneme;
   --  Convert an XML <require></require> node to an abstract phoneme

   function Transcribe (P : Phoneme_Instance) return String renames
     Phonemes_Impl.Transcribe;
   function Transcribe (PL : Phoneme_List) return String;
   --  Return the transcription of P(L) in IPA notation

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
