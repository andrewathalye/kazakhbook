pragma Ada_2012;

with Ada.Containers.Vectors;

with Toolkit.Features;
with Toolkit.Contexts;
with Toolkit.Phonemes_Impl;

with DOM.Core;

package Toolkit.Phonemes is
   subtype Phoneme_Database is Phonemes_Impl.Phoneme_Database;
   --  Database of all known phonemes.
   --  Linked to a Feature Database and Context Database

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

   -------------
   -- CURSORS --
   -------------
   function To_Cursor (C : Phoneme_Lists.Cursor) return Contexts.Cursor'Class;
   function To_Cursor
     (C : Abstract_Phoneme_Lists.Cursor) return Contexts.Cursor'Class;
   --  Return a read-only cursor that can be used to explore the context
   --   of an abstract phoneme or phoneme instance

   ----------------
   -- RESOLUTION --
   ----------------
   Indeterminate_Phoneme : exception renames
     Phonemes_Impl.Indeterminate_Phoneme;
   function Resolve
     (PDB : Phoneme_Database; AP : Abstract_Phoneme;
      Cur : Contexts.Cursor'Class) return Phoneme_Instance renames
     Phonemes_Impl.Resolve;
   --  Resolve a single abstract phoneme to a phoneme instance within a context
   --
   --  Raise Indeterminate_Phoneme if the context is insufficient to identify
   --  a phone

   function Resolve
     (PDB : Phoneme_Database; List : Abstract_Phoneme_List;
      Cur : Contexts.Cursor'Class) return Phoneme_List;
   --  Resolve a list of abstract phonemes to a list of concrete
   --  phoneme instances based upon context
   --
   --
   --  Raise Indeterminate_Phoneme if the context is insufficient
   --   to identify a phoneme.

   --------------------
   -- TRANSFORMATION --
   --------------------
   function Abstractise
     (Instance : Phoneme_Instance) return Abstract_Phoneme renames
     Phonemes_Impl.Abstractise;
   function Abstractise (List : Phoneme_List) return Abstract_Phoneme_List;
   --  Convert a phoneme or phoneme list to an abstract phoneme (list)
   --  This is used when transforming, as phoneme instances are 'final'
   --  and fixed to a specific context.

   procedure Add
     (AP : in out Abstract_Phoneme; FS : Features.Feature_Set) renames
     Phonemes_Impl.Add;
   procedure Subtract
     (AP : in out Abstract_Phoneme; FS : Features.Feature_Set) renames
     Phonemes_Impl.Subtract;
   --  Perform transformations on the features specified for
   --   an abstract phoneme

   function Get_Features
     (AP : Phoneme_Instance) return Features.Feature_Set renames
     Phonemes_Impl.Get_Features;
   function Get_Features
     (AP : Abstract_Phoneme) return Features.Feature_Set renames
     Phonemes_Impl.Get_Features;
   --  Get all features defined for a(n abstract) phoneme

   ----------------
   -- CONVERSION --
   ----------------
   function To_XML (Instance : Phoneme_Instance) return String renames
     Phonemes_Impl.To_XML;
   --  Convert a phoneme instance to an XML phoneme description

   Unknown_Phoneme : exception renames Phonemes_Impl.Unknown_Phoneme;
   function To_Ada
     (FDB : Features.Feature_Database; PDB : Phoneme_Database; Text : String)
      return Abstract_Phoneme renames
     Phonemes_Impl.To_Ada;
   --  Convert a textual feature-phoneme description to an abstract phoneme
   --  @phoneme name/value name/value ...

   function To_Ada
     (FDB : Features.Feature_Database; PDB : Phoneme_Database;
      XML : DOM.Core.Node) return Abstract_Phoneme;
   --  Convert an XML <require></require> node to an abstract phoneme

   function Transcribe (P : Phoneme_Instance) return String renames
     Phonemes_Impl.Transcribe;
   function Transcribe (PL : Phoneme_List) return String;
   --  Return the transcription of P(L) in IPA notation

   function Transcribe
     (PDB : Phoneme_Database; IPA : String) return Abstract_Phoneme_List;
   --  Transcribe IPA notation back into abstract phonemes

   --------------------
   -- INITIALISATION --
   --------------------
   Duplicate_Phoneme : exception;
   procedure Read
     (Doc :     DOM.Core.Document; FDB : Toolkit.Features.Feature_Database;
      CDB :     Toolkit.Contexts.Context_Database;
      PDB : out Phoneme_Database) renames
     Phonemes_Impl.Read;
   --  Read a phoneme database from an XML file
end Toolkit.Phonemes;
