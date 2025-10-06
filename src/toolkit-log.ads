pragma Ada_2012;

package Toolkit.Log is
   type Log_Category is
     (Warn, Grammars, Features, Contexts, Phonemes, Syllables, Symbols);

   procedure Put_Log (Category : Log_Category; Message : String);
   --  Log a message, depending on whether the user has enabled this category.

   procedure Enable (Category : Log_Category);
   --  Enable logging for this category
end Toolkit.Log;
