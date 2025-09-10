{ 
  nix-ada ? import ../nix-ada/default.nix {}
, rsyntaxtree
}:
nix-ada.pkgs.mkShell rec {
   nativeBuildInputs = [
      nix-ada.pkgs.gprbuild  
      nix-ada.pkgs.gnat
      nix-ada.libadalang-tools
      nix-ada.ada-language-server
      nix-ada.pkgs.nodejs
      nix-ada.pkgs.lemminx
      nix-ada.pkgs.gdb
      nix-ada.pkgs.which
      rsyntaxtree
      nix-ada.pkgs.graphviz
   ];
      
   buildInputs = [
      nix-ada.pkgs.gnatcoll-core
      nix-ada.pkgs.xmlada
   ];

   shellHook = ''
      export LIBRARY_TYPE=relocatable
   '';
}
