{
   inputs = {
     nix-ada.url = "github:andrewathalye/nix-ada/?commit=c883e52bdd01cb710bc77b958fa7cc5e3c3f00e5";
     rsyntaxtree-nix.url = "github:andrewathalye/rsyntaxtree-nix";
   };

   outputs = { self, nix-ada, rsyntaxtree-nix }:
   let
      nix-ada_s = nix-ada.packages.x86_64-linux;
      rsyntaxtree = rsyntaxtree-nix.packages.x86_64-linux.default;
   in
   with nix-ada_s;
   {
      devShells.x86_64-linux.default = import ./shell.nix { nix-ada = nix-ada_s; rsyntaxtree = rsyntaxtree; };
   };
}
