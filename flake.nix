{
   inputs.nix-ada.url = "github:andrewathalye/nix-ada/?commit=c883e52bdd01cb710bc77b958fa7cc5e3c3f00e5";

   outputs = { self, nix-ada }:
   let
      nix-ada_s = nix-ada.packages.x86_64-linux;
   in
   with nix-ada_s;
   {
      devShells.x86_64-linux.default = import ./shell.nix { nix-ada = nix-ada_s; };
   };
}
